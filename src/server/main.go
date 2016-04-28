package main

import (
	"encoding/binary"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"log"
	"math"
	"net"
    "net/http"
    "net/url"
	"github.com/golang/protobuf/proto"
	"reflect"
	"sort"
	"score/wire"
	"strconv"
	"time"
)

func main() {

	secure := false
	scheme := "http"
	if(secure) {
		scheme += "s"
	}

    domain := "domjudge.cs.fau.de"

    judgeUrl, _ := url.Parse (scheme + "://" + domain)
	bytes, err := ioutil.ReadFile("credentials.json")
	if(err != nil) {
		fmt.Println("please supply credentials in credentials.json")
		panic(err)
	}
	credentials := make(map[string]string)
	err = json.Unmarshal(bytes, &credentials)
	if(err != nil) {
		fmt.Println("credentials.json is invalid")
		panic(err)
	}
	judge := NewJudgeClient(judgeUrl, credentials["user"], credentials["password"])
	_ = judge
	_ = fmt.Println

	submissions := make(chan Submission)
	judgings := make(chan Judging)
	teams := make(chan []Team)
	contest := make(chan *Contest)

	sleep := time.Millisecond*100
	sanitySleep := time.Second*10

	go func() {
		min := SubmissionId(0)
		for {
			var s Submissions
			err := judge.GetJson(SUBMISSIONS, int64(min), &s)
			if(err != nil) {
				fmt.Println(err)
			} else {
				for _, v := range s {
					min = v.Id + 1
                    time.Sleep(10*time.Millisecond)
					submissions <- v
				}
			}
			time.Sleep(sleep)
		}
	}()
	go func() {
		min := JudgingId(0)
		for {
			var s []Judging
			err := judge.GetJson(JUDGINGS, int64(min), &s)
			if(err != nil) {
				fmt.Println(err)
			} else {
				for _, v := range s {
					min = v.Id + 1
                    time.Sleep(10*time.Millisecond)
					judgings <- v
				}
			}
			time.Sleep(sleep)
		}
	}()
	go func() {
		first := true
		for {
			var s []Team
			err := judge.GetJson(TEAMS, 0, &s)
			if(err != nil) {
				if(first) {
					panic(err)
				}
				fmt.Println(err)
			} else {
				teams <- s
			}
			first = false
			time.Sleep(sanitySleep)
		}
	}()
	go func() {
		first := true
		for {
			var s *Contest
			err := judge.GetJson(CONTEST, 0, &s)
			if(err != nil) {
				if(first) {
					panic(err)
				}
				fmt.Println(err)
			} else {
				contest <- s
			}
			first = false
			time.Sleep(sanitySleep)
		}
	}()

	var storedSubmissions []Submission
	var storedJudgings []Judging
	subscribe := make(chan (chan wire.Message))
	unsubscribe := make(chan (chan wire.Message))
	ContestState := NewContestState(<-contest, <-teams, nil)
	ContestState.BroadcastNewContest()

	listener, err := net.Listen("tcp", ":8080")
	if(err != nil) {
		log.Fatal(err)
	}
	go func() {
		for {
			conn, err := listener.Accept()
			if(err != nil) {
				log.Fatal(err)
			}
			go func() {
				messages := make(chan wire.Message)
				subscribe <- messages
				for {
					message := <-messages
					buf, _ := proto.Marshal(&message)
					binary.Write(conn, binary.BigEndian, int64(len(buf)))
					_, err := conn.Write(buf)
					if(err != nil) {
						unsubscribe <- messages
						return
					}
				}
			}()
		}
	}()

	fmt.Println("succesfully connected to judge and listening for clients")

	for {
		oldState := ContestState
		select {
		case s := <-submissions:
			storedSubmissions = append(storedSubmissions, s)
			ContestState.ApplySubmission(s)
		case j := <-judgings:
			storedJudgings = append(storedJudgings, j)
			ContestState.ApplyJudging(j)
		case t := <-teams:
			if(reflect.DeepEqual(ContestState.teams, t)) {
				continue
			}
			ContestState = NewContestState(ContestState.contest, t, ContestState.observers)
			ContestState.BroadcastNewContest()
		case c := <-contest:
			if(reflect.DeepEqual(ContestState.contest, c)) {
				continue
			}
			ContestState = NewContestState(c, ContestState.teams, ContestState.observers)
			ContestState.BroadcastNewContest()
		case s:= <-subscribe:
			log.Printf("subscribing %v", s)
			ContestState.observers = append(ContestState.observers, s)
			ContestState.Tell(s)
		case u:= <-unsubscribe:
			log.Printf("unsubscribing %v", u)
			for i, o := range ContestState.observers {
				if(o == u) {
					ContestState.observers = append(ContestState.observers[:i], ContestState.observers[i+1:]...)
					break
				}
			}
			close(u)
		}
		if(oldState != ContestState) {
			for _, s := range storedSubmissions {
				ContestState.ApplySubmission(s)
			}
			for _, j := range storedJudgings {
				ContestState.ApplyJudging(j)
			}
		}
	}

}

type ContestState struct {
	contest *Contest
	teams []Team
	observers []chan wire.Message
	scoreboard map[TeamId]map[ProblemId]Submissions
	submissions map[SubmissionId]Submission
    judgings map[SubmissionId]Judging
}

func NewContestState(contest *Contest, teams []Team, observers []chan wire.Message) (*ContestState) {
	state := ContestState{
		contest: contest,
		teams: teams,
		observers: observers,
        scoreboard: make(map[TeamId]map[ProblemId]Submissions),
		submissions: make(map[SubmissionId]Submission),
		judgings: make(map[SubmissionId]Judging),
	}
	for _, t := range teams {
		state.scoreboard[t.Id] = make(map[ProblemId]Submissions)
	}
	return &state
}

func (state *ContestState) Broadcast(message *wire.Message) {
	fmt.Printf("broadcasting %v\n", *message)
	for _, o := range state.observers {
		o <- *message
	}
}

func ToMessage(event *wire.Event) *wire.Message {
	return &wire.Message{
		MessageType: &wire.Message_Event{
			Event: event,
		},
	}
}

func (state *ContestState) Summarize(submissions Submissions) (event *wire.Event) {
	sort.Sort(submissions)
	if(len(submissions) > 0) {
		Team := int64(submissions[0].Team)
		Problem := int64(submissions[0].Problem)
		event = &wire.Event{
			Team: &Team,
			Problem: &Problem,
			SubmitCount: new(int64),
			Penalty: new(int64),
			State: new(wire.SState),
		}
	}
	for _, s := range submissions {
		*event.SubmitCount++
		judging, ok := state.judgings[s.Id]
		if(!ok) {
			*event.State = wire.SState_PENDING
			return
		}
		if(float64(state.contest.Freeze) <= s.Time) { //TODO how to handle resolving
			*event.State = wire.SState_PENDING
		} else {
			if(judging.Outcome == "correct") {
				*event.State = wire.SState_CORRECT
				//TODO is penalty float or int (or something else)?
				*event.Penalty += int64(math.Ceil(s.Time)) - state.contest.Start
				return
			} else { //TODO special-case compile error penalty
				*event.Penalty += state.contest.Penalty
				*event.State = wire.SState_WRONG
			}
		}
	}
	return
}

func (*ContestState) BroadcastNewContest() {
}

func (state *ContestState) ApplySubmission(submission Submission) {
	state.submissions[submission.Id] = submission
	team := submission.Team
	problem := submission.Problem
	teamScore, ok := state.scoreboard[team]
	if(!ok) {
		return
	}
	teamScore[problem] = append(teamScore[problem], submission)
	event := state.Summarize(teamScore[problem])
	state.Broadcast(ToMessage(event))
}

func (state *ContestState) ApplyJudging(judging Judging) {
	oldJudging, ok := state.judgings[judging.Submission]
	if(ok && oldJudging.Id > judging.Id) {
		return
	}
	state.judgings[judging.Submission] = judging
	submission, ok := state.submissions[judging.Submission]
	if(!ok) {
		return
	}
	problem := submission.Problem
	team := submission.Team
	teamScore, ok := state.scoreboard[team]
	if(!ok) {
		// Team not public, mute
		return
	}
	event := state.Summarize(teamScore[problem])
	state.Broadcast(ToMessage(event))
}

func (state *ContestState) Tell(observer chan wire.Message) {
	for _, problems := range state.scoreboard {
		for _, submissions := range problems {
			event := state.Summarize(submissions)
			if(event == nil) {
				continue
			}
			observer <- *ToMessage(event)
		}
	}
}

type JudgeClient struct {
	client http.Client
	username string
	password string
	judge *url.URL
	urls map[APIMethod] *url.URL
}

func NewJudgeClient(judge *url.URL, username string, password string) *JudgeClient {
	var client = &JudgeClient{
		client: http.Client{},
		judge: judge,
		urls: make(map[APIMethod]*url.URL),
		username: username,
		password: password,
	}
	client.urls[JUDGINGS], _ = judge.Parse("./api/judgings")
	client.urls[SUBMISSIONS], _ = judge.Parse("./api/submissions")
	client.urls[CONTEST], _ = judge.Parse("./api/contest")
	client.urls[TEAMS], _ = judge.Parse("./api/teams?public=true")
	for k, val := range client.urls {
		client.urls[k] = judge.ResolveReference(val)
	}
	return client
}

func (client *JudgeClient) GetJson(method APIMethod, min int64, p interface{}) (err error){
	url := client.urls[method]
	if(min != 0) {
		tmp := *url
		url = &tmp
		q := url.Query()
		q.Set("fromid", strconv.FormatInt(min,10))
		url.RawQuery = q.Encode()
	}
	request, _ := http.NewRequest("", url.String(), nil)
	request.SetBasicAuth(client.username, client.password)
	resp, err := client.client.Do(request)
	defer resp.Body.Close()
	if(err != nil) {
		return
	}
	if(resp.StatusCode != 200) {
		return fmt.Errorf("Got http response \"%s\"", resp.Status)
	}
	jsonDecoder := json.NewDecoder(resp.Body)
	err = jsonDecoder.Decode(p)
	return
}

type JudgingId int64
type SubmissionId int64
type TeamId int64
type ProblemId int64

type Judging struct {
	Id JudgingId
	Time float64
	Submission SubmissionId
	Outcome string
}

type Submission struct {
	Id SubmissionId
	Time float64
	Team TeamId
	Problem ProblemId
}
type Submissions []Submission

/* sort interface */
func (s Submissions) Len() int {
	return len(s)
}
func (s Submissions) Less(i, j int) bool {
	return s[i].Id < s[j].Id
}
func (s Submissions) Swap(i, j int) {
	s[i], s[j] = s[j], s[i]
}
/******************/

type Contest struct {
	Name string
	Freeze int64
	Start int64
	End int64
	Unfreeze int64
	Penalty int64
}

type Team struct {
	Id TeamId
	Name string
}

type APIMethod int8
const (
	JUDGINGS APIMethod = iota
	SUBMISSIONS
	CONTEST
	TEAMS
)
