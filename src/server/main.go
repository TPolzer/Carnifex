package main

import (
	"encoding/json"
	"fmt"
	"github.com/davecgh/go-spew/spew"
	"io/ioutil"
	"math"
    "net/http"
    "net/url"
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
		recv := make(chan []Submission)
		errc := make(chan error)
		for {
			go judge.GetJson(SUBMISSIONS, int64(min), recv, errc)
			select {
			case err := <-errc:
				fmt.Println(err)
			case s := <-recv:
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
		recv := make(chan []Judging)
		errc := make(chan error)
		for {
			go judge.GetJson(JUDGINGS, int64(min), recv, errc)
			select {
			case err := <-errc:
				fmt.Println(err)
			case s := <-recv:
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
		recv := make(chan []Team)
		errc := make(chan error)
		first := true
		for {
			go judge.GetJson(TEAMS, 0, recv, errc)
			select {
			case err := <-errc:
				if(first) {
					panic(err)
				}
				fmt.Println(err)
			case s := <-recv:
				teams <- s
			}
			first = false
			time.Sleep(sanitySleep)
		}
	}()
	go func() {
		recv := make(chan *Contest)
		errc := make(chan error)
		first := true
		for {
			go judge.GetJson(CONTEST, 0, recv, errc)
			select {
			case err := <-errc:
				if(first) {
					panic(err)
				}
				fmt.Println(err)
			case s := <-recv:
				contest <- s
			}
			first = false
			time.Sleep(sanitySleep)
		}
	}()

	var storedSubmissions []Submission
	var storedJudgings []Judging
	observers := new([]chan wire.Message)
	subscribe := make(chan (chan wire.Message))
	ContestState := NewContestState(<-contest, <-teams, observers)
	ContestState.BroadcastNewContest()

	go func() {
		messages := make(chan wire.Message)
		subscribe <- messages
		for {
			message := <-messages
			spew.Printf("message: %#v\n", message)
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
			ContestState = NewContestState(ContestState.contest, t, observers)
			ContestState.BroadcastNewContest()
		case c := <-contest:
			if(reflect.DeepEqual(ContestState.contest, c)) {
				continue
			}
			ContestState = NewContestState(c, ContestState.teams, observers)
			ContestState.BroadcastNewContest()
		case s:= <-subscribe:
			*observers = append(*observers, s)
			ContestState.Tell(s)
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
	observers *[]chan wire.Message
	scoreboard map[TeamId]map[ProblemId]Submissions
	submissions map[SubmissionId]Submission
    judgings map[SubmissionId]Judging
}

func NewContestState(contest *Contest, teams []Team, observers *[]chan wire.Message) (*ContestState) {
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
	for _, o := range *state.observers {
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
		if(float64(state.contest.Freeze) >= s.Time) { //TODO how to handle resolving
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
    fmt.Println("received Submission", submission)
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
    fmt.Println("received Judging", judging)
	oldJudging, ok := state.judgings[judging.Submission]
	if(ok && oldJudging.Id > judging.Id) {
		return
	}
	state.judgings[judging.Submission] = judging
	submission, ok := state.submissions[judging.Submission]
	if(!ok) {
		fmt.Printf("no submission at %v\n", judging.Submission)
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

func (client *JudgeClient) GetJson(method APIMethod, min int64, sink interface{}, errc chan<- error) {
	url := client.urls[method]
	if(min != 0) {
		tmp := *url
		url = &tmp
		q := url.Query()
		q.Set("fromid", strconv.FormatInt(min,10))
		url.RawQuery = q.Encode()
	}
	elemType := reflect.TypeOf(sink).Elem()
	request, _ := http.NewRequest("", url.String(), nil)
	request.SetBasicAuth(client.username, client.password)
	resp, err := client.client.Do(request)
	defer resp.Body.Close()
	if(err != nil) {
		errc <- err
		return
	}
	if(resp.StatusCode != 200) {
		errc <- fmt.Errorf("Got http response \"%s\"", resp.Status)
		return
	}
	jsonDecoder := json.NewDecoder(resp.Body)
	res := reflect.New(elemType)
	err = jsonDecoder.Decode(res.Interface())
	if(err != nil) {
		panic(err)
	}
	reflect.ValueOf(sink).Send(res.Elem())
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
