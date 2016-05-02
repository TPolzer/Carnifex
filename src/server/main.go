/*
 * This file is part of "The Scoreboard"
 * Copyright (C) 2016  Tobias Polzer

 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
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

type Config struct {
	BaseUrl string
	Simulate bool
	SimulationSpeed float64
	Poll_ms time.Duration
	Check_s time.Duration
}

func main() {

	secure := false
	scheme := "http"
	if(secure) {
		scheme += "s"
	}

	bytes, err := ioutil.ReadFile("credentials.json")
	if(err != nil) {
		log.Fatal("please supply credentials in credentials.json")
	}
	credentials := make(map[string]string)
	err = json.Unmarshal(bytes, &credentials)
	if(err != nil) {
		log.Fatal("credentials.json is invalid")
	}

	bytes, err = ioutil.ReadFile("config.json")
	if(err != nil) {
		log.Fatal("please supply configuration in config.json")
	}
	var config Config
	err = json.Unmarshal(bytes, &config)
	if(err != nil) {
		log.Fatal("config.json is invalid")
	}

	judgeUrl, err := url.Parse(config.BaseUrl)
	if(err != nil) {
		log.Fatal("invalid baseURL")
	}


	judge := NewJudgeClient(judgeUrl, credentials["user"], credentials["password"])

	submissions := make(chan Submission)
	judgings := make(chan Judging)
	teams := make(chan []Team)
	contest := make(chan *Contest)
	problems := make(chan []*wire.Problem)

	sleep := time.Millisecond*config.Poll_ms
	sanitySleep := time.Second*config.Check_s

	go judge.ChannelJson(teams, TEAMS, sanitySleep, false, false)
	go judge.ChannelJson(contest, CONTEST, sanitySleep, false, false)

	var storedSubmissions []Submission
	var storedJudgings []Judging
	subscribe := make(chan (chan *wire.Message))
	unsubscribe := make(chan (chan *wire.Message))
	ContestState := NewContestState(<-contest, <-teams, nil)

	//inject cid into URLs
	for _, m := range []APIMethod{SUBMISSIONS, JUDGINGS, PROBLEMS} {
		u := judge.urls[m]
		q := u.Query()
		q.Set("cid", strconv.FormatInt(ContestState.contest.Id, 10))
		u.RawQuery = q.Encode()
	}

	go judge.ChannelJson(problems, PROBLEMS, sanitySleep, false, false)
	go judge.ChannelJson(submissions, SUBMISSIONS, sleep, true, true)
	go judge.ChannelJson(judgings, JUDGINGS, sleep, true, true)

	ContestState.problems = <-problems

	if(config.Simulate) {
		submissions = simulate(submissions, float64(ContestState.contest.Start), config.SimulationSpeed).(chan Submission)
		judgings = simulate(judgings, float64(ContestState.contest.Start), config.SimulationSpeed).(chan Judging)
	}

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
				messages := make(chan *wire.Message)
				subscribe <- messages
				cc := make(chan error)
				go func() {
					//look for clients closing the connection
					_, err := conn.Read(make([]byte,1))
					cc <- err
				}()
				MessageLoop:
				for {
					select {
					case message := <-messages:
						buf, _ := proto.Marshal(message)
						err := binary.Write(conn, binary.BigEndian, int64(len(buf)))
						if(err != nil) {
							break MessageLoop
						}
						_, err = conn.Write(buf)
						if(err != nil) {
							break MessageLoop
						}
					case _ = <-cc:
						break MessageLoop
					}
				}
				unsubscribe <- messages
			}()
		}
	}()

	log.Print("succesfully connected to judge and listening for clients")

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
		case p := <-problems:
			if(reflect.DeepEqual(ContestState.problems, p)) {
				continue
			}
			ContestState = NewContestState(ContestState.contest, ContestState.teams, ContestState.observers)
			ContestState.problems = p
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

func simulate(src interface{}, start, multiplier float64) interface{} {
	sink := reflect.MakeChan(reflect.TypeOf(src), 0)
	go func() {
		src := reflect.ValueOf(src)
		for {
			x, ok := src.Recv()
			if(!ok) {
				return
			}
			t := x.FieldByName("Time").Float()
			t -= start
			t /= multiplier
			if(t < 0) {
				continue
			}
//			fmt.Printf("Delaying event for %v\n", time.Duration(t) * time.Second)
			time.AfterFunc(time.Duration(t) * time.Second, func () {
				sink.Send(x)
			})
		}
	}()
	return sink.Interface()
}

type ContestState struct {
	contest *Contest
	teams []Team
	problems []*wire.Problem
	observers []chan *wire.Message
	scoreboard map[TeamId]map[ProblemId]Submissions
	firsts map[ProblemId][]Submission
	submissions map[SubmissionId]Submission
    judgings map[SubmissionId]Judging
}

func NewContestState(contest *Contest, teams []Team, observers []chan *wire.Message) (*ContestState) {
	state := ContestState{
		contest: contest,
		teams: teams,
		observers: observers,
        scoreboard: make(map[TeamId]map[ProblemId]Submissions),
		submissions: make(map[SubmissionId]Submission),
		judgings: make(map[SubmissionId]Judging),
		firsts: make(map[ProblemId][]Submission),
	}
	for _, t := range teams {
		state.scoreboard[t.Id] = make(map[ProblemId]Submissions)
	}
	return &state
}

func (state *ContestState) Broadcast(message *wire.Message) {
	for _, o := range state.observers {
		o <- message
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
	event = state.summarize(submissions, true)
	if(event == nil) {
		return
	}
	unfrozen := state.summarize(submissions, false)
	if(!reflect.DeepEqual(event, unfrozen)) {
		event.Unfrozen = unfrozen
	}
	return
}

func (state *ContestState) summarize(submissions Submissions, respectFreeze bool) (event *wire.Event) {
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
	var penalty int64
	for _, s := range submissions {
		if(float64(state.contest.End) <= s.Time) {
			break;
		}
		*event.SubmitCount++
		judging, ok := state.judgings[s.Id]
		if(!ok) {
			*event.State = wire.SState_PENDING
			return
		}
		if(respectFreeze && float64(state.contest.Freeze) <= s.Time) {
			*event.State = wire.SState_PENDING
		} else {
			if(judging.Outcome == "correct") {
				*event.State = wire.SState_CORRECT
				*event.Penalty = penalty
				for _, sf := range state.firsts[s.Problem] {
					if(sf.Id == s.Id) {
						*event.State = wire.SState_FIRST
					}
				}
				//TODO how/when to round?
				*event.Penalty += int64(math.Ceil(s.Time)) - state.contest.Start
				return
			} else { //TODO special-case compile error penalty
				penalty += state.contest.Penalty
				*event.State = wire.SState_WRONG
			}
		}
	}
	if(*event.SubmitCount == 0) {
		return nil //only invalid submissions, don't count
	} else {
		return event
	}
}

func (state *ContestState) ContestSetup() (*wire.Message) {
	setup := new(wire.ContestSetup)
	setup.Name = &state.contest.Name
	setup.Problems = state.problems
	var teams []*wire.Team
	for i, _ := range state.teams {
		t := &state.teams[i] // force reference semantic
		id := int64(t.Id)
		team := &wire.Team{
			Name: &t.Name,
			Id: &id,
		}
		teams = append(teams, team)
	}
	setup.Teams = teams
	return &wire.Message{
		MessageType: &wire.Message_Setup{
			Setup: setup,
		},
	}
}

func (state *ContestState) BroadcastNewContest() {
	state.Broadcast(state.ContestSetup())
}

func (state *ContestState) ApplySubmission(submission Submission) {
	state.submissions[submission.Id] = submission
	team := submission.Team
	problem := submission.Problem
	teamScore, ok := state.scoreboard[team]
	if(!ok) {
		// Team not public, mute
		return
	}
	state.checkFirsts(submission)
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
	state.checkFirsts(submission)
	event := state.Summarize(teamScore[problem])
	state.Broadcast(ToMessage(event))
}

func (state *ContestState) checkFirsts(submission Submission) {
	judging, ok := state.judgings[submission.Id]
	if(!ok || judging.Outcome != "correct") {
		return
	}
	oldFirst, ok := state.firsts[submission.Problem]
	if(ok) {
		oldTime, newTime := math.Floor(oldFirst[0].Time/60), math.Floor(submission.Time/60) // TODO is this correct??
		if(oldTime < newTime) {
			return
		} else if (oldTime > newTime) {
			state.firsts[submission.Problem] = []Submission{}
			for _, s := range oldFirst {
				event := state.Summarize(state.scoreboard[s.Team][submission.Problem])
				state.Broadcast(ToMessage(event))
			}
		}
	}
	state.firsts[submission.Problem] = append(state.firsts[submission.Problem], submission)
}

func (state *ContestState) Tell(observer chan *wire.Message) {
	observer <- state.ContestSetup()
	for _, problems := range state.scoreboard {
		for _, submissions := range problems {
			event := state.Summarize(submissions)
			if(event == nil) {
				continue
			}
			observer <- ToMessage(event)
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
	client.urls[PROBLEMS], _ = judge.Parse("./api/problems")
	for k, val := range client.urls {
		client.urls[k] = judge.ResolveReference(val)
	}
	return client
}

func (client *JudgeClient) ChannelJson(sink interface{}, method APIMethod, sleep time.Duration, unpack, count bool) {
	min := int64(0)
	sendType := reflect.TypeOf(sink).Elem()
	recvType := sendType
	if(unpack) {
		recvType = reflect.SliceOf(sendType)
	}
	first := true
	for {
		sv := reflect.New(recvType)
		s := sv.Interface()
		err := client.GetJson(method, int64(min), s)
		if(err != nil) {
			if(first) {
				log.Fatal(err)
			}
			log.Print(err)
		} else {
			if(!unpack) {
				reflect.ValueOf(sink).Send(sv.Elem())
			} else {
				slice := sv.Elem()
				for i := 0; i<slice.Len(); i++ {
					v := slice.Index(i)
					if(count) {
						min = v.FieldByName("Id").Int() + 1
					}
					reflect.ValueOf(sink).Send(v)
				}
			}
		}
		first = false
		time.Sleep(sleep)
	}
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
	if(err != nil) {
		return
	}
	defer resp.Body.Close()
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
	Id int64
	Name string
	Freeze int64
	Start int64
	End int64
	Unfreeze int64
	Penalty int64
}

type ContestConfig struct {
	Compile_penalty int64
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
	PROBLEMS
)
