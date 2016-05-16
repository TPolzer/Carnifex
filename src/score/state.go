/*
 * This file is part of "Carnifex"
 * Copyright (C) 2016  Tobias Polzer, Dominik Paulus

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
package score

import (
	"github.com/golang/protobuf/proto"
	"log"
	"reflect"
	"score/wire"
	"sort"
	"sync"
)

type ContestState struct {
    Contest *Contest
	Config ContestConfig
    teams []Team
    Problems []*wire.Problem
	Unfreeze chan bool
	unfrozen int
    scoreboard map[TeamId]map[ProblemId]Submissions
    firsts map[ProblemId][]Submission
    submissions map[SubmissionId]Submission
    judgings map[SubmissionId]Judging
	EventLog *LogType
}

func (log *LogType) update(f func(*LogType)) {
	log.Lock.Lock()
	f(log)
	log.Cond.Broadcast()
	log.Lock.Unlock()
}

type LogType struct {
	Lock sync.Mutex
	Cond *sync.Cond
	Msgs []*wire.Message
	Version int64
}

func NewContestState() (*ContestState) {
    state := ContestState{
		Unfreeze: make(chan bool),
        scoreboard: make(map[TeamId]map[ProblemId]Submissions),
        submissions: make(map[SubmissionId]Submission),
        judgings: make(map[SubmissionId]Judging),
        firsts: make(map[ProblemId][]Submission),
		EventLog: new(LogType),
    }
	state.EventLog.Cond = sync.NewCond(&state.EventLog.Lock)
    return &state
}

func (state *ContestState) SetTeams(teams []Team) {
	state.teams = teams
    for _, t := range teams {
        state.scoreboard[t.Id] = make(map[ProblemId]Submissions)
    }
}

func (state *ContestState) ResetClone() (res *ContestState) {
	res = NewContestState()
	res.Contest = state.Contest
	res.SetTeams(state.teams)
	res.Unfreeze = state.Unfreeze
	res.EventLog.update(func (log *LogType) {
		log.Version++
		log.Msgs = nil
	})
	return
}

func (state *ContestState) EventLoop(submissions chan Submission, judgings chan Judging, Teams chan []Team, Contest chan *Contest, ContestConfig chan ContestConfig, Problems chan []*wire.Problem) {
	var storedSubmissions []Submission
    var storedJudgings []Judging
    state.BroadcastNewContest()
	for {
        oldState := state
        select {
        case s := <-submissions:
            storedSubmissions = append(storedSubmissions, s)
            state.ApplySubmission(s)
        case j := <-judgings:
            storedJudgings = append(storedJudgings, j)
            state.ApplyJudging(j)
        case t := <-Teams:
            if(reflect.DeepEqual(state.teams, t)) {
                continue
            }
			state = state.ResetClone()
			state.SetTeams(t)
            state.BroadcastNewContest()
        case c := <-Contest:
            if(reflect.DeepEqual(state.Contest, c)) {
                continue
            }
			state = state.ResetClone()
			state.Contest = c
            state.BroadcastNewContest()
		case c := <-ContestConfig:
			if(reflect.DeepEqual(state.Config, c)) {
				continue
			}
			state = state.ResetClone()
			state.Config = c
			state.BroadcastNewContest()
        case p := <-Problems:
            if(reflect.DeepEqual(state.Problems, p)) {
                continue
            }
            state = state.ResetClone()
            state.Problems = p
            state.BroadcastNewContest()
		case u:=<-state.Unfreeze:
			if(u) {
				state.unfrozen++
				state.Broadcast(Unfreeze(u))
			} else if(state.unfrozen > 0) {
				state.unfrozen--
				state.Broadcast(Unfreeze(u))
			}
        }
        if(oldState != state) {
            for _, s := range storedSubmissions {
                state.ApplySubmission(s)
            }
            for _, j := range storedJudgings {
                state.ApplyJudging(j)
            }
        }
    }
}

func (state *ContestState) Broadcast(message *wire.Message) {
	if(message == nil) {
		return
	}
	state.EventLog.update(func (log *LogType) {
		log.Msgs = append(log.Msgs, message)
	})
}

func Unfreeze(unfreeze bool) *wire.Message {
	return &wire.Message{
		MessageType: &wire.Message_Unfreeze{
			Unfreeze: unfreeze,
		},
	}
}

func ToMessage(event *wire.Event) *wire.Message {
	if(event == nil) {
		return nil
	}
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
        if(float64(state.Contest.End) <= s.Time) {
            break;
        }
        *event.SubmitCount++
        judging, ok := state.judgings[s.Id]
        if(!ok) {
            *event.State = wire.SState_PENDING
            return
        }
        if(respectFreeze && float64(state.Contest.Freeze) <= s.Time) {
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
                event.ContestTime = proto.Int64(int64((s.Time - float64(state.Contest.Start))/60))
                *event.Penalty += *event.ContestTime
                return
            } else { //TODO special-case compile error penalty
				if(!(state.Config.Compile_penalty == 0 && judging.Outcome == "compiler-error")) {
	                penalty += state.Contest.Penalty / 60
				}
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
    setup.Name = &state.Contest.Name
    setup.Problems = state.Problems
    setup.Start = &state.Contest.Start
    setup.Freeze = &state.Contest.Freeze
    setup.End = &state.Contest.End
    var Teams []*wire.Team
    for i, _ := range state.teams {
        t := &state.teams[i] // force reference semantic
        id := int64(t.Id)
        team := &wire.Team{
            Name: &t.Name,
            Id: &id,
            Affiliation: &t.Affiliation,
        }
        Teams = append(Teams, team)
    }
    setup.Teams = Teams
	if(state.Contest.SimulationSpeed != nil) {
		setup.SimulatedStart = state.Contest.SimulatedStart
		setup.SimulationSpeed = state.Contest.SimulationSpeed
	}
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
		oldTime, newTime := oldFirst[0].Time, submission.Time
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
