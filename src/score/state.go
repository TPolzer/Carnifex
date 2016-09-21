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
	"reflect"
	"score/wire"
	"sort"
	"sync"
)

var DumpData bool

type ContestState struct {
    Contest *Contest
	Config ContestConfig
    teams []Team
	teamOrder map[TeamId]SortOrderId
    Problems []*wire.Problem
	categories []*wire.Category
	Unfreeze chan bool
	unfrozen int
    scoreboard map[ScoreboardKey]Submissions
    firsts map[FirstKey]Submissions
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
		teamOrder: make(map[TeamId]SortOrderId),
		scoreboard: make(map[ScoreboardKey]Submissions),
		submissions: make(map[SubmissionId]Submission),
		judgings: make(map[SubmissionId]Judging),
		firsts: make(map[FirstKey]Submissions),
		EventLog: new(LogType),
    }
	state.EventLog.Cond = sync.NewCond(&state.EventLog.Lock)
    return &state
}

func (state *ContestState) SetTeams(teams []Team) {
	state.teams = teams
	state.teamOrder = make(map[TeamId]SortOrderId)
	for _, t := range teams {
		for _, c := range state.categories {
			if(*c.CategoryId == t.Category) {
				state.teamOrder[t.Id] = SortOrderId(*c.SortOrder)
				break
			}
		}
	}
}

func (state *ContestState) SetCategories(categories []*wire.Category) {
	state.categories = categories
	state.SetTeams(state.teams) // refresh sort order for teams
}

// ResetClone returns a fresh ContestState that shares all state, but has no applied submissions / judgings. The EventLog is atomically emptied and its version incremented.
func (state *ContestState) ResetClone() (res *ContestState) {
	res = NewContestState()
	res.Contest = state.Contest
	res.SetCategories(state.categories)
	res.SetTeams(state.teams)
	res.Unfreeze = state.Unfreeze
	res.EventLog = state.EventLog
	res.Config = state.Config
	res.unfrozen = state.unfrozen
	res.Problems = state.Problems
	res.EventLog.update(func (log *LogType) {
		log.Version++
		log.Msgs = nil
	})
	return
}

func (state *ContestState) EventLoop(submissions chan Submission, judgings chan Judging, Teams chan []Team, Contest chan *Contest, ContestConfig chan ContestConfig, Problems chan []*wire.Problem, Categories chan []*wire.Category) {
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
		case cs := <-Categories:
			if(reflect.DeepEqual(state.categories, cs)) {
				continue
			}
			state = state.ResetClone()
			state.SetCategories(cs)
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
	var SortOrder SortOrderId
    if(len(submissions) > 0) {
        Team := int64(submissions[0].Team)
		SortOrder = state.teamOrder[submissions[0].Team]
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
        if(respectFreeze && state.Contest.Freeze != nil && float64(*state.Contest.Freeze) <= s.Time) {
            *event.State = wire.SState_PENDING
        } else {
            if(judging.Outcome == "correct") {
                *event.State = wire.SState_CORRECT
                *event.Penalty = penalty
                for _, sf := range state.firsts[FirstKey{SortOrder, s.Problem}] {
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
    setup.Freeze = state.Contest.Freeze
    setup.End = &state.Contest.End
	setup.Categories = state.categories
    var Teams []*wire.Team
    for i, _ := range state.teams {
        t := &state.teams[i] // force reference semantic
        id := int64(t.Id)
        team := &wire.Team{
            Name: &t.Name,
            Id: &id,
            Affiliation: &t.Affiliation,
			Category: &t.Category,
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
    problem := submission.Problem
    _, ok := state.teamOrder[submission.Team]
    if(!ok) {
        // Team not public, mute
        return
    }
    state.checkFirsts(submission)
	key := ScoreboardKey{submission.Team, problem}
    state.scoreboard[key] = append(state.scoreboard[key], submission)
    event := state.Summarize(state.scoreboard[key])
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
    _, ok = state.teamOrder[submission.Team]
	if(!ok) {
		// Team not public, mute
		return
	}
	state.checkFirsts(submission)
	key := ScoreboardKey{submission.Team, problem}
	event := state.Summarize(state.scoreboard[key])
	state.Broadcast(ToMessage(event))
}

func (state *ContestState) checkFirsts(submission Submission) {
	SortOrder := state.teamOrder[submission.Team]
	judging, ok := state.judgings[submission.Id]
	if(!ok || judging.Outcome != "correct") {
		return
	}
	key := FirstKey{SortOrder,submission.Problem}
	oldFirst, ok := state.firsts[key]
	if(ok) {
		oldTime, newTime := oldFirst[0].Time, submission.Time
		if(oldTime < newTime) {
			return
		} else if (oldTime > newTime) {
			state.firsts[key] = []Submission{}
			for _, s := range oldFirst {
				event := state.Summarize(state.scoreboard[ScoreboardKey{s.Team,submission.Problem}])
				state.Broadcast(ToMessage(event))
			}
		}
	}
	state.firsts[key] = append(state.firsts[key], submission)
}
