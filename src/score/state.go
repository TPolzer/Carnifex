package score

import (
	"github.com/golang/protobuf/proto"
	"log"
	"reflect"
	"score/wire"
	"sort"
)

type ContestState struct {
    Contest *Contest
    Teams []Team
    Problems []*wire.Problem
    observers []chan *wire.Message
    scoreboard map[TeamId]map[ProblemId]Submissions
    firsts map[ProblemId][]Submission
    submissions map[SubmissionId]Submission
    judgings map[SubmissionId]Judging
}

func NewContestState(Contest *Contest, Teams []Team, observers []chan *wire.Message) (*ContestState) {
    state := ContestState{
        Contest: Contest,
        Teams: Teams,
        observers: observers,
        scoreboard: make(map[TeamId]map[ProblemId]Submissions),
        submissions: make(map[SubmissionId]Submission),
        judgings: make(map[SubmissionId]Judging),
        firsts: make(map[ProblemId][]Submission),
    }
    for _, t := range Teams {
        state.scoreboard[t.Id] = make(map[ProblemId]Submissions)
    }
    return &state
}

func (state *ContestState) EventLoop(submissions chan Submission, judgings chan Judging, Teams chan []Team, Contest chan *Contest, Problems chan []*wire.Problem, subscribe, unsubscribe chan (chan *wire.Message)) {
	var storedSubmissions []Submission
    var storedJudgings []Judging
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
            if(reflect.DeepEqual(state.Teams, t)) {
                continue
            }
            state = NewContestState(state.Contest, t, state.observers)
            state.BroadcastNewContest()
        case c := <-Contest:
            if(reflect.DeepEqual(state.Contest, c)) {
                continue
            }
            state = NewContestState(c, state.Teams, state.observers)
            state.BroadcastNewContest()
        case p := <-Problems:
            if(reflect.DeepEqual(state.Problems, p)) {
                continue
            }
            state = NewContestState(state.Contest, state.Teams, state.observers)
            state.Problems = p
            state.BroadcastNewContest()
        case s:= <-subscribe:
            log.Printf("subscribing %v", s)
            state.observers = append(state.observers, s)
            state.Tell(s)
        case u:= <-unsubscribe:
            log.Printf("unsubscribing %v", u)
            for i, o := range state.observers {
                if(o == u) {
                    state.observers = append(state.observers[:i], state.observers[i+1:]...)
                    break
                }
            }
            close(u)
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
                penalty += state.Contest.Penalty / 60
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
    var Teams []*wire.Team
    for i, _ := range state.Teams {
        t := &state.Teams[i] // force reference semantic
        id := int64(t.Id)
        team := &wire.Team{
            Name: &t.Name,
            Id: &id,
            Affiliation: &t.Affiliation,
        }
        Teams = append(Teams, team)
    }
    setup.Teams = Teams
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

func (state *ContestState) Tell(observer chan *wire.Message) {
	observer <- state.ContestSetup()
	for _, Problems := range state.scoreboard {
		for _, submissions := range Problems {
			event := state.Summarize(submissions)
			if(event == nil) {
				continue
			}
			observer <- ToMessage(event)
		}
	}
}
