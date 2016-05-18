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
package main

import (
	"encoding/json"
	"io/ioutil"
	"fmt"
	"log"
    "net/url"
	"reflect"
	"score"
	"score/wire"
	"time"
	"os"
	"sync"

	curses "github.com/rthornton128/goncurses"
)

var cursesLock *sync.Mutex

type Config struct {
	BaseUrl string
	Simulate bool
	SimulationSpeed float64
	SharedSecret *string
	Poll_ms time.Duration
	Check_s time.Duration
	ServerPort int
	DumpData bool
	Cid *string
	Insecure bool
}

type cursesWriter struct {
	window *curses.Window
}
func (c cursesWriter) Write(p []byte) (n int, err error) {
	cursesLock.Lock()
	c.window.Print(string(p[:len(p)]))
	c.window.NoutRefresh()
	curses.Update()
	cursesLock.Unlock()
	return len(p), nil
}

func main() {
	cursesLock = &sync.Mutex{}
	stdscr, err := curses.Init()
	if err != nil {
		log.Fatal("Unable to initialize curses interface: " + err.Error())
	}
	defer curses.End()

	curses.Echo(false)
	curses.CBreak(true)
	curses.Cursor(0)
	stdscr.Keypad(true)

	stdscr.Erase()

	rows, cols := stdscr.MaxYX()
	var logwin, statuswin *curses.Window
	logwin, err = curses.NewWindow(rows - 4, cols, 4, 0)
	if err != nil {
		log.Fatal(err)
	}
	logwin.ScrollOk(true)
	log.SetOutput(cursesWriter{logwin})
	statuswin, err = curses.NewWindow(4, 0, 0, 0)
	if err != nil {
		log.Fatal(err)
	}
	statuswin.Box(curses.ACS_VLINE, curses.ACS_HLINE)
	headline := "Carnifex"
	statuswin.MovePrint(1, cols/2 - len(headline)/2, headline)
	statuswin.NoutRefresh()
	logwin.NoutRefresh()
	curses.Update()
	logwin.Keypad(true)

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
	if(config.SharedSecret == nil) {
		log.Fatal("no sharedsecret provided in config")
	}

	if(config.DumpData) {
		os.Mkdir("api", 0777)
		score.DumpData = true
	}

	judgeUrl, err := url.Parse(config.BaseUrl)
	if(err != nil) {
		log.Fatal("invalid baseURL")
	}

	judge := score.NewJudgeClient(judgeUrl, credentials["user"], credentials["password"], config.Insecure)

	submissions := make(chan score.Submission)
	judgings := make(chan score.Judging)
	Teams := make(chan []score.Team)
	Contest := make(chan *score.Contest)
	Contests := make(chan map[string]*score.Contest)
	ContestConfig := make(chan score.ContestConfig)
	Problems := make(chan []*wire.Problem)

	sleep := time.Millisecond*config.Poll_ms
	sanitySleep := time.Second*config.Check_s

	go judge.ChannelJson(Teams, score.TEAMS, sanitySleep, false)
	go judge.ChannelJson(ContestConfig, score.CONFIG, sanitySleep, false)
	go judge.ChannelJson(Contests, score.CONTESTS, sanitySleep, false)

	go func(Contest chan *score.Contest) {
		for {
			contests := <-Contests
			if(config.Cid == nil && len(contests) == 1) {
				for k := range contests {
					config.Cid = &k
				}
			}
			if(config.Cid == nil) {
				log.Fatal("more than one contest active, but cid not set in config")
			}
			contest, ok := contests[*config.Cid]
			if(!ok) {
				log.Printf("Selected contest (cid %v) not available from judge!", *config.Cid)
				continue
			}
			Contest <- contest
		}
	}(Contest)

	if(config.Simulate) {
		realContest := Contest
		Contest = make(chan *score.Contest)
		start := time.Now().Unix() + 10
		go func(){
			for {
				c := <-realContest
				c.SimulationSpeed = &config.SimulationSpeed
				c.SimulatedStart = &start
				Contest <- c
			}
		}();
	}

	ContestState := score.NewContestState()
	ContestState.Contest = <-Contest
	ContestState.SetTeams(<-Teams)

	judge.InjectCid(ContestState.Contest.Id)

	go judge.ChannelJson(Problems, score.PROBLEMS, sanitySleep, false)
	go judge.ChannelJson(submissions, score.SUBMISSIONS, sleep, true)
	go judge.ChannelJson(judgings, score.JUDGINGS, sleep, true)

	ContestState.Problems = <-Problems

	if(config.Simulate) {
		submissions = simulate(submissions, float64(ContestState.Contest.Start), config.SimulationSpeed, 10*time.Second).(chan score.Submission)
		judgings = simulate(judgings, float64(ContestState.Contest.Start), config.SimulationSpeed, 10*time.Second).(chan score.Judging)
	}

	if(config.ServerPort == 0) {
		config.ServerPort = 8080
	}

	counter := make(chan bool)
	go func() {
		connections := 0
		for {
			cursesLock.Lock()
			statuswin.MovePrint(2, 1, fmt.Sprintf("Connected clients: %d", connections))
			statuswin.Refresh()
			cursesLock.Unlock()
			b := <-counter
			if b {
				connections++
			} else {
				connections--
			}
		}
	}()

	go ListenTCP(config.ServerPort, *config.SharedSecret, ContestState.EventLog, counter)
	go func() {
		for {
			switch logwin.GetChar() {
			case 'q':
				curses.End()
				os.Exit(0)
			case curses.KEY_LEFT:
				log.Println("Left key pressed")
				ContestState.Unfreeze <- false
			case curses.KEY_RIGHT:
				log.Println("Right key pressed")
				ContestState.Unfreeze <- true
			}
		}
	}()

	log.Print("succesfully connected to judge and listening for clients")

	ContestState.EventLoop(submissions, judgings, Teams, Contest, ContestConfig, Problems)
}

func simulate(src interface{}, start, multiplier float64, offset time.Duration) interface{} {
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
			time.AfterFunc(offset + time.Duration(t*1000) * time.Millisecond, func () {
				sink.Send(x)
			})
		}
	}()
	return sink.Interface()
}
