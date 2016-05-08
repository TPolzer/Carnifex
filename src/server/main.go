/*
 * This file is part of "The Scoreboard"
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
	"log"
    "net/url"
	"reflect"
	"score"
	"score/wire"
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


	judge := score.NewJudgeClient(judgeUrl, credentials["user"], credentials["password"])

	submissions := make(chan score.Submission)
	judgings := make(chan score.Judging)
	Teams := make(chan []score.Team)
	Contest := make(chan *score.Contest)
	Problems := make(chan []*wire.Problem)

	sleep := time.Millisecond*config.Poll_ms
	sanitySleep := time.Second*config.Check_s

	go judge.ChannelJson(Teams, score.TEAMS, sanitySleep, false, false)
	go judge.ChannelJson(Contest, score.CONTEST, sanitySleep, false, false)

	subscribe := make(chan (chan *wire.Message))
	unsubscribe := make(chan (chan *wire.Message))
	ContestState := score.NewContestState(<-Contest, <-Teams, nil)

	judge.InjectCid(ContestState.Contest.Id)

	go judge.ChannelJson(Problems, score.PROBLEMS, sanitySleep, false, false)
	go judge.ChannelJson(submissions, score.SUBMISSIONS, sleep, true, true)
	go judge.ChannelJson(judgings, score.JUDGINGS, sleep, true, true)

	ContestState.Problems = <-Problems

	if(config.Simulate) {
		submissions = simulate(submissions, float64(ContestState.Contest.Start), config.SimulationSpeed).(chan score.Submission)
		judgings = simulate(judgings, float64(ContestState.Contest.Start), config.SimulationSpeed).(chan score.Judging)
	}

	go ListenTCP(8080, subscribe, unsubscribe)

	log.Print("succesfully connected to judge and listening for clients")

	ContestState.EventLoop(submissions, judgings, Teams, Contest, Problems, subscribe, unsubscribe)
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
			time.AfterFunc(time.Duration(t*1000) * time.Millisecond, func () {
				sink.Send(x)
			})
		}
	}()
	return sink.Interface()
}
