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
	"fmt"
	"encoding/json"
	"log"
	"net/http"
	"net/url"
	"reflect"
	"strconv"
	"time"
)

type JudgeClient struct {
    client http.Client
    username string
    password string
    judge *url.URL
    urls map[APIMethod] *url.URL
}

type APIMethod int8
const (
	JUDGINGS APIMethod = iota
	SUBMISSIONS
	CONTEST
	CONFIG
	TEAMS
	PROBLEMS
)

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
    client.urls[CONFIG], _ = judge.Parse("./api/config")
    client.urls[TEAMS], _ = judge.Parse("./api/teams?public=true")
    client.urls[PROBLEMS], _ = judge.Parse("./api/problems")
    for k, val := range client.urls {
        client.urls[k] = judge.ResolveReference(val)
    }
    return client
}

func (client *JudgeClient) InjectCid(id int64) {
	for _, m := range []APIMethod{SUBMISSIONS, JUDGINGS, PROBLEMS} {
		u := client.urls[m]
		q := u.Query()
		q.Set("cid", strconv.FormatInt(id, 10))
		u.RawQuery = q.Encode()
	}
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
		return fmt.Errorf("Got http response \"%s\" while fetching \"%s\"", resp.Status, url)
	}
	jsonDecoder := json.NewDecoder(resp.Body)
	err = jsonDecoder.Decode(p)
	return
}
