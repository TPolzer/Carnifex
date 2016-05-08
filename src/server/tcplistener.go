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
	"encoding/binary"
	"log"
	"net"
	"github.com/golang/protobuf/proto"
	"score/wire"
)

func ListenTCP(port int, subscribe, unsubscribe chan (chan *wire.Message)) {
	listener, err := net.ListenTCP("tcp", &net.TCPAddr{Port:port})
    if(err != nil) {
        log.Fatal(err)
    }
	for {
		conn, err := listener.AcceptTCP()
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
}
