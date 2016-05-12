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
	"crypto/rand"
	"golang.org/x/crypto/nacl/secretbox"
	"golang.org/x/crypto/scrypt"

	"encoding/binary"
	"log"
	"net"
	"time"
	"github.com/golang/protobuf/proto"
	"score/wire"
)

func ListenTCP(port int, password string, subscribe, unsubscribe chan (chan *wire.Message), counter chan bool) {
	var key [32]byte
	salt := make([]byte, 32) // interoperability with libsodium depends on len == crypto_pwhash_SALTBYTES
	_, err := rand.Read(salt)
	if(err != nil) {
		log.Fatal(err)
	}
	tmp, err := scrypt.Key([]byte(password), salt, 16384, 8, 1, 32) // recommended parameters from https://godoc.org/golang.org/x/crypto/scrypt
	if(err != nil) {
		log.Fatal(err)
	}
	copy(key[:], tmp)

	listener, err := net.ListenTCP("tcp", &net.TCPAddr{Port:port})
    if(err != nil) {
        log.Fatal(err)
    }
	for {
		conn, err := listener.AcceptTCP()
		if(err != nil) {
			log.Fatal(err)
		}
		counter <- true
		go func() {
			defer func() {
				counter <- false
			}()
			var nonce [24]byte
			_, err := rand.Read(nonce[8:])
			if(err != nil) {
				log.Printf("Generating random nonce for client %v failed: %v\n", conn, err)
				conn.Close()
				return
			}
			ctr := uint64(0)
			write := func (m *wire.Message) (err error) {
				message, _ := proto.Marshal(m)
				err = binary.Write(conn, binary.BigEndian, int64(len(message)))
				if(err != nil) {
					return
				}
				binary.BigEndian.PutUint64(nonce[:8], ctr)
				ctr++
				encrypted := secretbox.Seal(nil, message, &nonce, &key)
				_, err = conn.Write(encrypted)
				return
			}
			_, err = conn.Write(append(nonce[8:], salt...))
			if(err != nil) {
				conn.Close()
				return
			}

			messages := make(chan *wire.Message)
			subscribe <- messages
			cc := make(chan error)
			go func() {
				//look for clients closing the connection
				//a client should never send any data
				_, err := conn.Read(make([]byte,1))
				cc <- err
			}()
			ticker := time.NewTicker(time.Second)
			defer ticker.Stop()
			beat := int64(0)
			MessageLoop:
			for {
				// Blocking writes only occur when the write buffer is full.
				// Linux has a default of 16KB, which should be enough for a whole
				// contest after setup. Blocking writes stall all clients, so kill
				// everybody who doesn't respond for bufferSize + 1s.
				select {
				case _ = <-ticker.C:
					conn.SetWriteDeadline(time.Now().Add(1*time.Second)) //
					err := write(&wire.Message{
						MessageType: &wire.Message_HeartBeat{
							HeartBeat: beat,
						},
					})
					beat++
					if(err != nil) {
						log.Println(err)
						break MessageLoop
					}
				case m := <-messages:
					conn.SetWriteDeadline(time.Now().Add(1*time.Second)) //
					if(m == nil) {
						continue
					}
					err := write(m)
					if(err != nil) {
						log.Println(err)
						break MessageLoop
					}
				case _ = <-cc:
					break MessageLoop
				}
			}
			unsubscribe <- messages
			conn.Close()
		}()
	}
}
