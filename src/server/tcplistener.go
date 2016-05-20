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
	"crypto/rand"
	"golang.org/x/crypto/nacl/secretbox"
	"golang.org/x/crypto/scrypt"

	"encoding/binary"
	"log"
	"net"
	"time"
	"github.com/golang/protobuf/proto"
	"score/wire"
	"score"
)

func ListenTCP(port int, password string, Log *score.LogType, counter chan bool) {
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
				conn.Close()
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
				// Blocking writes only occur when the write buffer is full.
				// Linux has a default of 16KB, which should be enough for a
				// whole contest after setup. Kill everybody who doesn't
				// respond for bufferSize + 10s (clients should reconnect
				// automatically).
				conn.SetWriteDeadline(time.Now().Add(10*time.Second)) //
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
			conn.SetWriteDeadline(time.Now().Add(1*time.Second))
			_, err = conn.Write(append(nonce[8:], salt...))
			if(err != nil) {
				conn.Close()
				return
			}

			messages := make(chan *wire.Message)
			closing := make(chan bool)
			defer close(closing)
			defer func() {
				Log.Lock.Lock()
				Log.Cond.Broadcast()
				Log.Lock.Unlock()
			}()

			go func() {
				Version := int64(-1)
				ctr := int(0)
				outer:
				for {
					Log.Lock.Lock()
					for Log.Version == Version && len(Log.Msgs) == ctr {
						select {
							case _ = <-closing:
								Log.Lock.Unlock()
								break outer
							default:
						}
						Log.Cond.Wait()
					}
					if(Log.Version != Version) {
						ctr = 0
						Version = Log.Version
					}
					toSend := Log.Msgs
					Log.Lock.Unlock()
					for ctr != len(toSend) {
						select {
						case _ = <-closing:
							break outer
						case messages <- toSend[ctr]:
							ctr++
						}
					}
				}
			}()

			cc := make(chan error,1)
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
				select {
				case _ = <-ticker.C:
					err := write(&wire.Message{
						MessageType: &wire.Message_HeartBeat{
							HeartBeat: beat,
						},
					})
					beat++
					if(err != nil) {
						log.Printf("sending heartbeat for %v failed %v", messages, err)
						break MessageLoop
					}
				case m := <-messages:
					if(m == nil) {
						continue
					}
					err := write(m)
					if(err != nil) {
						log.Printf("sending message for %v failed: %v", messages, err)
						break MessageLoop
					}
				case _ = <-cc:
					break MessageLoop
				}
			}
		}()
	}
}
