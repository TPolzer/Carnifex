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
