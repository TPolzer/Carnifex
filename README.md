# Carnifex

Carnifex is a software package to display a live scoreboard for ICPC style contests (with domjudge).
It consists of two parts: The server regularly polls all relevant information from domjudge and puts them together in a subscribable network feed. The clients watch this feed and produce a nice anmiated scoreboard out of it.

This software is experimental. Do not use it on critical systems.
CHANGE THE SECRET PASSPHRASE before using it on a public network. After decryption input is treated as trusted.

You probably need:
- protobuf headers
- a golang toolchain
- a C++ toolchain
- Qt5 development files
- libsodium development files
- cmake

And at (client) runtime:
- libprotobuf
- libsodium
- libqt5 quick svg
- DejaVu Sans and Symbola for proper display

The setup currently involves:
```
  $ export GOPATH="`pwd`"
  $ go get github.com/golang/protobuf/proto github.com/rthornton128/goncurses golang.org/x/crypto/nacl/secretbox golang.org/x/crypto/scrypt
  $ go build server
  $ cmake src/client
  $ make -j4
```

To start it:
- put your credentials into credentials.json
- adjust the server url in config.json
- put a secret passphrase in config.json
- possibly adjust server name/api path/port in config.json (see config.README)
- start ./server
- start ./client (cannot be started before the server, but will try to
  reconnect if server is restarted/lost/etc.)

To use it:
- The resolving mechanism is very basic. It goes forward backward in resolving
  by left/right arrow keys on the server side (might want to keep it around in
  a screen/tmux).
- If the scoreboard is not in resolving phase it normally cycles through all
  pages periodically, you can pause this and manually move around with space,
  up and down arrow keys.
