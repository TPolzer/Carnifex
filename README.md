# Scory McScoreFace

Scory McScoreFace is a software package to display a live scoreboard for ICPC style contests (with domjudge).
It consists of two parts: The server regularly polls all relevant information from domjudge and puts them together in a subscribable network feed. The clients watch this feed and produce a nice anmiated scoreboard out of it.

This software is unfinished, use at your own peril!

You probably need:
- protobuf headers
- a golang toolchain
- a C++ toolchain
- Qt5 development files
- libsodium development files
- cmake
- DejaVu Sans and Symbola for proper display

The setup currently involves:
- in the top level directory:
```
  $ export GOPATH="`pwd`"
  $ go build server
```
- in the src/client subdirectory
```
  $ cmake .
  $ make -j4
```

To use it:
- put your credentials into credentials.json
- adjust the server url in config.json
- put a secret passphrase in config.json for both server and client (the whole file can be shared between both)
- possibly adjust server name / port in config.json
- start the server
- start the client
