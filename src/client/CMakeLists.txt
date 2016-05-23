cmake_minimum_required(VERSION 3.0)

project(client)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

find_package(Qt5 REQUIRED COMPONENTS Qml Quick Core Network)
find_package(Qt5Widgets)

find_package(Protobuf REQUIRED)

PROTOBUF_GENERATE_CPP(PROTO_SRCS PROTO_HDRS ../score/wire/scoreboard.proto)

find_package(PkgConfig REQUIRED)
pkg_search_module(SODIUM REQUIRED libsodium)

# Tell CMake to create the helloworld executable
add_executable(client main.cpp ScoreboardClient.cpp qml.qrc scoreboard.pb.cc clock.cc qmlproto.cpp artwork/artwork.qrc ${PROTO_SRCS})

target_link_libraries(client Qt5::Widgets Qt5::Quick)
target_link_libraries(client ${PROTOBUF_LIBRARIES})
target_link_libraries(client ${SODIUM_LIBRARIES})

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${SODIUM_CFLAGS} -std=c++11 -Wall -Wextra")
set(CMAKE_CXX_FLAGS_DEBUG "-DQT_QML_DEBUG")
