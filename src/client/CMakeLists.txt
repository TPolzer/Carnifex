cmake_minimum_required(VERSION 3.0)

project(client)

set(CMAKE_INCLUDE_CURRENT_DIR ON)
set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

find_package(Qt5 REQUIRED COMPONENTS Qml Quick Core Network)
find_package(Protobuf REQUIRED)
find_package(Qt5Widgets)

# Tell CMake to create the helloworld executable
add_executable(client main.cpp ScoreboardClient.cpp qml.qrc scoreboard.pb.cc)
target_link_libraries(client Qt5::Widgets Qt5::Quick)
target_link_libraries(client ${PROTOBUF_LIBRARIES})
# set_property(TARGET client PROPERTY CXX_STANDARD 11)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
