import QtQuick 2.5
import QtQuick.Window 2.2
import "helper.js" as H

Window {
    visible: true
    id: contest
	color: 'black'

	property var em: height/30
	property var rs: em*1.2
    property var problems: []
	property var n: problems.length
    property var teams: []
	Team {
		id: dummyTeam
		submits: H.repeat(n, 0)
		pending: submits
		correct: submits
		penalties: submits
	}

    function contestSetup(problems, teams) {
        contest.problems = problems
        contest.teams = teams
    }
	function event(event) {
		console.print(event)
	}

    Rectangle {
		id: scoreboard
		anchors.fill: parent
		anchors.margins: em
		radius: em/2
		color: 'steelblue'
		Item {
			id: scoreboardContents
			anchors.fill: parent
			anchors.leftMargin: em/2
			anchors.rightMargin: em/2
			anchors.topMargin: em/3
			anchors.bottomMargin: em/3
			Rectangle {
				id: tableHead
				color: Qt.rgba(0,0,0,0)
				height: rs
				anchors.left: parent.left
				anchors.right: parent.right
				Row_t {
					visible: false
					id: columnPrototype
					height: rs
					anchors.left: parent.left
					anchors.right: parent.right
					team: dummyTeam
				}
				Repeater {
					model: columnPrototype.cols
					ScoreText {
						text: modelData.columnTitle ? modelData.columnTitle : ""
						width: modelData.width
						x: modelData.parent.x, columnPrototype.mapFromItem(modelData.parent, modelData.x, modelData.y).x // ew... hack to get triggers one level up
						horizontalAlignment: modelData.horizontalAlignment
					}
				}
			}
			Item {
				anchors.left: parent.left
				anchors.right: parent.right
				anchors.bottom: parent.bottom
				anchors.top: tableHead.bottom
                clip: true
                Repeater {
                    model: teams
                    Row_t {
						team: modelData
                        y: modelData.pos*rs
                        height: rs
                        anchors.left: parent.left
                        anchors.right: parent.right
                        Behavior on y { SmoothedAnimation { duration: 1500; velocity: -1 } }
                    }
                }
			}
		}
	}
}
