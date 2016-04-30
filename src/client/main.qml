import QtQuick 2.5
import QtQuick.Window 2.2
import "helper.js" as H

Window {
    visible: true
    id: contest
	color: 'black'

	property var em: height/30
	property var rs: em*1.2
    property var problems: ["A", "B", "C"]
	property var n: problems.length
	property var headings: ["Rank" , "Team"].concat(problems).concat(["Solved", "Time"])
    property var teams: []

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
			anchors.fill: parent
			anchors.leftMargin: em/2
			anchors.rightMargin: em/2
			anchors.topMargin: em/3
			anchors.bottomMargin: em/3
			Rectangle {
				property var columnSize: [3*em, width-11*em-n*1.5*em].concat(H.repeat(n, 1.5*em)).concat([3*em, 5*em])
				property var columnAlignment: [Text.AlignLeft, Text.AlignLeft].concat(H.repeat(n+1, Text.AlignHCenter)).concat([Text.AlignRight])
				id: tableHead
				color: Qt.rgba(0,0,0,0)
				height: rs
				anchors.left: parent.left
				anchors.right: parent.right
				Row {
					Repeater {
						model: headings
						ScoreText {
							text: modelData
							width: tableHead.columnSize[index]
							horizontalAlignment: tableHead.columnAlignment[index]
						}
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
                        y: modelData.pos*rs
                        height: rs
                        anchors.left: parent.left
                        anchors.right: parent.right
                        rank: modelData.rank
                        name: modelData.name
                        correct: modelData.correct
                        pending: modelData.pending
                        submits: modelData.submits
                        penalty: modelData.penalty
                        solved: modelData.solved
                        Behavior on y { SmoothedAnimation { duration: 1500; velocity: -1 } }
                    }
                }
			}
		}
	}
}
