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
	property var headings: ["Rank", "Team"].concat(problems).concat(["Solved", "Time"])
	property var problemIds: [42, 12, 3]
    property var teams: H.dummyTeams()

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
				property var columnSize: [4*em, width-14*em-n*2*em].concat(H.repeat(n, 2*em)).concat([4*em, 6*em])
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
                        y: (rank-1)*rs
                        height: rs
                        anchors.left: parent.left
                        anchors.right: parent.right
                        rank: modelData.rank
                        name: modelData.name
                        correct: modelData.correct
                        penalty: modelData.penalty
                    }
                }
			}
		}
	}
}
