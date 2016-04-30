import QtQuick 2.0
import "helper.js" as H

Item {
	property var team
	Row {
		height: parent.height
        ScoreText {
            anchors.verticalCenter: parent.verticalCenter
            text: team.rank
            width: tableHead.columnSize[0]
            horizontalAlignment: tableHead.columnAlignment[0]
        }
        ScoreText {
            anchors.verticalCenter: parent.verticalCenter
            text: team.name
            width: tableHead.columnSize[1]
            horizontalAlignment: tableHead.columnAlignment[1]
			clip: true
        }
        Repeater {
            model: contest.problems
            Item {
                width: tableHead.columnSize[index+2]
                height: parent.height
                Rectangle {
                    anchors.verticalCenter: parent.verticalCenter
                    anchors.horizontalCenter: parent.horizontalCenter
                    color: team.correct[index] ? 'green' : team.pending[index] ? 'blue' : team.submits[index] ? 'red' : Qt.rgba(0,0,0,0)
                    radius: contest.em/4
                    height: contest.em
                    width: parent.width*0.9
                    ScoreText {
                        anchors.horizontalCenter: parent.horizontalCenter
                        anchors.verticalCenter: parent.verticalCenter
                        horizontalAlignment: tableHead.columnAlignment[index+2]
                        text: team.submits[index]?team.submits[index]:''
                    }
                }
            }
        }
        ScoreText {
            anchors.verticalCenter: parent.verticalCenter
            text: team.solved
            width: tableHead.columnSize[tableHead.columnSize.length-2]
            horizontalAlignment: tableHead.columnAlignment[tableHead.columnSize.length-2]
        }
        ScoreText {
            anchors.verticalCenter: parent.verticalCenter
            text: team.penalty
            width: tableHead.columnSize[tableHead.columnSize.length-1]
            horizontalAlignment: tableHead.columnAlignment[tableHead.columnSize.length-1]
        }
	}
}
