import QtQuick 2.0
import "helper.js" as H

Item {
	property var rank
	property var name
    property var correct
    property var penalty
    property var pending
    property var submits
	Row {
        ScoreText {
            anchors.verticalCenter: parent.verticalCenter
            text: rank
            width: tableHead.columnSize[0]
            horizontalAlignment: tableHead.columnAlignment[0]
        }
        ScoreText {
            anchors.verticalCenter: parent.verticalCenter
            text: name
            width: tableHead.columnSize[1]
            horizontalAlignment: tableHead.columnAlignment[1]
        }
        Repeater {
            model: contest.problems
            Item {
                width: tableHead.columnSize[index+2]
                height: parent.height
                Rectangle {
                    anchors.verticalCenter: parent.verticalCenter
                    anchors.horizontalCenter: parent.horizontalCenter
                    color: correct[index] ? 'green' : pending[index] ? 'blue' : submits[index] ? 'red' : Qt.rgba(0,0,0,0)
                    radius: contest.em/4
                    height: contest.em
                    width: parent.width*0.9
                    ScoreText {
                        anchors.horizontalCenter: parent.horizontalCenter
                        anchors.verticalCenter: parent.verticalCenter
                        horizontalAlignment: tableHead.columnAlignment[index+2]
                        text: submits[index]?submits[index]:''
                    }
                }
            }
        }
        ScoreText {
            anchors.verticalCenter: parent.verticalCenter
            text: correct.reduce(H.add, 0)
            width: tableHead.columnSize[tableHead.columnSize.length-2]
            horizontalAlignment: tableHead.columnAlignment[tableHead.columnSize.length-2]
        }
        ScoreText {
            anchors.verticalCenter: parent.verticalCenter
            text: penalty
            width: tableHead.columnSize[tableHead.columnSize.length-1]
            horizontalAlignment: tableHead.columnAlignment[tableHead.columnSize.length-1]
        }
	}
}
