import QtQuick 2.3
import "helper.js" as H

Item {
	property var team
	property var cols: H.asArray(children).concat(H.asArray(row.children))
	ScoreText {
		property var columnTitle: "Rank"
		anchors.verticalCenter: parent.verticalCenter
		anchors.left: parent.left
		id: rank
		text: team.rank
		width: 3*em
		horizontalAlignment: Text.AlignLeft
	}
	ScoreText {
		property var columnTitle: "Team"
		anchors.verticalCenter: parent.verticalCenter
		text: team.name
		id: teamname
		anchors.left: rank.right
		anchors.right: row.left
		horizontalAlignment: Text.AlignLeft
		clip: true
	}
	Row {
		id: row
		property var columnTitle: ""
		anchors.verticalCenter: parent.verticalCenter
		anchors.right: solved.left
		height: parent.height
		Repeater {
			model: contest.problems
			Item {
				property var columnTitle: contest.problems[index]
				property var horizontalAlignment: Text.AlignHCenter
				width: 1.5*em
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
						horizontalAlignment: Text.AlignHCenter
						text: team.submits[index]?team.submits[index]:''
					}
				}
			}
		}
	}
	ScoreText {
		property var columnTitle: "Solved"
		anchors.verticalCenter: parent.verticalCenter
		anchors.right: time.left
		id: solved
		text: team.solved
		width: 4*em
		horizontalAlignment: Text.AlignRight
	}
	ScoreText {
		property var columnTitle: "Time"
		anchors.verticalCenter: parent.verticalCenter
		anchors.right: parent.right
		id: time
		text: team.penalty
		width: 4*em
		horizontalAlignment: Text.AlignRight
	}
}
