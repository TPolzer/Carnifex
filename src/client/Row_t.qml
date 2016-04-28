import QtQuick 2.0

Item {
	property var rank: 42
	property var teamname: "Dummer Teamname"
	Text {
		text: rank
	}
	Text {
		text: teamname
	}
	Row {
		x: 100
		spacing: 10
		Repeater {
			model: contest.problems
			Rectangle {
				width: child.width
				height: child.height
				color: Qt.rgba(0,0,0,0)	
				Text {
					   id: child
					   text: contest.problems[index]
                       font.pointSize: 30
                       anchors.centerIn: parent }
			}
		}
	}
}
