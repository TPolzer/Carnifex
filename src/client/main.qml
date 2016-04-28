import QtQuick 2.5
import QtQuick.Window 2.2

Window {
    visible: true
    id: contest
	color: 'black'

	property var em: height/30
	property var rs: em*1.2
    property var problems: ["A", "B", "C"]
	property var headings: ["Rank", "Team"].concat(problems).concat(["Solved", "Time"])
	property var problemIds: [42, 12, 3]

    Rectangle {
		property var columnSize: [4*em, width-13*em-2*problems.length*em].concat(Array.apply(null, problems).map(function(){return 2*em})).concat([4*em, 6*em])
		id: scoreboard
		anchors.fill: parent
		anchors.margins: em
		radius: em/2
		color: 'steelblue'
		Rectangle {
			id: tableHead
			color: Qt.rgba(0,0,0,0)
			height: rs
			anchors.left: parent.left
			anchors.right: parent.right
			Row {
				Repeater {
					model: headings
					Text {
						text: modelData
						font.pixelSize: em
						width: scoreboard.columnSize[index]
						horizontalAlignment: Text.AlignHCenter
						color: 'white'
					}
				}
			}
		}
		Rectangle {
			anchors.left: parent.left
			anchors.right: parent.right
			anchors.bottom: parent.bottom
			anchors.top: tableHead.bottom
			color: Qt.rgba(0,0,0,0)
			Row_t {
			}
		}
    }
}
