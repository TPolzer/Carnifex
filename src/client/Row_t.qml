/*
 * This file is part of "The Scoreboard"
 * Copyright (C) 2016  Tobias Polzer

 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */
import QtQuick 2.0
import "helper.js" as H

Item {
	property var team
	property var cols: [rank, teamname].concat(H.repArray(problems)).concat([solved, time])
	property int focused: -2
	Rectangle {
		anchors.fill: parent
		border.color: 'gold'
		border.width: em/8
		radius: em/4
		visible: focused != -2
		color: Qt.rgba(0,0,0,0)
	}
	ScoreText {
		property var columnTitle: "Rank"
		anchors.verticalCenter: parent.verticalCenter
		anchors.left: parent.left
		id: rank
		text: team.rank
		width: 3*em
		horizontalAlignment: Text.AlignLeft
	}
	Image {
		anchors.left: rank.right
		id: coat
		width: height
		anchors.top: parent.top
		anchors.bottom: parent.bottom
		anchors.margins: 0.1*em
		fillMode: Image.PreserveAspectFit
		source: contest.coats[team.affiliation]
	}
	ScoreText {
		anchors.leftMargin: 0.1*em
		property var columnTitle: "Team"
		anchors.verticalCenter: parent.verticalCenter
		text: team.name
		id: teamname
		anchors.left: coat.right
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
            id: problems
			model: contest.problems
			Item {
				property var columnTitle: contest.problems[index]
				property var horizontalAlignment: Text.AlignHCenter
				width: 1.5*em
				height: parent.height
				Rectangle {
					anchors.verticalCenter: parent.verticalCenter
					anchors.horizontalCenter: parent.horizontalCenter
					color: team.first[index] ? 'darkgreen' : team.correct[index] ? '#20E85C' : team.pending[index] ? 'blue' : team.submits[index] ? '#FF3860' : Qt.rgba(0,0,0,0)
					radius: contest.em/4
					border.color: 'gold'
					border.width: (focused == index) ? em/8 : 0
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
		width: 3.5*em
		horizontalAlignment: Text.AlignRight
	}
	ScoreText {
		property var columnTitle: "Time"
		anchors.verticalCenter: parent.verticalCenter
		anchors.right: parent.right
		id: time
		text: team.penalty
		width: 3.5*em
		horizontalAlignment: Text.AlignRight
	}
}
