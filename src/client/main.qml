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
import QtQuick.Window 2.0
import "helper.js" as H

Window {
    visible: true
    id: contest
	color: 'black'

	property double em: height/30
	property double rs: em*1.2
    property var problems: []
	property int n: problems.length
    property var teams: []
	property string name: ""
	property point focused: "-1,-1"
	Team {
		id: dummyTeam
		submits: H.repeat(n, 0)
		pending: submits
		correct: submits
		penalties: submits
	}

    function contestSetup(contestDesc, problems, teams) {
        contest.problems = problems
        contest.teams = teams
		contest.name = contestDesc.name
    }

	ScoreText {
		id: title
		text: name
		anchors.left: parent.left
		anchors.margins: em
	}

    Rectangle {
		id: scoreboard
		anchors.fill: parent
		anchors.margins: title.height
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
						x: H.walkUpX(columnPrototype, modelData)
						horizontalAlignment: modelData.horizontalAlignment
					}
				}
			}
			Item {
				id: table
				anchors.left: parent.left
				anchors.right: parent.right
				anchors.bottom: parent.bottom
				anchors.top: tableHead.bottom
                clip: true
				Item {
					width: parent.width
					height: rs*teams.length
					id: tableContents
					property int pages: Math.ceil(teams.length/Math.floor(table.height/rs))
					property int page: (contest.focused.y == -1) ? autopage : teams[contest.focused.y].pos/Math.floor(table.height/rs)
					property int autopage: 0
					y: rs ? -page*rs*Math.floor(table.height/rs) : 0
					Behavior on y { SmoothedAnimation {duration: 800; velocity: -1} }
					Repeater {
						model: teams
						Row_t {
							team: modelData
							focused: (contest.focused.y == index) ? contest.focused.x : 0
							y: modelData.pos*rs
							height: rs
							anchors.left: parent.left
							anchors.right: parent.right
							Behavior on y { SmoothedAnimation { duration: 1500; velocity: -1 } }
						}
					}
					Timer {
						id: pageTimer
						interval: 10000; running: true; repeat: true
						onTriggered: {
							tableContents.autopage = (tableContents.autopage-1+tableContents.pages)
							tableContents.autopage %= tableContents.pages
						}
					}
				}
			}
		}
	}
}
