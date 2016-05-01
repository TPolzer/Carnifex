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

	property var em: height/30
	property var rs: em*1.2
    property var problems: []
	property var n: problems.length
    property var teams: []
	property var name: ""
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
