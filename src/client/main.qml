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
import de.bulsa.clock 0.1
import "helper.js" as H
import "artwork/mapping.js" as C

Window {
    visible: true
    id: contest
	color: 'black'

	property var config: new Object()
	property double em: 10
	property double layoutEm: 10
	onHeightChanged: recalcEm()
	onWidthChanged: recalcEm()
	function recalcEm() {
		var ub = 1000, lb = 0;
		while(ub - lb > 1e-9) {
			var mid = ub/2 + lb/2;
			layoutEm = mid;
			var sign;
			var sign1 = Math.max(0,table.height)/layoutRs - config.minrows;
			var sign2 = Math.max(0,table.width)/layoutRs - config.mincols;
			if(sign1 < 0 || sign2 < 0)
				sign = -1;
			else
				sign = 1;
			if(sign < 0) {
				ub = mid;
			} else {
				lb = mid;
			}
		}
		em = layoutEm;
	}
	property double rs: em*1.2
	property double layoutRs: layoutEm*1.2
    property var problems: []
	property int n: problems.length
    property var teams: []
	property string name: ""
	property point focused: "-1,-1"
	property var start: new Date().valueOf()
	property var coats: C.coats()

	Team {
		id: dummyTeam
		submits: H.repeat(n, 0)
		pending: submits
		correct: submits
		penalties: submits
		affiliation: 'FAU Erlangen-Nürnberg'
	}

	function configure(c) {
		config = c
		onHeightChanged(height); // trigger row/column count
	}

    function contestSetup(contestDesc, problems, teams) {
        contest.problems = problems
        contest.teams = teams
		contest.name = contestDesc.name
		contest.start = contestDesc.start
		onHeightChanged(height); // trigger row/column count (title height could have changed)
    }

	ScoreText {
		id: title
		text: name
		anchors.left: parent.left
		anchors.margins: layoutEm
		font.pixelSize: layoutEm
	}
	ScoreText {
        id: clockDisplay
		text: sign + clock.formatUTCTime(new Date(Math.abs(sinceStart)), "hh':'mm':'ss'.'zzz").substr(0,10)
        property var sinceStart: clock.time - reference
        property var sign: (sinceStart < 0) ? '-' : ''
        property var reference: contest.start
		anchors.right: parent.right
		anchors.margins: em
        Clock {
            id: clock
            interval: 100
            offset: clockDisplay.reference%interval
        }
	}

    Rectangle {
		id: scoreboard
		anchors.fill: parent
		anchors.margins: title.height
		radius: layoutEm/2
		color: '#28B2FF'
		Item {
			id: scoreboardContents
			anchors.fill: parent
			anchors.leftMargin: layoutEm/2
			anchors.rightMargin: layoutEm/2
			anchors.topMargin: layoutEm/3
			anchors.bottomMargin: layoutEm/3
			Rectangle {
				id: tableHead
				color: Qt.rgba(0,0,0,0)
				height: layoutRs
				anchors.left: parent.left
				anchors.right: parent.right
				Row_t {
					visible: false
					id: columnPrototype
					height: layoutRs
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
					property int perPage: Math.max(config.minrows, Math.floor(table.height/rs)); // avoid precision problems with exactly minrows
					property int pages: Math.ceil(teams.length/perPage)
					property int page: (contest.focused.y == -1) ? autopage : teams[contest.focused.y].pos/perPage
					property int autopage: 0
					y: rs ? -page*rs*perPage : 0
					Behavior on y { SmoothedAnimation {duration: 800; velocity: -1} }
					Repeater {
						model: columnPrototype.cols
                        Rectangle {
                            color: Qt.rgba(0,0,0,0.1)
							height: parent ? parent.pages*table.height : 0
                            width: modelData.width
                            x: H.walkUpX(columnPrototype, modelData)
                            visible: (index+1)%2
                        }
                    }
					Repeater {
						model: teams
                        Rectangle {
                            color: Qt.rgba(0,0,0,0.1)
							height: rs
                            visible: index%2
							anchors.left: parent ? parent.left : undefined
							anchors.right: parent ? parent.right : undefined
							y: index*rs
                        }
                    }
					Repeater {
						model: teams
						Row_t {
							team: modelData
							focused: (contest.focused.y == index) ? contest.focused.x : 0
							y: modelData.pos*rs
							height: rs
							anchors.left: parent ? parent.left : undefined
							anchors.right: parent ? parent.right : undefined
							Behavior on y { SmoothedAnimation { duration: 1500; velocity: -1 } }
						}
					}
					Timer {
						id: pageTimer
						interval: (config.pageInterval === undefined) ? 10000 : config.pageInterval*1000;
						running: true; repeat: true
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
