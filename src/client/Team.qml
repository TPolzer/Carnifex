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
    property var name: ""
    property var rank: ""
    property int pos: 0
    property var submits: []
    property var pending: []
    property var correct: []
    property var penalties: []
    property var first: []
    property var solved: correct.reduce(H.add, 0)
    property int penalty: penalties.reduce(H.add, 0)
    property int firsts: first.reduce(H.add, 0)
}
