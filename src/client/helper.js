/*
 * This file is part of "Carnifex"
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
function repeat(n, v) {
	return Array.apply(null, new Array(n)).map(function(){return v})
}

function asArray(v) {
	var arr = [];
	for(var i=0; i<v.length; i++) {
		arr.push(v[i]);
	}
	return arr;
}

function maxArray(a) {
	var res = -Infinity;
	for(var x in a) {
		if(a[x] === null) continue;
		res = Math.max(res,a[x]);
	}
	return res;
}

function repArray(rep) {
	var arr = [];
	for(var i=0; i<rep.count; i++) {
		arr.push(rep.itemAt(i));
	}
	return arr;
}

function add(a, b) {
        return a + b;
}

function walkUpX(p, c) {
	var x = 0;
	while(p !== c) {
		x += c.x;
		c = c.parent;
	}
	return x;
}
