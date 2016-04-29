function repeat(n, v) {
	return Array.apply(null, new Array(n)).map(function(){return v})
}

function dummyTeams() {
	return [ { name: 'dummer Teamname',
        penalty: 110,
        rank: 1,
		submits: [ 0, 1, 42 ],
		pending: [ 0, 0, 1 ],
		correct: [ 0, 1, 0 ] },
	  { name: 'dümmerer Teamname',
        penalty: 20,
        rank: 2,
		submits: [ 1, 1, 1 ],
		pending: [ 0, 0, 1 ],
		correct: [ 0, 1, 0 ] } ];
}

function add(a, b) {
        return a + b;
}
