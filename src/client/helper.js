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
