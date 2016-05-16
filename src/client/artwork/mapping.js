function coats() {
	var m = {   
		'Universität des Saarlandes': 'artwork/Wappen_des_Saarlands.svgz',
		'FAU Erlangen-Nürnberg': 'artwork/Wappen_Erlangen.svgz',
		'Reykjavik University': 'artwork/ISL_Reykjavik_COA.svgz',
		'Georg-August-Universität Göttingen': 'artwork/DEU_Goettingen_COA.svgz',
		'TU München': 'artwork/Muenchen_Kleines_Stadtwappen.svgz',
		'Karlsruher Institut für Technologie': 'artwork/Coat_of_arms_de-bw_Karlsruhe.svgz',
	};
	return function (aff) {
		var res = m[aff];
		if(res != undefined) return res;
		return '';
	}
}
