function coats() {
	var m = {   
		'Universität des Saarlandes': 'artwork/Wappen_des_Saarlands.svgz',
		'FAU Erlangen-Nürnberg': 'artwork/Wappen_Erlangen.svgz',
		'Friedrich-Alexander-Universität Erlangen-Nürnberg': 'artwork/Wappen_Erlangen.svgz',
		'Reykjavik University': 'artwork/ISL_Reykjavik_COA.svgz',
		'Georg-August-Universität Göttingen': 'artwork/DEU_Goettingen_COA.svgz',
		'TU München': 'artwork/Muenchen_Kleines_Stadtwappen.svgz',
		'Karlsruher Institut für Technologie': 'artwork/Coat_of_arms_de-bw_Karlsruhe.svgz',
		'Rheinische Friedrich-Wilhelms-Universit\u00e4t Bonn': 'artwork/DEU_Bonn_COA.svgz',
		'Technische Universit\u00e4t Dortmund': 'artwork/Coat_of_arms_of_Dortmund.svgz',
		'Hochschule f\u00fcr angewandte Wissenschaften Landshut': 'artwork/Wappen_Landshut.svgz',
		'Frankfurt University of Applied Sciences': 'artwork/Wappen_Frankfurt_am_Main.svgz',
		'Universit\u00e4t Ulm': 'artwork/Coat_of_arms_of_Ulm.svgz',
		'Johannes Gutenberg-Universit\u00e4t Mainz': 'artwork/Coat_of_arms_of_Mainz-2008_new.svgz',
		'Julius-Maximilians-Universit\u00e4t W\u00fcrzburg': 'artwork/Wappen_von_Wuerzburg.svgz',
		'Universit\u00e4t zu L\u00fcbeck': 'artwork/Wappen_Lübeck.svgz',
		'Universit\u00e4t Osnabr\u00fcck': 'artwork/Osnabrück_Wappen.svgz',
	};
	return function (aff) {
		var res = m[aff];
		if(res != undefined) return res;
		return '';
	}
}
