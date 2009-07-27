// Ewok UBF Codec 
// Version 1.0.0
// Copyright (c) 2009 Simulacity.com. All rights reserved.
// UBF specification by Joe Armstrong 
// <http://www.sics.se/~joe/ubf/site/home.html>

//
var ubf_mimetype = "application/x-ubf";

//
function ubf_encode(params) {
	var ubf = "#";
	for (var i = 0; i < params.length; i++) {
		alert(params[i]);
		if (params[i].length == 2) {
			ubf += ubf_encode_param(params[i][0], params[i][1]);
			ubf += "&";
		} else {
			throw "Invalid parameter " + params[i];
		}
	}
	ubf += "$";
	return ubf;
}

//
function ubf_encode_param(key, value) {
	return "{" + ubf_encode_value(key) + "," 
		+ ubf_encode_value(value) + "}";
}

//
function ubf_encode_value(value) {
	return "\"" + value + "\"";
}

//
function ubf_decode(ubf) {
	false;
}
