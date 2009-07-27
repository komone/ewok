// Ewok ESP Functions for JQuery
// Version 1.0.0 (JQuery: 1.3.2)
// Copyright (c) 2009 Simulacity.com. All rights reserved.

// maybe
var esp_queue = new Array();
var esp_busy = false;

//
function esp_postback(params) {
	esp_spinner(true);
	$.ajax({ 
		url: document.location.href,
		type: 'post',
		data: ubf_encode(params),
		dataType: 'text', // ubf isn't explicitly supported in JQuery
		contentType: ubf_mimetype,
		success: function(data, textStatus) {
			esp_spinner(false);
			try {
				//alert("SUCCESS: " + transport.responseText);
				eval(data);
			} catch (E) {
				alert("JAVASCRIPT ERROR: " + data);
				alert(E);
			}
			esp_busy = false;
		},
		error: function(xmlHttpRequest, textStatus, errorThrown) {
			alert("FAIL: " + textStatus);
			esp_busy = false;
		}
	});
}

//
function esp_spinner(var enable) {
	var spinner = obj('spinner');
	if (spinner) {
		if (enable) {
			new Effect.Fade(spinner, { duration: 1.0 });
		} else {
			spinner.show();
		}
	}
}

//
// ewwww
//
function obj(path) {
	path = wf_normalize_partial_path(path);
	
	// Try the easy option...
	var el = document.getElementById(path);
	if (el) return el;
	
	// Not found, so scan recursively...
	return wf_scan_elements(path, document.childNodes);
}

function wf_scan_elements(path, elements) {
	if (!elements) return;
	
	for (var i=0; i<elements.length; i++) {
		var t = elements[i].id;
		if (t == undefined) continue;
		var pos = t.indexOf(path);
		if (pos == -1) continue;
		if (t.indexOf(path) + path.length == t.length) {
			return elements[i];
		}
	}
	
	for (var i=0; i<elements.length; i++) {
		var el = wf_scan_elements(path, elements[i].childNodes)
		if (el) return el;
	}

	return null;
}

function wf_normalize_partial_path(path) {
	var oldparts = wf_current_path.split(".");
	var newparts = path.split(".");
	var a = new Array();
	for (var i=0; i<newparts.length; i++) {
		var part = newparts[i];
		if (part == "me") a = oldparts;
		else if (part == "parent") a.pop();
		else a.push(part);
	}
	
	return a.join("__");
}
