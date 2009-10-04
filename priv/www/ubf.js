/*
	UBF(A) codec
	Version: 1.0 alpha
	Specification: <http://www.sics.se/~joe/ubf/site/home.html>
	Modified from: http://www.JSON.org/json2.js (2009-06-29)
    License: Public Domain.
    NO WARRANTY EXPRESSED OR IMPLIED. USE AT YOUR OWN RISK.
*/
/*
STATUS INCOMPLETE:
	1) Allows invalid escape sequences in strings (\f\t\r...) 	
	2) No binary support
	3) Does not account for UBF Register pushes
*/
/*
Usage:
    UBF.encode(JSObject) -> UBF.
    UBF.decode(UBF) -> JSObject.
*/
var UBF = UBF || {};
(function () {
	function f(n) {
	    return n < 10 ? '0' + n : n;
	}
	if (typeof Date.prototype.toUBF !== 'function') {
	    Date.prototype.toUBF = function (key) {
	        return isFinite(this.valueOf()) ?
	               this.getUTCFullYear()   + '-' +
	             f(this.getUTCMonth() + 1) + '-' +
	             f(this.getUTCDate())      + 'T' +
	             f(this.getUTCHours())     + ':' +
	             f(this.getUTCMinutes())   + ':' +
	             f(this.getUTCSeconds())   + 'Z' : null;
	    };
	    String.prototype.toUBF =
	    Number.prototype.toUBF =
	    Boolean.prototype.toUBF = function (key) {
	        return this.valueOf();
	    };
	}
	var cx = /[\u0000\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g;
	var escapable = /[\\\"\x00-\x1f\x7f-\x9f\u00ad\u0600-\u0604\u070f\u17b4\u17b5\u200c-\u200f\u2028-\u202f\u2060-\u206f\ufeff\ufff0-\uffff]/g;
	var meta = {    // table of character substitutions
        '\b': '\\b',
        '\t': '\\t',
        '\n': '\\n',
        '\f': '\\f',
        '\r': '\\r',
        '"' : '\\"',
        '\\': '\\\\'
    };
	var rep;
	
	function quote(string) {
	    escapable.lastIndex = 0;
	    return escapable.test(string) ?
	        '"' + string.replace(escapable, function (a) {
	            var c = meta[a];
	            return typeof c === 'string' ? c :
	                '\\u' + ('0000' + a.charCodeAt(0).toString(16)).slice(-4);
	        }) + '"' :
	        // '~"' + string + '"~`unicode`';
	        '"' + string + '"';
	}
	
	// ENCODER
	UBF.encode = function (value) {
		//
	    function to_string(key, holder) {
			var i, k, v, length;
			var partial;
			var value = holder[key];
			if (value && typeof value === 'object' 
					&& typeof value.toUBF === 'function') {
			    value = value.toUBF(key);
			}
			switch (typeof value) {
			case 'string':
			    return quote(value); // String(value.length) + quote(value);
			case 'number':
			    return isFinite(value) ? String(value) : 'infinity';
			case 'boolean':
				return '\'' + value + '\'';
			case 'null':
			    return String(value);
			case 'object':
				if (!value) {
				    return 'null';
				}
				partial = [];
				if (Object.prototype.toString.apply(value) === '[object Array]') {
				    length = value.length;
				    for (i = 0; i < length; i += 1) {
				        partial[i] = to_string(i, value) || 'null';
				    }
				    v = partial.length === 0 ? '#&' : '#' + partial.join('&') + '&';
				    return v;
				}
				for (k in value) {
				    if (Object.hasOwnProperty.call(value, k)) {
				        v = to_string(k, value);
				        if (v) {
				    		//alert(k + "," + v);
				            partial.push('{\'' + String(k) + '\',' + v + '}');
				        }
				    }
				}
				v = partial.length === 0 ? '#&$' : '#' + partial.join('&') + '&';
				return v;
			}
		}
		return to_string('', {'': value}) + '$';
	};
		
	// DECODER
	UBF.decode = function (text, reviver) { 
		var j;
		function walk(holder, key) {
			var k, v;
			value = holder[key];
			if (value && typeof value === 'object') {
			    for (k in value) {
			        if (Object.hasOwnProperty.call(value, k)) {
			            v = walk(value, k);
			            if (v !== undefined) {
			                value[k] = v;
			            } else {
			                delete value[k];
			            }
			        }
			    }
			}
			return reviver.call(holder, key, value);
		}
		var js = text.split(/("[^"]*"|'|,|#|&|{|})/);
		text = "";
		for (i = 0; i < js.length; i++) {
			var val;
			switch (js[i]) {
				case "": continue;
				case "'": val = "\""; break;
				case "{": continue;
				case "}": continue;
				case "$": val = ""; break;
				case ",": val = ":"; break;
				case "&": 
					var n = 1;
					while(js[i + n] !== "$" && js[1 + n] === "") n++;
					if (js[i + n] === "}" || js[i + n] === "$") {
						val = "}";
					} else {
						val = ",";
					}
					break;
				case "#": val = "{"; break;
				default: val = js[i]; break;
			}
			text += val;
			// + "<br/>";
		}
		cx.lastIndex = 0;
		if (cx.test(text)) {
		    text = text.replace(cx, function (a) {
		        return '\\u' +
		            ('0000' + a.charCodeAt(0).toString(16)).slice(-4);
		    });
		}
		return(text);
        if (/^[\],:{}\s]*$/.test(
        	text.replace(/\\(?:["\\\/bfnrt]|u[0-9a-fA-F]{4})/g, '@')
        		.replace(/"[^"\\\n\r]*"|true|false|null|-?\d+(?:\.\d*)?(?:[eE][+\-]?\d+)?/g, ']')
				.replace(/(?:^|:|,)(?:\s*\[)+/g, ''))) {
	    	j = eval('(' + text + ')');
		    return typeof reviver === 'function' ? walk({'': j}, '') : j;
		} else {
			return "error";
		}
		throw new SyntaxError('UBF.parse');
	};
}());
