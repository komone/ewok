// based on http://www.w3.org/TR/XMLHttpRequest/

var makeError = function() {
	var ErrorType = function() {
		Error.apply(this, arguments);
	};
	ErrorType.prototype = extend(new Error(), {
		'constructor': SecurityError
	});
	return ErrorType;
};

var SecurityError = makeError();
var NotSupportedError = makeError();
var InvalidStateError = makeError();
var NetworkError = makeError();


// TODO: Headers needs to append values for SOME keys... not sure which yet.
var Headers = prototypal({
	'constructor': function(headers) {
		this.clear();
	
		if (headers) {
			for (var key in headers) {
				this.set(key, headers[key]);
			}
		}
	},

	'set': function(key, value) {
		var lkey = key.toLowerCase();
		this.canonical[lkey] = value.toString();
		this.original[lkey] = key;
	},

	'get': function(key, def) {
		return this.has(key) ? this.canonical[key.toLowerCase()] : def;
	},

	'has': function(key) {
		return key.toLowerCase() in this.canonical;
	},

	'clear': function() {
		extend(this, {
			'canonical': {},
			'original': {}
		});
	},

	'toString': function() {
		var out = [];
		for (var key in this.canonical) {
			if (this.canonical.hasOwnProperty(key)) {
				out.push(this.original[key], ': ', this.canonical[value], '\r\n');
			}
		}
		return out.join('');
	},

	'toObject': function() {
		var headers = {};
		oeach.call(this, this.canonical, function(key, value) {
			headers[this.original[key]] = value;
		});
		return headers;
	}
});


var XMLHttpRequest = prototypal({
	'constructor': function() {
		this.requestHeaders = new Headers();
	},

	// recognized methods
	'METHODS': ['CONNECT', 'DELETE', 'GET', 'HEAD', 'OPTIONS', 'POST', 'PUT', 'TRACE', 'TRACK'],
	'METHOD_RE': /^CONNECT|DELETE|GET|HEAD|OPTIONS|POST|PUT|TRACE|TRACK$/i,
	// unsafe methods
	'UNSAFE_METHOD_RE': /^CONNECT|TRACE|TRACK$/,
	// allowed url schemes
	'SCHEME_RE': /^https?:/i,

	// states
	'UNSENT': 0,
	'OPENED': 1,
	'HEADERS_RECEIVED': 2,
	'LOADING': 3,
	'DONE': 4,

	// events

	'readyState': this.UNSENT,
	'onreadystatechange': function() {},

	// request

	'async': true,
	'user': null,
	'password': null,
	'sent': false,
	'error': false,

	'open': function(method, url, async, user, password) {
		this.method = method.match(this.METHOD_RE) ? method.toUpperCase() : method;

		if (this.method.match(this.UNSAFE_METHOD_RE)) {
			throw new SecurityError('unsafe method: ' + this.method);
		}
		
		// TODO: URL class for managing this better.
		// drop fragment
		this.url = url.split('#')[0];

		if (!this.url.match(this.SCHEME_RE)) {
			throw new NotSupportedError('unsupported scheme: ' + this.url.split(':')[0]);
		}
		
		if (arguments.length > 2) {
			this.async = async;
			if (arguments.length > 3) {
				this.user = user;
				if (arguments.length > 4) {
					this.password = password;
				}
			}
		}

		this.abort();
		this.requestHeaders.clear();
		extend(this, {
			'sent': false,
			'responseText': null,
			'responseXML': null,
			'readyState': this.OPENED
		});
		this.onreadystatechange();
	},

	'setRequestHeader': function(header, value) {
		if (this.readyState == this.OPENED) {
			if (!this.sent) {
				// TODO: check security-sensitive headers
				this.requestHeaders.set(header, value);
			} else {
				throw new InvalidStateError('send in progress');
			}
		} else {
			throw new InvalidStateError('XHR is not OPENED');
		}
	},

	'send': function(data) {
		if (this.readyState != this.OPENED) {
			throw new InvalidStateError('XHR not OPENED');
		}

		if (this.sent) {
			throw new InvalidStateError('XHR already sent');
		}

		if (this.method.match(/GET|HEAD/i)) {
			data = null;
		}

		this.requestBody = data;
		this.error = false;

		var request = {
			'async': this.async,
			'method': this.method, 
			'user': this.user,
			'password': this.password,
			'body': this.requestBody,
			'headers': this.requestHeaders.toObject(),
			'onNetworkError': method(this, 'onNetworkError')
		};
		
		if (this.async) {
			this.sent = true;
			this.onreadystatechange();
			request.onHeaders = method(this, 'onHeaders');
			request.onData = method(this, 'onData');
		}

		this.requestId = v8.http.request(request);
	},

	'onHeaders': function(headers) {
		this.responseHeaders = new Headers(headers);
		this.readyState = this.HEADERS_RECEIVED;
		this.onreadystatechange();
	},

	// FIXME: this isn't quite right yet.
	'onData': function(data, done) {
		this.responseText = data;
		this.readyState = done ? this.DONE : this.LOADING;
		this.onreadystatechange();
	},

	'onNetworkError': function() {
		extend(this, {
			'responseText': null,
			'responseXML': null
		});
		this.requestHeaders.clear();
		this.readyState = this.DONE;
		
		if (async) {
			this.onreadystatechange();
		} else {
			throw new NetworkError();
		}
	},

	'abort': function() {
		extend(this, {
			'responseText': null,
			'responseXML': null,
			'error': true
		});
		this.requestHeaders.clear();

		if (!(this.readyState == this.UNSENT ||
			  (this.readyState == this.OPENED && !this.sent) ||
			  this.readyState == this.DONE)) {
			this.readyState = this.DONE;
			this.sent = false;
			this.onreadystatechange();
		}
		this.readyState = this.UNSENT;
	},

	// response

	'status': null,
	'statusText': null,
	'responseText': null,
	'responseXML': null,

	'getResponseHeader': function(header) {
		if (this.readyState == this.UNSENT ||
			this.readyState == this.OPENED) {
			throw new InvalidStateError();
		}
		
		if (this.error) {
			return null;
		}

		if (header.match(/^Set-Cookie2?$/i)) {
			return null;
		}

		return this.responseHeaders.get(header, null);
	},

	'getAllResponseHeaders': function() {
		if (this.readyState == this.UNSENT ||
			this.readyState == this.OPENED) {
			throw new InvalidStateError();
		}
		
		if (this.error) {
			return '';
		}

		return this.responseHeaders.toString();
	}
});
