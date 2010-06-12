oeach({
	'indexOf': function(elt /*, from*/)
	{
		var len = this.length >>> 0;
		
		var from = Number(arguments[1]) || 0;
		from = (from < 0)
			? Math.ceil(from)
			: Math.floor(from);
		if (from < 0)
			from += len;
		
		for (; from < len; from++)
		{
			if (from in this &&
				this[from] === elt)
				return from;
		}
		return -1;
	},

	'lastIndexOf': function(elt /*, from*/)
	{
		var len = this.length;
		
		var from = Number(arguments[1]);
		if (isNaN(from))
		{
			from = len - 1;
		}
		else
		{
			from = (from < 0)
				? Math.ceil(from)
				: Math.floor(from);
			if (from < 0)
				from += len;
			else if (from >= len)
				from = len - 1;
		}
		
		for (; from > -1; from--)
		{
			if (from in this &&
				this[from] === elt)
				return from;
		}
		return -1;
	},

	'filter': function(fun /*, thisp*/)
	{
		var len = this.length >>> 0;
		if (typeof fun != "function")
			throw new TypeError();
		
		var res = new Array();
		var thisp = arguments[1];
		for (var i = 0; i < len; i++)
		{
			if (i in this)
			{
				var val = this[i]; // in case fun mutates this
				if (fun.call(thisp, val, i, this))
					res.push(val);
			}
		}

		return res;
	}
}, function(fname, fun) {
	if (!(fname in Array.prototype)) {
		Array.prototype[fname] = fun;
	}
});


each(['shift', 'pop', 'push', 'slice', 'concat', 'indexOf', 'lastIndexOf'], function(fname) {
	if (!(fname in Array)) {
		Array[fname] = function(array) {
			return Array[fname].apply(array, Array.prototype.slice.apply(arguments, 1));
		};
		}
});
