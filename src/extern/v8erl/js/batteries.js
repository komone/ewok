var batteries = {
    'features': {
        'assignPrototype': '__proto__' in Object
    },

    'extend': function(object) {
        var srcs = Array.slice(arguments, 1);
        srcs.reverse();
        batteries.each.call(this, srcs, function(src) {
            batteries.oeach.call(this, src, function(key, value) {
                object[key] = value;
            });
        });
        return object;
    },
    
    'method': function(object, fun) {
        if (typeof fun == 'string') {
            fun = object[fun];
        }
        
        return function() {
            return fun.apply(object, arguments);
        }
    },

    'partial': function(fun) {
        var pargs = Array.slice(arguments, 1);
        return function() {
            return fun.apply(this, Array.concat(pargs, arguments));
        };
    },
    
    'heir': (batteries.features.assignPrototype ? 
              function(object) {
                  return { '__proto__': object };
              } : 
              function(object) {
                  var heir = function() {};
                  heir.prototype = object;
                  return new heir();
              }),

    'prototypal': function(prototype) {
        if (!('constructor' in prototype)) {
            prototype.constructor = function() {};
        }
        prototype.constructor.prototype = prototype;
        return prototype.constructor;
    },

    '$break': {},

    'each': function(iterable, fun) {
        for (var i = 0, len = iterable.length; i < len; ++i) {
            if ((i in iterable) && 
                (fun.call(this, iterable[i], i, iterable) === batteries.$break)) {
                break;
            }
        }
    },

    'fold': function(iterable, fold, fun) {
        batteries.each.call(this, function(v, i) {
            fold = fun.call(this, fold, v, i, iterable);
        });
        return fold;
    },

    'range': function(start, stop, step) {
        start = start || 0;
        stop = stop || 0;
        step = step || 1;
        var r = [];
        for (var i = start; i < stop; i += step) {
            r.push(i);
        }
        return r;
    },

    'map': function(iterable, fun) {
        var mapped = new Array(iterable.length);
        batteries.each.call(this, iterable, function(v, i) {
            mapped[i] = fun.apply(this, arguments);
        });
        return mapped;
    },

    'filter': function(iterable, fun) {
        var filtered = [];
        batteries.each.call(this, iterable, function(value, i) {
            if (fun.apply(this, arguments)) {
                filtered.push(value);
            }
        });
        return filtered;
    },

    'takewhile': function(iterable, fun) {
        var end = 0;
        batteries.each.call(this, iterable, function(v, i) {
            end = i;
            return fun.apply(this, arguments) ? undefined : batteries.$break;
        });
        return Array.slice(iterable, 0, end);
    },

    'dropwhile': function(iterable, fun) {
        var start = 0;
        batteries.each.call(this, iterable, function(v, i) {
            start = i;
            return fun.apply(this, arguments) ? undefined : batteries.$break;
        });
        return Array.slice(iterable, start);
    },

    'all': function(iterable, fun) { // not one fails
        var value = true;
        batteries.each.call(this, iterable, function() {
            if (!fun.apply(this, arguments)) {
                value = false;
            }
            return value ? undefined : batteries.$break;
        });
        return value;
    },

    'any': function(iterable, fun) { // at least one passes
        var value = false;
        batteries.each.call(this, iterable, function() {
            if (fun.apply(this, arguments)) {
                value = true;
            }
            return value ? batteries.$break : undefined;
        });
        return value;
    },

    'lambda': function(value) {
        return function() {
            return value;
        };
    },

    'not': function(fun) {
        return function() {
            return !fun.apply(this, arguments);
        };
    },

    'and': function() {
        var funs = arguments;
        return function() {
            var args = arguments;
            batteries.all.call(this, funs, function(fun) {
                return fun.apply(this, args);
            });
        };
    },

    'or': function() {
        var funs = arguments;
        return function() {
            var args = arguments;
            batteries.any.call(this, funs, function(fun) {
                return fun.apply(this, args);
            });
        };
    },

    'keys': function(object) {
        var keys = [];
        for (var key in object) {
            if (object.hasOwnProperty(key)) {
                keys.push(key);
            }
        }
        return keys;
    },

    'values': function(object) {
        var values = [];
        for (var key in object) {
            if (object.hasOwnProperty(key)) {
                values.push(object[key]);
            }
        }
        return values;
    },

    'oeach': function(object, fun, own) {
        for (var key in object) {
            if (!own || object.hasOwnProperty(key)) {
                if (fun.call(this, key, object[key], object) === batteries.$break) {
                    break;
                }
            }
        }
    },

    'omap': function(object, fun, own) {
        var mapped = batteries.heir(object.prototype);
        for (var key in object) {
            if (!own || object.hasOwnProperty(key)) {
                mapped[key] = fun.call(this, key, object[key], object);
            }
        }
        return mapped;
    },

    'ofilter': function(object, fun, own) {
        var filtered = batteries.heir(object.prototype);
        for (var key in object) {
            if (!own || object.hasOwnProperty(key)) {
                if (fun.call(this, key, object[key], object)) {
                    filtered[key] = object[key];
                }
            }
        }
        return filtered;
    },

    'type': function(v) {
        var type;
        var to = typeof v;
        if (to == 'object') {
            if (v == null) {
                type = 'null';
            } else if ('length' in v) {
                type = 'array';
            } else if (v.constructor == RegExp) {
                type = 'regexp';
            } else {
                type = 'object';
            }
        } else {
            type = to;
        }
        return type;
    },

    'resolve': function(o) {
        if (typeof o == 'string') {
            o = this[o];
        }

        if (o != null) {
            each(Array.slice(arguments, 1), function(key) {
                o = o[key];
                return o != null ? undefined : batteries.$break;
            });
        }
        return o;
    },

	'copy': function(object) {
		return batteries.omap(object, function(key, value) {
			return value; 
		});
	}
};

// aliases
batteries.extend(batteries, {
    'closure': method,
    '$m': method,
    'curry': partial,
    'heir': heir,
    'forEach': each,
    'every': all,
    'some': any
});

(function() {
    // propagate to the global unless told otherwise
    if (!this.__NAMESPACE_BATTERIES__) {
        batteries.extend(this, batteries);
	}
}).call(null);