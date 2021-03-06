(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.0/optimize for better performance and smaller assets.');


var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File !== 'undefined' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[36m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = elm$core$Set$toList(x);
		y = elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = elm$core$Dict$toList(x);
		y = elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? elm$core$Basics$LT : n ? elm$core$Basics$GT : elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === elm$core$Basics$EQ ? 0 : ord === elm$core$Basics$LT ? -1 : 1;
	}));
});



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return !isNaN(word)
		? elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? elm$core$Maybe$Nothing
		: elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? elm$core$Maybe$Just(n) : elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? elm$core$Result$Ok(value)
		: (value instanceof String)
			? elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return (elm$core$Result$isOk(result)) ? result : elm$core$Result$Err(A2(elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!elm$core$Result$isOk(result))
					{
						return elm$core$Result$Err(A2(elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return elm$core$Result$Ok(elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if (elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return elm$core$Result$Err(elm$json$Json$Decode$OneOf(elm$core$List$reverse(errors)));

		case 1:
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!elm$core$Result$isOk(result))
		{
			return elm$core$Result$Err(A2(elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2(elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return elm$core$Result$Err(A2(elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	var initPair = init(result.a);
	var model = initPair.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		var pair = A2(update, msg, model);
		stepper(model = pair.a, viewMetadata);
		_Platform_enqueueEffects(managers, pair.b, subscriptions(model));
	}

	_Platform_enqueueEffects(managers, initPair.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS
//
// Effects must be queued!
//
// Say your init contains a synchronous command, like Time.now or Time.here
//
//   - This will produce a batch of effects (FX_1)
//   - The synchronous task triggers the subsequent `update` call
//   - This will produce a batch of effects (FX_2)
//
// If we just start dispatching FX_2, subscriptions from FX_2 can be processed
// before subscriptions from FX_1. No good! Earlier versions of this code had
// this problem, leading to these reports:
//
//   https://github.com/elm/core/issues/980
//   https://github.com/elm/core/pull/981
//   https://github.com/elm/compiler/issues/1776
//
// The queue is necessary to avoid ordering issues for synchronous commands.


// Why use true/false here? Why not just check the length of the queue?
// The goal is to detect "are we currently dispatching effects?" If we
// are, we need to bail and let the ongoing while loop handle things.
//
// Now say the queue has 1 element. When we dequeue the final element,
// the queue will be empty, but we are still actively dispatching effects.
// So you could get queue jumping in a really tricky category of cases.
//
var _Platform_effectsQueue = [];
var _Platform_effectsActive = false;


function _Platform_enqueueEffects(managers, cmdBag, subBag)
{
	_Platform_effectsQueue.push({ p: managers, q: cmdBag, r: subBag });

	if (_Platform_effectsActive) return;

	_Platform_effectsActive = true;
	for (var fx; fx = _Platform_effectsQueue.shift(); )
	{
		_Platform_dispatchEffects(fx.p, fx.q, fx.r);
	}
	_Platform_effectsActive = false;
}


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				s: bag.n,
				t: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.t)
		{
			x = temp.s(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		u: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		u: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].u;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2(elm$json$Json$Decode$map, func, handler.a)
				:
			A3(elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? elm$browser$Browser$Internal(next)
							: elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return elm$core$Result$isOk(result) ? elm$core$Maybe$Just(result.a) : elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail(elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2(elm$core$Task$perform, elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});




// STRINGS


var _Parser_isSubString = F5(function(smallString, offset, row, col, bigString)
{
	var smallLength = smallString.length;
	var isGood = offset + smallLength <= bigString.length;

	for (var i = 0; isGood && i < smallLength; )
	{
		var code = bigString.charCodeAt(offset);
		isGood =
			smallString[i++] === bigString[offset++]
			&& (
				code === 0x000A /* \n */
					? ( row++, col=1 )
					: ( col++, (code & 0xF800) === 0xD800 ? smallString[i++] === bigString[offset++] : 1 )
			)
	}

	return _Utils_Tuple3(isGood ? offset : -1, row, col);
});



// CHARS


var _Parser_isSubChar = F3(function(predicate, offset, string)
{
	return (
		string.length <= offset
			? -1
			:
		(string.charCodeAt(offset) & 0xF800) === 0xD800
			? (predicate(_Utils_chr(string.substr(offset, 2))) ? offset + 2 : -1)
			:
		(predicate(_Utils_chr(string[offset]))
			? ((string[offset] === '\n') ? -2 : (offset + 1))
			: -1
		)
	);
});


var _Parser_isAsciiCode = F3(function(code, offset, string)
{
	return string.charCodeAt(offset) === code;
});



// NUMBERS


var _Parser_chompBase10 = F2(function(offset, string)
{
	for (; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (code < 0x30 || 0x39 < code)
		{
			return offset;
		}
	}
	return offset;
});


var _Parser_consumeBase = F3(function(base, offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var digit = string.charCodeAt(offset) - 0x30;
		if (digit < 0 || base <= digit) break;
		total = base * total + digit;
	}
	return _Utils_Tuple2(offset, total);
});


var _Parser_consumeBase16 = F2(function(offset, string)
{
	for (var total = 0; offset < string.length; offset++)
	{
		var code = string.charCodeAt(offset);
		if (0x30 <= code && code <= 0x39)
		{
			total = 16 * total + code - 0x30;
		}
		else if (0x41 <= code && code <= 0x46)
		{
			total = 16 * total + code - 55;
		}
		else if (0x61 <= code && code <= 0x66)
		{
			total = 16 * total + code - 87;
		}
		else
		{
			break;
		}
	}
	return _Utils_Tuple2(offset, total);
});



// FIND STRING


var _Parser_findSubString = F5(function(smallString, offset, row, col, bigString)
{
	var newOffset = bigString.indexOf(smallString, offset);
	var target = newOffset < 0 ? bigString.length : newOffset + smallString.length;

	while (offset < target)
	{
		var code = bigString.charCodeAt(offset++);
		code === 0x000A /* \n */
			? ( col=1, row++ )
			: ( col++, (code & 0xF800) === 0xD800 && offset++ )
	}

	return _Utils_Tuple3(newOffset, row, col);
});
var author$project$Calendar2$Week = {$: 'Week'};
var author$project$Calendar2$State = function (a) {
	return {$: 'State', a: a};
};
var author$project$Calendar2$Msg$Agenda = {$: 'Agenda'};
var author$project$Calendar2$Msg$Day = {$: 'Day'};
var author$project$Calendar2$Msg$Month = {$: 'Month'};
var author$project$Calendar2$Msg$Week = {$: 'Week'};
var author$project$Calendar2$toInternalTimespan = function (timeSpan) {
	switch (timeSpan.$) {
		case 'Month':
			return author$project$Calendar2$Msg$Month;
		case 'Week':
			return author$project$Calendar2$Msg$Week;
		case 'Day':
			return author$project$Calendar2$Msg$Day;
		default:
			return author$project$Calendar2$Msg$Agenda;
	}
};
var elm$core$Maybe$Nothing = {$: 'Nothing'};
var author$project$Calendar2$Internal$init = F2(
	function (timeSpan, viewing) {
		return {dragState: elm$core$Maybe$Nothing, selected: elm$core$Maybe$Nothing, timeSpan: timeSpan, viewing: viewing};
	});
var elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var elm$core$Basics$identity = function (x) {
	return x;
};
var author$project$Calendar2$init = F2(
	function (timeSpan, viewing) {
		return author$project$Calendar2$State(
			A2(
				author$project$Calendar2$Internal$init,
				author$project$Calendar2$toInternalTimespan(timeSpan),
				viewing));
	});
var elm$time$Time$Oct = {$: 'Oct'};
var elm$core$Basics$EQ = {$: 'EQ'};
var elm$core$Basics$LT = {$: 'LT'};
var elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var elm$core$Array$foldr = F3(
	function (func, baseCase, _n0) {
		var tree = _n0.c;
		var tail = _n0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3(elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3(elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			elm$core$Elm$JsArray$foldr,
			helper,
			A3(elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var elm$core$List$cons = _List_cons;
var elm$core$Array$toList = function (array) {
	return A3(elm$core$Array$foldr, elm$core$List$cons, _List_Nil, array);
};
var elm$core$Basics$GT = {$: 'GT'};
var elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var elm$core$Dict$toList = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var elm$core$Dict$keys = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2(elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var elm$core$Set$toList = function (_n0) {
	var dict = _n0.a;
	return elm$core$Dict$keys(dict);
};
var elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var elm$time$Time$utc = A2(elm$time$Time$Zone, 0, _List_Nil);
var justinmimbs$time_extra$Time$Extra$Parts = F7(
	function (year, month, day, hour, minute, second, millisecond) {
		return {day: day, hour: hour, millisecond: millisecond, minute: minute, month: month, second: second, year: year};
	});
var elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var elm$core$Basics$gt = _Utils_gt;
var elm$core$Basics$lt = _Utils_lt;
var elm$core$Basics$clamp = F3(
	function (low, high, number) {
		return (_Utils_cmp(number, low) < 0) ? low : ((_Utils_cmp(number, high) > 0) ? high : number);
	});
var elm$core$Basics$add = _Basics_add;
var justinmimbs$date$Date$RD = function (a) {
	return {$: 'RD', a: a};
};
var elm$core$Basics$and = _Basics_and;
var elm$core$Basics$eq = _Utils_equal;
var elm$core$Basics$modBy = _Basics_modBy;
var elm$core$Basics$neq = _Utils_notEqual;
var elm$core$Basics$or = _Basics_or;
var justinmimbs$date$Date$isLeapYear = function (y) {
	return ((!A2(elm$core$Basics$modBy, 4, y)) && A2(elm$core$Basics$modBy, 100, y)) || (!A2(elm$core$Basics$modBy, 400, y));
};
var justinmimbs$date$Date$daysBeforeMonth = F2(
	function (y, m) {
		var leapDays = justinmimbs$date$Date$isLeapYear(y) ? 1 : 0;
		switch (m.$) {
			case 'Jan':
				return 0;
			case 'Feb':
				return 31;
			case 'Mar':
				return 59 + leapDays;
			case 'Apr':
				return 90 + leapDays;
			case 'May':
				return 120 + leapDays;
			case 'Jun':
				return 151 + leapDays;
			case 'Jul':
				return 181 + leapDays;
			case 'Aug':
				return 212 + leapDays;
			case 'Sep':
				return 243 + leapDays;
			case 'Oct':
				return 273 + leapDays;
			case 'Nov':
				return 304 + leapDays;
			default:
				return 334 + leapDays;
		}
	});
var elm$core$Basics$mul = _Basics_mul;
var elm$core$Basics$sub = _Basics_sub;
var elm$core$Basics$fdiv = _Basics_fdiv;
var elm$core$Basics$floor = _Basics_floor;
var elm$core$Basics$toFloat = _Basics_toFloat;
var justinmimbs$date$Date$floorDiv = F2(
	function (a, b) {
		return elm$core$Basics$floor(a / b);
	});
var justinmimbs$date$Date$daysBeforeYear = function (y1) {
	var y = y1 - 1;
	var leapYears = (A2(justinmimbs$date$Date$floorDiv, y, 4) - A2(justinmimbs$date$Date$floorDiv, y, 100)) + A2(justinmimbs$date$Date$floorDiv, y, 400);
	return (365 * y) + leapYears;
};
var justinmimbs$date$Date$daysInMonth = F2(
	function (y, m) {
		switch (m.$) {
			case 'Jan':
				return 31;
			case 'Feb':
				return justinmimbs$date$Date$isLeapYear(y) ? 29 : 28;
			case 'Mar':
				return 31;
			case 'Apr':
				return 30;
			case 'May':
				return 31;
			case 'Jun':
				return 30;
			case 'Jul':
				return 31;
			case 'Aug':
				return 31;
			case 'Sep':
				return 30;
			case 'Oct':
				return 31;
			case 'Nov':
				return 30;
			default:
				return 31;
		}
	});
var justinmimbs$date$Date$fromCalendarDate = F3(
	function (y, m, d) {
		return justinmimbs$date$Date$RD(
			(justinmimbs$date$Date$daysBeforeYear(y) + A2(justinmimbs$date$Date$daysBeforeMonth, y, m)) + A3(
				elm$core$Basics$clamp,
				1,
				A2(justinmimbs$date$Date$daysInMonth, y, m),
				d));
	});
var elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var elm$time$Time$millisToPosix = elm$time$Time$Posix;
var justinmimbs$date$Date$toRataDie = function (_n0) {
	var rd = _n0.a;
	return rd;
};
var justinmimbs$time_extra$Time$Extra$dateToMillis = function (date) {
	var daysSinceEpoch = justinmimbs$date$Date$toRataDie(date) - 719163;
	return daysSinceEpoch * 86400000;
};
var elm$core$Basics$idiv = _Basics_idiv;
var elm$time$Time$posixToMillis = function (_n0) {
	var millis = _n0.a;
	return millis;
};
var elm$time$Time$flooredDiv = F2(
	function (numerator, denominator) {
		return elm$core$Basics$floor(numerator / denominator);
	});
var elm$time$Time$toAdjustedMinutesHelp = F3(
	function (defaultOffset, posixMinutes, eras) {
		toAdjustedMinutesHelp:
		while (true) {
			if (!eras.b) {
				return posixMinutes + defaultOffset;
			} else {
				var era = eras.a;
				var olderEras = eras.b;
				if (_Utils_cmp(era.start, posixMinutes) < 0) {
					return posixMinutes + era.offset;
				} else {
					var $temp$defaultOffset = defaultOffset,
						$temp$posixMinutes = posixMinutes,
						$temp$eras = olderEras;
					defaultOffset = $temp$defaultOffset;
					posixMinutes = $temp$posixMinutes;
					eras = $temp$eras;
					continue toAdjustedMinutesHelp;
				}
			}
		}
	});
var elm$time$Time$toAdjustedMinutes = F2(
	function (_n0, time) {
		var defaultOffset = _n0.a;
		var eras = _n0.b;
		return A3(
			elm$time$Time$toAdjustedMinutesHelp,
			defaultOffset,
			A2(
				elm$time$Time$flooredDiv,
				elm$time$Time$posixToMillis(time),
				60000),
			eras);
	});
var elm$core$Basics$ge = _Utils_ge;
var elm$core$Basics$le = _Utils_le;
var elm$core$Basics$negate = function (n) {
	return -n;
};
var elm$time$Time$toCivil = function (minutes) {
	var rawDay = A2(elm$time$Time$flooredDiv, minutes, 60 * 24) + 719468;
	var era = (((rawDay >= 0) ? rawDay : (rawDay - 146096)) / 146097) | 0;
	var dayOfEra = rawDay - (era * 146097);
	var yearOfEra = ((((dayOfEra - ((dayOfEra / 1460) | 0)) + ((dayOfEra / 36524) | 0)) - ((dayOfEra / 146096) | 0)) / 365) | 0;
	var dayOfYear = dayOfEra - (((365 * yearOfEra) + ((yearOfEra / 4) | 0)) - ((yearOfEra / 100) | 0));
	var mp = (((5 * dayOfYear) + 2) / 153) | 0;
	var month = mp + ((mp < 10) ? 3 : (-9));
	var year = yearOfEra + (era * 400);
	return {
		day: (dayOfYear - ((((153 * mp) + 2) / 5) | 0)) + 1,
		month: month,
		year: year + ((month <= 2) ? 1 : 0)
	};
};
var elm$time$Time$toDay = F2(
	function (zone, time) {
		return elm$time$Time$toCivil(
			A2(elm$time$Time$toAdjustedMinutes, zone, time)).day;
	});
var elm$time$Time$Apr = {$: 'Apr'};
var elm$time$Time$Aug = {$: 'Aug'};
var elm$time$Time$Dec = {$: 'Dec'};
var elm$time$Time$Feb = {$: 'Feb'};
var elm$time$Time$Jan = {$: 'Jan'};
var elm$time$Time$Jul = {$: 'Jul'};
var elm$time$Time$Jun = {$: 'Jun'};
var elm$time$Time$Mar = {$: 'Mar'};
var elm$time$Time$May = {$: 'May'};
var elm$time$Time$Nov = {$: 'Nov'};
var elm$time$Time$Sep = {$: 'Sep'};
var elm$time$Time$toMonth = F2(
	function (zone, time) {
		var _n0 = elm$time$Time$toCivil(
			A2(elm$time$Time$toAdjustedMinutes, zone, time)).month;
		switch (_n0) {
			case 1:
				return elm$time$Time$Jan;
			case 2:
				return elm$time$Time$Feb;
			case 3:
				return elm$time$Time$Mar;
			case 4:
				return elm$time$Time$Apr;
			case 5:
				return elm$time$Time$May;
			case 6:
				return elm$time$Time$Jun;
			case 7:
				return elm$time$Time$Jul;
			case 8:
				return elm$time$Time$Aug;
			case 9:
				return elm$time$Time$Sep;
			case 10:
				return elm$time$Time$Oct;
			case 11:
				return elm$time$Time$Nov;
			default:
				return elm$time$Time$Dec;
		}
	});
var elm$time$Time$toYear = F2(
	function (zone, time) {
		return elm$time$Time$toCivil(
			A2(elm$time$Time$toAdjustedMinutes, zone, time)).year;
	});
var justinmimbs$date$Date$fromPosix = F2(
	function (zone, posix) {
		return A3(
			justinmimbs$date$Date$fromCalendarDate,
			A2(elm$time$Time$toYear, zone, posix),
			A2(elm$time$Time$toMonth, zone, posix),
			A2(elm$time$Time$toDay, zone, posix));
	});
var elm$time$Time$toHour = F2(
	function (zone, time) {
		return A2(
			elm$core$Basics$modBy,
			24,
			A2(
				elm$time$Time$flooredDiv,
				A2(elm$time$Time$toAdjustedMinutes, zone, time),
				60));
	});
var elm$time$Time$toMillis = F2(
	function (_n0, time) {
		return A2(
			elm$core$Basics$modBy,
			1000,
			elm$time$Time$posixToMillis(time));
	});
var elm$time$Time$toMinute = F2(
	function (zone, time) {
		return A2(
			elm$core$Basics$modBy,
			60,
			A2(elm$time$Time$toAdjustedMinutes, zone, time));
	});
var elm$time$Time$toSecond = F2(
	function (_n0, time) {
		return A2(
			elm$core$Basics$modBy,
			60,
			A2(
				elm$time$Time$flooredDiv,
				elm$time$Time$posixToMillis(time),
				1000));
	});
var justinmimbs$time_extra$Time$Extra$timeFromClock = F4(
	function (hour, minute, second, millisecond) {
		return (((hour * 3600000) + (minute * 60000)) + (second * 1000)) + millisecond;
	});
var justinmimbs$time_extra$Time$Extra$timeFromPosix = F2(
	function (zone, posix) {
		return A4(
			justinmimbs$time_extra$Time$Extra$timeFromClock,
			A2(elm$time$Time$toHour, zone, posix),
			A2(elm$time$Time$toMinute, zone, posix),
			A2(elm$time$Time$toSecond, zone, posix),
			A2(elm$time$Time$toMillis, zone, posix));
	});
var justinmimbs$time_extra$Time$Extra$toOffset = F2(
	function (zone, posix) {
		var millis = elm$time$Time$posixToMillis(posix);
		var localMillis = justinmimbs$time_extra$Time$Extra$dateToMillis(
			A2(justinmimbs$date$Date$fromPosix, zone, posix)) + A2(justinmimbs$time_extra$Time$Extra$timeFromPosix, zone, posix);
		return ((localMillis - millis) / 60000) | 0;
	});
var justinmimbs$time_extra$Time$Extra$posixFromDateTime = F3(
	function (zone, date, time) {
		var millis = justinmimbs$time_extra$Time$Extra$dateToMillis(date) + time;
		var offset0 = A2(
			justinmimbs$time_extra$Time$Extra$toOffset,
			zone,
			elm$time$Time$millisToPosix(millis));
		var posix1 = elm$time$Time$millisToPosix(millis - (offset0 * 60000));
		var offset1 = A2(justinmimbs$time_extra$Time$Extra$toOffset, zone, posix1);
		if (_Utils_eq(offset0, offset1)) {
			return posix1;
		} else {
			var posix2 = elm$time$Time$millisToPosix(millis - (offset1 * 60000));
			var offset2 = A2(justinmimbs$time_extra$Time$Extra$toOffset, zone, posix2);
			return _Utils_eq(offset1, offset2) ? posix2 : posix1;
		}
	});
var justinmimbs$time_extra$Time$Extra$partsToPosix = F2(
	function (zone, _n0) {
		var year = _n0.year;
		var month = _n0.month;
		var day = _n0.day;
		var hour = _n0.hour;
		var minute = _n0.minute;
		var second = _n0.second;
		var millisecond = _n0.millisecond;
		return A3(
			justinmimbs$time_extra$Time$Extra$posixFromDateTime,
			zone,
			A3(justinmimbs$date$Date$fromCalendarDate, year, month, day),
			A4(
				justinmimbs$time_extra$Time$Extra$timeFromClock,
				A3(elm$core$Basics$clamp, 0, 23, hour),
				A3(elm$core$Basics$clamp, 0, 59, minute),
				A3(elm$core$Basics$clamp, 0, 59, second),
				A3(elm$core$Basics$clamp, 0, 999, millisecond)));
	});
var author$project$Fixtures$eventFive = {
	end: A2(
		justinmimbs$time_extra$Time$Extra$partsToPosix,
		elm$time$Time$utc,
		A7(justinmimbs$time_extra$Time$Extra$Parts, 2020, elm$time$Time$Oct, 11, 7, 0, 0, 0)),
	id: '5',
	start: A2(
		justinmimbs$time_extra$Time$Extra$partsToPosix,
		elm$time$Time$utc,
		A7(justinmimbs$time_extra$Time$Extra$Parts, 2020, elm$time$Time$Oct, 11, 4, 0, 0, 0)),
	title: 'GUSA5/ Friends'
};
var author$project$Fixtures$eventFour = {
	end: A2(
		justinmimbs$time_extra$Time$Extra$partsToPosix,
		elm$time$Time$utc,
		A7(justinmimbs$time_extra$Time$Extra$Parts, 2020, elm$time$Time$Oct, 9, 7, 0, 0, 0)),
	id: '4',
	start: A2(
		justinmimbs$time_extra$Time$Extra$partsToPosix,
		elm$time$Time$utc,
		A7(justinmimbs$time_extra$Time$Extra$Parts, 2020, elm$time$Time$Oct, 9, 4, 0, 0, 0)),
	title: 'GUSA4/ Friends'
};
var author$project$Fixtures$eventOne = {
	end: A2(
		justinmimbs$time_extra$Time$Extra$partsToPosix,
		elm$time$Time$utc,
		A7(justinmimbs$time_extra$Time$Extra$Parts, 2020, elm$time$Time$Oct, 9, 5, 0, 0, 0)),
	id: '1',
	start: A2(
		justinmimbs$time_extra$Time$Extra$partsToPosix,
		elm$time$Time$utc,
		A7(justinmimbs$time_extra$Time$Extra$Parts, 2020, elm$time$Time$Oct, 9, 3, 0, 0, 0)),
	title: 'GUSA1/ Friends'
};
var author$project$Fixtures$eventThree = {
	end: A2(
		justinmimbs$time_extra$Time$Extra$partsToPosix,
		elm$time$Time$utc,
		A7(justinmimbs$time_extra$Time$Extra$Parts, 2020, elm$time$Time$Oct, 9, 20, 0, 0, 0)),
	id: '3',
	start: A2(
		justinmimbs$time_extra$Time$Extra$partsToPosix,
		elm$time$Time$utc,
		A7(justinmimbs$time_extra$Time$Extra$Parts, 2020, elm$time$Time$Oct, 9, 17, 0, 0, 0)),
	title: 'GUSA3/ Friends'
};
var author$project$Fixtures$eventTwo = {
	end: A2(
		justinmimbs$time_extra$Time$Extra$partsToPosix,
		elm$time$Time$utc,
		A7(justinmimbs$time_extra$Time$Extra$Parts, 2020, elm$time$Time$Oct, 9, 13, 0, 0, 0)),
	id: '2',
	start: A2(
		justinmimbs$time_extra$Time$Extra$partsToPosix,
		elm$time$Time$utc,
		A7(justinmimbs$time_extra$Time$Extra$Parts, 2020, elm$time$Time$Oct, 9, 12, 0, 0, 0)),
	title: 'GUSA2/ Friends'
};
var author$project$Fixtures$events = _List_fromArray(
	[author$project$Fixtures$eventOne, author$project$Fixtures$eventTwo, author$project$Fixtures$eventThree, author$project$Fixtures$eventFour, author$project$Fixtures$eventFive]);
var author$project$Fixtures$viewing = A2(
	justinmimbs$time_extra$Time$Extra$partsToPosix,
	elm$time$Time$utc,
	A7(justinmimbs$time_extra$Time$Extra$Parts, 2020, elm$time$Time$Oct, 9, 16, 0, 0, 0));
var elm$core$Basics$compare = _Utils_compare;
var author$project$Main$flippedComparison = F2(
	function (a, b) {
		var _n0 = A2(elm$core$Basics$compare, a, b);
		switch (_n0.$) {
			case 'LT':
				return elm$core$Basics$GT;
			case 'EQ':
				return elm$core$Basics$EQ;
			default:
				return elm$core$Basics$LT;
		}
	});
var elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var elm$core$Dict$empty = elm$core$Dict$RBEmpty_elm_builtin;
var elm$core$Dict$Black = {$: 'Black'};
var elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var elm$core$Dict$Red = {$: 'Red'};
var elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _n1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _n3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					key,
					value,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _n5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _n6 = left.d;
				var _n7 = _n6.a;
				var llK = _n6.b;
				var llV = _n6.c;
				var llLeft = _n6.d;
				var llRight = _n6.e;
				var lRight = left.e;
				return A5(
					elm$core$Dict$RBNode_elm_builtin,
					elm$core$Dict$Red,
					lK,
					lV,
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5(elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Red, key, value, elm$core$Dict$RBEmpty_elm_builtin, elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _n1 = A2(elm$core$Basics$compare, key, nKey);
			switch (_n1.$) {
				case 'LT':
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3(elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5(elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3(elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _n0 = A3(elm$core$Dict$insertHelp, key, value, dict);
		if ((_n0.$ === 'RBNode_elm_builtin') && (_n0.a.$ === 'Red')) {
			var _n1 = _n0.a;
			var k = _n0.b;
			var v = _n0.c;
			var l = _n0.d;
			var r = _n0.e;
			return A5(elm$core$Dict$RBNode_elm_builtin, elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _n0;
			return x;
		}
	});
var elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var elm$core$Dict$fromList = function (assocs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, dict) {
				var key = _n0.a;
				var value = _n0.b;
				return A3(elm$core$Dict$insert, key, value, dict);
			}),
		elm$core$Dict$empty,
		assocs);
};
var elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return elm$core$Maybe$Just(x);
	} else {
		return elm$core$Maybe$Nothing;
	}
};
var elm$core$List$length = function (xs) {
	return A3(
		elm$core$List$foldl,
		F2(
			function (_n0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var elm$core$List$reverse = function (list) {
	return A3(elm$core$List$foldl, elm$core$List$cons, _List_Nil, list);
};
var elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							elm$core$List$foldl,
							fn,
							acc,
							elm$core$List$reverse(r4)) : A4(elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4(elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var elm$core$List$sortWith = _List_sortWith;
var elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var elm$core$String$fromInt = _String_fromNumber;
var elm$core$String$toInt = _String_toInt;
var author$project$Main$base_model = {
	calendarState: A2(author$project$Calendar2$init, author$project$Calendar2$Week, author$project$Fixtures$viewing),
	curEventId: elm$core$String$fromInt(
		A2(
			elm$core$Maybe$withDefault,
			elm$core$List$length(author$project$Fixtures$events),
			elm$core$List$head(
				A2(
					elm$core$List$sortWith,
					author$project$Main$flippedComparison,
					A2(
						elm$core$List$map,
						A2(
							elm$core$Basics$composeL,
							A2(
								elm$core$Basics$composeL,
								elm$core$Maybe$withDefault(0),
								elm$core$String$toInt),
							function ($) {
								return $.id;
							}),
						author$project$Fixtures$events))))),
	eventExtendAmount: elm$time$Time$millisToPosix(0),
	eventPreview: elm$core$Maybe$Nothing,
	events: elm$core$Dict$fromList(
		A2(
			elm$core$List$map,
			function (event) {
				return _Utils_Tuple2(event.id, event);
			},
			author$project$Fixtures$events)),
	selectedEvent: elm$core$Maybe$Nothing
};
var elm$core$Basics$False = {$: 'False'};
var elm$core$Basics$True = {$: 'True'};
var elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var elm$core$Array$branchFactor = 32;
var elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var elm$core$Basics$ceiling = _Basics_ceiling;
var elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var elm$core$Array$shiftStep = elm$core$Basics$ceiling(
	A2(elm$core$Basics$logBase, 2, elm$core$Array$branchFactor));
var elm$core$Elm$JsArray$empty = _JsArray_empty;
var elm$core$Array$empty = A4(elm$core$Array$Array_elm_builtin, 0, elm$core$Array$shiftStep, elm$core$Elm$JsArray$empty, elm$core$Elm$JsArray$empty);
var elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _n0 = A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodes);
			var node = _n0.a;
			var remainingNodes = _n0.b;
			var newAcc = A2(
				elm$core$List$cons,
				elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var elm$core$Tuple$first = function (_n0) {
	var x = _n0.a;
	return x;
};
var elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = elm$core$Basics$ceiling(nodeListSize / elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2(elm$core$Elm$JsArray$initializeFromList, elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2(elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var elm$core$Elm$JsArray$length = _JsArray_length;
var elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail),
				elm$core$Array$shiftStep,
				elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * elm$core$Array$branchFactor;
			var depth = elm$core$Basics$floor(
				A2(elm$core$Basics$logBase, elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2(elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				elm$core$Array$Array_elm_builtin,
				elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2(elm$core$Basics$max, 5, depth * elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = elm$core$Array$Leaf(
					A3(elm$core$Elm$JsArray$initialize, elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2(elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var elm$core$Basics$remainderBy = _Basics_remainderBy;
var elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return elm$core$Array$empty;
		} else {
			var tailLen = len % elm$core$Array$branchFactor;
			var tail = A3(elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - elm$core$Array$branchFactor;
			return A5(elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var elm$core$Basics$append = _Utils_append;
var elm$core$Char$toCode = _Char_toCode;
var elm$core$Char$isLower = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var elm$core$Char$isUpper = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var elm$core$Char$isAlpha = function (_char) {
	return elm$core$Char$isLower(_char) || elm$core$Char$isUpper(_char);
};
var elm$core$Char$isDigit = function (_char) {
	var code = elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var elm$core$Char$isAlphaNum = function (_char) {
	return elm$core$Char$isLower(_char) || (elm$core$Char$isUpper(_char) || elm$core$Char$isDigit(_char));
};
var elm$core$List$map2 = _List_map2;
var elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2(elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var elm$core$List$range = F2(
	function (lo, hi) {
		return A3(elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$map2,
			f,
			A2(
				elm$core$List$range,
				0,
				elm$core$List$length(xs) - 1),
			xs);
	});
var elm$core$String$all = _String_all;
var elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var elm$core$String$uncons = _String_uncons;
var elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var elm$json$Json$Decode$indent = function (str) {
	return A2(
		elm$core$String$join,
		'\n    ',
		A2(elm$core$String$split, '\n', str));
};
var elm$json$Json$Encode$encode = _Json_encode;
var elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + (elm$core$String$fromInt(i + 1) + (') ' + elm$json$Json$Decode$indent(
			elm$json$Json$Decode$errorToString(error))));
	});
var elm$json$Json$Decode$errorToString = function (error) {
	return A2(elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _n1 = elm$core$String$uncons(f);
						if (_n1.$ === 'Nothing') {
							return false;
						} else {
							var _n2 = _n1.a;
							var _char = _n2.a;
							var rest = _n2.b;
							return elm$core$Char$isAlpha(_char) && A2(elm$core$String$all, elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + (elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2(elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									elm$core$String$join,
									'',
									elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										elm$core$String$join,
										'',
										elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + (elm$core$String$fromInt(
								elm$core$List$length(errors)) + ' ways:'));
							return A2(
								elm$core$String$join,
								'\n\n',
								A2(
									elm$core$List$cons,
									introduction,
									A2(elm$core$List$indexedMap, elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								elm$core$String$join,
								'',
								elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + (elm$json$Json$Decode$indent(
						A2(elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var elm$core$Platform$Cmd$batch = _Platform_batch;
var elm$core$Platform$Cmd$none = elm$core$Platform$Cmd$batch(_List_Nil);
var author$project$Main$init = function (_n0) {
	return _Utils_Tuple2(author$project$Main$base_model, elm$core$Platform$Cmd$none);
};
var elm$core$Platform$Sub$batch = _Platform_batch;
var elm$core$Platform$Sub$none = elm$core$Platform$Sub$batch(_List_Nil);
var author$project$Calendar2$subscriptions = function (_n0) {
	var state = _n0.a;
	return elm$core$Platform$Sub$none;
};
var author$project$Main$CancelEventPreview = function (a) {
	return {$: 'CancelEventPreview', a: a};
};
var author$project$Main$SetCalendarState = function (a) {
	return {$: 'SetCalendarState', a: a};
};
var elm$core$Platform$Sub$map = _Platform_map;
var elm$browser$Browser$Events$Document = {$: 'Document'};
var elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var elm$core$Task$succeed = _Scheduler_succeed;
var elm$browser$Browser$Events$init = elm$core$Task$succeed(
	A2(elm$browser$Browser$Events$State, _List_Nil, elm$core$Dict$empty));
var elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var elm$core$Task$andThen = _Scheduler_andThen;
var elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var elm$core$Basics$never = function (_n0) {
	never:
	while (true) {
		var nvr = _n0.a;
		var $temp$_n0 = nvr;
		_n0 = $temp$_n0;
		continue never;
	}
};
var elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var elm$core$Task$init = elm$core$Task$succeed(_Utils_Tuple0);
var elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			elm$core$Task$andThen,
			function (a) {
				return A2(
					elm$core$Task$andThen,
					function (b) {
						return elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var elm$core$Task$sequence = function (tasks) {
	return A3(
		elm$core$List$foldr,
		elm$core$Task$map2(elm$core$List$cons),
		elm$core$Task$succeed(_List_Nil),
		tasks);
};
var elm$core$Platform$sendToApp = _Platform_sendToApp;
var elm$core$Task$spawnCmd = F2(
	function (router, _n0) {
		var task = _n0.a;
		return _Scheduler_spawn(
			A2(
				elm$core$Task$andThen,
				elm$core$Platform$sendToApp(router),
				task));
	});
var elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			elm$core$Task$map,
			function (_n0) {
				return _Utils_Tuple0;
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Task$spawnCmd(router),
					commands)));
	});
var elm$core$Task$onSelfMsg = F3(
	function (_n0, _n1, _n2) {
		return elm$core$Task$succeed(_Utils_Tuple0);
	});
var elm$core$Task$cmdMap = F2(
	function (tagger, _n0) {
		var task = _n0.a;
		return elm$core$Task$Perform(
			A2(elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager(elm$core$Task$init, elm$core$Task$onEffects, elm$core$Task$onSelfMsg, elm$core$Task$cmdMap);
var elm$core$Task$command = _Platform_leaf('Task');
var elm$core$Task$perform = F2(
	function (toMessage, task) {
		return elm$core$Task$command(
			elm$core$Task$Perform(
				A2(elm$core$Task$map, toMessage, task)));
	});
var elm$json$Json$Decode$map = _Json_map1;
var elm$json$Json$Decode$map2 = _Json_map2;
var elm$json$Json$Decode$succeed = _Json_succeed;
var elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var elm$core$String$length = _String_length;
var elm$core$String$slice = _String_slice;
var elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			elm$core$String$slice,
			n,
			elm$core$String$length(string),
			string);
	});
var elm$core$String$startsWith = _String_startsWith;
var elm$url$Url$Http = {$: 'Http'};
var elm$url$Url$Https = {$: 'Https'};
var elm$core$String$indexes = _String_indexes;
var elm$core$String$isEmpty = function (string) {
	return string === '';
};
var elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(elm$core$String$slice, 0, n, string);
	});
var elm$core$String$contains = _String_contains;
var elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if (elm$core$String$isEmpty(str) || A2(elm$core$String$contains, '@', str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, ':', str);
			if (!_n0.b) {
				return elm$core$Maybe$Just(
					A6(elm$url$Url$Url, protocol, str, elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_n0.b.b) {
					var i = _n0.a;
					var _n1 = elm$core$String$toInt(
						A2(elm$core$String$dropLeft, i + 1, str));
					if (_n1.$ === 'Nothing') {
						return elm$core$Maybe$Nothing;
					} else {
						var port_ = _n1;
						return elm$core$Maybe$Just(
							A6(
								elm$url$Url$Url,
								protocol,
								A2(elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return elm$core$Maybe$Nothing;
				}
			}
		}
	});
var elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '/', str);
			if (!_n0.b) {
				return A5(elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _n0.a;
				return A5(
					elm$url$Url$chompBeforePath,
					protocol,
					A2(elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '?', str);
			if (!_n0.b) {
				return A4(elm$url$Url$chompBeforeQuery, protocol, elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _n0.a;
				return A4(
					elm$url$Url$chompBeforeQuery,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if (elm$core$String$isEmpty(str)) {
			return elm$core$Maybe$Nothing;
		} else {
			var _n0 = A2(elm$core$String$indexes, '#', str);
			if (!_n0.b) {
				return A3(elm$url$Url$chompBeforeFragment, protocol, elm$core$Maybe$Nothing, str);
			} else {
				var i = _n0.a;
				return A3(
					elm$url$Url$chompBeforeFragment,
					protocol,
					elm$core$Maybe$Just(
						A2(elm$core$String$dropLeft, i + 1, str)),
					A2(elm$core$String$left, i, str));
			}
		}
	});
var elm$url$Url$fromString = function (str) {
	return A2(elm$core$String$startsWith, 'http://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Http,
		A2(elm$core$String$dropLeft, 7, str)) : (A2(elm$core$String$startsWith, 'https://', str) ? A2(
		elm$url$Url$chompAfterProtocol,
		elm$url$Url$Https,
		A2(elm$core$String$dropLeft, 8, str)) : elm$core$Maybe$Nothing);
};
var elm$browser$Browser$Events$spawn = F3(
	function (router, key, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						elm$core$Platform$sendToSelf,
						router,
						A2(elm$browser$Browser$Events$Event, key, event));
				}));
	});
var elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3(elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _n0) {
				stepState:
				while (true) {
					var list = _n0.a;
					var result = _n0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _n2 = list.a;
						var lKey = _n2.a;
						var lValue = _n2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_n0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_n0 = $temp$_n0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _n3 = A3(
			elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _n3.a;
		var intermediateResult = _n3.b;
		return A3(
			elm$core$List$foldl,
			F2(
				function (_n4, result) {
					var k = _n4.a;
					var v = _n4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3(elm$core$Dict$foldl, elm$core$Dict$insert, t2, t1);
	});
var elm$core$Process$kill = _Scheduler_kill;
var elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _n6) {
				var deads = _n6.a;
				var lives = _n6.b;
				var news = _n6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						elm$core$List$cons,
						A3(elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_n4, pid, _n5) {
				var deads = _n5.a;
				var lives = _n5.b;
				var news = _n5.c;
				return _Utils_Tuple3(
					A2(elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _n2, _n3) {
				var deads = _n3.a;
				var lives = _n3.b;
				var news = _n3.c;
				return _Utils_Tuple3(
					deads,
					A3(elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2(elm$core$List$map, elm$browser$Browser$Events$addKey, subs);
		var _n0 = A6(
			elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, elm$core$Dict$empty, _List_Nil));
		var deadPids = _n0.a;
		var livePids = _n0.b;
		var makeNewPids = _n0.c;
		return A2(
			elm$core$Task$andThen,
			function (pids) {
				return elm$core$Task$succeed(
					A2(
						elm$browser$Browser$Events$State,
						newSubs,
						A2(
							elm$core$Dict$union,
							livePids,
							elm$core$Dict$fromList(pids))));
			},
			A2(
				elm$core$Task$andThen,
				function (_n1) {
					return elm$core$Task$sequence(makeNewPids);
				},
				elm$core$Task$sequence(
					A2(elm$core$List$map, elm$core$Process$kill, deadPids))));
	});
var elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _n0 = f(mx);
		if (_n0.$ === 'Just') {
			var x = _n0.a;
			return A2(elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			elm$core$List$foldr,
			elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _n0, state) {
		var key = _n0.key;
		var event = _n0.event;
		var toMessage = function (_n2) {
			var subKey = _n2.a;
			var _n3 = _n2.b;
			var node = _n3.a;
			var name = _n3.b;
			var decoder = _n3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : elm$core$Maybe$Nothing;
		};
		var messages = A2(elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			elm$core$Task$andThen,
			function (_n1) {
				return elm$core$Task$succeed(state);
			},
			elm$core$Task$sequence(
				A2(
					elm$core$List$map,
					elm$core$Platform$sendToApp(router),
					messages)));
	});
var elm$browser$Browser$Events$subMap = F2(
	function (func, _n0) {
		var node = _n0.a;
		var name = _n0.b;
		var decoder = _n0.c;
		return A3(
			elm$browser$Browser$Events$MySub,
			node,
			name,
			A2(elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager(elm$browser$Browser$Events$init, elm$browser$Browser$Events$onEffects, elm$browser$Browser$Events$onSelfMsg, 0, elm$browser$Browser$Events$subMap);
var elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return elm$browser$Browser$Events$subscription(
			A3(elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var elm$browser$Browser$Events$onKeyDown = A2(elm$browser$Browser$Events$on, elm$browser$Browser$Events$Document, 'keydown');
var elm$json$Json$Decode$field = _Json_decodeField;
var elm$json$Json$Decode$string = _Json_decodeString;
var ohanhi$keyboard$Keyboard$RawKey = function (a) {
	return {$: 'RawKey', a: a};
};
var ohanhi$keyboard$Keyboard$eventKeyDecoder = A2(
	elm$json$Json$Decode$field,
	'key',
	A2(elm$json$Json$Decode$map, ohanhi$keyboard$Keyboard$RawKey, elm$json$Json$Decode$string));
var ohanhi$keyboard$Keyboard$downs = function (toMsg) {
	return elm$browser$Browser$Events$onKeyDown(
		A2(elm$json$Json$Decode$map, toMsg, ohanhi$keyboard$Keyboard$eventKeyDecoder));
};
var author$project$Main$subscriptions = function (model_) {
	return elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2(
				elm$core$Platform$Sub$map,
				author$project$Main$SetCalendarState,
				author$project$Calendar2$subscriptions(model_.calendarState)),
				ohanhi$keyboard$Keyboard$downs(author$project$Main$CancelEventPreview)
			]));
};
var author$project$Calendar2$Internal$Drag = F3(
	function (start, current, kind) {
		return {current: current, kind: kind, start: start};
	});
var author$project$Calendar2$Internal$Event = function (a) {
	return {$: 'Event', a: a};
};
var author$project$Calendar2$Internal$TimeSlot = function (a) {
	return {$: 'TimeSlot', a: a};
};
var author$project$Calendar2$Internal$changeTimeSpan = F2(
	function (timeSpan, state) {
		return _Utils_update(
			state,
			{timeSpan: timeSpan});
	});
var author$project$Calendar2$Internal$getNewDateBasedOnPosition = F3(
	function (date, xy, state) {
		return date;
	});
var author$project$Calendar2$Internal$getTimeDiffForPosition = F2(
	function (xy, state) {
		var timeDiff = function (_n2) {
			var start = _n2.start;
			var current = _n2.current;
			return 30 * (((current.y - start.y) / 20) | 0);
		};
		var _n0 = state.timeSpan;
		if (_n0.$ === 'Month') {
			return 0;
		} else {
			var _n1 = state.dragState;
			if (_n1.$ === 'Just') {
				var drag = _n1.a;
				return 1;
			} else {
				return 0;
			}
		}
	});
var justinmimbs$time_extra$Time$Extra$Day = {$: 'Day'};
var justinmimbs$time_extra$Time$Extra$Month = {$: 'Month'};
var justinmimbs$time_extra$Time$Extra$Week = {$: 'Week'};
var justinmimbs$date$Date$Days = {$: 'Days'};
var justinmimbs$date$Date$Months = {$: 'Months'};
var elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var justinmimbs$date$Date$monthToNumber = function (m) {
	switch (m.$) {
		case 'Jan':
			return 1;
		case 'Feb':
			return 2;
		case 'Mar':
			return 3;
		case 'Apr':
			return 4;
		case 'May':
			return 5;
		case 'Jun':
			return 6;
		case 'Jul':
			return 7;
		case 'Aug':
			return 8;
		case 'Sep':
			return 9;
		case 'Oct':
			return 10;
		case 'Nov':
			return 11;
		default:
			return 12;
	}
};
var justinmimbs$date$Date$numberToMonth = function (mn) {
	var _n0 = A2(elm$core$Basics$max, 1, mn);
	switch (_n0) {
		case 1:
			return elm$time$Time$Jan;
		case 2:
			return elm$time$Time$Feb;
		case 3:
			return elm$time$Time$Mar;
		case 4:
			return elm$time$Time$Apr;
		case 5:
			return elm$time$Time$May;
		case 6:
			return elm$time$Time$Jun;
		case 7:
			return elm$time$Time$Jul;
		case 8:
			return elm$time$Time$Aug;
		case 9:
			return elm$time$Time$Sep;
		case 10:
			return elm$time$Time$Oct;
		case 11:
			return elm$time$Time$Nov;
		default:
			return elm$time$Time$Dec;
	}
};
var justinmimbs$date$Date$toCalendarDateHelp = F3(
	function (y, m, d) {
		toCalendarDateHelp:
		while (true) {
			var monthDays = A2(justinmimbs$date$Date$daysInMonth, y, m);
			var mn = justinmimbs$date$Date$monthToNumber(m);
			if ((mn < 12) && (_Utils_cmp(d, monthDays) > 0)) {
				var $temp$y = y,
					$temp$m = justinmimbs$date$Date$numberToMonth(mn + 1),
					$temp$d = d - monthDays;
				y = $temp$y;
				m = $temp$m;
				d = $temp$d;
				continue toCalendarDateHelp;
			} else {
				return {day: d, month: m, year: y};
			}
		}
	});
var justinmimbs$date$Date$divWithRemainder = F2(
	function (a, b) {
		return _Utils_Tuple2(
			A2(justinmimbs$date$Date$floorDiv, a, b),
			A2(elm$core$Basics$modBy, b, a));
	});
var justinmimbs$date$Date$year = function (_n0) {
	var rd = _n0.a;
	var _n1 = A2(justinmimbs$date$Date$divWithRemainder, rd, 146097);
	var n400 = _n1.a;
	var r400 = _n1.b;
	var _n2 = A2(justinmimbs$date$Date$divWithRemainder, r400, 36524);
	var n100 = _n2.a;
	var r100 = _n2.b;
	var _n3 = A2(justinmimbs$date$Date$divWithRemainder, r100, 1461);
	var n4 = _n3.a;
	var r4 = _n3.b;
	var _n4 = A2(justinmimbs$date$Date$divWithRemainder, r4, 365);
	var n1 = _n4.a;
	var r1 = _n4.b;
	var n = (!r1) ? 0 : 1;
	return ((((n400 * 400) + (n100 * 100)) + (n4 * 4)) + n1) + n;
};
var justinmimbs$date$Date$toOrdinalDate = function (_n0) {
	var rd = _n0.a;
	var y = justinmimbs$date$Date$year(
		justinmimbs$date$Date$RD(rd));
	return {
		ordinalDay: rd - justinmimbs$date$Date$daysBeforeYear(y),
		year: y
	};
};
var justinmimbs$date$Date$toCalendarDate = function (_n0) {
	var rd = _n0.a;
	var date = justinmimbs$date$Date$toOrdinalDate(
		justinmimbs$date$Date$RD(rd));
	return A3(justinmimbs$date$Date$toCalendarDateHelp, date.year, elm$time$Time$Jan, date.ordinalDay);
};
var justinmimbs$date$Date$add = F3(
	function (unit, n, _n0) {
		var rd = _n0.a;
		switch (unit.$) {
			case 'Years':
				return A3(
					justinmimbs$date$Date$add,
					justinmimbs$date$Date$Months,
					12 * n,
					justinmimbs$date$Date$RD(rd));
			case 'Months':
				var date = justinmimbs$date$Date$toCalendarDate(
					justinmimbs$date$Date$RD(rd));
				var wholeMonths = ((12 * (date.year - 1)) + (justinmimbs$date$Date$monthToNumber(date.month) - 1)) + n;
				var m = justinmimbs$date$Date$numberToMonth(
					A2(elm$core$Basics$modBy, 12, wholeMonths) + 1);
				var y = A2(justinmimbs$date$Date$floorDiv, wholeMonths, 12) + 1;
				return justinmimbs$date$Date$RD(
					(justinmimbs$date$Date$daysBeforeYear(y) + A2(justinmimbs$date$Date$daysBeforeMonth, y, m)) + A2(
						elm$core$Basics$min,
						date.day,
						A2(justinmimbs$date$Date$daysInMonth, y, m)));
			case 'Weeks':
				return justinmimbs$date$Date$RD(rd + (7 * n));
			default:
				return justinmimbs$date$Date$RD(rd + n);
		}
	});
var justinmimbs$time_extra$Time$Extra$Millisecond = {$: 'Millisecond'};
var justinmimbs$time_extra$Time$Extra$add = F4(
	function (interval, n, zone, posix) {
		add:
		while (true) {
			switch (interval.$) {
				case 'Millisecond':
					return elm$time$Time$millisToPosix(
						elm$time$Time$posixToMillis(posix) + n);
				case 'Second':
					var $temp$interval = justinmimbs$time_extra$Time$Extra$Millisecond,
						$temp$n = n * 1000,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 'Minute':
					var $temp$interval = justinmimbs$time_extra$Time$Extra$Millisecond,
						$temp$n = n * 60000,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 'Hour':
					var $temp$interval = justinmimbs$time_extra$Time$Extra$Millisecond,
						$temp$n = n * 3600000,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 'Day':
					return A3(
						justinmimbs$time_extra$Time$Extra$posixFromDateTime,
						zone,
						A3(
							justinmimbs$date$Date$add,
							justinmimbs$date$Date$Days,
							n,
							A2(justinmimbs$date$Date$fromPosix, zone, posix)),
						A2(justinmimbs$time_extra$Time$Extra$timeFromPosix, zone, posix));
				case 'Month':
					return A3(
						justinmimbs$time_extra$Time$Extra$posixFromDateTime,
						zone,
						A3(
							justinmimbs$date$Date$add,
							justinmimbs$date$Date$Months,
							n,
							A2(justinmimbs$date$Date$fromPosix, zone, posix)),
						A2(justinmimbs$time_extra$Time$Extra$timeFromPosix, zone, posix));
				case 'Year':
					var $temp$interval = justinmimbs$time_extra$Time$Extra$Month,
						$temp$n = n * 12,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 'Quarter':
					var $temp$interval = justinmimbs$time_extra$Time$Extra$Month,
						$temp$n = n * 3,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				case 'Week':
					var $temp$interval = justinmimbs$time_extra$Time$Extra$Day,
						$temp$n = n * 7,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
				default:
					var weekday = interval;
					var $temp$interval = justinmimbs$time_extra$Time$Extra$Day,
						$temp$n = n * 7,
						$temp$zone = zone,
						$temp$posix = posix;
					interval = $temp$interval;
					n = $temp$n;
					zone = $temp$zone;
					posix = $temp$posix;
					continue add;
			}
		}
	});
var author$project$Calendar2$Internal$page = F2(
	function (step, state) {
		var _n0 = state;
		var timeSpan = _n0.timeSpan;
		var viewing = _n0.viewing;
		switch (timeSpan.$) {
			case 'Week':
				return _Utils_update(
					state,
					{
						viewing: A4(justinmimbs$time_extra$Time$Extra$add, justinmimbs$time_extra$Time$Extra$Week, step, elm$time$Time$utc, viewing)
					});
			case 'Day':
				return _Utils_update(
					state,
					{
						viewing: A4(justinmimbs$time_extra$Time$Extra$add, justinmimbs$time_extra$Time$Extra$Day, step, elm$time$Time$utc, viewing)
					});
			default:
				return _Utils_update(
					state,
					{
						viewing: A4(justinmimbs$time_extra$Time$Extra$add, justinmimbs$time_extra$Time$Extra$Month, step, elm$time$Time$utc, viewing)
					});
		}
	});
var elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return elm$core$Maybe$Just(
				f(value));
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var author$project$Calendar2$Internal$update = F4(
	function (eventConfig, timeSlotConfig, msg, state) {
		switch (msg.$) {
			case 'PageBack':
				return _Utils_Tuple2(
					A2(author$project$Calendar2$Internal$page, -1, state),
					elm$core$Maybe$Nothing);
			case 'PageForward':
				return _Utils_Tuple2(
					A2(author$project$Calendar2$Internal$page, 1, state),
					elm$core$Maybe$Nothing);
			case 'ChangeTimeSpan':
				var timeSpan = msg.a;
				return _Utils_Tuple2(
					A2(author$project$Calendar2$Internal$changeTimeSpan, timeSpan, state),
					elm$core$Maybe$Nothing);
			case 'TimeSlotClick':
				var date = msg.a;
				var xy = msg.b;
				return _Utils_Tuple2(
					state,
					A2(timeSlotConfig.onClick, date, xy));
			case 'TimeSlotMouseEnter':
				var date = msg.a;
				var xy = msg.b;
				return _Utils_Tuple2(
					state,
					A2(timeSlotConfig.onMouseEnter, date, xy));
			case 'TimeSlotMouseLeave':
				var date = msg.a;
				var xy = msg.b;
				return _Utils_Tuple2(
					state,
					A2(timeSlotConfig.onMouseLeave, date, xy));
			case 'TimeSlotDragStart':
				var date = msg.a;
				var xy = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						state,
						{
							dragState: elm$core$Maybe$Just(
								{
									current: xy,
									kind: author$project$Calendar2$Internal$TimeSlot(date),
									start: xy
								})
						}),
					A2(timeSlotConfig.onDragStart, date, xy));
			case 'TimeSlotDragging':
				var date = msg.a;
				var me = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						state,
						{
							dragState: A2(
								elm$core$Maybe$map,
								function (_n1) {
									var start = _n1.start;
									var kind = _n1.kind;
									return A3(author$project$Calendar2$Internal$Drag, start, me, kind);
								},
								state.dragState)
						}),
					A2(
						timeSlotConfig.onDragging,
						A3(author$project$Calendar2$Internal$getNewDateBasedOnPosition, date, me, state),
						me));
			case 'TimeSlotDragEnd':
				var date = msg.a;
				var xy = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						state,
						{dragState: elm$core$Maybe$Nothing}),
					A2(
						timeSlotConfig.onDragEnd,
						A3(author$project$Calendar2$Internal$getNewDateBasedOnPosition, date, xy, state),
						xy));
			case 'EventClick':
				var eventId = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						state,
						{
							selected: elm$core$Maybe$Just(eventId)
						}),
					eventConfig.onClick(eventId));
			case 'EventMouseEnter':
				var eventId = msg.a;
				return _Utils_Tuple2(
					state,
					eventConfig.onMouseEnter(eventId));
			case 'EventMouseLeave':
				var eventId = msg.a;
				return _Utils_Tuple2(
					state,
					eventConfig.onMouseLeave(eventId));
			case 'EventDragStart':
				var eventId = msg.a;
				var xy = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						state,
						{
							dragState: elm$core$Maybe$Just(
								{
									current: xy,
									kind: author$project$Calendar2$Internal$Event(eventId),
									start: xy
								})
						}),
					eventConfig.onDragStart(eventId));
			case 'EventDragging':
				var eventId = msg.a;
				var me = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						state,
						{
							dragState: A2(
								elm$core$Maybe$map,
								function (_n2) {
									var start = _n2.start;
									var kind = _n2.kind;
									return A3(author$project$Calendar2$Internal$Drag, start, me, kind);
								},
								state.dragState)
						}),
					A2(
						eventConfig.onDragging,
						eventId,
						A2(author$project$Calendar2$Internal$getTimeDiffForPosition, me, state)));
			default:
				var eventId = msg.a;
				var xy = msg.b;
				return _Utils_Tuple2(
					_Utils_update(
						state,
						{dragState: elm$core$Maybe$Nothing}),
					A2(
						eventConfig.onDragEnd,
						eventId,
						A2(author$project$Calendar2$Internal$getTimeDiffForPosition, xy, state)));
		}
	});
var author$project$Calendar2$update = F4(
	function (_n0, _n1, _n2, _n3) {
		var eventConfig_ = _n0.a;
		var timeSlotConfig_ = _n1.a;
		var msg = _n2.a;
		var state = _n3.a;
		var _n4 = A4(author$project$Calendar2$Internal$update, eventConfig_, timeSlotConfig_, msg, state);
		var updatedCalendar = _n4.a;
		var calendarMsg = _n4.b;
		return _Utils_Tuple2(
			author$project$Calendar2$State(updatedCalendar),
			calendarMsg);
	});
var author$project$Main$newEventId = function (eventId) {
	return elm$core$String$fromInt(
		1 + A2(
			elm$core$Maybe$withDefault,
			0,
			elm$core$String$toInt(eventId)));
};
var elm$core$Debug$log = _Debug_log;
var author$project$Main$addEventPreviewToEvents = function (model_) {
	var defaultEmptyTitle = function (event) {
		return _Utils_update(
			event,
			{
				title: (event.title === '') ? '(No Title)' : event.title
			});
	};
	var addToEvents = function (event) {
		return A3(
			elm$core$Dict$insert,
			event.id,
			A2(
				elm$core$Debug$log,
				'newEvent',
				defaultEmptyTitle(event)),
			model_.events);
	};
	return _Utils_update(
		model_,
		{
			curEventId: author$project$Main$newEventId(model_.curEventId),
			events: A2(
				elm$core$Maybe$withDefault,
				model_.events,
				A2(
					elm$core$Maybe$map,
					A2(
						elm$core$Basics$composeL,
						addToEvents,
						function ($) {
							return $.event;
						}),
					model_.eventPreview))
		});
};
var author$project$Main$changeEventPreviewTitle = F2(
	function (title, model_) {
		var changeEventTitle = function (event) {
			return _Utils_update(
				event,
				{title: title});
		};
		var changePreviewTitle = function (preview) {
			return _Utils_update(
				preview,
				{
					event: changeEventTitle(preview.event)
				});
		};
		return _Utils_update(
			model_,
			{
				eventPreview: A2(elm$core$Maybe$map, changePreviewTitle, model_.eventPreview)
			});
	});
var author$project$Calendar2$EventConfig = function (a) {
	return {$: 'EventConfig', a: a};
};
var author$project$Calendar2$eventConfig = function (_n0) {
	var onClick = _n0.onClick;
	var onMouseEnter = _n0.onMouseEnter;
	var onMouseLeave = _n0.onMouseLeave;
	var onDragStart = _n0.onDragStart;
	var onDragging = _n0.onDragging;
	var onDragEnd = _n0.onDragEnd;
	return author$project$Calendar2$EventConfig(
		{onClick: onClick, onDragEnd: onDragEnd, onDragStart: onDragStart, onDragging: onDragging, onMouseEnter: onMouseEnter, onMouseLeave: onMouseLeave});
};
var author$project$Main$ExtendEvent = F2(
	function (a, b) {
		return {$: 'ExtendEvent', a: a, b: b};
	});
var author$project$Main$ExtendingEvent = F2(
	function (a, b) {
		return {$: 'ExtendingEvent', a: a, b: b};
	});
var author$project$Main$SelectEvent = function (a) {
	return {$: 'SelectEvent', a: a};
};
var author$project$Main$eventConfig = author$project$Calendar2$eventConfig(
	{
		onClick: function (eventId) {
			return elm$core$Maybe$Just(
				author$project$Main$SelectEvent(eventId));
		},
		onDragEnd: F2(
			function (eventId, timeDiff) {
				return elm$core$Maybe$Just(
					A2(author$project$Main$ExtendEvent, eventId, timeDiff));
			}),
		onDragStart: function (_n0) {
			return elm$core$Maybe$Nothing;
		},
		onDragging: F2(
			function (eventId, timeDiff) {
				return elm$core$Maybe$Just(
					A2(author$project$Main$ExtendingEvent, eventId, timeDiff));
			}),
		onMouseEnter: function (_n1) {
			return elm$core$Maybe$Nothing;
		},
		onMouseLeave: function (_n2) {
			return elm$core$Maybe$Nothing;
		}
	});
var author$project$Main$removeEventPreview = function (model_) {
	return _Utils_update(
		model_,
		{eventPreview: elm$core$Maybe$Nothing});
};
var author$project$Calendar2$TimeSlotConfig = function (a) {
	return {$: 'TimeSlotConfig', a: a};
};
var author$project$Calendar2$timeSlotConfig = function (_n0) {
	var onClick = _n0.onClick;
	var onMouseEnter = _n0.onMouseEnter;
	var onMouseLeave = _n0.onMouseLeave;
	var onDragStart = _n0.onDragStart;
	var onDragging = _n0.onDragging;
	var onDragEnd = _n0.onDragEnd;
	return author$project$Calendar2$TimeSlotConfig(
		{onClick: onClick, onDragEnd: onDragEnd, onDragStart: onDragStart, onDragging: onDragging, onMouseEnter: onMouseEnter, onMouseLeave: onMouseLeave});
};
var author$project$Main$CreateEventPreview = F2(
	function (a, b) {
		return {$: 'CreateEventPreview', a: a, b: b};
	});
var author$project$Main$ExtendEventPreview = F2(
	function (a, b) {
		return {$: 'ExtendEventPreview', a: a, b: b};
	});
var author$project$Main$SelectDate = F2(
	function (a, b) {
		return {$: 'SelectDate', a: a, b: b};
	});
var author$project$Main$ShowCreateEventDialog = F2(
	function (a, b) {
		return {$: 'ShowCreateEventDialog', a: a, b: b};
	});
var author$project$Main$timeSlotConfig = author$project$Calendar2$timeSlotConfig(
	{
		onClick: F2(
			function (date, xy) {
				return elm$core$Maybe$Just(
					A2(author$project$Main$SelectDate, date, xy));
			}),
		onDragEnd: F2(
			function (date, xy) {
				return elm$core$Maybe$Just(
					A2(author$project$Main$ShowCreateEventDialog, date, xy));
			}),
		onDragStart: F2(
			function (date, xy) {
				return elm$core$Maybe$Just(
					A2(author$project$Main$CreateEventPreview, date, xy));
			}),
		onDragging: F2(
			function (date, xy) {
				return elm$core$Maybe$Just(
					A2(author$project$Main$ExtendEventPreview, date, xy));
			}),
		onMouseEnter: F2(
			function (_n0, _n1) {
				return elm$core$Maybe$Nothing;
			}),
		onMouseLeave: F2(
			function (_n2, _n3) {
				return elm$core$Maybe$Nothing;
			})
	});
var author$project$Main$Event = F4(
	function (id, title, start, end) {
		return {end: end, id: id, start: start, title: title};
	});
var author$project$Main$createEventPreview = F4(
	function (date, xy, minutes, model_) {
		var _n0 = A2(elm$core$Debug$log, 'createEventPreview:', date);
		var newEvent = A4(
			author$project$Main$Event,
			author$project$Main$newEventId(model_.curEventId),
			'',
			date,
			A4(justinmimbs$time_extra$Time$Extra$add, justinmimbs$time_extra$Time$Extra$Millisecond, minutes, elm$time$Time$utc, date));
		var eventPreview = {event: newEvent, position: xy, showDialog: false};
		return _Utils_update(
			model_,
			{
				eventPreview: elm$core$Maybe$Just(eventPreview)
			});
	});
var author$project$Main$extendEventPreview = F3(
	function (date, xy, model_) {
		var _n0 = A2(elm$core$Debug$log, 'extendEventPreview:', date);
		var extend = function (eventPreview) {
			var event = eventPreview.event;
			var position = eventPreview.position;
			return _Utils_update(
				eventPreview,
				{
					event: _Utils_update(
						event,
						{
							end: A2(elm$core$Debug$log, 'finalEnd', date)
						})
				});
		};
		return _Utils_update(
			model_,
			{
				eventPreview: A2(elm$core$Maybe$map, extend, model_.eventPreview)
			});
	});
var elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _n1 = A2(elm$core$Basics$compare, targetKey, key);
				switch (_n1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var author$project$Main$selectEvent = F2(
	function (eventId, model_) {
		return _Utils_update(
			model_,
			{
				selectedEvent: A2(elm$core$Dict$get, eventId, model_.events)
			});
	});
var elm$core$Basics$not = _Basics_not;
var author$project$Main$toggleEventPreviewDialog = function (eventPreview) {
	return _Utils_update(
		eventPreview,
		{showDialog: !eventPreview.showDialog});
};
var author$project$Main$showCreateEventDialog = function (model_) {
	return _Utils_update(
		model_,
		{
			eventPreview: A2(elm$core$Maybe$map, author$project$Main$toggleEventPreviewDialog, model_.eventPreview)
		});
};
var author$project$Main$updateCalendar = F2(
	function (msg, model_) {
		var _n0 = A2(elm$core$Debug$log, 'calendarMsg:', msg);
		switch (msg.$) {
			case 'SelectDate':
				var date = msg.a;
				var xy = msg.b;
				return author$project$Main$showCreateEventDialog(
					A4(author$project$Main$createEventPreview, date, xy, 60, model_));
			case 'CreateEventPreview':
				var date = msg.a;
				var xy = msg.b;
				return A4(author$project$Main$createEventPreview, date, xy, 30, model_);
			case 'ExtendEventPreview':
				var date = msg.a;
				var xy = msg.b;
				return A3(author$project$Main$extendEventPreview, date, xy, model_);
			case 'ShowCreateEventDialog':
				var date = msg.a;
				var xy = msg.b;
				return author$project$Main$showCreateEventDialog(
					A3(author$project$Main$extendEventPreview, date, xy, model_));
			case 'SelectEvent':
				var eventId = msg.a;
				return A2(author$project$Main$selectEvent, eventId, model_);
			case 'ExtendingEvent':
				var timeDiff = msg.b;
				return _Utils_update(
					model_,
					{
						eventExtendAmount: elm$time$Time$millisToPosix(timeDiff)
					});
			default:
				var eventId = msg.a;
				var timeDiff = msg.b;
				var newEnd = function (end) {
					return elm$time$Time$millisToPosix(
						timeDiff + elm$time$Time$posixToMillis(end));
				};
				var maybeEvent = A2(elm$core$Dict$get, eventId, model_.events);
				var extendEvent = function (event) {
					return _Utils_update(
						event,
						{
							end: newEnd(event.end)
						});
				};
				var updateEvents = function (event) {
					return A3(
						elm$core$Dict$insert,
						eventId,
						extendEvent(event),
						model_.events);
				};
				if (maybeEvent.$ === 'Nothing') {
					return model_;
				} else {
					var event = maybeEvent.a;
					return _Utils_update(
						model_,
						{
							events: updateEvents(event)
						});
				}
		}
	});
var ohanhi$keyboard$Keyboard$Backspace = {$: 'Backspace'};
var ohanhi$keyboard$Keyboard$Clear = {$: 'Clear'};
var ohanhi$keyboard$Keyboard$Copy = {$: 'Copy'};
var ohanhi$keyboard$Keyboard$CrSel = {$: 'CrSel'};
var ohanhi$keyboard$Keyboard$Cut = {$: 'Cut'};
var ohanhi$keyboard$Keyboard$Delete = {$: 'Delete'};
var ohanhi$keyboard$Keyboard$EraseEof = {$: 'EraseEof'};
var ohanhi$keyboard$Keyboard$ExSel = {$: 'ExSel'};
var ohanhi$keyboard$Keyboard$Insert = {$: 'Insert'};
var ohanhi$keyboard$Keyboard$Paste = {$: 'Paste'};
var ohanhi$keyboard$Keyboard$Redo = {$: 'Redo'};
var ohanhi$keyboard$Keyboard$Undo = {$: 'Undo'};
var ohanhi$keyboard$Keyboard$editingKey = function (_n0) {
	var value = _n0.a;
	switch (value) {
		case 'Backspace':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Backspace);
		case 'Clear':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Clear);
		case 'Copy':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Copy);
		case 'CrSel':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$CrSel);
		case 'Cut':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Cut);
		case 'Delete':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Delete);
		case 'EraseEof':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$EraseEof);
		case 'ExSel':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ExSel);
		case 'Insert':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Insert);
		case 'Paste':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Paste);
		case 'Redo':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Redo);
		case 'Undo':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Undo);
		default:
			return elm$core$Maybe$Nothing;
	}
};
var ohanhi$keyboard$Keyboard$F1 = {$: 'F1'};
var ohanhi$keyboard$Keyboard$F10 = {$: 'F10'};
var ohanhi$keyboard$Keyboard$F11 = {$: 'F11'};
var ohanhi$keyboard$Keyboard$F12 = {$: 'F12'};
var ohanhi$keyboard$Keyboard$F13 = {$: 'F13'};
var ohanhi$keyboard$Keyboard$F14 = {$: 'F14'};
var ohanhi$keyboard$Keyboard$F15 = {$: 'F15'};
var ohanhi$keyboard$Keyboard$F16 = {$: 'F16'};
var ohanhi$keyboard$Keyboard$F17 = {$: 'F17'};
var ohanhi$keyboard$Keyboard$F18 = {$: 'F18'};
var ohanhi$keyboard$Keyboard$F19 = {$: 'F19'};
var ohanhi$keyboard$Keyboard$F2 = {$: 'F2'};
var ohanhi$keyboard$Keyboard$F20 = {$: 'F20'};
var ohanhi$keyboard$Keyboard$F3 = {$: 'F3'};
var ohanhi$keyboard$Keyboard$F4 = {$: 'F4'};
var ohanhi$keyboard$Keyboard$F5 = {$: 'F5'};
var ohanhi$keyboard$Keyboard$F6 = {$: 'F6'};
var ohanhi$keyboard$Keyboard$F7 = {$: 'F7'};
var ohanhi$keyboard$Keyboard$F8 = {$: 'F8'};
var ohanhi$keyboard$Keyboard$F9 = {$: 'F9'};
var ohanhi$keyboard$Keyboard$functionKey = function (_n0) {
	var value = _n0.a;
	switch (value) {
		case 'F1':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F1);
		case 'F2':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F2);
		case 'F3':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F3);
		case 'F4':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F4);
		case 'F5':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F5);
		case 'F6':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F6);
		case 'F7':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F7);
		case 'F8':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F8);
		case 'F9':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F9);
		case 'F10':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F10);
		case 'F11':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F11);
		case 'F12':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F12);
		case 'F13':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F13);
		case 'F14':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F14);
		case 'F15':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F15);
		case 'F16':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F16);
		case 'F17':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F17);
		case 'F18':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F18);
		case 'F19':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F19);
		case 'F20':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$F20);
		default:
			return elm$core$Maybe$Nothing;
	}
};
var ohanhi$keyboard$Keyboard$ChannelDown = {$: 'ChannelDown'};
var ohanhi$keyboard$Keyboard$ChannelUp = {$: 'ChannelUp'};
var ohanhi$keyboard$Keyboard$MediaFastForward = {$: 'MediaFastForward'};
var ohanhi$keyboard$Keyboard$MediaPause = {$: 'MediaPause'};
var ohanhi$keyboard$Keyboard$MediaPlay = {$: 'MediaPlay'};
var ohanhi$keyboard$Keyboard$MediaPlayPause = {$: 'MediaPlayPause'};
var ohanhi$keyboard$Keyboard$MediaRecord = {$: 'MediaRecord'};
var ohanhi$keyboard$Keyboard$MediaRewind = {$: 'MediaRewind'};
var ohanhi$keyboard$Keyboard$MediaStop = {$: 'MediaStop'};
var ohanhi$keyboard$Keyboard$MediaTrackNext = {$: 'MediaTrackNext'};
var ohanhi$keyboard$Keyboard$MediaTrackPrevious = {$: 'MediaTrackPrevious'};
var ohanhi$keyboard$Keyboard$mediaKey = function (_n0) {
	var value = _n0.a;
	switch (value) {
		case 'ChannelDown':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ChannelDown);
		case 'ChannelUp':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ChannelUp);
		case 'MediaFastForward':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$MediaFastForward);
		case 'MediaPause':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$MediaPause);
		case 'MediaPlay':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$MediaPlay);
		case 'MediaPlayPause':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$MediaPlayPause);
		case 'MediaRecord':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$MediaRecord);
		case 'MediaRewind':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$MediaRewind);
		case 'MediaStop':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$MediaStop);
		case 'MediaTrackNext':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$MediaTrackNext);
		case 'MediaTrackPrevious':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$MediaTrackPrevious);
		default:
			return elm$core$Maybe$Nothing;
	}
};
var ohanhi$keyboard$Keyboard$Alt = {$: 'Alt'};
var ohanhi$keyboard$Keyboard$AltGraph = {$: 'AltGraph'};
var ohanhi$keyboard$Keyboard$CapsLock = {$: 'CapsLock'};
var ohanhi$keyboard$Keyboard$Control = {$: 'Control'};
var ohanhi$keyboard$Keyboard$Fn = {$: 'Fn'};
var ohanhi$keyboard$Keyboard$FnLock = {$: 'FnLock'};
var ohanhi$keyboard$Keyboard$Hyper = {$: 'Hyper'};
var ohanhi$keyboard$Keyboard$Meta = {$: 'Meta'};
var ohanhi$keyboard$Keyboard$NumLock = {$: 'NumLock'};
var ohanhi$keyboard$Keyboard$ScrollLock = {$: 'ScrollLock'};
var ohanhi$keyboard$Keyboard$Shift = {$: 'Shift'};
var ohanhi$keyboard$Keyboard$Super = {$: 'Super'};
var ohanhi$keyboard$Keyboard$Symbol = {$: 'Symbol'};
var ohanhi$keyboard$Keyboard$SymbolLock = {$: 'SymbolLock'};
var ohanhi$keyboard$Keyboard$modifierKey = function (_n0) {
	var value = _n0.a;
	switch (value) {
		case 'Alt':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Alt);
		case 'AltGraph':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$AltGraph);
		case 'CapsLock':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$CapsLock);
		case 'Control':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Control);
		case 'Fn':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Fn);
		case 'FnLock':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$FnLock);
		case 'Hyper':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Hyper);
		case 'Meta':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Meta);
		case 'NumLock':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$NumLock);
		case 'ScrollLock':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ScrollLock);
		case 'Shift':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Shift);
		case 'Super':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Super);
		case 'OS':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Super);
		case 'Symbol':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Symbol);
		case 'SymbolLock':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$SymbolLock);
		default:
			return elm$core$Maybe$Nothing;
	}
};
var ohanhi$keyboard$Keyboard$ArrowDown = {$: 'ArrowDown'};
var ohanhi$keyboard$Keyboard$ArrowLeft = {$: 'ArrowLeft'};
var ohanhi$keyboard$Keyboard$ArrowRight = {$: 'ArrowRight'};
var ohanhi$keyboard$Keyboard$ArrowUp = {$: 'ArrowUp'};
var ohanhi$keyboard$Keyboard$End = {$: 'End'};
var ohanhi$keyboard$Keyboard$Home = {$: 'Home'};
var ohanhi$keyboard$Keyboard$PageDown = {$: 'PageDown'};
var ohanhi$keyboard$Keyboard$PageUp = {$: 'PageUp'};
var ohanhi$keyboard$Keyboard$navigationKey = function (_n0) {
	var value = _n0.a;
	switch (value) {
		case 'ArrowDown':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ArrowDown);
		case 'ArrowLeft':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ArrowLeft);
		case 'ArrowRight':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ArrowRight);
		case 'ArrowUp':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ArrowUp);
		case 'Down':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ArrowDown);
		case 'Left':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ArrowLeft);
		case 'Right':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ArrowRight);
		case 'Up':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ArrowUp);
		case 'End':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$End);
		case 'Home':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Home);
		case 'PageDown':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$PageDown);
		case 'PageUp':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$PageUp);
		default:
			return elm$core$Maybe$Nothing;
	}
};
var ohanhi$keyboard$Keyboard$oneOf = F2(
	function (fns, key) {
		oneOf:
		while (true) {
			if (!fns.b) {
				return elm$core$Maybe$Nothing;
			} else {
				var fn = fns.a;
				var rest = fns.b;
				var _n1 = fn(key);
				if (_n1.$ === 'Just') {
					var a = _n1.a;
					return elm$core$Maybe$Just(a);
				} else {
					var $temp$fns = rest,
						$temp$key = key;
					fns = $temp$fns;
					key = $temp$key;
					continue oneOf;
				}
			}
		}
	});
var ohanhi$keyboard$Keyboard$AppSwitch = {$: 'AppSwitch'};
var ohanhi$keyboard$Keyboard$Call = {$: 'Call'};
var ohanhi$keyboard$Keyboard$Camera = {$: 'Camera'};
var ohanhi$keyboard$Keyboard$CameraFocus = {$: 'CameraFocus'};
var ohanhi$keyboard$Keyboard$EndCall = {$: 'EndCall'};
var ohanhi$keyboard$Keyboard$GoBack = {$: 'GoBack'};
var ohanhi$keyboard$Keyboard$GoHome = {$: 'GoHome'};
var ohanhi$keyboard$Keyboard$HeadsetHook = {$: 'HeadsetHook'};
var ohanhi$keyboard$Keyboard$LastNumberRedial = {$: 'LastNumberRedial'};
var ohanhi$keyboard$Keyboard$MannerMode = {$: 'MannerMode'};
var ohanhi$keyboard$Keyboard$Notification = {$: 'Notification'};
var ohanhi$keyboard$Keyboard$VoiceDial = {$: 'VoiceDial'};
var ohanhi$keyboard$Keyboard$phoneKey = function (_n0) {
	var value = _n0.a;
	switch (value) {
		case 'AppSwitch':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$AppSwitch);
		case 'Call':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Call);
		case 'Camera':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Camera);
		case 'CameraFocus':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$CameraFocus);
		case 'EndCall':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$EndCall);
		case 'GoBack':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$GoBack);
		case 'GoHome':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$GoHome);
		case 'HeadsetHook':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$HeadsetHook);
		case 'LastNumberRedial':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$LastNumberRedial);
		case 'Notification':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Notification);
		case 'MannerMode':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$MannerMode);
		case 'VoiceDial':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$VoiceDial);
		default:
			return elm$core$Maybe$Nothing;
	}
};
var ohanhi$keyboard$Keyboard$Again = {$: 'Again'};
var ohanhi$keyboard$Keyboard$Attn = {$: 'Attn'};
var ohanhi$keyboard$Keyboard$Cancel = {$: 'Cancel'};
var ohanhi$keyboard$Keyboard$ContextMenu = {$: 'ContextMenu'};
var ohanhi$keyboard$Keyboard$Escape = {$: 'Escape'};
var ohanhi$keyboard$Keyboard$Execute = {$: 'Execute'};
var ohanhi$keyboard$Keyboard$Find = {$: 'Find'};
var ohanhi$keyboard$Keyboard$Finish = {$: 'Finish'};
var ohanhi$keyboard$Keyboard$Help = {$: 'Help'};
var ohanhi$keyboard$Keyboard$Pause = {$: 'Pause'};
var ohanhi$keyboard$Keyboard$Play = {$: 'Play'};
var ohanhi$keyboard$Keyboard$Props = {$: 'Props'};
var ohanhi$keyboard$Keyboard$Select = {$: 'Select'};
var ohanhi$keyboard$Keyboard$ZoomIn = {$: 'ZoomIn'};
var ohanhi$keyboard$Keyboard$ZoomOut = {$: 'ZoomOut'};
var ohanhi$keyboard$Keyboard$uiKey = function (_n0) {
	var value = _n0.a;
	switch (value) {
		case 'Again':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Again);
		case 'Attn':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Attn);
		case 'Cancel':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Cancel);
		case 'ContextMenu':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ContextMenu);
		case 'Escape':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Escape);
		case 'Execute':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Execute);
		case 'Find':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Find);
		case 'Finish':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Finish);
		case 'Help':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Help);
		case 'Pause':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Pause);
		case 'Play':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Play);
		case 'Props':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Props);
		case 'Select':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Select);
		case 'ZoomIn':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ZoomIn);
		case 'ZoomOut':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$ZoomOut);
		default:
			return elm$core$Maybe$Nothing;
	}
};
var ohanhi$keyboard$Keyboard$Enter = {$: 'Enter'};
var ohanhi$keyboard$Keyboard$Spacebar = {$: 'Spacebar'};
var ohanhi$keyboard$Keyboard$Tab = {$: 'Tab'};
var ohanhi$keyboard$Keyboard$whitespaceKey = function (_n0) {
	var value = _n0.a;
	switch (value) {
		case 'Enter':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Enter);
		case 'Tab':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Tab);
		case 'Spacebar':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Spacebar);
		case ' ':
			return elm$core$Maybe$Just(ohanhi$keyboard$Keyboard$Spacebar);
		default:
			return elm$core$Maybe$Nothing;
	}
};
var ohanhi$keyboard$Keyboard$anyKeyWith = function (charParser) {
	return ohanhi$keyboard$Keyboard$oneOf(
		_List_fromArray(
			[ohanhi$keyboard$Keyboard$whitespaceKey, charParser, ohanhi$keyboard$Keyboard$modifierKey, ohanhi$keyboard$Keyboard$navigationKey, ohanhi$keyboard$Keyboard$editingKey, ohanhi$keyboard$Keyboard$functionKey, ohanhi$keyboard$Keyboard$uiKey, ohanhi$keyboard$Keyboard$phoneKey, ohanhi$keyboard$Keyboard$mediaKey]));
};
var ohanhi$keyboard$Keyboard$Character = function (a) {
	return {$: 'Character', a: a};
};
var ohanhi$keyboard$Keyboard$characterKeyOriginal = function (_n0) {
	var value = _n0.a;
	return (elm$core$String$length(value) === 1) ? elm$core$Maybe$Just(
		ohanhi$keyboard$Keyboard$Character(value)) : elm$core$Maybe$Nothing;
};
var ohanhi$keyboard$Keyboard$anyKeyOriginal = ohanhi$keyboard$Keyboard$anyKeyWith(ohanhi$keyboard$Keyboard$characterKeyOriginal);
var author$project$Main$pureUpdate = F2(
	function (msg, model_) {
		switch (msg.$) {
			case 'SetCalendarState':
				var calendarMsg = msg.a;
				var _n1 = A4(author$project$Calendar2$update, author$project$Main$eventConfig, author$project$Main$timeSlotConfig, calendarMsg, model_.calendarState);
				var updatedCalendar = _n1.a;
				var maybeMsg = _n1.b;
				var newModel = _Utils_update(
					model_,
					{calendarState: updatedCalendar});
				if (maybeMsg.$ === 'Nothing') {
					return newModel;
				} else {
					var updateMsg = maybeMsg.a;
					return A2(author$project$Main$updateCalendar, updateMsg, newModel);
				}
			case 'CreateEventTitle':
				var title = msg.a;
				return A2(author$project$Main$changeEventPreviewTitle, title, model_);
			case 'AddEventPreviewToEvents':
				return author$project$Main$removeEventPreview(
					author$project$Main$addEventPreviewToEvents(model_));
			default:
				var keyCode = msg.a;
				var key = ohanhi$keyboard$Keyboard$anyKeyOriginal(keyCode);
				if (key.$ === 'Just') {
					if (key.a.$ === 'Alt') {
						var _n4 = key.a;
						return author$project$Main$removeEventPreview(model_);
					} else {
						return model_;
					}
				} else {
					return model_;
				}
		}
	});
var author$project$Main$update = F2(
	function (msg, model_) {
		return _Utils_Tuple2(
			A2(author$project$Main$pureUpdate, msg, model_),
			elm$core$Platform$Cmd$none);
	});
var author$project$Calendar2$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var elm$core$List$sortBy = _List_sortBy;
var justinmimbs$date$Date$Day = {$: 'Day'};
var elm$time$Time$Fri = {$: 'Fri'};
var elm$time$Time$Mon = {$: 'Mon'};
var elm$time$Time$Sat = {$: 'Sat'};
var elm$time$Time$Sun = {$: 'Sun'};
var elm$time$Time$Thu = {$: 'Thu'};
var elm$time$Time$Tue = {$: 'Tue'};
var elm$time$Time$Wed = {$: 'Wed'};
var justinmimbs$date$Date$weekdayNumber = function (_n0) {
	var rd = _n0.a;
	var _n1 = A2(elm$core$Basics$modBy, 7, rd);
	if (!_n1) {
		return 7;
	} else {
		var n = _n1;
		return n;
	}
};
var justinmimbs$date$Date$weekdayToNumber = function (wd) {
	switch (wd.$) {
		case 'Mon':
			return 1;
		case 'Tue':
			return 2;
		case 'Wed':
			return 3;
		case 'Thu':
			return 4;
		case 'Fri':
			return 5;
		case 'Sat':
			return 6;
		default:
			return 7;
	}
};
var justinmimbs$date$Date$daysSincePreviousWeekday = F2(
	function (wd, date) {
		return A2(
			elm$core$Basics$modBy,
			7,
			(justinmimbs$date$Date$weekdayNumber(date) + 7) - justinmimbs$date$Date$weekdayToNumber(wd));
	});
var justinmimbs$date$Date$firstOfMonth = F2(
	function (y, m) {
		return justinmimbs$date$Date$RD(
			(justinmimbs$date$Date$daysBeforeYear(y) + A2(justinmimbs$date$Date$daysBeforeMonth, y, m)) + 1);
	});
var justinmimbs$date$Date$firstOfYear = function (y) {
	return justinmimbs$date$Date$RD(
		justinmimbs$date$Date$daysBeforeYear(y) + 1);
};
var elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var justinmimbs$date$Date$month = A2(
	elm$core$Basics$composeR,
	justinmimbs$date$Date$toCalendarDate,
	function ($) {
		return $.month;
	});
var justinmimbs$date$Date$monthToQuarter = function (m) {
	return ((justinmimbs$date$Date$monthToNumber(m) + 2) / 3) | 0;
};
var justinmimbs$date$Date$quarter = A2(elm$core$Basics$composeR, justinmimbs$date$Date$month, justinmimbs$date$Date$monthToQuarter);
var justinmimbs$date$Date$quarterToMonth = function (q) {
	return justinmimbs$date$Date$numberToMonth((q * 3) - 2);
};
var justinmimbs$date$Date$floor = F2(
	function (interval, date) {
		var rd = date.a;
		switch (interval.$) {
			case 'Year':
				return justinmimbs$date$Date$firstOfYear(
					justinmimbs$date$Date$year(date));
			case 'Quarter':
				return A2(
					justinmimbs$date$Date$firstOfMonth,
					justinmimbs$date$Date$year(date),
					justinmimbs$date$Date$quarterToMonth(
						justinmimbs$date$Date$quarter(date)));
			case 'Month':
				return A2(
					justinmimbs$date$Date$firstOfMonth,
					justinmimbs$date$Date$year(date),
					justinmimbs$date$Date$month(date));
			case 'Week':
				return justinmimbs$date$Date$RD(
					rd - A2(justinmimbs$date$Date$daysSincePreviousWeekday, elm$time$Time$Mon, date));
			case 'Monday':
				return justinmimbs$date$Date$RD(
					rd - A2(justinmimbs$date$Date$daysSincePreviousWeekday, elm$time$Time$Mon, date));
			case 'Tuesday':
				return justinmimbs$date$Date$RD(
					rd - A2(justinmimbs$date$Date$daysSincePreviousWeekday, elm$time$Time$Tue, date));
			case 'Wednesday':
				return justinmimbs$date$Date$RD(
					rd - A2(justinmimbs$date$Date$daysSincePreviousWeekday, elm$time$Time$Wed, date));
			case 'Thursday':
				return justinmimbs$date$Date$RD(
					rd - A2(justinmimbs$date$Date$daysSincePreviousWeekday, elm$time$Time$Thu, date));
			case 'Friday':
				return justinmimbs$date$Date$RD(
					rd - A2(justinmimbs$date$Date$daysSincePreviousWeekday, elm$time$Time$Fri, date));
			case 'Saturday':
				return justinmimbs$date$Date$RD(
					rd - A2(justinmimbs$date$Date$daysSincePreviousWeekday, elm$time$Time$Sat, date));
			case 'Sunday':
				return justinmimbs$date$Date$RD(
					rd - A2(justinmimbs$date$Date$daysSincePreviousWeekday, elm$time$Time$Sun, date));
			default:
				return date;
		}
	});
var justinmimbs$date$Date$isBetweenInt = F3(
	function (a, b, x) {
		return (_Utils_cmp(a, x) < 1) && (_Utils_cmp(x, b) < 1);
	});
var justinmimbs$date$Date$isBetween = F3(
	function (_n0, _n1, _n2) {
		var a = _n0.a;
		var b = _n1.a;
		var x = _n2.a;
		return A3(justinmimbs$date$Date$isBetweenInt, a, b, x);
	});
var author$project$Calendar2$Agenda$eventsGroupedByDate = F2(
	function (config, events) {
		var initEventGroup = function (event) {
			return {
				date: A2(
					justinmimbs$date$Date$floor,
					justinmimbs$date$Date$Day,
					A2(
						justinmimbs$date$Date$fromPosix,
						elm$time$Time$utc,
						config.start(event))),
				events: _List_fromArray(
					[event])
			};
		};
		var buildEventGroups = F2(
			function (event, eventGroups) {
				var eventStart = A2(
					justinmimbs$date$Date$fromPosix,
					elm$time$Time$utc,
					config.start(event));
				var isEventPartOfGroup = function (eventGroup) {
					return A3(
						justinmimbs$date$Date$isBetween,
						eventGroup.date,
						A3(justinmimbs$date$Date$add, justinmimbs$date$Date$Days, 1, eventGroup.date),
						eventStart);
				};
				if (!eventGroups.b) {
					return _List_fromArray(
						[
							initEventGroup(event)
						]);
				} else {
					var eventGroup = eventGroups.a;
					var restOfEventGroups = eventGroups.b;
					return isEventPartOfGroup(eventGroup) ? A2(
						elm$core$List$cons,
						_Utils_update(
							eventGroup,
							{
								events: A2(elm$core$List$cons, event, eventGroup.events)
							}),
						restOfEventGroups) : A2(
						elm$core$List$cons,
						initEventGroup(event),
						eventGroups);
				}
			});
		return A3(
			elm$core$List$foldr,
			buildEventGroups,
			_List_Nil,
			A2(
				elm$core$List$sortBy,
				A2(elm$core$Basics$composeL, elm$time$Time$posixToMillis, config.start),
				events));
	});
var elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var elm$html$Html$text = elm$virtual_dom$VirtualDom$text;
var elm$html$Html$th = _VirtualDom_node('th');
var elm$html$Html$thead = _VirtualDom_node('thead');
var elm$html$Html$tr = _VirtualDom_node('tr');
var elm$json$Json$Encode$string = _Json_wrap;
var elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			elm$json$Json$Encode$string(string));
	});
var elm$html$Html$Attributes$class = elm$html$Html$Attributes$stringProperty('className');
var author$project$Calendar2$Agenda$viewAgendaHeader = A2(
	elm$html$Html$thead,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('elm-calendar--agenda-header')
		]),
	_List_fromArray(
		[
			A2(
			elm$html$Html$tr,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					elm$html$Html$th,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('elm-calendar--header-cell')
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Date')
						])),
					A2(
					elm$html$Html$th,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('elm-calendar--header-cell')
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Time')
						])),
					A2(
					elm$html$Html$th,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('elm-calendar--header-cell')
						]),
					_List_fromArray(
						[
							elm$html$Html$text('Event')
						]))
				]))
		]));
var author$project$Helpers$hourString = function (posix) {
	var mm = elm$core$String$fromInt(
		A2(elm$time$Time$toMinute, elm$time$Time$utc, posix));
	var hh = elm$core$String$fromInt(
		A2(elm$time$Time$toHour, elm$time$Time$utc, posix));
	return hh + (':' + '00');
};
var elm$html$Html$td = _VirtualDom_node('td');
var author$project$Calendar2$Agenda$viewTimeAndEvent = F2(
	function (config, event) {
		var startTime = author$project$Helpers$hourString(
			config.start(event));
		var endTime = author$project$Helpers$hourString(
			config.end(event));
		var timeRange = startTime + (' - ' + endTime);
		return _List_fromArray(
			[
				A2(
				elm$html$Html$td,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('elm-calendar--agenda-cell')
					]),
				_List_fromArray(
					[
						elm$html$Html$text(timeRange)
					])),
				A2(
				elm$html$Html$td,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('elm-calendar--agenda-cell')
					]),
				_List_fromArray(
					[
						elm$html$Html$text(
						config.title(event))
					]))
			]);
	});
var author$project$Calendar2$Agenda$viewAgendaRow = F2(
	function (config, event) {
		return A2(
			elm$html$Html$tr,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--agenda-day')
				]),
			A2(author$project$Calendar2$Agenda$viewTimeAndEvent, config, event));
	});
var elm$html$Html$Attributes$rowspan = function (n) {
	return A2(
		_VirtualDom_attribute,
		'rowspan',
		elm$core$String$fromInt(n));
};
var elm$core$String$cons = _String_cons;
var elm$core$String$fromChar = function (_char) {
	return A2(elm$core$String$cons, _char, '');
};
var elm$core$Bitwise$and = _Bitwise_and;
var elm$core$Bitwise$shiftRightBy = _Bitwise_shiftRightBy;
var elm$core$String$repeatHelp = F3(
	function (n, chunk, result) {
		return (n <= 0) ? result : A3(
			elm$core$String$repeatHelp,
			n >> 1,
			_Utils_ap(chunk, chunk),
			(!(n & 1)) ? result : _Utils_ap(result, chunk));
	});
var elm$core$String$repeat = F2(
	function (n, chunk) {
		return A3(elm$core$String$repeatHelp, n, chunk, '');
	});
var elm$core$String$padLeft = F3(
	function (n, _char, string) {
		return _Utils_ap(
			A2(
				elm$core$String$repeat,
				n - elm$core$String$length(string),
				elm$core$String$fromChar(_char)),
			string);
	});
var elm$core$String$right = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(
			elm$core$String$slice,
			-n,
			elm$core$String$length(string),
			string);
	});
var justinmimbs$date$Date$day = A2(
	elm$core$Basics$composeR,
	justinmimbs$date$Date$toCalendarDate,
	function ($) {
		return $.day;
	});
var justinmimbs$date$Date$monthNumber = A2(elm$core$Basics$composeR, justinmimbs$date$Date$month, justinmimbs$date$Date$monthToNumber);
var justinmimbs$date$Date$ordinalDay = A2(
	elm$core$Basics$composeR,
	justinmimbs$date$Date$toOrdinalDate,
	function ($) {
		return $.ordinalDay;
	});
var elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var justinmimbs$date$Date$padSignedInt = F2(
	function (length, _int) {
		return _Utils_ap(
			(_int < 0) ? '-' : '',
			A3(
				elm$core$String$padLeft,
				length,
				_Utils_chr('0'),
				elm$core$String$fromInt(
					elm$core$Basics$abs(_int))));
	});
var justinmimbs$date$Date$daysBeforeWeekYear = function (y) {
	var jan4 = justinmimbs$date$Date$daysBeforeYear(y) + 4;
	return jan4 - justinmimbs$date$Date$weekdayNumber(
		justinmimbs$date$Date$RD(jan4));
};
var justinmimbs$date$Date$numberToWeekday = function (wdn) {
	var _n0 = A2(elm$core$Basics$max, 1, wdn);
	switch (_n0) {
		case 1:
			return elm$time$Time$Mon;
		case 2:
			return elm$time$Time$Tue;
		case 3:
			return elm$time$Time$Wed;
		case 4:
			return elm$time$Time$Thu;
		case 5:
			return elm$time$Time$Fri;
		case 6:
			return elm$time$Time$Sat;
		default:
			return elm$time$Time$Sun;
	}
};
var justinmimbs$date$Date$toWeekDate = function (_n0) {
	var rd = _n0.a;
	var wdn = justinmimbs$date$Date$weekdayNumber(
		justinmimbs$date$Date$RD(rd));
	var wy = justinmimbs$date$Date$year(
		justinmimbs$date$Date$RD(rd + (4 - wdn)));
	var week1Day1 = justinmimbs$date$Date$daysBeforeWeekYear(wy) + 1;
	return {
		weekNumber: 1 + (((rd - week1Day1) / 7) | 0),
		weekYear: wy,
		weekday: justinmimbs$date$Date$numberToWeekday(wdn)
	};
};
var justinmimbs$date$Date$weekNumber = A2(
	elm$core$Basics$composeR,
	justinmimbs$date$Date$toWeekDate,
	function ($) {
		return $.weekNumber;
	});
var justinmimbs$date$Date$weekYear = A2(
	elm$core$Basics$composeR,
	justinmimbs$date$Date$toWeekDate,
	function ($) {
		return $.weekYear;
	});
var justinmimbs$date$Date$weekday = A2(elm$core$Basics$composeR, justinmimbs$date$Date$weekdayNumber, justinmimbs$date$Date$numberToWeekday);
var justinmimbs$date$Date$ordinalSuffix = function (n) {
	var nn = A2(elm$core$Basics$modBy, 100, n);
	var _n0 = A2(
		elm$core$Basics$min,
		(nn < 20) ? nn : A2(elm$core$Basics$modBy, 10, nn),
		4);
	switch (_n0) {
		case 1:
			return 'st';
		case 2:
			return 'nd';
		case 3:
			return 'rd';
		default:
			return 'th';
	}
};
var justinmimbs$date$Date$withOrdinalSuffix = function (n) {
	return _Utils_ap(
		elm$core$String$fromInt(n),
		justinmimbs$date$Date$ordinalSuffix(n));
};
var justinmimbs$date$Date$formatField = F4(
	function (language, _char, length, date) {
		switch (_char.valueOf()) {
			case 'y':
				if (length === 2) {
					return A2(
						elm$core$String$right,
						2,
						A3(
							elm$core$String$padLeft,
							2,
							_Utils_chr('0'),
							elm$core$String$fromInt(
								justinmimbs$date$Date$year(date))));
				} else {
					return A2(
						justinmimbs$date$Date$padSignedInt,
						length,
						justinmimbs$date$Date$year(date));
				}
			case 'Y':
				if (length === 2) {
					return A2(
						elm$core$String$right,
						2,
						A3(
							elm$core$String$padLeft,
							2,
							_Utils_chr('0'),
							elm$core$String$fromInt(
								justinmimbs$date$Date$weekYear(date))));
				} else {
					return A2(
						justinmimbs$date$Date$padSignedInt,
						length,
						justinmimbs$date$Date$weekYear(date));
				}
			case 'Q':
				switch (length) {
					case 1:
						return elm$core$String$fromInt(
							justinmimbs$date$Date$quarter(date));
					case 2:
						return elm$core$String$fromInt(
							justinmimbs$date$Date$quarter(date));
					case 3:
						return 'Q' + elm$core$String$fromInt(
							justinmimbs$date$Date$quarter(date));
					case 4:
						return justinmimbs$date$Date$withOrdinalSuffix(
							justinmimbs$date$Date$quarter(date));
					case 5:
						return elm$core$String$fromInt(
							justinmimbs$date$Date$quarter(date));
					default:
						return '';
				}
			case 'M':
				switch (length) {
					case 1:
						return elm$core$String$fromInt(
							justinmimbs$date$Date$monthNumber(date));
					case 2:
						return A3(
							elm$core$String$padLeft,
							2,
							_Utils_chr('0'),
							elm$core$String$fromInt(
								justinmimbs$date$Date$monthNumber(date)));
					case 3:
						return language.monthNameShort(
							justinmimbs$date$Date$month(date));
					case 4:
						return language.monthName(
							justinmimbs$date$Date$month(date));
					case 5:
						return A2(
							elm$core$String$left,
							1,
							language.monthNameShort(
								justinmimbs$date$Date$month(date)));
					default:
						return '';
				}
			case 'w':
				switch (length) {
					case 1:
						return elm$core$String$fromInt(
							justinmimbs$date$Date$weekNumber(date));
					case 2:
						return A3(
							elm$core$String$padLeft,
							2,
							_Utils_chr('0'),
							elm$core$String$fromInt(
								justinmimbs$date$Date$weekNumber(date)));
					default:
						return '';
				}
			case 'd':
				switch (length) {
					case 1:
						return elm$core$String$fromInt(
							justinmimbs$date$Date$day(date));
					case 2:
						return A3(
							elm$core$String$padLeft,
							2,
							_Utils_chr('0'),
							elm$core$String$fromInt(
								justinmimbs$date$Date$day(date)));
					case 3:
						return language.dayWithSuffix(
							justinmimbs$date$Date$day(date));
					default:
						return '';
				}
			case 'D':
				switch (length) {
					case 1:
						return elm$core$String$fromInt(
							justinmimbs$date$Date$ordinalDay(date));
					case 2:
						return A3(
							elm$core$String$padLeft,
							2,
							_Utils_chr('0'),
							elm$core$String$fromInt(
								justinmimbs$date$Date$ordinalDay(date)));
					case 3:
						return A3(
							elm$core$String$padLeft,
							3,
							_Utils_chr('0'),
							elm$core$String$fromInt(
								justinmimbs$date$Date$ordinalDay(date)));
					default:
						return '';
				}
			case 'E':
				switch (length) {
					case 1:
						return language.weekdayNameShort(
							justinmimbs$date$Date$weekday(date));
					case 2:
						return language.weekdayNameShort(
							justinmimbs$date$Date$weekday(date));
					case 3:
						return language.weekdayNameShort(
							justinmimbs$date$Date$weekday(date));
					case 4:
						return language.weekdayName(
							justinmimbs$date$Date$weekday(date));
					case 5:
						return A2(
							elm$core$String$left,
							1,
							language.weekdayNameShort(
								justinmimbs$date$Date$weekday(date)));
					case 6:
						return A2(
							elm$core$String$left,
							2,
							language.weekdayNameShort(
								justinmimbs$date$Date$weekday(date)));
					default:
						return '';
				}
			case 'e':
				switch (length) {
					case 1:
						return elm$core$String$fromInt(
							justinmimbs$date$Date$weekdayNumber(date));
					case 2:
						return elm$core$String$fromInt(
							justinmimbs$date$Date$weekdayNumber(date));
					default:
						return A4(
							justinmimbs$date$Date$formatField,
							language,
							_Utils_chr('E'),
							length,
							date);
				}
			default:
				return '';
		}
	});
var justinmimbs$date$Date$formatWithTokens = F3(
	function (language, tokens, date) {
		return A3(
			elm$core$List$foldl,
			F2(
				function (token, formatted) {
					if (token.$ === 'Field') {
						var _char = token.a;
						var length = token.b;
						return _Utils_ap(
							A4(justinmimbs$date$Date$formatField, language, _char, length, date),
							formatted);
					} else {
						var str = token.a;
						return _Utils_ap(str, formatted);
					}
				}),
			'',
			tokens);
	});
var elm$core$Result$withDefault = F2(
	function (def, result) {
		if (result.$ === 'Ok') {
			var a = result.a;
			return a;
		} else {
			return def;
		}
	});
var elm$parser$Parser$DeadEnd = F3(
	function (row, col, problem) {
		return {col: col, problem: problem, row: row};
	});
var elm$parser$Parser$problemToDeadEnd = function (p) {
	return A3(elm$parser$Parser$DeadEnd, p.row, p.col, p.problem);
};
var elm$parser$Parser$Advanced$bagToList = F2(
	function (bag, list) {
		bagToList:
		while (true) {
			switch (bag.$) {
				case 'Empty':
					return list;
				case 'AddRight':
					var bag1 = bag.a;
					var x = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2(elm$core$List$cons, x, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
				default:
					var bag1 = bag.a;
					var bag2 = bag.b;
					var $temp$bag = bag1,
						$temp$list = A2(elm$parser$Parser$Advanced$bagToList, bag2, list);
					bag = $temp$bag;
					list = $temp$list;
					continue bagToList;
			}
		}
	});
var elm$parser$Parser$Advanced$run = F2(
	function (_n0, src) {
		var parse = _n0.a;
		var _n1 = parse(
			{col: 1, context: _List_Nil, indent: 1, offset: 0, row: 1, src: src});
		if (_n1.$ === 'Good') {
			var value = _n1.b;
			return elm$core$Result$Ok(value);
		} else {
			var bag = _n1.b;
			return elm$core$Result$Err(
				A2(elm$parser$Parser$Advanced$bagToList, bag, _List_Nil));
		}
	});
var elm$parser$Parser$run = F2(
	function (parser, source) {
		var _n0 = A2(elm$parser$Parser$Advanced$run, parser, source);
		if (_n0.$ === 'Ok') {
			var a = _n0.a;
			return elm$core$Result$Ok(a);
		} else {
			var problems = _n0.a;
			return elm$core$Result$Err(
				A2(elm$core$List$map, elm$parser$Parser$problemToDeadEnd, problems));
		}
	});
var justinmimbs$date$Pattern$Literal = function (a) {
	return {$: 'Literal', a: a};
};
var elm$parser$Parser$Advanced$Bad = F2(
	function (a, b) {
		return {$: 'Bad', a: a, b: b};
	});
var elm$parser$Parser$Advanced$Good = F3(
	function (a, b, c) {
		return {$: 'Good', a: a, b: b, c: c};
	});
var elm$parser$Parser$Advanced$Parser = function (a) {
	return {$: 'Parser', a: a};
};
var elm$parser$Parser$Advanced$andThen = F2(
	function (callback, _n0) {
		var parseA = _n0.a;
		return elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _n1 = parseA(s0);
				if (_n1.$ === 'Bad') {
					var p = _n1.a;
					var x = _n1.b;
					return A2(elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _n1.a;
					var a = _n1.b;
					var s1 = _n1.c;
					var _n2 = callback(a);
					var parseB = _n2.a;
					var _n3 = parseB(s1);
					if (_n3.$ === 'Bad') {
						var p2 = _n3.a;
						var x = _n3.b;
						return A2(elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _n3.a;
						var b = _n3.b;
						var s2 = _n3.c;
						return A3(elm$parser$Parser$Advanced$Good, p1 || p2, b, s2);
					}
				}
			});
	});
var elm$parser$Parser$andThen = elm$parser$Parser$Advanced$andThen;
var elm$parser$Parser$Advanced$lazy = function (thunk) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _n0 = thunk(_Utils_Tuple0);
			var parse = _n0.a;
			return parse(s);
		});
};
var elm$parser$Parser$lazy = elm$parser$Parser$Advanced$lazy;
var elm$parser$Parser$Advanced$Empty = {$: 'Empty'};
var elm$parser$Parser$Advanced$Append = F2(
	function (a, b) {
		return {$: 'Append', a: a, b: b};
	});
var elm$parser$Parser$Advanced$oneOfHelp = F3(
	function (s0, bag, parsers) {
		oneOfHelp:
		while (true) {
			if (!parsers.b) {
				return A2(elm$parser$Parser$Advanced$Bad, false, bag);
			} else {
				var parse = parsers.a.a;
				var remainingParsers = parsers.b;
				var _n1 = parse(s0);
				if (_n1.$ === 'Good') {
					var step = _n1;
					return step;
				} else {
					var step = _n1;
					var p = step.a;
					var x = step.b;
					if (p) {
						return step;
					} else {
						var $temp$s0 = s0,
							$temp$bag = A2(elm$parser$Parser$Advanced$Append, bag, x),
							$temp$parsers = remainingParsers;
						s0 = $temp$s0;
						bag = $temp$bag;
						parsers = $temp$parsers;
						continue oneOfHelp;
					}
				}
			}
		}
	});
var elm$parser$Parser$Advanced$oneOf = function (parsers) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3(elm$parser$Parser$Advanced$oneOfHelp, s, elm$parser$Parser$Advanced$Empty, parsers);
		});
};
var elm$parser$Parser$oneOf = elm$parser$Parser$Advanced$oneOf;
var elm$parser$Parser$Advanced$succeed = function (a) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A3(elm$parser$Parser$Advanced$Good, false, a, s);
		});
};
var elm$parser$Parser$succeed = elm$parser$Parser$Advanced$succeed;
var elm$core$Basics$always = F2(
	function (a, _n0) {
		return a;
	});
var elm$parser$Parser$Advanced$map2 = F3(
	function (func, _n0, _n1) {
		var parseA = _n0.a;
		var parseB = _n1.a;
		return elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _n2 = parseA(s0);
				if (_n2.$ === 'Bad') {
					var p = _n2.a;
					var x = _n2.b;
					return A2(elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p1 = _n2.a;
					var a = _n2.b;
					var s1 = _n2.c;
					var _n3 = parseB(s1);
					if (_n3.$ === 'Bad') {
						var p2 = _n3.a;
						var x = _n3.b;
						return A2(elm$parser$Parser$Advanced$Bad, p1 || p2, x);
					} else {
						var p2 = _n3.a;
						var b = _n3.b;
						var s2 = _n3.c;
						return A3(
							elm$parser$Parser$Advanced$Good,
							p1 || p2,
							A2(func, a, b),
							s2);
					}
				}
			});
	});
var elm$parser$Parser$Advanced$ignorer = F2(
	function (keepParser, ignoreParser) {
		return A3(elm$parser$Parser$Advanced$map2, elm$core$Basics$always, keepParser, ignoreParser);
	});
var elm$parser$Parser$ignorer = elm$parser$Parser$Advanced$ignorer;
var elm$parser$Parser$Expecting = function (a) {
	return {$: 'Expecting', a: a};
};
var elm$parser$Parser$Advanced$Token = F2(
	function (a, b) {
		return {$: 'Token', a: a, b: b};
	});
var elm$parser$Parser$toToken = function (str) {
	return A2(
		elm$parser$Parser$Advanced$Token,
		str,
		elm$parser$Parser$Expecting(str));
};
var elm$parser$Parser$Advanced$AddRight = F2(
	function (a, b) {
		return {$: 'AddRight', a: a, b: b};
	});
var elm$parser$Parser$Advanced$DeadEnd = F4(
	function (row, col, problem, contextStack) {
		return {col: col, contextStack: contextStack, problem: problem, row: row};
	});
var elm$parser$Parser$Advanced$fromState = F2(
	function (s, x) {
		return A2(
			elm$parser$Parser$Advanced$AddRight,
			elm$parser$Parser$Advanced$Empty,
			A4(elm$parser$Parser$Advanced$DeadEnd, s.row, s.col, x, s.context));
	});
var elm$parser$Parser$Advanced$isSubString = _Parser_isSubString;
var elm$parser$Parser$Advanced$token = function (_n0) {
	var str = _n0.a;
	var expecting = _n0.b;
	var progress = !elm$core$String$isEmpty(str);
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			var _n1 = A5(elm$parser$Parser$Advanced$isSubString, str, s.offset, s.row, s.col, s.src);
			var newOffset = _n1.a;
			var newRow = _n1.b;
			var newCol = _n1.c;
			return _Utils_eq(newOffset, -1) ? A2(
				elm$parser$Parser$Advanced$Bad,
				false,
				A2(elm$parser$Parser$Advanced$fromState, s, expecting)) : A3(
				elm$parser$Parser$Advanced$Good,
				progress,
				_Utils_Tuple0,
				{col: newCol, context: s.context, indent: s.indent, offset: newOffset, row: newRow, src: s.src});
		});
};
var elm$parser$Parser$token = function (str) {
	return elm$parser$Parser$Advanced$token(
		elm$parser$Parser$toToken(str));
};
var justinmimbs$date$Pattern$escapedQuote = A2(
	elm$parser$Parser$ignorer,
	elm$parser$Parser$succeed(
		justinmimbs$date$Pattern$Literal('\'')),
	elm$parser$Parser$token('\'\''));
var elm$parser$Parser$UnexpectedChar = {$: 'UnexpectedChar'};
var elm$parser$Parser$Advanced$isSubChar = _Parser_isSubChar;
var elm$parser$Parser$Advanced$chompIf = F2(
	function (isGood, expecting) {
		return elm$parser$Parser$Advanced$Parser(
			function (s) {
				var newOffset = A3(elm$parser$Parser$Advanced$isSubChar, isGood, s.offset, s.src);
				return _Utils_eq(newOffset, -1) ? A2(
					elm$parser$Parser$Advanced$Bad,
					false,
					A2(elm$parser$Parser$Advanced$fromState, s, expecting)) : (_Utils_eq(newOffset, -2) ? A3(
					elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: 1, context: s.context, indent: s.indent, offset: s.offset + 1, row: s.row + 1, src: s.src}) : A3(
					elm$parser$Parser$Advanced$Good,
					true,
					_Utils_Tuple0,
					{col: s.col + 1, context: s.context, indent: s.indent, offset: newOffset, row: s.row, src: s.src}));
			});
	});
var elm$parser$Parser$chompIf = function (isGood) {
	return A2(elm$parser$Parser$Advanced$chompIf, isGood, elm$parser$Parser$UnexpectedChar);
};
var elm$parser$Parser$Advanced$mapChompedString = F2(
	function (func, _n0) {
		var parse = _n0.a;
		return elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _n1 = parse(s0);
				if (_n1.$ === 'Bad') {
					var p = _n1.a;
					var x = _n1.b;
					return A2(elm$parser$Parser$Advanced$Bad, p, x);
				} else {
					var p = _n1.a;
					var a = _n1.b;
					var s1 = _n1.c;
					return A3(
						elm$parser$Parser$Advanced$Good,
						p,
						A2(
							func,
							A3(elm$core$String$slice, s0.offset, s1.offset, s0.src),
							a),
						s1);
				}
			});
	});
var elm$parser$Parser$Advanced$getChompedString = function (parser) {
	return A2(elm$parser$Parser$Advanced$mapChompedString, elm$core$Basics$always, parser);
};
var elm$parser$Parser$getChompedString = elm$parser$Parser$Advanced$getChompedString;
var elm$core$String$foldr = _String_foldr;
var elm$core$String$toList = function (string) {
	return A3(elm$core$String$foldr, elm$core$List$cons, _List_Nil, string);
};
var elm$parser$Parser$Advanced$chompWhileHelp = F5(
	function (isGood, offset, row, col, s0) {
		chompWhileHelp:
		while (true) {
			var newOffset = A3(elm$parser$Parser$Advanced$isSubChar, isGood, offset, s0.src);
			if (_Utils_eq(newOffset, -1)) {
				return A3(
					elm$parser$Parser$Advanced$Good,
					_Utils_cmp(s0.offset, offset) < 0,
					_Utils_Tuple0,
					{col: col, context: s0.context, indent: s0.indent, offset: offset, row: row, src: s0.src});
			} else {
				if (_Utils_eq(newOffset, -2)) {
					var $temp$isGood = isGood,
						$temp$offset = offset + 1,
						$temp$row = row + 1,
						$temp$col = 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				} else {
					var $temp$isGood = isGood,
						$temp$offset = newOffset,
						$temp$row = row,
						$temp$col = col + 1,
						$temp$s0 = s0;
					isGood = $temp$isGood;
					offset = $temp$offset;
					row = $temp$row;
					col = $temp$col;
					s0 = $temp$s0;
					continue chompWhileHelp;
				}
			}
		}
	});
var elm$parser$Parser$Advanced$chompWhile = function (isGood) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A5(elm$parser$Parser$Advanced$chompWhileHelp, isGood, s.offset, s.row, s.col, s);
		});
};
var elm$parser$Parser$chompWhile = elm$parser$Parser$Advanced$chompWhile;
var elm$parser$Parser$Advanced$getOffset = elm$parser$Parser$Advanced$Parser(
	function (s) {
		return A3(elm$parser$Parser$Advanced$Good, false, s.offset, s);
	});
var elm$parser$Parser$getOffset = elm$parser$Parser$Advanced$getOffset;
var elm$parser$Parser$Advanced$keeper = F2(
	function (parseFunc, parseArg) {
		return A3(elm$parser$Parser$Advanced$map2, elm$core$Basics$apL, parseFunc, parseArg);
	});
var elm$parser$Parser$keeper = elm$parser$Parser$Advanced$keeper;
var elm$parser$Parser$Problem = function (a) {
	return {$: 'Problem', a: a};
};
var elm$parser$Parser$Advanced$problem = function (x) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return A2(
				elm$parser$Parser$Advanced$Bad,
				false,
				A2(elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var elm$parser$Parser$problem = function (msg) {
	return elm$parser$Parser$Advanced$problem(
		elm$parser$Parser$Problem(msg));
};
var justinmimbs$date$Pattern$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var justinmimbs$date$Pattern$fieldRepeats = function (str) {
	var _n0 = elm$core$String$toList(str);
	if (_n0.b && (!_n0.b.b)) {
		var _char = _n0.a;
		return A2(
			elm$parser$Parser$keeper,
			A2(
				elm$parser$Parser$keeper,
				elm$parser$Parser$succeed(
					F2(
						function (x, y) {
							return A2(justinmimbs$date$Pattern$Field, _char, 1 + (y - x));
						})),
				A2(
					elm$parser$Parser$ignorer,
					elm$parser$Parser$getOffset,
					elm$parser$Parser$chompWhile(
						elm$core$Basics$eq(_char)))),
			elm$parser$Parser$getOffset);
	} else {
		return elm$parser$Parser$problem('expected exactly one char');
	}
};
var justinmimbs$date$Pattern$field = A2(
	elm$parser$Parser$andThen,
	justinmimbs$date$Pattern$fieldRepeats,
	elm$parser$Parser$getChompedString(
		elm$parser$Parser$chompIf(elm$core$Char$isAlpha)));
var justinmimbs$date$Pattern$finalize = A2(
	elm$core$List$foldl,
	F2(
		function (token, tokens) {
			var _n0 = _Utils_Tuple2(token, tokens);
			if (((_n0.a.$ === 'Literal') && _n0.b.b) && (_n0.b.a.$ === 'Literal')) {
				var x = _n0.a.a;
				var _n1 = _n0.b;
				var y = _n1.a.a;
				var rest = _n1.b;
				return A2(
					elm$core$List$cons,
					justinmimbs$date$Pattern$Literal(
						_Utils_ap(x, y)),
					rest);
			} else {
				return A2(elm$core$List$cons, token, tokens);
			}
		}),
	_List_Nil);
var elm$parser$Parser$Advanced$map = F2(
	function (func, _n0) {
		var parse = _n0.a;
		return elm$parser$Parser$Advanced$Parser(
			function (s0) {
				var _n1 = parse(s0);
				if (_n1.$ === 'Good') {
					var p = _n1.a;
					var a = _n1.b;
					var s1 = _n1.c;
					return A3(
						elm$parser$Parser$Advanced$Good,
						p,
						func(a),
						s1);
				} else {
					var p = _n1.a;
					var x = _n1.b;
					return A2(elm$parser$Parser$Advanced$Bad, p, x);
				}
			});
	});
var elm$parser$Parser$map = elm$parser$Parser$Advanced$map;
var justinmimbs$date$Pattern$isLiteralChar = function (_char) {
	return (!_Utils_eq(
		_char,
		_Utils_chr('\''))) && (!elm$core$Char$isAlpha(_char));
};
var justinmimbs$date$Pattern$literal = A2(
	elm$parser$Parser$map,
	justinmimbs$date$Pattern$Literal,
	elm$parser$Parser$getChompedString(
		A2(
			elm$parser$Parser$ignorer,
			A2(
				elm$parser$Parser$ignorer,
				elm$parser$Parser$succeed(_Utils_Tuple0),
				elm$parser$Parser$chompIf(justinmimbs$date$Pattern$isLiteralChar)),
			elm$parser$Parser$chompWhile(justinmimbs$date$Pattern$isLiteralChar))));
var elm$parser$Parser$ExpectingEnd = {$: 'ExpectingEnd'};
var elm$parser$Parser$Advanced$end = function (x) {
	return elm$parser$Parser$Advanced$Parser(
		function (s) {
			return _Utils_eq(
				elm$core$String$length(s.src),
				s.offset) ? A3(elm$parser$Parser$Advanced$Good, false, _Utils_Tuple0, s) : A2(
				elm$parser$Parser$Advanced$Bad,
				false,
				A2(elm$parser$Parser$Advanced$fromState, s, x));
		});
};
var elm$parser$Parser$end = elm$parser$Parser$Advanced$end(elm$parser$Parser$ExpectingEnd);
var justinmimbs$date$Pattern$quotedHelp = function (result) {
	return elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				elm$parser$Parser$andThen,
				function (str) {
					return justinmimbs$date$Pattern$quotedHelp(
						_Utils_ap(result, str));
				},
				elm$parser$Parser$getChompedString(
					A2(
						elm$parser$Parser$ignorer,
						A2(
							elm$parser$Parser$ignorer,
							elm$parser$Parser$succeed(_Utils_Tuple0),
							elm$parser$Parser$chompIf(
								elm$core$Basics$neq(
									_Utils_chr('\'')))),
						elm$parser$Parser$chompWhile(
							elm$core$Basics$neq(
								_Utils_chr('\'')))))),
				A2(
				elm$parser$Parser$andThen,
				function (_n0) {
					return justinmimbs$date$Pattern$quotedHelp(result + '\'');
				},
				elm$parser$Parser$token('\'\'')),
				elm$parser$Parser$succeed(result)
			]));
};
var justinmimbs$date$Pattern$quoted = A2(
	elm$parser$Parser$keeper,
	A2(
		elm$parser$Parser$ignorer,
		elm$parser$Parser$succeed(justinmimbs$date$Pattern$Literal),
		elm$parser$Parser$chompIf(
			elm$core$Basics$eq(
				_Utils_chr('\'')))),
	A2(
		elm$parser$Parser$ignorer,
		justinmimbs$date$Pattern$quotedHelp(''),
		elm$parser$Parser$oneOf(
			_List_fromArray(
				[
					elm$parser$Parser$chompIf(
					elm$core$Basics$eq(
						_Utils_chr('\''))),
					elm$parser$Parser$end
				]))));
var justinmimbs$date$Pattern$patternHelp = function (tokens) {
	return elm$parser$Parser$oneOf(
		_List_fromArray(
			[
				A2(
				elm$parser$Parser$andThen,
				function (token) {
					return justinmimbs$date$Pattern$patternHelp(
						A2(elm$core$List$cons, token, tokens));
				},
				elm$parser$Parser$oneOf(
					_List_fromArray(
						[justinmimbs$date$Pattern$field, justinmimbs$date$Pattern$literal, justinmimbs$date$Pattern$escapedQuote, justinmimbs$date$Pattern$quoted]))),
				elm$parser$Parser$lazy(
				function (_n0) {
					return elm$parser$Parser$succeed(
						justinmimbs$date$Pattern$finalize(tokens));
				})
			]));
};
var justinmimbs$date$Pattern$fromString = function (str) {
	return A2(
		elm$core$Result$withDefault,
		_List_fromArray(
			[
				justinmimbs$date$Pattern$Literal(str)
			]),
		A2(
			elm$parser$Parser$run,
			justinmimbs$date$Pattern$patternHelp(_List_Nil),
			str));
};
var justinmimbs$date$Date$formatWithLanguage = F2(
	function (language, pattern) {
		var tokens = elm$core$List$reverse(
			justinmimbs$date$Pattern$fromString(pattern));
		return A2(justinmimbs$date$Date$formatWithTokens, language, tokens);
	});
var justinmimbs$date$Date$monthToName = function (m) {
	switch (m.$) {
		case 'Jan':
			return 'January';
		case 'Feb':
			return 'February';
		case 'Mar':
			return 'March';
		case 'Apr':
			return 'April';
		case 'May':
			return 'May';
		case 'Jun':
			return 'June';
		case 'Jul':
			return 'July';
		case 'Aug':
			return 'August';
		case 'Sep':
			return 'September';
		case 'Oct':
			return 'October';
		case 'Nov':
			return 'November';
		default:
			return 'December';
	}
};
var justinmimbs$date$Date$weekdayToName = function (wd) {
	switch (wd.$) {
		case 'Mon':
			return 'Monday';
		case 'Tue':
			return 'Tuesday';
		case 'Wed':
			return 'Wednesday';
		case 'Thu':
			return 'Thursday';
		case 'Fri':
			return 'Friday';
		case 'Sat':
			return 'Saturday';
		default:
			return 'Sunday';
	}
};
var justinmimbs$date$Date$language_en = {
	dayWithSuffix: justinmimbs$date$Date$withOrdinalSuffix,
	monthName: justinmimbs$date$Date$monthToName,
	monthNameShort: A2(
		elm$core$Basics$composeR,
		justinmimbs$date$Date$monthToName,
		elm$core$String$left(3)),
	weekdayName: justinmimbs$date$Date$weekdayToName,
	weekdayNameShort: A2(
		elm$core$Basics$composeR,
		justinmimbs$date$Date$weekdayToName,
		elm$core$String$left(3))
};
var justinmimbs$date$Date$format = function (pattern) {
	return A2(justinmimbs$date$Date$formatWithLanguage, justinmimbs$date$Date$language_en, pattern);
};
var author$project$Calendar2$Agenda$viewAgendaRowWithDate = F3(
	function (config, eventGroup, event) {
		var dateString = A2(justinmimbs$date$Date$format, 'EE MM d', eventGroup.date);
		var timeCell = A2(
			elm$html$Html$td,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--agenda-date-cell'),
					elm$html$Html$Attributes$rowspan(
					elm$core$List$length(eventGroup.events))
				]),
			_List_fromArray(
				[
					elm$html$Html$text(dateString)
				]));
		return A2(
			elm$html$Html$tr,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--agenda-day')
				]),
			A2(
				elm$core$List$cons,
				timeCell,
				A2(author$project$Calendar2$Agenda$viewTimeAndEvent, config, event)));
	});
var elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3(elm$core$List$foldr, elm$core$List$cons, ys, xs);
		}
	});
var elm$core$List$concat = function (lists) {
	return A3(elm$core$List$foldr, elm$core$List$append, _List_Nil, lists);
};
var elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2(elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var elm$html$Html$table = _VirtualDom_node('table');
var elm$html$Html$tbody = _VirtualDom_node('tbody');
var justinmimbs$date$Date$Month = {$: 'Month'};
var justinmimbs$date$Date$Weeks = {$: 'Weeks'};
var justinmimbs$date$Date$Years = {$: 'Years'};
var justinmimbs$date$Date$intervalToUnits = function (interval) {
	switch (interval.$) {
		case 'Year':
			return _Utils_Tuple2(1, justinmimbs$date$Date$Years);
		case 'Quarter':
			return _Utils_Tuple2(3, justinmimbs$date$Date$Months);
		case 'Month':
			return _Utils_Tuple2(1, justinmimbs$date$Date$Months);
		case 'Day':
			return _Utils_Tuple2(1, justinmimbs$date$Date$Days);
		default:
			var week = interval;
			return _Utils_Tuple2(1, justinmimbs$date$Date$Weeks);
	}
};
var justinmimbs$date$Date$ceiling = F2(
	function (interval, date) {
		var floored = A2(justinmimbs$date$Date$floor, interval, date);
		if (_Utils_eq(date, floored)) {
			return date;
		} else {
			var _n0 = justinmimbs$date$Date$intervalToUnits(interval);
			var n = _n0.a;
			var unit = _n0.b;
			return A3(justinmimbs$date$Date$add, unit, n, floored);
		}
	});
var author$project$Calendar2$Agenda$view = F3(
	function (config, events, posix) {
		var isDateInMonth = function (eventsDate) {
			return A3(
				justinmimbs$date$Date$isBetween,
				A2(justinmimbs$date$Date$floor, justinmimbs$date$Date$Month, eventsDate),
				A2(justinmimbs$date$Date$ceiling, justinmimbs$date$Date$Month, eventsDate),
				eventsDate);
		};
		var groupedEvents = A2(author$project$Calendar2$Agenda$eventsGroupedByDate, config, events);
		var getAgendaRowView = function (eventGroup) {
			var _n0 = eventGroup.events;
			if (!_n0.b) {
				return _List_Nil;
			} else {
				var firstEvent = _n0.a;
				var restOfEvents = _n0.b;
				return A2(
					elm$core$List$cons,
					A3(author$project$Calendar2$Agenda$viewAgendaRowWithDate, config, eventGroup, firstEvent),
					A2(
						elm$core$List$map,
						author$project$Calendar2$Agenda$viewAgendaRow(config),
						restOfEvents));
			}
		};
		var filteredEventsByMonth = A2(
			elm$core$List$filter,
			A2(
				elm$core$Basics$composeL,
				isDateInMonth,
				function ($) {
					return $.date;
				}),
			groupedEvents);
		var date = A2(justinmimbs$date$Date$fromPosix, elm$time$Time$utc, posix);
		return A2(
			elm$html$Html$table,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--agenda')
				]),
			_List_fromArray(
				[
					author$project$Calendar2$Agenda$viewAgendaHeader,
					A2(
					elm$html$Html$tbody,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('elm-calendar--agenda-tbody')
						]),
					elm$core$List$concat(
						A2(elm$core$List$map, getAgendaRowView, filteredEventsByMonth)))
				]));
	});
var elm$html$Html$a = _VirtualDom_node('a');
var elm$html$Html$div = _VirtualDom_node('div');
var elm$html$Html$Attributes$href = function (url) {
	return A2(
		elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var author$project$Calendar2$Day$viewDate = function (posix) {
	var title_day = A2(
		justinmimbs$date$Date$format,
		'EE M/d',
		A2(justinmimbs$date$Date$fromPosix, elm$time$Time$utc, posix));
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--date-header')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$a,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('elm-calendar--date'),
						elm$html$Html$Attributes$href('#')
					]),
				_List_fromArray(
					[
						elm$html$Html$text(title_day)
					]))
			]));
};
var author$project$Calendar2$Day$viewTimeGutterHeader = A2(
	elm$html$Html$div,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('elm-calendar--time-gutter-header')
		]),
	_List_Nil);
var author$project$Calendar2$Day$viewDayHeader = function (day) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--day-header')
			]),
		_List_fromArray(
			[
				author$project$Calendar2$Day$viewTimeGutterHeader,
				author$project$Calendar2$Day$viewDate(day)
			]));
};
var author$project$Calendar2$Day$flip = F3(
	function (_function, argB, argA) {
		return A2(_function, argA, argB);
	});
var author$project$Calendar2$Event$msPerSecond = 1000;
var author$project$Calendar2$Event$msPerMinute = 60 * author$project$Calendar2$Event$msPerSecond;
var author$project$Calendar2$Event$msPerHour = 60 * author$project$Calendar2$Event$msPerMinute;
var author$project$Calendar2$Event$msFromTimeParts = F4(
	function (hh, mm, ss, ms) {
		return ((ms + (author$project$Calendar2$Event$msPerSecond * ss)) + (author$project$Calendar2$Event$msPerMinute * mm)) + (author$project$Calendar2$Event$msPerHour * hh);
	});
var author$project$Calendar2$Event$msPerDay = 24 * author$project$Calendar2$Event$msPerHour;
var author$project$Calendar2$Event$fractionalDay = function (posix) {
	var timeOfDayMS = A4(
		author$project$Calendar2$Event$msFromTimeParts,
		A2(elm$time$Time$toHour, elm$time$Time$utc, posix),
		A2(elm$time$Time$toMinute, elm$time$Time$utc, posix),
		A2(elm$time$Time$toSecond, elm$time$Time$utc, posix),
		A2(elm$time$Time$toMillis, elm$time$Time$utc, posix));
	return timeOfDayMS / author$project$Calendar2$Event$msPerDay;
};
var justinmimbs$time_extra$Time$Extra$Minute = {$: 'Minute'};
var author$project$Calendar2$Event$fractionalEndDay = function (posix2) {
	var posix = A4(justinmimbs$time_extra$Time$Extra$add, justinmimbs$time_extra$Time$Extra$Minute, -7, elm$time$Time$utc, posix2);
	var timeOfDayMS = A4(
		author$project$Calendar2$Event$msFromTimeParts,
		A2(elm$time$Time$toHour, elm$time$Time$utc, posix),
		A2(elm$time$Time$toMinute, elm$time$Time$utc, posix),
		A2(elm$time$Time$toSecond, elm$time$Time$utc, posix),
		A2(elm$time$Time$toMillis, elm$time$Time$utc, posix));
	return timeOfDayMS / author$project$Calendar2$Event$msPerDay;
};
var elm$core$String$fromFloat = _String_fromNumber;
var elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var elm$html$Html$Attributes$style = elm$virtual_dom$VirtualDom$style;
var author$project$Calendar2$Event$styleDayEvent = F2(
	function (start, end) {
		var ratio = 1;
		var startPercent = author$project$Calendar2$Event$fractionalDay(start) * ratio;
		var frame_hight_size = 41 * 24;
		var startPercentage = elm$core$String$fromFloat(frame_hight_size * startPercent) + 'px';
		var endPercent = author$project$Calendar2$Event$fractionalEndDay(end) * ratio;
		var height = elm$core$String$fromFloat(frame_hight_size * (endPercent - startPercent)) + 'px';
		return _List_fromArray(
			[
				A2(elm$html$Html$Attributes$style, 'top', startPercentage),
				A2(elm$html$Html$Attributes$style, 'height', height),
				A2(elm$html$Html$Attributes$style, 'left', '0%'),
				A2(elm$html$Html$Attributes$style, 'width', '90%'),
				A2(elm$html$Html$Attributes$style, 'position', 'absolute')
			]);
	});
var elm$core$Tuple$second = function (_n0) {
	var y = _n0.b;
	return y;
};
var elm$html$Html$Attributes$classList = function (classes) {
	return elm$html$Html$Attributes$class(
		A2(
			elm$core$String$join,
			' ',
			A2(
				elm$core$List$map,
				elm$core$Tuple$first,
				A2(elm$core$List$filter, elm$core$Tuple$second, classes))));
};
var author$project$Calendar2$Event$eventStyling = F5(
	function (config, event, eventRange, timeSpan, customClasses) {
		var eventStart = config.start(event);
		var eventEnd = config.end(event);
		var styles = function () {
			switch (timeSpan.$) {
				case 'Month':
					return _List_Nil;
				case 'Week':
					return _List_Nil;
				case 'Day':
					return A2(author$project$Calendar2$Event$styleDayEvent, eventStart, eventEnd);
				default:
					return _List_Nil;
			}
		}();
		var classes = function () {
			switch (eventRange.$) {
				case 'StartsAndEnds':
					return 'elm-calendar--event elm-calendar--event-starts-and-ends';
				case 'ContinuesAfter':
					return 'elm-calendar--event elm-calendar--event-continues-after';
				case 'ContinuesPrior':
					return 'elm-calendar--event elm-calendar--event-continues-prior';
				case 'ContinuesAfterAndPrior':
					return 'elm-calendar--event';
				default:
					return '';
			}
		}();
		return _Utils_ap(
			_List_fromArray(
				[
					elm$html$Html$Attributes$classList(
					A2(
						elm$core$List$cons,
						_Utils_Tuple2(classes, true),
						customClasses))
				]),
			styles);
	});
var author$project$Calendar2$Msg$EventClick = function (a) {
	return {$: 'EventClick', a: a};
};
var author$project$Calendar2$Msg$EventMouseEnter = function (a) {
	return {$: 'EventMouseEnter', a: a};
};
var author$project$Calendar2$Msg$EventMouseLeave = function (a) {
	return {$: 'EventMouseLeave', a: a};
};
var elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var elm$html$Html$node = elm$virtual_dom$VirtualDom$node;
var elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var elm$html$Html$Events$onClick = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'click',
		elm$json$Json$Decode$succeed(msg));
};
var elm$html$Html$Events$onMouseEnter = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'mouseenter',
		elm$json$Json$Decode$succeed(msg));
};
var elm$html$Html$Events$onMouseLeave = function (msg) {
	return A2(
		elm$html$Html$Events$on,
		'mouseleave',
		elm$json$Json$Decode$succeed(msg));
};
var author$project$Calendar2$Event$eventSegment = F5(
	function (config, event, selectedId, eventRange, timeSpan) {
		var eventId = config.toId(event);
		var isSelected = A2(
			elm$core$Maybe$withDefault,
			false,
			A2(
				elm$core$Maybe$map,
				elm$core$Basics$eq(eventId),
				selectedId));
		var _n0 = A2(config.event, event, isSelected);
		var nodeName = _n0.nodeName;
		var classes = _n0.classes;
		var children = _n0.children;
		return A3(
			elm$html$Html$node,
			nodeName,
			_Utils_ap(
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(
						author$project$Calendar2$Msg$EventClick(eventId)),
						elm$html$Html$Events$onMouseEnter(
						author$project$Calendar2$Msg$EventMouseEnter(eventId)),
						elm$html$Html$Events$onMouseLeave(
						author$project$Calendar2$Msg$EventMouseLeave(eventId))
					]),
				A5(author$project$Calendar2$Event$eventStyling, config, event, eventRange, timeSpan, classes)),
			children);
	});
var author$project$Calendar2$Event$maybeViewDayEvent = F4(
	function (config, event, selectedId, eventRange) {
		if (eventRange.$ === 'ExistsOutside') {
			return elm$core$Maybe$Nothing;
		} else {
			return elm$core$Maybe$Just(
				A5(author$project$Calendar2$Event$eventSegment, config, event, selectedId, eventRange, author$project$Calendar2$Msg$Day));
		}
	});
var author$project$Calendar2$Event$ContinuesAfter = {$: 'ContinuesAfter'};
var author$project$Calendar2$Event$ContinuesAfterAndPrior = {$: 'ContinuesAfterAndPrior'};
var author$project$Calendar2$Event$ContinuesPrior = {$: 'ContinuesPrior'};
var author$project$Calendar2$Event$ExistsOutside = {$: 'ExistsOutside'};
var author$project$Calendar2$Event$StartsAndEnds = {$: 'StartsAndEnds'};
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$dayToInt = function (_n0) {
	var day = _n0.a;
	return day;
};
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$compareDays = F2(
	function (lhs, rhs) {
		return A2(
			elm$core$Basics$compare,
			PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$dayToInt(lhs),
			PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$dayToInt(rhs));
	});
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$monthToInt = function (month) {
	switch (month.$) {
		case 'Jan':
			return 1;
		case 'Feb':
			return 2;
		case 'Mar':
			return 3;
		case 'Apr':
			return 4;
		case 'May':
			return 5;
		case 'Jun':
			return 6;
		case 'Jul':
			return 7;
		case 'Aug':
			return 8;
		case 'Sep':
			return 9;
		case 'Oct':
			return 10;
		case 'Nov':
			return 11;
		default:
			return 12;
	}
};
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$compareMonths = F2(
	function (lhs, rhs) {
		return A2(
			elm$core$Basics$compare,
			PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$monthToInt(lhs),
			PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$monthToInt(rhs));
	});
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$yearToInt = function (_n0) {
	var year = _n0.a;
	return year;
};
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$compareYears = F2(
	function (lhs, rhs) {
		return A2(
			elm$core$Basics$compare,
			PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$yearToInt(lhs),
			PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$yearToInt(rhs));
	});
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$getDay = function (_n0) {
	var date = _n0.a;
	return date.day;
};
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$getMonth = function (_n0) {
	var month = _n0.a.month;
	return month;
};
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$getYear = function (_n0) {
	var year = _n0.a.year;
	return year;
};
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$compare = F2(
	function (lhs, rhs) {
		var _n0 = _Utils_Tuple3(
			A2(
				PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$compareYears,
				PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$getYear(lhs),
				PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$getYear(rhs)),
			A2(
				PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$compareMonths,
				PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$getMonth(lhs),
				PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$getMonth(rhs)),
			A2(
				PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$compareDays,
				PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$getDay(lhs),
				PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$getDay(rhs)));
		var yearsComparison = _n0.a;
		var monthsComparison = _n0.b;
		var daysComparison = _n0.c;
		if (yearsComparison.$ === 'EQ') {
			if (monthsComparison.$ === 'EQ') {
				return daysComparison;
			} else {
				return monthsComparison;
			}
		} else {
			return yearsComparison;
		}
	});
var PanagiotisGeorgiadis$elm_datetime$Calendar$compare = PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$compare;
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$compareHours = F2(
	function (_n0, _n1) {
		var lhs = _n0.a;
		var rhs = _n1.a;
		return A2(elm$core$Basics$compare, lhs, rhs);
	});
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$compareMilliseconds = F2(
	function (_n0, _n1) {
		var lhs = _n0.a;
		var rhs = _n1.a;
		return A2(elm$core$Basics$compare, lhs, rhs);
	});
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$compareMinutes = F2(
	function (_n0, _n1) {
		var lhs = _n0.a;
		var rhs = _n1.a;
		return A2(elm$core$Basics$compare, lhs, rhs);
	});
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$compareSeconds = F2(
	function (_n0, _n1) {
		var lhs = _n0.a;
		var rhs = _n1.a;
		return A2(elm$core$Basics$compare, lhs, rhs);
	});
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$getHours = function (_n0) {
	var hours = _n0.a.hours;
	return hours;
};
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$getMilliseconds = function (_n0) {
	var milliseconds = _n0.a.milliseconds;
	return milliseconds;
};
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$getMinutes = function (_n0) {
	var minutes = _n0.a.minutes;
	return minutes;
};
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$getSeconds = function (_n0) {
	var seconds = _n0.a.seconds;
	return seconds;
};
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$compare = F2(
	function (lhs, rhs) {
		var _n0 = _Utils_Tuple3(
			A2(
				PanagiotisGeorgiadis$elm_datetime$Clock$Internal$compareHours,
				PanagiotisGeorgiadis$elm_datetime$Clock$Internal$getHours(lhs),
				PanagiotisGeorgiadis$elm_datetime$Clock$Internal$getHours(rhs)),
			A2(
				PanagiotisGeorgiadis$elm_datetime$Clock$Internal$compareMinutes,
				PanagiotisGeorgiadis$elm_datetime$Clock$Internal$getMinutes(lhs),
				PanagiotisGeorgiadis$elm_datetime$Clock$Internal$getMinutes(rhs)),
			A2(
				PanagiotisGeorgiadis$elm_datetime$Clock$Internal$compareSeconds,
				PanagiotisGeorgiadis$elm_datetime$Clock$Internal$getSeconds(lhs),
				PanagiotisGeorgiadis$elm_datetime$Clock$Internal$getSeconds(rhs)));
		var hoursComparison = _n0.a;
		var minutesComparison = _n0.b;
		var secondsComparison = _n0.c;
		if (hoursComparison.$ === 'EQ') {
			if (minutesComparison.$ === 'EQ') {
				if (secondsComparison.$ === 'EQ') {
					return A2(
						PanagiotisGeorgiadis$elm_datetime$Clock$Internal$compareMilliseconds,
						PanagiotisGeorgiadis$elm_datetime$Clock$Internal$getMilliseconds(lhs),
						PanagiotisGeorgiadis$elm_datetime$Clock$Internal$getMilliseconds(rhs));
				} else {
					return secondsComparison;
				}
			} else {
				return minutesComparison;
			}
		} else {
			return hoursComparison;
		}
	});
var PanagiotisGeorgiadis$elm_datetime$Clock$compare = PanagiotisGeorgiadis$elm_datetime$Clock$Internal$compare;
var PanagiotisGeorgiadis$elm_datetime$DateTime$Internal$compare = F2(
	function (_n0, _n1) {
		var lhs = _n0.a;
		var rhs = _n1.a;
		var _n2 = A2(PanagiotisGeorgiadis$elm_datetime$Calendar$compare, lhs.date, rhs.date);
		if (_n2.$ === 'EQ') {
			return A2(PanagiotisGeorgiadis$elm_datetime$Clock$compare, lhs.time, rhs.time);
		} else {
			var ord = _n2;
			return ord;
		}
	});
var PanagiotisGeorgiadis$elm_datetime$DateTime$compare = PanagiotisGeorgiadis$elm_datetime$DateTime$Internal$compare;
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$Date = function (a) {
	return {$: 'Date', a: a};
};
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$Day = function (a) {
	return {$: 'Day', a: a};
};
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$Year = function (a) {
	return {$: 'Year', a: a};
};
var PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$fromPosix = function (posix) {
	return PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$Date(
		{
			day: PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$Day(
				A2(elm$time$Time$toDay, elm$time$Time$utc, posix)),
			month: A2(elm$time$Time$toMonth, elm$time$Time$utc, posix),
			year: PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$Year(
				A2(elm$time$Time$toYear, elm$time$Time$utc, posix))
		});
};
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$Hour = function (a) {
	return {$: 'Hour', a: a};
};
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$Millisecond = function (a) {
	return {$: 'Millisecond', a: a};
};
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$Minute = function (a) {
	return {$: 'Minute', a: a};
};
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$Second = function (a) {
	return {$: 'Second', a: a};
};
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$Time = function (a) {
	return {$: 'Time', a: a};
};
var PanagiotisGeorgiadis$elm_datetime$Clock$Internal$fromPosix = function (posix) {
	return PanagiotisGeorgiadis$elm_datetime$Clock$Internal$Time(
		{
			hours: PanagiotisGeorgiadis$elm_datetime$Clock$Internal$Hour(
				A2(elm$time$Time$toHour, elm$time$Time$utc, posix)),
			milliseconds: PanagiotisGeorgiadis$elm_datetime$Clock$Internal$Millisecond(
				A2(elm$time$Time$toMillis, elm$time$Time$utc, posix)),
			minutes: PanagiotisGeorgiadis$elm_datetime$Clock$Internal$Minute(
				A2(elm$time$Time$toMinute, elm$time$Time$utc, posix)),
			seconds: PanagiotisGeorgiadis$elm_datetime$Clock$Internal$Second(
				A2(elm$time$Time$toSecond, elm$time$Time$utc, posix))
		});
};
var PanagiotisGeorgiadis$elm_datetime$DateTime$Internal$DateTime = function (a) {
	return {$: 'DateTime', a: a};
};
var PanagiotisGeorgiadis$elm_datetime$DateTime$Internal$fromPosix = function (timePosix) {
	return PanagiotisGeorgiadis$elm_datetime$DateTime$Internal$DateTime(
		{
			date: PanagiotisGeorgiadis$elm_datetime$Calendar$Internal$fromPosix(timePosix),
			time: PanagiotisGeorgiadis$elm_datetime$Clock$Internal$fromPosix(timePosix)
		});
};
var PanagiotisGeorgiadis$elm_datetime$DateTime$fromPosix = PanagiotisGeorgiadis$elm_datetime$DateTime$Internal$fromPosix;
var author$project$Calendar2$Event$isAfter = F2(
	function (end, target) {
		var dt_target = PanagiotisGeorgiadis$elm_datetime$DateTime$fromPosix(target);
		var dt_end = PanagiotisGeorgiadis$elm_datetime$DateTime$fromPosix(end);
		return _Utils_eq(
			A2(PanagiotisGeorgiadis$elm_datetime$DateTime$compare, dt_end, dt_target),
			elm$core$Basics$LT) ? true : false;
	});
var author$project$Calendar2$Event$isBefore = F2(
	function (begin, target) {
		var dt_target = PanagiotisGeorgiadis$elm_datetime$DateTime$fromPosix(target);
		var dt_begin = PanagiotisGeorgiadis$elm_datetime$DateTime$fromPosix(begin);
		return _Utils_eq(
			A2(PanagiotisGeorgiadis$elm_datetime$DateTime$compare, dt_target, dt_begin),
			elm$core$Basics$LT) ? true : false;
	});
var author$project$Calendar2$Event$isBetween = F3(
	function (begin, end, target) {
		var dt_target = PanagiotisGeorgiadis$elm_datetime$DateTime$fromPosix(target);
		var dt_end = PanagiotisGeorgiadis$elm_datetime$DateTime$fromPosix(end);
		var dt_begin = PanagiotisGeorgiadis$elm_datetime$DateTime$fromPosix(begin);
		return _Utils_eq(
			A2(PanagiotisGeorgiadis$elm_datetime$DateTime$compare, dt_begin, dt_target),
			elm$core$Basics$LT) ? (_Utils_eq(
			A2(PanagiotisGeorgiadis$elm_datetime$DateTime$compare, dt_target, dt_end),
			elm$core$Basics$LT) ? true : false) : false;
	});
var justinmimbs$date$Date$Friday = {$: 'Friday'};
var justinmimbs$date$Date$Monday = {$: 'Monday'};
var justinmimbs$date$Date$Quarter = {$: 'Quarter'};
var justinmimbs$date$Date$Saturday = {$: 'Saturday'};
var justinmimbs$date$Date$Sunday = {$: 'Sunday'};
var justinmimbs$date$Date$Thursday = {$: 'Thursday'};
var justinmimbs$date$Date$Tuesday = {$: 'Tuesday'};
var justinmimbs$date$Date$Wednesday = {$: 'Wednesday'};
var justinmimbs$date$Date$Week = {$: 'Week'};
var justinmimbs$date$Date$Year = {$: 'Year'};
var justinmimbs$time_extra$Time$Extra$floorDate = F3(
	function (dateInterval, zone, posix) {
		return A3(
			justinmimbs$time_extra$Time$Extra$posixFromDateTime,
			zone,
			A2(
				justinmimbs$date$Date$floor,
				dateInterval,
				A2(justinmimbs$date$Date$fromPosix, zone, posix)),
			0);
	});
var justinmimbs$time_extra$Time$Extra$floor = F3(
	function (interval, zone, posix) {
		switch (interval.$) {
			case 'Millisecond':
				return posix;
			case 'Second':
				return A3(
					justinmimbs$time_extra$Time$Extra$posixFromDateTime,
					zone,
					A2(justinmimbs$date$Date$fromPosix, zone, posix),
					A4(
						justinmimbs$time_extra$Time$Extra$timeFromClock,
						A2(elm$time$Time$toHour, zone, posix),
						A2(elm$time$Time$toMinute, zone, posix),
						A2(elm$time$Time$toSecond, zone, posix),
						0));
			case 'Minute':
				return A3(
					justinmimbs$time_extra$Time$Extra$posixFromDateTime,
					zone,
					A2(justinmimbs$date$Date$fromPosix, zone, posix),
					A4(
						justinmimbs$time_extra$Time$Extra$timeFromClock,
						A2(elm$time$Time$toHour, zone, posix),
						A2(elm$time$Time$toMinute, zone, posix),
						0,
						0));
			case 'Hour':
				return A3(
					justinmimbs$time_extra$Time$Extra$posixFromDateTime,
					zone,
					A2(justinmimbs$date$Date$fromPosix, zone, posix),
					A4(
						justinmimbs$time_extra$Time$Extra$timeFromClock,
						A2(elm$time$Time$toHour, zone, posix),
						0,
						0,
						0));
			case 'Day':
				return A3(justinmimbs$time_extra$Time$Extra$floorDate, justinmimbs$date$Date$Day, zone, posix);
			case 'Month':
				return A3(justinmimbs$time_extra$Time$Extra$floorDate, justinmimbs$date$Date$Month, zone, posix);
			case 'Year':
				return A3(justinmimbs$time_extra$Time$Extra$floorDate, justinmimbs$date$Date$Year, zone, posix);
			case 'Quarter':
				return A3(justinmimbs$time_extra$Time$Extra$floorDate, justinmimbs$date$Date$Quarter, zone, posix);
			case 'Week':
				return A3(justinmimbs$time_extra$Time$Extra$floorDate, justinmimbs$date$Date$Week, zone, posix);
			case 'Monday':
				return A3(justinmimbs$time_extra$Time$Extra$floorDate, justinmimbs$date$Date$Monday, zone, posix);
			case 'Tuesday':
				return A3(justinmimbs$time_extra$Time$Extra$floorDate, justinmimbs$date$Date$Tuesday, zone, posix);
			case 'Wednesday':
				return A3(justinmimbs$time_extra$Time$Extra$floorDate, justinmimbs$date$Date$Wednesday, zone, posix);
			case 'Thursday':
				return A3(justinmimbs$time_extra$Time$Extra$floorDate, justinmimbs$date$Date$Thursday, zone, posix);
			case 'Friday':
				return A3(justinmimbs$time_extra$Time$Extra$floorDate, justinmimbs$date$Date$Friday, zone, posix);
			case 'Saturday':
				return A3(justinmimbs$time_extra$Time$Extra$floorDate, justinmimbs$date$Date$Saturday, zone, posix);
			default:
				return A3(justinmimbs$time_extra$Time$Extra$floorDate, justinmimbs$date$Date$Sunday, zone, posix);
		}
	});
var justinmimbs$time_extra$Time$Extra$posixToParts = F2(
	function (zone, posix) {
		return {
			day: A2(elm$time$Time$toDay, zone, posix),
			hour: A2(elm$time$Time$toHour, zone, posix),
			millisecond: A2(elm$time$Time$toMillis, zone, posix),
			minute: A2(elm$time$Time$toMinute, zone, posix),
			month: A2(elm$time$Time$toMonth, zone, posix),
			second: A2(elm$time$Time$toSecond, zone, posix),
			year: A2(elm$time$Time$toYear, zone, posix)
		};
	});
var author$project$Calendar2$Event$rangeDescription = F4(
	function (start, end, interval, posix) {
		var start_parts = A2(justinmimbs$time_extra$Time$Extra$posixToParts, elm$time$Time$utc, start);
		var parts = A2(justinmimbs$time_extra$Time$Extra$posixToParts, elm$time$Time$utc, posix);
		var end_parts = A2(justinmimbs$time_extra$Time$Extra$posixToParts, elm$time$Time$utc, end);
		var endInterval = A4(justinmimbs$time_extra$Time$Extra$add, interval, 1, elm$time$Time$utc, posix);
		var endsAfterInterval = A2(author$project$Calendar2$Event$isAfter, endInterval, end);
		var begInterval = A3(justinmimbs$time_extra$Time$Extra$floor, interval, elm$time$Time$utc, posix);
		var endsThisInterval = A3(author$project$Calendar2$Event$isBetween, begInterval, endInterval, end);
		var startsBeforeInterval = A2(author$project$Calendar2$Event$isBefore, begInterval, start);
		var startsThisInterval = A3(author$project$Calendar2$Event$isBetween, begInterval, endInterval, start);
		return (startsThisInterval && endsThisInterval) ? author$project$Calendar2$Event$StartsAndEnds : ((startsBeforeInterval && endsAfterInterval) ? author$project$Calendar2$Event$ContinuesAfterAndPrior : ((startsThisInterval && endsAfterInterval) ? author$project$Calendar2$Event$ContinuesAfter : ((endsThisInterval && startsBeforeInterval) ? author$project$Calendar2$Event$ContinuesPrior : author$project$Calendar2$Event$ExistsOutside)));
	});
var author$project$Calendar2$Day$viewDayEvent = F4(
	function (config, day, selectedId, event) {
		var eventStart = config.start(event);
		var eventEnd = config.end(event);
		var eventRange = A4(author$project$Calendar2$Event$rangeDescription, eventStart, eventEnd, justinmimbs$time_extra$Time$Extra$Day, day);
		return A4(author$project$Calendar2$Event$maybeViewDayEvent, config, event, selectedId, eventRange);
	});
var author$project$Calendar2$Day$viewDayEvents = F4(
	function (config, events, selectedId, day) {
		return A2(
			elm$core$List$filterMap,
			A3(author$project$Calendar2$Day$viewDayEvent, config, day, selectedId),
			events);
	});
var author$project$Calendar2$Msg$TimeSlotClick = F2(
	function (a, b) {
		return {$: 'TimeSlotClick', a: a, b: b};
	});
var author$project$Calendar2$Msg$TimeSlotDragStart = F2(
	function (a, b) {
		return {$: 'TimeSlotDragStart', a: a, b: b};
	});
var author$project$Calendar2$Msg$TimeSlotMouseEnter = F2(
	function (a, b) {
		return {$: 'TimeSlotMouseEnter', a: a, b: b};
	});
var author$project$Calendar2$Msg$TimeSlotMouseLeave = F2(
	function (a, b) {
		return {$: 'TimeSlotMouseLeave', a: a, b: b};
	});
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions = {preventDefault: true, stopPropagation: false};
var elm$virtual_dom$VirtualDom$Custom = function (a) {
	return {$: 'Custom', a: a};
};
var elm$html$Html$Events$custom = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$Custom(decoder));
	});
var elm$json$Json$Decode$map6 = _Json_map6;
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$Event = F6(
	function (keys, button, clientPos, offsetPos, pagePos, screenPos) {
		return {button: button, clientPos: clientPos, keys: keys, offsetPos: offsetPos, pagePos: pagePos, screenPos: screenPos};
	});
var elm$json$Json$Decode$int = _Json_decodeInt;
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$BackButton = {$: 'BackButton'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ErrorButton = {$: 'ErrorButton'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ForwardButton = {$: 'ForwardButton'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MainButton = {$: 'MainButton'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MiddleButton = {$: 'MiddleButton'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$SecondButton = {$: 'SecondButton'};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonFromId = function (id) {
	switch (id) {
		case 0:
			return mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MainButton;
		case 1:
			return mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$MiddleButton;
		case 2:
			return mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$SecondButton;
		case 3:
			return mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$BackButton;
		case 4:
			return mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ForwardButton;
		default:
			return mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$ErrorButton;
	}
};
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonDecoder = A2(
	elm$json$Json$Decode$map,
	mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonFromId,
	A2(elm$json$Json$Decode$field, 'button', elm$json$Json$Decode$int));
var elm$json$Json$Decode$float = _Json_decodeFloat;
var mpizenberg$elm_pointer_events$Internal$Decode$clientPos = A3(
	elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2(elm$json$Json$Decode$field, 'clientX', elm$json$Json$Decode$float),
	A2(elm$json$Json$Decode$field, 'clientY', elm$json$Json$Decode$float));
var elm$json$Json$Decode$bool = _Json_decodeBool;
var elm$json$Json$Decode$map3 = _Json_map3;
var mpizenberg$elm_pointer_events$Internal$Decode$Keys = F3(
	function (alt, ctrl, shift) {
		return {alt: alt, ctrl: ctrl, shift: shift};
	});
var mpizenberg$elm_pointer_events$Internal$Decode$keys = A4(
	elm$json$Json$Decode$map3,
	mpizenberg$elm_pointer_events$Internal$Decode$Keys,
	A2(elm$json$Json$Decode$field, 'altKey', elm$json$Json$Decode$bool),
	A2(elm$json$Json$Decode$field, 'ctrlKey', elm$json$Json$Decode$bool),
	A2(elm$json$Json$Decode$field, 'shiftKey', elm$json$Json$Decode$bool));
var mpizenberg$elm_pointer_events$Internal$Decode$offsetPos = A3(
	elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2(elm$json$Json$Decode$field, 'offsetX', elm$json$Json$Decode$float),
	A2(elm$json$Json$Decode$field, 'offsetY', elm$json$Json$Decode$float));
var mpizenberg$elm_pointer_events$Internal$Decode$pagePos = A3(
	elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2(elm$json$Json$Decode$field, 'pageX', elm$json$Json$Decode$float),
	A2(elm$json$Json$Decode$field, 'pageY', elm$json$Json$Decode$float));
var mpizenberg$elm_pointer_events$Internal$Decode$screenPos = A3(
	elm$json$Json$Decode$map2,
	F2(
		function (a, b) {
			return _Utils_Tuple2(a, b);
		}),
	A2(elm$json$Json$Decode$field, 'screenX', elm$json$Json$Decode$float),
	A2(elm$json$Json$Decode$field, 'screenY', elm$json$Json$Decode$float));
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$eventDecoder = A7(elm$json$Json$Decode$map6, mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$Event, mpizenberg$elm_pointer_events$Internal$Decode$keys, mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$buttonDecoder, mpizenberg$elm_pointer_events$Internal$Decode$clientPos, mpizenberg$elm_pointer_events$Internal$Decode$offsetPos, mpizenberg$elm_pointer_events$Internal$Decode$pagePos, mpizenberg$elm_pointer_events$Internal$Decode$screenPos);
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions = F3(
	function (event, options, tag) {
		return A2(
			elm$html$Html$Events$custom,
			event,
			A2(
				elm$json$Json$Decode$map,
				function (ev) {
					return {
						message: tag(ev),
						preventDefault: options.preventDefault,
						stopPropagation: options.stopPropagation
					};
				},
				mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$eventDecoder));
	});
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onClick = A2(mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions, 'click', mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions);
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onDown = A2(mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions, 'mousedown', mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions);
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onEnter = A2(mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions, 'mouseenter', mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions);
var mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onLeave = A2(mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onWithOptions, 'mouseleave', mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$defaultOptions);
var author$project$Calendar2$Day$viewTimeSlot = function (date) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--time-slot'),
				mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onClick(
				function (event) {
					return A2(author$project$Calendar2$Msg$TimeSlotClick, date, event);
				}),
				mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onEnter(
				function (event) {
					return A2(author$project$Calendar2$Msg$TimeSlotMouseEnter, date, event);
				}),
				mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onLeave(
				function (event) {
					return A2(author$project$Calendar2$Msg$TimeSlotMouseLeave, date, event);
				}),
				mpizenberg$elm_pointer_events$Html$Events$Extra$Mouse$onDown(
				function (event) {
					return A2(author$project$Calendar2$Msg$TimeSlotDragStart, date, event);
				})
			]),
		_List_Nil);
};
var author$project$Calendar2$Day$viewDaySlotGroup = function (date) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--time-slot-group')
			]),
		_List_fromArray(
			[
				author$project$Calendar2$Day$viewTimeSlot(date),
				author$project$Calendar2$Day$viewTimeSlot(date)
			]));
};
var justinmimbs$time_extra$Time$Extra$Hour = {$: 'Hour'};
var justinmimbs$time_extra$Time$Extra$ceiling = F3(
	function (interval, zone, posix) {
		var floored = A3(justinmimbs$time_extra$Time$Extra$floor, interval, zone, posix);
		return _Utils_eq(floored, posix) ? posix : A4(justinmimbs$time_extra$Time$Extra$add, interval, 1, zone, floored);
	});
var justinmimbs$time_extra$Time$Extra$rangeHelp = F6(
	function (interval, step, zone, until, revList, current) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(
				elm$time$Time$posixToMillis(current),
				elm$time$Time$posixToMillis(until)) < 0) {
				var $temp$interval = interval,
					$temp$step = step,
					$temp$zone = zone,
					$temp$until = until,
					$temp$revList = A2(elm$core$List$cons, current, revList),
					$temp$current = A4(justinmimbs$time_extra$Time$Extra$add, interval, step, zone, current);
				interval = $temp$interval;
				step = $temp$step;
				zone = $temp$zone;
				until = $temp$until;
				revList = $temp$revList;
				current = $temp$current;
				continue rangeHelp;
			} else {
				return elm$core$List$reverse(revList);
			}
		}
	});
var justinmimbs$time_extra$Time$Extra$range = F5(
	function (interval, step, zone, start, until) {
		return A6(
			justinmimbs$time_extra$Time$Extra$rangeHelp,
			interval,
			A2(elm$core$Basics$max, 1, step),
			zone,
			until,
			_List_Nil,
			A3(justinmimbs$time_extra$Time$Extra$ceiling, interval, zone, start));
	});
var author$project$Helpers$hours = function (posix) {
	var start = A3(justinmimbs$time_extra$Time$Extra$floor, justinmimbs$time_extra$Time$Extra$Day, elm$time$Time$utc, posix);
	var last = A4(justinmimbs$time_extra$Time$Extra$add, justinmimbs$time_extra$Time$Extra$Hour, 24, elm$time$Time$utc, start);
	return A5(justinmimbs$time_extra$Time$Extra$range, justinmimbs$time_extra$Time$Extra$Hour, 1, elm$time$Time$utc, start, last);
};
var author$project$Calendar2$Day$viewDaySlot = F4(
	function (config, events, selectedId, day) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--day-slot')
				]),
			A2(
				author$project$Calendar2$Day$flip(elm$core$Basics$append),
				A4(author$project$Calendar2$Day$viewDayEvents, config, events, selectedId, day),
				A2(
					elm$core$List$map,
					author$project$Calendar2$Day$viewDaySlotGroup,
					author$project$Helpers$hours(day))));
	});
var elm$html$Html$span = _VirtualDom_node('span');
var author$project$Calendar2$Day$viewHourSlot = function (date) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--hour-slot')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$span,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('elm-calendar--time-slot-text')
					]),
				_List_fromArray(
					[
						elm$html$Html$text(
						author$project$Helpers$hourString(date))
					]))
			]));
};
var author$project$Calendar2$Day$viewTimeSlotGroup = function (date) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--time-slot-group')
			]),
		_List_fromArray(
			[
				author$project$Calendar2$Day$viewHourSlot(date),
				A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('elm-calendar--time-slot')
					]),
				_List_Nil)
			]));
};
var author$project$Calendar2$Day$viewTimeGutter = function (date) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--time-gutter')
			]),
		A2(
			elm$core$List$map,
			author$project$Calendar2$Day$viewTimeSlotGroup,
			author$project$Helpers$hours(date)));
};
var author$project$Calendar2$Day$view = F4(
	function (config, events, selectedId, day) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--day')
				]),
			_List_fromArray(
				[
					author$project$Calendar2$Day$viewDayHeader(day),
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('elm-calendar--day-content')
						]),
					_List_fromArray(
						[
							author$project$Calendar2$Day$viewTimeGutter(day),
							A4(author$project$Calendar2$Day$viewDaySlot, config, events, selectedId, day)
						]))
				]));
	});
var author$project$Calendar2$Msg$PageBack = {$: 'PageBack'};
var author$project$Calendar2$Msg$PageForward = {$: 'PageForward'};
var elm$html$Html$button = _VirtualDom_node('button');
var author$project$Calendar2$Internal$viewPagination = A2(
	elm$html$Html$div,
	_List_fromArray(
		[
			elm$html$Html$Attributes$class('elm-calendar--paginators')
		]),
	_List_fromArray(
		[
			A2(
			elm$html$Html$button,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--button'),
					elm$html$Html$Events$onClick(author$project$Calendar2$Msg$PageBack)
				]),
			_List_fromArray(
				[
					elm$html$Html$text('back')
				])),
			A2(
			elm$html$Html$button,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--button'),
					elm$html$Html$Events$onClick(author$project$Calendar2$Msg$PageForward)
				]),
			_List_fromArray(
				[
					elm$html$Html$text('next')
				]))
		]));
var author$project$Calendar2$Msg$ChangeTimeSpan = function (a) {
	return {$: 'ChangeTimeSpan', a: a};
};
var author$project$Calendar2$Internal$viewTimeSpanSelection = function (timeSpan) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--time-spans')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$button,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('elm-calendar--button'),
						elm$html$Html$Events$onClick(
						author$project$Calendar2$Msg$ChangeTimeSpan(author$project$Calendar2$Msg$Month))
					]),
				_List_fromArray(
					[
						elm$html$Html$text('Month')
					])),
				A2(
				elm$html$Html$button,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('elm-calendar--button'),
						elm$html$Html$Events$onClick(
						author$project$Calendar2$Msg$ChangeTimeSpan(author$project$Calendar2$Msg$Week))
					]),
				_List_fromArray(
					[
						elm$html$Html$text('Week')
					])),
				A2(
				elm$html$Html$button,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('elm-calendar--button'),
						elm$html$Html$Events$onClick(
						author$project$Calendar2$Msg$ChangeTimeSpan(author$project$Calendar2$Msg$Day))
					]),
				_List_fromArray(
					[
						elm$html$Html$text('Day')
					])),
				A2(
				elm$html$Html$button,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('elm-calendar--button'),
						elm$html$Html$Events$onClick(
						author$project$Calendar2$Msg$ChangeTimeSpan(author$project$Calendar2$Msg$Agenda))
					]),
				_List_fromArray(
					[
						elm$html$Html$text('Agenda')
					]))
			]));
};
var elm$html$Html$h2 = _VirtualDom_node('h2');
var author$project$Calendar2$Internal$viewTitle = function (viewing) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--month-title')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$h2,
				_List_Nil,
				_List_fromArray(
					[
						elm$html$Html$text(
						A2(
							justinmimbs$date$Date$format,
							'MMMM yyyy',
							A2(justinmimbs$date$Date$fromPosix, elm$time$Time$utc, viewing)))
					]))
			]));
};
var author$project$Calendar2$Internal$viewToolbar = F2(
	function (viewing, timeSpan) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--toolbar')
				]),
			_List_fromArray(
				[
					author$project$Calendar2$Internal$viewPagination,
					author$project$Calendar2$Internal$viewTitle(viewing),
					author$project$Calendar2$Internal$viewTimeSpanSelection(timeSpan)
				]));
	});
var author$project$Calendar2$Month$dayOfWeekFromWeekdayNumber = function (n) {
	switch (n) {
		case 1:
			return elm$time$Time$Mon;
		case 2:
			return elm$time$Time$Tue;
		case 3:
			return elm$time$Time$Wed;
		case 4:
			return elm$time$Time$Thu;
		case 5:
			return elm$time$Time$Fri;
		case 6:
			return elm$time$Time$Sat;
		default:
			return elm$time$Time$Sun;
	}
};
var author$project$Calendar2$Month$toJapaneseWeekday = function (weekday) {
	switch (weekday.$) {
		case 'Mon':
			return '???';
		case 'Tue':
			return '???';
		case 'Wed':
			return '???';
		case 'Thu':
			return '???';
		case 'Fri':
			return '???';
		case 'Sat':
			return '???';
		default:
			return '???';
	}
};
var author$project$Calendar2$Month$viewDay = function (weekday) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--month-day-header')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$a,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('elm-calendar--date'),
						elm$html$Html$Attributes$href('#')
					]),
				_List_fromArray(
					[
						elm$html$Html$text(
						author$project$Calendar2$Month$toJapaneseWeekday(weekday))
					]))
			]));
};
var author$project$Calendar2$Month$viewMonthHeader = function () {
	var viewDayOfWeek = function (_int) {
		return author$project$Calendar2$Month$viewDay(
			author$project$Calendar2$Month$dayOfWeekFromWeekdayNumber(_int));
	};
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--row')
			]),
		A2(
			elm$core$List$map,
			viewDayOfWeek,
			A2(elm$core$List$range, 0, 6)));
}();
var author$project$Calendar2$Month$viewMonthRowBackground = function (week) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--month-row-background')
			]),
		A2(
			elm$core$List$map,
			function (_n0) {
				return A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('elm-calendar--cell')
						]),
					_List_Nil);
			},
			week));
};
var author$project$Calendar2$Event$cellWidth = 100.0 / 7;
var author$project$Calendar2$Event$offsetLength = function (date) {
	return author$project$Calendar2$Event$cellWidth * justinmimbs$date$Date$weekdayNumber(date);
};
var author$project$Calendar2$Event$offsetPercentage = function (date) {
	return elm$core$String$fromFloat(
		author$project$Calendar2$Event$offsetLength(date)) + '%';
};
var author$project$Calendar2$Event$styleRowSegment = function (widthPercentage) {
	return _List_fromArray(
		[
			A2(elm$html$Html$Attributes$style, 'flex-basis', widthPercentage),
			A2(elm$html$Html$Attributes$style, 'max-width', widthPercentage)
		]);
};
var author$project$Calendar2$Event$rowSegment = F2(
	function (widthPercentage, children) {
		return A2(
			elm$html$Html$div,
			author$project$Calendar2$Event$styleRowSegment(widthPercentage),
			children);
	});
var elm$core$Basics$truncate = _Basics_truncate;
var justinmimbs$date$Date$toMonths = function (rd) {
	var date = justinmimbs$date$Date$toCalendarDate(
		justinmimbs$date$Date$RD(rd));
	var wholeMonths = (12 * (date.year - 1)) + (justinmimbs$date$Date$monthToNumber(date.month) - 1);
	return wholeMonths + (date.day / 100);
};
var justinmimbs$date$Date$diff = F3(
	function (unit, _n0, _n1) {
		var rd1 = _n0.a;
		var rd2 = _n1.a;
		switch (unit.$) {
			case 'Years':
				return (((justinmimbs$date$Date$toMonths(rd2) - justinmimbs$date$Date$toMonths(rd1)) | 0) / 12) | 0;
			case 'Months':
				return (justinmimbs$date$Date$toMonths(rd2) - justinmimbs$date$Date$toMonths(rd1)) | 0;
			case 'Weeks':
				return ((rd2 - rd1) / 7) | 0;
			default:
				return rd2 - rd1;
		}
	});
var author$project$Calendar2$Event$viewMonthEvent = F4(
	function (config, event, selectedId, eventRange) {
		var eventStart = A2(
			justinmimbs$date$Date$fromPosix,
			elm$time$Time$utc,
			config.start(event));
		var eventEnd = A2(
			justinmimbs$date$Date$fromPosix,
			elm$time$Time$utc,
			config.end(event));
		var numDaysThisWeek = function () {
			switch (eventRange.$) {
				case 'StartsAndEnds':
					return A3(justinmimbs$date$Date$diff, justinmimbs$date$Date$Days, eventStart, eventEnd) + 1;
				case 'ContinuesAfter':
					return (7 - justinmimbs$date$Date$weekdayNumber(eventStart)) + 1;
				case 'ContinuesPrior':
					return (7 - justinmimbs$date$Date$weekdayNumber(eventEnd)) + 1;
				case 'ContinuesAfterAndPrior':
					return 7;
				default:
					return 0;
			}
		}();
		var eventWidthPercentage = function (eventRange2) {
			return elm$core$String$fromFloat(author$project$Calendar2$Event$cellWidth * numDaysThisWeek) + '%';
		};
		return (author$project$Calendar2$Event$offsetLength(eventStart) > 0) ? A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--row')
				]),
			_List_fromArray(
				[
					A2(
					author$project$Calendar2$Event$rowSegment,
					author$project$Calendar2$Event$offsetPercentage(eventStart),
					_List_Nil),
					A2(
					author$project$Calendar2$Event$rowSegment,
					eventWidthPercentage(eventRange),
					_List_fromArray(
						[
							A5(author$project$Calendar2$Event$eventSegment, config, event, selectedId, eventRange, author$project$Calendar2$Msg$Month)
						]))
				])) : A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--row')
				]),
			_List_fromArray(
				[
					A2(
					author$project$Calendar2$Event$rowSegment,
					eventWidthPercentage(eventRange),
					_List_fromArray(
						[
							A5(author$project$Calendar2$Event$eventSegment, config, event, selectedId, eventRange, author$project$Calendar2$Msg$Month)
						]))
				]));
	});
var author$project$Calendar2$Event$maybeViewMonthEvent = F4(
	function (config, event, selectedId, eventRange) {
		if (eventRange.$ === 'ExistsOutside') {
			return elm$core$Maybe$Nothing;
		} else {
			return elm$core$Maybe$Just(
				A4(author$project$Calendar2$Event$viewMonthEvent, config, event, selectedId, eventRange));
		}
	});
var elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return elm$core$Maybe$Nothing;
		}
	});
var justinmimbs$time_extra$Time$Extra$Sunday = {$: 'Sunday'};
var author$project$Calendar2$Month$viewWeekEvent = F4(
	function (config, week, selectedId, event) {
		var eventStart = config.start(event);
		var eventEnd = config.end(event);
		var eventRange = function (sunday) {
			return A4(author$project$Calendar2$Event$rangeDescription, eventStart, eventEnd, justinmimbs$time_extra$Time$Extra$Sunday, sunday);
		};
		return A2(
			elm$core$Maybe$andThen,
			A3(author$project$Calendar2$Event$maybeViewMonthEvent, config, event, selectedId),
			A2(
				elm$core$Maybe$map,
				eventRange,
				elm$core$List$head(week)));
	});
var elm$core$List$takeReverse = F3(
	function (n, list, kept) {
		takeReverse:
		while (true) {
			if (n <= 0) {
				return kept;
			} else {
				if (!list.b) {
					return kept;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs,
						$temp$kept = A2(elm$core$List$cons, x, kept);
					n = $temp$n;
					list = $temp$list;
					kept = $temp$kept;
					continue takeReverse;
				}
			}
		}
	});
var elm$core$List$takeTailRec = F2(
	function (n, list) {
		return elm$core$List$reverse(
			A3(elm$core$List$takeReverse, n, list, _List_Nil));
	});
var elm$core$List$takeFast = F3(
	function (ctr, n, list) {
		if (n <= 0) {
			return _List_Nil;
		} else {
			var _n0 = _Utils_Tuple2(n, list);
			_n0$1:
			while (true) {
				_n0$5:
				while (true) {
					if (!_n0.b.b) {
						return list;
					} else {
						if (_n0.b.b.b) {
							switch (_n0.a) {
								case 1:
									break _n0$1;
								case 2:
									var _n2 = _n0.b;
									var x = _n2.a;
									var _n3 = _n2.b;
									var y = _n3.a;
									return _List_fromArray(
										[x, y]);
								case 3:
									if (_n0.b.b.b.b) {
										var _n4 = _n0.b;
										var x = _n4.a;
										var _n5 = _n4.b;
										var y = _n5.a;
										var _n6 = _n5.b;
										var z = _n6.a;
										return _List_fromArray(
											[x, y, z]);
									} else {
										break _n0$5;
									}
								default:
									if (_n0.b.b.b.b && _n0.b.b.b.b.b) {
										var _n7 = _n0.b;
										var x = _n7.a;
										var _n8 = _n7.b;
										var y = _n8.a;
										var _n9 = _n8.b;
										var z = _n9.a;
										var _n10 = _n9.b;
										var w = _n10.a;
										var tl = _n10.b;
										return (ctr > 1000) ? A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A2(elm$core$List$takeTailRec, n - 4, tl))))) : A2(
											elm$core$List$cons,
											x,
											A2(
												elm$core$List$cons,
												y,
												A2(
													elm$core$List$cons,
													z,
													A2(
														elm$core$List$cons,
														w,
														A3(elm$core$List$takeFast, ctr + 1, n - 4, tl)))));
									} else {
										break _n0$5;
									}
							}
						} else {
							if (_n0.a === 1) {
								break _n0$1;
							} else {
								break _n0$5;
							}
						}
					}
				}
				return list;
			}
			var _n1 = _n0.b;
			var x = _n1.a;
			return _List_fromArray(
				[x]);
		}
	});
var elm$core$List$take = F2(
	function (n, list) {
		return A3(elm$core$List$takeFast, 0, n, list);
	});
var author$project$Calendar2$Month$viewMonthRowContent = F4(
	function (config, events, selectedId, week) {
		var eventRows = A2(
			elm$core$List$take,
			3,
			A2(
				elm$core$List$filterMap,
				A3(author$project$Calendar2$Month$viewWeekEvent, config, week, selectedId),
				events));
		var dateCell = function (posix) {
			return A2(
				elm$html$Html$div,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('elm-calendar--date-cell')
					]),
				_List_fromArray(
					[
						elm$html$Html$text(
						elm$core$String$fromInt(
							justinmimbs$date$Date$day(
								A2(justinmimbs$date$Date$fromPosix, elm$time$Time$utc, posix))))
					]));
		};
		var datesRow = A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--row')
				]),
			A2(elm$core$List$map, dateCell, week));
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--month-week')
				]),
			A2(elm$core$List$cons, datesRow, eventRows));
	});
var author$project$Calendar2$Month$viewMonthRow = F4(
	function (config, events, selectedId, week) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--month-row')
				]),
			_List_fromArray(
				[
					author$project$Calendar2$Month$viewMonthRowBackground(week),
					A4(author$project$Calendar2$Month$viewMonthRowContent, config, events, selectedId, week)
				]));
	});
var elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var elm_community$list_extra$List$Extra$groupsOfWithStep = F3(
	function (size, step, xs) {
		var xs_ = A2(elm$core$List$drop, step, xs);
		var thisGroup = A2(elm$core$List$take, size, xs);
		var okayLength = _Utils_eq(
			size,
			elm$core$List$length(thisGroup));
		var okayArgs = (size > 0) && (step > 0);
		return (okayArgs && okayLength) ? A2(
			elm$core$List$cons,
			thisGroup,
			A3(elm_community$list_extra$List$Extra$groupsOfWithStep, size, step, xs_)) : _List_Nil;
	});
var elm_community$list_extra$List$Extra$groupsOf = F2(
	function (size, xs) {
		return A3(elm_community$list_extra$List$Extra$groupsOfWithStep, size, size, xs);
	});
var author$project$Helpers$weekRangesFromMonth = F2(
	function (year, month) {
		var firstOfMonth = A2(
			justinmimbs$time_extra$Time$Extra$partsToPosix,
			elm$time$Time$utc,
			A7(justinmimbs$time_extra$Time$Extra$Parts, year, month, 1, 0, 0, 0, 0));
		var firstOfNextMonth = A4(justinmimbs$time_extra$Time$Extra$add, justinmimbs$time_extra$Time$Extra$Month, 1, elm$time$Time$utc, firstOfMonth);
		return A2(
			elm_community$list_extra$List$Extra$groupsOf,
			7,
			A5(
				justinmimbs$time_extra$Time$Extra$range,
				justinmimbs$time_extra$Time$Extra$Day,
				1,
				elm$time$Time$utc,
				A3(justinmimbs$time_extra$Time$Extra$floor, justinmimbs$time_extra$Time$Extra$Sunday, elm$time$Time$utc, firstOfMonth),
				A3(justinmimbs$time_extra$Time$Extra$ceiling, justinmimbs$time_extra$Time$Extra$Sunday, elm$time$Time$utc, firstOfNextMonth)));
	});
var author$project$Calendar2$Month$view = F4(
	function (config, events, selectedId, viewing) {
		var weeks = A2(
			author$project$Helpers$weekRangesFromMonth,
			A2(elm$time$Time$toYear, elm$time$Time$utc, viewing),
			A2(elm$time$Time$toMonth, elm$time$Time$utc, viewing));
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--column')
				]),
			_List_fromArray(
				[
					author$project$Calendar2$Month$viewMonthHeader,
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('elm-calendar--month')
						]),
					A2(
						elm$core$List$map,
						A3(author$project$Calendar2$Month$viewMonthRow, config, events, selectedId),
						weeks))
				]));
	});
var author$project$Calendar2$Week$viewWeekDay = F4(
	function (config, events, selectedId, posix) {
		var viewDaySlots = A2(
			elm$core$List$map,
			author$project$Calendar2$Day$viewDaySlotGroup,
			author$project$Helpers$hours(posix));
		var dayEvents = A4(author$project$Calendar2$Day$viewDayEvents, config, events, selectedId, posix);
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--day')
				]),
			_Utils_ap(viewDaySlots, dayEvents));
	});
var author$project$Calendar2$Week$viewWeekContent = F5(
	function (config, events, selectedId, viewing, days) {
		var weekDays = A2(
			elm$core$List$map,
			A3(author$project$Calendar2$Week$viewWeekDay, config, events, selectedId),
			days);
		var timeGutter = author$project$Calendar2$Day$viewTimeGutter(viewing);
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--week-content')
				]),
			A2(elm$core$List$cons, timeGutter, weekDays));
	});
var author$project$Calendar2$Day$viewAllDayCell = function (days) {
	var viewAllDayText = A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--all-day-text')
			]),
		_List_fromArray(
			[
				elm$html$Html$text('All day')
			]));
	var viewAllDay = function (day) {
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--all-day')
				]),
			_List_Nil);
	};
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--all-day-cell')
			]),
		A2(
			elm$core$List$cons,
			viewAllDayText,
			A2(elm$core$List$map, viewAllDay, days)));
};
var author$project$Calendar2$Week$viewDates = function (days) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--dates')
			]),
		A2(
			elm$core$List$cons,
			author$project$Calendar2$Day$viewTimeGutterHeader,
			A2(elm$core$List$map, author$project$Calendar2$Day$viewDate, days)));
};
var author$project$Calendar2$Week$viewWeekHeader = function (days) {
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('elm-calendar--week-header')
			]),
		_List_fromArray(
			[
				author$project$Calendar2$Week$viewDates(days),
				author$project$Calendar2$Day$viewAllDayCell(days)
			]));
};
var author$project$Helpers$dayRangeOfWeek = function (posix) {
	return A5(
		justinmimbs$time_extra$Time$Extra$range,
		justinmimbs$time_extra$Time$Extra$Day,
		1,
		elm$time$Time$utc,
		A3(justinmimbs$time_extra$Time$Extra$floor, justinmimbs$time_extra$Time$Extra$Sunday, elm$time$Time$utc, posix),
		A3(justinmimbs$time_extra$Time$Extra$ceiling, justinmimbs$time_extra$Time$Extra$Sunday, elm$time$Time$utc, posix));
};
var author$project$Calendar2$Week$view = F4(
	function (config, events, selectedId, viewing) {
		var weekRange = author$project$Helpers$dayRangeOfWeek(viewing);
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--week')
				]),
			_List_fromArray(
				[
					author$project$Calendar2$Week$viewWeekHeader(weekRange),
					A5(author$project$Calendar2$Week$viewWeekContent, config, events, selectedId, viewing, weekRange)
				]));
	});
var elm$html$Html$Attributes$draggable = _VirtualDom_attribute('draggable');
var author$project$Calendar2$Internal$view = F3(
	function (config, events, _n0) {
		var viewing = _n0.viewing;
		var timeSpan = _n0.timeSpan;
		var selected = _n0.selected;
		var calendarView = function () {
			switch (timeSpan.$) {
				case 'Month':
					return A4(author$project$Calendar2$Month$view, config, events, selected, viewing);
				case 'Week':
					return A4(author$project$Calendar2$Week$view, config, events, selected, viewing);
				case 'Day':
					return A4(author$project$Calendar2$Day$view, config, events, selected, viewing);
				default:
					return A3(author$project$Calendar2$Agenda$view, config, events, viewing);
			}
		}();
		return A2(
			elm$html$Html$div,
			_List_fromArray(
				[
					elm$html$Html$Attributes$class('elm-calendar--container'),
					elm$html$Html$Attributes$draggable('false')
				]),
			_List_fromArray(
				[
					A2(
					elm$html$Html$div,
					_List_fromArray(
						[
							elm$html$Html$Attributes$class('elm-calendar--calendar')
						]),
					_List_fromArray(
						[
							A2(author$project$Calendar2$Internal$viewToolbar, viewing, timeSpan),
							calendarView
						]))
				]));
	});
var elm$virtual_dom$VirtualDom$map = _VirtualDom_map;
var elm$html$Html$map = elm$virtual_dom$VirtualDom$map;
var author$project$Calendar2$view = F3(
	function (_n0, events, _n1) {
		var config = _n0.a;
		var state = _n1.a;
		return A2(
			elm$html$Html$map,
			author$project$Calendar2$Internal,
			A3(author$project$Calendar2$Internal$view, config, events, state));
	});
var author$project$Calendar2$EventView = function (a) {
	return {$: 'EventView', a: a};
};
var author$project$Calendar2$eventView = function (_n0) {
	var nodeName = _n0.nodeName;
	var classes = _n0.classes;
	var children = _n0.children;
	return author$project$Calendar2$EventView(
		{children: children, classes: classes, nodeName: nodeName});
};
var author$project$Calendar2$ViewConfig = function (a) {
	return {$: 'ViewConfig', a: a};
};
var author$project$Calendar2$viewConfig = function (_n0) {
	var toId = _n0.toId;
	var title = _n0.title;
	var start = _n0.start;
	var end = _n0.end;
	var event = _n0.event;
	var extractEventView = function (eventView_) {
		var eventView3 = eventView_.a;
		return eventView3;
	};
	var eventView2 = F2(
		function (id, selected) {
			return extractEventView(
				A2(event, id, selected));
		});
	return author$project$Calendar2$ViewConfig(
		{end: end, event: eventView2, start: start, title: title, toId: toId});
};
var author$project$Main$viewConfig = author$project$Calendar2$viewConfig(
	{
		end: function ($) {
			return $.end;
		},
		event: F2(
			function (event, isSelected) {
				return author$project$Calendar2$eventView(
					{
						children: _List_fromArray(
							[
								A2(
								elm$html$Html$div,
								_List_Nil,
								_List_fromArray(
									[
										elm$html$Html$text(event.title)
									]))
							]),
						classes: _List_fromArray(
							[
								_Utils_Tuple2('elm-calendar--event-content', true),
								_Utils_Tuple2('elm-calendar--event-content--is-selected', isSelected)
							]),
						nodeName: 'div'
					});
			}),
		start: function ($) {
			return $.start;
		},
		title: function ($) {
			return $.title;
		},
		toId: function ($) {
			return $.id;
		}
	});
var author$project$Main$px = function (str) {
	return str + 'px';
};
var author$project$Main$AddEventPreviewToEvents = {$: 'AddEventPreviewToEvents'};
var author$project$Main$CreateEventTitle = function (a) {
	return {$: 'CreateEventTitle', a: a};
};
var elm$html$Html$h3 = _VirtualDom_node('h3');
var elm$html$Html$input = _VirtualDom_node('input');
var elm$html$Html$Attributes$value = elm$html$Html$Attributes$stringProperty('value');
var elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			elm$virtual_dom$VirtualDom$on,
			event,
			elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3(elm$core$List$foldr, elm$json$Json$Decode$field, decoder, fields);
	});
var elm$html$Html$Events$targetValue = A2(
	elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	elm$json$Json$Decode$string);
var elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			elm$json$Json$Decode$map,
			elm$html$Html$Events$alwaysStop,
			A2(elm$json$Json$Decode$map, tagger, elm$html$Html$Events$targetValue)));
};
var author$project$Main$viewCreateEventDialog = function (_n0) {
	var event = _n0.event;
	var position = _n0.position;
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('create-event-dialog')
			]),
		_List_fromArray(
			[
				A2(
				elm$html$Html$h3,
				_List_fromArray(
					[
						elm$html$Html$Attributes$class('create-event-title')
					]),
				_List_fromArray(
					[
						elm$html$Html$text('Create event')
					])),
				A2(
				elm$html$Html$input,
				_List_fromArray(
					[
						elm$html$Html$Events$onInput(author$project$Main$CreateEventTitle),
						elm$html$Html$Attributes$value(event.title),
						elm$html$Html$Attributes$class('create-event-input')
					]),
				_List_Nil),
				A2(
				elm$html$Html$button,
				_List_fromArray(
					[
						elm$html$Html$Events$onClick(author$project$Main$AddEventPreviewToEvents),
						elm$html$Html$Attributes$class('create-event-button')
					]),
				_List_fromArray(
					[
						elm$html$Html$text('Create Event')
					]))
			]));
};
var justinmimbs$time_extra$Time$Extra$toFractionalDay = F2(
	function (zone, posix) {
		return A2(justinmimbs$time_extra$Time$Extra$timeFromPosix, zone, posix) / 86400000;
	});
var justinmimbs$time_extra$Time$Extra$toMonths = F2(
	function (zone, posix) {
		var wholeMonths = (12 * (A2(elm$time$Time$toYear, zone, posix) - 1)) + (justinmimbs$date$Date$monthToNumber(
			A2(elm$time$Time$toMonth, zone, posix)) - 1);
		var fractionalMonth = (A2(elm$time$Time$toDay, zone, posix) + A2(justinmimbs$time_extra$Time$Extra$toFractionalDay, zone, posix)) / 100;
		return wholeMonths + fractionalMonth;
	});
var justinmimbs$time_extra$Time$Extra$toRataDieMoment = F2(
	function (zone, posix) {
		return justinmimbs$date$Date$toRataDie(
			A2(justinmimbs$date$Date$fromPosix, zone, posix)) + A2(justinmimbs$time_extra$Time$Extra$toFractionalDay, zone, posix);
	});
var justinmimbs$time_extra$Time$Extra$diff = F4(
	function (interval, zone, posix1, posix2) {
		diff:
		while (true) {
			switch (interval.$) {
				case 'Millisecond':
					return elm$time$Time$posixToMillis(posix2) - elm$time$Time$posixToMillis(posix1);
				case 'Second':
					return (A4(justinmimbs$time_extra$Time$Extra$diff, justinmimbs$time_extra$Time$Extra$Millisecond, zone, posix1, posix2) / 1000) | 0;
				case 'Minute':
					return (A4(justinmimbs$time_extra$Time$Extra$diff, justinmimbs$time_extra$Time$Extra$Millisecond, zone, posix1, posix2) / 60000) | 0;
				case 'Hour':
					return (A4(justinmimbs$time_extra$Time$Extra$diff, justinmimbs$time_extra$Time$Extra$Millisecond, zone, posix1, posix2) / 3600000) | 0;
				case 'Day':
					return (A2(justinmimbs$time_extra$Time$Extra$toRataDieMoment, zone, posix2) - A2(justinmimbs$time_extra$Time$Extra$toRataDieMoment, zone, posix1)) | 0;
				case 'Month':
					return (A2(justinmimbs$time_extra$Time$Extra$toMonths, zone, posix2) - A2(justinmimbs$time_extra$Time$Extra$toMonths, zone, posix1)) | 0;
				case 'Year':
					return (A4(justinmimbs$time_extra$Time$Extra$diff, justinmimbs$time_extra$Time$Extra$Month, zone, posix1, posix2) / 12) | 0;
				case 'Quarter':
					return (A4(justinmimbs$time_extra$Time$Extra$diff, justinmimbs$time_extra$Time$Extra$Month, zone, posix1, posix2) / 3) | 0;
				case 'Week':
					return (A4(justinmimbs$time_extra$Time$Extra$diff, justinmimbs$time_extra$Time$Extra$Day, zone, posix1, posix2) / 7) | 0;
				default:
					var weekday = interval;
					var $temp$interval = justinmimbs$time_extra$Time$Extra$Week,
						$temp$zone = zone,
						$temp$posix1 = A3(justinmimbs$time_extra$Time$Extra$floor, weekday, zone, posix1),
						$temp$posix2 = A3(justinmimbs$time_extra$Time$Extra$floor, weekday, zone, posix2);
					interval = $temp$interval;
					zone = $temp$zone;
					posix1 = $temp$posix1;
					posix2 = $temp$posix2;
					continue diff;
			}
		}
	});
var author$project$Main$viewCreateEvent = function (preview) {
	var event = preview.event;
	var position = preview.position;
	var showDialog = preview.showDialog;
	var duration = A4(justinmimbs$time_extra$Time$Extra$diff, justinmimbs$time_extra$Time$Extra$Minute, elm$time$Time$utc, event.start, event.end);
	var height = elm$core$String$fromInt(20 * ((duration / 30) | 0));
	return A2(
		elm$html$Html$div,
		_List_fromArray(
			[
				elm$html$Html$Attributes$class('event-preview'),
				A2(elm$html$Html$Attributes$style, 'position', 'absolute'),
				A2(
				elm$html$Html$Attributes$style,
				'top',
				author$project$Main$px(
					elm$core$String$fromFloat(position.offsetPos.b))),
				A2(
				elm$html$Html$Attributes$style,
				'left',
				author$project$Main$px(
					elm$core$String$fromFloat(position.offsetPos.a))),
				A2(
				elm$html$Html$Attributes$style,
				'height',
				author$project$Main$px(height)),
				A2(elm$html$Html$Attributes$style, 'z-index', '2')
			]),
		_List_fromArray(
			[
				showDialog ? author$project$Main$viewCreateEventDialog(preview) : elm$html$Html$text(''),
				elm$html$Html$text('New Event')
			]));
};
var elm$core$Dict$values = function (dict) {
	return A3(
		elm$core$Dict$foldr,
		F3(
			function (key, value, valueList) {
				return A2(elm$core$List$cons, value, valueList);
			}),
		_List_Nil,
		dict);
};
var author$project$Main$view = function (model_) {
	var selectedEvent = model_.selectedEvent;
	var events = elm$core$Dict$values(model_.events);
	return A2(
		elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				function () {
				var _n0 = model_.eventPreview;
				if (_n0.$ === 'Just') {
					var preview = _n0.a;
					return author$project$Main$viewCreateEvent(preview);
				} else {
					return elm$html$Html$text('');
				}
			}(),
				A2(
				elm$html$Html$map,
				author$project$Main$SetCalendarState,
				A3(author$project$Calendar2$view, author$project$Main$viewConfig, events, model_.calendarState))
			]));
};
var elm$browser$Browser$element = _Browser_element;
var author$project$Main$main = elm$browser$Browser$element(
	{init: author$project$Main$init, subscriptions: author$project$Main$subscriptions, update: author$project$Main$update, view: author$project$Main$view});
_Platform_export({'Main':{'init':author$project$Main$main(
	elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));