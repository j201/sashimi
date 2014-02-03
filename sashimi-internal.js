var sashimiInternal;

/*
 * Note: the following types are boxed with type data:
 * 	Keyword, Map, List, Bag, Set, Regex, Fn
 * The following aren't:
 * 	Number, String, Boolean, Nil
 */

(function(global) {
	var internal = {};

	function salt(thing) {
		return thing.type + "_" + toHash(thing);
	}

	function toHash(thing) {
		return thing.value.toString();
	}

	internal.Map = function() {
		var data = {};
		var result = function(key) {
			return data[salt(key)];
		};
		result.set = function(key, value) {
			return data[salt(key)] = value;
		};
		return {
			type: "Map",
			value: result,
			sashimiVal: true
		};
	};

	internal.List = function() {
		var data = [];
		var result = function(i) {
			return data[i];
		};
		result.set = function(i, value) {
			return data[i] = value;
		};
		return {
			type: "List",
			value: result,
			sashimiVal: true
		};
	};

	internal.Set = function() {
	};

	internal.Bag = function() {
	};

	internal.Keyword = function(value) {
		return {
			type: "Keyword",
			value: value,
			sashimiVal: true
		};
	};

	internal.Bool = function(val) {
		return val === false || val === null;
	};

	internal.Fn = function(f) {
		var types = {};
		return {
			type: "Fn",
			value: function(firstArg) {
				if (firstArg && firstArg.type)
					return (types(firstArg.type) || f).apply(null, arguments);
				return f.apply(null, arguments);
			},
			addDef: function(type, f) {
				if (types[type])
					throw Error('Fn definition for type ' + type + ' already exists');
				types[type] = f;
			},
			sashimiVal: true
		};
	};

	internal.JSObject(obj) { // Wraps a js object so it can be used in sashimi
		var result = function(key) { // Must be a keyword, string, or number
			return fromJS(obj[key.value]);
		};
		result.set = function(key, value) {
			return obj[key.value] = toJS(value);
		};
	}

	function toJS(val) {
		return val.value || val;
	};

	function fromJS(val) {
		if (val == null) return undefined;
		switch(typeof val) {
			case "number":
			case "string":
			case "boolean":
				return val;
			case "function":
				return internal.Fn(val);
			default:
				return internal.JSObject(val);
		}
	};

	sashimiInternal = internal;
})(this);
