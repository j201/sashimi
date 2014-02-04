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

	internal.Fn = function(f, thisVal) {
		var types = {};
		var defaultFn = f || function(firstArg) { throw Error('Fn not defined for ' + firstArg); };
		var result = function(firstArg) {
			if (firstArg && firstArg.sashimiVal)
				return (types(firstArg.type) || defaultFn).apply(thisVal, arguments);
			return defaultFn.apply(thisVal, arguments);
		};
		result.type = "Fn";
		result.addDef = function(type, f) {
			if (types[type])
				throw Error('Fn definition for type ' + type + ' already exists');
			types[type] = f;
			return result;
		};
		result.hasDefault = Boolean(f);
		result.setDefault = function(f, newThisVal) {
			if (result.hasDefault) throw Error('Default fn value already defined');
			thisVal = newThisVal;
			defaultFn = f;
			result.hasDefault = true;
			return result;
		};
		result.sashimiVal = true;
		return result;
	};

	internal.JSObject(obj) { // Wraps a js object so it can be used in sashimi
		var result = function(key) { // Must be a keyword, string, or number
			return fromJS(obj[key.value], obj);
		};
		result.set = function(key, value) {
			return obj[key.value] = toJS(value);
		};
	}

	function toJS(val) {
		return val.value || val;
	};

	function fromJS(val, context) {
		if (val == null) return undefined;
		switch(typeof val) {
			case "number":
			case "string":
			case "boolean":
				return val;
			case "function":
				return internal.Fn(val, context);
			default:
				return internal.JSObject(val);
		}
	};

	sashimiInternal = internal;
})(this);