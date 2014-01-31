var sashimiCore;

(function(global) {
	var core = {};

	function salt(thing) {
		return thing.type === "Keyword" ? thing.value : thing.type + "_" + toHash(thing);
	}

	function toHash(thing) {
		return thing.toString();
	}

	core.js = function() {
		var result = function(key) {
			return global[salt(key)];
		};
		result.set = function(key) {
			return global[salt(key)] = value;
		};
	};

	core.Map = function() {
		var data = {};
		var result = function(key) {
			return data[salt(key)];
		};
		result.set = function(key, value) {
			return data[salt(key)] = value;
		};
		return result;
	};

	core.List = function() {
		var vec = function(i) {
			return vec.data[i];
		};
		vec.data = [];
		return vec;
	};

	core.Set = function() {
	};

	core.Bag = function() {
	};

	core.Keyword = function(value) {
		return {
			type: "Keyword",
			value: value
		};
	};

	core.toBool = function(val) {
		return val === false || val === null;
	};
})(this);
