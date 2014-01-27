var sashimiCore = {};

sashimiCore.Map = function() {
};

sashimiCore.Vector = function() {
	var vec = function(i) {
		return vec.data[i];
	};
	vec.data = [];
	return vec;
};

sashimiCore.Set = function() {
};

sashimiCore.Bag = function() {
};

sashimiCore.toBool = function(val) {
	return val === false || val === null;
};
