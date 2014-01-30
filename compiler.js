var parse = require('./parser').parse;
var argv = require('optimist').argv;
var fs = require('fs');
var beautify = require('js-beautify').js_beautify;
var L = require('lodash');

L.mixin({ repeat: function(times, value) {
		var result = [];
		for (var i = 0; i < times; i++) result.push(value);
		return result;
}});

var files = argv._;

// for now
fs.readFile(files[0], "utf-8", function(err, text) {
	if (err) {
		console.log(err);
		return;
	}
	console.log(beautify(compile(text)));
});

function strSet() {
	var data = {};
	var api = {
		add: function(str) { data[str] = true; },
		remove: function(str) { data[str] = undefined; },
		has: function(str) { return Boolean(data[str]); },
		toString: function() { return Object.keys(data).join(); }
	};
	for (var i = 0; i < arguments.length; i++) api.add(arguments[i]);
	return api;
}

function log() {
	console.log.apply(console, arguments);
	return arguments[0];
}

var initialState = {
	js: "",
	scope: [strSet(), strSet()]
};

function inScope(name, scope) {
	return scope.some(function(set) { return set.has(name); });
}

function compile(text) {
	console.log(JSON.stringify(parse(text)) + "");
	return ";(function(){" +
		parse(text).reduce(function (state, statement) {
			return compileStatement(statement, state);
		}, initialState).js +
		"}();";
}

function compileStatement(statement, state) {
	if (statement.type === "assignment" && statement.assignee.type !== "mapAccess") {
		if (inScope(statement.assignee, state.scope))
			throw new Error("Identifier already defined: " + statement.assignee);
		state.scope[1].add(statement.assignee);
		state.js += "var " + statement.assignee + "_sa = " + compileExpr(statement.value, state.scope) + ";";
	} else {
		state.js += compileExpr(statement, state.scope) + ";";
	}
	return state;
}

function compileExpr(expr, scope) {
	if (expr.type === "string") {
		return '"' + expr.value + '"';
	} else if (expr.type === "regex") {
		return '(' + expr.value + ')';
	} else if (expr.type === "number") {
		return expr.value;
	} else if (expr.type === "js") {
		return expr.value;
	} else if (expr.type === 'nil') {
		return 'undefined';
	} else if (expr.type === 'boolean') {
		return expr.value;
	} else if (expr.type === "keyword") {
		return "sashimiCore.Keyword('" + expr.value + "')";
	} else if (expr.type === "identifier") {
		if (!inScope(expr.value, scope)) // TODO: namespace if in core
			throw new Error(expr.value + " is not defined." + L.last(scope).toString());
		return expr.value + "_sa";
	} else if (expr.type === "if") {
		return "(" + compileExpr(expr.condition, scope) + " ? " + compileExpr(expr.consequent, scope) + " : " + compileExpr(expr.alternative, scope) + ")"; // TODO: might need more parens
	} else if (expr.type === "fn") {
		return compileFn(expr, scope);
	} else if (expr.type === "let") {
		return compileLet(expr, scope);
	} else if (expr.type === 'map') {
		return 'sashimiCore.Map(' + expr.arguments.map(function(arg) { return compileExpr(arg, scope); }).join(',') + ')';
	} else if (expr.type === 'list') {
		return 'sashimiCore.List(' + expr.arguments.map(function(arg) { return compileExpr(arg, scope); }).join(',') + ')';
	} else if (expr.type === 'set') {
		return 'sashimiCore.Set(' + expr.arguments.map(function(arg) { return compileExpr(arg, scope); }).join(',') + ')';
	} else if (expr.type === 'bag') {
		return 'sashimiCore.Bag(' + expr.arguments.map(function(arg) { return compileExpr(arg, scope); }).join(',') + ')';
	} else if (expr.type === 'mapAccess') {
		return compileExpr(expr.map, scope) + "(" + compileExpr(expr.key, scope) + ")";
	} else if (expr.type === "assignment") {
		return compileAssignment(expr, scope);
	} else if (expr.type === "binaryOperation") {
		return compileBinaryOperation(expr, scope);
	} else if (expr.type === "unaryOperation") {
		return '(' + expr.operator + compileExpr(expr.operand, scope) + ')';
	} else if (expr.type === "functionCall") {
		return compileExpr(expr.function, scope) + '(' + expr.arguments.map(function(arg) { return compileExpr(arg, scope); }).join(',') + ')';
	} else if (expr.type === 'exprList') {
		return '(' + expr.value.map(function(arg) { return compileExpr(arg, scope); }).join(',') + ')';
	} else {
		return "not supported: " + expr.type;
	}
}

function compileFn(expr, scope) {
	// Each body must have a different number of parameters, and only the one with the greatest number of parameters can have a rest parameter
	if (expr.bodies.length === 1) {
		var nonRest = L.last(expr.bodies[0].bindings).rest ? expr.bodies[0].bindings : expr.bodies[0].bindings.slice(0, -1);
		return "function(" +
			nonRest.map(function(binding) { return binding.name + "_sa"; }).join(", ") +
			") {" +
			compileFnBody(expr.bodies[0], scope, false);
	}

	return "function(){" +
		expr.bodies.map(function(body) { return compileFnBody(body, scope, true); }).join('') +
		"else { throw new Error('fn is not defined for the given number of arguments.'); }}";
}

function compileFnBody(body, scope, multipleBodies) {
	console.log(body);
	var newScope = scope.concat(body.bindings.reduce(function(set, binding) {
		if (set.has(binding.name))
			throw new Error("Duplicate parameter: " + binding.name);
		set.add(binding.name);
		return set;
	}, strSet()));

	var rest, nonRest = body.bindings;
	if (L.last(body.bindings).rest) {
		rest = L.last(body.bindings);
		nonRest = body.bindings.slice(0, -1);
	}

	bodyText = "";
	if (multipleBodies) {
		bodyText += "if (arguments.length " + (rest ? ">" : "===") + nonRest.length + ") {";
		bodyText += "var " + nonRest.map(function(binding, i) { return binding.name + "_sa = arguments[" + i + "]"; }).join(", ");
	}

	if (rest)
		bodyText += "var " + rest.name + "_sa = Array.prototype.slice.call(arguments, " + nonRest.length + ");";

	bodyText += nonRest.map(function(binding) {
		return 'default' in binding ?
			"if (" + binding.name + "_sa === undefined) " + binding.name + "_sa = " + compileExpr(binding.default, newScope) + ";" :
			"";
	}).join('') +
		"return " + compileExpr(body.value, newScope) + ";}";

	return bodyText;
}

function compileLet(expr, scope) {
	var newScope = scope.concat(expr.bindings.reduce(function(set, binding) {
		if (set.has(binding.name))
			throw new Error("Duplicate parameter: " + binding.name);
		set.add(binding.name);
		return set;
	}, strSet()));

	return "(function() {" + // TODO: Find a better way to make this into an expression
		expr.bindings.map(function(binding) { // TODO: check that bindings only reference previous bindings
			return "var " + binding.name + "_sa = " + compileExpr(binding.value, scope) + ";";
		}).join('') +
		"return " + compileExpr(expr.value, newScope) + ";}())";
}


function compileBinaryOperation(expr, scope) {
	if (expr.operator === '&') {
		return "(function(){ var op1_sashc = " + compileExpr(expr.operands[0], scope) +
			"; return sashimiCore.toBool(op1_sashc) ? op1_sashc :" + compileExpr(expr.operands[1], scope) + ";})()";
	} else if (expr.operator === '|') {
		return "(function(){ var op1_sashc = " + compileExpr(expr.operands[0], scope) +
			"; return sashimiCore.toBool(op1_sashc) ? " + compileExpr(expr.operands[1], scope) + ": op1_sashc;})()";
	} else {
		return "(" + compileExpr(expr.operands[0], scope) + expr.operator + compileExpr(expr.operands[1], scope) + ")";
	}
}

function compileAssignment(expr, scope) {
	console.log(expr);
	if (expr.assignee.type === 'mapAccess')
		return '((' + compileExpr(expr.assignee.map, scope) + ').set(' + compileExpr(expr.assignee.key, scope) + ',' + compileExpr(expr.value, scope) + ')';
	return "(" + expr.assignee + "_sa = " + compileExpr(expr.value, scope) + ")";
}
