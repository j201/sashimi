var parse = require('./parser').parse;
var argv = require('optimist').argv;
var fs = require('fs');
var beautify = require('js-beautify').js_beautify;
var L = require('lodash');

var sashimiCore = {}; // TODO

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
	scope: [strSet()],
	types: strSet()
};

function inScope(name, scope) {
	return (name in sashimiCore) || scope.some(function(set) { return set.has(name); });
}

function compile(text) {
	console.log(JSON.stringify(parse(text)) + "");
	return ";(function(){" +
		parse(text).reduce(function (module, statement) {
			return compileStatement(statement, module);
		}, initialState).js +
		"}();";
}

function compileStatement(statement, module) {
	if (statement.type === "assignment" && statement.assignee.type !== "mapAccess") {
		if (inScope(statement.assignee, module.scope)) throw Error("Identifier already defined: " + statement.assignee);
		module.scope[0].add(statement.assignee);
		module.js += "var " + statement.assignee + "_sa = " + compileExpr(statement.value, module.scope) + ";";
	} else if (statement.type === "typeDeclaration") {
		console.log(statement);
		if (inScope(statement.typeName, module.scope)) throw Error("Identifier already defined: " + statement.typeName);
		module.scope[0].add(statement.typeName);
		module.js += "var " + statement.typeName + "_sa = " + compileExpr(statement.factory, module.scope) + ";";
	} else if (statement.type === "methodDefinition") {
		if (inScope(statement.methodName, module.scope))
			return compileIdentifier(statement.methodName) + ".addDef('" + statement.typeName + "'," + compileFn(statement.value, module.scope) + ")";
		module.scope[0].add(statement.methodName);
		module.js += 'var ' + statement.methodName + '_sa = sashimiInternal.Fn().addDef("' + statement.typeName + '",' + compileFn(statement.value, module.scope) + ")";
	} else {
		module.js += compileExpr(statement, module.scope) + ";";
	}
	return module;
}

function compileExpr(expr, scope) {
	return expr.type === "string" ?
		'"' + expr.value + '"' :
	expr.type === "regex" ?
		'(' + expr.value + ')' :
	expr.type === "number" ?
		expr.value :
	expr.type === "js" ?
		expr.value :
	expr.type === 'nil' ?
		'undefined' :
	expr.type === 'boolean' ?
		expr.value :
	expr.type === "keyword" ?
		"sashimiInternal.Keyword('" + expr.value + "')" :
	expr.type === "identifier" ?
		compileIdentifier(expr, scope) :
	expr.type === "if" ?
		"(" + compileExpr(expr.condition, scope) + " ? " + compileExpr(expr.consequent, scope) + " : " + compileExpr(expr.alternative, scope) + ")" : // TODO: might need more parens
	expr.type === "fn" ?
		'sashimiInternal.Fn(' + compileFn(expr, scope) + ')' :
	expr.type === "let" ?
		compileLet(expr, scope) :
	expr.type === 'map' ?
		'sashimiInternal.Map(' + expr.arguments.map(function(arg) { return compileExpr(arg, scope); }).join(',') + ')' :
	expr.type === 'list' ?
		'sashimiInternal.List(' + expr.arguments.map(function(arg) { return compileExpr(arg, scope); }).join(',') + ')' :
	expr.type === 'set' ?
		'sashimiInternal.Set(' + expr.arguments.map(function(arg) { return compileExpr(arg, scope); }).join(',') + ')' :
	expr.type === 'bag' ?
		'sashimiInternal.Bag(' + expr.arguments.map(function(arg) { return compileExpr(arg, scope); }).join(',') + ')' :
	expr.type === 'mapAccess' ?
		compileMapAccess(expr, scope) :
	expr.type === "assignment" ?
		compileAssignment(expr, scope) :
	expr.type === "binaryOperation" ?
		compileBinaryOperation(expr, scope) :
	expr.type === "unaryOperation" ?
		compileUnaryOperation(expr, scope) :
	expr.type === "chain" ?
		compileChain(expr, scope) :
	expr.type === "functionCall" ?
		compileExpr(expr.function, scope) + '(' + expr.arguments.map(function(arg) { return compileExpr(arg, scope); }).join(',') + ')' :
	expr.type === 'exprList' ?
		'(' + expr.value.map(function(arg) { return compileExpr(arg, scope); }).join(',') + ')' :
		"not supported: " + expr.type;
}

function compileIdentifier(expr, scope) {
	if (!scope.some(function(set) { return set.has(expr.value); })) {
		if (expr.value in sashimiCore)
			return "sashimiCore." + expr.value;
		console.log(expr);
		throw Error(expr.value + " is not defined. " + L.last(scope).toString());
	}
	return expr.value + "_sa";
}

function compileFn(expr, scope) {
	if (expr.bodies.length === 1) {
		var nonRest = L.last(expr.bodies[0].bindings).rest ? expr.bodies[0].bindings : expr.bodies[0].bindings.slice(0, -1);
		return "function(" +
			nonRest.map(function(binding) { return binding.name + "_sa"; }).join(", ") +
			") {" +
			compileFnBody(expr.bodies[0], scope, false);
	}

	// Each body must have a different number of parameters, and only the one with the greatest number of parameters can have a rest parameter
	var lengths = expr.bodies.map(function(body) { return body.bindings.length; });
	var maxLength = L.max(lengths);
	if (lengths.length !== L.uniq(lengths).length)
		throw Error("Multiple function bodies with the same number of parameters");
	if (expr.bodies.some(function(body) { return body.bindings.length !== maxLength && L.last(body.bindings).rest; }))
		throw Error("Rest parameter must be on function body with the most parameters");

	return "function(){" +
		expr.bodies.map(function(body) { return compileFnBody(body, scope, true); }).join('') +
		"else { throw Error('fn is not defined for the given number of arguments.'); }}";
}

function compileFnBody(body, scope, multipleBodies) {
	var newScope = scope.concat(body.bindings.reduce(function(set, binding) {
		if (set.has(binding.name))
			throw Error("Duplicate parameter: " + binding.name);
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
			throw Error("Duplicate parameter: " + binding.name);
		set.add(binding.name);
		return set;
	}, strSet()));

	return "(function() {" + // TODO: Find a better way to make this into an expression
		expr.bindings.map(function(binding) { // TODO: check that bindings only reference previous bindings
			return "var " + binding.name + "_sa = " + compileExpr(binding.value, scope) + ";";
		}).join('') +
		"return " + compileExpr(expr.value, newScope) + ";}())";
}

function compileMapAccess(expr, scope) {
	if (expr.map.type === 'identifier' && expr.map.value === 'js')
		return "sashimiInternal.fromJS(" + expr.key.value + ")";
	return compileExpr(expr.map, scope) + "(" + compileExpr(expr.key, scope) + ")";
}

function compileAssignment(expr, scope) {
	if (expr.assignee.type === 'mapAccess')
		return '((' + compileExpr(expr.assignee.map, scope) + ').set(' + compileExpr(expr.assignee.key, scope) + ',' + compileExpr(expr.value, scope) + ')';
	return "(" + expr.assignee + "_sa = " + compileExpr(expr.value, scope) + ")";
}

function compileBinaryOperation(expr, scope) {
	if (expr.operator === '&') {
		return "(function(){ var op1_sashc = " + compileExpr(expr.operands[0], scope) +
			"; return sashimiInternal.Bool(op1_sashc) ? op1_sashc :" + compileExpr(expr.operands[1], scope) + ";})()";
	} else if (expr.operator === '|') {
		return "(function(){ var op1_sashc = " + compileExpr(expr.operands[0], scope) +
			"; return sashimiInternal.Bool(op1_sashc) ? " + compileExpr(expr.operands[1], scope) + ": op1_sashc;})()";
	} else {
		return "(" + compileExpr(expr.operands[0], scope) + (expr.operator === '==' ? '===' : expr.operator) + compileExpr(expr.operands[1], scope) + ")";
	}
}

function compileUnaryOperation(expr, scope) {
	if (expr.operator === '!')
		return "(!sashimiInternal.Bool(" + compileExpr(expr.operand, scope) + "))";
	return '(' + expr.operator + compileExpr(expr.operand, scope) + ')';
}	

function compileChain(expr, scope) {
	if (expr.function.type !== "functionCall")
		throw Error("A chain expression must include a function call");
	expr.function.arguments.unshift(expr.caller);
	return compileExpr(expr.function, scope);
}
