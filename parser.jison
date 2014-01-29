%lex
%%

\"(?:[^\"\\]|\\\"|\\\\)*\" return 'string'
\/(?:[^\/\\]|\\\/|\\\\)*\/[gi]+ return 'regex'
\`(?:[^\`]|\`\`)*\`	return 'js'
\/\/[^\n]+\n	/* ignore comment */
\/\*[^]+?\*\/	/* ignore comment */
\s+				/* ignore */
\-?\d+(?:\.\d*(?:[eE]\-?\d+)?|[eE]\-?\d+)? return 'number'
\:\w+			return 'keyword'
'module'		return 'module'
'import'		return 'import'
'export'		return 'export'
'fn'			return 'fn'
'let'			return 'let'
'if'			return 'if'
'true'			return 'true'
'false'			return 'false'
'nil'			return 'nil'
[\w\.]*\.[\w\.]* return 'identifierWithPeriods'
\w+				return 'identifier'
'\'('			return '\'('
'('				return '('
')'				return ')'
'#{'			return '#{'
'{'				return '{'
'}'				return '}'
'#['			return '#['
'['				return '['
']'				return ']'
'&'				return '&'
'+'				return '+'
'-'				return '-'
'/'				return '/'
'*'				return '*'
'^'				return '^'
'&'				return '&'
'|'				return '|'
'!'				return '!'
';'				return ';'
','				return ','
'.'				return '.'
'<='			return '<='
'>='			return '>='
'<'				return '<'
'>'				return '>'
'=='			return '=='
'!='			return '!='
'='				return '='
':'				return ':'
'#'				return '#'
<<EOF>>	return 'EOF'

/lex

%left ';' ','
%right 'fn' ':'
%right 'let'
%right 'if'
%right '='
%left '|'
%left '&'
%left '<=' '>=' '<' '>' '==' '!='
%left '+' '-'
%left '*' '/'
%left '^'
%right UMINUS
%right '!'
%left '(' ')'
%left '.'
%left keyword

%%

file :
	statements EOF { return $1; }
;

statements :
	statement { $$ = [$1] }
	| statements statement { $1.push($2); $$ = $1; }
;

statement: moduleStatement | exportStatement | expr ';';

moduleStatement: 'module' moduleIdentifier ';' { $$ = { type: 'module', name: $2 } };

moduleIdentifier: identifier | identifierWithPeriods;

exportStatement: 'export' expr ';' { $$ = { type: 'export', value: $2 } };

expr:
	string { $$ = { type: 'string', value: yytext.slice(1, -1) } }
	| regex { $$ = { type: 'regex', value: yytext } }
	| js { $$ = { type: 'js', value: yytext.slice(1, -1) } }
	| number { $$ = { type: 'number', value: Number(yytext) } }
	| nil { $$ = { type: 'nil' } }
	| boolean
	| keywordLiteral
	| identifier { $$ = { type: 'identifier', value: yytext } }
	| importExpr
	| ifExpr
	| fnExpr
	| letExpr
	| map | list | set | bag
	| mapAccess
	| binaryOperation
	| unaryOperation
	| assignment
	| expr '.' expr { $$ = { type: "dotExpression", caller: $1, function: $3 } } /* Note: MUST be of the form expr.expr(...) */
	| expr '(' delimitedExprs ')' { $$ = { type: 'functionCall', function: $1, arguments: $3 } }
	| '(' delimitedExprs ')' { $$ = { type: 'exprList', value: $2 } }
;

exprOptionalComma: expr | expr ',';

exprsOptionalComma:
	exprOptionalComma { $$ = [$1] }
	| exprOptionalComma exprOptionalComma { $1.push($2); $$ = $1; }
;

separatedExprs:
	expr ',' { $$ = [$1] }
	| separatedExprs expr ',' { $1.push($2); $$ = $1; }
;

delimitedExprs: /* Separated exprs where the last one doesn't have a comma */
	expr { $$ = [$1] }
	| delimitedExprs ',' expr { $1.push($2); $$ = $1; }
;

boolean:
	true { $$ = { type: 'boolean', value: 'true' }; }
	| false { $$ = { type: 'boolean', value: 'false' }; }
;

keywordLiteral: keyword { $$ = { type: 'keyword', value: yytext.slice(1) } };

importExpr: 'import' moduleIdentifier { $$ = { type: 'import', name: $2 } };

ifExpr: 'if' expr ':' expr ',' expr { $$ = { type: 'if', condition: $2, consequent: $4, alternative: $6 } };

letExpr: 'let' letBindings ':' expr { $$ = { type: 'let', bindings: $2, value: $4 } };

letBindings:
	letBinding { $$ = [$1]; }
	| letBindings letBinding { $1.push($2); $$ = $1; }
;

letBinding: identifier  '=' exprOptionalComma { $$ = { name: $1, value: $2 } };

fnExpr: 'fn' fnBodies { $$ = { type: 'fn', bodies: $2 } };

fnBodies:
	fnBody { $$ = [$1] }
	| fnBodies ',' fnBody  { $1.push($2); $$ = $1; }
;

fnBody: fnBindings ':' expr { $$ = { bindings: $1, value: $3 } };

fnBindings:
	nonRestParams { $$ = $1 }
	| nonRestParams restParam { $1.push($2); $$ = $1; }
;

nonRestParams:
	nonRestParam { $$ = [$1] }
	| nonRestParams nonRestParam { $1.push($2); $$ = $1; }
;

nonRestParam:
	identifier { $$ = { name: $1 } }
	| identifier ',' { $$ = { name: $1 } }
	| identifier '=' expr { $$ = { name: $1, default: $3 } }
	| identifier '=' expr ',' { $$ = { name: $1, default: $3 } }
;

restParam: '&' identifier { $$ = { name: $2, rest: true } };

map: '{' delimitedExprs '}' { $$ = { type: 'map', arguments: $2 } };

list: '[' delimitedExprs ']' { $$ = { type: 'list', arguments: $2 } };

set: '#{' delimitedExprs '}' { $$ = { type: 'set', arguments: $2 } };

bag: '#[' delimitedExprs ']' { $$ = { type: 'bag', arguments: $2 } };

mapAccess: expr keywordLiteral { $$ = { type: 'mapAccess', map: $1, key: $2 } };

assignment:
	identifier '=' expr { $$ = { type: 'assignment', assignee: $1, value: $3 } }
	| mapAccess '=' expr { $$ = { type: 'assignment', assignee: $1, value: $3 } }
;

binaryOperation:
	expr '+' expr { $$ = { type: 'binaryOperation', operator: $2, operands: [$1, $3] } }
	| expr '-' expr { $$ = { type: 'binaryOperation', operator: $2, operands: [$1, $3] } }
	| expr '/' expr { $$ = { type: 'binaryOperation', operator: $2, operands: [$1, $3] } }
	| expr '*' expr { $$ = { type: 'binaryOperation', operator: $2, operands: [$1, $3] } }
	| expr '&' expr { $$ = { type: 'binaryOperation', operator: $2, operands: [$1, $3] } }
	| expr '|' expr { $$ = { type: 'binaryOperation', operator: $2, operands: [$1, $3] } }
	| expr '<' expr { $$ = { type: 'binaryOperation', operator: $2, operands: [$1, $3] } }
	| expr '>' expr { $$ = { type: 'binaryOperation', operator: $2, operands: [$1, $3] } }
	| expr '<=' expr { $$ = { type: 'binaryOperation', operator: $2, operands: [$1, $3] } }
	| expr '>=' expr { $$ = { type: 'binaryOperation', operator: $2, operands: [$1, $3] } }
	| expr '==' expr { $$ = { type: 'binaryOperation', operator: $2, operands: [$1, $3] } }
	| expr '!=' expr { $$ = { type: 'binaryOperation', operator: $2, operands: [$1, $3] } }
;

unaryOperation:
	'-' expr { $$ = { type: 'unaryOperation', operator: $1, operand: $2 } }
	| '!' expr { $$ = { type: 'unaryOperation', operator: $1, operand: $2 } }
;

