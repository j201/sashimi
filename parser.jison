%lex
%%

\"(?:[^\"\\]|\\\"|\\\\)*\" return 'string'
\/(?:[^\/\\]|\\\/|\\\\)*\/[gi]+ return 'regex'
\`(?:[^\`]|\`\`)*\`	return 'js'
\/\/[^\n]+\n	/* ignore comment */
\/\*[^]+?\*\/	/* ignore comment */
\s+				/* ignore whitespace */
\-?\d+(?:\.\d*(?:[eE]\-?\d+)?|[eE]\-?\d+)? return 'number'
/* \:\w+			return 'keyword' */
'module'		return 'module'
'import'		return 'import'
'export'		return 'export'
'type'			return 'type'
'fn'			return 'fn'
'let'			return 'let'
'if'			return 'if'
'true'			return 'true'
'false'			return 'false'
'nil'			return 'nil'
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
'^'				return '^'
'#'				return '#'
'~'				return '~'
<<EOF>>	return 'EOF'

/lex

%left identifier /* THIS MAKES IT WORK AND WAT */
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
%right UMINUS
%right '!'
%left '^'
%left '(' ')'
%left '.'

%%

file :
	statements EOF { return $1; }
;

statements :
	statement { $$ = [$1] }
	| statements statement { $1.push($2); $$ = $1; }
;

statement: moduleStatement | exportStatement | typeDeclaration | methodDefinition | expr ';';

moduleStatement: 'module' string ';' { $$ = { type: 'module', name: $2 } };

exportStatement:
	'export' '=' expr ';' { $$ = { type: 'export', value: $3 } }
	| 'export' identifier '=' expr ';' { $$ = { type: 'exportedDefinition', name: $2, value: $4 } }
;

typeDeclaration: 'type' identifier '=' fnExpr ';' { $$ = { type: 'typeDeclaration', typeName: $2, factory: $4 } };

methodDefinition: identifier '^' identifier '=' fnExpr ';' { $$ = { type: 'methodDefinition', typeName: $1, methodName: $3, value: $5 } };

expr:
	string { $$ = { type: 'string', value: yytext.slice(1, -1) } }
	| regex { $$ = { type: 'regex', value: yytext } }
	| js { $$ = { type: 'js', value: yytext.slice(1, -1) } }
	| number { $$ = { type: 'number', value: Number(yytext) } }
	| nil { $$ = { type: 'nil' } }
	| boolean
	| keyword
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
	| expr '^' expr { $$ = { type: "chain", caller: $1, function: $3 } } /* Note: MUST be of the form expr.expr(...) */
	| expr '(' delimitedExprs ')' { $$ = { type: 'functionCall', function: $1, arguments: $3 } }
	| expr '(' ')' { $$ = { type: 'functionCall', function: $1, arguments: [] } }
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
	| delimitedExprs ',' expr { $1.push($3); $$ = $1; }
;

boolean:
	true { $$ = { type: 'boolean', value: 'true' }; }
	| false { $$ = { type: 'boolean', value: 'false' }; }
;

keyword: '.' identifier { $$ = { type: 'keyword', value: $2 } };

importExpr: 'import' string { $$ = { type: 'import', name: $2 } };

ifExpr: 'if' expr ':' expr ',' expr { $$ = { type: 'if', condition: $2, consequent: $4, alternative: $6 } };

letExpr: 'let' letBindings ':' expr { $$ = { type: 'let', bindings: $2, value: $4 } };

letBindings:
	letBinding { $$ = [$1]; }
	| letBindings letBinding { $1.push($2); $$ = $1; }
;

letBinding: identifier  '=' exprOptionalComma { $$ = { name: $1, value: $2 } };

fnExpr:
	'fn' fnBody { $$ = { type: 'fn', bodies: [$2] } }
	| 'fn' '[' fnBodies ']' { $$ = { type: 'fn', bodies: $3 } }
;

fnBodies:
	fnBody { $$ = [$1] }
	| fnBodies ',' fnBody  { $1.push($3); $$ = $1; }
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

mapAccess: expr keyword { $$ = { type: 'mapAccess', map: $1, key: $2 } };

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
