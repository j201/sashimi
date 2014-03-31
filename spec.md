#Sashimi Spec

##Values

Sashimi values have a primary value, which they evaluate to as an expression on their own, and internal properties. An identifier preceded by `..` represents an internal property attached to a value. It should not be directly accessible by Sashimi code. For example, `foo..bar` represents the internal `bar` property on `foo`.

##Value Types

Maps, bags, sets, and lists are mutable. All other values are immutable.

###Nil

Equivalent to the JS value `undefined`.

###String

Equivalent to a JS string.

###Number

Equivalent to a JS number.

###Regex

Parsed as a JS regex, although the supported functions are different.

###Keyword

A value that is only equal to the same keyword.

###Boolean

Equivalent to a JS boolean.

###Map

Associates arbitrary keys to arbitrary values.

###Bag

An unordered group of values.

###Set

A bag in which a value can only appear once.

###List

An ordered group of values.

###Function

Equivalent to a JS function, except can have method definitions (alternate functions that are dispatched instead based on the type of the first argument), and cannot have properties.

##Literals

Literals have precedence in the following order

###String

Equivalent to JS string literals, except that the apostrophe character cannot be used as the delimiter, only quotation marks.

###Regex

Equivalent to JS RegExp literals.

###Number

Equivalent to JS number literals.

###Nil

The symbol `nil`.

###Boolean

The symbol `true` or `false`

###Keyword

The `.` character followed by a sequence of letters, numbers, or underscores.

###Set

A series of expressions separated by commas and surrounded by the tokens `#{` and `}`.

###Bag

A series of expressions separated by commas and surrounded by the tokens `#[` and `]`.

###Map

A series of expressions separated by commas and surrounded by the tokens `{` and `}`.

###List

A series of expressions separated by commas and surrounded by the tokens `[` and `]`.

###Function

In BNF,

Function literal ::= `fn` fnBody | `fn` `[` fnBodies `]`  
fnBodies ::= fnBody | fnBodies `,` fnBody  
fnBody ::= fnBindings `:` expression | `:` expression  
fnBindings ::= nonRestParams | nonRestParams restParams | restParams  
nonRestParams ::= nonRestParam | nonRestParams restParam  
nonRestParam ::= identifier | identifier `,` | identifier `=` expression | identifier `=` expression `,`  
restParam ::= `&` identifier

##Parsing

Whitspace separates tokens but is otherwise ignored. If an error is thrown in the evaluation of a statement or expression, then the compilation or interpretation of the code stops immediately and a JavaScript error is thrown.

###Operators

The following operators exist:

`+` `-` `/` `*` `**` `&` `|` `>` `<` `>=` `<=` `==` `!=` `!` `=` `^` `#`

###Reserved Words

In addition to the literals and operators, the following tokens are reserved:

`fn` `if` `when` `case` `let` `module` `import` `export` `type`

###Identifiers

Any sequence of letters, numbers, and underscores that is not a literal or reserved word is an identifier.

##The `..type` Stack

All values have an internal `..type` property that is a stack of strings. It supports the following abstract operations:

- `push(value..type, typeName)` - Adds the `typeName` string to the top of the `..type` stack.
- `first(value..type)` - Returns the top type string on the `..type` stack.
- `next(value..type)` - Returns the `..type` stack without the top string.
- `has(value..type, str)` - Returns `true` if the string in the second parameter is one of the elements of the `..type` stack, else false.
- `empty(value..type` - Returns `true` if there are no strings in the `..type` stack, else `false`.

###Default `..type` Values

Unless otherwise modified, the following primitive values have the following single value on their type stacks:

Value Type | Type Stack Value
--- | ---
Nil | "Nil"
String | "String"
Number | "Number"
Regex | "Regex"
Keyword | "Keyword"
Boolean | "Boolean"
Map | "Map"
Bag | "Bag"
Set | "Set"
List | "List"
Function | "Function"

All of the above type stack values are always considered to exist in any namespace and cannot be redefined with a type declaration.

##Scope

Files have their own scope, modules have their own scope, and all expressions have their own scope, although this only should need to be created by the compiler for expressions that bind variables, such as function literals or let expressions. Scope is lexical.

##Expressions

An expression is one of the following, in order of precedence:

###Literal

Any of the literals described above.

####Function literal

TODO: specify the behaviour of a function literal

###Identifier

If the identifier exists in the current scope or any of the enclosing scopes, evaluates to the value bound to that identifier, with inner scopes taking precedence. Otherwise, an error is thrown. Note that the identifier binding does not have to occur before the identifier is first referenced, as long as it does occur somewhere in an enclosing scope (definitions are "hoisted"). However, if the identifier is evaluated before it has been bound, an error is thrown.

###Import Expression

`import` `string literal`

If a module with the same name as the value of the string literal has been defined, then evaluates to the export value of that module. Otherwise, an error is thrown.

###If Expression

`if` `condition expression` `:` `consequent expression` `,` `alternate expression`

Evaluates the condition expression. If the result is not equal to `nil` or `false`, evaluates to the value of the consequent expression, otherwise evaluates to the value of the alternate expression. Only one of the consequent and alternate expressions may be evaluated.

###Let Expression

Let Expression  ::= `let` letBindings `:` expression
letBindings ::= letBinding | letBindings letBinding
letBinding ::= identifier  `=` expression | identifier `=` expression `,`

A let expression creates a new scope with the identifiers listed in the letBindings bound to the result of evaluating each corresponding expression in the letBinding and evaluates an expression in that scope. Bindings are evaluated in order and may reference the result of previous bindings. So, `let a = 1, b = a: b` is equivalent to `let a = 1: let b = a: b`.

###Map Keyword Access

`expression` `keyword`

Equivalent to the function call `expression(keyword)`. If the result of evaluating `expression` does not have `Map` in its `..type` stack, an error is thrown.

###Binary Operation

`expression` `operator` `expression`

Where operator is one of the following:

- `-`, `/`, `*`, `>`, `<`, `>=`, or `<=`: If either operand evaluates to a non-number value, an error is thrown. Otherwise, evaluates to the Sashimi number equivalent of using the same operator in JS with the JS equivalents of the results of evaluating the operands.
- `+`: If both operands evaluate to strings, evaluates to the concatenation of the strings. If both operands evaluate to numbers, evaluates to the Sashimi number equivalent of using the JS `+` operator on the JS equivalents of the results of evaluating the operands.
- `**`: If either operand evaluates to a non-number value, an error is thrown. Otherwise, evaluates to the Sashimi number equivalent of calling the JS function `Math.pow` on the JS equivalents of the results of evaluating the operands.
- `&`: If the first operand evaluates to `false` or `nil`, evaluates to the first operand, otherwise evaluates to the second operand. The second operand is only evaluated if the first operand does not evaluate to `false` or `nil`.
- `|`: If the first operand does not evaluate to `false` or `nil`, evaluates to the first operand, otherwise evaluates to the second operand. The second operand is only evaluated if the first operand evaluates to to `false` or `nil`.
- `==`:
	- Let `o1` be the result of evaluating the first operand and `o2` be the result of evaluating the second.
	- If `o1` and `o2` have a different value type, evaluates to `false`.
	- If `first(o1..type)` and `first(o2..type)` are not the same string, evaluates to `false`.
	- If `o1` and `o2` are mutable and they refer to the same value (reference equality), evaluates to `true`.
	- If `o1` and `o2` are immutable and their values are equivalent (value equality), evaluates to `true`.
	- Evaluates to `false`.
- `!=`: Gives the opposite boolean value of the result of evaluating the operands with `==`.

###Unary Operation

`operator` `expression`

####Operator Precendence and Associativity

##Statements

A statement is one of the following:

###Definition

`identifier` `=` `expression` `;`

If the given identifier is already defined in the scope, an error is thrown. Otherwise, the expression is evaluated and its value is assigned to the given identifier.

###Method Definition

`identifier` `#` `identifier` `=` `expression` `;`

- If the first identifier is not the name of a type in the scope, an error is thrown.
- If the second identifier is bound to a value other than a function, an error is thrown.
- If the second identifier is not bound, then it is bound to a new instance of the function `fn: nil`.
- If the first identifier is not bound to a type function, an error is thrown.
- Let `f` be the function identified by the second identifier and `t` be the `..returnType` of the type function identified by the first identifier.
- If `f` already has a method definition for `t`, an error is thrown.
- Let `v` be the result of evaluating the expression.
- If `v` is not a function, an error is thrown.
- `v` is assigned to `f` as a method definition for type `t`.

###Module Declaration

`'module'` `string literal` `;`

If the module named by the string already exists, an error is thrown. Otherwise, sets the current module to the string until the end of the file or the next module declaration.

###Module Export Statement

`'export'` `=` `expression` `;`

If there have been any module export statements or exported definitions in the current module, an error is thrown. Otherwise, sets the export value of the module to the expression.

###Exported Definition

`'export'` `identifier` `=` `expression` `;`

- If there has been a module export statement in the current module, an error is thrown.
- If there have not been any export definitions in the current module, the module's export value is set to an empty map.
- Let `e` be the current module export value.
- Let `k` be the keyword that would be produced by the keyword literal created by prepending a period to the identifier.
- If `k` exists as a key in `e`, an exception is thrown.
- `k` is added as a ky to `e` with the value of the expression.

###Type Definition

`'type'` `identifier` `=` `function expression` `;`

- Let `t` be the string text of the identifier.
- If 
CONTINUE HERE

Evaluates as if a definition statement without the `'type'` token, but modifies the function expression so that the values returned by it will have the identifier pushed onto their `..type` stack and so that the function expression has its `..isTypeFn` set to `true` and its `..

###Expression Statement

`expression` `;`

If a statement is a valid expression and cannot be parsed as another type of statement, the expression is evaluated.

##Modules

Sashimi code can either be executed on its own or within a module. A module is a section of code started with a module statement and terminated by another module statement or the end of the code file. When a module is started, all bindings are cleared except those in the core library. The export value of a module, as specified by a module export statements or by exported definitions, is saved for the duration of the execution of the program importing the module. A module can be imported by an import expression. A module must not be executed more than once during the execution of a program and it must not be executed unless imported or specified as the program entry point. It is recommended that modules be split up into separate files and then compiled together. How these files are specified is implementation-dependent. If modules have circular dependencies, an error is thrown.
