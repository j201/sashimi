#Sashimi Spec

##Types

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
- Let `f` be the function identified by the second identifier and `t` be the return type of the type function identified by the first identifier.
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

Evaluates as if a definition statement without the `'type'` token, but modifies the function expression so that the values returned by it will have the identifier pushed onto their type stack and so that the function expression is annotated as a type function with the identifier as its return type.

###Expression Statement

`expression` `;`

If a statement is a valid expression and cannot be parsed as another type of statement, the expression is evaluated.

##Modules

Sashimi code can either be executed on its own or within a module. A module is a section of code started with a module statement and terminated by another module statement or the end of the code file. When a module is started, all bindings are cleared except those in the core library. The export value of a module, as specified by a module export statements or by exported definitions, is saved for the duration of the execution of the program importing the module. A module can be imported by an import expression. A module must not be executed more than once during the execution of a program and it must not be executed unless imported or specified as the program entry point. It is recommended that modules be split up into separate files and then compiled together. How these files are specified is implementation-dependent. If modules have circular dependencies, an error is thrown.