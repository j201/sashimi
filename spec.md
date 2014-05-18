#Sashimi Spec

##Values

Sashimi values have a primary value, which they evaluate to as an expression on their own, and internal properties. An identifier preceded by `..` represents an internal property attached to a value. It should not be directly accessible by Sashimi code. For example, `foo..bar` represents the internal `bar` property on `foo`.

##Value Types

All values are immutable.

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

Equivalent to a JS function, except can have method definitions (alternate functions that are dispatched instead based on the tag of the first argument), and cannot have properties.

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

##The `..tag` Stack

All values have an internal `..tag` property that is a stack of keywords. It supports the following abstract operations:
TODO: remove unused operations

- `withTag(value, tagName)` - Evaluates to a value that is the same as `value`, but with `tagName` pushed to its `..tag` stack.
- `push(value..tag, tagName)` - Adds the `tagName` keyword to the top of the `..tag` stack.
- `first(value..tag)` - Returns the top tag keyword on the `..tag` stack.
- `next(value..tag)` - Returns the `..tag` stack without the top keyword.
- `has(value..tag, str)` - Returns `true` if the keyword in the second parameter is one of the elements of the `..tag` stack, else false.
- `empty(value..tag)` - Returns `true` if there are no keywords in the `..tag` stack, else `false`.

###Default `..tag` Values

Unless otherwise modified, the following primitive values have the following single value on their tag stacks:

Value Type | tag stack Value
--- | ---
Nil | .Nil
String | .String
Number | .Number
Regex | .Regex
Keyword | .Keyword
Boolean | .Boolean
Map | .Map
Bag | .Bag
Set | .Set
List | .List
Function | .Function

All of the above tag stack values are always considered to exist in any namespace and cannot be redefined with a tag declaration.

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

Equivalent to the function call `expression(keyword)`. If the result of evaluating `expression` does not have `Map` in its `..tag` stack, an error is thrown.

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
	- If `first(o1..tag)` and `first(o2..tag)` are not the same string, evaluates to `false`.
	- If the values of `o1` and `o2` are equivalent (value equality), evaluates to `true`.
	- Evaluates to `false`.
- `!=`: Evaluates to the opposite boolean value of the result of evaluating the operands with `==`.
- `~`: If the result of evaluating the second operand is not a keyword, an error is thrown, otherwise evaluates to `withTag(<first operand>, <second operand>)`

###Unary Operation

`operator` `expression`

Where operator is one of the following:

- `-`: If the operand evaluates to a non-number value, an error is thrown. Otherwise, evaluates to the value produced by evaluating `0 - o`, where `o` is the result of evaluating the operand.
- `!`: If the operand evaluates to `false` or `nil`, evaluates to `true`. Otherwise, evaluates to false.

###Expression Group

`(` `comma separated expressions` `)`  
where `comma separated expressions` ::= `expression` | `expression` `,` `comma separated expressions`

Evaluates the expressions in order, evaluating to the value produced when the last expression is evaluated.

###Function Call

`expression` `(` `comma separated expressions` `)`  
where `comma separated expressions` ::= `expression` | `expression` `,` `comma separated expressions`

If the value produced by evaluating the first expression isn't a function, an error is thrown. Otherwise, that value is called with the values produced by evaluating the expressions inside the parens as arguments.

####Precendence and Associativity

Precendence | Expression/Operator | Associativity
-|-|-
1 | Expression Group `(`, `)` | N/A
2 | Map Keyword Access | Left
3 | Function Call | Left
4 | `!`, unary `-` | Right
5 | `**` | Right
6 | `*`, `/` | Left
7 | `+`, `-` | Left
8 | `>`, `<`, `>=`, `<=` | Left
9 | `==`, `!=` | Left
10 | `&` | Left
11 | `|` | Left
12 | `=` | Left
13 | If Expression | Right
14 | Let Expression | Right
15 | Function Expression | Right
16 | `,` | Left
17 | `;` | Left

##Statements

A statement is one of the following:

###Definition

`identifier` `=` `expression` `;`

If the given identifier is already defined in the scope, an error is thrown. Otherwise, the expression is evaluated and its value is assigned to the given identifier.

###Method Definition

`identifier` `#` `identifier` `=` `expression` `;`

- If the first identifier is not the name of a tag in the scope, an error is thrown.
- If the second identifier is bound to a value other than a function, an error is thrown.
- If the second identifier is not bound, then it is bound to a new instance of the function `fn: nil`.
- If the first identifier is not bound to a tag function, an error is thrown.
- Let `f` be the function identified by the second identifier and `t` be the `..returnTag` of the tag function identified by the first identifier.
- If `f` already has a method definition for `t`, an error is thrown.
- Let `v` be the result of evaluating the expression.
- If `v` is not a function, an error is thrown.
- `v` is assigned to `f` as a method definition for tag `t`.

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

###Expression Statement

`expression` `;`

If a statement is a valid expression and cannot be parsed as another type of statement, the expression is evaluated.

##Modules

Sashimi code can either be executed on its own or within a module. A module is a section of code started with a module statement and terminated by another module statement or the end of the code file. When a module is started, all bindings are cleared except those in the core library. The export value of a module, as specified by a module export statements or by exported definitions, is saved for the duration of the execution of the program importing the module. A module can be imported by an import expression. A module must not be executed more than once during the execution of a program and it must not be executed unless imported or specified as the program entry point. It is recommended that modules be split up into separate files and then compiled together. How these files are specified is implementation-dependent. If modules have circular dependencies, an error is thrown.

##Core Functions

The following functions are defined by default. A collection is a value that is a map, bag, set, or list (regardless of its tag). The following syntax is used:

`functionName(arg1: ValueType, arg2: ValueTypeOpt1 | ValueTypeOpt2, arg3)`

If an argument is not of the value type specified, an error is thrown.

###count(coll: Collection)

Returns the number of elements in a map, bag, set, or list. For a map, this is defined as the number of key/value pairs.

###get(coll: Collection, key)

If `coll` is a set, returns `true` if `key` is an element of the set, otherwise returns `false`. If `coll` is a bag, returns the number of times `key` exists in the bag if the bag does contain it, otherwise returns `nil`. If `coll` is a map, returns the value associated with `key`, or `nil` if there is no such association. If `coll` is a list and `key` is not a non-negative integral number, an error is thrown. Otherwise, the `key`th element of the list is returned.

Calling a collection as a function with one value is equivalent to calling `get` on that collection and value.

###set(coll: Map | List, key, value)

If `coll` is a map, returns a new map with `key` associated with `value`. This replaces the existing association for `key` if one exists. Otherwise, if `key` is not a non-negative integral number, an error is thrown. Otherwise, returns a new list with the element at `key` set to `value`. If `key` is greater than or equal to the length of the list, an error is thrown.

###first(l: List)

If `l` has no elements, an error is thrown. Otherwise, returns the first element of `l`.

###rest(l: List)

If `l` has no elements, an error is thrown. Otherwise, returns a list equal to `l` except without `l`'s first element.

###last(l: List)

If `l` has no elements, an error is thrown. Otherwise, returns the last element of `l`.

###butLast(l: list)

If `l` has no elements, an error is thrown. Otherwise, returns a list equal to `l` except without `l`'s last element.

###concat(l1: List, l2: List)

Returns a list consisting of the elements of `l1` followed by the elements of `l2`.

###cons(l: List, value)

Returns a list `lReturn` such that `rest(lReturn) == l` and `head(lReturn) == value)`.

###append(l: List, value)

Returns a list `lReturn` such that `butLast(lReturn) == l` and `last(lReturn) == value)`.

###map(l: List, f: Function)

Returns a list of the return values resulting from calling `f` on each element of `l` in order.

###filter(l: List, f: Function)

Returns a list of the elements of `l`, in order, where calling `f` on the element doesn't return `false` or `nil`.

###reduce(l: List, f: Function)

If `l` is empty, an error is thrown. Otherwise, returns `reduce(rest(l), first(l), f)`.

###reduce(l: List, initial, f: Function)

If `l` is empty, returns `initial`. Otherwise, returns `reduce(rest(l), f(initial, first(l)), f)`.
