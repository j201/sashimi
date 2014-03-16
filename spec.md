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

##Parsing

Whitspace separates tokens but is otherwise ignored.

###Operators

The following operators exist:

`+` `-` `/` `*` `**` `&` `|` `>` `<` `>=` `<=` `==` `!=` `!` `=`

###Reserved Words

In addition to the literals and operators, the following tokens are reserved:

`fn` `if` `when` `case` `let` `module` `import` `export` `type`

###Identifiers

Any sequence of letters, numbers, and underscores that is not a literal or reserved word is an identifier.
