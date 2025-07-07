module.exports = grammar({
  // Define the name of your language. This will be used by Tree-sitter.
  name: 'b',

  // Define tokens that can appear anywhere between other tokens (like whitespace and comments).
  extras: $ => [
    /\s/, // Whitespace (spaces, tabs, newlines)
    $.comment // Comments
  ],

  // Operator precedence levels (higher number means higher precedence)
  // These are defined here for clarity and used throughout the expression rules.
  // Moved outside the 'rules' object as it's a constant, not a grammar rule.
  rules: {
    // The top-level rule of the grammar. A program consists of zero or more definitions.
    program: $ => repeat($.definition),

    // A definition can be either a variable/constant declaration or a function definition.
    definition: $ => choice(
      // Example: name { [constant] } ival {, ival} ;
      // This represents a declaration like: `myVar {10} 5, 6;` or `myArray {} 1, 2, 3;`
      seq(
        $.name,
        // Optional group: {[ {constant}01 ]}01
        // This allows for `name {constant}` or `name {}` or just `name`
        optional(seq('{', optional($.constant), '}')),
        // Optional group: {ival {, ival}0}01
        // This allows for `ival`, `ival, ival`, or nothing
        optional(seq($.ival, repeat(seq(',', $.ival)))),
        ';'
      ),
      // Example: name ( {name {, name}0}01 ) statement
      // This represents a function definition: `myFunc (arg1, arg2) { ... }` or `myFunc () { ... }`
      seq(
        $.name,
        '(',
        // Optional group: {name {, name}0}01
        // This allows for `name`, `name, name`, or nothing (for function parameters)
        optional(seq($.name, repeat(seq(',', $.name)))),
        ')',
        $.statement
      )
    ),

    // An 'ival' (initial value) can be a constant or a name.
    ival: $ => choice(
      $.constant,
      $.name
    ),

    // A statement defines an action or a block of code.
    statement: $ => choice(
      // auto name {constant}01 {, name {constant}01}0 ; statement
      // Local variable declaration with optional initial value/size
      seq(
        'auto',
        $.name,
        optional($.constant), // {constant}01
        repeat(seq(',', $.name, optional($.constant))), // {, name {constant}01}0
        ';',
        $.statement
      ),
      // extrn name {, name}0 ; statement
      // External variable declaration
      seq(
        'extrn',
        $.name,
        repeat(seq(',', $.name)), // {, name}0
        ';',
        $.statement
      ),
      // name : statement (label)
      seq($.name, ':', $.statement),
      // case constant : statement
      seq('case', $.constant, ':', $.statement),
      // { {statement}0 } (code block)
      seq('{', repeat($.statement), '}'),
      // if ( expression ) statement {else statement}01
      // FIX: Apply prec.right to resolve the dangling else conflict
      prec.right(seq(
        'if', '(', $.expression, ')', $.statement,
        optional(seq('else', $.statement)) // {else statement}01
      )),
      // while ( expression ) statement
      seq('while', '(', $.expression, ')', $.statement),
      // switch expression statement
      seq('switch', $.expression, $.statement),
      // goto expression ;
      seq('goto', $.expression, ';'),
      // return {( expression )}01 ;
      seq('return', optional(seq('(', $.expression, ')')), ';'), // {( expression )}01
      // {expression}01 ; (expression statement)
      seq(optional($.expression), ';') // {expression}01 ;
    ),

    // 'expression' is the top-level rule for expressions (formerly 'rvalue').
    // It's defined as an assignment expression, which handles all lower precedence levels.
    expression: $ => $.assignment_expression,

    // Primary expressions are the basic building blocks of expressions.
    primary_expression: $ => choice(
      $.name,
      $.constant,
      seq('(', $.expression, ')') // Parenthesized expression
    ),

    // Postfix expressions handle operators that come after the operand.
    postfix_expression: $ => prec.left(120, choice( // Using literal for PREC.POSTFIX_UNARY
      $.primary_expression, // A primary expression is also a postfix expression
      seq($.postfix_expression, '[', $.expression, ']'), // Array access
      seq($.postfix_expression, '(', optional(seq($.expression, repeat(seq(',', $.expression)))), ')'), // Function call
      seq($.postfix_expression, $.inc_dec) // Postfix increment/decrement
    )),

    // Unary expressions handle operators that come before the operand.
    // All prefix unary operators have the same precedence and bind right-to-left.
    unary_expression: $ => prec.right(110, choice( // Using literal for PREC.PREFIX_UNARY
      $.postfix_expression, // A postfix expression is also a unary expression
      seq($.unary, $.unary_expression), // Unary operators (-, !)
      seq('&', $.unary_expression),     // Address-of
      seq('*', $.unary_expression),     // Dereference
      seq($.inc_dec, $.unary_expression) // Prefix increment/decrement
    )),

    // Multiplicative expressions handle *, /, %
    multiplicative_expression: $ => prec.left(90, choice( // Using literal for PREC.MULTIPLY
      $.unary_expression,
      seq($.multiplicative_expression, $.binary_op_mul, $.unary_expression),
      seq($.multiplicative_expression, $.binary_op_div, $.unary_expression),
      seq($.multiplicative_expression, $.binary_op_mod, $.unary_expression)
    )),

    // Additive expressions handle +, -
    additive_expression: $ => prec.left(80, choice( // Using literal for PREC.ADD
      $.multiplicative_expression,
      seq($.additive_expression, $.binary_op_plus, $.multiplicative_expression),
      seq($.additive_expression, $.binary_op_minus, $.multiplicative_expression)
    )),

    // Shift expressions handle <<, >>
    shift_expression: $ => prec.left(70, choice( // Using literal for PREC.SHIFT
      $.additive_expression,
      seq($.shift_expression, $.binary_op_lshift, $.additive_expression),
      seq($.shift_expression, $.binary_op_rshift, $.additive_expression)
    )),

    // Relational expressions handle <, <=, >, >=
    relational_expression: $ => prec.left(60, choice( // Using literal for PREC.RELATIONAL
      $.shift_expression,
      seq($.relational_expression, $.binary_op_lt, $.shift_expression),
      seq($.relational_expression, $.binary_op_le, $.shift_expression),
      seq($.relational_expression, $.binary_op_gt, $.shift_expression),
      seq($.relational_expression, $.binary_op_ge, $.shift_expression)
    )),

    // Equality expressions handle ==, !=
    equality_expression: $ => prec.left(50, choice( // Using literal for PREC.EQUALITY
      $.relational_expression,
      seq($.equality_expression, $.binary_op_eq, $.relational_expression),
      seq($.equality_expression, $.binary_op_neq, $.relational_expression)
    )),

    // Logical AND expression handle & (binary)
    logical_and_expression: $ => prec.left(40, choice( // Using literal for PREC.LOGICAL_AND
      $.equality_expression,
      seq($.logical_and_expression, $.binary_op_and, $.equality_expression)
    )),

    // Logical OR expression handle |
    logical_or_expression: $ => prec.left(30, choice( // Using literal for PREC.LOGICAL_OR
      $.logical_and_expression,
      seq($.logical_or_expression, $.binary_op_or, $.logical_and_expression)
    )),

    // Ternary conditional expression
    ternary_expression: $ => prec.right(20, choice( // Using literal for PREC.TERNARY
      $.logical_or_expression,
      seq($.logical_or_expression, '?', $.expression, ':', $.ternary_expression)
    )),

    // Assignment expression
    assignment_expression: $ => prec.right(10, choice( // Using literal for PREC.ASSIGN
      $.ternary_expression, // Any expression can be an assignment expression
      seq($.lvalue, $.assign, $.assignment_expression) // Assignment requires an lvalue on LHS
    )),

    // 'lvalue' represents an expression that can appear on the left side of an assignment.
    // It is now defined in terms of the expression hierarchy.
    lvalue: $ => choice(
      $.name, // A simple name is an lvalue
      // Forms that result in an lvalue through operators
      prec.dynamic(110, seq('*', $.unary_expression)), // Dereference (using literal for PREC.PREFIX_UNARY)
      prec.dynamic(120, seq($.postfix_expression, '[', $.expression, ']')) // Array access (using literal for PREC.POSTFIX_UNARY)
    ),

    // The 'assign' rule is defined as '=' optionally followed by a binary operator.
    // This is an unusual syntax for assignment operators (e.g., resulting in `=|`, `==`),
    // but it directly follows the provided BNF.
    assign: $ => prec.right(9, seq('=', optional($.binary))), // Lower precedence than any expression starting with &

    // Increment/decrement operators
    inc_dec: $ => choice(
      '++',
      '--'
    ),

    // Unary operators (for - and !)
    unary: $ => choice(
      '-',
      '!'
    ),

    // Individual binary operator tokens.
    binary_op_or: $ => '|',
    binary_op_and: $ => '&',
    binary_op_eq: $ => '==',
    binary_op_neq: $ => '!=',
    binary_op_lt: $ => '<',
    binary_op_le: $ => '<=',
    binary_op_gt: $ => '>',
    binary_op_ge: $ => '>=',
    binary_op_lshift: $ => '<<',
    binary_op_rshift: $ => '>>',
    binary_op_minus: $ => '-',
    binary_op_plus: $ => '+',
    binary_op_mod: $ => '%',
    binary_op_mul: $ => '*',
    binary_op_div: $ => '/',

    // The 'binary' rule, as used by 'assign', lists all possible binary operator symbols.
    binary: $ => choice(
      $.binary_op_or,
      $.binary_op_and,
      $.binary_op_eq,
      $.binary_op_neq,
      $.binary_op_lt,
      $.binary_op_le,
      $.binary_op_gt,
      $.binary_op_ge,
      $.binary_op_lshift,
      $.binary_op_rshift,
      $.binary_op_minus,
      $.binary_op_plus,
      $.binary_op_mod,
      $.binary_op_mul,
      $.binary_op_div,
    ),

    // A constant can be an integer, character, or string literal.
    constant: $ => choice(
      $.integer_constant,
      $.char_constant,
      $.string_constant
    ),

    // Define integer_constant as a single token matching one or more digits.
    integer_constant: $ => token(/[0-9]+/),

    // A character constant is one or two characters enclosed in single quotes.
    // Updated to use '*' as the escape character and define specific escape sequences.
    char_constant: $ => seq(
      "'",
      choice(
        /[^'\n\r*]/, // Any character except single quote, newline, carriage return, or asterisk
        seq('*', choice('0', 'e', '(', ')', 't', '*', "'", '"', 'n')) // Specific escape sequences
      ),
      optional( // Allows for a second character {char}12
        choice(
          /[^'\n\r*]/,
          seq('*', choice('0', 'e', '(', ')', 't', '*', "'", '"', 'n'))
        )
      ),
      "'"
    ),

    // A string constant is zero or more characters enclosed in double quotes.
    // Updated to use '*' as the escape character and define specific escape sequences.
    string_constant: $ => seq(
      '"',
      repeat( // {char}0
        choice(
          /[^"\n\r*]/, // Any character except double quote, newline, carriage return, or asterisk
          seq('*', choice('0', 'e', '(', ')', 't', '*', "'", '"', 'n')) // Specific escape sequences
        )
      ),
      '"'
    ),

    // A name starts with an alpha character, followed by 0 to 7 alpha-digits.
    // This means a name can be 1 to 8 characters long.
    // Keywords are implicitly reserved because they are defined as literal strings.
    name: $ => token(/[a-zA-Z_][a-zA-Z0-9_]{0,7}/),

    // An alpha-digit is either an alpha character or a digit.
    // This rule is no longer directly used in `name` as it's now a single token regex.
    // It's kept for conceptual clarity if other rules were to use it.
    alpha_digit: $ => choice(
      $.alpha,
      $.digit
    ),

    // Terminal rules (regex for character classes)
    // 'alpha' now includes underscore '_' as per the updated specification.
    // Backspace is not included in the regex for 'alpha' as it's not a typical identifier character.
    alpha: $ => /[a-zA-Z_]/,
    digit: $ => /[0-9]/,

    // http://stackoverflow.com/questions/13014947/regex-to-match-a-c-style-multiline-comment/36328890#36328890 
    comment: $ => token(seq(
      '/*',
      /[^*]*\*+([^/*][^*]*\*+)*/,
      '/'
    )),
  }
});
