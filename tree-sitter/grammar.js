const PREC = {
  OR: 1,
  AND: 2,
  NOT: 3,
  COMPARE: 4,
  CONCAT: 5,
  ADD: 6,
  MULTIPLY: 7,
  UNARY: 8,
  SELECTOR: 9,
  CALL: 10,
};

function escapeRegExp(text) {
  return text.replace(/[\\^$.*+?()[\]{}|]/g, "\\$&");
}

function caseInsensitive(word) {
  return new RegExp(
    word
      .split("")
      .map((ch) => {
        if (/[a-z]/i.test(ch)) {
          return `[${ch.toLowerCase()}${ch.toUpperCase()}]`;
        }
        return escapeRegExp(ch);
      })
      .join(""),
  );
}

function keyword($, word) {
  return alias(token(prec(2, caseInsensitive(word))), $.keyword);
}

function keywordChoice($, words) {
  return choice(...words.map((word) => keyword($, word)));
}

function keywordIdentifier($, word) {
  return alias(token(prec(2, caseInsensitive(word))), $.identifier);
}

function keywordIdentifierChoice($, words) {
  return choice(...words.map((word) => keywordIdentifier($, word)));
}

function assignmentTail($) {
  return repeat1(choice($._expression, keywordChoice($, EXPRESSION_KEYWORDS), $.operator, $.punctuation, $.tail_fragment));
}

function messageTail($) {
  return repeat1(choice(keywordChoice($, MESSAGE_TAIL_KEYWORDS), $._raw_token, $.tail_fragment));
}

function memoryTransferTail($) {
  return repeat1(choice($._memory_transfer_assignment, $._compound_tail_keyword, keywordChoice($, MEMORY_TRANSFER_KEYWORDS), $._raw_token, $.tail_fragment));
}

function declarationKeywordFieldName($) {
  return prec(
    3,
    seq(
      keywordIdentifierChoice($, DECLARATION_FIELD_NAME_KEYWORDS),
      keywordChoice($, ["TYPE", "LIKE"]),
    ),
  );
}

const DECLARATION_KEYWORDS = [
  "DATA",
  "TYPES",
  "CONSTANTS",
  "FIELD-SYMBOLS",
  "STATICS",
  "TABLES",
  "RANGES",
  "PARAMETERS",
  "PARAMETER",
  "SELECT-OPTIONS",
  "CONTROLS",
  "CLASS-DATA",
  "TYPE-POOLS",
  "FUNCTION-POOL",
];

const SIMPLE_STATEMENT_KEYWORDS = [
  "CLEAR",
  "REFRESH",
  "FREE",
  "UNASSIGN",
  "MOVE",
  "MOVE-CORRESPONDING",
  "SUBTRACT",
  "MULTIPLY",
  "DIVIDE",
  "COMPUTE",
  "CONCATENATE",
  "SPLIT",
  "CONDENSE",
  "REPLACE",
  "TRANSLATE",
  "SHIFT",
  "SEARCH",
  "PERFORM",
  "WRITE",
  "ASSERT",
  "CHECK",
  "RETURN",
  "LEAVE",
  "CONTINUE",
  "EXIT",
  "STOP",
  "COMMIT",
  "ROLLBACK",
  "DESCRIBE",
  "RECEIVE",
  "GET",
  "SET",
  "LOG-POINT",
  "RAISE",
  "AUTHORITY-CHECK",
  "FIELD-GROUPS",
  "FIELD",
  "ASSIGN",
  "OVERLAY",
  "PACK",
  "CONVERT",
  "WAIT",
  "SKIP",
  "ULINE",
  "NEW-LINE",
  "NEW-PAGE",
  "RESERVE",
  "BACK",
  "FORMAT",
  "POSITION",
  "HIDE",
  "SELECTION-SCREEN",
  "READ",
  "INSERT",
  "APPEND",
  "MODIFY",
  "OPEN",
  "FETCH",
  "CLOSE",
  "GENERATE",
  "INTERFACES",
  "EVENTS",
  "CLASS-EVENTS",
  "ALIASES",
  "ENDSELECT",
];

const CALLABLE_KEYWORDS = [
  "ADD",
  "APPEND",
  "CALL",
  "CLEANUP",
  "CREATE",
  "DATA",
  "DELETE",
  "FORM",
  "GET",
  "INSERT",
  "LINES",
  "MODIFY",
  "RAISE",
  "READ",
  "SET",
  "SORT",
  "UNPACK",
  "UPDATE",
];

const EXPRESSION_KEYWORDS = [
  "AND",
  "OR",
  "NOT",
  "IS",
  "INITIAL",
  "ASSIGNED",
  "BOUND",
  "BETWEEN",
  "IN",
  "LIKE",
  "EQ",
  "NE",
  "LT",
  "LE",
  "GT",
  "GE",
  "CO",
  "CN",
  "CA",
  "NA",
  "CS",
  "NS",
  "CP",
  "NP",
  "DIV",
  "MOD",
  "BIT-AND",
  "BIT-OR",
  "BIT-XOR",
  "VALUE",
  "NEW",
  "CONV",
  "COND",
  "SWITCH",
  "REDUCE",
  "REF",
  "CORRESPONDING",
  "FILTER",
  "INIT",
  "LET",
  "FOR",
  "THEN",
  "UNTIL",
  "NEXT",
  "BASE",
  "MAPPING",
  "EXCEPT",
  "OPTIONAL",
  "DEFAULT",
  "LINES",
  "OF",
  "TYPE",
  "LIKE",
  "REF",
  "TO",
  "TABLE",
  "STANDARD",
  "SORTED",
  "HASHED",
  "WITH",
  "USING",
  "KEY",
  "EMPTY",
  "UNIQUE",
  "NON-UNIQUE",
  "INTO",
  "FROM",
  "WHERE",
  "GROUP",
  "BY",
  "HAVING",
  "ORDER",
  "AS",
  "ON",
  "JOIN",
  "INNER",
  "LEFT",
  "OUTER",
  "RIGHT",
  "FULL",
  "UNION",
  "ALL",
  "DISTINCT",
  "UP",
  "ROWS",
  "PACKAGE",
  "SIZE",
  "OFFSET",
  "SINGLE",
  "CLIENT",
  "SPECIFIED",
  "BYPASSING",
  "BUFFER",
  "CONNECTION",
  "APPENDING",
  "TRANSPORTING",
  "NO",
  "FIELDS",
  "INDEX",
  "ASSIGNING",
  "REFERENCE",
  "CHANGING",
  "IMPORTING",
  "EXPORTING",
  "RETURNING",
  "RECEIVING",
  "EXCEPTIONS",
  "RAISING",
  "OPTIONAL",
  "DEFAULT",
];

const COMPONENT_NAME_KEYWORDS = [
  "ABSTRACT",
  "ACTION",
  "ADD",
  "ALIAS",
  "ALL",
  "ANY",
  "APPEND",
  "APPEND",
  "ASSIGN",
  "BASE",
  "BEGIN",
  "BOUND",
  "BY",
  "CALL",
  "CASE",
  "CAST",
  "CLASS",
  "CLEANUP",
  "CLIENT",
  "COMPONENT",
  "COMPONENTS",
  "CONSTANTS",
  "CONTROLS",
  "DATA",
  "DEFAULT",
  "DELETE",
  "END",
  "EVENT",
  "EVENTS",
  "EXCEPTION",
  "EXCEPTIONS",
  "EXPORT",
  "FIELD",
  "FIELDS",
  "FILTER",
  "FINAL",
  "FOR",
  "FORM",
  "FROM",
  "FUNCTION",
  "GET",
  "GROUP",
  "IMPORT",
  "INCLUDE",
  "INDEX",
  "INSERT",
  "INTERFACE",
  "INTERFACES",
  "KEY",
  "LEFT",
  "LINE",
  "LINES",
  "LOOP",
  "METHOD",
  "METHODS",
  "MODIFY",
  "NAME",
  "NEW",
  "OBJECT",
  "ORDER",
  "PARAMETER",
  "PARAMETERS",
  "PRIVATE",
  "PROGRAM",
  "PROTECTED",
  "PUBLIC",
  "READ",
  "RAISE",
  "RECEIVE",
  "REF",
  "REFERENCE",
  "REPORT",
  "RESULT",
  "RETURN",
  "RETURNING",
  "RIGHT",
  "SCREEN",
  "SECTION",
  "SELECT",
  "SET",
  "SIZE",
  "SORT",
  "SOURCE",
  "TABLE",
  "TABLES",
  "THEN",
  "TO",
  "TYPE",
  "TYPES",
  "UNION",
  "UNPACK",
  "UPDATE",
  "VALUE",
  "WHERE",
  "WITH",
  "WORK",
];

const CONSTRUCTOR_ARGUMENT_KEYWORDS = [
  ...EXPRESSION_KEYWORDS,
  "WHEN",
  "ELSE",
];

const MESSAGE_TAIL_KEYWORDS = [
  "ID",
  "TYPE",
  "NUMBER",
  "WITH",
  "INTO",
  "DISPLAY",
  "LIKE",
  "RAISING",
];

const MEMORY_TRANSFER_KEYWORDS = [
  "TO",
  "FROM",
  "MEMORY",
  "ID",
  "DATABASE",
  "SHARED",
  "BUFFER",
  "INTERNAL",
  "TABLE",
  "DATA",
  "CLIENT",
];

const FIND_TAIL_KEYWORDS = [
  "ALL",
  "COUNT",
  "FIRST",
  "IN",
  "LENGTH",
  "LINE",
  "MATCH",
  "OCCURRENCE",
  "OCCURRENCES",
  "OF",
  "OFFSET",
  "REGEX",
  "RESULTS",
  "SECTION",
  "SUBMATCHES",
  "TABLE",
];

const STATEMENT_TAIL_KEYWORDS = [
  "ACCEPTING",
  "ADD",
  "ADJACENT",
  "ASCENDING",
  "BEGIN",
  "BINARY",
  "BLOCK",
  "CLEAR",
  "COMPARING",
  "CURSOR",
  "DESCENDING",
  "DESTINATION",
  "DISTINCT",
  "DUPLICATE",
  "DUPLICATES",
  "ENTRIES",
  "FIELD",
  "FIELD-SYMBOL",
  "FRAME",
  "KEYS",
  "MESSAGE",
  "NEXT",
  "SEARCH",
  "SELECT",
  "SEPARATED",
  "TITLE",
  "USING",
  "USER-COMMAND",
  "OBLIGATORY",
  "RADIOBUTTON",
  "RANGE",
  "LENGTH",
  "END",
  "FINAL",
  "FUNCTION",
  "VIA",
  "JOB",
  "NUMBER",
  "RETURN",
  "OTHERS",
  "WORK",
  "DATA",
  "AT",
  "OUTPUT",
  "TIME",
  "STAMP",
  "ZONE",
  "DATE",
  "DISPLAY",
  "GET",
  "APPEND",
  "INSERT",
  "METHOD",
  "MODIFY",
  "NEW",
  "OF",
  "SOURCE",
  "RESULT",
  "SET",
  "SORT",
  "TRANSFORMATION",
  "XML",
  "OPTIONS",
  "PATCH",
  "POST",
  "PUT",
  "READ",
  "UPDATE",
  "TIMES",
  "ABSTRACT",
  "ANY",
  "CATEGORY",
  "COMPONENT",
  "COMPONENTS",
  "CREATE",
  "DELETE",
  "DEFINITION",
  "DEFERRED",
  "ENUM",
  "EVENT",
  "EXCEPTION",
  "FOR",
  "GROUP",
  "HOLD",
  "IGNORING",
  "IMPLEMENTS",
  "INHERITING",
  "INSTANCE",
  "PREFERRED",
  "PRIVATE",
  "PROTECTED",
  "PUBLIC",
  "RAISING",
  "REDEFINITION",
  "SHORTDUMP-ID",
  "TESTING",
  "VALUE",
  "WAIT",
];

const DECLARATION_FIELD_NAME_KEYWORDS = [
  "FIELD",
  "SOURCE",
];

const STATEMENT_FIELD_NAME_KEYWORDS = [
  "METHOD",
];

module.exports = grammar({
  name: "abap",

  word: ($) => $.identifier,

  extras: ($) => [/[ \t\r\n]+/, $.comment, $.pragma],

  externals: ($) => [
    $.star_comment,
    $.template_start,
    $.template_text,
    $.template_interpolation_start,
    $.template_interpolation_end,
    $.template_end,
  ],

  supertypes: ($) => [
    $._statement,
    $.declaration_statement,
    $._expression,
    $._literal,
  ],

  conflicts: ($) => [
    [$.unary_expression, $.binary_expression, $.selector_expression],
    [$.named_argument, $.binary_expression],
    [$.qualified_name, $.selector_expression],
    [$.binary_expression, $.selector_expression],
    [$.parenthesized_expression, $._raw_token],
    [$.parenthesized_expression, $.argument_list],
    [$._expression, $._raw_token],
    [$._expression, $._argument_token],
    [$._expression, $._argument_value],
    [$.named_argument, $._argument_token],
    [$._tail_token, $._raw_token],
    [$.component_name, $._tail_token],
    [$.component_name, $._raw_token],
    [$.component_name, $._tail_token, $._raw_token],
    [$.dynamic_component, $._raw_token],
    [$.constructor_expression, $._tail_token],
    [$.constructor_expression, $._raw_token],
    [$.simple_statement, $._raw_token],
    [$.simple_statement, $._compound_tail_keyword, $._raw_token],
    [$.simple_statement, $._compound_tail_keyword],
    [$.routine_name, $._expression],
    [$._expression, $._memory_transfer_value],
    [$._expression, $._method_signature_raw_token],
    [$._method_signature_tail_token, $._method_signature_raw_token],
    [$.catch_clause],
  ],

  rules: {
    source_file: ($) => repeat($._statement),

    _statement: ($) =>
      choice(
        $.declaration_statement,
        $.if_statement,
        $.case_statement,
        $.while_statement,
        $.do_statement,
        $.loop_statement,
        $.at_statement,
        $.try_statement,
        $.class_forward_declaration,
        $.interface_forward_declaration,
        $.class_definition,
        $.class_implementation,
        $.interface_definition,
        $.method_signature,
        $.method_definition,
        $.form_definition,
        $.function_definition,
        $.module_definition,
        $.event_block,
        $.macro_definition,
        $.call_statement,
        $.describe_statement,
        $.delete_statement,
        $.update_statement,
        $.sort_statement,
        $.create_statement,
        $.add_statement,
        $.unpack_statement,
        $.sql_dml_statement,
        $.select_statement,
        $.submit_statement,
        $.set_pf_status_statement,
        $.set_titlebar_statement,
        $.exec_sql_statement,
        $.report_statement,
        $.include_statement,
        $.assign_statement,
        $.find_statement,
        $.assignment_statement,
        $.expression_statement,
        $.simple_statement,
        $.unknown_statement,
      ),

    pragma: (_) => token(prec(3, /##[A-Za-z0-9_]+(\[[^\]\r\n]*\])?/)),

    identifier: (_) => token(/[A-Za-z_][A-Za-z0-9_]*/),
    escaped_identifier: (_) => token(/![A-Za-z_][A-Za-z0-9_]*/),
    field_symbol: (_) => token(/<[A-Za-z_][A-Za-z0-9_]*>/),
    field_symbol_path: (_) =>
      token(prec(1, /<[A-Za-z_][A-Za-z0-9_]*>(?:-[A-Za-z_][A-Za-z0-9_]*)+/)),
    _slash_identifier: (_) =>
      token(/\/[A-Za-z][A-Za-z0-9_]*\/[A-Za-z_][A-Za-z0-9_]*/),
    cte_identifier: (_) => token(prec(1, /\+[A-Za-z_][A-Za-z0-9_]*/)),
    field_path: (_) =>
      token(prec(1, /[A-Za-z_][A-Za-z0-9_]*(?:-[A-Za-z_][A-Za-z0-9_]*)+/)),
    sql_wildcard_selector: (_) =>
      token(prec(1, /[A-Za-z_][A-Za-z0-9_]*~\*/)),
    static_type_path: (_) =>
      token(
        prec(
          1,
          /[A-Za-z_][A-Za-z0-9_]*(?:=>[A-Za-z_][A-Za-z0-9_]*)(?:-[A-Za-z_][A-Za-z0-9_]*)*/,
        ),
      ),
    dynamic_name: (_) => token(prec(1, /\([A-Za-z_][A-Za-z0-9_]*\)/)),

    _name: ($) =>
      choice(
        $.identifier,
        $.escaped_identifier,
        $.field_symbol,
        alias($._slash_identifier, $.identifier),
        $.cte_identifier,
      ),

    _argument_name: ($) =>
      choice(
        $._name,
        keywordIdentifierChoice($, COMPONENT_NAME_KEYWORDS),
      ),

    component_name: ($) =>
      prec(
        3,
        choice(
          $._name,
          keywordIdentifierChoice($, COMPONENT_NAME_KEYWORDS),
        ),
      ),

    dynamic_component: ($) =>
      seq(
        "(",
        repeat1(choice($._raw_token, keywordChoice($, STATEMENT_TAIL_KEYWORDS))),
        ")",
      ),

    routine_name: ($) => choice($.qualified_name, $.component_name),

    qualified_name: ($) =>
      prec.left(
        PREC.SELECTOR,
        seq(
          $._name,
          repeat(seq(choice("-", "->", "=>", "~"), $.component_name)),
        ),
      ),

    number: (_) => token(/\d+/),

    comment: ($) => choice($.line_comment, $.star_comment),

    line_comment: (_) => token(/"[^\r\n]*/),

    string: (_) =>
      choice(
        token(seq("'", repeat(choice(/[^'\r\n]/, "''")), "'")),
        token(seq("`", repeat(choice(/[^`\r\n]/, "``")), "`")),
      ),

    string_template: ($) =>
      seq(
        $.template_start,
        repeat(choice($.template_text, $.template_interpolation)),
        $.template_end,
      ),

    template_interpolation: ($) =>
      seq(
        $.template_interpolation_start,
        repeat($._template_token),
        $.template_interpolation_end,
      ),

    _template_token: ($) =>
      choice(
        $._expression,
        keywordChoice($, [
          "WIDTH",
          "ALIGN",
          "DECIMALS",
          "ALPHA",
          "DATE",
          "TIME",
          "TIMESTAMP",
          "CASE",
          "SIGN",
          "STYLE",
          "COUNTRY",
        ]),
        keywordChoice($, EXPRESSION_KEYWORDS),
        $.operator,
        $.template_punctuation,
      ),

    template_punctuation: (_) => token(choice(",", ":", "(", ")", "[", "]")),

    _literal: ($) => choice($.number, $.string, $.string_template),

    _expression: ($) =>
      choice(
        $.binary_expression,
        $.unary_expression,
        $.selector_expression,
        $.interface_selector_expression,
        $.wildcard_selector_expression,
        $.call_expression,
        $.table_expression,
        $.substring_expression,
        $.constructor_expression,
        $.parenthesized_expression,
        $.host_expression,
        $.field_symbol_path,
        $.field_path,
        $.static_type_path,
        $.qualified_name,
        $._literal,
      ),

    host_expression: ($) => seq("@", field("value", $._expression)),

    parenthesized_expression: ($) =>
      seq("(", optional(seq($._expression, repeat(seq(",", $._expression)))), ")"),

    unary_expression: ($) =>
      prec.left(PREC.UNARY, seq(field("operator", choice("+", "-", keyword($, "NOT"))), field("operand", $._expression))),

    binary_expression: ($) =>
      choice(
        prec.left(PREC.OR, seq(field("left", $._expression), field("operator", keyword($, "OR")), field("right", $._expression))),
        prec.left(PREC.AND, seq(field("left", $._expression), field("operator", keyword($, "AND")), field("right", $._expression))),
        prec.left(PREC.COMPARE, seq(field("left", $._expression), field("operator", keyword($, "IS")), optional(field("negation", keyword($, "NOT"))), field("right", keywordChoice($, ["INITIAL", "BOUND", "ASSIGNED"])))),
        prec.left(PREC.COMPARE, seq(field("left", $._expression), field("operator", choice("=", "<>", "<", ">", "<=", ">=", keywordChoice($, ["EQ", "NE", "LT", "LE", "GT", "GE", "IS", "BETWEEN", "IN", "LIKE", "CO", "CN", "CA", "NA", "CS", "NS", "CP", "NP"]))), field("right", $._expression))),
        prec.left(PREC.CONCAT, seq(field("left", $._expression), field("operator", "&&"), field("right", $._expression))),
        prec.left(PREC.ADD, seq(field("left", $._expression), field("operator", choice("+", "-")), field("right", $._expression))),
        prec.left(PREC.MULTIPLY, seq(field("left", $._expression), field("operator", choice("*", "/", keyword($, "DIV"), keyword($, "MOD"))), field("right", $._expression))),
      ),

    selector_expression: ($) =>
      prec.left(
        PREC.SELECTOR,
        seq(
          field("object", $._expression),
          field("operator", choice("-", "->", "=>")),
          field("property", choice($.component_name, $.dynamic_component)),
        ),
      ),

    wildcard: (_) => token("*"),

    wildcard_selector_expression: ($) =>
      prec.left(
        PREC.SELECTOR,
        seq(
          field("object", $._expression),
          field("operator", choice("->", "=>", "~")),
          field("property", $.wildcard),
        ),
      ),

    interface_selector_expression: ($) =>
      prec.left(
        PREC.SELECTOR,
        seq(
          field("object", $._expression),
          field("operator", "~"),
          field("property", choice($.component_name, $.dynamic_component)),
        ),
      ),

    call_expression: ($) =>
      prec.left(
        PREC.CALL,
        seq(field("function", choice($._expression, keywordChoice($, CALLABLE_KEYWORDS))), field("arguments", $.argument_list)),
      ),

    argument_list: ($) =>
      seq(
        "(",
        repeat(choice($.constructor_row, $.named_argument, $._expression, $._argument_token)),
        ")",
      ),

    _argument_token: ($) =>
      choice(
        $.static_type_path,
        $.unary_expression,
        $.selector_expression,
        $.interface_selector_expression,
        $.wildcard_selector_expression,
        $.call_expression,
        $.table_expression,
        $.substring_expression,
        $.constructor_expression,
        $.parenthesized_expression,
        $.host_expression,
        $.field_symbol_path,
        $.field_path,
        $.positional_name,
        $._literal,
        keywordChoice($, CONSTRUCTOR_ARGUMENT_KEYWORDS),
        $.operator,
        $.punctuation,
      ),

    named_argument: ($) =>
      prec(PREC.COMPARE + 1, seq(field("name", $._argument_name), $.equals, field("value", $._expression))),

    equals: (_) => token(prec(3, "=")),

    _argument_value: ($) =>
      choice(
        $.static_type_path,
        $.field_symbol_path,
        $.field_path,
        $.unary_expression,
        $.selector_expression,
        $.interface_selector_expression,
        $.wildcard_selector_expression,
        $.call_expression,
        $.table_expression,
        $.substring_expression,
        $.constructor_expression,
        $.parenthesized_expression,
        $.host_expression,
        $.qualified_name,
        $._literal,
      ),

    positional_name: ($) => prec(-1, $.qualified_name),

    table_expression: ($) =>
      prec.left(
        PREC.CALL,
        seq(field("table", $._expression), "[", repeat(choice($.named_argument, $._expression, keywordChoice($, EXPRESSION_KEYWORDS), $.operator, $.punctuation)), "]"),
      ),

    substring_expression: (_) =>
      token(
        prec(
          1,
          /[A-Za-z_][A-Za-z0-9_]*(?:-[A-Za-z_][A-Za-z0-9_]*)*(?:(?:\+\d+)?\(\d+\)|\+\d+)/,
        ),
      ),

    constructor_expression: ($) =>
      prec.left(
        PREC.CALL,
        seq(
          field("constructor", keywordChoice($, ["VALUE", "NEW", "CONV", "COND", "SWITCH", "REDUCE", "FILTER", "CAST"])),
          choice(
            seq(field("type", choice("#", $.qualified_name, $.component_name)), field("arguments", $.argument_list)),
            field("arguments", $.argument_list),
          ),
        ),
      ),

    declaration_statement: ($) =>
      choice(
        $.data_declaration,
        $.types_declaration,
        $.constants_declaration,
        $.field_symbols_declaration,
        $.statics_declaration,
        $.tables_declaration,
        $.ranges_declaration,
        $.parameters_declaration,
        $.select_options_declaration,
        $.controls_declaration,
        $.class_data_declaration,
        $.type_pools_declaration,
        $.function_pool_declaration,
        $.include_type_declaration,
      ),

    data_declaration: ($) => seq(keyword($, "DATA"), optional(":"), optional($._statement_tail), "."),
    types_declaration: ($) => seq(keyword($, "TYPES"), optional(":"), optional($._statement_tail), "."),
    constants_declaration: ($) => seq(keyword($, "CONSTANTS"), optional(":"), optional($._statement_tail), "."),
    field_symbols_declaration: ($) => seq(keyword($, "FIELD-SYMBOLS"), optional(":"), optional($._statement_tail), "."),
    statics_declaration: ($) => seq(keyword($, "STATICS"), optional(":"), optional($._statement_tail), "."),
    tables_declaration: ($) => seq(keyword($, "TABLES"), optional(":"), optional($._statement_tail), "."),
    ranges_declaration: ($) => seq(keyword($, "RANGES"), optional(":"), optional($._statement_tail), "."),
    parameters_declaration: ($) => seq(keywordChoice($, ["PARAMETERS", "PARAMETER"]), optional(":"), optional($._statement_tail), "."),
    select_options_declaration: ($) => seq(keyword($, "SELECT-OPTIONS"), optional(":"), optional($._statement_tail), "."),
    controls_declaration: ($) => seq(keyword($, "CONTROLS"), optional(":"), optional($._statement_tail), "."),
    class_data_declaration: ($) => seq(keyword($, "CLASS-DATA"), optional(":"), optional($._statement_tail), "."),
    type_pools_declaration: ($) => seq(keyword($, "TYPE-POOLS"), optional(":"), optional($._statement_tail), "."),
    function_pool_declaration: ($) => seq(keyword($, "FUNCTION-POOL"), optional(field("name", $._name)), optional($._statement_tail), "."),

    include_type_declaration: ($) =>
      seq(
        keyword($, "INCLUDE"),
        keywordChoice($, ["TYPE", "STRUCTURE"]),
        field("name", optional($._name)),
        optional($._statement_tail),
        ".",
      ),

    declaration_clause: ($) =>
      seq(
        optional(","),
        choice(
          seq(
            optional(keywordChoice($, ["BEGIN", "END", "INCLUDE"])),
            optional(keyword($, "OF")),
            field("name", $._name),
          ),
          seq(keywordChoice($, ["BEGIN", "END", "INCLUDE"]), keyword($, "OF"), field("name", $._name)),
          $.declaration_addition,
          $._expression,
        ),
        repeat(choice($.declaration_addition, $._expression, keywordChoice($, EXPRESSION_KEYWORDS), $.operator, $.punctuation)),
        optional(","),
      ),

    declaration_addition: ($) =>
      keywordChoice($, ["TYPE", "LIKE", "VALUE", "LENGTH", "DECIMALS", "OCCURS", "WITH", "KEY", "READ-ONLY", "AS", "RENAMING", "OPTIONAL", "DEFAULT"]),

    report_statement: ($) =>
      seq(keywordChoice($, ["REPORT", "PROGRAM"]), field("name", optional($._name)), optional($._statement_tail), "."),

    include_statement: ($) =>
      seq(keyword($, "INCLUDE"), optional(":"), repeat1(choice($._name, ",", keywordChoice($, ["IF", "FOUND"]))), "."),

    if_statement: ($) =>
      seq(
        keyword($, "IF"),
        field("condition", $._statement_tail),
        ".",
        field("body", repeat($._statement)),
        repeat($.elseif_clause),
        optional($.else_clause),
        keyword($, "ENDIF"),
        ".",
      ),

    elseif_clause: ($) =>
      seq(keyword($, "ELSEIF"), field("condition", $._statement_tail), ".", field("body", repeat($._statement))),

    else_clause: ($) => seq(keyword($, "ELSE"), ".", field("body", repeat($._statement))),

    case_statement: ($) =>
      seq(
        keyword($, "CASE"),
        field("value", $._statement_tail),
        ".",
        repeat($.when_clause),
        keyword($, "ENDCASE"),
        ".",
      ),

    when_clause: ($) =>
      seq(keyword($, "WHEN"), field("condition", $._statement_tail), ".", field("body", repeat($._statement))),

    while_statement: ($) =>
      seq(keyword($, "WHILE"), field("condition", $._statement_tail), ".", field("body", repeat($._statement)), keyword($, "ENDWHILE"), "."),

    do_statement: ($) =>
      seq(keyword($, "DO"), optional($._statement_tail), ".", field("body", repeat($._statement)), keyword($, "ENDDO"), "."),

    loop_statement: ($) =>
      seq(keyword($, "LOOP"), choice(prec(1, seq(keyword($, "AT"), keyword($, "SCREEN"))), $._statement_tail), ".", field("body", repeat($._statement)), keyword($, "ENDLOOP"), "."),

    at_statement: ($) =>
      seq(
        keyword($, "AT"),
        choice(
          keyword($, "FIRST"),
          keyword($, "LAST"),
          seq(keyword($, "NEW"), field("name", $._name)),
          seq(keyword($, "END"), keyword($, "OF"), field("name", $._name)),
        ),
        optional($._statement_tail),
        ".",
        field("body", repeat($._statement)),
        keyword($, "ENDAT"),
        ".",
      ),

    try_statement: ($) =>
      seq(
        keyword($, "TRY"),
        ".",
        field("body", repeat($._statement)),
        repeat($.catch_clause),
        optional($.cleanup_clause),
        keyword($, "ENDTRY"),
        ".",
      ),

    catch_clause: ($) =>
      seq(keyword($, "CATCH"), $._statement_tail, ".", field("body", repeat($._statement))),

    cleanup_clause: ($) =>
      seq(keyword($, "CLEANUP"), ".", field("body", repeat($._statement))),

    class_definition: ($) =>
      seq(
        keyword($, "CLASS"),
        field("name", $._name),
        keyword($, "DEFINITION"),
        optional($._statement_tail),
        ".",
        field("body", repeat(choice($.visibility_section, $.method_signature, $.declaration_statement, $.simple_statement, $.unknown_statement))),
        keyword($, "ENDCLASS"),
        ".",
      ),

    class_forward_declaration: ($) =>
      prec(
        2,
        seq(
          keyword($, "CLASS"),
          field("name", $._name),
          keyword($, "DEFINITION"),
          keyword($, "DEFERRED"),
          optional($._statement_tail),
          ".",
        ),
      ),

    class_implementation: ($) =>
      seq(
        keyword($, "CLASS"),
        field("name", $._name),
        keyword($, "IMPLEMENTATION"),
        optional($._statement_tail),
        ".",
        field("body", repeat($._statement)),
        keyword($, "ENDCLASS"),
        ".",
      ),

    visibility_section: ($) =>
      seq(keywordChoice($, ["PUBLIC", "PROTECTED", "PRIVATE"]), keyword($, "SECTION"), "."),

    method_signature: ($) =>
      prec(
        2,
        seq(
          keywordChoice($, ["METHODS", "CLASS-METHODS"]),
          optional(":"),
          optional($._method_signature_entries),
          ".",
        ),
      ),

    _method_signature_entries: ($) =>
      seq($._method_signature_entry, repeat(seq(",", $._method_signature_entry))),

    _method_signature_entry: ($) =>
      seq(field("name", $.routine_name), optional($._method_signature_tail)),

    _method_signature_tail: ($) => repeat1($._method_signature_tail_token),

    _method_signature_tail_token: ($) =>
      choice(
        $._compound_tail_keyword,
        $.type_ref_tail,
        $.value_tail,
        keywordChoice($, STATEMENT_TAIL_KEYWORDS),
        $._method_signature_raw_token,
        $.tail_fragment,
      ),

    _method_signature_raw_token: ($) =>
      choice(
        $.static_type_path,
        $.field_symbol_path,
        $.field_path,
        $._expression,
        keywordChoice($, EXPRESSION_KEYWORDS),
        $.operator,
        $._method_signature_punctuation,
      ),

    _method_signature_punctuation: (_) => token(choice(":", "(", ")", "[", "]")),

    interface_definition: ($) =>
      seq(
        keyword($, "INTERFACE"),
        field("name", $._name),
        optional($._statement_tail),
        ".",
        field("body", repeat(choice($.method_signature, $.declaration_statement, $.simple_statement, $.unknown_statement))),
        keyword($, "ENDINTERFACE"),
        ".",
      ),

    interface_forward_declaration: ($) =>
      prec(
        2,
        seq(
          keyword($, "INTERFACE"),
          field("name", $._name),
          keyword($, "DEFERRED"),
          optional($._statement_tail),
          ".",
        ),
      ),

    method_definition: ($) =>
      seq(keyword($, "METHOD"), field("name", $.routine_name), optional($._statement_tail), ".", field("body", repeat($._statement)), keyword($, "ENDMETHOD"), "."),

    form_definition: ($) =>
      seq(keyword($, "FORM"), field("name", $._name), optional($._statement_tail), ".", field("body", repeat($._statement)), keyword($, "ENDFORM"), "."),

    function_definition: ($) =>
      seq(keyword($, "FUNCTION"), field("name", $._name), optional($._statement_tail), ".", field("body", repeat($._statement)), keyword($, "ENDFUNCTION"), "."),

    module_definition: ($) =>
      seq(
        keyword($, "MODULE"),
        field("name", $._name),
        optional(field("direction", keywordChoice($, ["INPUT", "OUTPUT"]))),
        ".",
        field("body", repeat($._statement)),
        keyword($, "ENDMODULE"),
        ".",
      ),

    event_block: ($) =>
      seq(
        field("name", keywordChoice($, [
          "INITIALIZATION",
          "LOAD-OF-PROGRAM",
          "START-OF-SELECTION",
          "END-OF-SELECTION",
          "TOP-OF-PAGE",
          "END-OF-PAGE",
          "AT SELECTION-SCREEN",
        ])),
        optional($._statement_tail),
        ".",
      ),

    macro_definition: ($) =>
      seq(keyword($, "DEFINE"), field("name", optional($._name)), optional($._statement_tail), ".", field("body", repeat($._statement)), keyword($, "END-OF-DEFINITION"), "."),

    call_statement: ($) =>
      prec(
        3,
        seq(
          keyword($, "CALL"),
          keywordChoice($, ["FUNCTION", "METHOD", "TRANSFORMATION", "SCREEN", "SELECTION-SCREEN"]),
          repeat($._call_tail_token),
          ".",
        ),
      ),

    _call_tail_token: ($) =>
      choice(
        $.call_parameter_assignment,
        $._sql_tail_token,
      ),

    call_parameter_assignment: ($) =>
      prec(
        2,
        seq(
          field("name", $._call_parameter_name),
          $.equals,
          field("value", $._call_parameter_value),
        ),
      ),

    _call_parameter_name: ($) =>
      choice(
        $._name,
        keywordIdentifier($, "RETURN"),
      ),

    _call_parameter_value: ($) => $._sql_tail_token,

    describe_statement: ($) =>
      prec(
        3,
        seq(
          keyword($, "DESCRIBE"),
          keyword($, "FIELD"),
          field("field", $._describe_field_operand),
          keyword($, "LENGTH"),
          field("length", $._describe_field_operand),
          keyword($, "IN"),
          keyword($, "CHARACTER"),
          keyword($, "MODE"),
          ".",
        ),
      ),

    _describe_field_operand: ($) =>
      choice(
        $.static_type_path,
        $.field_symbol_path,
        $.field_path,
        $.dynamic_name,
        $.qualified_name,
      ),

    select_statement: ($) =>
      prec(
        1,
        seq(
          keywordChoice($, ["SELECT", "WITH"]),
          repeat1($._sql_tail_token),
          ".",
        ),
      ),

    sql_dml_statement: ($) =>
      prec(
        3,
        seq(
          keyword($, "DELETE"),
          keyword($, "FROM"),
          repeat($._sql_tail_token),
          ".",
        ),
      ),

    delete_statement: ($) =>
      prec(
        3,
        seq(
          keyword($, "DELETE"),
          choice(
            $.qualified_name,
            $.field_path,
            $.field_symbol_path,
            keywordChoice($, ["TABLE", "ADJACENT"]),
          ),
          repeat($._sql_tail_token),
          ".",
        ),
      ),

    update_statement: ($) =>
      prec(
        3,
        choice(
          seq(
            keyword($, "UPDATE"),
            choice(
              $.dynamic_name,
              $.qualified_name,
              $.field_path,
              $.field_symbol_path,
            ),
            keyword($, "SET"),
            repeat($._sql_tail_token),
            ".",
          ),
          seq(
            keyword($, "UPDATE"),
            choice(
              $.dynamic_name,
              $.qualified_name,
              $.field_path,
              $.field_symbol_path,
            ),
            keyword($, "FROM"),
            repeat($._sql_tail_token),
            ".",
          ),
        ),
      ),

    sort_statement: ($) =>
      prec(
        3,
        seq(
          keyword($, "SORT"),
          choice($.qualified_name, $.field_path, $.field_symbol_path),
          repeat($._sql_tail_token),
          ".",
        ),
      ),

    create_statement: ($) =>
      prec(
        3,
        seq(
          keyword($, "CREATE"),
          keywordChoice($, ["OBJECT", "DATA"]),
          repeat($._sql_tail_token),
          ".",
        ),
      ),

    add_statement: ($) =>
      prec(
        3,
        seq(
          keyword($, "ADD"),
          choice($._literal, $.qualified_name, $.field_path, $.field_symbol_path),
          repeat($._sql_tail_token),
          ".",
        ),
      ),

    unpack_statement: ($) =>
      prec(
        3,
        seq(
          keyword($, "UNPACK"),
          choice($._literal, $.qualified_name, $.field_path, $.field_symbol_path),
          repeat($._sql_tail_token),
          ".",
        ),
      ),

    _sql_tail_token: ($) =>
      choice(
        $.static_type_path,
        $.field_symbol_path,
        $.field_path,
        $.dynamic_name,
        $.substring_expression,
        $.sql_wildcard_selector,
        $.qualified_name,
        $._literal,
        keywordChoice($, EXPRESSION_KEYWORDS),
        keywordChoice($, STATEMENT_TAIL_KEYWORDS),
        $.operator,
        $.punctuation,
        $.tail_fragment,
      ),

    submit_statement: ($) =>
      prec(
        2,
        seq(
          keyword($, "SUBMIT"),
          repeat($._submit_tail_token),
          ".",
        ),
      ),

    _submit_tail_token: ($) =>
      choice(
        keywordChoice($, STATEMENT_TAIL_KEYWORDS),
        $.qualified_name,
        $._literal,
        keywordChoice($, EXPRESSION_KEYWORDS),
        $.operator,
        $.punctuation,
      ),

    set_pf_status_statement: ($) =>
      prec(
        3,
        seq(
          keyword($, "SET"),
          keyword($, "PF-STATUS"),
          field("status", choice($._literal, $.qualified_name, $.field_path, $.field_symbol_path, $.dynamic_name)),
          optional($._statement_tail),
          ".",
        ),
      ),

    set_titlebar_statement: ($) =>
      prec(
        3,
        seq(
          keyword($, "SET"),
          keyword($, "TITLEBAR"),
          field("title", choice($._literal, $.qualified_name, $.field_path, $.field_symbol_path, $.dynamic_name)),
          optional($._statement_tail),
          ".",
        ),
      ),

    exec_sql_statement: ($) =>
      seq(keyword($, "EXEC"), keyword($, "SQL"), ".", repeat($.unknown_statement), keyword($, "ENDEXEC"), "."),

    assign_statement: ($) =>
      prec(
        3,
        seq(
          keyword($, "ASSIGN"),
          keyword($, "COMPONENT"),
          field("component", choice($._literal, $.qualified_name, $.field_path, $.field_symbol_path, $.dynamic_name, $.dynamic_component)),
          keyword($, "OF"),
          keyword($, "STRUCTURE"),
          field("structure", choice($.qualified_name, $.field_path, $.field_symbol_path, $.selector_expression, $.table_expression, $.dynamic_name)),
          keyword($, "TO"),
          field("target", choice($.field_symbol, $.field_symbol_path)),
          ".",
        ),
      ),

    find_statement: ($) =>
      prec(
        3,
        seq(
          keyword($, "FIND"),
          repeat1($._find_tail_token),
          ".",
        ),
      ),

    _find_tail_token: ($) =>
      choice(
        keywordChoice($, FIND_TAIL_KEYWORDS),
        $.static_type_path,
        $.field_symbol_path,
        $.field_path,
        $.dynamic_name,
        $.substring_expression,
        $.qualified_name,
        $._literal,
        $.operator,
        $.punctuation,
        $.tail_fragment,
      ),

    assignment_statement: ($) =>
      prec(
        2,
        seq(
          field("left", choice($.qualified_name, $.selector_expression, $.table_expression)),
          field("operator", choice("=", "?=")),
          field("right", optional(assignmentTail($))),
          ".",
        ),
      ),

    expression_statement: ($) =>
      prec(2, seq(field("expression", $.call_expression), optional($._statement_tail), ".")),

    simple_statement: ($) =>
      choice(
        prec(1, seq(field("keyword", keyword($, "MODIFY")), keyword($, "SCREEN"), optional($._statement_tail), ".")),
        prec(1, seq(field("keyword", keyword($, "MESSAGE")), optional(messageTail($)), ".")),
        prec(1, seq(field("keyword", keyword($, "EXPORT")), optional(memoryTransferTail($)), ".")),
        prec(1, seq(field("keyword", keyword($, "IMPORT")), optional(memoryTransferTail($)), ".")),
        seq(field("keyword", keywordChoice($, SIMPLE_STATEMENT_KEYWORDS)), optional($._statement_tail), "."),
      ),

    unknown_statement: ($) => prec.dynamic(-10, seq(repeat1($._raw_token), ".")),

    _statement_tail: ($) => repeat1($._tail_token),

    _tail_token: ($) =>
      choice(
        $.keyword_field_assignment,
        declarationKeywordFieldName($),
        $._compound_tail_keyword,
        $.type_ref_tail,
        $.value_tail,
        keywordChoice($, STATEMENT_TAIL_KEYWORDS),
        $._raw_token,
        $.tail_fragment,
      ),

    keyword_field_assignment: ($) =>
      prec(
        PREC.COMPARE + 1,
        seq(
          field("name", keywordIdentifierChoice($, STATEMENT_FIELD_NAME_KEYWORDS)),
          $.equals,
          field("value", $._argument_value),
        ),
      ),

    type_ref_tail: ($) =>
      prec(
        2,
        seq(
          keyword($, "REF"),
          keyword($, "TO"),
          choice($.qualified_name, $.component_name, $.dynamic_component),
        ),
      ),

    value_tail: ($) =>
      prec(
        PREC.COMPARE + 1,
        seq(
          keywordChoice($, ["VALUE", "DEFAULT"]),
          choice(
            $._expression,
            $._literal,
            $.qualified_name,
            $.selector_expression,
            $.component_name,
            keywordChoice($, COMPONENT_NAME_KEYWORDS),
          ),
        ),
      ),

    _memory_transfer_assignment: ($) =>
      prec(
        PREC.COMPARE + 1,
        seq(
          field("name", $._argument_name),
          $.equals,
          field("value", $._memory_transfer_value),
        ),
      ),

    _memory_transfer_value: ($) =>
      choice(
        $.static_type_path,
        $.field_symbol_path,
        $.field_path,
        $.dynamic_name,
        $.substring_expression,
        $.host_expression,
        $.qualified_name,
        $._literal,
      ),

    _compound_tail_keyword: ($) =>
      choice(
        seq(keyword($, "LOWER"), keyword($, "CASE")),
        seq(keyword($, "MODIF"), keyword($, "ID")),
        seq(keyword($, "MEMORY"), keyword($, "ID")),
        seq(keyword($, "AS"), keyword($, "CHECKBOX")),
        seq(keyword($, "ON"), keyword($, "VALUE-REQUEST")),
        seq(keyword($, "TO"), keyword($, "SCREEN")),
      ),

    _raw_token: ($) =>
      choice(
        $.static_type_path,
        $.field_symbol_path,
        $.field_path,
        $._expression,
        keywordChoice($, EXPRESSION_KEYWORDS),
        $.operator,
        $.punctuation,
      ),

    tail_fragment: (_) =>
      token(prec(-1, /[^ \t\r\n.,:()\[\]{}'"`|=<>+\-*\/&@#?~!]+/)),

    operator: (_) =>
      token(choice("->*", "=>*", "->", "=>", "?=", "<=", ">=", "<>", "&&", "=", "+", "-", "*", "/", "<", ">", "~", "@", "#", "&")),

    punctuation: (_) => token(choice(",", ":", "(", ")", "[", "]")),

    constructor_row: ($) =>
      prec(
        1,
        seq(
          "(",
          repeat1(choice($.constructor_row, $.named_argument, $._raw_token)),
          ")",
        ),
      ),
  },
});
