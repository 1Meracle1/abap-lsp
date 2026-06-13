(comment) @comment
(star_comment) @comment
(line_comment) @comment
(pragma) @preproc

(keyword) @keyword

(number) @number
(string) @string
(string_template) @string.special
(template_text) @string.special
(template_start) @punctuation.bracket
(template_end) @punctuation.bracket
(template_interpolation_start) @punctuation.bracket
(template_interpolation_end) @punctuation.bracket

(field_symbol) @variable
(field_symbol_path) @variable
(cte_identifier) @variable.special
(field_path) @variable
(static_type_path) @variable
(dynamic_name) @variable
(sql_wildcard_selector) @variable
(identifier) @variable
((sort_statement
  (keyword)
  .
  (_)
  .
  (qualified_name (identifier) @keyword))
  (#match? @keyword "(?i)^STABLE$")
  (#set! priority 110))
((sort_statement
  (keyword) @keyword
  .
  (keyword) @variable)
  (#match? @keyword "(?i)^BY$")
  (#match? @variable "(?i)^FIELD$")
  (#set! priority 110))
((_
  (keyword) @_type_keyword
  .
  (qualified_name (identifier) @type.builtin))
  (#match? @_type_keyword "(?i)^TYPE$")
  (#match? @type.builtin "(?i)^(c|n|d|t|i|int[1248]|f|p|decfloat(16|34)?|string|x|xstring|utclong|data|any|simple|numeric|clike|csequence|xsequence|object|char[0-9]+)$")
  (#set! priority 110))
((_
  (keyword) @_type_keyword
  .
  (keyword) @_table_keyword
  .
  (keyword) @_of_keyword
  .
  (qualified_name (identifier) @type.builtin))
  (#match? @_type_keyword "(?i)^TYPE$")
  (#match? @_table_keyword "(?i)^TABLE$")
  (#match? @_of_keyword "(?i)^OF$")
  (#match? @type.builtin "(?i)^(c|n|d|t|i|int[1248]|f|p|decfloat(16|34)?|string|x|xstring|utclong|data|any|simple|numeric|clike|csequence|xsequence|object|char[0-9]+)$")
  (#set! priority 110))
((_
  (keyword) @_type_keyword
  .
  (keyword) @_table_kind
  .
  (keyword) @_table_keyword
  .
  (keyword) @_of_keyword
  .
  (qualified_name (identifier) @type.builtin))
  (#match? @_type_keyword "(?i)^TYPE$")
  (#match? @_table_kind "(?i)^(ANY|INDEX|STANDARD|SORTED|HASHED)$")
  (#match? @_table_keyword "(?i)^TABLE$")
  (#match? @_of_keyword "(?i)^OF$")
  (#match? @type.builtin "(?i)^(c|n|d|t|i|int[1248]|f|p|decfloat(16|34)?|string|x|xstring|utclong|data|any|simple|numeric|clike|csequence|xsequence|object|char[0-9]+)$")
  (#set! priority 110))
((constructor_expression
  type: (component_name (identifier) @type.builtin))
  (#match? @type.builtin "(?i)^(c|n|d|t|i|int[1248]|f|p|decfloat(16|34)?|string|x|xstring|utclong|data|any|simple|numeric|clike|csequence|xsequence|object|char[0-9]+)$")
  (#set! priority 110))
((constructor_expression
  type: (qualified_name (identifier) @type.builtin))
  (#match? @type.builtin "(?i)^(c|n|d|t|i|int[1248]|f|p|decfloat(16|34)?|string|x|xstring|utclong|data|any|simple|numeric|clike|csequence|xsequence|object|char[0-9]+)$")
  (#set! priority 110))
(escaped_identifier) @variable
(wildcard) @operator

(operator) @operator
(punctuation) @punctuation.delimiter
(template_punctuation) @punctuation.delimiter

[
  "->"
  "=>"
  "~"
  "-"
  "+"
  "*"
  "/"
  "="
  "?="
  "<="
  ">="
  "<>"
  "<"
  ">"
  "&&"
  "@"
  "#"
] @operator

[
  "("
  ")"
  "["
  "]"
] @punctuation.bracket

[
  "."
  ","
  ":"
] @punctuation.delimiter
