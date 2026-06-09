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
(field_symbol) @variable.special
(cte_identifier) @variable.special
(identifier) @variable
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
  type: (identifier) @type.builtin)
  (#match? @type.builtin "(?i)^(c|n|d|t|i|int[1248]|f|p|decfloat(16|34)?|string|x|xstring|utclong|data|any|simple|numeric|clike|csequence|xsequence|object|char[0-9]+)$")
  (#set! priority 110))
((constructor_expression
  type: (qualified_name (identifier) @type.builtin))
  (#match? @type.builtin "(?i)^(c|n|d|t|i|int[1248]|f|p|decfloat(16|34)?|string|x|xstring|utclong|data|any|simple|numeric|clike|csequence|xsequence|object|char[0-9]+)$")
  (#set! priority 110))
(escaped_identifier) @variable
(operator) @operator
(punctuation) @punctuation.delimiter
