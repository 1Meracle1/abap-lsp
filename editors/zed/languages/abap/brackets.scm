("(" @open ")" @close)
("[" @open "]" @close)

(string_template
  (template_start) @open
  (template_end) @close)

(template_interpolation
  (template_interpolation_start) @open
  (template_interpolation_end) @close)
