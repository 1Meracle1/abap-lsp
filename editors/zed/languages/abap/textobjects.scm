(comment)+ @comment.around

(class_definition) @class.around
(class_implementation) @class.around
(interface_definition) @class.around

(method_definition) @function.around
(method_signature) @function.around
(form_definition) @function.around
(function_definition) @function.around
(module_definition) @function.around
(event_block) @function.around
(macro_definition) @function.around

(class_definition
  body: (_) @class.inside)

(class_implementation
  body: (_) @class.inside)

(interface_definition
  body: (_) @class.inside)

(method_definition
  body: (_) @function.inside)

(form_definition
  body: (_) @function.inside)

(function_definition
  body: (_) @function.inside)

(module_definition
  body: (_) @function.inside)

(macro_definition
  body: (_) @function.inside)
