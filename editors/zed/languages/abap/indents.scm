((if_statement (keyword) @end) @indent
 (#eq? @end "ENDIF"))

((case_statement (keyword) @end) @indent
 (#eq? @end "ENDCASE"))

((while_statement (keyword) @end) @indent
 (#eq? @end "ENDWHILE"))

((do_statement (keyword) @end) @indent
 (#eq? @end "ENDDO"))

((loop_statement (keyword) @end) @indent
 (#eq? @end "ENDLOOP"))

((try_statement (keyword) @end) @indent
 (#eq? @end "ENDTRY"))

((class_definition (keyword) @end) @indent
 (#eq? @end "ENDCLASS"))

((class_implementation (keyword) @end) @indent
 (#eq? @end "ENDCLASS"))

((interface_definition (keyword) @end) @indent
 (#eq? @end "ENDINTERFACE"))

((method_definition (keyword) @end) @indent
 (#eq? @end "ENDMETHOD"))

((form_definition (keyword) @end) @indent
 (#eq? @end "ENDFORM"))

((function_definition (keyword) @end) @indent
 (#eq? @end "ENDFUNCTION"))

((module_definition (keyword) @end) @indent
 (#eq? @end "ENDMODULE"))

((macro_definition (keyword) @end) @indent
 (#eq? @end "END-OF-DEFINITION"))
