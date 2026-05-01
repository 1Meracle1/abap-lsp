# Object-Oriented ABAP

Object-oriented ABAP defines classes, interfaces, methods, attributes, events,
and inheritance. Local classes can live in report includes; global classes are
repository objects.

`abap-lsp` support: class/interface declarations, deferred/load statements,
visibility sections, inheritance clauses, implementation blocks, method bodies,
`METHODS`, `CLASS-METHODS`, method parameters, raising clauses, events,
interfaces, aliases, `CREATE OBJECT`, `NEW`, static and instance calls, event
handler signatures, and AMDP SQLScript bodies as opaque islands are supported
in common forms.

## Class Definition And Implementation

```abap
CLASS lcl_counter DEFINITION FINAL.
  PUBLIC SECTION.
    " Constructor initializes each instance.
    METHODS constructor
      IMPORTING iv_start TYPE i DEFAULT 0.

    " Instance methods read or change instance attributes.
    METHODS next
      RETURNING VALUE(rv_value) TYPE i.

    " Static methods are called on the class, not an object instance.
    CLASS-METHODS describe
      RETURNING VALUE(rv_text) TYPE string.

  PRIVATE SECTION.
    DATA mv_value TYPE i.
ENDCLASS.

CLASS lcl_counter IMPLEMENTATION.
  METHOD constructor.
    mv_value = iv_start.
  ENDMETHOD.

  METHOD next.
    mv_value = mv_value + 1.
    rv_value = mv_value.
  ENDMETHOD.

  METHOD describe.
    rv_text = 'simple counter'.
  ENDMETHOD.
ENDCLASS.
```

Semantics:

- `DEFINITION` declares the public/protected/private shape,
- `IMPLEMENTATION` contains method bodies,
- `PUBLIC SECTION` members are visible to callers,
- `PROTECTED SECTION` members are visible to subclasses,
- `PRIVATE SECTION` members are visible only inside the class.

## Creating Objects And Calling Methods

```abap
" Modern constructor expression. The inferred type comes from the target.
DATA(lo_counter) = NEW lcl_counter( iv_start = 10 ).

" Instance method call through an object reference.
DATA(lv_next) = lo_counter->next( ).

" Static method call through the class.
DATA(lv_description) = lcl_counter=>describe( ).

" Classic object creation is still common in older code.
DATA lo_old_counter TYPE REF TO lcl_counter.
CREATE OBJECT lo_old_counter
  EXPORTING
    iv_start = 5.
```

Semantics:

- `->` dereferences an object reference for instance members,
- `=>` accesses static class members,
- method actual parameters can be named (`iv_start = 10`) or grouped under
  `EXPORTING`, `IMPORTING`, `CHANGING`, and `RECEIVING`,
- `RETURNING VALUE(rv_result)` maps to expression-style method calls.

## Interfaces, Aliases, And Inheritance

```abap
INTERFACE lif_status_provider.
  METHODS get_status
    RETURNING VALUE(rv_status) TYPE c.
ENDINTERFACE.

CLASS lcl_order DEFINITION INHERITING FROM lcl_base.
  PUBLIC SECTION.
    INTERFACES lif_status_provider.

    " ALIASES provides a shorter local name for an interface component.
    ALIASES get_status FOR lif_status_provider~get_status.
ENDCLASS.

CLASS lcl_order IMPLEMENTATION.
  METHOD lif_status_provider~get_status.
    rv_status = 'O'.
  ENDMETHOD.
ENDCLASS.
```

Semantics:

- interfaces define required public behavior without instance data,
- implementing classes provide methods named `interface~method`,
- `ALIASES` exposes an interface component through a class-local name,
- `INHERITING FROM` creates an inheritance relationship with a superclass.

## Events And Handlers

```abap
CLASS lcl_sender DEFINITION.
  PUBLIC SECTION.
    EVENTS changed EXPORTING VALUE(iv_id) TYPE string.
    METHODS save.
ENDCLASS.

CLASS lcl_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS on_changed
      FOR EVENT changed OF lcl_sender
      IMPORTING iv_id.
ENDCLASS.

CLASS lcl_sender IMPLEMENTATION.
  METHOD save.
    " RAISE EVENT notifies registered handlers.
    RAISE EVENT changed EXPORTING iv_id = '4711'.
  ENDMETHOD.
ENDCLASS.

SET HANDLER lo_receiver->on_changed FOR lo_sender.
```

Semantics:

- event declarations define the event name and exported payload,
- handler methods declare `FOR EVENT ... OF ...`,
- `SET HANDLER` registers handler methods for a sender or globally,
- event delivery is runtime behavior; static analysis can record names but not
  every dynamic registration target.

## AMDP Bodies

```abap
CLASS lcl_amdp DEFINITION.
  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.
    CLASS-METHODS get_rows
      FOR TABLE FUNCTION ztf_demo.
ENDCLASS.

CLASS lcl_amdp IMPLEMENTATION.
  METHOD get_rows
    BY DATABASE FUNCTION FOR HDB
    LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY
    USING ztable.

    -- SQLScript is not ABAP. The ABAP parser preserves this body as an island.
    RETURN SELECT key, value FROM ztable;
  ENDMETHOD.
ENDCLASS.
```

Semantics:

- AMDP method headers are ABAP, but the body language is SQLScript,
- dependencies after `USING` name database objects used by the procedure or
  function,
- ABAP tools should not parse SQLScript statements as ABAP statements.

