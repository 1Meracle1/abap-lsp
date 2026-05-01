# Open SQL And Database Access

Open SQL is ABAP's database access language. It is embedded in ABAP source and
uses ABAP host variables for values supplied by the program.

`abap-lsp` support: structured `SELECT`, `SELECT SINGLE`, projection lists,
`FROM`, aliases, joins, `WHERE`, `GROUP BY`, `HAVING`, `ORDER BY`, `FOR ALL
ENTRIES`, `INTO`, `APPENDING`, `DISTINCT`, `UP TO`, `PACKAGE SIZE`, `OFFSET`,
`BYPASSING BUFFER`, `CONNECTION`, host expressions, dynamic `WHERE`, CTEs,
`UNION`, cursors, database `INSERT`, `UPDATE`, and `DELETE` are parsed in common
forms. SQL facts are exported conservatively.

## SELECT SINGLE And Host Variables

```abap
DATA lv_status TYPE c.
DATA lv_order_id TYPE string VALUE '4711'.

" Modern Open SQL marks ABAP variables with @.
" SELECT SINGLE expects at most one row. Without ORDER BY, the chosen row can be
" database-dependent when the WHERE clause is not unique.
SELECT SINGLE status
  FROM zorders
  INTO @lv_status
  WHERE order_id = @lv_order_id.

IF sy-subrc = 0.
  WRITE / lv_status.
ENDIF.
```

Semantics:

- `@lv_order_id` is an ABAP host variable, not a database column,
- `sy-subrc = 0` means a row was read,
- `sy-subrc <> 0` means no row was found for ordinary `SELECT SINGLE`,
- always make the `WHERE` clause key-like when exactly one row is intended.

## Selecting Into Internal Tables

```abap
" INTO TABLE replaces the target table content with the result set.
SELECT order_id, amount, status
  FROM zorders
  INTO TABLE @DATA(lt_orders)
  WHERE status = @gc_status_open
  ORDER BY order_id.

" APPENDING TABLE keeps existing rows and appends new result rows.
SELECT order_id, amount, status
  FROM zorders_archive
  APPENDING TABLE @lt_orders
  WHERE status = @gc_status_open.
```

Semantics:

- `INTO TABLE` is a bulk fetch,
- `APPENDING TABLE` is a bulk append,
- `INTO CORRESPONDING FIELDS OF TABLE` maps result columns by name,
- use explicit projections instead of `SELECT *` unless the code intentionally
  needs the complete DDIC row.

## Joins, Aggregates, And Grouping

```abap
SELECT h~order_id,
       h~customer_id,
       SUM( i~amount ) AS total_amount
  FROM zorder_head AS h
  INNER JOIN zorder_item AS i
    ON i~order_id = h~order_id
  INTO TABLE @DATA(lt_totals)
  WHERE h~status = @gc_status_open
  GROUP BY h~order_id, h~customer_id
  HAVING SUM( i~amount ) > 0
  ORDER BY h~order_id.
```

Semantics:

- `alias~column` qualifies a column with a table alias,
- aggregate columns require grouping for non-aggregated projected columns,
- `WHERE` filters rows before grouping,
- `HAVING` filters grouped results after aggregation.

## FOR ALL ENTRIES

```abap
" FOR ALL ENTRIES uses rows of an ABAP table as a predicate source.
" Always guard against an empty driving table; otherwise many ABAP releases
" treat the condition as absent and can read the whole database table.
IF lt_keys IS NOT INITIAL.
  SELECT order_id, amount
    FROM zorders
    INTO TABLE @DATA(lt_found)
    FOR ALL ENTRIES IN @lt_keys
    WHERE order_id = @lt_keys-order_id.
ENDIF.
```

Semantics:

- `FOR ALL ENTRIES` is a set-like expansion from an internal table,
- duplicate key rows can duplicate work, so normalize the driving table first
  for large queries,
- an empty driving table is a classic production-risk pattern.

## Common Table Expressions And Set Operators

```abap
" +open_orders is a local SQL common table expression.
" It exists only for this Open SQL statement.
WITH +open_orders AS (
  SELECT order_id, customer_id
    FROM zorders
    WHERE status = @gc_status_open
)
SELECT customer_id
  FROM +open_orders
  INTO TABLE @DATA(lt_customers)
  ORDER BY customer_id.

" UNION combines result sets with compatible projections.
SELECT order_id FROM zorders
UNION ALL
SELECT order_id FROM zorders_archive
INTO TABLE @DATA(lt_all_ids).
```

Semantics:

- CTE names in ABAP Open SQL commonly start with `+`,
- CTEs improve readability when a query has staged logic,
- `UNION ALL` preserves duplicates; `UNION` removes duplicates.

## Cursors And Package Processing

```abap
DATA lv_cursor TYPE cursor.

OPEN CURSOR WITH HOLD @lv_cursor FOR
  SELECT order_id, amount
    FROM zorders
    WHERE status = @gc_status_open.

DO.
  FETCH NEXT CURSOR @lv_cursor
    INTO TABLE @DATA(lt_package)
    PACKAGE SIZE 100.

  IF sy-subrc <> 0.
    EXIT.
  ENDIF.

  PERFORM process_package TABLES lt_package.
ENDDO.

CLOSE CURSOR @lv_cursor.
```

Semantics:

- cursors let code process large result sets in packages,
- `WITH HOLD` keeps the cursor over commits where supported,
- always close cursors once processing is complete.

## Database Modification And Native SQL

```abap
" Open SQL UPDATE modifies database rows. The WHERE clause should be explicit.
UPDATE zorders
  SET status = @gc_status_closed
  WHERE order_id = @lv_order_id.

IF sy-subrc = 0.
  COMMIT WORK.
ELSE.
  ROLLBACK WORK.
ENDIF.

EXEC SQL.
  SELECT COUNT(*) INTO :lv_count FROM zorders
ENDEXEC.
```

Semantics:

- Open SQL `INSERT`, `UPDATE`, and `DELETE` affect database state,
- `sy-subrc` and `sy-dbcnt` report operation outcomes for many database
  statements,
- `COMMIT WORK` and `ROLLBACK WORK` control the logical unit of work,
- `EXEC SQL` is native SQL and database-specific; this project preserves it as
  an opaque island instead of parsing its body as ABAP.

