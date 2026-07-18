CLASS zcl_demo_sqlscript_lab DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_amdp_marker_hdb.

    TYPES:
      BEGIN OF ty_flight_result,
        carrid                 TYPE scarr-carrid,
        carrname               TYPE scarr-carrname,
        connid                 TYPE spfli-connid,
        fldate                 TYPE sflight-fldate,
        calendar_year          TYPE i,
        calendar_month         TYPE i,
        cityfrom               TYPE spfli-cityfrom,
        cityto                 TYPE spfli-cityto,
        route_key              TYPE c LENGTH 64,
        price                  TYPE sflight-price,
        currency               TYPE sflight-currency,
        seatsmax               TYPE sflight-seatsmax,
        seatsocc               TYPE sflight-seatsocc,
        free_seats             TYPE i,
        occupancy_pct          TYPE p LENGTH 8 DECIMALS 2,
        carrier_avg_pct        TYPE p LENGTH 8 DECIMALS 2,
        previous_occupancy_pct TYPE p LENGTH 8 DECIMALS 2,
        next_flight_date       TYPE sflight-fldate,
        estimated_revenue      TYPE p LENGTH 16 DECIMALS 2,
        running_revenue        TYPE p LENGTH 16 DECIMALS 2,
        route_flight_no        TYPE int8,
        carrier_dense_rank     TYPE int8,
        load_band              TYPE c LENGTH 10,
      END OF ty_flight_result,
      tt_flight_result TYPE STANDARD TABLE OF ty_flight_result
        WITH EMPTY KEY,

      BEGIN OF ty_carrier_summary,
        carrid            TYPE scarr-carrid,
        carrname          TYPE scarr-carrname,
        currency          TYPE sflight-currency,
        flight_count      TYPE int8,
        route_count       TYPE int8,
        avg_occupancy_pct TYPE p LENGTH 8 DECIMALS 2,
        minimum_price     TYPE sflight-price,
        maximum_price     TYPE sflight-price,
        total_revenue     TYPE p LENGTH 16 DECIMALS 2,
        top_route         TYPE c LENGTH 64,
        top_route_flights TYPE int8,
      END OF ty_carrier_summary,
      tt_carrier_summary TYPE STANDARD TABLE OF ty_carrier_summary
        WITH EMPTY KEY,

      BEGIN OF ty_rollup,
        carrid            TYPE scarr-carrid,
        currency          TYPE sflight-currency,
        aggregation_level TYPE c LENGTH 12,
        flight_count      TYPE int8,
        avg_occupancy_pct TYPE p LENGTH 8 DECIMALS 2,
        total_revenue     TYPE p LENGTH 16 DECIMALS 2,
      END OF ty_rollup,
      tt_rollup TYPE STANDARD TABLE OF ty_rollup
        WITH EMPTY KEY,

      BEGIN OF ty_carrid,
        carrid TYPE scarr-carrid,
      END OF ty_carrid,
      tt_carrid TYPE STANDARD TABLE OF ty_carrid
        WITH EMPTY KEY,

      BEGIN OF ty_schedule_line,
        period_no          TYPE i,
        opening_balance    TYPE p LENGTH 16 DECIMALS 2,
        interest_amount    TYPE p LENGTH 16 DECIMALS 2,
        closing_balance    TYPE p LENGTH 16 DECIMALS 2,
        cumulative_interest TYPE p LENGTH 16 DECIMALS 2,
        balance_band       TYPE c LENGTH 10,
      END OF ty_schedule_line,
      tt_schedule TYPE STANDARD TABLE OF ty_schedule_line
        WITH EMPTY KEY,

      BEGIN OF ty_message,
        message_type TYPE c LENGTH 1,
        error_code   TYPE i,
        message_text TYPE c LENGTH 200,
      END OF ty_message,
      tt_message TYPE STANDARD TABLE OF ty_message
        WITH EMPTY KEY.

    CLASS-METHODS analyze_flights
      IMPORTING
        VALUE(iv_carrid)       TYPE scarr-carrid
        VALUE(iv_date_from)    TYPE sflight-fldate
        VALUE(iv_date_to)      TYPE sflight-fldate
        VALUE(iv_min_occupancy) TYPE p LENGTH 8 DECIMALS 2
      EXPORTING
        VALUE(et_flights)      TYPE tt_flight_result
        VALUE(et_summary)      TYPE tt_carrier_summary
        VALUE(et_rollup)       TYPE tt_rollup
        VALUE(ev_row_count)    TYPE i.

    CLASS-METHODS apply_trusted_filter
      IMPORTING
        VALUE(it_flights) TYPE tt_flight_result
        VALUE(iv_filter)  TYPE c LENGTH 500
      EXPORTING
        VALUE(et_flights) TYPE tt_flight_result.

    CLASS-METHODS compare_carrier_sets
      IMPORTING
        VALUE(it_left)         TYPE tt_carrid
        VALUE(it_right)        TYPE tt_carrid
      EXPORTING
        VALUE(et_union)        TYPE tt_carrid
        VALUE(et_intersection) TYPE tt_carrid
        VALUE(et_left_only)    TYPE tt_carrid.

    CLASS-METHODS build_compound_schedule
      IMPORTING
        VALUE(iv_principal)   TYPE p LENGTH 16 DECIMALS 2
        VALUE(iv_annual_rate) TYPE p LENGTH 8 DECIMALS 4
        VALUE(iv_periods)     TYPE i
      EXPORTING
        VALUE(et_schedule)    TYPE tt_schedule
        VALUE(et_messages)    TYPE tt_message.
ENDCLASS.


CLASS zcl_demo_sqlscript_lab IMPLEMENTATION.
  METHOD analyze_flights
    BY DATABASE PROCEDURE FOR HDB
    LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY
    USING scarr spfli sflight.

    lt_enriched =
      WITH flight_source AS
      (
        SELECT
          f.carrid,
          c.carrname,
          f.connid,
          f.fldate,
          p.cityfrom,
          p.cityto,
          f.price,
          f.currency,
          f.seatsmax,
          f.seatsocc
        FROM sflight AS f
        INNER JOIN scarr AS c
          ON  c.mandt  = f.mandt
          AND c.carrid = f.carrid
        INNER JOIN spfli AS p
          ON  p.mandt  = f.mandt
          AND p.carrid = f.carrid
          AND p.connid = f.connid
        WHERE f.mandt = SESSION_CONTEXT( 'CLIENT' )
          AND ( TRIM( :iv_carrid ) = '' OR f.carrid = :iv_carrid )
          AND f.fldate BETWEEN :iv_date_from AND :iv_date_to
          AND EXISTS
          (
            SELECT 1
            FROM spfli AS px
            WHERE px.mandt  = f.mandt
              AND px.carrid = f.carrid
              AND px.connid = f.connid
          )
      )
      SELECT
        src.carrid,
        src.carrname,
        src.connid,
        src.fldate,
        YEAR( TO_DATE( src.fldate, 'YYYYMMDD' ) ) AS calendar_year,
        MONTH( TO_DATE( src.fldate, 'YYYYMMDD' ) ) AS calendar_month,
        src.cityfrom,
        src.cityto,
        CAST(
          UPPER( TRIM( src.cityfrom ) ) || '-' ||
          UPPER( TRIM( src.cityto ) )
          AS NVARCHAR(64)
        ) AS route_key,
        src.price,
        src.currency,
        src.seatsmax,
        src.seatsocc,
        CAST( src.seatsmax - src.seatsocc AS INTEGER ) AS free_seats,
        CAST(
          ROUND(
            COALESCE(
              100 * src.seatsocc / NULLIF( src.seatsmax, 0 ),
              0
            ),
            2
          ) AS DECIMAL(15,2)
        ) AS occupancy_pct,
        CAST(
          ROUND( src.price * src.seatsocc, 2 ) AS DECIMAL(31,2)
        ) AS estimated_revenue,
        CASE
          WHEN src.seatsmax = 0 THEN 'NO CAPACITY'
          WHEN 100 * src.seatsocc / NULLIF( src.seatsmax, 0 ) >= 90 THEN 'VERY HIGH'
          WHEN 100 * src.seatsocc / NULLIF( src.seatsmax, 0 ) >= 75 THEN 'HIGH'
          WHEN 100 * src.seatsocc / NULLIF( src.seatsmax, 0 ) >= 50 THEN 'MEDIUM'
          ELSE 'LOW'
        END AS load_band
      FROM flight_source AS src;

    lt_windowed =
      SELECT
        e.carrid,
        e.carrname,
        e.connid,
        e.fldate,
        e.calendar_year,
        e.calendar_month,
        e.cityfrom,
        e.cityto,
        e.route_key,
        e.price,
        e.currency,
        e.seatsmax,
        e.seatsocc,
        e.free_seats,
        e.occupancy_pct,
        CAST(
          ROUND(
            AVG( e.occupancy_pct ) OVER
            (
              PARTITION BY e.carrid
            ),
            2
          ) AS DECIMAL(15,2)
        ) AS carrier_avg_pct,
        CAST(
          COALESCE(
            LAG( e.occupancy_pct, 1 ) OVER
            (
              PARTITION BY e.carrid, e.connid
              ORDER BY e.fldate
            ),
            0
          ) AS DECIMAL(15,2)
        ) AS previous_occupancy_pct,
        LEAD( e.fldate, 1 ) OVER
        (
          PARTITION BY e.carrid, e.connid
          ORDER BY e.fldate
        ) AS next_flight_date,
        e.estimated_revenue,
        CAST(
          SUM( e.estimated_revenue ) OVER
          (
            PARTITION BY e.carrid, e.currency
            ORDER BY e.fldate, e.connid
            ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
          ) AS DECIMAL(31,2)
        ) AS running_revenue,
        ROW_NUMBER() OVER
        (
          PARTITION BY e.carrid, e.connid
          ORDER BY e.fldate
        ) AS route_flight_no,
        DENSE_RANK() OVER
        (
          PARTITION BY e.carrid
          ORDER BY e.occupancy_pct DESC
        ) AS carrier_dense_rank,
        e.load_band
      FROM :lt_enriched AS e;

    et_flights =
      SELECT
        carrid,
        carrname,
        connid,
        fldate,
        calendar_year,
        calendar_month,
        cityfrom,
        cityto,
        route_key,
        price,
        currency,
        seatsmax,
        seatsocc,
        free_seats,
        occupancy_pct,
        carrier_avg_pct,
        previous_occupancy_pct,
        next_flight_date,
        estimated_revenue,
        running_revenue,
        route_flight_no,
        carrier_dense_rank,
        load_band
      FROM :lt_windowed
      WHERE occupancy_pct >= :iv_min_occupancy
      ORDER BY carrid, fldate, connid;

    lt_route_totals =
      SELECT
        carrid,
        currency,
        route_key,
        COUNT(*) AS route_flights
      FROM :et_flights
      GROUP BY carrid, currency, route_key;

    lt_ranked_routes =
      SELECT
        carrid,
        currency,
        route_key,
        route_flights,
        ROW_NUMBER() OVER
        (
          PARTITION BY carrid, currency
          ORDER BY route_flights DESC, route_key
        ) AS route_rank
      FROM :lt_route_totals;

    lt_summary =
      SELECT
        carrid,
        MAX( carrname ) AS carrname,
        currency,
        COUNT(*) AS flight_count,
        COUNT( DISTINCT route_key ) AS route_count,
        CAST(
          ROUND( AVG( occupancy_pct ), 2 ) AS DECIMAL(15,2)
        ) AS avg_occupancy_pct,
        MIN( price ) AS minimum_price,
        MAX( price ) AS maximum_price,
        CAST(
          SUM( estimated_revenue ) AS DECIMAL(31,2)
        ) AS total_revenue
      FROM :et_flights
      GROUP BY carrid, currency
      HAVING COUNT(*) > 0;

    et_summary =
      SELECT
        s.carrid,
        s.carrname,
        s.currency,
        s.flight_count,
        s.route_count,
        s.avg_occupancy_pct,
        s.minimum_price,
        s.maximum_price,
        s.total_revenue,
        r.route_key AS top_route,
        r.route_flights AS top_route_flights
      FROM :lt_summary AS s
      LEFT OUTER JOIN :lt_ranked_routes AS r
        ON  r.carrid     = s.carrid
        AND r.currency   = s.currency
        AND r.route_rank = 1
      ORDER BY s.carrid, s.currency;

    et_rollup =
      SELECT
        CAST(
          CASE
            WHEN GROUPING( f.carrid ) = 1 THEN '*'
            ELSE f.carrid
          END AS NVARCHAR(3)
        ) AS carrid,
        CAST(
          CASE
            WHEN GROUPING( f.currency ) = 1 THEN ''
            ELSE f.currency
          END AS NVARCHAR(5)
        ) AS currency,
        CAST(
          CASE
            WHEN GROUPING( f.carrid ) = 1 THEN 'GRAND TOTAL'
            WHEN GROUPING( f.currency ) = 1 THEN 'CARRIER'
            ELSE 'CURRENCY'
          END AS NVARCHAR(12)
        ) AS aggregation_level,
        COUNT(*) AS flight_count,
        CAST(
          ROUND( AVG( f.occupancy_pct ), 2 ) AS DECIMAL(15,2)
        ) AS avg_occupancy_pct,
        CAST(
          SUM( f.estimated_revenue ) AS DECIMAL(31,2)
        ) AS total_revenue
      FROM :et_flights AS f
      GROUP BY GROUPING SETS
      (
        ( f.carrid, f.currency ),
        ( f.carrid ),
        ( )
      );

    SELECT COUNT(*) INTO ev_row_count
      FROM :et_flights;
  ENDMETHOD.


  METHOD apply_trusted_filter
    BY DATABASE PROCEDURE FOR HDB
    LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY.

    -- APPLY_FILTER is appropriate only for a trusted, validated expression.
    -- Example: OCCUPANCY_PCT >= 80 AND CURRENCY = 'USD'
    IF TRIM( :iv_filter ) = '' THEN
      et_flights =
        SELECT *
        FROM :it_flights;
    ELSE
      et_flights = APPLY_FILTER( :it_flights, :iv_filter );
    END IF;
  ENDMETHOD.


  METHOD compare_carrier_sets
    BY DATABASE PROCEDURE FOR HDB
    LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY.

    et_union =
      SELECT carrid FROM :it_left
      UNION
      SELECT carrid FROM :it_right;

    et_intersection =
      SELECT carrid FROM :it_left
      INTERSECT
      SELECT carrid FROM :it_right;

    et_left_only =
      SELECT carrid FROM :it_left
      EXCEPT
      SELECT carrid FROM :it_right;
  ENDMETHOD.


  METHOD build_compound_schedule
    BY DATABASE PROCEDURE FOR HDB
    LANGUAGE SQLSCRIPT
    OPTIONS READ-ONLY.

    DECLARE lv_period   INTEGER = 1;
    DECLARE lv_opening  DECIMAL(31,2) = :iv_principal;
    DECLARE lv_interest DECIMAL(31,2) = 0;
    DECLARE lv_closing  DECIMAL(31,2) = :iv_principal;

    DECLARE invalid_input CONDITION FOR SQL_ERROR_CODE 10001;

    DECLARE EXIT HANDLER FOR invalid_input
    BEGIN
      et_schedule =
        SELECT
          CAST( 0 AS INTEGER ) AS period_no,
          CAST( 0 AS DECIMAL(31,2) ) AS opening_balance,
          CAST( 0 AS DECIMAL(31,2) ) AS interest_amount,
          CAST( 0 AS DECIMAL(31,2) ) AS closing_balance,
          CAST( 0 AS DECIMAL(31,2) ) AS cumulative_interest,
          CAST( '' AS NVARCHAR(10) ) AS balance_band
        FROM dummy
        WHERE 1 = 0;

      et_messages =
        SELECT
          CAST( 'E' AS NVARCHAR(1) ) AS message_type,
          CAST( ::SQL_ERROR_CODE AS INTEGER ) AS error_code,
          CAST( ::SQL_ERROR_MESSAGE AS NVARCHAR(200) ) AS message_text
        FROM dummy;
    END;

    IF :iv_principal < 0 THEN
      SIGNAL invalid_input
        SET MESSAGE_TEXT = 'Principal must not be negative';
    ELSEIF :iv_annual_rate < 0 THEN
      SIGNAL invalid_input
        SET MESSAGE_TEXT = 'Annual rate must not be negative';
    ELSEIF :iv_periods < 0 OR :iv_periods > 1200 THEN
      SIGNAL invalid_input
        SET MESSAGE_TEXT = 'Periods must be between 0 and 1200';
    END IF;

    lt_schedule =
      SELECT
        CAST( 0 AS INTEGER ) AS period_no,
        CAST( 0 AS DECIMAL(31,2) ) AS opening_balance,
        CAST( 0 AS DECIMAL(31,2) ) AS interest_amount,
        CAST( 0 AS DECIMAL(31,2) ) AS closing_balance
      FROM dummy
      WHERE 1 = 0;

    WHILE :lv_period <= :iv_periods DO
      lv_interest =
        ROUND( :lv_opening * :iv_annual_rate / 1200, 2 );
      lv_closing =
        :lv_opening + :lv_interest;

      lt_schedule =
        SELECT
          period_no,
          opening_balance,
          interest_amount,
          closing_balance
        FROM :lt_schedule
        UNION ALL
        SELECT
          :lv_period AS period_no,
          :lv_opening AS opening_balance,
          :lv_interest AS interest_amount,
          :lv_closing AS closing_balance
        FROM dummy;

      lv_opening = :lv_closing;
      lv_period = :lv_period + 1;
    END WHILE;

    et_schedule =
      SELECT
        period_no,
        opening_balance,
        interest_amount,
        closing_balance,
        CAST(
          SUM( interest_amount ) OVER
          (
            ORDER BY period_no
            ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
          ) AS DECIMAL(31,2)
        ) AS cumulative_interest,
        CAST(
          CASE
            WHEN closing_balance >= 1000000 THEN 'MILLION+'
            WHEN closing_balance >= 100000 THEN 'SIX FIGURE'
            WHEN closing_balance >= 10000 THEN 'FIVE FIGURE'
            ELSE 'STANDARD'
          END AS NVARCHAR(10)
        ) AS balance_band
      FROM :lt_schedule
      ORDER BY period_no;

    et_messages =
      SELECT
        CAST( 'S' AS NVARCHAR(1) ) AS message_type,
        CAST( 0 AS INTEGER ) AS error_code,
        CAST( 'Schedule created' AS NVARCHAR(200) ) AS message_text
      FROM dummy;
  ENDMETHOD.
ENDCLASS.
