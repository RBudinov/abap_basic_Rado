CLASS zcl_rbv_abap_course_basics1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
    INTERFACES zif_abap_course_basics .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rbv_abap_course_basics1 IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lo_course_basics) = NEW zcl_rbv_abap_course_basics1( ).

    "Hello world
    DATA lv_result TYPE string.
    lv_result = lo_course_basics->zif_abap_course_basics~hello_world(
    iv_name = 'Radoslav Budinov'
    ).
    out->write( lv_result ).

    "Calculator
    DATA lv_result_calc TYPE i.
    lv_result_calc = lo_course_basics->zif_abap_course_basics~calculator(
    iv_first_number = 5
    Iv_second_number = 6
    iv_operator = '*'
    ).
    out->write( lv_result_calc ).

    "FizzBuzz
    DATA lv_result_fb TYPE string.
    lv_result_fb = lo_course_basics->zif_abap_course_basics~fizz_buzz( ).
    out->write( lv_result_fb ).

    "dateparse
    DATA: lv_date TYPE string.
    lv_date = lo_course_basics->zif_abap_course_basics~date_parsing(
    iv_date = '22 June 1987'
    ).
    out->write( lv_date ).

    "scrabble
    DATA lv_result_scrab TYPE i.
    lv_result_scrab = lo_course_basics->zif_abap_course_basics~scrabble_score(
    iv_word = 'Myzraela'
    ).
    out->write( lv_result_scrab ).

    "Timestamp
    DATA lv_result_timestmp TYPE timestampl.
    lv_result_timestmp = lo_course_basics->zif_abap_course_basics~get_current_date_time(  ).
    out->write( lv_result_timestmp ).
    out->write( cl_abap_char_utilities=>newline ).
    "Internal tables
    DATA: lt_task7_1 TYPE zif_abap_course_basics=>ltty_travel_id,
          lt_task7_2 TYPE zif_abap_course_basics=>ltty_travel_id,
          lt_task7_3 TYPE zif_abap_course_basics=>ltty_travel_id.

    zif_abap_course_basics~internal_tables(
      IMPORTING
        et_travel_ids_task7_1 = lt_task7_1
        et_travel_ids_task7_2 = lt_task7_2
        et_travel_ids_task7_3 = lt_task7_3
    ).


    LOOP AT lt_task7_1 INTO DATA(ls_travel).
      out->write( |ID1: { ls_travel-travel_id }| ).
    ENDLOOP.
    out->write( cl_abap_char_utilities=>newline ).

    LOOP AT lt_task7_2 INTO DATA(ls_travel2).
      out->write( |ID2: { ls_travel2-travel_id }| ).
    ENDLOOP.
    out->write( cl_abap_char_utilities=>newline ).

    LOOP AT lt_task7_3 INTO DATA(ls_travel3).
      out->write( |ID3: { ls_travel3-travel_id }| ).
    ENDLOOP.
    out->write( cl_abap_char_utilities=>newline ).


    "Open SQL task
    DATA: lt_task8_1 TYPE zif_abap_course_basics=>ltty_travel_id,
          lt_task8_2 TYPE zif_abap_course_basics=>ltty_travel_id,
          lt_task8_3 TYPE zif_abap_course_basics=>ltty_travel_id.

    out->write( cl_abap_char_utilities=>newline ).


    zif_abap_course_basics~open_sql(
      IMPORTING
        et_travel_ids_task8_1 = lt_task8_1
        et_travel_ids_task8_2 = lt_task8_2
        et_travel_ids_task8_3 = lt_task8_3
    ).

    out->write( cl_abap_char_utilities=>newline ).

    LOOP AT lt_task8_1 INTO DATA(ls_travelb).
      out->write( |ID1: { ls_travelb-travel_id }| ).
    ENDLOOP.

    out->write( cl_abap_char_utilities=>newline ).

    LOOP AT lt_task8_2 INTO DATA(ls_travel2b).
      out->write( |ID2: { ls_travel2b-travel_id }| ).
    ENDLOOP.

    out->write( cl_abap_char_utilities=>newline ).

    LOOP AT lt_task8_3 INTO DATA(ls_travel3b).
      out->write( |ID3: { ls_travel3b-travel_id }| ).
    ENDLOOP.

  ENDMETHOD.



  METHOD zif_abap_course_basics~calculator.
    "iv_first_number  TYPE i
    "           iv_second_number TYPE i
    "          iv_operator      TYPE char1
    CASE iv_operator.
      WHEN '+'.
        rv_result = iv_first_number + iv_second_number.
      WHEN '-'.
        rv_result = iv_first_number - iv_second_number.
      WHEN '*'.
        rv_result = iv_first_number * iv_second_number.
      WHEN '/'.
        IF iv_second_number = 0.
*    RAISE EXCEPTION NEW zcx_my_exceptions( textid =  ).
        ELSE.
          rv_result = iv_first_number / iv_second_number.
        ENDIF.
      WHEN OTHERS.
    ENDCASE.
  ENDMETHOD.


  METHOD zif_abap_course_basics~date_parsing.
* iv_date
* rv_result

    DATA: lt_parts      TYPE STANDARD TABLE OF string WITH EMPTY KEY,
          lv_day_str    TYPE string,
          lv_year_str   TYPE string,
          lv_day        TYPE i,
          lv_month_text TYPE string,
          lv_month      TYPE i,
          lv_year       TYPE i,
          lv_date       TYPE d,
          lv_result     TYPE string.

    SPLIT iv_date AT space INTO TABLE lt_parts.
    READ TABLE lt_parts INDEX 1 INTO lv_day_str.
    READ TABLE lt_parts INDEX 2 INTO lv_month_text.
    READ TABLE lt_parts INDEX 3 INTO lv_year_str.

    lv_day  = CONV i( lv_day_str ).
    lv_year = CONV i( lv_year_str ).

    IF lv_month_text CO '0123456789'.
      lv_month = CONV i( lv_month_text ).
    ELSE.
      CASE to_upper( lv_month_text ).
        WHEN 'JANUARY'.   lv_month = 1.
        WHEN 'FEBRUARY'.  lv_month = 2.
        WHEN 'MARCH'.     lv_month = 3.
        WHEN 'APRIL'.     lv_month = 4.
        WHEN 'MAY'.       lv_month = 5.
        WHEN 'JUNE'.      lv_month = 6.
        WHEN 'JULY'.      lv_month = 7.
        WHEN 'AUGUST'.    lv_month = 8.
        WHEN 'SEPTEMBER'. lv_month = 9.
        WHEN 'OCTOBER'.   lv_month = 10.
        WHEN 'NOVEMBER'.  lv_month = 11.
        WHEN 'DECEMBER'.  lv_month = 12.
        WHEN OTHERS.
      ENDCASE.
    ENDIF.


    lv_result = |{ lv_year }{ lv_month }{ lv_day }|.

    rv_result = CONV d( lv_result ).

  ENDMETHOD.


  METHOD zif_abap_course_basics~fizz_buzz.
    DATA: lv_result TYPE string,
          lv_line   TYPE string,
          lv_index  TYPE i.
    DO 100 TIMES.
      lv_index = sy-index.
      CLEAR lv_line.

      IF lv_index MOD 15 = 0.
        lv_line = 'FizzBuzz'.
      ELSEIF lv_index MOD 3 = 0.
        lv_line = 'Fizz'.
      ELSEIF lv_index MOD 5 = 0.
        lv_line = 'Buzz'.
      ELSE.
        lv_line = lv_index.
      ENDIF.

      IF lv_result IS INITIAL.
        lv_result = lv_line.
      ELSE.
        CONCATENATE lv_result lv_line INTO lv_result SEPARATED BY cl_abap_char_utilities=>newline.
      ENDIF.
    ENDDO.

    rv_result = lv_result.
  ENDMETHOD.


  METHOD zif_abap_course_basics~get_current_date_time.
    GET TIME STAMP FIELD DATA(lv_timestamp).
    rv_result = lv_timestamp.
  ENDMETHOD.


  METHOD zif_abap_course_basics~hello_world.
    " iv_name
    " rv_result
    DATA message TYPE string.
    message = |Hello { iv_name }, your system user is { sy-uname }.|.
    "  rv_result = message.
  ENDMETHOD.


  METHOD zif_abap_course_basics~internal_tables.
  SELECT SINGLE @abap_true FROM ZTRAVEL_RDB INTO @DATA(lv_isnot_empty).
    IF lv_isnot_empty <> abap_true.
      SELECT * FROM ZTRAVEL_RDB INTO TABLE @DATA(lt_ztravel).
      DELETE ZTRAVEL_RDB FROM TABLE @lt_ztravel.
      COMMIT WORK AND WAIT.

      INSERT ZTRAVEL_RDB FROM (
        SELECT FROM /dmo/travel
        FIELDS uuid( ) AS travel_uuid,
               travel_id AS travel_id,
               agency_id AS agency_id,
               customer_id AS customer_id,
               begin_date AS begin_date,
               end_date AS end_date,
               booking_fee AS booking_fee,
               total_price AS total_price,
               currency_code AS currency_code,
               description AS description,
               CASE status
                 WHEN 'B' THEN 'A'
                 WHEN 'X' THEN 'X'
                 ELSE 'O'
               END AS overall_status,
               createdby AS createdby,
               createdat AS createdat,
               lastchangedby AS last_changed_by,
               lastchangedat AS last_changed_at
        ORDER BY travel_id
      ).
      COMMIT WORK AND WAIT.
    ELSE.

    ENDIF.

    SELECT * FROM ZTRAVEL_RDB INTO TABLE @DATA(lt_travels).

    " Task 7.1
    et_travel_ids_task7_1 = VALUE #(
      FOR travel IN lt_travels
      WHERE ( agency_id = '070001' AND
              booking_fee = '20' AND
              currency_code = 'JPY' )
      ( travel_id = travel-travel_id )
    ).

    " Task 7.2
    DATA: lc_usd_to_eur TYPE decfloat16 VALUE '1.1',
               lc_jpy_to_usd TYPE decfloat16 VALUE '140',
               lc_threshold  TYPE decfloat16 VALUE '2000'.

    CLEAR et_travel_ids_task7_2.

    LOOP AT lt_travels INTO DATA(ls_travel).
      DATA(lv_converted_price) = ls_travel-total_price.

      IF ls_travel-currency_code = 'EUR'.
        lv_converted_price = ls_travel-total_price * lc_usd_to_eur.
      ELSEIF ls_travel-currency_code = 'JPY'.
        lv_converted_price = ls_travel-total_price / lc_jpy_to_usd.
      ENDIF.

      IF lv_converted_price > lc_threshold.
        APPEND VALUE #( travel_id = ls_travel-travel_id ) TO et_travel_ids_task7_2.
      ENDIF.
    ENDLOOP.

    " Task 7.3
    DELETE lt_travels WHERE currency_code <> 'EUR'.
    SORT lt_travels BY total_price begin_date.

    et_travel_ids_task7_3 = VALUE #(
      FOR i = 1 WHILE i <= 10 AND i <= lines( lt_travels )
      ( travel_id = lt_travels[ i ]-travel_id )
    ).

  ENDMETHOD.


  METHOD zif_abap_course_basics~open_sql.
  CONSTANTS:
      lc_usd_to_eur TYPE p LENGTH 8 DECIMALS 4 VALUE '1.1000',
      lc_jpy_to_usd TYPE p LENGTH 8 DECIMALS 4 VALUE '140.0000',
      lc_threshold  TYPE p LENGTH 8 DECIMALS 2 VALUE '2000.00'.

    " Task 8.1
    SELECT travel_id
      FROM ZTRAVEL_RDB
      WHERE agency_id = '070001'
        AND booking_fee = '20'
        AND currency_code = 'JPY'
      INTO TABLE @et_travel_ids_task8_1.

    " Task 8.2
    SELECT travel_id FROM ZTRAVEL_RDB
      WHERE currency_code = 'USD'
        AND total_price > @lc_threshold
      INTO TABLE @et_travel_ids_task8_2.

    SELECT travel_id FROM ZTRAVEL_RDB
    WHERE currency_code = 'EUR'
      AND ( CAST( total_price AS DEC( 15,4 ) ) * @lc_usd_to_eur ) > @lc_threshold
    APPENDING TABLE @et_travel_ids_task8_2.

    SELECT travel_id FROM ZTRAVEL_RDB
      WHERE currency_code = 'JPY'
        AND total_price > ( @lc_threshold * @lc_jpy_to_usd )
      APPENDING TABLE @et_travel_ids_task8_2.

    " Task 8.3
    SELECT travel_id
      FROM ZTRAVEL_RDB
      WHERE currency_code = 'EUR'
      ORDER BY total_price, begin_date
      INTO TABLE @et_travel_ids_task8_3
      UP TO 10 ROWS.
  ENDMETHOD.


  METHOD zif_abap_course_basics~scrabble_score.
   DATA: lv_char         TYPE c LENGTH 1,
        lv_total_score  TYPE i VALUE 0,
        lv_offset       TYPE i,
        lv_index        TYPE i,
        lv_alphabet     TYPE string VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.

  DO strlen( iv_word ) TIMES.
    lv_offset = sy-index - 1.
    lv_char = iv_word+lv_offset(1).
    TRANSLATE lv_char TO UPPER CASE.


    FIND lv_char IN lv_alphabet MATCH OFFSET lv_index.
    IF sy-subrc = 0.
      lv_total_score = lv_total_score + lv_index + 1.
    ENDIF.
  ENDDO.

  rv_result = lv_total_score.

ENDMETHOD.


ENDCLASS.
