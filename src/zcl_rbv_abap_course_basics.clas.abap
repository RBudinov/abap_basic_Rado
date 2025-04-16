CLASS zcl_rbv_abap_course_basics DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun.
    INTERFACES zif_abap_course_basics .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rbv_abap_course_basics IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.
    DATA(lo_course_basics) = NEW zcl_rbv_abap_course_basics( ).


    DATA: lv_result TYPE string.
    lv_result = lo_course_basics->zif_abap_course_basics~hello_world(
     iv_name = 'Radoslav Budinov'
     ).
    out->write( lv_result ).


    DATA: lv_intres TYPE i.
    lv_intres = lo_course_basics->zif_abap_course_basics~calculator(
    iv_first_number = '100'
    iv_second_number = '3'
    iv_operator = '/'
    ).
    out->write( lv_intres ).

    DATA: lv_fizzbuzz TYPE string.
    lv_fizzbuzz = lo_course_basics->zif_abap_course_basics~fizz_buzz( ).
    out->write( lv_fizzbuzz ).


    DATA: lv_date TYPE string.
    lv_date = lo_course_basics->zif_abap_course_basics~date_parsing(
    iv_date = '22 June1987'
    ).
    out->write( lv_date ).

  ENDMETHOD.


  METHOD zif_abap_course_basics~calculator.
*  iv_first_number
*  iv_second_number
*  iv_operator
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
          lv_day        TYPE i,
          lv_month_text TYPE string,
          lv_month      TYPE i,
          lv_year       TYPE i,
          lv_date       TYPE d.

  SPLIT iv_date AT space INTO TABLE lt_parts.
  READ TABLE lt_parts INDEX 1 INTO DATA(lv_day_str).
  READ TABLE lt_parts INDEX 1 INTO lv_month_text.
  READ TABLE lt_parts INDEX 1 INTO DATA(lv_year_str).

  lv_day = lv_day_str.
  lv_year = lv_year_str.

  IF lv_month_text CO '0123456789'.
  lv_month = lv_month_text.
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

    lv_date = |{ lv_year }{ lv_month WIDTH = 2 }{ lv_day WIDTH = 2 }|.
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

      " Add to final result string
      IF lv_result IS INITIAL.
        lv_result = lv_line.
      ELSE.
        CONCATENATE lv_result lv_line INTO lv_result SEPARATED BY cl_abap_char_utilities=>newline.
      ENDIF.
    ENDDO.

    rv_result = lv_result.
  ENDMETHOD.


  METHOD zif_abap_course_basics~get_current_date_time.
  ENDMETHOD.


  METHOD zif_abap_course_basics~hello_world.
    DATA: message TYPE string.
    message = |Hello { iv_name }, your system user is { sy-uname }.|.
    rv_result = message.
  ENDMETHOD.


  METHOD zif_abap_course_basics~internal_tables.
  ENDMETHOD.


  METHOD zif_abap_course_basics~open_sql.
  ENDMETHOD.


  METHOD zif_abap_course_basics~scrabble_score.
  ENDMETHOD.
ENDCLASS.
