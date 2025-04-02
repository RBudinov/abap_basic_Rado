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
* MESSAGE e000.
    ENDCASE.

  ENDMETHOD.


  METHOD zif_abap_course_basics~date_parsing.
  ENDMETHOD.


  METHOD zif_abap_course_basics~fizz_buzz.
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
