*"* use this source file for your ABAP unit test classes

CLASS zcl_abap_ut_seltab_to_where DEFINITION DEFERRED.
CLASS zcl_abap_seltab_to_where DEFINITION LOCAL FRIENDS zcl_abap_ut_seltab_to_where.

CLASS zcl_abap_ut_seltab_to_where DEFINITION
  FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS
.
*?﻿<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">
*?<asx:values>
*?<TESTCLASS_OPTIONS>
*?<TEST_CLASS>zcl_abap_ut_seltab_to_where
*?</TEST_CLASS>
*?<TEST_MEMBER>f_Cut
*?</TEST_MEMBER>
*?<OBJECT_UNDER_TEST>zcl_abap_seltab_to_where
*?</OBJECT_UNDER_TEST>
*?<OBJECT_IS_LOCAL/>
*?<GENERATE_FIXTURE>X
*?</GENERATE_FIXTURE>
*?<GENERATE_CLASS_FIXTURE>X
*?</GENERATE_CLASS_FIXTURE>
*?<GENERATE_INVOCATION/>
*?<GENERATE_ASSERT_EQUAL>X
*?</GENERATE_ASSERT_EQUAL>
*?</TESTCLASS_OPTIONS>
*?</asx:values>
*?</asx:abap>
  PRIVATE SECTION.
    CLASS-DATA: mt_seltab  TYPE zcl_abap_seltab_to_where=>tt_named_seltables.
*                mt_sflight TYPE STANDARD TABLE OF sflight.
    CONSTANTS: mc_db_table TYPE tabname VALUE 'SFLIGHT'.

    CLASS-METHODS: class_setup.
    CLASS-METHODS: class_teardown.
    METHODS: setup.
    METHODS: teardown.
    METHODS: combine_seltabs_01 FOR TESTING.
    METHODS: combine_seltabs_02 FOR TESTING.
    METHODS: combine_seltabs_03 FOR TESTING.
    METHODS: combine_seltabs_04 FOR TESTING.
    METHODS: combine_seltabs_05 FOR TESTING.
    METHODS: combine_seltabs_06 FOR TESTING.
    METHODS: combine_seltabs_07 FOR TESTING.
    METHODS: combine_seltabs_08 FOR TESTING.
    METHODS: combine_seltabs_09 FOR TESTING.
    METHODS: combine_seltabs_10 FOR TESTING.
*    METHODS: combine_seltabs_07 FOR TESTING.
ENDCLASS.       "zcl_abap_ut_seltab_to_where


CLASS zcl_abap_ut_seltab_to_where IMPLEMENTATION.

  METHOD class_setup.
    DATA: lr_ref          TYPE REF TO data.
*
    DATA: lo_struct   TYPE REF TO cl_abap_structdescr,
          lt_comp     TYPE cl_abap_structdescr=>component_table,
          lt_comp_rng TYPE cl_abap_structdescr=>component_table,
          ls_comp     LIKE LINE OF lt_comp.

    DATA: ls_seltab TYPE zcl_abap_seltab_to_where=>ts_named_seltable.
    DATA: lt_sflight TYPE STANDARD TABLE OF sflight.

    FIELD-SYMBOLS: <ls_data> TYPE any.

***********************************************************************
**initial variables
***********************************************************************
    lo_struct ?= cl_abap_typedescr=>describe_by_name( mc_db_table  ).
    lt_comp = lo_struct->get_components( ).
    CLEAR lo_struct.

*    "step 2
    LOOP AT lt_comp INTO ls_comp.
      CLEAR lt_comp_rng.

      ls_seltab-name = ls_comp-name.

      ls_comp-name = 'LOW'.
      APPEND ls_comp TO lt_comp_rng.
      ls_comp-name = 'HIGH'.
      APPEND ls_comp TO lt_comp_rng.

      ls_comp-name = 'SIGN'.
      ls_comp-type ?= cl_abap_elemdescr=>get_c( p_length = 1 ).
      APPEND ls_comp TO lt_comp_rng.
      ls_comp-name = 'OPTION'.
      ls_comp-type ?= cl_abap_elemdescr=>get_c( p_length = 2 ).
      APPEND ls_comp TO lt_comp_rng.


      lo_struct = cl_abap_structdescr=>get( p_components = lt_comp_rng p_strict = abap_false ).
*construct range table of a field
      CREATE DATA lr_ref TYPE HANDLE lo_struct.
      ASSIGN lr_ref->* TO <ls_data>.

      CREATE DATA ls_seltab-dref LIKE TABLE OF <ls_data>.
      APPEND ls_seltab TO mt_seltab.

    ENDLOOP.



  ENDMETHOD.


  METHOD class_teardown.


  ENDMETHOD.






  METHOD combine_seltabs_01.
    DATA: lv_where_src  TYPE string,
          lv_where_tgt  TYPE string,
          lt_sfl_src    TYPE STANDARD TABLE OF sflight,
          lt_sfl_tgt    TYPE STANDARD TABLE OF sflight,
          lt_rng_carrid TYPE RANGE OF sflight-carrid.
    DATA: lt_seltab LIKE mt_seltab.

    FIELD-SYMBOLS: <ls_seltab> TYPE zcl_abap_seltab_to_where=>ts_named_seltable.

**********************************************************************
*Test Case 1.
*Simple EQ condition
**********************************************************************
    lt_seltab = mt_seltab.
    lv_where_src = |( CARRID EQ 'PDB' )|.
    READ TABLE lt_seltab WITH KEY name = 'CARRID' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_carrid = VALUE #( sign = 'I' option = 'EQ' ( low = 'PDB' ) ).
       <ls_seltab>-dref = ref #( lt_rng_carrid ).
    ENDIF.

    lv_where_tgt = zcl_abap_seltab_to_where=>combine_seltabs(
                         it_named_seltabs  = lt_seltab
                         )
                         .

    cl_aunit_assert=>assert_equals(
      exp = lv_where_src
      act = lv_where_tgt
      msg = |SRC: { lv_where_src }, TGT: { lv_where_tgt }.|
    ).


  ENDMETHOD.


  METHOD combine_seltabs_02.
    DATA: lv_where_src  TYPE string,
          lv_where_tgt  TYPE string,
          lt_rng_carrid TYPE RANGE OF sflight-carrid.
    DATA: lt_seltab LIKE mt_seltab.

    FIELD-SYMBOLS: <ls_seltab> TYPE zcl_abap_seltab_to_where=>ts_named_seltable.


**********************************************************************
*Test Case 2.
*Simple NE condition
**********************************************************************
    lt_seltab = mt_seltab.
    lv_where_src = |( CARRID NE 'PDB' )|.
    READ TABLE lt_seltab WITH KEY name = 'CARRID' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_carrid = VALUE #( sign = 'I' option = 'NE' ( low = 'PDB' ) ).
      <ls_seltab>-dref = ref #( lt_rng_carrid ).
    ENDIF.

    lv_where_tgt = zcl_abap_seltab_to_where=>combine_seltabs(
                         it_named_seltabs  = lt_seltab
                         )
                         .

    cl_aunit_assert=>assert_equals(
      exp = lv_where_src
      act = lv_where_tgt
      msg = |CASE 2. SRC: { lv_where_src }, TGT: { lv_where_tgt }.|
    ).

  ENDMETHOD.

  METHOD combine_seltabs_03.
    DATA: lv_where_src  TYPE string,
          lv_where_tgt  TYPE string,
          lt_sfl_src    TYPE STANDARD TABLE OF sflight,
          lt_sfl_tgt    TYPE STANDARD TABLE OF sflight,
          lt_rng_carrid TYPE RANGE OF sflight-carrid.
    DATA: lt_seltab LIKE mt_seltab.

    FIELD-SYMBOLS: <ls_seltab> TYPE zcl_abap_seltab_to_where=>ts_named_seltable.


**********************************************************************
*Test Case 3.
*SIGN = 'E' and OPTION = 'EQ'.
**********************************************************************
    lt_seltab = mt_seltab.
    lv_where_src = |( NOT ( CARRID EQ 'PDB' ) )|.
    READ TABLE lt_seltab WITH KEY name = 'CARRID' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_carrid = VALUE #( sign = 'E' option = 'EQ' ( low = 'PDB' ) ).
      <ls_seltab>-dref = ref #( lt_rng_carrid ).
    ENDIF.

    lv_where_tgt = zcl_abap_seltab_to_where=>combine_seltabs(
                         it_named_seltabs  = lt_seltab
                         )
                         .
    cl_aunit_assert=>assert_equals(
      exp = lv_where_src
      act = lv_where_tgt
      msg = |SRC: { lv_where_src }, TGT: { lv_where_tgt }.|
    ).
  ENDMETHOD.

  METHOD combine_seltabs_04.
    DATA: lv_where_src  TYPE string,
          lv_where_tgt  TYPE string,
          lt_sfl_src    TYPE STANDARD TABLE OF sflight,
          lt_sfl_tgt    TYPE STANDARD TABLE OF sflight,
          lt_rng_carrid TYPE RANGE OF sflight-carrid.
    DATA: lt_seltab LIKE mt_seltab.

    FIELD-SYMBOLS: <ls_seltab>  TYPE zcl_abap_seltab_to_where=>ts_named_seltable,
                   <lt_sfl_src> TYPE STANDARD TABLE,
                   <lt_sfl_tgt> TYPE STANDARD TABLE.


**********************************************************************
*Test Case 4.
*Handle NULL in DB.
**********************************************************************
    lt_seltab = mt_seltab.
    lv_where_src = |( CARRID EQ '' OR CARRID IS NULL )|.
    READ TABLE lt_seltab WITH KEY name = 'CARRID' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_carrid = VALUE #( sign = 'I' option = 'EQ' ( low = '' ) ).
      <ls_seltab>-dref = ref #( lt_rng_carrid ).
    ENDIF.

    lv_where_tgt = zcl_abap_seltab_to_where=>combine_seltabs(
                         it_named_seltabs  = lt_seltab
                         )
                         .

    cl_aunit_assert=>assert_equals(
      exp = lv_where_src
      act = lv_where_tgt
      msg = |SRC: { lv_where_src }, TGT: { lv_where_tgt }.|
    ).
  ENDMETHOD.

  METHOD combine_seltabs_05.
    DATA: lv_where_src  TYPE string,
          lv_where_tgt  TYPE string,
          lt_sfl_src    TYPE STANDARD TABLE OF sflight,
          lt_sfl_tgt    TYPE STANDARD TABLE OF sflight,
          lt_rng_carrid TYPE RANGE OF sflight-carrid,
          lt_rng_connid TYPE RANGE OF sflight-connid.
    DATA: lt_seltab LIKE mt_seltab.

    FIELD-SYMBOLS: <ls_seltab>  TYPE zcl_abap_seltab_to_where=>ts_named_seltable,
                   <lt_sfl_src> TYPE STANDARD TABLE,
                   <lt_sfl_tgt> TYPE STANDARD TABLE.


**********************************************************************
*Test Case 5.
*Condition on two fields.
**********************************************************************
    lt_seltab = mt_seltab.
    lv_where_src = |( CARRID EQ '' OR CARRID IS NULL ) AND ( CONNID EQ '0820' )|.

    READ TABLE lt_seltab WITH KEY name = 'CARRID' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_carrid = VALUE #( sign = 'I' option = 'EQ' ( low = '' ) ).
      <ls_seltab>-dref = ref #( lt_rng_carrid ).
    ENDIF.

    READ TABLE lt_seltab WITH KEY name = 'CONNID' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_connid = VALUE #( sign = 'I' option = 'EQ' ( low = '0820' ) ).
      <ls_seltab>-dref = ref #( lt_rng_connid ).
    ENDIF.

    lv_where_tgt = zcl_abap_seltab_to_where=>combine_seltabs(
                         it_named_seltabs  = lt_seltab
                         )
                         .

    cl_aunit_assert=>assert_equals(
      exp = lv_where_src
      act = lv_where_tgt
      msg = |SRC: { lv_where_src }, TGT: { lv_where_tgt }.|
    ).
  ENDMETHOD.

  METHOD combine_seltabs_06.
    DATA: lv_where_src  TYPE string,
          lv_where_tgt  TYPE string,
          lt_sfl_src    TYPE STANDARD TABLE OF sflight,
          lt_sfl_tgt    TYPE STANDARD TABLE OF sflight,
          lt_rng_carrid TYPE RANGE OF sflight-carrid,
          lt_rng_connid TYPE RANGE OF sflight-connid.
    DATA: lt_seltab LIKE mt_seltab.

    FIELD-SYMBOLS: <ls_seltab>  TYPE zcl_abap_seltab_to_where=>ts_named_seltable,
                   <lt_sfl_src> TYPE STANDARD TABLE,
                   <lt_sfl_tgt> TYPE STANDARD TABLE.


**********************************************************************
*Test Case 6.
*Condition on two fields. Two values are selected on the first field.
**********************************************************************
    lt_seltab = mt_seltab.
    lv_where_src = |( CARRID EQ 'AC' OR CARRID EQ 'LH' ) AND ( CONNID EQ '0820' )|.

    READ TABLE lt_seltab WITH KEY name = 'CARRID' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_carrid = VALUE #( sign = 'I' option = 'EQ' ( low = 'AC' )
                                                        ( low = 'LH' ) ).
      <ls_seltab>-dref = ref #( lt_rng_carrid ).
    ENDIF.

    READ TABLE lt_seltab WITH KEY name = 'CONNID' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_connid = VALUE #( sign = 'I' option = 'EQ' ( low = '0820' ) ).
      <ls_seltab>-dref = ref #( lt_rng_connid ).
    ENDIF.

    lv_where_tgt = zcl_abap_seltab_to_where=>combine_seltabs(
                         it_named_seltabs  = lt_seltab
                         )
                         .

    cl_aunit_assert=>assert_equals(
      exp = lv_where_src
      act = lv_where_tgt
      msg = |SRC: { lv_where_src }, TGT: { lv_where_tgt }.|
    ).
  ENDMETHOD.

  METHOD combine_seltabs_07.
    DATA: lv_where_src  TYPE string,
          lv_where_tgt  TYPE string,
          lt_sfl_src    TYPE STANDARD TABLE OF sflight,
          lt_sfl_tgt    TYPE STANDARD TABLE OF sflight,
          lt_rng_fldate TYPE RANGE OF sflight-fldate.
    .
    DATA: lt_seltab LIKE mt_seltab.

    FIELD-SYMBOLS: <ls_seltab>  TYPE zcl_abap_seltab_to_where=>ts_named_seltable,
                   <lt_sfl_src> TYPE STANDARD TABLE,
                   <lt_sfl_tgt> TYPE STANDARD TABLE.

**********************************************************************
*Test Case 7.
*Select a single date .
**********************************************************************
    lt_seltab = mt_seltab.
    lv_where_src = |( FLDATE EQ '20190319' )|.

    READ TABLE lt_seltab WITH KEY name = 'FLDATE' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_fldate = VALUE #( sign = 'I' option = 'EQ' ( low = '20190319' ) ).
      <ls_seltab>-dref = ref #( lt_rng_fldate ).
    ENDIF.


    lv_where_tgt = zcl_abap_seltab_to_where=>combine_seltabs(
                         it_named_seltabs  = lt_seltab
                         )
                         .

    cl_aunit_assert=>assert_equals(
      exp = lv_where_src
      act = lv_where_tgt
      msg = |SRC: { lv_where_src }, TGT: { lv_where_tgt }.|
    ).

  ENDMETHOD.



  METHOD combine_seltabs_08.
    DATA: lv_where_src  TYPE string,
          lv_where_tgt  TYPE string,
          lt_sfl_src    TYPE STANDARD TABLE OF sflight,
          lt_sfl_tgt    TYPE STANDARD TABLE OF sflight,
          lt_rng_fldate TYPE RANGE OF sflight-fldate.
    .
    DATA: lt_seltab LIKE mt_seltab.

    FIELD-SYMBOLS: <ls_seltab>  TYPE zcl_abap_seltab_to_where=>ts_named_seltable,
                   <lt_sfl_src> TYPE STANDARD TABLE,
                   <lt_sfl_tgt> TYPE STANDARD TABLE.

**********************************************************************
*Test Case 8.
*Select on date range
**********************************************************************
    lt_seltab = mt_seltab.
    lv_where_src = |( FLDATE BETWEEN '20190319' AND '20191231' )|.

    READ TABLE lt_seltab WITH KEY name = 'FLDATE' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_fldate = VALUE #( sign = 'I' option = 'BT' ( low = '20190319' high = '20191231' ) ).
       <ls_seltab>-dref = ref #( lt_rng_fldate ).
    ENDIF.


    lv_where_tgt = zcl_abap_seltab_to_where=>combine_seltabs(
                         it_named_seltabs  = lt_seltab
                         )
                         .

    cl_aunit_assert=>assert_equals(
      exp = lv_where_src
      act = lv_where_tgt
      msg = |SRC: { lv_where_src }, TGT: { lv_where_tgt }.|
    ).

  ENDMETHOD.

  METHOD combine_seltabs_09.
    DATA: lv_where_src    TYPE string,
          lv_where_tgt    TYPE string,
          lt_sfl_src      TYPE STANDARD TABLE OF sflight,
          lt_sfl_tgt      TYPE STANDARD TABLE OF sflight,
          lt_rng_carrid   TYPE RANGE OF sflight-carrid,
          lt_rng_connid   TYPE RANGE OF sflight-connid,
          lt_rng_currency TYPE RANGE OF sflight-currency,
          lt_rng_fldate   TYPE RANGE OF sflight-fldate.
    .
    DATA: lt_seltab LIKE mt_seltab.

    FIELD-SYMBOLS: <ls_seltab>  TYPE zcl_abap_seltab_to_where=>ts_named_seltable,
                   <lt_sfl_src> TYPE STANDARD TABLE,
                   <lt_sfl_tgt> TYPE STANDARD TABLE.

**********************************************************************
*Test Case 9.
*include wildcard characters
**********************************************************************
    lt_seltab = mt_seltab.
    lv_where_src = |( CARRID LIKE 'L%' )|.
    lv_where_src = |{ lv_where_src } AND ( CURRENCY NOT LIKE 'US_' )|.


    READ TABLE lt_seltab WITH KEY name = 'CARRID' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_carrid = VALUE #( sign = 'I' option = 'CP' ( low = 'L*' ) ).
      <ls_seltab>-dref = ref #( lt_rng_carrid ).
    ENDIF.

    READ TABLE lt_seltab WITH KEY name = 'CURRENCY' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_currency = VALUE #( sign = 'I' option = 'NP' ( low = 'US+' ) ).
       <ls_seltab>-dref = ref #( lt_rng_currency ).
    ENDIF.


    lv_where_tgt = zcl_abap_seltab_to_where=>combine_seltabs(
                         it_named_seltabs  = lt_seltab
                         )
                         .

    cl_aunit_assert=>assert_equals(
      exp = lv_where_src
      act = lv_where_tgt
      msg = |SRC: { lv_where_src }, TGT: { lv_where_tgt }.|
    ).

  ENDMETHOD.

  METHOD combine_seltabs_10.
    DATA: lv_where_src    TYPE string,
          lv_where_tgt    TYPE string,
          lt_sfl_src      TYPE STANDARD TABLE OF sflight,
          lt_sfl_tgt      TYPE STANDARD TABLE OF sflight,
          lt_rng_carrid   TYPE RANGE OF sflight-carrid,
          lt_rng_connid   TYPE RANGE OF sflight-connid,
          lt_rng_currency TYPE RANGE OF sflight-currency,
          lt_rng_fldate   TYPE RANGE OF sflight-fldate.
    .
    DATA: lt_seltab LIKE mt_seltab.

    FIELD-SYMBOLS: <ls_seltab>  TYPE zcl_abap_seltab_to_where=>ts_named_seltable,
                   <lt_sfl_src> TYPE STANDARD TABLE,
                   <lt_sfl_tgt> TYPE STANDARD TABLE.

**********************************************************************
*Test Case 10.
*Complex conditions
**********************************************************************
    lt_seltab = mt_seltab.
    lv_where_src = |( CARRID EQ 'LH' OR CARRID EQ 'AC' OR CARRID LIKE 'L%' )|.
    lv_where_src = |{ lv_where_src } AND ( CONNID NE '1111' )|.
    lv_where_src = |{ lv_where_src } AND ( FLDATE BETWEEN '20190319' AND '20191231' )|.
    lv_where_src = |{ lv_where_src } AND ( ( CURRENCY NOT LIKE 'AB_' ) AND NOT ( CURRENCY NE 'USD' ) )|.

    READ TABLE lt_seltab WITH KEY name = 'CARRID' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_carrid = VALUE #( ( sign = 'I' option = 'EQ'  low = 'LH' )
                               ( sign = 'I' option = 'EQ'  low = 'AC' )
                               ( sign = 'I' option = 'CP'  low = 'L*' ) ).
      <ls_seltab>-dref = ref #( lt_rng_carrid ).
    ENDIF.

    READ TABLE lt_seltab WITH KEY name = 'CURRENCY' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_currency = VALUE #( ( sign = 'I' option = 'NP'  low = 'AB+' )
                                 ( sign = 'E' option = 'NE'  low = 'USD' ) ).
      <ls_seltab>-dref = ref #( lt_rng_currency ).
    ENDIF.



    READ TABLE lt_seltab WITH KEY name = 'CONNID' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_connid = VALUE #( sign = 'I' option = 'NE' ( low = '1111' ) ).
       <ls_seltab>-dref = ref #( lt_rng_connid ).
    ENDIF.



    READ TABLE lt_seltab WITH KEY name = 'FLDATE' ASSIGNING <ls_seltab>.
    IF sy-subrc = 0.
      lt_rng_fldate = VALUE #( sign = 'I' option = 'BT' ( low = '20190319' high = '20191231' ) ).
      <ls_seltab>-dref = ref #( lt_rng_fldate ).
    ENDIF.


    lv_where_tgt = zcl_abap_seltab_to_where=>combine_seltabs(
                         it_named_seltabs  = lt_seltab
                         )
                         .

    cl_aunit_assert=>assert_equals(
      exp = lv_where_src
      act = lv_where_tgt
      msg = |SRC: { lv_where_src }, TGT: { lv_where_tgt }.|
    ).

  ENDMETHOD.


  METHOD setup.
*      .
  ENDMETHOD.


  METHOD teardown.



  ENDMETHOD.
ENDCLASS.
