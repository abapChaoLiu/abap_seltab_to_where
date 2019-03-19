CLASS zcl_abap_seltab_to_where DEFINITION
  PUBLIC
  CREATE PRIVATE

  GLOBAL FRIENDS if_lib_seltab .

  PUBLIC SECTION.

    TYPES: BEGIN OF  ts_named_seltable,
             name TYPE string,
             dref TYPE REF TO data,
           END OF ts_named_seltable.
    TYPES: BEGIN OF  ts_named_oref,
             name TYPE string,
             oref TYPE REF TO object,
           END OF ts_named_oref.


    TYPES tt_named_seltabs TYPE STANDARD TABLE OF ts_named_oref.
    TYPES tt_named_seltables TYPE STANDARD TABLE OF ts_named_seltable.

    CLASS-DATA mf_null_handle TYPE rs_bool VALUE abap_true ##NO_TEXT.
    DATA mv_escape TYPE string VALUE '@' ##NO_TEXT.
    CONSTANTS mc_sign_incl TYPE char1 VALUE 'I' ##NO_TEXT.
    CONSTANTS mc_sign_excl TYPE char1 VALUE 'E' ##NO_TEXT.

    CLASS-METHODS new
      IMPORTING
        !it_sel          TYPE table
        !iv_expand_empty TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rr_ref)    TYPE REF TO zcl_abap_seltab_to_where .
    CLASS-METHODS new_flat
      IMPORTING
        !iv_sel          TYPE string
        !iv_expand_empty TYPE abap_bool DEFAULT abap_true
      RETURNING
        VALUE(rr_ref)    TYPE REF TO zcl_abap_seltab_to_where .
    METHODS sql_where_condition
      IMPORTING
        !iv_field      TYPE string
      RETURNING
        VALUE(rv_cond) TYPE string
      RAISING
        cx_shdb_exception .
    CLASS-METHODS resolve_named_seltabs
      IMPORTING
        !it_named_seltabs TYPE tt_named_seltabs
        !iv_operator      TYPE string DEFAULT 'AND'
      RETURNING
        VALUE(rv_sql)     TYPE string
      RAISING
        cx_shdb_exception .
    CLASS-METHODS combine_seltabs
      IMPORTING
        !it_named_seltabs TYPE tt_named_seltables
        !iv_client_field  TYPE string OPTIONAL
        !if_null_handle   TYPE rs_bool DEFAULT abap_true
        !iv_abap_or_amdp  TYPE char4 DEFAULT 'ABAP'
      RETURNING
        VALUE(rv_where)   TYPE string
      RAISING
        cx_shdb_exception .
  PROTECTED SECTION.
  PRIVATE SECTION.

    DATA _mr_seltab TYPE REF TO data .
    DATA _mr_table_descr TYPE REF TO cl_abap_tabledescr .
    DATA _mv_quote_l TYPE string .
    DATA _mv_quote_r TYPE string .
    DATA _mv_clike TYPE abap_bool .
    DATA _mv_expand_empty TYPE abap_bool .

    METHODS constructor
      IMPORTING
        !it_sel          TYPE table
        !iv_expand_empty TYPE abap_bool DEFAULT abap_true .
    METHODS _sql_operator
      IMPORTING
        !iv_selop       TYPE char2
      RETURNING
        VALUE(rv_sqlop) TYPE string .
    METHODS _build_conditions
      IMPORTING
        !iv_field      TYPE string
        !iv_sign       TYPE char1
      RETURNING
        VALUE(rv_cond) TYPE string
      RAISING
        cx_shdb_exception .
    CLASS-METHODS _empty_seltab
      RETURNING
        VALUE(rr_ref) TYPE REF TO cl_abap_tabledescr .
ENDCLASS.



CLASS zcl_abap_seltab_to_where IMPLEMENTATION.


  METHOD combine_seltabs.
    DATA:
      ls_seltab TYPE ts_named_seltable,
      lr_seltab TYPE REF TO zcl_abap_seltab_to_where,
      lv_sep    TYPE string.
    FIELD-SYMBOLS
      <fs_seltab> TYPE table.

    mf_null_handle = if_null_handle.


    IF iv_client_field IS NOT INITIAL.
      rv_where = |{ iv_client_field } = '{ sy-mandt }'|.
      lv_sep = ` AND `.
    ENDIF.

    LOOP AT it_named_seltabs INTO ls_seltab.
      ASSIGN ls_seltab-dref->* TO <fs_seltab>.
      IF <fs_seltab> IS NOT INITIAL.
        lr_seltab = zcl_abap_seltab_to_where=>new( <fs_seltab> ).
        IF rv_where IS NOT INITIAL.
          rv_where = |{ rv_where }{ lv_sep } ( { lr_seltab->sql_where_condition( ls_seltab-name ) } ) |.
          lv_sep = ' AND '.
        ELSE.
          rv_where = |( { lr_seltab->sql_where_condition( ls_seltab-name ) } )|.
          lv_sep = ' AND '.
        ENDIF.
      ENDIF.

    ENDLOOP.

*added on 1/28/2019
    CONDENSE rv_where.

  ENDMETHOD.


  METHOD constructor.
    DATA:
      lr_struc_descr TYPE REF TO cl_abap_structdescr,
      lt_fields      TYPE abap_component_tab,
      ls_low         TYPE abap_componentdescr,
      ls_high        TYPE abap_componentdescr.
    FIELD-SYMBOLS:
      <lt_seltab> TYPE table,
      <ls_all>    TYPE data.

    "regard empty selection options:
    _mv_expand_empty = iv_expand_empty.
    IF it_sel IS INITIAL AND _mv_expand_empty = abap_true.
      _mr_table_descr = _empty_seltab( ).
    ELSE.
      _mr_table_descr ?= cl_abap_datadescr=>describe_by_data( it_sel ).
    ENDIF.

    lr_struc_descr ?= _mr_table_descr->get_table_line_type( ).

    "precondition: expects selection tables only:
    lt_fields = lr_struc_descr->get_components( ).
    mac_field_exists 'SIGN'.
    mac_field_exists 'OPTION'.
    mac_field_exists 'LOW'.
    mac_field_exists 'HIGH'.
    mac_check_type 'LOW' 'HIGH'.

    CREATE DATA _mr_seltab TYPE HANDLE _mr_table_descr.
    ASSIGN _mr_seltab->* TO <lt_seltab>.

    IF it_sel IS INITIAL AND _mv_expand_empty = abap_true.
      "empty seltab == return all values!
      APPEND if_lib_seltab=>mc_c_all TO <lt_seltab>.
    ELSE.
      "Fill _mr_seltab with input
      <lt_seltab> = it_sel.
    ENDIF.

  ENDMETHOD.                    "constructor


  METHOD new.
    CREATE OBJECT rr_ref
      EXPORTING
        it_sel          = it_sel
        iv_expand_empty = iv_expand_empty.
  ENDMETHOD.                    "if_lib_seltab~new


  METHOD new_flat.
    DATA:
      lt_lines TYPE STANDARD TABLE OF string,
      lv_line  TYPE string,
      ls_sel   TYPE if_lib_seltab=>ts_sel,
      lt_sel   TYPE if_lib_seltab=>tt_sel.

    SPLIT iv_sel AT ';' INTO TABLE lt_lines.
    LOOP AT lt_lines INTO lv_line.
      CONDENSE lv_line.
      IF find( val = lv_line  sub = ',' ) = -1.
        "simplify user-syntax for just a sequence of values:
        ls_sel-sign = 'I'.
        ls_sel-option = 'EQ'.
        ls_sel-low = lv_line.
        CLEAR: ls_sel-high.
      ELSE.
        CLEAR: ls_sel-high.
        SPLIT lv_line AT ',' INTO ls_sel-sign ls_sel-option ls_sel-low ls_sel-high.
      ENDIF.
      APPEND ls_sel TO lt_sel.
    ENDLOOP.
    rr_ref = new( it_sel = lt_sel iv_expand_empty = iv_expand_empty ).

  ENDMETHOD.                    "new_flat


  METHOD resolve_named_seltabs.
    DATA:
      ls_seltab TYPE ts_named_oref,
      lr_seltab TYPE REF TO zcl_abap_seltab_to_where,
      lv_where  TYPE string,
      lv_sep    TYPE string.

    LOOP AT it_named_seltabs INTO ls_seltab.
      lr_seltab ?= ls_seltab-oref.
      lv_where = lr_seltab->sql_where_condition( ls_seltab-name ).
      IF lv_where IS NOT INITIAL.
        rv_sql = |{ rv_sql }{ lv_sep }({ lv_where })|.
        lv_sep = | { iv_operator } |.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD sql_where_condition.
    DATA:
      lv_field   TYPE string,
      lv_include TYPE string,
      lv_exclude TYPE string.

    "Regard fieldnames containing namespaces:
    IF find( val = iv_field  sub = '/' ) >= 0.
      lv_field = |"{ iv_field }"|.
    ELSE.
      lv_field = iv_field.
    ENDIF.

    "Collect amount of including and excluding lines (OR):
    lv_include = _build_conditions( iv_field = lv_field  iv_sign = mc_sign_incl ).
    lv_exclude = _build_conditions( iv_field = lv_field  iv_sign = mc_sign_excl ).

    "Reduce the including amount by the excluding amount (AND NOT)
    IF lv_include IS NOT INITIAL AND lv_exclude IS NOT INITIAL.
      rv_cond = |( { lv_include } ) AND NOT ( { lv_exclude } )|.
    ELSEIF lv_exclude IS NOT INITIAL.
      rv_cond = | NOT ({ lv_exclude } )|.
    ELSEIF lv_include IS NOT INITIAL.
      rv_cond = lv_include.
    ELSE.
      rv_cond = ''.
    ENDIF.

  ENDMETHOD.                    "sql_where_condition


  METHOD _build_conditions.
    DATA:
      lv_line          TYPE string,
      lv_op            TYPE string,
      lv_sep           TYPE string,
      lv_escape        TYPE abap_bool,
      lv_bracket_left  TYPE char1,
      lv_bracket_right TYPE char1,
      lv_sign_count    TYPE i.
    FIELD-SYMBOLS:
      <lt_seltab> TYPE table,
      <ls_seltab> TYPE data,
      <sign>      TYPE data,
      <option>    TYPE data,
      <low>       TYPE data,
      <high>      TYPE data.

    ASSIGN _mr_seltab->* TO <lt_seltab>.
    IF <lt_seltab> IS INITIAL AND _mv_expand_empty = abap_false.
      RETURN.
    ENDIF.

    CLEAR lv_sep.
    LOOP AT <lt_seltab> ASSIGNING <ls_seltab>.

      CLEAR: lv_escape.
      ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <ls_seltab> TO <sign>.
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_seltab> TO <option>.
      ASSIGN COMPONENT 'LOW'    OF STRUCTURE <ls_seltab> TO <low>.
      ASSIGN COMPONENT 'HIGH'   OF STRUCTURE <ls_seltab> TO <high>.

      "avoid usage of options which are not supported for the particular data type:
      IF ( <option> = 'CP' OR <option> = 'NP' ) AND _mv_clike = abap_false.
        cx_shdb_exception=>raise( |select option { <option> } is allowd only for character-like data types| ).
      ENDIF.

      IF <sign> = iv_sign.

        IF sy-tabix > 1. rv_cond = |{ rv_cond } |. ENDIF.

        IF _mv_clike = abap_true.
          "Escape special chars (ATTENTION: the correct sequence is important!)"
          REPLACE ALL OCCURRENCES OF |'| IN <low>  WITH |''|.
          REPLACE ALL OCCURRENCES OF |'| IN <high> WITH |''|.
          IF <option> = 'CP' OR <option> = 'NP'.
            "special escapes, for compare options only:
            REPLACE ALL OCCURRENCES OF |@| IN <low>  WITH |{ mv_escape }@|. lv_escape = boolc( sy-subrc < 4 OR lv_escape = abap_true ).
            REPLACE ALL OCCURRENCES OF |_| IN <low>  WITH |{ mv_escape }_|. lv_escape = boolc( sy-subrc < 4 OR lv_escape = abap_true ).
            REPLACE ALL OCCURRENCES OF |%| IN <low>  WITH |{ mv_escape }%|. lv_escape = boolc( sy-subrc < 4 OR lv_escape = abap_true ).
          ENDIF.
        ENDIF.

        lv_op = _sql_operator( <option> ).
        IF lv_escape = abap_true. lv_op = |{ lv_op } ESCAPE '{ mv_escape }' |. ENDIF.

        REPLACE ALL OCCURRENCES OF '%LOW%'  IN lv_op WITH |{ <low> }|.
        REPLACE ALL OCCURRENCES OF '%HIGH%' IN lv_op WITH |{ <high> }|.
        IF <option> = 'CP' OR <option> = 'NP'.
          "wildcard transformation for compare options only:
          REPLACE ALL OCCURRENCES OF '*'    IN lv_op WITH '%'.
          REPLACE ALL OCCURRENCES OF '+'    IN lv_op WITH '_'.
        ENDIF.

        "handle NULL in where clause.
        IF mf_null_handle = abap_true AND
           <sign> = mc_sign_incl.
          REPLACE ALL OCCURRENCES OF |EQ ''| IN lv_op WITH |EQ '' OR { iv_field } IS NULL|.
        ENDIF.

        lv_line = |{ iv_field } { lv_op } |.

        rv_cond = | { rv_cond }{ lv_sep } { lv_bracket_left }{ lv_line }{ lv_bracket_right } |.
        lv_sep = ' OR'.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD _empty_seltab.
    DATA:
      lt_all LIKE STANDARD TABLE OF if_lib_seltab=>mc_c_all.
    "if seltab is empty, always create a char-like seltab type:
    rr_ref ?= cl_abap_datadescr=>describe_by_data( lt_all ).
  ENDMETHOD.


  METHOD _sql_operator.
    CASE iv_selop.
      WHEN: 'EQ'. rv_sqlop = |EQ { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'NE'. rv_sqlop = |NE { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'GE'. rv_sqlop = |GE { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'LE'. rv_sqlop = |LE { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'GT'. rv_sqlop = |GT { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'LT'. rv_sqlop = |LT { _mv_quote_l }%LOW%{ _mv_quote_r }|.

      WHEN: 'CP'. rv_sqlop = |LIKE { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'NP'. rv_sqlop = |NOT LIKE { _mv_quote_l }%LOW%{ _mv_quote_r }|.
      WHEN: 'BT'. rv_sqlop = |BETWEEN { _mv_quote_l }%LOW%{ _mv_quote_r } AND { _mv_quote_l }%HIGH%{ _mv_quote_r }|.
      WHEN: 'NB'. rv_sqlop = |NOT BETWEEN { _mv_quote_l }%LOW%{ _mv_quote_r } AND { _mv_quote_l }%HIGH%{ _mv_quote_r }|.
    ENDCASE.

  ENDMETHOD.
ENDCLASS.
