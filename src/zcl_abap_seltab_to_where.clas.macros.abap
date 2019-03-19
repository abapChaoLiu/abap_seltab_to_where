*"* use this source file for any macro definitions you need
*"* in the implementation part of the class
  DEFINE  mac_field_exists.
    READ TABLE lt_fields WITH KEY name = &1 TRANSPORTING NO FIELDS.
    ASSERT sy-subrc = 0.
  END-OF-DEFINITION.

  DEFINE mac_assign.
    "Usage: mac_assign <structure> <field-position> <name> <type>
    FIELD-SYMBOLS &3 TYPE &4.
    ASSIGN COMPONENT &2 OF STRUCTURE &1 TO &3.
  END-OF-DEFINITION.

  DEFINE mac_check_type.
    READ TABLE lt_fields WITH KEY name = &1 INTO ls_low.
    READ TABLE lt_fields WITH KEY name = &2 INTO ls_high.
    ASSERT ls_low-type->type_kind = ls_high-type->type_kind.
    CASE ls_low-type->type_kind.
      WHEN cl_abap_typedescr=>typekind_class        "not allowed types in SELTAB
        OR cl_abap_typedescr=>typekind_dref
        OR cl_abap_typedescr=>typekind_iref
        OR cl_abap_typedescr=>typekind_oref
        OR cl_abap_typedescr=>typekind_intf
        OR cl_abap_typedescr=>typekind_struct1
        OR cl_abap_typedescr=>typekind_struct2
        OR cl_abap_typedescr=>typekind_table
        OR cl_abap_typedescr=>typekind_bref.
        ASSERT ls_low-type->type_kind IS NOT INITIAL.
      WHEN cl_abap_typedescr=>typekind_char         "char-like types
        OR cl_abap_typedescr=>typekind_clike
        OR cl_abap_typedescr=>typekind_csequence
        OR cl_abap_typedescr=>typekind_string.
        _mv_quote_l = `'`.
        _mv_quote_r = `'`.
        _mv_clike = abap_true.
      WHEN cl_abap_typedescr=>typekind_any
        OR cl_abap_typedescr=>typekind_data
        OR cl_abap_typedescr=>typekind_date
        OR cl_abap_typedescr=>typekind_num
        OR cl_abap_typedescr=>typekind_time
        OR cl_abap_typedescr=>typekind_xstring.
        _mv_quote_l = `'`.
        _mv_quote_r = `'`.
      WHEN cl_abap_typedescr=>typekind_hex          "hexadecimal types
        OR cl_abap_typedescr=>typekind_xsequence.
        _mv_quote_l = `'`.
        _mv_quote_r = `'`.

      WHEN cl_abap_typedescr=>typekind_decfloat     "numeric types
        OR cl_abap_typedescr=>typekind_decfloat16
        OR cl_abap_typedescr=>typekind_decfloat34
        OR cl_abap_typedescr=>typekind_float
        OR cl_abap_typedescr=>typekind_int
        OR cl_abap_typedescr=>typekind_int1
        OR cl_abap_typedescr=>typekind_int2
        OR cl_abap_typedescr=>typekind_numeric
        OR cl_abap_typedescr=>typekind_packed
        OR cl_abap_typedescr=>typekind_simple
        OR cl_abap_typedescr=>typekind_w.
        CLEAR: _mv_quote_l, _mv_quote_r.
    ENDCASE.
  END-OF-DEFINITION.
