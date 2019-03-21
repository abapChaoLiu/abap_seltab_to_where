*&---------------------------------------------------------------------*
*& Report ZR_DEMO_SELTAB_TO_WHERE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zr_demo_seltab_to_where.

DATA: lt_seltab     TYPE zcl_abap_seltab_to_where=>tt_named_seltables,
      lt_rng_carrid TYPE RANGE OF sflight-carrid.

lt_rng_carrid = VALUE #( sign = 'I' option = 'EQ' ( low = 'ABC' ) ).

lt_seltab = VALUE #( ( name = 'CARRID' dref = REF #( lt_rng_carrid ) ) ).

DATA(lv_where) = zcl_abap_seltab_to_where=>combine_seltabs(
                   it_named_seltabs  = lt_seltab
                   ).

WRITE: lv_where.
