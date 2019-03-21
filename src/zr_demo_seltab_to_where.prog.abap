*&---------------------------------------------------------------------*
*& Report ZR_DEMO_SELTAB_TO_WHERE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ZR_DEMO_SELTAB_TO_WHERE.

data: lt_seltab  TYPE zcl_abap_seltab_to_where=>tt_named_seltables,
      lt_rng_carrid TYPE RANGE OF sflight-carrid.

      lt_rng_carrid = VALUE #( sign = 'I' option = 'EQ' ( low = 'ABC' ) ).

      lt_seltab = value #( ( name = 'CARRID' dref = ref #( lt_rng_carrid ) ) ).

      data(lv_where) = zcl_abap_seltab_to_where=>combine_seltabs(
                         it_named_seltabs  = lt_seltab
                         ).

write: lv_where.
