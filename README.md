# seltab_to_where
Convert ABAP selection range tables to an SQL where clause
# Simple Demo
```
REPORT ZR_DEMO_SELTAB_TO_WHERE.

data: lt_seltab  TYPE zcl_abap_seltab_to_where=>tt_named_seltables,
      lt_rng_carrid TYPE RANGE OF sflight-carrid.

      lt_rng_carrid = VALUE #( sign = 'I' option = 'EQ' ( low = 'ABC' ) ).

      lt_seltab = value #( ( name = 'CARRID' dref = ref #( lt_rng_carrid ) ) ).

      data(lv_where) = zcl_abap_seltab_to_where=>combine_seltabs(
                         it_named_seltabs  = lt_seltab          ).

write: lv_where.
```
# Credits and references
Class `ZCL_ABAP_SELTAB_TO_WHERE` is modified from class `CL_SHDB_SELTAB` (More detail can be found in this post https://blogs.sap.com/2015/03/30/handling-of-select-options-parameters-within-amdp/). 
