![Build Status](https://github.com/abapChaoLiu/abap_seltab_to_where/workflows/abapLint/badge.svg)

# abap_seltab_to_where
Convert ABAP selection range tables to a SQL where clause.
## Demo 1
```abap
REPORT ZR_DEMO_SELTAB_TO_WHERE.

data: lt_seltab  TYPE zcl_abap_seltab_to_where=>tt_named_seltables,
      lt_rng_carrid TYPE RANGE OF sflight-carrid.


      lt_rng_carrid = VALUE #( sign = 'I' option = 'EQ' ( low = 'ABC' ) ).
      lt_seltab = VALUE #( ( name = 'CARRID' dref = ref #( lt_rng_carrid ) ) ).

      data(lv_where_clause) = zcl_abap_seltab_to_where=>combine_seltabs(
                              it_named_seltabs  = lt_seltab          ).

write: lv_where_clause.
"OUTPUT--->: ( CARRID EQ 'ABC' )
```
## Demo 2
```abap
REPORT ZR_DEMO_SELTAB_TO_WHERE.

data: lt_seltab  TYPE zcl_abap_seltab_to_where=>tt_named_seltables,
      lt_rng_carrid TYPE RANGE OF sflight-carrid,
      lt_rng_fldate TYPE RANGE OF sflight-fldate.


      lt_rng_carrid = VALUE #( sign = 'I' option = 'EQ' ( low = 'ABC' )
                                                        ( low = 'DEF' ) ).
                                                        
      lt_rng_fldate = VALUE #( sign = 'I' option = 'BT' ( low = '20200101' high = '20201231' ) ).
      
      lt_seltab = VALUE #( ( name = 'CARRID' dref = ref #( lt_rng_carrid ) )
                           ( name = 'FLDATE' dref = ref #( lt_rng_fldate ) ) ).

      data(lv_where_clause) = zcl_abap_seltab_to_where=>combine_seltabs(
                              it_named_seltabs  = lt_seltab          ).

write: lv_where_clause.
"OUTPUT--->: ( CARRID EQ 'ABC' OR CARRID EQ 'DEF' ) AND ( FLDATE BETWEEN '20200101' AND '20201231' )
```

## PR is welcome!
- Unit Test class is provided. Feel free to add more test cases.

## Credits and references
Class `ZCL_ABAP_SELTAB_TO_WHERE` is modified from class `CL_SHDB_SELTAB` (More detail can be found in this post https://blogs.sap.com/2015/03/30/handling-of-select-options-parameters-within-amdp/). 

## Let's make ABAP great again! ;)
