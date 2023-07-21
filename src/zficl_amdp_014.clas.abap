CLASS zficl_amdp_014 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_amdp_marker_hdb .

    "**********************************************************************

    TYPES: gtt_zmmdt00091 TYPE TABLE OF ZFIVT00107,
           gtt_bseg       TYPE TABLE OF zfivt00105.

    "**********************************************************************

    CLASS-METHODS:
      get_zmmdt00091 IMPORTING
                       VALUE(im_where)      TYPE sxmsbody
                     EXPORTING
                       VALUE(et_zmmdt00091) TYPE gtt_zmmdt00091
                     RAISING
                       cx_amdp_error,

      get_bseg IMPORTING
                 VALUE(im_where) TYPE sxmsbody
               EXPORTING
                 VALUE(et_bseg)  TYPE gtt_bseg
               RAISING
                 cx_amdp_error.

    "**********************************************************************

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zficl_amdp_014 IMPLEMENTATION.

  METHOD get_zmmdt00091    BY DATABASE PROCEDURE FOR HDB
                           LANGUAGE SQLSCRIPT
                           OPTIONS READ-ONLY
                           USING ZFIVT00107.

    et_zmmdt00091 = APPLY_FILTER (ZFIVT00107, :im_where);

  ENDMETHOD.

  METHOD get_bseg    BY DATABASE PROCEDURE FOR HDB
                           LANGUAGE SQLSCRIPT
                           OPTIONS READ-ONLY
                           USING zfivt00105.

    et_bseg = APPLY_FILTER (zfivt00105, :im_where);

  ENDMETHOD.

ENDCLASS.
