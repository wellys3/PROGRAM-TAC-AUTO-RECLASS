*&---------------------------------------------------------------------*
*& Include          ZFI02R0037_TOP
*&---------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Type-Pools                                                         *
*--------------------------------------------------------------------*
*TYPE-POOLS: icon, truxs, col, fiehc.
*--------------------------------------------------------------------*
* End - Type-Pools                                                   *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Nodes                                                              *
*--------------------------------------------------------------------*
*NODES: peras.
*--------------------------------------------------------------------*
* End - Nodes                                                        *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Infotype
*--------------------------------------------------------------------*
*INFOTYPES: 0000, 0001, 2006 MODE N.
*INFOTYPES: 0000, 0001, 2006.
*--------------------------------------------------------------------*
* End Infotype
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Tables                                                             *
*--------------------------------------------------------------------*
TABLES: acdoca, zmmdt00091, zfidt00318.
*--------------------------------------------------------------------*
* End - Tables                                                       *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Constants                                                   *
*--------------------------------------------------------------------*
CONSTANTS: gc_report_title   TYPE lvc_title VALUE 'Program TAC Auto Reclass',
*           gc_filename     TYPE string VALUE 'OTH_ADMF_GL_LEDGER',
           gc_rbukrs         TYPE bukrs VALUE 'ADMF',
           gc_kind_created   TYPE zfidt00319-kind VALUE '01',
           gc_kind_send_ob   TYPE zfidt00319-kind VALUE '02_A',
           gc_kind_resend_ob TYPE zfidt00319-kind VALUE '02_B',
           gc_kind_post      TYPE zfidt00319-kind VALUE '03_A',
           gc_kind_post_cl   TYPE zfidt00319-kind VALUE '03_B'.
*--------------------------------------------------------------------*
* End - Global Constants                                             *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Types                                                       *
*--------------------------------------------------------------------*
*Custom
TYPES: BEGIN OF gty_zfidt00318,
         select TYPE bcselect.
    INCLUDE TYPE zfidt00318.
TYPES: END OF gty_zfidt00318.

TYPES: BEGIN OF gty_zficd_zfidt00318,
         select TYPE bcselect.
    INCLUDE TYPE zficd_zfidt00318.
TYPES: END OF gty_zficd_zfidt00318.

TYPES: gtt_zfidt00318   TYPE TABLE OF zfidt00318,
       gtt_zfidt00318_2 TYPE TABLE OF gty_zfidt00318,
       gtt_zfidt00318_3 TYPE TABLE OF gty_zficd_zfidt00318,
       gtt_zfidt00317   TYPE TABLE OF zfivt00103,
       gtt_zmmdt00091   TYPE TABLE OF zfivt00107,
       gtt_bseg         TYPE TABLE OF zfivt00105.

*--------------------------------------------------------------------*

*Standard
TYPES: BEGIN OF gty_named_dref,
         name TYPE string,
         dref TYPE REF TO data,
       END OF gty_named_dref.

TYPES: gtt_return   TYPE TABLE OF bapiret2.
*--------------------------------------------------------------------*
* End - Global Types                                                 *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Variable                                                    *
*--------------------------------------------------------------------*
*Custom

*---Variable Program - Table & Work Area
DATA: git_zfidt00318   TYPE TABLE OF zfidt00318,
      gwa_zfidt00318   TYPE zfidt00318,
      git_zfidt00318_2 TYPE TABLE OF gty_zfidt00318,
      gwa_zfidt00318_2 TYPE gty_zfidt00318,
      git_zfidt00318_3 TYPE TABLE OF gty_zficd_zfidt00318,
      gwa_zfidt00318_3 TYPE gty_zficd_zfidt00318,
      git_zfidt00317   TYPE TABLE OF zficd_zfidt00317,
      git_zmmdt00091   TYPE TABLE OF zfivt00107,
      git_bseg         TYPE TABLE OF zfivt00105.

*--------------------------------------------------------------------*
*Standard

DATA: gd_return  TYPE string,
      git_return TYPE TABLE OF bapiret2,
      gwa_return TYPE bapiret2.

*---Variable Program - Single Value
DATA: gd_rb         TYPE char20,
      gd_rb_2       TYPE char20,
      gd_line_excel TYPE i,
      gd_tabix      TYPE i,
      gd_subrc      TYPE sy-subrc,
      gd_message    TYPE text255,
      gd_answer(1). "Variable for Popup Answer.

*---For AMDP Class
DATA: gd_where          TYPE sxmsbody,
      gd_where1         TYPE sxmsbody,
      gd_where2         TYPE sxmsbody,
      gd_where3         TYPE sxmsbody,
      gd_where4         TYPE sxmsbody,
      gd_where5         TYPE sxmsbody,
      git_named_seltabs TYPE TABLE OF gty_named_dref,
      gwa_named_seltabs TYPE gty_named_dref.

*---For Refresh ALV
DATA: gwa_stable     TYPE lvc_s_stbl,
      gd_refreshmode TYPE salv_de_constant.

*---For Debugger
DATA: git_terminal          TYPE TABLE OF tvarvc WITH HEADER LINE,
      gd_opcode_usr_attr(1) TYPE x VALUE 5,
      gd_terminal           TYPE usr41-terminal,
      gd_zdebug             TYPE text255,
      gd_flag               TYPE text255.

*---For Status Progress
DATA: gd_percent TYPE i,
      gd_lines   TYPE i.

*---Variable Get Execution Time
DATA: gd_start   TYPE p DECIMALS 3,
      gd_stop    TYPE p DECIMALS 3,
      gd_run     TYPE p DECIMALS 3,
      gd_run_str TYPE text255.
*--------------------------------------------------------------------*
* End - Global Variable                                              *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Global Range                                                       *
*--------------------------------------------------------------------*
*Custom

*RANGES: gra_zzku FOR acdoca-zzku,
*        gra_zzcp FOR acdoca-zzcp.

*--------------------------------------------------------------------*
*Standard

RANGES: gra_racct FOR acdoca-racct,
        gra_matnr FOR zmmdt00091-matnr,
        gra_blart FOR bkpf-blart,
        gra_shkzg FOR bseg-shkzg.

*--------------------------------------------------------------------*
* End - Global Variable                                              *
*--------------------------------------------------------------------*


*--------------------------------------------------------------------*
* Define                                                             *
*--------------------------------------------------------------------*
DEFINE f_fill_range.
  &1-sign = &2.
  &1-option = &3.
  &1-low = &4.
  &1-high = &5.
  APPEND &1.
END-OF-DEFINITION.

"Example: f_fill_range: lra_lptyp 'I' 'EQ' lwa_lptyp-lptyp ''.
*--------------------------------------------------------------------*
* End - Define                                                       *
*--------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Selection Screen                                                     *
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK a01 WITH FRAME TITLE text900.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS rb1 RADIOBUTTON GROUP rb1 DEFAULT 'X' USER-COMMAND uco.
SELECTION-SCREEN COMMENT 4(30) text801.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS rb2 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT 4(35) text802.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS rb3 RADIOBUTTON GROUP rb1.
SELECTION-SCREEN COMMENT 4(30) text803.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK a01.

SELECTION-SCREEN BEGIN OF BLOCK a02 WITH FRAME TITLE text901.
SELECT-OPTIONS: s1_bukrs FOR acdoca-rbukrs MEMORY ID zfi02r0037_s1_1 NO-EXTENSION NO INTERVALS DEFAULT gc_rbukrs MODIF ID m01.
SELECTION-SCREEN END OF BLOCK a02.

SELECTION-SCREEN BEGIN OF BLOCK a03 WITH FRAME TITLE text902.
*SELECT-OPTIONS: s2_mblnr FOR zmmdt00091-mblnr MEMORY ID zfi02r0037_s2_1 NO-EXTENSION NO INTERVALS MODIF ID m01,
SELECT-OPTIONS: s2_belnr FOR zmmdt00091-belnr MEMORY ID zfi02r0037_s2_1 MODIF ID m01,
                s2_mblnr FOR zmmdt00091-mblnr MEMORY ID zfi02r0037_s2_2 MODIF ID m01,
                s2_mjahr FOR acdoca-gjahr MEMORY ID zfi02r0037_s2_3 NO-EXTENSION NO INTERVALS MODIF ID m01,
                s2_ebeln FOR zmmdt00091-ebeln MEMORY ID zfi02r0037_s2_4 NO-EXTENSION NO INTERVALS MODIF ID m01.
SELECTION-SCREEN END OF BLOCK a03.

SELECTION-SCREEN BEGIN OF BLOCK a04 WITH FRAME TITLE text903.
*SELECT-OPTIONS: s3_belnr FOR acdoca-belnr MEMORY ID zfi02r0037_s3_1 NO-EXTENSION NO INTERVALS MODIF ID m01,
SELECT-OPTIONS: s3_belnr FOR acdoca-belnr MEMORY ID zfi02r0037_s3_1 MODIF ID m01,
                s3_gjahr FOR acdoca-gjahr MEMORY ID zfi02r0037_s3_2 NO-EXTENSION NO INTERVALS MODIF ID m01,
                s3_racct FOR acdoca-racct MEMORY ID zfi02r0037_s3_3 NO-EXTENSION NO INTERVALS MODIF ID m01.
SELECTION-SCREEN END OF BLOCK a04.

*--------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK a11 WITH FRAME TITLE text910.
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS rb11 RADIOBUTTON GROUP rb2 DEFAULT 'X' USER-COMMAND uco2 MODIF ID m02.
SELECTION-SCREEN COMMENT 4(30) text811 MODIF ID m02.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS rb12 RADIOBUTTON GROUP rb2 MODIF ID m02.
SELECTION-SCREEN COMMENT 4(30) text812 MODIF ID m02.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS rb13 RADIOBUTTON GROUP rb2 MODIF ID m02.
SELECTION-SCREEN COMMENT 4(30) text813 MODIF ID m02.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK a11.

SELECTION-SCREEN BEGIN OF BLOCK a05 WITH FRAME TITLE text904.
*SELECT-OPTIONS: s3_belnr FOR acdoca-belnr MEMORY ID zfi02r0037_s3_1 NO-EXTENSION NO INTERVALS MODIF ID m01,
SELECT-OPTIONS: s4_bukrs FOR zfidt00318-bukrs MEMORY ID zfi02r0037_s4_1 MODIF ID m03 DEFAULT gc_rbukrs,
                s4_gjahr FOR zfidt00318-gjahr MEMORY ID zfi02r0037_s4_2 NO-EXTENSION NO INTERVALS MODIF ID m03,
                s4_belnr FOR zfidt00318-belnr MEMORY ID zfi02r0037_s4_3 MODIF ID m03,
                s4_erdat FOR zfidt00318-erdat MEMORY ID zfi02r0037_s4_4 NO-EXTENSION NO INTERVALS MODIF ID m03,
                s4_gjah2 FOR zfidt00318-gjahr_reclass MEMORY ID zfi02r0037_s4_5 NO-EXTENSION NO INTERVALS MODIF ID m04,
                s4_beln2 FOR zfidt00318-belnr_reclass MEMORY ID zfi02r0037_s4_6 NO-EXTENSION NO INTERVALS MODIF ID m04.
SELECTION-SCREEN END OF BLOCK a05.
*----------------------------------------------------------------------*
* End - Selection Screen                                               *
*----------------------------------------------------------------------*
