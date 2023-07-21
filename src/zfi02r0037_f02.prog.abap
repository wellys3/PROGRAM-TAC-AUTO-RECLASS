*&---------------------------------------------------------------------*
*& Include          ZFI02R0037_F02
*&---------------------------------------------------------------------*


CLASS gcl_falv DEFINITION INHERITING FROM zcl_falv.
  PUBLIC SECTION.

  PROTECTED SECTION.

    "redefinition of event handler
    METHODS evf_user_command REDEFINITION.
    METHODS evf_top_of_page REDEFINITION.
    METHODS evf_data_changed REDEFINITION.
    METHODS evf_data_changed_finished REDEFINITION.
    METHODS evf_hotspot_click REDEFINITION.
  PRIVATE SECTION.

ENDCLASS.


CLASS gcl_falv IMPLEMENTATION.

  METHOD evf_user_command.

    CASE e_ucomm.
      WHEN zcl_falv_dynamic_status=>b_01.

        me->check_changed_data( ).

        "*--------------------------------------------------------------------*
        "Popup confirm save table

        CLEAR gd_answer.
        gd_message = 'Are you sure to save all data to ZFIDT00318?'.
        PERFORM f_confirm    USING 'Are you sure to save?'
                                   gd_message
                                   'Yes'
                                   'No'
                          CHANGING gd_answer.

        CHECK gd_answer EQ '1'.

        CLEAR gd_subrc.
        PERFORM f_save_to_table    USING gc_kind_created
                                         git_zfidt00318
                                CHANGING gd_subrc.

        "*--------------------------------------------------------------------*
        "Popup confirm resend outbond data

        IF gd_subrc EQ 0.

          SELECT a~*
            FROM @git_zfidt00318 AS a
              WHERE ind_tac_max NE @space
                INTO TABLE @DATA(lit_zfidt00318_2)
            ##itab_key_in_select
            ##itab_db_select.

          IF lit_zfidt00318_2[] IS NOT INITIAL.

            "*--------------------------------------------------------------------*

            CLEAR gd_answer.
            gd_message = 'Are you sure to send outbond data?'.
            PERFORM f_confirm    USING 'Are you sure to send?'
                                       gd_message
                                       'Yes'
                                       'No'
                              CHANGING gd_answer.

            CHECK gd_answer EQ '1'.

            PERFORM f_send_outbond USING gc_kind_send_ob
                                         git_zfidt00318[].

            "*--------------------------------------------------------------------*

          ENDIF.

          "*--------------------------------------------------------------------*
          "Get updated value from ZFIDT00318

          LOOP AT git_zfidt00318 ASSIGNING FIELD-SYMBOL(<lfs_zfidt00318>).

            SELECT SINGLE * FROM zfidt00318 INTO @DATA(lwa_zfidt00318)
              WHERE bukrs = @<lfs_zfidt00318>-bukrs AND
                    gjahr = @<lfs_zfidt00318>-gjahr AND
                    belnr = @<lfs_zfidt00318>-belnr AND
                    zeile = @<lfs_zfidt00318>-zeile AND
                    zcount = @<lfs_zfidt00318>-zcount AND
                    ind_run_post = 'X'.

            IF sy-subrc EQ 0.

              <lfs_zfidt00318>-ind_run_post = lwa_zfidt00318-ind_run_post.
              <lfs_zfidt00318>-ind_feed_tac_max = lwa_zfidt00318-ind_feed_tac_max.
              <lfs_zfidt00318>-prctr = lwa_zfidt00318-prctr.
              <lfs_zfidt00318>-zzcc = lwa_zfidt00318-zzcc.
              <lfs_zfidt00318>-zzku = lwa_zfidt00318-zzku.
              <lfs_zfidt00318>-zzcp = lwa_zfidt00318-zzcp.
              <lfs_zfidt00318>-zzpr = lwa_zfidt00318-zzpr.
              <lfs_zfidt00318>-zzph = lwa_zfidt00318-zzph.
              <lfs_zfidt00318>-zzch = lwa_zfidt00318-zzch.
              <lfs_zfidt00318>-zzpo = lwa_zfidt00318-zzpo.

            ENDIF.

          ENDLOOP.

          "*--------------------------------------------------------------------*

          me->layout->set_col_opt( iv_value = 'X' ).
          me->layout->set_cwidth_opt( iv_value = 'X' ).
          me->soft_refresh( ).

          "*--------------------------------------------------------------------*

        ENDIF.

      WHEN zcl_falv_dynamic_status=>b_02.

        "*--------------------------------------------------------------------*
        "Validation 1

        READ TABLE git_zfidt00318_2 TRANSPORTING NO FIELDS WITH KEY select = 'X'.
        IF sy-subrc EQ 0.

          "*--------------------------------------------------------------------*
          "Validation 2

          READ TABLE git_zfidt00318_2 TRANSPORTING NO FIELDS WITH KEY select = 'X'
                                                                      ind_run_post = 'X'.
          IF sy-subrc EQ 0.
            MESSAGE 'Selected data with ZFLAG_RUN_POST = ''X'' can''t be resend again' TYPE 'S' DISPLAY LIKE 'W'.
            EXIT.
          ENDIF.

          "*--------------------------------------------------------------------*
          "Popup confirm resend outbond data

          CLEAR gd_answer.
          gd_message = 'Are you sure to resend selected data?'.
          PERFORM f_confirm    USING 'Are you sure to resend?'
                                     gd_message
                                     'Yes'
                                     'No'
                            CHANGING gd_answer.

          CHECK gd_answer EQ '1'.

          "*--------------------------------------------------------------------*
          "Prepare data for pre-resend

          DATA(lit_zfidt00318) = git_zfidt00318_2[].
          DELETE lit_zfidt00318 WHERE select EQ ''.

          CLEAR git_zfidt00318[].
          MOVE-CORRESPONDING lit_zfidt00318 TO git_zfidt00318.

          PERFORM f_send_outbond USING gc_kind_resend_ob
                                       git_zfidt00318[].

          "*--------------------------------------------------------------------*
          "Get updated value from ZFIDT00318

          LOOP AT git_zfidt00318_2 ASSIGNING FIELD-SYMBOL(<lfs_zfidt00318_2>)
            WHERE select EQ 'X'.

            CLEAR lwa_zfidt00318.
            SELECT SINGLE * FROM zfidt00318 INTO lwa_zfidt00318
              WHERE bukrs = <lfs_zfidt00318_2>-bukrs AND
                    gjahr = <lfs_zfidt00318_2>-gjahr AND
                    belnr = <lfs_zfidt00318_2>-belnr AND
                    zeile = <lfs_zfidt00318_2>-zeile AND
                    zcount = <lfs_zfidt00318_2>-zcount AND
                    ind_run_post = 'X'.

            IF sy-subrc EQ 0.

              <lfs_zfidt00318_2>-select = ''.

              <lfs_zfidt00318_2>-ind_run_post = lwa_zfidt00318-ind_run_post.
              <lfs_zfidt00318_2>-ind_feed_tac_max = lwa_zfidt00318-ind_feed_tac_max.
              <lfs_zfidt00318_2>-prctr = lwa_zfidt00318-prctr.
              <lfs_zfidt00318_2>-zzcc = lwa_zfidt00318-zzcc.
              <lfs_zfidt00318_2>-zzku = lwa_zfidt00318-zzku.
              <lfs_zfidt00318_2>-zzcp = lwa_zfidt00318-zzcp.
              <lfs_zfidt00318_2>-zzpr = lwa_zfidt00318-zzpr.
              <lfs_zfidt00318_2>-zzph = lwa_zfidt00318-zzph.
              <lfs_zfidt00318_2>-zzch = lwa_zfidt00318-zzch.
              <lfs_zfidt00318_2>-zzpo = lwa_zfidt00318-zzpo.

            ENDIF.

          ENDLOOP.

          "*--------------------------------------------------------------------*
          "Refresh ALV

          me->layout->set_col_opt( iv_value = 'X' ).
          me->layout->set_cwidth_opt( iv_value = 'X' ).
          me->soft_refresh( ).

        ELSE.
          MESSAGE 'No data selected' TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.

      WHEN zcl_falv_dynamic_status=>b_03.

        "*--------------------------------------------------------------------*
        "Validation 1

*        READ TABLE git_zfidt00318_2 TRANSPORTING NO FIELDS WITH KEY select = 'X'.
        READ TABLE git_zfidt00318_3 TRANSPORTING NO FIELDS WITH KEY select = 'X'.
        IF sy-subrc EQ 0.

          "*--------------------------------------------------------------------*
          "Validation 2

*          LOOP AT git_zfidt00318_2 INTO DATA(lwa_zfidt00318_2)
          LOOP AT git_zfidt00318_3 INTO DATA(lwa_zfidt00318_3)
            WHERE select = 'X' AND
                  ( belnr_reclass NE space OR gjahr_reclass NE space ).
          ENDLOOP.
          IF sy-subrc EQ 0.
            MESSAGE 'Selected data is already posted, can''t be post again' TYPE 'S' DISPLAY LIKE 'W'.
            EXIT.
          ENDIF.

          "*--------------------------------------------------------------------*
          "Popup confirm

          CLEAR gd_answer.
          gd_message = 'Are you sure to post selected data?'.
          PERFORM f_confirm    USING 'Are you sure to post?'
                                     gd_message
                                     'Yes'
                                     'No'
                            CHANGING gd_answer.

          CHECK gd_answer EQ '1'.

          "*--------------------------------------------------------------------*
          "Prepare data for pre-posting

*          CLEAR lit_zfidt00318[].
*          lit_zfidt00318 = git_zfidt00318_2[].
          DATA(lit_zfidt00318_3) = git_zfidt00318_3[].
          DELETE lit_zfidt00318_3 WHERE select EQ ''.

*          CLEAR git_zfidt00318[].
*          MOVE-CORRESPONDING lit_zfidt00318 TO git_zfidt00318.

*          PERFORM f_pre_posting USING git_zfidt00318.
          PERFORM f_pre_posting USING lit_zfidt00318_3.

          "*--------------------------------------------------------------------*
          "Get updated value from ZFIDT00318

*          LOOP AT git_zfidt00318_2 ASSIGNING <lfs_zfidt00318_2>
          LOOP AT git_zfidt00318_3 ASSIGNING FIELD-SYMBOL(<lfs_zfidt00318_3>)
            WHERE select EQ 'X'.

            CLEAR lwa_zfidt00318.
            SELECT SINGLE * FROM zfidt00318 INTO lwa_zfidt00318
              WHERE bukrs = <lfs_zfidt00318_3>-bukrs AND
                    gjahr = <lfs_zfidt00318_3>-gjahr AND
                    belnr = <lfs_zfidt00318_3>-belnr AND
                    zeile = <lfs_zfidt00318_3>-zeile AND
                    zcount = <lfs_zfidt00318_3>-zcount AND
                    belnr_reclass NE space.

            IF sy-subrc EQ 0.

              <lfs_zfidt00318_3>-select = ''.

              <lfs_zfidt00318_3>-gjahr_reclass = lwa_zfidt00318-gjahr_reclass.
              <lfs_zfidt00318_3>-belnr_reclass = lwa_zfidt00318-belnr_reclass.

            ENDIF.

          ENDLOOP.

          "*--------------------------------------------------------------------*
          "Refresh ALV

          me->layout->set_col_opt( iv_value = 'X' ).
          me->layout->set_cwidth_opt( iv_value = 'X' ).
          me->soft_refresh( ).

          "*--------------------------------------------------------------------*

        ELSE.
          MESSAGE 'No data selected' TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.

      WHEN OTHERS.
        super->evf_user_command( e_ucomm ).
    ENDCASE.

  ENDMETHOD.

  METHOD evf_top_of_page.

    DATA: ld_text TYPE text255.

    e_dyndoc_id->add_text( text = 'PT Adira Dinamika Multifinance Tbk'
                           sap_emphasis = cl_dd_area=>strong ).

    "*--------------------------------------------------------------------*

    e_dyndoc_id->new_line( repeat = 0 ).

    "*--------------------------------------------------------------------*

    e_dyndoc_id->add_table(
      EXPORTING
        no_of_columns = 2
        border = '0'
      IMPORTING
        table               = DATA(lr_table)
    ).

    lr_table->add_column(
      EXPORTING
        width               = '10%'
      IMPORTING
        column              = DATA(lr_col1)
    ).
    lr_table->add_column(
      EXPORTING
        width               = '70%'
      IMPORTING
        column              = DATA(lr_col2)
    ).

    "*--------------------------------------------------------------------*

    CLEAR ld_text.
    ld_text = gc_report_title.
    lr_col1->add_text( text = 'Program Name:' sap_style = cl_dd_area=>standard ).
    CLEAR ld_text.
    ld_text = gc_report_title.
    lr_col2->add_text( text = ld_text sap_style = cl_dd_area=>standard ).

    "*--------------------------------------------------------------------*

    lr_table->new_row( ).
    CLEAR ld_text.
    WRITE gd_run_str TO ld_text. CONDENSE ld_text.
    CONCATENATE ld_text 'seconds' INTO ld_text SEPARATED BY space.
    lr_col1->add_text( text = 'Execution Time:' sap_style = cl_dd_area=>standard ).
    lr_col2->add_text( text = ld_text sap_style = cl_dd_area=>standard ).

    "*--------------------------------------------------------------------*

    lr_table->new_row( ).
    CLEAR ld_text.

    CASE gd_rb.
      WHEN 'RB1'.
        DESCRIBE TABLE git_zfidt00318 LINES DATA(ld_lines).
      WHEN 'RB2'.
        DESCRIBE TABLE git_zfidt00318_2 LINES ld_lines.
      WHEN 'RB3'.
        DESCRIBE TABLE git_zfidt00318_3 LINES ld_lines.
    ENDCASE.

    WRITE ld_lines TO ld_text. CONDENSE ld_text.
    CONCATENATE ld_text 'row(s)' INTO ld_text SEPARATED BY space.
    lr_col1->add_text( text = 'Total Row(s):' sap_style = cl_dd_area=>standard ).
    lr_col2->add_text( text = ld_text sap_style = cl_dd_area=>standard ).
    CLEAR ld_lines.

    "*--------------------------------------------------------------------*

    e_dyndoc_id->merge_document( ).

  ENDMETHOD.

  METHOD evf_data_changed.

    me->layout->set_col_opt( iv_value = 'X' ).
    me->layout->set_cwidth_opt( iv_value = 'X' ).
    me->soft_refresh( ).

  ENDMETHOD.

  METHOD evf_data_changed_finished.

    IF e_modified EQ 'X'.
      me->layout->set_col_opt( iv_value = 'X' ).
      me->layout->set_cwidth_opt( iv_value = 'X' ).
      me->soft_refresh( ).
    ENDIF.

  ENDMETHOD.

  METHOD evf_hotspot_click.

    DATA: ld_matnr TYPE zfidt00318-matnr.

    "*--------------------------------------------------------------------*

    CASE e_column_id-fieldname.
      WHEN 'BELNR'.

        CASE gd_rb.
          WHEN 'RB1'.
            READ TABLE git_zfidt00318 INTO DATA(lwa_zfidt00318) INDEX e_row_id-index.
            IF sy-subrc EQ 0 AND lwa_zfidt00318-belnr IS NOT INITIAL.
              SET PARAMETER ID : 'BLN' FIELD lwa_zfidt00318-belnr.
              SET PARAMETER ID : 'BUK' FIELD lwa_zfidt00318-bukrs.
              SET PARAMETER ID : 'GJR' FIELD lwa_zfidt00318-gjahr.
              CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
            ENDIF.
          WHEN 'RB2'.
            READ TABLE git_zfidt00318_2 INTO DATA(lwa_zfidt00318_2) INDEX e_row_id-index.
            IF sy-subrc EQ 0 AND lwa_zfidt00318_2-belnr IS NOT INITIAL.
              SET PARAMETER ID : 'BLN' FIELD lwa_zfidt00318_2-belnr.
              SET PARAMETER ID : 'BUK' FIELD lwa_zfidt00318_2-bukrs.
              SET PARAMETER ID : 'GJR' FIELD lwa_zfidt00318_2-gjahr.
              CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
            ENDIF.
          WHEN 'RB3'.
            READ TABLE git_zfidt00318_3 INTO DATA(lwa_zfidt00318_3) INDEX e_row_id-index.
            IF sy-subrc EQ 0 AND lwa_zfidt00318_3-belnr IS NOT INITIAL.
              SET PARAMETER ID : 'BLN' FIELD lwa_zfidt00318_3-belnr.
              SET PARAMETER ID : 'BUK' FIELD lwa_zfidt00318_3-bukrs.
              SET PARAMETER ID : 'GJR' FIELD lwa_zfidt00318_3-gjahr.
              CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
            ENDIF.
        ENDCASE.

      WHEN 'MBLNR'.

        CASE gd_rb.
          WHEN 'RB1'.

            CLEAR lwa_zfidt00318.
            READ TABLE git_zfidt00318 INTO lwa_zfidt00318 INDEX e_row_id-index.
            IF sy-subrc EQ 0 AND lwa_zfidt00318-mblnr IS NOT INITIAL.

              CALL FUNCTION 'MIGO_DIALOG'
                EXPORTING
                  i_action            = 'A04'
                  i_refdoc            = 'R02'
                  i_notree            = space
                  i_skip_first_screen = 'X'
                  i_deadend           = 'X'
                  i_okcode            = 'OK_GO'
                  i_new_rollarea      = 'X'
                  i_mblnr             = lwa_zfidt00318-mblnr
                  i_mjahr             = lwa_zfidt00318-mjahr
                EXCEPTIONS
                  illegal_combination = 1
                  OTHERS              = 2.
              IF sy-subrc <> 0.
                "Implement suitable error handling here
              ENDIF.
            ENDIF.

          WHEN 'RB2'.

            CLEAR lwa_zfidt00318_2.
            READ TABLE git_zfidt00318_2 INTO lwa_zfidt00318_2 INDEX e_row_id-index.
            IF sy-subrc EQ 0 AND lwa_zfidt00318_2-mblnr IS NOT INITIAL.

              CALL FUNCTION 'MIGO_DIALOG'
                EXPORTING
                  i_action            = 'A04'
                  i_refdoc            = 'R02'
                  i_notree            = space
                  i_skip_first_screen = 'X'
                  i_deadend           = 'X'
                  i_okcode            = 'OK_GO'
                  i_new_rollarea      = 'X'
                  i_mblnr             = lwa_zfidt00318_2-mblnr
                  i_mjahr             = lwa_zfidt00318_2-mjahr
                EXCEPTIONS
                  illegal_combination = 1
                  OTHERS              = 2.
              IF sy-subrc <> 0.
                "Implement suitable error handling here
              ENDIF.
            ENDIF.

          WHEN 'RB3'.

            CLEAR lwa_zfidt00318_3.
            READ TABLE git_zfidt00318_3 INTO lwa_zfidt00318_3 INDEX e_row_id-index.
            IF sy-subrc EQ 0 AND lwa_zfidt00318_3-mblnr IS NOT INITIAL.

              CALL FUNCTION 'MIGO_DIALOG'
                EXPORTING
                  i_action            = 'A04'
                  i_refdoc            = 'R02'
                  i_notree            = space
                  i_skip_first_screen = 'X'
                  i_deadend           = 'X'
                  i_okcode            = 'OK_GO'
                  i_new_rollarea      = 'X'
                  i_mblnr             = lwa_zfidt00318_3-mblnr
                  i_mjahr             = lwa_zfidt00318_3-mjahr
                EXCEPTIONS
                  illegal_combination = 1
                  OTHERS              = 2.
              IF sy-subrc <> 0.
                "Implement suitable error handling here
              ENDIF.
            ENDIF.

        ENDCASE.

      WHEN 'MATNR'.

        CLEAR ld_matnr.
        CASE gd_rb.
          WHEN 'RB1'.

            CLEAR lwa_zfidt00318.
            READ TABLE git_zfidt00318 INTO lwa_zfidt00318 INDEX e_row_id-index.
            IF sy-subrc EQ 0 AND lwa_zfidt00318-matnr IS NOT INITIAL.
              ld_matnr = lwa_zfidt00318-matnr.
            ENDIF.

          WHEN 'RB2'.

            CLEAR lwa_zfidt00318_2.
            READ TABLE git_zfidt00318_2 INTO lwa_zfidt00318_2 INDEX e_row_id-index.
            IF sy-subrc EQ 0 AND lwa_zfidt00318_2-matnr IS NOT INITIAL.
              ld_matnr = lwa_zfidt00318_2-matnr.
            ENDIF.

          WHEN 'RB3'.

            CLEAR lwa_zfidt00318_3.
            READ TABLE git_zfidt00318_3 INTO lwa_zfidt00318_3 INDEX e_row_id-index.
            IF sy-subrc EQ 0 AND lwa_zfidt00318_3-matnr IS NOT INITIAL.
              ld_matnr = lwa_zfidt00318_3-matnr.
            ENDIF.

        ENDCASE.

        CHECK ld_matnr IS NOT INITIAL.

        PERFORM f_call_trans_mm03 USING ld_matnr.

      WHEN 'BELNR_PPH'.

        CASE gd_rb.
          WHEN 'RB1'.

            CLEAR lwa_zfidt00318.
            READ TABLE git_zfidt00318 INTO lwa_zfidt00318 INDEX e_row_id-index.
            IF sy-subrc EQ 0 AND lwa_zfidt00318-belnr_pph IS NOT INITIAL.
              SET PARAMETER ID : 'BLN' FIELD lwa_zfidt00318-belnr_pph.
              SET PARAMETER ID : 'BUK' FIELD lwa_zfidt00318-bukrs.
              SET PARAMETER ID : 'GJR' FIELD lwa_zfidt00318-gjahr_pph.
              CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
            ENDIF.

          WHEN 'RB2'.

            CLEAR lwa_zfidt00318_2.
            READ TABLE git_zfidt00318_2 INTO lwa_zfidt00318_2 INDEX e_row_id-index.
            IF sy-subrc EQ 0 AND lwa_zfidt00318_2-belnr_pph IS NOT INITIAL.
              SET PARAMETER ID : 'BLN' FIELD lwa_zfidt00318_2-belnr_pph.
              SET PARAMETER ID : 'BUK' FIELD lwa_zfidt00318_2-bukrs.
              SET PARAMETER ID : 'GJR' FIELD lwa_zfidt00318_2-gjahr_pph.
              CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
            ENDIF.

          WHEN 'RB3'.

            CLEAR lwa_zfidt00318_3.
            READ TABLE git_zfidt00318_3 INTO lwa_zfidt00318_3 INDEX e_row_id-index.
            IF sy-subrc EQ 0 AND lwa_zfidt00318_3-belnr_pph IS NOT INITIAL.
              SET PARAMETER ID : 'BLN' FIELD lwa_zfidt00318_3-belnr_pph.
              SET PARAMETER ID : 'BUK' FIELD lwa_zfidt00318_3-bukrs.
              SET PARAMETER ID : 'GJR' FIELD lwa_zfidt00318_3-gjahr_pph.
              CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
            ENDIF.
        ENDCASE.

      WHEN 'BELNR_RECLASS'.

        CLEAR lwa_zfidt00318_3.
        READ TABLE git_zfidt00318_3 INTO lwa_zfidt00318_3 INDEX e_row_id-index.
        IF sy-subrc EQ 0 AND lwa_zfidt00318_3-belnr_reclass IS NOT INITIAL.

          SET PARAMETER ID : 'BLN' FIELD lwa_zfidt00318_3-belnr_reclass.
          SET PARAMETER ID : 'BUK' FIELD lwa_zfidt00318_3-bukrs.
          SET PARAMETER ID : 'GJR' FIELD lwa_zfidt00318_3-gjahr_reclass.
          CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.

        ENDIF.

    ENDCASE.

  ENDMETHOD.

ENDCLASS.


*&---------------------------------------------------------------------*
*& Form F_DISPLAY_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GIT_DATA
*&---------------------------------------------------------------------*
FORM f_display_data  USING  p_git_zfidt00318 TYPE gtt_zfidt00318
                            p_git_zfidt00318_2 TYPE gtt_zfidt00318_2
                            p_git_zfidt00318_3 TYPE gtt_zfidt00318_3.

  "FALV creation with only table passed
*  DATA(lcl_falv) = zcl_falv=>create( CHANGING ct_table = p_git_data ).

  "Creation of falv with local redefinition
  DATA lcl_falv TYPE REF TO gcl_falv.

  CASE gd_rb.
    WHEN 'RB1'.

      lcl_falv ?= gcl_falv=>create( EXPORTING  i_subclass = cl_abap_classdescr=>describe_by_name( p_name = 'GCL_FALV' )
                                    CHANGING ct_table = p_git_zfidt00318 ) .

      "Add title variable
      lcl_falv->title_v1 = gc_report_title.

      "Set checkbox
      "lcl_falv->column( 'SELECT' )->set_edit( abap_true ). "Set checkbox editable
      "lcl_falv->column( 'SELECT' )->set_col_opt( iv_value = 'X' ). "Set column optimization

      "Set hotpsot
      lcl_falv->column( 'BELNR' )->set_hotspot( 'X' ). "Set hotspot
      lcl_falv->column( 'MBLNR' )->set_hotspot( 'X' ). "Set hotspot
      lcl_falv->column( 'MATNR' )->set_hotspot( 'X' ). "Set hotspot
      lcl_falv->column( 'BELNR_PPH' )->set_hotspot( 'X' ). "Set hotspot
      lcl_falv->column( 'BELNR_RECLASS' )->set_hotspot( 'X' ). "Set hotspot

      "Set layout
      lcl_falv->layout->set_zebra( iv_value = 'X' ). "Set zebra
      lcl_falv->layout->set_col_opt( iv_value = 'X' ).
      lcl_falv->layout->set_cwidth_opt( iv_value = 'X' ).
      lcl_falv->layout->set_totals_bef( 'X' ). "Set sum on Top
      lcl_falv->layout->set_sel_mode( 'A' ).

      "Set Gui status to fully dynamic (no standard buttons of ALV Grid)
      "lcl_falv->gui_status->fully_dynamic = abap_true.

      "Modify field
      PERFORM f_modify_field USING lcl_falv.

      "Add button
      PERFORM f_add_button USING lcl_falv.

      "Change grid to edit mode
      "lcl_falv->set_editable( iv_modify = abap_true ).

      "Set size top of page
      lcl_falv->top_of_page_height = 75.

      "Display full screen grid
      lcl_falv->show_top_of_page( )->display( ).

    WHEN 'RB2'.

      lcl_falv ?= gcl_falv=>create( EXPORTING  i_subclass = cl_abap_classdescr=>describe_by_name( p_name = 'GCL_FALV' )
                                    CHANGING ct_table = p_git_zfidt00318_2 ) .

      "Add title variable
      lcl_falv->title_v1 = gc_report_title.

      "Set checkbox
      lcl_falv->set_mark_field( 'SELECT' ).
      lcl_falv->column( 'SELECT' )->set_edit( abap_true ). "Set checkbox editable
      lcl_falv->column( 'SELECT' )->set_col_opt( iv_value = 'X' ). "Set column optimization

      "Set hotspot
      lcl_falv->column( 'BELNR' )->set_hotspot( 'X' ). "Set hotspot
      lcl_falv->column( 'MBLNR' )->set_hotspot( 'X' ). "Set hotspot
      lcl_falv->column( 'MATNR' )->set_hotspot( 'X' ). "Set hotspot
      lcl_falv->column( 'BELNR_PPH' )->set_hotspot( 'X' ). "Set hotspot
      lcl_falv->column( 'BELNR_RECLASS' )->set_hotspot( 'X' ). "Set hotspot

      "Set layout
      lcl_falv->layout->set_zebra( iv_value = 'X' ). "Set zebra
      lcl_falv->layout->set_col_opt( iv_value = 'X' ).
      lcl_falv->layout->set_cwidth_opt( iv_value = 'X' ).
      lcl_falv->layout->set_totals_bef( 'X' ). "Set sum on Top
      lcl_falv->layout->set_sel_mode( 'A' ).

      "Set Gui status to fully dynamic (no standard buttons of ALV Grid)
      "lcl_falv->gui_status->fully_dynamic = abap_true.

      "Modify field
      PERFORM f_modify_field USING lcl_falv.

      "Add button
      PERFORM f_add_button USING lcl_falv.

      "Change grid to edit mode
      lcl_falv->set_editable( iv_modify = abap_true ).

      "Set size top of page
      lcl_falv->top_of_page_height = 75.

      "Display full screen grid
      lcl_falv->show_top_of_page( )->display( ).

    WHEN 'RB3'.

      CASE gd_rb_2.
        WHEN 'RB11'.

          lcl_falv ?= gcl_falv=>create( EXPORTING  i_subclass = cl_abap_classdescr=>describe_by_name( p_name = 'GCL_FALV' )
                                        CHANGING ct_table = p_git_zfidt00318_3 ) .

          "Add title variable
          lcl_falv->title_v1 = gc_report_title.

          "Set checkbox
          lcl_falv->set_mark_field( 'SELECT' ).
          lcl_falv->column( 'SELECT' )->set_edit( abap_true ). "Set checkbox editable
          lcl_falv->column( 'SELECT' )->set_col_opt( iv_value = 'X' ). "Set column optimization

          "Set hotspot
          lcl_falv->column( 'BELNR' )->set_hotspot( 'X' ). "Set hotspot
          lcl_falv->column( 'MBLNR' )->set_hotspot( 'X' ). "Set hotspot
          lcl_falv->column( 'MATNR' )->set_hotspot( 'X' ). "Set hotspot
          lcl_falv->column( 'BELNR_PPH' )->set_hotspot( 'X' ). "Set hotspot
          lcl_falv->column( 'BELNR_RECLASS' )->set_hotspot( 'X' ). "Set hotspot

          "Set layout
          lcl_falv->layout->set_zebra( iv_value = 'X' ). "Set zebra
          lcl_falv->layout->set_col_opt( iv_value = 'X' ).
          lcl_falv->layout->set_cwidth_opt( iv_value = 'X' ).
          lcl_falv->layout->set_totals_bef( 'X' ). "Set sum on Top
          lcl_falv->layout->set_sel_mode( 'A' ).

          "Set Gui status to fully dynamic (no standard buttons of ALV Grid)
          "lcl_falv->gui_status->fully_dynamic = abap_true.

          "Modify field
          PERFORM f_modify_field USING lcl_falv.

          "Add button
          PERFORM f_add_button USING lcl_falv.

          "Change grid to edit mode
          lcl_falv->set_editable( iv_modify = abap_true ).

          "Set size top of page
          lcl_falv->top_of_page_height = 75.

          "Display full screen grid
          lcl_falv->show_top_of_page( )->display( ).

        WHEN 'RB12'.

          lcl_falv ?= gcl_falv=>create( EXPORTING  i_subclass = cl_abap_classdescr=>describe_by_name( p_name = 'GCL_FALV' )
                                        CHANGING ct_table = p_git_zfidt00318_3 ) .

          "Add title variable
          lcl_falv->title_v1 = gc_report_title.

          "Set checkbox
          "lcl_falv->set_mark_field( 'SELECT' ).
          "lcl_falv->column( 'SELECT' )->set_edit( abap_true ). "Set checkbox editable
          "lcl_falv->column( 'SELECT' )->set_col_opt( iv_value = 'X' ). "Set column optimization

          "Set hotspot
          lcl_falv->column( 'BELNR' )->set_hotspot( 'X' ). "Set hotspot
          lcl_falv->column( 'MBLNR' )->set_hotspot( 'X' ). "Set hotspot
          lcl_falv->column( 'MATNR' )->set_hotspot( 'X' ). "Set hotspot
          lcl_falv->column( 'BELNR_PPH' )->set_hotspot( 'X' ). "Set hotspot
          lcl_falv->column( 'BELNR_RECLASS' )->set_hotspot( 'X' ). "Set hotspot

          "Set layout
          lcl_falv->layout->set_zebra( iv_value = 'X' ). "Set zebra
          lcl_falv->layout->set_col_opt( iv_value = 'X' ).
          lcl_falv->layout->set_cwidth_opt( iv_value = 'X' ).
          lcl_falv->layout->set_totals_bef( 'X' ). "Set sum on Top
          lcl_falv->layout->set_sel_mode( 'A' ).

          "Set Gui status to fully dynamic (no standard buttons of ALV Grid)
          "lcl_falv->gui_status->fully_dynamic = abap_true.

          "Modify field
          PERFORM f_modify_field USING lcl_falv.

          "Add button
          PERFORM f_add_button USING lcl_falv.

          "Change grid to edit mode
          "lcl_falv->set_editable( iv_modify = abap_true ).

          "Set size top of page
          lcl_falv->top_of_page_height = 75.

          "Display full screen grid
          lcl_falv->show_top_of_page( )->display( ).

        WHEN 'RB13'.

          lcl_falv ?= gcl_falv=>create( EXPORTING  i_subclass = cl_abap_classdescr=>describe_by_name( p_name = 'GCL_FALV' )
                                        CHANGING ct_table = p_git_zfidt00318_3 ) .

          "Add title variable
          lcl_falv->title_v1 = gc_report_title.

          "Set checkbox
          "lcl_falv->set_mark_field( 'SELECT' ).
          "lcl_falv->column( 'SELECT' )->set_edit( abap_true ). "Set checkbox editable
          "lcl_falv->column( 'SELECT' )->set_col_opt( iv_value = 'X' ). "Set column optimization

          "Set hotspot
          "lcl_falv->column( 'BELNR_RECLASS' )->set_hotspot( 'X' ). "Set hotspot

          "Set layout
          lcl_falv->layout->set_zebra( iv_value = 'X' ). "Set zebra
          lcl_falv->layout->set_col_opt( iv_value = 'X' ).
          lcl_falv->layout->set_cwidth_opt( iv_value = 'X' ).
          lcl_falv->layout->set_totals_bef( 'X' ). "Set sum on Top
          lcl_falv->layout->set_sel_mode( 'A' ).

          "Set Gui status to fully dynamic (no standard buttons of ALV Grid)
          "lcl_falv->gui_status->fully_dynamic = abap_true.

          "Modify field
          PERFORM f_modify_field USING lcl_falv.

          "Add button
          PERFORM f_add_button USING lcl_falv.

          "Change grid to edit mode
          "lcl_falv->set_editable( iv_modify = abap_true ).

          "Set size top of page
          lcl_falv->top_of_page_height = 75.

          "Display full screen grid
          lcl_falv->show_top_of_page( )->display( ).

      ENDCASE.

  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_ADD_BUTTON
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> FALV
*&---------------------------------------------------------------------*
FORM f_add_button USING p_me TYPE REF TO gcl_falv.

  CASE gd_rb.
    WHEN 'RB1'.

      p_me->gui_status->add_button(
        EXPORTING
          iv_button              = zcl_falv_dynamic_status=>b_01
          iv_text                = 'Save to Table'
          iv_icon                = icon_system_save
          iv_qinfo               = 'Save to Table ZFIDT00318'
        EXCEPTIONS
          button_already_filled  = 1
          button_does_not_exists = 2
          icon_and_text_empty    = 3
          OTHERS                 = 4
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    WHEN 'RB2'.

      p_me->gui_status->add_button(
        EXPORTING
          iv_button              = zcl_falv_dynamic_status=>b_02
          iv_text                = 'Resend'
          iv_icon                = icon_transportation_mode
          iv_qinfo               = 'Resend Outbond Data'
        EXCEPTIONS
          button_already_filled  = 1
          button_does_not_exists = 2
          icon_and_text_empty    = 3
          OTHERS                 = 4
      ).
      IF sy-subrc <> 0.
        MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
      ENDIF.

    WHEN 'RB3'.

      CASE gd_rb_2.
        WHEN 'RB11'.

          p_me->gui_status->add_button(
            EXPORTING
              iv_button              = zcl_falv_dynamic_status=>b_03
              iv_text                = 'Posting'
              iv_icon                = icon_execute_object
              iv_qinfo               = 'Posting / Posting with Clearing'
            EXCEPTIONS
              button_already_filled  = 1
              button_does_not_exists = 2
              icon_and_text_empty    = 3
              OTHERS                 = 4
          ).
          IF sy-subrc <> 0.
            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
          ENDIF.

        WHEN 'RB12'.

        WHEN 'RB13'.

      ENDCASE.

  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_MODIFY_FIELD
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> FALV
*&---------------------------------------------------------------------*
FORM f_modify_field USING p_me TYPE REF TO gcl_falv.

  CASE gd_rb.
    WHEN 'RB1'.

      p_me->column( 'MANDT' )->set_no_out( 'X' ).

      p_me->column( 'MANDT' )->set_key( '' ).
      p_me->column( 'BUKRS' )->set_key( '' ).
      p_me->column( 'BELNR' )->set_key( '' ).
      p_me->column( 'GJAHR' )->set_key( '' ).
      p_me->column( 'ZEILE' )->set_key( '' ).
      p_me->column( 'ZCOUNT' )->set_key( '' ).
      p_me->column( 'ERDAT' )->set_key( '' ).

    WHEN 'RB2'.

      p_me->column( 'MANDT' )->set_no_out( 'X' ).

      p_me->column( 'MANDT' )->set_key( '' ).
      p_me->column( 'BUKRS' )->set_key( '' ).
      p_me->column( 'BELNR' )->set_key( '' ).
      p_me->column( 'GJAHR' )->set_key( '' ).
      p_me->column( 'ERDAT' )->set_key( '' ).
      p_me->column( 'ZCOUNT' )->set_key( '' ).
      p_me->column( 'ERDAT' )->set_key( '' ).

    WHEN 'RB3'.

      CASE gd_rb_2.
        WHEN 'RB11'.

          p_me->column( 'BUKRS' )->set_key( '' ).
          p_me->column( 'GJAHR' )->set_key( '' ).
          p_me->column( 'BELNR' )->set_key( '' ).
          p_me->column( 'ZEILE' )->set_key( '' ).
          p_me->column( 'ZCOUNT' )->set_key( '' ).

        WHEN 'RB12'.

          p_me->column( 'SELECT' )->set_no_out( 'X' ).

          p_me->column( 'BUKRS' )->set_key( '' ).
          p_me->column( 'GJAHR' )->set_key( '' ).
          p_me->column( 'BELNR' )->set_key( '' ).
          p_me->column( 'ZEILE' )->set_key( '' ).
          p_me->column( 'ZCOUNT' )->set_key( '' ).

        WHEN 'RB13'.

          p_me->column( 'SELECT' )->set_no_out( 'X' ).

          p_me->column( 'BUKRS' )->set_key( '' ).
          p_me->column( 'GJAHR' )->set_key( '' ).
          p_me->column( 'BELNR' )->set_key( '' ).
          p_me->column( 'ZEILE' )->set_key( '' ).
          p_me->column( 'ZCOUNT' )->set_key( '' ).

      ENDCASE.

  ENDCASE.

ENDFORM.
