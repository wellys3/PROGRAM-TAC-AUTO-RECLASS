*&---------------------------------------------------------------------*
*& Include          ZFI02R0037_F01
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form F_PRE_EXECUTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_pre_execute CHANGING p_subrc.

*--------------------------------------------------------------------*
*Get Table Maintain ZFIDT00302

  CLEAR git_zfidt00317[].
  SELECT * FROM zficd_zfidt00317 INTO TABLE @git_zfidt00317.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_EXECUTE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_execute .

*--------------------------------------------------------------------*

  CASE gd_rb.
    WHEN 'RB1'.

      PERFORM f_start_timer.

      PERFORM f_get_data CHANGING git_zmmdt00091[]
                                  git_bseg[].

      IF git_zmmdt00091[] IS INITIAL AND
         git_bseg[] IS INITIAL.
        MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
      ELSE.

        PERFORM f_prepare_data    USING git_zmmdt00091[]
                                        git_bseg[]
                               CHANGING git_zfidt00318[].

        IF git_zfidt00318[] IS NOT INITIAL.

          "*--------------------------------------------------------------------*

          IF sy-batch EQ 'X'.

            CLEAR gd_subrc.
            PERFORM f_save_to_table    USING gc_kind_created
                                             git_zfidt00318
                                    CHANGING gd_subrc.

            IF gd_subrc EQ 0.

              PERFORM f_send_outbond USING gc_kind_send_ob
                                           git_zfidt00318[].

            ENDIF.

          ENDIF.

          "*--------------------------------------------------------------------*

          PERFORM f_stop_timer.

          "This program sucessfully executed! (Exec. Time & seconds)
          WAIT UP TO 1 SECONDS.
          CLEAR gd_message.
          MESSAGE s071(zfimsg) WITH gd_run_str INTO gd_message.
          PERFORM f_progress_bar_single USING gd_message 'S' 'S'.

          "*--------------------------------------------------------------------*

          PERFORM f_display_data USING git_zfidt00318[]
                                       git_zfidt00318_2[]
                                       git_zfidt00318_3[].
        ELSE.
          MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
        ENDIF.

      ENDIF.

    WHEN 'RB2'.

      PERFORM f_start_timer.

      PERFORM f_get_data_2 CHANGING git_zfidt00318_2[].

      PERFORM f_stop_timer.

      "*--------------------------------------------------------------------*

      "This program sucessfully executed! (Exec. Time & seconds)
      WAIT UP TO 1 SECONDS.
      CLEAR gd_message.
      MESSAGE s071(zfimsg) WITH gd_run_str INTO gd_message.
      PERFORM f_progress_bar_single USING gd_message 'S' 'S'.

      "*--------------------------------------------------------------------*

      IF git_zfidt00318_2[] IS NOT INITIAL.
        PERFORM f_display_data USING git_zfidt00318[]
                                     git_zfidt00318_2[]
                                     git_zfidt00318_3[].
      ELSE.
        MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
      ENDIF.

    WHEN 'RB3'.

      CASE gd_rb_2.
        WHEN 'RB11'.

          PERFORM f_start_timer.

          PERFORM f_get_data_3 CHANGING git_zfidt00318_3[].

          PERFORM f_stop_timer.

          "*--------------------------------------------------------------------*

          "This program sucessfully executed! (Exec. Time & seconds)
          WAIT UP TO 1 SECONDS.
          CLEAR gd_message.
          MESSAGE s071(zfimsg) WITH gd_run_str INTO gd_message.
          PERFORM f_progress_bar_single USING gd_message 'S' 'S'.

          "*--------------------------------------------------------------------*

          IF git_zfidt00318_3[] IS NOT INITIAL.
            PERFORM f_display_data USING git_zfidt00318[]
                                         git_zfidt00318_2[]
                                         git_zfidt00318_3[].

          ELSE.
            MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
          ENDIF.

        WHEN 'RB12'.

          PERFORM f_start_timer.

          PERFORM f_get_data_3 CHANGING git_zfidt00318_3[].

          PERFORM f_stop_timer.

          "*--------------------------------------------------------------------*

          "This program sucessfully executed! (Exec. Time & seconds)
          WAIT UP TO 1 SECONDS.
          CLEAR gd_message.
          MESSAGE s071(zfimsg) WITH gd_run_str INTO gd_message.
          PERFORM f_progress_bar_single USING gd_message 'S' 'S'.

          "*--------------------------------------------------------------------*

          IF git_zfidt00318_3[] IS NOT INITIAL.
            PERFORM f_display_data USING git_zfidt00318[]
                                         git_zfidt00318_2[]
                                         git_zfidt00318_3[].
          ELSE.
            MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
          ENDIF.

        WHEN 'RB13'.

          IF sy-batch EQ ''.
            MESSAGE 'This option can''t be running with foreground' TYPE 'S' DISPLAY LIKE 'W'.
            EXIT.
          ENDIF.

          PERFORM f_start_timer.

          PERFORM f_get_data_3 CHANGING git_zfidt00318_3[].

          IF git_zfidt00318_3[] IS NOT INITIAL.

            PERFORM f_pre_posting USING git_zfidt00318_3.

            PERFORM f_stop_timer.

            "This program sucessfully executed! (Exec. Time & seconds)
            WAIT UP TO 1 SECONDS.
            CLEAR gd_message.
            MESSAGE s071(zfimsg) WITH gd_run_str INTO gd_message.
            PERFORM f_progress_bar_single USING gd_message 'S' 'S'.

            "*--------------------------------------------------------------------*


            PERFORM f_display_data USING git_zfidt00318[]
                                         git_zfidt00318_2[]
                                         git_zfidt00318_3[].
          ELSE.
            MESSAGE 'No data found' TYPE 'S' DISPLAY LIKE 'W'.
          ENDIF.

      ENDCASE.

  ENDCASE.

*--------------------------------------------------------------------*

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GIT_ZFIDT00317[]
*&      <-- GIT_ZMMDT00091[]
*&      <-- GIT_BSEG[]
*&---------------------------------------------------------------------*
FORM f_get_data  CHANGING p_git_zmmdt00091 TYPE gtt_zmmdt00091
                          p_git_bseg TYPE gtt_bseg.


  "*--------------------------------------------------------------------*
  "Get Data ZMMDT00091

  PERFORM f_progress_bar_single USING 'Getting data from ZMMDT00091...' 'S' 'S'.

  "*--------------------------------------------------------------------*

  SELECT a~*
    FROM @git_zfidt00317 AS a
      WHERE ind_pajak_hadiah EQ 'X'
        INTO TABLE @DATA(lit_zfidt00317)
    ##itab_key_in_select
    ##itab_db_select.

  SORT lit_zfidt00317 ASCENDING BY racct.

  "*--------------------------------------------------------------------*

  IF lit_zfidt00317[] IS NOT INITIAL.

    CLEAR: gra_racct[], gra_matnr[], gra_blart[].
    LOOP AT lit_zfidt00317 INTO DATA(lwa_zfidt00317).
      IF lwa_zfidt00317-racct IS NOT INITIAL.
        f_fill_range: gra_racct 'I' 'EQ' lwa_zfidt00317-racct ''.
      ENDIF.

      IF lwa_zfidt00317-matnr IS NOT INITIAL.
        f_fill_range: gra_matnr 'I' 'EQ' lwa_zfidt00317-matnr ''.
      ENDIF.

      IF lwa_zfidt00317-blart IS NOT INITIAL.
        f_fill_range: gra_blart 'I' 'EQ' lwa_zfidt00317-blart ''.
      ENDIF.
    ENDLOOP.

    SORT gra_racct ASCENDING BY low.
    DELETE ADJACENT DUPLICATES FROM gra_racct COMPARING low.

    SORT gra_matnr ASCENDING BY low.
    DELETE ADJACENT DUPLICATES FROM gra_matnr COMPARING low.

    SORT gra_blart ASCENDING BY low.
    DELETE ADJACENT DUPLICATES FROM gra_blart COMPARING low.

    "***

    CLEAR git_named_seltabs[].

    IF s2_belnr[] IS INITIAL.

      IF s2_mblnr[] IS NOT INITIAL.
        CLEAR gwa_named_seltabs.
        gwa_named_seltabs-name = 'MBLNR'.
        gwa_named_seltabs-dref = REF #( s2_mblnr[] ).
        APPEND gwa_named_seltabs TO git_named_seltabs.
      ENDIF.

      IF s2_mjahr[] IS NOT INITIAL.
        CLEAR gwa_named_seltabs.
        gwa_named_seltabs-name = 'MJAHR'.
        gwa_named_seltabs-dref = REF #( s2_mjahr[] ).
        APPEND gwa_named_seltabs TO git_named_seltabs.
      ENDIF.

    ELSE.

      IF s2_belnr[] IS NOT INITIAL.
        CLEAR gwa_named_seltabs.
        gwa_named_seltabs-name = 'BELNR'.
        gwa_named_seltabs-dref = REF #( s2_belnr[] ).
        APPEND gwa_named_seltabs TO git_named_seltabs.
      ENDIF.

      IF s2_mjahr[] IS NOT INITIAL.
        CLEAR gwa_named_seltabs.
        gwa_named_seltabs-name = 'GJAHR'.
        gwa_named_seltabs-dref = REF #( s2_mjahr[] ).
        APPEND gwa_named_seltabs TO git_named_seltabs.
      ENDIF.

    ENDIF.

    IF s2_ebeln[] IS NOT INITIAL.
      CLEAR gwa_named_seltabs.
      gwa_named_seltabs-name = 'EBELN'.
      gwa_named_seltabs-dref = REF #( s2_ebeln[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.
    ENDIF.

    IF gra_racct[] IS NOT INITIAL.
      CLEAR gwa_named_seltabs.
      gwa_named_seltabs-name = 'GL_BIAYA'.
      gwa_named_seltabs-dref = REF #( gra_racct[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.
    ENDIF.

    IF gra_matnr[] IS NOT INITIAL.
      CLEAR gwa_named_seltabs.
      gwa_named_seltabs-name = 'MATNR'.
      gwa_named_seltabs-dref = REF #( gra_matnr[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.
    ENDIF.

    IF gra_blart[] IS NOT INITIAL.
      CLEAR gwa_named_seltabs.
      gwa_named_seltabs-name = 'BLART'.
      gwa_named_seltabs-dref = REF #( gra_blart[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.
    ENDIF.

    "***

    TRY.
        CLEAR gd_where.
        gd_where = cl_shdb_seltab=>combine_seltabs(
          EXPORTING it_named_seltabs = git_named_seltabs[]
                    iv_client_field = 'MANDT'
        ).
      CATCH cx_shdb_exception.
    ENDTRY.

    "***

    TRY.
        CLEAR p_git_zmmdt00091[].
        CALL METHOD zficl_amdp_014=>get_zmmdt00091
          EXPORTING
            im_where      = gd_where
          IMPORTING
            et_zmmdt00091 = p_git_zmmdt00091.
      CATCH cx_amdp_error. " Exceptions when calling AMDP methods
    ENDTRY.

  ENDIF.

  "*--------------------------------------------------------------------*
  "Get Data BSEG

  WAIT UP TO 1 SECONDS.
  PERFORM f_progress_bar_single USING 'Getting data from BSEG...' 'S' 'S'.

  "*--------------------------------------------------------------------*

  CLEAR lit_zfidt00317[].
  SELECT a~*
    FROM @git_zfidt00317 AS a
      WHERE ind_pajak_hadiah EQ '' AND
            racct IN @s3_racct
        INTO TABLE @lit_zfidt00317
    ##itab_key_in_select
    ##itab_db_select.

  SORT lit_zfidt00317 ASCENDING BY racct.

  "*--------------------------------------------------------------------*

  IF lit_zfidt00317[] IS NOT INITIAL.

    CLEAR: gra_racct[], gra_matnr[], gra_blart[].
    LOOP AT lit_zfidt00317 INTO lwa_zfidt00317.
      IF lwa_zfidt00317-racct IS NOT INITIAL.
        f_fill_range: gra_racct 'I' 'EQ' lwa_zfidt00317-racct ''.
      ENDIF.

*****      IF lwa_zfidt00317-matnr IS NOT INITIAL.
*****        f_fill_range: gra_matnr 'I' 'EQ' lwa_zfidt00317-matnr ''.
*****      ENDIF.

      IF lwa_zfidt00317-shkzg IS NOT INITIAL.
        f_fill_range: gra_shkzg 'I' 'EQ' lwa_zfidt00317-shkzg ''.
      ENDIF.

      IF lwa_zfidt00317-blart IS NOT INITIAL.
        f_fill_range: gra_blart 'I' 'EQ' lwa_zfidt00317-blart ''.
      ENDIF.
    ENDLOOP.

    SORT gra_racct ASCENDING BY low.
    DELETE ADJACENT DUPLICATES FROM gra_racct COMPARING low.

*****    SORT gra_matnr ASCENDING BY low.
*****    DELETE ADJACENT DUPLICATES FROM gra_matnr COMPARING low.

    SORT gra_shkzg ASCENDING BY low.
    DELETE ADJACENT DUPLICATES FROM gra_shkzg COMPARING low.

    SORT gra_blart ASCENDING BY low.
    DELETE ADJACENT DUPLICATES FROM gra_blart COMPARING low.

    "***

    CLEAR git_named_seltabs[].
    IF s1_bukrs[] IS NOT INITIAL.
      CLEAR gwa_named_seltabs.
      gwa_named_seltabs-name = 'BUKRS'.
      gwa_named_seltabs-dref = REF #( s1_bukrs[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.

    ENDIF.

    IF s3_belnr[] IS NOT INITIAL.
      CLEAR gwa_named_seltabs.
      gwa_named_seltabs-name = 'BELNR'.
      gwa_named_seltabs-dref = REF #( s3_belnr[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.
    ENDIF.

    IF s3_gjahr[] IS NOT INITIAL.
      CLEAR gwa_named_seltabs.
      gwa_named_seltabs-name = 'GJAHR'.
      gwa_named_seltabs-dref = REF #( s3_gjahr[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.
    ENDIF.

    IF gra_racct[] IS NOT INITIAL.
      CLEAR gwa_named_seltabs.
      gwa_named_seltabs-name = 'HKONT'.
      gwa_named_seltabs-dref = REF #( gra_racct[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.
    ENDIF.

*****    IF gra_matnr[] IS NOT INITIAL.
*****      CLEAR gwa_named_seltabs.
*****      gwa_named_seltabs-name = 'MATNR'.
*****      gwa_named_seltabs-dref = REF #( gra_matnr[] ).
*****      APPEND gwa_named_seltabs TO git_named_seltabs.
*****    ENDIF.

    IF gra_shkzg[] IS NOT INITIAL.
      CLEAR gwa_named_seltabs.
      gwa_named_seltabs-name = 'SHKZG'.
      gwa_named_seltabs-dref = REF #( gra_shkzg[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.
    ENDIF.

    IF gra_blart[] IS NOT INITIAL.
      CLEAR gwa_named_seltabs.
      gwa_named_seltabs-name = 'BLART'.
      gwa_named_seltabs-dref = REF #( gra_blart[] ).
      APPEND gwa_named_seltabs TO git_named_seltabs.
    ENDIF.

    "***

    TRY.
        CLEAR gd_where.
        gd_where = cl_shdb_seltab=>combine_seltabs(
          EXPORTING it_named_seltabs = git_named_seltabs[]
                    iv_client_field = 'MANDT'
        ).
      CATCH cx_shdb_exception.
    ENDTRY.

    "***

    TRY.
        CLEAR p_git_bseg[].
        CALL METHOD zficl_amdp_014=>get_bseg
          EXPORTING
            im_where = gd_where
          IMPORTING
            et_bseg  = p_git_bseg.
      CATCH cx_amdp_error. " Exceptions when calling AMDP methods
    ENDTRY.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_PREPARE_DATA
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GIT_ZMMDT00091[]
*&      --> GIT_BSEG[]
*&---------------------------------------------------------------------*
FORM f_prepare_data  USING    p_git_zmmdt00091 TYPE gtt_zmmdt00091
                              p_git_bseg TYPE gtt_bseg
                     CHANGING p_git_zfidt00318 TYPE gtt_zfidt00318.

  "*--------------------------------------------------------------------*
  "Prepare Data ZMMDT00091

  WAIT UP TO 1 SECONDS.
  PERFORM f_progress_bar_single USING 'Preparing data...' 'S' 'S'.

  IF p_git_zmmdt00091[] IS NOT INITIAL.

    DATA(lit_zfidt00317) = git_zfidt00317[].

    DELETE lit_zfidt00317 WHERE ind_pajak_hadiah EQ ''.
    SORT lit_zfidt00317 ASCENDING BY racct.

    CLEAR: gd_percent, gd_lines.
    DESCRIBE TABLE p_git_zmmdt00091 LINES gd_lines.

    LOOP AT p_git_zmmdt00091 INTO DATA(lwa_zmmdt00091).

      PERFORM f_progress_bar USING 'Preparing data from ZMMDT00091...'
                                    sy-tabix
                                    gd_lines.

      CLEAR gwa_zfidt00318.
      gwa_zfidt00318-bukrs = gc_rbukrs.
      gwa_zfidt00318-belnr = lwa_zmmdt00091-belnr.
      gwa_zfidt00318-gjahr = lwa_zmmdt00091-gjahr.
      gwa_zfidt00318-zeile = lwa_zmmdt00091-zeile.
      gwa_zfidt00318-zcount = lwa_zmmdt00091-zcount.


      gwa_zfidt00318-mblnr = lwa_zmmdt00091-mblnr.
      gwa_zfidt00318-mjahr = lwa_zmmdt00091-mjahr.
      gwa_zfidt00318-matnr = lwa_zmmdt00091-matnr.

*      gwa_zfidt00318-bukrs_pph = lwa_zmmdt00091-bukrs_pph.
      gwa_zfidt00318-belnr_pph = lwa_zmmdt00091-belnr_pph.
      gwa_zfidt00318-gjahr_pph = lwa_zmmdt00091-gjahr_pph.

      gwa_zfidt00318-racct = lwa_zmmdt00091-gl_biaya.
      gwa_zfidt00318-zuonr = lwa_zmmdt00091-no_pk.

      gwa_zfidt00318-currency = lwa_zmmdt00091-waers.
      gwa_zfidt00318-amount = lwa_zmmdt00091-amt_gr.
      gwa_zfidt00318-nilai_pph = lwa_zmmdt00091-nilai_pph.
      gwa_zfidt00318-name1 = lwa_zmmdt00091-name1.
      gwa_zfidt00318-npwp = lwa_zmmdt00091-npwp.
      gwa_zfidt00318-ktp = lwa_zmmdt00091-ktp.

      TRY.
          DATA(ld_ind_req) = lit_zfidt00317[ racct = lwa_zmmdt00091-gl_biaya ]-ind_req.

          IF ld_ind_req EQ space.
            gwa_zfidt00318-ind_run_post = 'X'.
          ENDIF.

        CATCH cx_root INTO DATA(lcl_exc).
      ENDTRY.

*      gwa_zfidt00318-ind_feed_tac = ''.

      CLEAR lcl_exc.
      TRY.
          gwa_zfidt00318-ind_tac_max = lit_zfidt00317[ racct = lwa_zmmdt00091-gl_biaya ]-ind_tac_max.
        CATCH cx_root INTO lcl_exc.
      ENDTRY.

*      gwa_zfidt00318-prctr = ''.
*      gwa_zfidt00318-zzcc = ''.
*      gwa_zfidt00318-zzku = ''.
*      gwa_zfidt00318-zzcp = ''.
*      gwa_zfidt00318-zzpr = ''.
*      gwa_zfidt00318-zzph = ''.
*      gwa_zfidt00318-zzch = ''.
*      gwa_zfidt00318-zzpo = ''.

*      gwa_zfidt00318-gjahr_reclass = ''.
*      gwa_zfidt00318-belnr_reclass = ''.

      gwa_zfidt00318-erdat = sy-datum.

      APPEND gwa_zfidt00318 TO p_git_zfidt00318.

      "*--------------------------------------------------------------------*

    ENDLOOP.

  ENDIF.

  "*--------------------------------------------------------------------*
  "Prepare Data BSEG

  IF p_git_bseg[] IS NOT INITIAL.

    CLEAR lit_zfidt00317.
    lit_zfidt00317 = git_zfidt00317[].

    DELETE lit_zfidt00317 WHERE ind_pajak_hadiah EQ 'X'.
    SORT lit_zfidt00317 ASCENDING BY racct.

    CLEAR: gd_percent, gd_lines.
    DESCRIBE TABLE p_git_bseg LINES gd_lines.

    LOOP AT p_git_bseg INTO DATA(lwa_bseg).

      PERFORM f_progress_bar USING 'Preparing data from BSEG...'
                                    sy-tabix
                                    gd_lines.

      CLEAR gwa_zfidt00318.
      gwa_zfidt00318-bukrs = gc_rbukrs.
      gwa_zfidt00318-belnr = lwa_bseg-belnr.
      gwa_zfidt00318-gjahr = lwa_bseg-gjahr.
      gwa_zfidt00318-zeile = lwa_bseg-buzei.

      gwa_zfidt00318-racct = lwa_bseg-hkont.
      gwa_zfidt00318-zuonr = lwa_bseg-zuonr.

      gwa_zfidt00318-currency = lwa_bseg-waers.
      gwa_zfidt00318-amount = lwa_bseg-wrbtr.

      gwa_zfidt00318-prctr = lwa_bseg-prctr.

*      gwa_zfidt00318-name1 = ''.
*      gwa_zfidt00318-npwp = ''.
*      gwa_zfidt00318-ktp = ''.

*      CLEAR lcl_exc.
*      CLEAR ld_ind_req.
*      TRY.
*          ld_ind_req = lit_zfidt00317[ racct = lwa_zmmdt00091-gl_biaya ]-ind_req.
*
*          IF ld_ind_req EQ space.
*            gwa_zfidt00318-ind_run_post = 'X'.
*          ENDIF.
*
*        CATCH cx_root INTO lcl_exc.
*      ENDTRY.
      gwa_zfidt00318-ind_run_post = 'X'.

*      gwa_zfidt00318-ind_feed_tac = ''.

      CLEAR lcl_exc.
      TRY.
          gwa_zfidt00318-ind_tac_max = lit_zfidt00317[ racct = lwa_bseg-hkont ]-ind_tac_max.
        CATCH cx_root INTO lcl_exc.
      ENDTRY.

*      gwa_zfidt00318-prctr = ''.
*      gwa_zfidt00318-zzcc = ''.
*      gwa_zfidt00318-zzku = ''.
*      gwa_zfidt00318-zzcp = ''.
*      gwa_zfidt00318-zzpr = ''.
*      gwa_zfidt00318-zzph = ''.
*      gwa_zfidt00318-zzch = ''.
*      gwa_zfidt00318-zzpo = ''.
*      gwa_zfidt00318-gjahr_reclass = ''.
*      gwa_zfidt00318-belnr_reclass = ''.

      gwa_zfidt00318-erdat = sy-datum.

      APPEND gwa_zfidt00318 TO p_git_zfidt00318.

      "*--------------------------------------------------------------------*

    ENDLOOP.

  ENDIF.

  SORT p_git_zfidt00318 ASCENDING BY bukrs gjahr belnr zeile zcount.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_SAVE_TO_TABLE
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GIT_ZFIDT00318
*&---------------------------------------------------------------------*
FORM f_save_to_table  USING    p_kind_of_log
                               p_git_zfidt00318 TYPE gtt_zfidt00318
                      CHANGING p_subrc.

*--------------------------------------------------------------------*

  IF p_git_zfidt00318[] IS NOT INITIAL.
    MODIFY zfidt00318 FROM TABLE p_git_zfidt00318.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
      PERFORM f_progress_bar_single USING 'Successfully saved to ZFIDT00318' 'S' 'S'.

      p_subrc = 0.

      "*--------------------------------------------------------------------*

      PERFORM f_insert_log USING p_kind_of_log "Kind of Log
                                 p_git_zfidt00318
                                 gwa_zfidt00318
                                 gwa_zfidt00318_3
                                 gwa_return
                                 git_return.

    ENDIF.
  ELSE.
    p_subrc = 1.
    PERFORM f_progress_bar_single USING 'There is no data, saving will be ignored' 'S' 'W'.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_SEND_OUTBOND
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_GIT_ZFIDT00318[]
*&---------------------------------------------------------------------*
FORM f_send_outbond  USING p_kind_of_log
                           p_git_zfidt00318 TYPE gtt_zfidt00318.

  DATA: lit_zfidt00318 TYPE TABLE OF zfidt00318.

  IF p_git_zfidt00318[] IS NOT INITIAL.

    LOOP AT p_git_zfidt00318 INTO DATA(lwa_zfidt00318).

      IF lwa_zfidt00318-ind_tac_max IS NOT INITIAL.

        CLEAR lit_zfidt00318[].
        APPEND lwa_zfidt00318 TO lit_zfidt00318[].

        CLEAR git_return[].
        CALL FUNCTION 'ZFIFM_OB_TAC_AUTO_RECLASS'
          TABLES
            it_tac    = lit_zfidt00318
            it_return = git_return.

        PERFORM f_insert_log USING p_kind_of_log "Kind of Log
                                   git_zfidt00318
                                   lwa_zfidt00318
                                   gwa_zfidt00318_3
                                   gwa_return
                                   git_return.

      ELSE.

        CLEAR git_return[].
        APPEND VALUE #( type = 'S' message = 'IND_TAC_MAX is blank, there''is no need to send outbond' ) TO git_return.

        PERFORM f_insert_log USING p_kind_of_log "Kind of Log
                                   git_zfidt00318
                                   lwa_zfidt00318
                                   gwa_zfidt00318_3
                                   gwa_return
                                   git_return.

      ENDIF.

    ENDLOOP.

    PERFORM f_progress_bar_single USING 'Sucessfully send outbond data' 'S' 'S'.

  ELSE.
    PERFORM f_progress_bar_single USING 'There is no data, send outbound will be ignored' 'S' 'W'.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_DATA_2
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GIT_ZFIDT00318[]
*&---------------------------------------------------------------------*
FORM f_get_data_2  CHANGING p_git_zfidt00318_2 TYPE gtt_zfidt00318_2.

  PERFORM f_progress_bar_single USING 'Getting data from ZFIDT00318...' 'S' 'S'.

  SELECT * FROM zfidt00318
    INTO CORRESPONDING FIELDS OF TABLE p_git_zfidt00318_2
    WHERE bukrs IN s4_bukrs AND
          belnr IN s4_belnr AND
          gjahr IN s4_gjahr AND
          erdat IN s4_erdat AND

          ind_run_post EQ '' AND
*          ind_feed_tac EQ '' AND
          belnr_reclass EQ space.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_GET_DATA_3
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      <-- GIT_ZFIDT00318[]
*&---------------------------------------------------------------------*
FORM f_get_data_3  CHANGING p_git_zfidt00318_3 TYPE gtt_zfidt00318_3.

  PERFORM f_progress_bar_single USING 'Getting data from ZFIDT00318...' 'S' 'S'.

  CASE gd_rb_2.
    WHEN 'RB11'.

      SELECT * FROM zficd_zfidt00318
        INTO CORRESPONDING FIELDS OF TABLE @p_git_zfidt00318_3
        WHERE bukrs IN @s4_bukrs AND
              belnr IN @s4_belnr AND
              gjahr IN @s4_gjahr AND
              erdat IN @s4_erdat AND

              ind_run_post EQ 'X' AND
              belnr_reclass EQ @space AND

              ( ind_feed_tac_max EQ 'N' OR ind_feed_tac_max EQ '' ).

    WHEN 'RB12'.

      SELECT * FROM zficd_zfidt00318
        INTO CORRESPONDING FIELDS OF TABLE @p_git_zfidt00318_3
        WHERE bukrs IN @s4_bukrs AND
              belnr IN @s4_belnr AND
              gjahr IN @s4_gjahr AND
              erdat IN @s4_erdat AND

              ind_run_post EQ 'X' AND

              gjahr_reclass IN @s4_gjah2 AND
              ( belnr_reclass NE @space AND belnr_reclass IN @s4_beln2 ).

      SELECT * FROM zficd_zfidt00318
        APPENDING CORRESPONDING FIELDS OF TABLE @p_git_zfidt00318_3
        WHERE bukrs IN @s4_bukrs AND
              belnr IN @s4_belnr AND
              gjahr IN @s4_gjahr AND
              erdat IN @s4_erdat AND

              ind_run_post EQ 'X' AND

              ind_feed_tac_max EQ 'Y'.

      SORT p_git_zfidt00318_3 ASCENDING BY bukrs gjahr belnr zeile zcount.

    WHEN 'RB13'.

      SELECT * FROM zficd_zfidt00318
        INTO CORRESPONDING FIELDS OF TABLE @p_git_zfidt00318_3
        WHERE ind_run_post EQ 'X' AND
              ind_feed_tac_max EQ 'N' AND
              belnr_reclass EQ @space.

  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_PRE_POSTING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> GIT_ZFIDT00318
*&---------------------------------------------------------------------*
FORM f_pre_posting  USING    p_git_zfidt00318_3 TYPE gtt_zfidt00318_3.

  LOOP AT p_git_zfidt00318_3 ASSIGNING FIELD-SYMBOL(<lfs_zfidt00318_3>).

    READ TABLE git_zfidt00317 INTO DATA(lwa_zfidt00317) WITH KEY racct = <lfs_zfidt00318_3>-racct.
    IF sy-subrc EQ 0.

      CASE lwa_zfidt00317-xopvw.
        WHEN ''.

          PERFORM f_posting    USING lwa_zfidt00317
                            CHANGING <lfs_zfidt00318_3>.
        WHEN 'X'.

          PERFORM f_posting_with_cl    USING lwa_zfidt00317
                                    CHANGING <lfs_zfidt00318_3>.

      ENDCASE.

    ENDIF.

  ENDLOOP.
  IF sy-subrc EQ 0.
    PERFORM f_progress_bar_single USING 'Posting data executed, for result see in transaction log' 'S' 'S'.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_POSTING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_ZFIDT00317
*&      --> <LFS_ZFIDT00318>
*&---------------------------------------------------------------------*
FORM f_posting     USING p_lwa_zfidt00317 TYPE zficd_zfidt00317
                CHANGING p_lwa_zfidt00318 TYPE gty_zficd_zfidt00318.

  "Declaration BAPI_ACC_DOCUMENT_POST
  DATA: lwa_docheader         TYPE bapiache09,
        lit_accountgl         TYPE TABLE OF bapiacgl09,
        lit_accountreceivable TYPE TABLE OF bapiacar09,
        lit_accountpayable    TYPE TABLE OF bapiacap09,
        lit_currencyamount    TYPE TABLE OF bapiaccr09,
        lit_extension2        TYPE TABLE OF bapiparex,
        lit_return            TYPE bapiret2_tty,
        ld_obj_type           TYPE bapiache09-obj_type,
        ld_obj_key            TYPE bapiache09-obj_key,
        ld_obj_sys            TYPE bapiache09-obj_sys.

*--------------------------------------------------------------------*

  CLEAR: lwa_docheader,
         lit_accountgl[],
         lit_accountreceivable[],
         lit_accountpayable[],
         lit_currencyamount[],
         lit_extension2[],
         lit_return[].

  "*--------------------------------------------------------------------*

  "Collect data for posting
  PERFORM f_collect_data_posting     USING p_lwa_zfidt00317
                                           p_lwa_zfidt00318
                                  CHANGING lwa_docheader
                                           lit_accountgl[]
                                           lit_accountreceivable[]
                                           lit_accountpayable[]
                                           lit_currencyamount[]
                                           lit_extension2[].

  "*--------------------------------------------------------------------*
  "Posting

  PERFORM f_call_post    USING lwa_docheader
                               lit_accountgl[]
                               lit_accountpayable[]
                               lit_accountreceivable[]
                               lit_currencyamount[]
                               lit_extension2[]
                      CHANGING p_lwa_zfidt00318
                               ld_obj_type
                               ld_obj_key
                               ld_obj_sys
                               lit_return[].

  "*--------------------------------------------------------------------*
  "Insert log
  CLEAR gwa_zfidt00318.
  MOVE-CORRESPONDING p_lwa_zfidt00318 TO gwa_zfidt00318.
  PERFORM f_insert_log USING gc_kind_post "Kind of Log
                             git_zfidt00318
                             gwa_zfidt00318
                             gwa_zfidt00318_3
                             gwa_return
                             lit_return.

  "*--------------------------------------------------------------------*

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_POSTING_WITH_CL
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_ZFIDT00317
*&      --> <LFS_ZFIDT00318>
*&---------------------------------------------------------------------*
FORM f_posting_with_cl  USING    p_lwa_zfidt00317 TYPE zficd_zfidt00317
                                 p_lwa_zfidt00318 TYPE gty_zficd_zfidt00318.

  "Parameter BAPI POSTING_INTERFACE_CLEARING
  DATA: lit_ftclear TYPE TABLE OF ftclear,
        lit_ftpost  TYPE TABLE OF ftpost,
        lit_blntab  TYPE TABLE OF blntab,
        lit_fttax   TYPE TABLE OF fttax,
        lwa_return  TYPE zfist00145.

*--------------------------------------------------------------------*

  CLEAR: lit_ftclear[], lit_ftpost[], lit_blntab[], lit_fttax[].

  "--------------------------------------------------------------------*
  "Collect data for posting clearing

  PERFORM: f_collect_data_clearing    USING p_lwa_zfidt00317
                                            p_lwa_zfidt00318
                                   CHANGING lit_ftclear[]
                                            lit_ftpost[]
                                            lit_blntab[]
                                            lit_fttax[].

  "*--------------------------------------------------------------------*
  "Post with clearing

  PERFORM f_call_clearing    USING lit_ftclear[]
                                   lit_ftpost[]
                                   lit_blntab[]
                                   lit_fttax[]
                          CHANGING p_lwa_zfidt00318
                                   lwa_return.

  "*--------------------------------------------------------------------*
  "Insert log

  CLEAR gwa_return.
  gwa_return-type = lwa_return-msgty.
  gwa_return-message = lwa_return-msgout.

  PERFORM f_insert_log USING gc_kind_post_cl "Kind of Log
                             git_zfidt00318
                             gwa_zfidt00318
                             p_lwa_zfidt00318
                             gwa_return
                             git_return.

*--------------------------------------------------------------------*

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_INSERT_LOG
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_BAPIRET2
*&---------------------------------------------------------------------*
FORM f_insert_log  USING    p_kind_of_log
                            p_lit_zfidt00318 TYPE gtt_zfidt00318
                            p_lwa_zfidt00318 TYPE zfidt00318
                            p_lwa_zfidt00318_2 TYPE gty_zficd_zfidt00318
                            p_lwa_return TYPE bapiret2
                            p_lit_return TYPE gtt_return.

  DATA: lwa_zfidt00319 TYPE zfidt00319.

*--------------------------------------------------------------------*

  CASE p_kind_of_log.
    WHEN gc_kind_created.

      LOOP AT p_lit_zfidt00318 INTO DATA(lwa_zfidt00318).

        SELECT SINGLE MAX( seqno ) FROM zfidt00319 INTO @DATA(ld_seqno)
          WHERE bukrs EQ @lwa_zfidt00318-bukrs AND
                gjahr EQ @lwa_zfidt00318-gjahr AND
                belnr EQ @lwa_zfidt00318-belnr AND
                zeile EQ @lwa_zfidt00318-zeile AND
                zcount EQ @lwa_zfidt00318-zcount AND
                kind EQ @p_kind_of_log.
        IF sy-subrc EQ 0.
          ADD 1 TO ld_seqno.
        ELSE.
          ld_seqno = 1.
        ENDIF.

        "*--------------------------------------------------------------------*

        CLEAR lwa_zfidt00319.
        MOVE-CORRESPONDING lwa_zfidt00318 TO lwa_zfidt00319.
        lwa_zfidt00319-kind = p_kind_of_log.
        lwa_zfidt00319-seqno = ld_seqno.

        CALL FUNCTION 'ZFIFM_GET_TIME_2'
          IMPORTING
            ex_time = lwa_zfidt00319-erzeit.

        lwa_zfidt00319-type = 'S'.
        lwa_zfidt00319-message = 'TAC created successfully'.
        lwa_zfidt00319-ernam = sy-uname.
        MODIFY zfidt00319 FROM lwa_zfidt00319.
        IF sy-subrc EQ 0.
          COMMIT WORK AND WAIT.
        ENDIF.

        CLEAR ld_seqno.

      ENDLOOP.

    WHEN gc_kind_post.

      CLEAR ld_seqno.
      SELECT SINGLE MAX( seqno ) FROM zfidt00319 INTO ld_seqno
        WHERE bukrs EQ p_lwa_zfidt00318_2-bukrs AND
              gjahr EQ p_lwa_zfidt00318_2-gjahr AND
              belnr EQ p_lwa_zfidt00318_2-belnr AND
              zeile EQ p_lwa_zfidt00318_2-zeile AND
              zcount EQ p_lwa_zfidt00318_2-zcount AND
              kind EQ p_kind_of_log.
      IF sy-subrc EQ 0.
        ADD 1 TO ld_seqno.
      ELSE.
        ld_seqno = 1.
      ENDIF.

      "*--------------------------------------------------------------------*

      LOOP AT p_lit_return INTO DATA(lwa_return).

        CLEAR lwa_zfidt00319.
        lwa_zfidt00319-bukrs = p_lwa_zfidt00318_2-bukrs.
        lwa_zfidt00319-gjahr = p_lwa_zfidt00318_2-gjahr.
        lwa_zfidt00319-belnr = p_lwa_zfidt00318_2-belnr.
        lwa_zfidt00319-zeile = p_lwa_zfidt00318_2-zeile.
        lwa_zfidt00319-zcount = p_lwa_zfidt00318_2-zcount.
        lwa_zfidt00319-kind = p_kind_of_log.
        lwa_zfidt00319-seqno = ld_seqno.

        CALL FUNCTION 'ZFIFM_GET_TIME_2'
          IMPORTING
            ex_date = lwa_zfidt00319-erdat
            ex_time = lwa_zfidt00319-erzeit.

        lwa_zfidt00319-type = lwa_return-type.
        lwa_zfidt00319-message = lwa_return-message.
        lwa_zfidt00319-ernam = sy-uname.
        MODIFY zfidt00319 FROM lwa_zfidt00319.
        IF sy-subrc EQ 0.
          COMMIT WORK AND WAIT.
        ENDIF.

        ADD 1 TO ld_seqno.

      ENDLOOP.

    WHEN gc_kind_post_cl.

      SELECT SINGLE MAX( seqno ) FROM zfidt00319 INTO ld_seqno
        WHERE bukrs EQ p_lwa_zfidt00318_2-bukrs AND
              gjahr EQ p_lwa_zfidt00318_2-gjahr AND
              belnr EQ p_lwa_zfidt00318_2-belnr AND
              zeile EQ p_lwa_zfidt00318_2-zeile AND
              zcount EQ p_lwa_zfidt00318_2-zcount AND
              kind EQ p_kind_of_log.
      IF sy-subrc EQ 0.
        ADD 1 TO ld_seqno.
      ELSE.
        ld_seqno = 1.
      ENDIF.

      "*--------------------------------------------------------------------*

      CLEAR lwa_zfidt00319.
      lwa_zfidt00319-bukrs = p_lwa_zfidt00318_2-bukrs.
      lwa_zfidt00319-gjahr = p_lwa_zfidt00318_2-gjahr.
      lwa_zfidt00319-belnr = p_lwa_zfidt00318_2-belnr.
      lwa_zfidt00319-zeile = p_lwa_zfidt00318_2-zeile.
      lwa_zfidt00319-zcount = p_lwa_zfidt00318_2-zcount.
      lwa_zfidt00319-kind = p_kind_of_log.
      lwa_zfidt00319-seqno = ld_seqno.

      CALL FUNCTION 'ZFIFM_GET_TIME_2'
        IMPORTING
          ex_date = lwa_zfidt00319-erdat
          ex_time = lwa_zfidt00319-erzeit.

      lwa_zfidt00319-type = p_lwa_return-type.
      lwa_zfidt00319-message = p_lwa_return-message.
      lwa_zfidt00319-ernam = sy-uname.
      MODIFY zfidt00319 FROM lwa_zfidt00319.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.

    WHEN gc_kind_send_ob.

      CLEAR ld_seqno.
      SELECT SINGLE MAX( seqno ) FROM zfidt00319 INTO ld_seqno
        WHERE bukrs EQ p_lwa_zfidt00318-bukrs AND
              gjahr EQ p_lwa_zfidt00318-gjahr AND
              belnr EQ p_lwa_zfidt00318-belnr AND
              zeile EQ p_lwa_zfidt00318-zeile AND
              zcount EQ p_lwa_zfidt00318-zcount AND
              kind EQ p_kind_of_log.
      IF sy-subrc EQ 0.
        ADD 1 TO ld_seqno.
      ELSE.
        ld_seqno = 1.
      ENDIF.

      "*--------------------------------------------------------------------*

      LOOP AT p_lit_return INTO lwa_return.

        CLEAR lwa_zfidt00319.
        lwa_zfidt00319-bukrs = p_lwa_zfidt00318-bukrs.
        lwa_zfidt00319-gjahr = p_lwa_zfidt00318-gjahr.
        lwa_zfidt00319-belnr = p_lwa_zfidt00318-belnr.
        lwa_zfidt00319-zeile = p_lwa_zfidt00318-zeile.
        lwa_zfidt00319-zcount = p_lwa_zfidt00318-zcount.
        lwa_zfidt00319-kind = p_kind_of_log.
        lwa_zfidt00319-seqno = ld_seqno.

        CALL FUNCTION 'ZFIFM_GET_TIME_2'
          IMPORTING
            ex_date = lwa_zfidt00319-erdat
            ex_time = lwa_zfidt00319-erzeit.

        lwa_zfidt00319-type = lwa_return-type.
        lwa_zfidt00319-message = lwa_return-message.
        lwa_zfidt00319-ernam = sy-uname.
        MODIFY zfidt00319 FROM lwa_zfidt00319.
        IF sy-subrc EQ 0.
          COMMIT WORK AND WAIT.
        ENDIF.

        ADD 1 TO ld_seqno.

      ENDLOOP.

    WHEN gc_kind_resend_ob.

      CLEAR ld_seqno.
      SELECT SINGLE MAX( seqno ) FROM zfidt00319 INTO ld_seqno
        WHERE bukrs EQ p_lwa_zfidt00318-bukrs AND
              gjahr EQ p_lwa_zfidt00318-gjahr AND
              belnr EQ p_lwa_zfidt00318-belnr AND
              zeile EQ p_lwa_zfidt00318-zeile AND
              zcount EQ p_lwa_zfidt00318-zcount AND
              kind EQ p_kind_of_log.
      IF sy-subrc EQ 0.
        ADD 1 TO ld_seqno.
      ELSE.
        ld_seqno = 1.
      ENDIF.

      "*--------------------------------------------------------------------*

      LOOP AT p_lit_return INTO lwa_return.

        CLEAR lwa_zfidt00319.
        lwa_zfidt00319-bukrs = p_lwa_zfidt00318-bukrs.
        lwa_zfidt00319-gjahr = p_lwa_zfidt00318-gjahr.
        lwa_zfidt00319-belnr = p_lwa_zfidt00318-belnr.
        lwa_zfidt00319-zeile = p_lwa_zfidt00318-zeile.
        lwa_zfidt00319-zcount = p_lwa_zfidt00318-zcount.
        lwa_zfidt00319-kind = p_kind_of_log.
        lwa_zfidt00319-seqno = ld_seqno.

        CALL FUNCTION 'ZFIFM_GET_TIME_2'
          IMPORTING
            ex_date = lwa_zfidt00319-erdat
            ex_time = lwa_zfidt00319-erzeit.

        lwa_zfidt00319-type = lwa_return-type.
        lwa_zfidt00319-message = lwa_return-message.
        lwa_zfidt00319-ernam = sy-uname.
        MODIFY zfidt00319 FROM lwa_zfidt00319.
        IF sy-subrc EQ 0.
          COMMIT WORK AND WAIT.
        ENDIF.

        ADD 1 TO ld_seqno.

      ENDLOOP.

  ENDCASE.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_COLLECT_DATA_CLEARING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_LWA_ZFIDT00317
*&      --> P_LWA_ZFIDT00318
*&      <-- LIT_FTCLEAR[]
*&      <-- LIT_FTPOST[]
*&      <-- LIT_BLNTAB[]
*&      <-- LIT_FTTAX[]
*&---------------------------------------------------------------------*
FORM f_collect_data_clearing  USING    p_lwa_zfidt00317 TYPE zficd_zfidt00317
                                       p_lwa_zfidt00318 TYPE gty_zficd_zfidt00318
                              CHANGING p_lit_ftclear TYPE fdm_t_ftclear
                                       p_lit_ftpost TYPE fdm_t_ftpost
                                       p_lit_blntab TYPE epic_t_ebr_blntab
                                       p_lit_fttax TYPE  feb_t_fttax.

  DATA: lwa_ftclear TYPE ftclear,
        lwa_ftpost  TYPE ftpost,
        lwa_blntab  TYPE blntab,
        lwa_fttax   TYPE fttax.

*--------------------------------------------------------------------*

  CLEAR lwa_ftclear.
  lwa_ftclear-agkoa = 'S'.
  lwa_ftclear-agkon = p_lwa_zfidt00317-racct.
  lwa_ftclear-agbuk = gc_rbukrs.
  lwa_ftclear-xnops = 'X'.
  lwa_ftclear-selfd = 'BELNR'.
  lwa_ftclear-selvon = p_lwa_zfidt00318-belnr.
  APPEND lwa_ftclear TO p_lit_ftclear.

  "*--------------------------------------------------------------------*

  SELECT SINGLE * FROM zficd_bseg_2
    INTO @DATA(lwa_zfivt00109)
      WHERE bukrs EQ @p_lwa_zfidt00318-bukrs AND
            gjahr EQ @p_lwa_zfidt00318-gjahr AND
            belnr EQ @p_lwa_zfidt00318-belnr AND
            buzei EQ @p_lwa_zfidt00318-zeile.

  "*--------------------------------------------------------------------*

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'BKPF-BUKRS'.
  lwa_ftpost-fval = p_lwa_zfidt00318-bukrs. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'BKPF-BUDAT'.
  PERFORM f_conv_date_with_separator USING lwa_zfivt00109-h_budat
                                           '.'
                                     CHANGING lwa_ftpost-fval.
  CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'BKPF-BLDAT'.
  PERFORM f_conv_date_with_separator USING lwa_zfivt00109-h_budat
                                           '.'
                                     CHANGING lwa_ftpost-fval.
  CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'BKPF-BLART'.
  lwa_ftpost-fval = p_lwa_zfidt00317-blart_to. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'BKPF-WAERS'.
  lwa_ftpost-fval = p_lwa_zfidt00318-currency. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'BKPF-MONAT'.
  lwa_ftpost-fval = lwa_zfivt00109-h_budat+4(2). CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'BKPF-BKTXT'.
  lwa_ftpost-fval = 'RECLASS JURNAL AUTO TAC'. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'BKPF-XBLNR'.
  lwa_ftpost-fval = lwa_zfivt00109-xblnr. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  "*--------------------------------------------------------------------*

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'BSEG-SGTXT'.
  lwa_ftpost-fval = lwa_zfivt00109-sgtxt. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'BSEG-ZUONR'.
  lwa_ftpost-fval = lwa_zfivt00109-zuonr. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'BSEG-WRBTR'.
  lwa_ftpost-fval = lwa_zfivt00109-zuonr.
  PERFORM f_convert_amount   USING 'TO_EXTERNAL'
                                    p_lwa_zfidt00318-currency
                                    p_lwa_zfidt00318-amount
                           CHANGING lwa_ftpost-fval.
  REPLACE ALL OCCURRENCES OF '.0000' IN lwa_ftpost-fval WITH ''. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'BSEG-PRCTR'.
  lwa_ftpost-fval = lwa_zfivt00109-prctr. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  "*--------------------------------------------------------------------*

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'RF05A-NEWKO'.
  lwa_ftpost-fval = p_lwa_zfidt00317-racct_to. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'RF05A-AUGTX'.
  lwa_ftpost-fval = 'RECLASS JURNAL AUTO TAC'. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'RF05A-NEWBS'.
  CASE p_lwa_zfidt00317-shkzg.
    WHEN 'S'.
      lwa_ftpost-fval = '40'.
    WHEN 'H'.
      lwa_ftpost-fval = '50'.
  ENDCASE.
  CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  "*--------------------------------------------------------------------*

  CASE p_lwa_zfidt00317-xbilk.
    WHEN 'X'.

    WHEN ''.
      CLEAR lwa_ftpost.
      lwa_ftpost-stype = 'K'.
      lwa_ftpost-count = '001'.
      lwa_ftpost-fnam = 'COBL-KOSTL'.
      lwa_ftpost-fval = lwa_zfivt00109-kostl. CONDENSE lwa_ftpost-fval.
      APPEND lwa_ftpost TO p_lit_ftpost.
  ENDCASE.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'COBL-ZZKU'.
  lwa_ftpost-fval = lwa_zfivt00109-zzku. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'COBL-ZZCP'.
  lwa_ftpost-fval = lwa_zfivt00109-zzcp. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'COBL-ZZPR'.
  lwa_ftpost-fval = lwa_zfivt00109-zzpr. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'COBL-ZZCH'.
  lwa_ftpost-fval = lwa_zfivt00109-zzch. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

*  CLEAR lwa_ftpost.
*  lwa_ftpost-stype = 'K'.
*  lwa_ftpost-count = '001'.
*  lwa_ftpost-fnam = 'COBL-ZZPO'.
*  lwa_ftpost-fval = lwa_zfivt00109-zzpo. CONDENSE lwa_ftpost-fval.
*  APPEND lwa_ftpost TO p_lit_ftpost.

  CLEAR lwa_ftpost.
  lwa_ftpost-stype = 'K'.
  lwa_ftpost-count = '001'.
  lwa_ftpost-fnam = 'COBL-ZZCC'.
  lwa_ftpost-fval = lwa_zfivt00109-zzcc. CONDENSE lwa_ftpost-fval.
  APPEND lwa_ftpost TO p_lit_ftpost.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_CALL_CLEARING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LIT_FTCLEAR[]
*&      --> LIT_FTPOST[]
*&      --> LIT_BLNTAB[]
*&      --> LIT_FTTAX[]
*&      <-- LWA_MSG_CLEARING
*&---------------------------------------------------------------------*
FORM f_call_clearing  USING    p_lit_ftclear TYPE fdm_t_ftclear
                               p_lit_ftpost TYPE fdm_t_ftpost
                               p_lit_blntab TYPE epic_t_ebr_blntab
                               p_lit_fttax TYPE feb_t_fttax
                      CHANGING p_lwa_zfidt00318 TYPE gty_zficd_zfidt00318
                               p_return TYPE zfist00145.

  CLEAR p_return.
  CALL FUNCTION 'ZFIFM_POSTING_CLEARING'
    IMPORTING
      exs_msg     = p_return
    TABLES
      imt_ftclear = p_lit_ftclear
      imt_ftpost  = p_lit_ftpost
      imt_blntab  = p_lit_blntab
      imt_fttax   = p_lit_fttax.

  IF p_return-msgty EQ 'S' AND p_return-msgid = 'F5' AND p_return-msgno = '312'.

    p_lwa_zfidt00318-belnr_reclass = p_return-msgv1(10).
    p_lwa_zfidt00318-gjahr_reclass = p_lwa_zfidt00318-bldat(4).

    UPDATE zfidt00318 SET belnr_reclass = p_lwa_zfidt00318-belnr_reclass
                          gjahr_reclass = p_lwa_zfidt00318-gjahr_reclass
      WHERE bukrs = p_lwa_zfidt00318-bukrs AND
            gjahr = p_lwa_zfidt00318-gjahr AND
            belnr = p_lwa_zfidt00318-belnr AND
            zeile = p_lwa_zfidt00318-zeile AND
            zcount = p_lwa_zfidt00318-zcount.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_COLLECT_DATA_POSTING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> P_LWA_ZFIDT00317
*&      --> P_LWA_ZFIDT00318
*&      <-- LWA_DOCHEADER
*&      <-- LIT_ACCOUNTGL[]
*&      <-- LIT_ACCOUNTPAYABLE[]
*&      <-- LIT_ACCOUNTRECEIVABLE[]
*&      <-- LIT_CURRENCYAMOUNT[]
*&      <-- LIT_EXTENSION2[]
*&---------------------------------------------------------------------*
FORM f_collect_data_posting  USING    p_lwa_zfidt00317 TYPE zficd_zfidt00317
                                      p_lwa_zfidt00318 TYPE gty_zficd_zfidt00318
                             CHANGING p_lwa_docheader TYPE bapiache09
                                      p_lit_accountgl TYPE bapiacgl09_tab
                                      p_lit_accountreceivable TYPE bapiacar09_tab
                                      p_lit_accountpayable TYPE bapiacap09_tab
                                      p_lit_currencyamount TYPE bapiaccr09_tab
                                      p_lit_extension2 TYPE  bapiparex_t.

*  TYPES: BEGIN OF lty_bseg,
*           bukrs TYPE bseg-bukrs,
*           gjahr TYPE bseg-gjahr,
*           belnr TYPE bseg-belnr,
*           buzei TYPE bseg-buzei,
*           hkont TYPE bseg-hkont,
*           matnr TYPE bseg-matnr,
*         END OF lty_bseg.

  DATA: lwa_accountgl      LIKE LINE OF p_lit_accountgl,
        lwa_currencyamount TYPE bapiaccr09,
        lwa_extension2     TYPE bapiparex.

  DATA: ld_numbering  TYPE posnr_acc,
        ld_amount     TYPE bapiaccr09-amt_doccur,
        ld_sum_amount TYPE bapiaccr09-amt_doccur.

*        lra_belnr     TYPE bseg-belnr,
*        lra_gjahr     TYPE bseg-gjahr.
*        lwa_bseg      TYPE lty_bseg.

*--------------------------------------------------------------------*

*  CLEAR lra_belnr[].
*  f_fill_range: lra_belnr 'I' 'EQ' p_lwa_zfidt00318-belnr ''.
*  f_fill_range: lra_belnr 'I' 'EQ' p_lwa_zfidt00318-belnr_pph ''.
*  SORT lra_belnr ASCENDING BY low.
*  DELETE ADJACENT DUPLICATES FROM lra_belnr COMPARING low.
*
*  CLEAR lra_gjahr[].
*  f_fill_range: lra_gjahr 'I' 'EQ' p_lwa_zfidt00318-gjahr ''.
*  f_fill_range: lra_gjahr 'I' 'EQ' p_lwa_zfidt00318-gjahr_pph ''.
*  SORT lra_gjahr ASCENDING BY low.
*  DELETE ADJACENT DUPLICATES FROM lra_gjahr COMPARING low.
*
**  CLEAR lwa_bseg.
**  SELECT SINGLE bukrs gjahr belnr buzei hkont matnr
*  SELECT bukrs ,gjahr, belnr, buzei, hkont, matnr, prctr, kostl
*    FROM bseg
**    INTO @data(lwa_bseg)
*    INTO TABLE @DATA(lit_bseg)
*    WHERE bukrs EQ @p_lwa_zfidt00318-bukrs AND
*          gjahr IN @lra_gjahr AND
*          belnr IN @lra_belnr AND
*          hkont EQ @p_lwa_zfidt00318-racct AND
*          matnr EQ @p_lwa_zfidt00318-matnr.

*--------------------------------------------------------------------*

  CLEAR ld_numbering.

  "Document Header
  p_lwa_docheader-username = sy-uname.
  p_lwa_docheader-header_txt = p_lwa_zfidt00318-bktxt.
  p_lwa_docheader-comp_code = gc_rbukrs.
  p_lwa_docheader-doc_date = p_lwa_zfidt00318-bldat.
  p_lwa_docheader-pstng_date = p_lwa_zfidt00318-bldat.
  p_lwa_docheader-trans_date = p_lwa_zfidt00318-bldat.
  p_lwa_docheader-fisc_year = p_lwa_zfidt00318-bldat(4).
  p_lwa_docheader-fis_period = p_lwa_zfidt00318-bldat+4(2).
  p_lwa_docheader-doc_type = p_lwa_zfidt00317-blart_to.

  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "Line 1

  "Account GL
  ADD 1 TO ld_numbering.

  CLEAR lwa_accountgl.
  lwa_accountgl-itemno_acc = ld_numbering.
  lwa_accountgl-gl_account = p_lwa_zfidt00318-racct.
*  lwa_accountgl-item_text = ''.
  lwa_accountgl-alloc_nmbr = p_lwa_zfidt00318-zuonr.
*  lwa_accountgl-costcenter = |{ p_lwa_zfidt00318-zzcc ALPHA = IN }|.
*  lwa_accountgl-profit_ctr = |{ p_lwa_zfidt00318-prctr ALPHA = IN }|.

  SELECT SINGLE bukrs ,gjahr, belnr, buzei, sgtxt, hkont, matnr, prctr, kostl, zzku, zzcp, zzpr, zzch, zzpo, zzcc
    FROM bseg
    INTO @DATA(lwa_bseg)
    WHERE bukrs EQ @p_lwa_zfidt00318-bukrs AND
          gjahr EQ @p_lwa_zfidt00318-gjahr AND
          belnr EQ @p_lwa_zfidt00318-belnr AND
          hkont EQ @p_lwa_zfidt00318-racct AND
          matnr EQ @p_lwa_zfidt00318-matnr.
  IF sy-subrc EQ 0.
    lwa_accountgl-item_text = lwa_bseg-sgtxt.
    lwa_accountgl-costcenter = lwa_bseg-kostl.
    lwa_accountgl-profit_ctr = lwa_bseg-prctr.

    "Extension
    CLEAR lwa_extension2.
    lwa_extension2-structure = 'ZZCC'.
    lwa_extension2-valuepart1 = ld_numbering.
    lwa_extension2-valuepart2 = lwa_bseg-zzcc.
    APPEND lwa_extension2 TO p_lit_extension2.

    CLEAR lwa_extension2.
    lwa_extension2-structure = 'ZZKU'.
    lwa_extension2-valuepart1 = ld_numbering.
    lwa_extension2-valuepart2 = lwa_bseg-zzku.
    APPEND lwa_extension2 TO p_lit_extension2.

    CLEAR lwa_extension2.
    lwa_extension2-structure = 'ZZCP'.
    lwa_extension2-valuepart1 = ld_numbering.
    lwa_extension2-valuepart2 = lwa_bseg-zzcp.
    APPEND lwa_extension2 TO p_lit_extension2.

    CLEAR lwa_extension2.
    lwa_extension2-structure = 'ZZPR'.
    lwa_extension2-valuepart1 = ld_numbering.
    lwa_extension2-valuepart2 = lwa_bseg-zzpr.
    APPEND lwa_extension2 TO p_lit_extension2.

    CLEAR lwa_extension2.
    lwa_extension2-structure = 'ZZCH'.
    lwa_extension2-valuepart1 = ld_numbering.
    lwa_extension2-valuepart2 = lwa_bseg-zzch.
    APPEND lwa_extension2 TO p_lit_extension2.

    CLEAR lwa_extension2.
    lwa_extension2-structure = 'ZZPO'.
    lwa_extension2-valuepart1 = ld_numbering.
    lwa_extension2-valuepart2 = lwa_bseg-zzpo.
    APPEND lwa_extension2 TO p_lit_extension2.

  ENDIF.

  APPEND lwa_accountgl TO p_lit_accountgl.

  "Currency Amount
  CLEAR lwa_currencyamount.
  lwa_currencyamount-itemno_acc = ld_numbering.
  lwa_currencyamount-currency = p_lwa_zfidt00318-currency.
  CLEAR ld_amount. ld_amount = p_lwa_zfidt00318-amount * -1.
  PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                    p_lwa_zfidt00318-currency
                                    ld_amount
                           CHANGING lwa_currencyamount-amt_doccur.
  ADD ld_amount TO ld_sum_amount.
  APPEND lwa_currencyamount TO p_lit_currencyamount.

  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "Line 2

  "Account GL
  ADD 1 TO ld_numbering.

  CLEAR lwa_accountgl.
  lwa_accountgl-itemno_acc = ld_numbering.
  lwa_accountgl-gl_account = p_lwa_zfidt00318-racct.
*  lwa_accountgl-item_text = ''.
  lwa_accountgl-alloc_nmbr = p_lwa_zfidt00318-zuonr.
*  lwa_accountgl-costcenter = |{ p_lwa_zfidt00318-zzcc ALPHA = IN }|.
*  lwa_accountgl-profit_ctr = |{ p_lwa_zfidt00318-prctr ALPHA = IN }|.

  CLEAR lwa_bseg.
  SELECT SINGLE bukrs gjahr belnr buzei sgtxt hkont matnr prctr kostl zzku zzcp zzpr zzch zzpo zzcc
    FROM bseg
    INTO lwa_bseg
    WHERE bukrs EQ p_lwa_zfidt00318-bukrs AND
          gjahr EQ p_lwa_zfidt00318-gjahr_pph AND
          belnr EQ p_lwa_zfidt00318-belnr_pph AND
          hkont EQ p_lwa_zfidt00318-racct.
  IF sy-subrc EQ 0.
    lwa_accountgl-item_text = lwa_bseg-sgtxt.
    lwa_accountgl-costcenter = lwa_bseg-kostl.
    lwa_accountgl-profit_ctr = lwa_bseg-prctr.

    "Extension
    CLEAR lwa_extension2.
    lwa_extension2-structure = 'ZZCC'.
    lwa_extension2-valuepart1 = ld_numbering.
    lwa_extension2-valuepart2 = lwa_bseg-zzcc.
    APPEND lwa_extension2 TO p_lit_extension2.

    CLEAR lwa_extension2.
    lwa_extension2-structure = 'ZZKU'.
    lwa_extension2-valuepart1 = ld_numbering.
    lwa_extension2-valuepart2 = lwa_bseg-zzku.
    APPEND lwa_extension2 TO p_lit_extension2.

    CLEAR lwa_extension2.
    lwa_extension2-structure = 'ZZCP'.
    lwa_extension2-valuepart1 = ld_numbering.
    lwa_extension2-valuepart2 = lwa_bseg-zzcp.
    APPEND lwa_extension2 TO p_lit_extension2.

    CLEAR lwa_extension2.
    lwa_extension2-structure = 'ZZPR'.
    lwa_extension2-valuepart1 = ld_numbering.
    lwa_extension2-valuepart2 = lwa_bseg-zzpr.
    APPEND lwa_extension2 TO p_lit_extension2.

    CLEAR lwa_extension2.
    lwa_extension2-structure = 'ZZCH'.
    lwa_extension2-valuepart1 = ld_numbering.
    lwa_extension2-valuepart2 = lwa_bseg-zzch.
    APPEND lwa_extension2 TO p_lit_extension2.

    CLEAR lwa_extension2.
    lwa_extension2-structure = 'ZZPO'.
    lwa_extension2-valuepart1 = ld_numbering.
    lwa_extension2-valuepart2 = lwa_bseg-zzpo.
    APPEND lwa_extension2 TO p_lit_extension2.

  ENDIF.

  lwa_accountgl-material = p_lwa_zfidt00318-matnr.
  APPEND lwa_accountgl TO p_lit_accountgl.

  "Currency Amount
  CLEAR lwa_currencyamount.
  lwa_currencyamount-itemno_acc = ld_numbering.
  lwa_currencyamount-currency = p_lwa_zfidt00318-currency.
  CLEAR ld_amount. ld_amount = p_lwa_zfidt00318-nilai_pph * -1.
  PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                    p_lwa_zfidt00318-currency
                                    ld_amount
                           CHANGING lwa_currencyamount-amt_doccur.
  ADD ld_amount TO ld_sum_amount.
  APPEND lwa_currencyamount TO p_lit_currencyamount.

  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "*--------------------------------------------------------------------*
  "Line 3

  "Account GL
  ADD 1 TO ld_numbering.

  CLEAR lwa_accountgl.
  lwa_accountgl-itemno_acc = ld_numbering.
  lwa_accountgl-gl_account = p_lwa_zfidt00317-racct_to.
*  lwa_accountgl-item_text = ''.
  lwa_accountgl-alloc_nmbr = p_lwa_zfidt00318-zuonr.
  lwa_accountgl-material = p_lwa_zfidt00318-matnr.

  CASE p_lwa_zfidt00317-xbilk.
    WHEN 'X'.
      lwa_accountgl-costcenter = ''.
    WHEN ''.
      lwa_accountgl-costcenter = p_lwa_zfidt00318-zzcc.
  ENDCASE.

  lwa_accountgl-profit_ctr = p_lwa_zfidt00318-prctr.

  APPEND lwa_accountgl TO p_lit_accountgl.

  "Currency Amount
  CLEAR lwa_currencyamount.
  lwa_currencyamount-itemno_acc = ld_numbering.
  lwa_currencyamount-currency = p_lwa_zfidt00318-currency.
  CLEAR ld_amount. ld_amount = ld_sum_amount * -1.
  PERFORM f_convert_amount    USING 'TO_EXTERNAL'
                                    p_lwa_zfidt00318-currency
                                    ld_amount
                           CHANGING lwa_currencyamount-amt_doccur.
  APPEND lwa_currencyamount TO p_lit_currencyamount.


  CASE p_lwa_zfidt00317-ind_req.
    WHEN 'X'.

      "Extension
      CLEAR lwa_extension2.
      lwa_extension2-structure = 'ZZCC'.
      lwa_extension2-valuepart1 = ld_numbering.
      lwa_extension2-valuepart2 = p_lwa_zfidt00318-zzcc.
      APPEND lwa_extension2 TO p_lit_extension2.

      CLEAR lwa_extension2.
      lwa_extension2-structure = 'ZZKU'.
      lwa_extension2-valuepart1 = ld_numbering.
      lwa_extension2-valuepart2 = p_lwa_zfidt00318-zzku.
      APPEND lwa_extension2 TO p_lit_extension2.

      CLEAR lwa_extension2.
      lwa_extension2-structure = 'ZZCP'.
      lwa_extension2-valuepart1 = ld_numbering.
      lwa_extension2-valuepart2 = p_lwa_zfidt00318-zzcp.
      APPEND lwa_extension2 TO p_lit_extension2.

      CLEAR lwa_extension2.
      lwa_extension2-structure = 'ZZPR'.
      lwa_extension2-valuepart1 = ld_numbering.
      lwa_extension2-valuepart2 = p_lwa_zfidt00318-zzpr.
      APPEND lwa_extension2 TO p_lit_extension2.

*      CLEAR lwa_extension2.
*      lwa_extension2-structure = 'ZZPH'.
*      lwa_extension2-valuepart1 = ld_numbering.
*      lwa_extension2-valuepart2 = p_lwa_zfidt00318-zzph.
*      APPEND lwa_extension2 TO p_lit_extension2.

      CLEAR lwa_extension2.
      lwa_extension2-structure = 'ZZCH'.
      lwa_extension2-valuepart1 = ld_numbering.
      lwa_extension2-valuepart2 = p_lwa_zfidt00318-zzch.
      APPEND lwa_extension2 TO p_lit_extension2.

      CLEAR lwa_extension2.
      lwa_extension2-structure = 'ZZPO'.
      lwa_extension2-valuepart1 = ld_numbering.
      lwa_extension2-valuepart2 = p_lwa_zfidt00318-zzpo.
      APPEND lwa_extension2 TO p_lit_extension2.

    WHEN ''.

      CLEAR lwa_bseg.
      SELECT SINGLE bukrs ,gjahr, belnr, buzei, sgtxt, hkont, matnr, prctr, kostl, zzku, zzcp, zzpr, zzch, zzpo, zzcc
        FROM bseg
        INTO @lwa_bseg
        WHERE bukrs EQ @p_lwa_zfidt00318-bukrs AND
              gjahr EQ @p_lwa_zfidt00318-gjahr AND
              belnr EQ @p_lwa_zfidt00318-belnr AND
              hkont EQ @p_lwa_zfidt00317-racct_to AND
              matnr EQ @p_lwa_zfidt00318-matnr.
      IF sy-subrc EQ 0.

        "Extension
        CLEAR lwa_extension2.
        lwa_extension2-structure = 'ZZCC'.
        lwa_extension2-valuepart1 = ld_numbering.
        lwa_extension2-valuepart2 = lwa_bseg-zzcc.
        APPEND lwa_extension2 TO p_lit_extension2.

        CLEAR lwa_extension2.
        lwa_extension2-structure = 'ZZKU'.
        lwa_extension2-valuepart1 = ld_numbering.
        lwa_extension2-valuepart2 = lwa_bseg-zzku.
        APPEND lwa_extension2 TO p_lit_extension2.

        CLEAR lwa_extension2.
        lwa_extension2-structure = 'ZZCP'.
        lwa_extension2-valuepart1 = ld_numbering.
        lwa_extension2-valuepart2 = lwa_bseg-zzcp.
        APPEND lwa_extension2 TO p_lit_extension2.

        CLEAR lwa_extension2.
        lwa_extension2-structure = 'ZZPR'.
        lwa_extension2-valuepart1 = ld_numbering.
        lwa_extension2-valuepart2 = lwa_bseg-zzpr.
        APPEND lwa_extension2 TO p_lit_extension2.

*        CLEAR lwa_extension2.
*        lwa_extension2-structure = 'ZZPH'.
*        lwa_extension2-valuepart1 = ld_numbering.
*        lwa_extension2-valuepart2 = lwa_bseg-zzph.
*        APPEND lwa_extension2 TO p_lit_extension2.

        CLEAR lwa_extension2.
        lwa_extension2-structure = 'ZZCH'.
        lwa_extension2-valuepart1 = ld_numbering.
        lwa_extension2-valuepart2 = lwa_bseg-zzch.
        APPEND lwa_extension2 TO p_lit_extension2.

        CLEAR lwa_extension2.
        lwa_extension2-structure = 'ZZPO'.
        lwa_extension2-valuepart1 = ld_numbering.
        lwa_extension2-valuepart2 = lwa_bseg-zzpo.
        APPEND lwa_extension2 TO p_lit_extension2.

      ENDIF.

  ENDCASE.



  "*--------------------------------------------------------------------*

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_CALL_POSTING
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> PU_PROC
*&      --> LIT_BLNTAB[]
*&      --> LIT_FTCLEAR[]
*&      --> LIT_FTPOST[]
*&      --> LIT_FTTAX[]
*&      <-- LWA_RETURN
*&---------------------------------------------------------------------*
FORM f_call_post  USING    p_lwa_docheader TYPE bapiache09
                           p_lit_accountgl TYPE bapiacgl09_tab
                           p_lit_accountpayable TYPE bapiacap09_tab
                           p_lit_accountreceivable TYPE bapiacar09_tab
                           p_lit_currencyamount TYPE bapiaccr09_tab
                           p_lit_extension2 TYPE  bapiparex_t
                  CHANGING p_lwa_zfidt00318 TYPE gty_zficd_zfidt00318
                           p_type
                           p_key
                           p_sys
                           p_lit_return TYPE bapiret2_tty.

  DATA: lit_return         TYPE TABLE OF bapiret2,
        lwa_return         TYPE bapiret2,
        ld_check_success   TYPE boolean,
        ld_posting_success TYPE boolean.

*--------------------------------------------------------------------*

  CLEAR: p_type,
         p_key,
         p_sys,
         p_lit_return[].

  "*--------------------------------------------------------------------*

  CLEAR lit_return[].
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_CHECK'
    EXPORTING
      documentheader    = p_lwa_docheader
    TABLES
      accountgl         = p_lit_accountgl
      accountreceivable = p_lit_accountreceivable
      accountpayable    = p_lit_accountpayable
      currencyamount    = p_lit_currencyamount
      return            = lit_return
      extension2        = p_lit_extension2.

  APPEND LINES OF lit_return[] TO p_lit_return[].

  CLEAR ld_check_success.
  READ TABLE p_lit_return INTO lwa_return WITH KEY type = 'E'.
  IF sy-subrc EQ 0.
    "Do nothing
  ELSE.
    ld_check_success = 'X'.
  ENDIF.

  "*--------------------------------------------------------------------*

  CHECK ld_check_success EQ 'X'.

  CLEAR lit_return[].
  CALL FUNCTION 'BAPI_ACC_DOCUMENT_POST'
    EXPORTING
      documentheader    = p_lwa_docheader
    IMPORTING
      obj_type          = p_type
      obj_key           = p_key
      obj_sys           = p_sys
    TABLES
      accountgl         = p_lit_accountgl
      accountreceivable = p_lit_accountreceivable
      accountpayable    = p_lit_accountpayable
      currencyamount    = p_lit_currencyamount
      return            = lit_return
      extension2        = p_lit_extension2.

  APPEND LINES OF lit_return[] TO p_lit_return[].

  CLEAR ld_posting_success.
  READ TABLE lit_return INTO lwa_return WITH KEY type = 'E'.
  IF sy-subrc NE 0.

    READ TABLE lit_return INTO lwa_return WITH KEY type = 'S'.
    IF sy-subrc EQ 0.
      ld_posting_success = 'X'.
    ENDIF.

  ENDIF.

  "*--------------------------------------------------------------------*

  CLEAR lwa_return.
  IF ld_posting_success EQ 'X'.

    CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
      IMPORTING
        return = lwa_return.

    p_lwa_zfidt00318-belnr_reclass = p_key+0(10).
    p_lwa_zfidt00318-gjahr_reclass = p_lwa_zfidt00318-bldat(4).

    "*--------------------------------------------------------------------*

    UPDATE zfidt00318 SET belnr_reclass = p_lwa_zfidt00318-belnr_reclass
                          gjahr_reclass = p_lwa_zfidt00318-gjahr_reclass
      WHERE bukrs = p_lwa_zfidt00318-bukrs AND
            gjahr = p_lwa_zfidt00318-gjahr AND
            belnr = p_lwa_zfidt00318-belnr AND
            zeile = p_lwa_zfidt00318-zeile AND
            zcount = p_lwa_zfidt00318-zcount.
    IF sy-subrc EQ 0.
      COMMIT WORK AND WAIT.
    ENDIF.

  ELSE.

    CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'
      IMPORTING
        return = lwa_return.

  ENDIF.

  IF lwa_return IS NOT INITIAL.
    APPEND lwa_return TO p_lit_return.
  ENDIF.

  "*--------------------------------------------------------------------*

ENDFORM.


*&---------------------------------------------------------------------*
*& Form F_CALL_TRANS_MM03
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LD_MATNR
*&---------------------------------------------------------------------*
FORM f_call_trans_mm03 USING p_matnr.

  DATA: lit_bdcdata    TYPE TABLE OF bdcdata,
        lit_btci_d0070 TYPE TABLE OF bdcdata.

*--------------------------------------------------------------------*
  CLEAR: lit_bdcdata[], lit_btci_d0070[].

  PERFORM bdc_dynpro      USING 'SAPLMGMM' '0060'
                       CHANGING lit_bdcdata[].

  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'RMMG1-MATNR'
                       CHANGING lit_bdcdata[].

  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ENTR'
                       CHANGING lit_bdcdata[].

  PERFORM bdc_field       USING 'RMMG1-MATNR'
                                p_matnr
                       CHANGING lit_bdcdata[].

  PERFORM bdc_dynpro      USING 'SAPLMGMM' '0070'
                       CHANGING lit_bdcdata[].

  PERFORM bdc_field       USING 'BDC_CURSOR'
                                'MSICHTAUSW-DYTXT(02)'
                       CHANGING lit_bdcdata[].

  PERFORM bdc_field       USING 'BDC_OKCODE'
                                '=ENTR'
                       CHANGING lit_bdcdata[].

  PERFORM bdc_field       USING 'MSICHTAUSW-KZSEL(01)'
                                'X'
                       CHANGING lit_bdcdata[].

  PERFORM bdc_field       USING 'MSICHTAUSW-KZSEL(02)'
                                'X'
                       CHANGING lit_bdcdata[].

*  CALL FUNCTION 'MATERIAL_BTCI_SELECTION_NEW'
*    EXPORTING
*      material                  = p_matnr
*      selection                 = 'K' " --> Basic Data
*      tcode                     = 'MM03'
*    TABLES
*      btci_d0070                = lit_btci_d0070
*    EXCEPTIONS
*      material_not_found        = 1
*      material_number_missing   = 2
*      material_type_missing     = 3
*      material_type_not_found   = 4
*      no_active_dynpro_selected = 5
*      no_authority              = 6
*      OTHERS                    = 7.
*  IF sy-subrc <> 0.
*    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
*               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*  ELSE.
*    APPEND LINES OF lit_btci_d0070 TO lit_bdcdata.
  CALL TRANSACTION 'MM03' USING lit_bdcdata MODE 'E'.
*  ENDIF.

ENDFORM.


FORM bdc_dynpro    USING program dynpro
                CHANGING p_lit_bdcdata TYPE bdcdata_tab.

  DATA: lwa_bdcdata TYPE bdcdata.

*--------------------------------------------------------------------*

  CLEAR lwa_bdcdata.
  lwa_bdcdata-program  = program.
  lwa_bdcdata-dynpro   = dynpro.
  lwa_bdcdata-dynbegin = 'X'.
  APPEND lwa_bdcdata TO p_lit_bdcdata.

ENDFORM.

*----------------------------------------------------------------------*
*        INSERT FIELD                                                  *
*----------------------------------------------------------------------*
FORM bdc_field    USING fnam fval
               CHANGING p_lit_bdcdata TYPE bdcdata_tab.

  DATA: lwa_bdcdata TYPE bdcdata.

*--------------------------------------------------------------------*

  CLEAR lwa_bdcdata.
  lwa_bdcdata-fnam = fnam.
  lwa_bdcdata-fval = fval.
  APPEND lwa_bdcdata TO p_lit_bdcdata.

ENDFORM.
