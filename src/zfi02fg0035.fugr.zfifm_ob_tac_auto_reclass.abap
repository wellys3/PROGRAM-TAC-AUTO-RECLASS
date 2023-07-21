FUNCTION zfifm_ob_tac_auto_reclass.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_TAC STRUCTURE  ZFIDT00318
*"      IT_RETURN STRUCTURE  BAPIRET2
*"----------------------------------------------------------------------

  DATA: lcl_proxy        TYPE REF TO zco_si_tac_auto_reclass_ob, "Class sproxy outbond

        lwa_data_proxy_o TYPE zmt_tac_auto_reclass_req,  "Local work area header output proxy
        lwa_data_proxy_i TYPE zmt_tac_auto_reclass_ib_resp,  "Local work area header input proxy

        lwa_data         TYPE zdt_tac_auto_reclass_req_data,
        lwa_return       TYPE bapiret2,

        ld_amount        TYPE bapicurr-bapicurr.

*--------------------------------------------------------------------*

*  CLEAR ch_return.
  CLEAR: it_return[],
         lwa_data_proxy_o,
         lwa_data_proxy_i.

*--------------------------------------------------------------------*
*Prepare data

*****  MOVE-CORRESPONDING it_tac[] TO lwa_data_proxy_o-mt_tac_auto_reclass_req-data[].
*****
*****  LOOP AT lwa_data_proxy_o-mt_tac_auto_reclass_req-data ASSIGNING FIELD-SYMBOL(<lfs_data>).
*****
*****    READ TABLE it_tac INTO DATA(lwa_tac) INDEX 1.
*****    IF sy-subrc EQ 0.
*****      <lfs_data>-wrbtr = lwa_tac-nilai_pph + lwa_tac-amount.
*****    ENDIF.
*****
*****  ENDLOOP.

*---

  LOOP AT it_tac INTO DATA(lwa_tac).

    CLEAR: lwa_data, ld_amount.
    MOVE-CORRESPONDING lwa_tac TO lwa_data.

    CLEAR ld_amount.
    ld_amount = lwa_tac-nilai_pph + lwa_tac-amount.

    PERFORM f_convert_amount USING 'TO_EXTERNAL'
                                   lwa_tac-currency
                                   ld_amount
                             CHANGING ld_amount.

    lwa_data-amount = ld_amount.

    REPLACE ALL OCCURRENCES OF '.0000' IN lwa_data-amount WITH ''.


    APPEND lwa_data TO lwa_data_proxy_o-mt_tac_auto_reclass_req-data.

  ENDLOOP.

*--------------------------------------------------------------------*
*Call Proxy

  TRY .
      CREATE OBJECT lcl_proxy.

      CALL METHOD lcl_proxy->si_tac_auto_reclass_ob
        EXPORTING
          output = lwa_data_proxy_o
        IMPORTING
          input  = lwa_data_proxy_i.

    CATCH cx_ai_system_fault INTO DATA(lcl_cx_ai_system_fault).

      CLEAR lwa_return.
      lwa_return-type = 'E'.
      lwa_return-message = lcl_cx_ai_system_fault->get_text( ).
      APPEND lwa_return TO it_return.
      EXIT.

  ENDTRY.

*--------------------------------------------------------------------*
*Capture message return from outbond

  PERFORM f_populate_return    USING lwa_data_proxy_i
                            CHANGING it_return[].

*--------------------------------------------------------------------*
*Get result from proxy

  LOOP AT it_tac ASSIGNING FIELD-SYMBOL(<lfs_tac>).

    READ TABLE lwa_data_proxy_i-mt_tac_auto_reclass_ib_resp-data-sz_req INTO DATA(lwa_sz_req) INDEX 1.
    IF sy-subrc EQ 0.

      IF lwa_sz_req-rec_invoice_no IS NOT INITIAL.
        <lfs_tac>-ind_run_post = 'X'.
      ENDIF.

      <lfs_tac>-ind_feed_tac_max = lwa_sz_req-rec_flag_tac_max.
      <lfs_tac>-prctr = lwa_sz_req-rec_profit_center.
      <lfs_tac>-zzcc = lwa_sz_req-rec_cost_center.
      <lfs_tac>-zzku = lwa_sz_req-rec_ku.
      <lfs_tac>-zzcp = lwa_sz_req-rec_cp.
      <lfs_tac>-zzpr = lwa_sz_req-rec_product.
      <lfs_tac>-zzph = lwa_sz_req-rec_product_matrix.
      <lfs_tac>-zzch = lwa_sz_req-rec_channel.
      <lfs_tac>-zzpo = lwa_sz_req-rec_portofolio.

      UPDATE zfidt00318 SET ind_run_post = <lfs_tac>-ind_run_post
                            ind_feed_tac_max = <lfs_tac>-ind_feed_tac_max
                            prctr = <lfs_tac>-prctr
                            zzcc = <lfs_tac>-zzcc
                            zzku = <lfs_tac>-zzku
                            zzcp = <lfs_tac>-zzcp
                            zzpr = <lfs_tac>-zzpr
                            zzph = <lfs_tac>-zzph
                            zzch = <lfs_tac>-zzch
                            zzpo = <lfs_tac>-zzpo
        WHERE bukrs = <lfs_tac>-bukrs AND
              gjahr = <lfs_tac>-gjahr AND
              belnr = <lfs_tac>-belnr AND
              zeile = <lfs_tac>-zeile AND
              zcount = <lfs_tac>-zcount.
      IF sy-subrc EQ 0.
        COMMIT WORK AND WAIT.
      ENDIF.

      CLEAR lwa_return.
      lwa_return-type = 'S'.
      lwa_return-message = 'Send outbond data successfully and ZFLAG_RUN_POST set as ''X'''.
      APPEND lwa_return TO it_return.

*      ELSE.
*
*        CLEAR lwa_return.
*        lwa_return-type = 'W'.
*        lwa_return-message = 'Send outbond data successfully and ZFLAG_RUN_POST set as <BLANK>'.
*        APPEND lwa_return TO it_return.
*
*      ENDIF.

    ELSE.

      CLEAR lwa_return.
      lwa_return-type = 'E'.
      lwa_return-message = 'Send outbond data successfully but no return'.
      APPEND lwa_return TO it_return.

    ENDIF.

  ENDLOOP.

ENDFUNCTION.
