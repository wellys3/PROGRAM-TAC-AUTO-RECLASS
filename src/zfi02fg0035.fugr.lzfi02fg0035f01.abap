*----------------------------------------------------------------------*
***INCLUDE LZFI02FG0035F01.
*----------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Form F_POPULATE_RETURN
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LWA_DATA_PROXY_I
*&      <-- IT_RETURN
*&---------------------------------------------------------------------*
FORM f_populate_return  USING    p_lwa_data_proxy_i TYPE zmt_tac_auto_reclass_ib_resp
                        CHANGING p_it_return TYPE  bapiret2_t.

  DATA: lwa_return TYPE bapiret2,
        ld_return  TYPE bapiret2-type.

*--------------------------------------------------------------------*

  CLEAR ld_return.
  IF p_lwa_data_proxy_i-mt_tac_auto_reclass_ib_resp-header-src_code EQ '200'.
    ld_return = 'S'.
  ELSE.
    ld_return = 'E'.
  ENDIF.

*--------------------------------------------------------------------*

  CLEAR lwa_return.
  lwa_return = ld_return.
  CONCATENATE 'CODE:' p_lwa_data_proxy_i-mt_tac_auto_reclass_ib_resp-header-code
    INTO lwa_return-message
      SEPARATED BY space.
  APPEND lwa_return TO p_it_return.

  CLEAR lwa_return.
  lwa_return = ld_return.
  CONCATENATE 'MESSAGE:' p_lwa_data_proxy_i-mt_tac_auto_reclass_ib_resp-header-message
    INTO lwa_return-message
      SEPARATED BY space.
  APPEND lwa_return TO p_it_return.


  CLEAR lwa_return.
  lwa_return = ld_return.
  CONCATENATE 'SRC_CODE:' p_lwa_data_proxy_i-mt_tac_auto_reclass_ib_resp-header-src_code
    INTO lwa_return-message
      SEPARATED BY space.
  APPEND lwa_return TO p_it_return.

  CLEAR lwa_return.
  lwa_return = ld_return.
  CONCATENATE 'SRC_MESSAGE:' p_lwa_data_proxy_i-mt_tac_auto_reclass_ib_resp-header-src_message
    INTO lwa_return-message
      SEPARATED BY space.
  APPEND lwa_return TO p_it_return.

  CLEAR lwa_return.
  lwa_return = ld_return.
  CONCATENATE 'REQUEST_ID:' p_lwa_data_proxy_i-mt_tac_auto_reclass_ib_resp-header-add_info-request_id
    INTO lwa_return-message
      SEPARATED BY space.
  APPEND lwa_return TO p_it_return.

  CLEAR lwa_return.
  lwa_return = ld_return.
  CONCATENATE 'REQUEST_TIMESTAMP:' p_lwa_data_proxy_i-mt_tac_auto_reclass_ib_resp-header-add_info-request_timestamp
    INTO lwa_return-message
      SEPARATED BY space.
  APPEND lwa_return TO p_it_return.

  CLEAR lwa_return.
  lwa_return = ld_return.
  CONCATENATE 'REFNO:' p_lwa_data_proxy_i-mt_tac_auto_reclass_ib_resp-header-add_info-ref_no
    INTO lwa_return-message
      SEPARATED BY space.
  APPEND lwa_return TO p_it_return.

  IF p_lwa_data_proxy_i-mt_tac_auto_reclass_ib_resp-data-sz_req[] IS NOT INITIAL.

    LOOP AT p_lwa_data_proxy_i-mt_tac_auto_reclass_ib_resp-data-sz_req INTO DATA(lwa_sz_req).

      CLEAR lwa_return.
      lwa_return = ld_return.
      CONCATENATE 'BELNR:' lwa_sz_req-rec_invoice_no
        INTO lwa_return-message
          SEPARATED BY space.
      APPEND lwa_return TO p_it_return.

      CLEAR lwa_return.
      lwa_return = ld_return.
      CONCATENATE 'AMOUNT:' lwa_sz_req-req_amount
        INTO lwa_return-message
          SEPARATED BY space.
      APPEND lwa_return TO p_it_return.

      CLEAR lwa_return.
      lwa_return = ld_return.
      CONCATENATE 'PRCTR:' lwa_sz_req-rec_profit_center
        INTO lwa_return-message
          SEPARATED BY space.
      APPEND lwa_return TO p_it_return.

      CLEAR lwa_return.
      lwa_return = ld_return.
      CONCATENATE 'ZZCC:' lwa_sz_req-rec_cost_center
        INTO lwa_return-message
          SEPARATED BY space.
      APPEND lwa_return TO p_it_return.

      CLEAR lwa_return.
      lwa_return = ld_return.
      CONCATENATE 'ZZKU:' lwa_sz_req-rec_ku
        INTO lwa_return-message
          SEPARATED BY space.
      APPEND lwa_return TO p_it_return.

      CLEAR lwa_return.
      lwa_return = ld_return.
      CONCATENATE 'ZZCP:' lwa_sz_req-rec_cp
        INTO lwa_return-message
          SEPARATED BY space.
      APPEND lwa_return TO p_it_return.

      CLEAR lwa_return.
      lwa_return = ld_return.
      CONCATENATE 'ZZPR:' lwa_sz_req-rec_product
        INTO lwa_return-message
          SEPARATED BY space.
      APPEND lwa_return TO p_it_return.

      CLEAR lwa_return.
      lwa_return = ld_return.
      CONCATENATE 'ZZPH:' lwa_sz_req-rec_product_matrix
        INTO lwa_return-message
          SEPARATED BY space.
      APPEND lwa_return TO p_it_return.

      CLEAR lwa_return.
      lwa_return = ld_return.
      CONCATENATE 'ZZCH:' lwa_sz_req-rec_channel
        INTO lwa_return-message
          SEPARATED BY space.
      APPEND lwa_return TO p_it_return.

      CLEAR lwa_return.
      lwa_return = ld_return.
      CONCATENATE 'ZZPO:' lwa_sz_req-rec_portofolio
        INTO lwa_return-message
          SEPARATED BY space.
      APPEND lwa_return TO p_it_return.

      CLEAR lwa_return.
      lwa_return = ld_return.
      CONCATENATE 'IND_FEED_TAC_MAX:' lwa_sz_req-rec_flag_tac_max
        INTO lwa_return-message
          SEPARATED BY space.
      APPEND lwa_return TO p_it_return.

    ENDLOOP.

  ENDIF.

ENDFORM.


FORM f_convert_amount  USING p_kind
                             p_currency
                             p_amount
                    CHANGING p_output.
  DATA: ld_io  TYPE bapicurr-bapicurr.
  CASE p_kind.
    WHEN 'TO_INTERNAL'.
      ld_io = p_amount.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_INTERNAL'
        EXPORTING
          currency             = p_currency
          amount_external      = ld_io
          max_number_of_digits = 23
        IMPORTING
          amount_internal      = p_output.
    WHEN 'TO_EXTERNAL'.
      CALL FUNCTION 'BAPI_CURRENCY_CONV_TO_EXTERNAL'
        EXPORTING
          currency        = p_currency
          amount_internal = p_amount
        IMPORTING
          amount_external = ld_io.
      IF sy-subrc EQ 0.
        p_output = ld_io.
      ENDIF.
  ENDCASE.
ENDFORM.
