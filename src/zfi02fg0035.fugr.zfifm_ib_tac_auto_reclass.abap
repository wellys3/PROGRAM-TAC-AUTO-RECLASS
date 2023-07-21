FUNCTION zfifm_ib_tac_auto_reclass.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  TABLES
*"      IT_TAC TYPE  ZFITT00192
*"----------------------------------------------------------------------

*****Notes:
*****This function is not used!!!

*****  DATA: lit_zfidt00318 TYPE TABLE OF zfidt00318.
*****
******--------------------------------------------------------------------*
*****
*****  CLEAR lit_zfidt00318[].
*****  MOVE-CORRESPONDING it_tac[] TO lit_zfidt00318[].
*****
*****  IF lit_zfidt00318[] IS NOT INITIAL.
*****
*****    LOOP AT lit_zfidt00318 ASSIGNING FIELD-SYMBOL(<lfs_zfidt003118>).
*****
*****      <lfs_zfidt003118>-zflag_run_post = 'X'.
*****      <lfs_zfidt003118>-ind_feed_tac = 'X'.
*****
*****      UPDATE zfidt00318 SET zflag_run_post = <lfs_zfidt003118>-zflag_run_post
*****                            ind_feed_tac = <lfs_zfidt003118>-ind_feed_tac
*****                            prctr = <lfs_zfidt003118>-prctr
*****                            zzcc = <lfs_zfidt003118>-zzcc
*****                            zzku = <lfs_zfidt003118>-zzku
*****                            zzcp = <lfs_zfidt003118>-zzcp
*****                            zzpr = <lfs_zfidt003118>-zzpr
*****                            zzph = <lfs_zfidt003118>-zzph
*****                            zzch = <lfs_zfidt003118>-zzch
*****                            zzpo = <lfs_zfidt003118>-zzpo
*****        WHERE bukrs = <lfs_zfidt003118>-bukrs AND
*****              gjahr = <lfs_zfidt003118>-gjahr AND
*****              belnr = <lfs_zfidt003118>-belnr AND
*****              zeile = <lfs_zfidt003118>-zeile AND
*****              zcount = <lfs_zfidt003118>-zcount.
*****
*****    ENDLOOP.
*****
*****  ENDIF.

ENDFUNCTION.
