@AbapCatalog.sqlViewName: 'ZFIVT00108'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'ZFIDT00318 for Program ZFI02R0037'
define view ZFICD_ZFIDT00318
  as select from    zfidt00318 as a
    left outer join bkpf       as b on  b.bukrs = 'ADMF'
                                    and b.belnr = a.belnr
                                    and b.gjahr = a.gjahr
{
  key a.mandt,
  key a.bukrs,
  key a.gjahr,
  key a.belnr,
  key a.zeile,
  key a.zcount,

      a.erdat,
      a.racct,
      a.currency,
      a.mjahr,
      a.mblnr,
      a.matnr,

      @Semantics: { amount : {currencyCode: 'currency'} }
      a.amount,

      a.gjahr_pph,
      a.belnr_pph,

      @Semantics: { amount : {currencyCode: 'currency'} }
      a.nilai_pph,

      a.zuonr,
      a.name1,
      a.npwp,
      a.ktp,
      a.ind_tac_max,
      a.ind_run_post,
      a.ind_feed_tac_max,
      a.prctr,
      a.zzcc,
      a.zzku,
      a.zzcp,
      a.zzpr,
      a.zzph,
      a.zzch,
      a.zzpo,
      a.gjahr_reclass,
      a.belnr_reclass,
      
      b.bktxt,
      b.bldat
}
