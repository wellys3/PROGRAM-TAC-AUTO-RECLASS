@AbapCatalog.sqlViewName: 'ZFIVT00107'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'ZMMDT00091 for Program ZFI02R0037'
define view ZFICD_ZMMDT00091_3
  as select from    ZFICD_ZMMDT00091 as a
    left outer join zfidt00318       as b on  b.bukrs = 'ADMF'
                                          and b.belnr = a.belnr
                                          and b.gjahr = a.gjahr
{
  key a.mandt,
  key a.mblnr,
  key a.mjahr,
  key a.zeile,
  key a.zcount,

      a.gjahr,
      a.belnr,

      a.ebeln,
      a.budat,
      a.matnr,
      a.werks,
      a.name1,
      a.ktp,
      a.npwp,
      a.waers,

      @Semantics: { amount : {currencyCode: 'waers'} }
      a.amt_gr,

      @Semantics: { amount : {currencyCode: 'waers'} }
      a.base_amt,

      @Semantics: { amount : {currencyCode: 'waers'} }
      a.nilai_pph,

      a.gl_biaya,
      a.gl_pph,

      a.bukrs_pph,
      a.belnr_pph,
      a.gjahr_pph,
      a.blart,

      a.no_pk,
      a.mtype,
      a.msg

}
where
      b.belnr is null
  and b.gjahr is null;
