@AbapCatalog.sqlViewName: 'ZFIVT00104'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'ZMMDT00091 for Program ZFI02R0037'
define view ZFICD_ZMMDT00091
  as select from    ZFICD_ZMMDT00091_2 as a
    left outer join bkpf               as c on c.awkey = concat(a.mblnr, a.mjahr)
{
  key a.mandt,
  key a.mblnr,
  key a.mjahr,
  key a.zeile,
  key a.zcount,
      
      a.awkey,
      c.gjahr as gjahr,
      c.belnr as belnr,

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
where c.ldgrp = '';
