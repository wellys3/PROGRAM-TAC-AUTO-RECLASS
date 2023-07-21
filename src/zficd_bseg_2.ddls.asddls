@AbapCatalog.sqlViewName: 'ZFIVT00109'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'BSEG for Program ZFI02R0037'
define view ZFICD_BSEG_2
  as select from    bseg as a
    left outer join bkpf as c on  c.bukrs = a.bukrs
                              and c.belnr = a.belnr
                              and c.gjahr = a.gjahr
  //    left outer join zfidt00318 as b on  b.bukrs = 'ADMF'
  //                                    and b.belnr = a.belnr
  //                                    and b.gjahr = a.gjahr
{
  key a.mandt,
  key a.bukrs,
  key a.belnr,
  key a.gjahr,
  key a.buzei,

      a.prctr,
      a.kostl,
      a.h_budat,
      a.zuonr,
      a.sgtxt,

      c.bktxt,
      c.blart,
      c.xblnr,
      c.waers,

      @Semantics: { amount : {currencyCode: 'waers'} }
      a.wrbtr,
      a.hkont,
      a.matnr,
      
      a.zzku,
      a.zzcp,
      a.zzpr,
      a.zzch,
      a.zzpo,
      a.zzcc
}
//where
//      b.belnr is null
//  and b.gjahr is null;
