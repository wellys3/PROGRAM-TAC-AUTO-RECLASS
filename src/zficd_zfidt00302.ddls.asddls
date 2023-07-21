@AbapCatalog.sqlViewName: 'ZFIVT00095'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'ZFIDT00302 for Program ZFI02R0032'
define view ZFICD_zfidt00302
  as select from    zfidt00302 as a
  //      left outer join ska1   as b on  b.ktopl = 'ADMF'
  //                                and a.saknr  = b.saknr
    left outer join skb1       as b on  a.rbukrs = b.bukrs
                                    and a.racct  = b.saknr
{

  key a.mandt,
  key a.rbukrs,
  key a.racct,
  key a.zzku_from,
  key a.zzku_to,
  key a.zzcp_from,
  key a.zzcp_to,

      a.racct_desc,
      a.racct_antasena,
      a.racct_antasena_desc,
      
      b.waers as waers_skb1
}
