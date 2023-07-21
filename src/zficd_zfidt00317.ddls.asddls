@AbapCatalog.sqlViewName: 'ZFIVT00103'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'ZFIDT00317 for Program ZFI02R0037'
define view ZFICD_ZFIDT00317
  as select from    zfidt00317 as a
    left outer join skb1       as b on  b.bukrs = 'ADMF'
                                    and a.racct = b.saknr
    left outer join ska1       as c on  c.ktopl = 'ADMF'
                                    and a.racct_to = c.saknr
{
  key a.mandt,
  key a.racct,
  key a.blart,
  key a.matnr,
      
      a.shkzg,
      a.racct_to,
      a.blart_to,
      a.ind_tac_max,
      a.ind_req,
      a.ind_pajak_hadiah,

      b.xopvw,

      c.xbilk
}
