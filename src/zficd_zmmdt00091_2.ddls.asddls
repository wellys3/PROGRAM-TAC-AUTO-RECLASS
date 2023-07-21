@AbapCatalog.sqlViewName: 'ZFIVT00106'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@ObjectModel.usageType.serviceQuality: #B
@ObjectModel.usageType.dataClass: #TRANSACTIONAL
@ObjectModel.usageType.sizeCategory: #XXL
@EndUserText.label: 'ZMMDT00091 with AWKEY for Program ZFI02R0037'
define view ZFICD_ZMMDT00091_2
  as select from zmmdt00091 as a
      left outer join bkpf       as c on  c.bukrs = 'ADMF'
                                      and c.belnr = a.belnr
                                      and c.gjahr = a.gjahr
{
  key a.mandt,
  key a.mblnr,
  key a.mjahr,
  key a.zeile,
  key a.zcount,

      concat(a.mblnr,a.mjahr ) as awkey,

      a.delivery_note,
      a.ebeln,
      a.ebelp,
      a.budat,
      a.matnr,
      a.menge,
      a.meins,
      a.werks,
      a.lgort,
      a.lifnr,
      a.name1,
      a.otv,
      a.ktp,
      a.npwp,
      a.ablad,
      a.address,
      a.undian,
      a.otv_type,
      a.vtc,
      a.itc,
      a.witht,
      a.withcd,
      a.tax_code,

      a.waers,

      @Semantics: { amount : {currencyCode: 'waers'} }
      a.amt_gr,

      @Semantics: { amount : {currencyCode: 'waers'} }
      a.base_amt,

      @Semantics: { amount : {currencyCode: 'waers'} }
      a.nilai_pph,

      a.gl_biaya,
      a.gl_pph,
      a.kostl,
      a.aufnr,
      a.zstatus,

      a.bukrs as bukrs_pph,
      a.belnr as belnr_pph,
      a.gjahr as gjahr_pph,
      a.buzei,
      c.blart,
      
      a.zaddress,
      a.no_pk,
      a.mtype,
      a.msg,
      a.filein,
      a.upldt,
      a.uplby,
      a.aenam,
      a.aedat,
      aezet

}
