FUNCTION zfifm_get_time_2.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  EXPORTING
*"     REFERENCE(EX_DATE) TYPE  SY-DATUM
*"     REFERENCE(EX_TIME) TYPE  C
*"----------------------------------------------------------------------

  DATA: ld_ts      TYPE timestampl,
        ld_tsc(25) TYPE c,
        ld_time    TYPE time,
        ld_date    TYPE date,
        ld_ms      TYPE p DECIMALS 1,
        ld_zonlo   TYPE sy-zonlo.

  CLEAR: ld_ts, ld_tsc.

  ld_ms = '0.1'.

  ld_zonlo = 'UTC+7'.

  GET TIME STAMP FIELD ld_ts.
  CONVERT TIME STAMP ld_ts TIME ZONE ld_zonlo INTO TIME ld_time DATE ld_date.

  MOVE ld_ts TO ld_tsc.
  CONDENSE ld_tsc.

  CONCATENATE ld_time+0(2) ':' ld_time+2(2) ':' ld_time+4(2)  ':' ld_tsc+15(3) INTO ex_time.
  CONCATENATE ld_date+0(4) ld_date+4(2) ld_date+6(2) INTO ex_date.

  WAIT UP TO ld_ms SECONDS.

ENDFUNCTION.
