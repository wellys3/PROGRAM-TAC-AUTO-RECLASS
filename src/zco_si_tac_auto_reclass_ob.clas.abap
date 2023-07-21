class ZCO_SI_TAC_AUTO_RECLASS_OB definition
  public
  inheriting from CL_PROXY_CLIENT
  create public .

public section.

  methods CONSTRUCTOR
    importing
      !LOGICAL_PORT_NAME type PRX_LOGICAL_PORT_NAME optional
    raising
      CX_AI_SYSTEM_FAULT .
  methods SI_TAC_AUTO_RECLASS_OB
    importing
      !OUTPUT type ZMT_TAC_AUTO_RECLASS_REQ
    exporting
      !INPUT type ZMT_TAC_AUTO_RECLASS_IB_RESP
    raising
      CX_AI_SYSTEM_FAULT .
protected section.
private section.
ENDCLASS.



CLASS ZCO_SI_TAC_AUTO_RECLASS_OB IMPLEMENTATION.


  method CONSTRUCTOR.

  super->constructor(
    class_name          = 'ZCO_SI_TAC_AUTO_RECLASS_OB'
    logical_port_name   = logical_port_name
  ).

  endmethod.


  method SI_TAC_AUTO_RECLASS_OB.

  data:
    ls_parmbind type abap_parmbind,
    lt_parmbind type abap_parmbind_tab.

  ls_parmbind-name = 'OUTPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>importing.
  get reference of OUTPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  ls_parmbind-name = 'INPUT'.
  ls_parmbind-kind = cl_abap_objectdescr=>exporting.
  get reference of INPUT into ls_parmbind-value.
  insert ls_parmbind into table lt_parmbind.

  if_proxy_client~execute(
    exporting
      method_name = 'SI_TAC_AUTO_RECLASS_OB'
    changing
      parmbind_tab = lt_parmbind
  ).

  endmethod.
ENDCLASS.
