*---------------------------------------------------------------------*
*    view related data declarations
*---------------------------------------------------------------------*
*...processing: ZFIDT00317......................................*
DATA:  BEGIN OF STATUS_ZFIDT00317                    .   "state vector
         INCLUDE STRUCTURE VIMSTATUS.
DATA:  END OF STATUS_ZFIDT00317                    .
CONTROLS: TCTRL_ZFIDT00317
            TYPE TABLEVIEW USING SCREEN '0001'.
*.........table declarations:.................................*
TABLES: *ZFIDT00317                    .
TABLES: ZFIDT00317                     .

* general table data declarations..............
  INCLUDE LSVIMTDT                                .
