*&---------------------------------------------------------------------*
*& Report ZFI02R0037
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*


*&---------------------------------------------------------------------*
*& Description : Program TAC Auto Reclass
*&
*& Module      : Financial Accounting
*& Functional  : - Frits Bilery Aritonang (frits.aritonang@equine.co.id)
*&               - Danang Yoga Wijaya (danang.wijaya@equine.co.id)
*& FSD Loc.    : - SO2_MIME_REPOSITORY --> SAP --> PUBLIC --> ZFSD
*& FSD         : -
*& Developer   : Welly Sugiarto (welly.sugiarto@equine.co.id)
*& Date        : June 2nd, 2023
*& Copyright   : © 2023 PT Equine Global
*&               © 2023 PT Adira Dinamika Multi Finance
*&
*& Transport Request History (Any changes of TR will be updated here future):
*& *  A4DK908903 SAPABAP EG-AB-FI CR18 - Program Auto Reclass WSU FBA #1
*&    Changelog: #1 Initial Release
*&---------------------------------------------------------------------*


REPORT zfi02r0037.


*----------------------------------------------------------------------*
* Includes                                                             *
*----------------------------------------------------------------------*
INCLUDE zfi02r0037_top. "Types, Data, Constant Declaration & Selection-Screen.
INCLUDE zfi02r0037_f00. "Other Function for whole this program
INCLUDE zfi02r0037_f01. "Get Data
INCLUDE zfi02r0037_f02. "Display Data
*----------------------------------------------------------------------*
* End - Includes                                                       *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Initialization                                                       *
*----------------------------------------------------------------------*
INITIALIZATION.
  PERFORM f_initialization.
*----------------------------------------------------------------------*
* End - Initialization                                                 *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* Start-of-Selection                                                   *
*----------------------------------------------------------------------*
START-OF-SELECTION.

  CLEAR gd_subrc.
  PERFORM f_pre_execute CHANGING gd_subrc.

  CHECK gd_subrc EQ 0.
  PERFORM f_execute.

END-OF-SELECTION.
*----------------------------------------------------------------------*
* End - Start-of-Selection                                             *
*----------------------------------------------------------------------*


*----------------------------------------------------------------------*
* At-Selection-Screen                                                  *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
*  PERFORM f_download_template.
  PERFORM f_mandatory_validation.
*
AT SELECTION-SCREEN OUTPUT.
  PERFORM f_modify_screen.
*
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
*  PERFORM f_get_file_dir CHANGING p_file.
*----------------------------------------------------------------------*
* End - At-Selection-Screen                                            *
*----------------------------------------------------------------------*
