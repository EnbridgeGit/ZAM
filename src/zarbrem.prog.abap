*&---------------------------------------------------------------------*
*& Report  ZARBREM
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Program Name       : ZARBREM                                        *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 07-Jan-2014                                    *
*& Object ID          : SDP46414 Remittance Advice (ZARBREM) Batch     *
*&                      Program                                        *
*& Application Area   : MM                                             *
*& Description        : This report generates the Remittance Advice for*
*&                      Ariba.                                         *
*&---------------------------------------------------------------------*
*----------------------------------------------------------------------*
*Modification Log(Latest Version on Top)                               *
*----------------------------------------------------------------------*
* Version No    : 1.0                                                  *
* Date          : 17-Apr-2014                                          *
* Modified By   : Praveena Anusuri                                     *
* Correction No : D30K923329                                           *
*& Description  : Send Check Number to Ariba                           *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 2.0                                                  *
* Date          : 02-May-2014                                          *
* Modified By   : Praveena Anusuri                                     *
* Correction No : D30K923399                                           *
*& Description  : If there is no payment made, then no Ariba Invoice   *
*&                status/remittance should be sent to the vendor       *
*----------------------------------------------------------------------*
*----------------------------------------------------------------------*
* Version No    : 3.0                                                  *
* Date          : 09-Oct-2014                                          *
* Modified By   : Praveena Anusuri                                     *
* Correction No : D30K923399                                           *
*& Description  : Vendor info not populating.                          *
*----------------------------------------------------------------------*

REPORT  zarbrem NO STANDARD PAGE HEADING
                LINE-SIZE 132.

*Include for data declarations
INCLUDE zarbrem_top.
*Include for subroutines
INCLUDE zarbrem_f01.

*----------------------------------------------------------------------*
*START-OF-SELECTION
*----------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM get_pymt_details.
  PERFORM get_output_data.

*----------------------------------------------------------------------*
*END-OF-SELECTION
*----------------------------------------------------------------------*
END-OF-SELECTION.
  IF lt_output_alv IS NOT INITIAL.
    PERFORM display_output.
  ELSE.
    MESSAGE 'No data selected'(002) TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.
