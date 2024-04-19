REPORT zaamr008 NO STANDARD PAGE HEADING LINE-SIZE 255
                LINE-COUNT 65 MESSAGE-ID pp.

************************************************************************
*
*   PROGRAM:    ZAAMR008
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Union Gas & Centra Gas
*   DATE:       January 1998.
*   Updated:    November 2002.
*   Programmer: Catherine McCoy
*
*   Changes made: field for Cost Centre was added to Selection Screen.
*   Also lines were removed from report and the Print to file section
*   changed for ease to exporting to excel. Centra Group Assets  have
*   been excluded.
*
*   The purpose of this program is to produce the Asset History by
*   Vintage report.  The last 6 years of transactions (depending on the
*   selection screen) is listed with the opening and closing balace for
*   each main/sub asset.  The report is organized by plant code within
*   vintage within asset class as per the selection criteria.  Totals
*   are produced at each level by which the report is organized as well
*   as by company and final grand totals.
*
************************************************************************

TABLES:   t001,                           " Company codes
          anla,                           " Asset master record-segment
          anlb,                           " Depreciation terms
          anlc,                           " Asset value fields
          anlz.                           " asset allocations (by time)

DATA:     rwerks(4),                                        " rpt plant
          rvyear(4),                                    " rpt vintage
          rowcnt        TYPE i,                         " nbr of rows
          totcnt        TYPE i,                         " total rows
          yr1_hdg(15)   VALUE '          Trans',        " rpt heading
          yr2_hdg(15)   VALUE '          Trans',        " rpt heading
          yr3_hdg(15)   VALUE '          Trans',        " rpt heading
          yr4_hdg(15)   VALUE '          Trans',        " rpt heading
          yr5_hdg(15)   VALUE '          Trans',        " rpt heading
          yr6_hdg(15)   VALUE '          Trans',        " rpt heading
          year1         LIKE anlc-gjahr,                " extract year
          year2         LIKE anlc-gjahr,                " extract year
          year3         LIKE anlc-gjahr,                " extract year
          year4         LIKE anlc-gjahr,                " extract year
          year5         LIKE anlc-gjahr,                " extract year
          year6         LIKE anlc-gjahr,                " extract year
          opening       LIKE anlc-kansw.                " save opening

DATA:     BEGIN OF reptab OCCURS 1000,
            bukrs       LIKE anla-bukrs,                " company code
            anlkl       LIKE anla-anlkl,                " asset class
            vyear       LIKE anlb-vyear,                " vintage year
            werks       LIKE anlz-werks,                " plant code
            kostl       LIKE anlz-kostl,          " Cost centre - cmccoy
            txt50       LIKE anla-txt50,                " description
            anln1       LIKE anlb-anln1,                " main asset
            anln2       LIKE anlb-anln2,                    " sub asset
            menge       like anla-menge,          " quantity
            opening     LIKE anlc-kansw,                " opening balanc
            yr1amt      LIKE anlc-kansw,                " year 1 amount
            yr2amt      LIKE anlc-kansw,                " year 2 amount
            yr3amt      LIKE anlc-kansw,                " year 3 amount
            yr4amt      LIKE anlc-kansw,                " year 4 amount
            yr5amt      LIKE anlc-kansw,                " year 5 amount
            yr6amt      LIKE anlc-kansw,                " year 6 amount
            closing     LIKE anlc-kansw,                " closing balanc
          END OF reptab.

DATA:     BEGIN OF filine,                              " output file ln
            bukrs       LIKE anla-bukrs,                " company code
            sp01(2),                                        " spaces
            anlkl       LIKE anla-anlkl,                " asset class
            sp02(2),                                        " spaces
            vyear(4),                                   " vintage year
            sp03(2),                                        " spaces
            werks       LIKE anlz-werks,                " plant code
            sp04(2),                                        " spaces
            anln1       LIKE anlb-anln1,                " main asset
            sp05(2),                                        " spaces
            anln2       LIKE anlb-anln2,                    " sub asset
            sp06(2),                                        " spaces
            txt50       LIKE anla-txt50,                " description
            sp07(2),                                        " spaces
            menge(18),                                      " quantity
            sp08(2),                                        " spaces
            opening(17),                                " opening balanc
            sp09(2),                                        " spaces
            yr1amt(17),                                 " year 1 amount
            sp10(2),                                        " spaces
            yr2amt(17),                                 " year 2 amount
            sp11(2),                                        " spaces
            yr3amt(17),                                 " year 3 amount
            sp12(2),                                        " spaces
            yr4amt(17),                                 " year 4 amount
            sp13(2),                                        " spaces
            yr5amt(17),                                 " year 5 amount
            sp14(2),                                        " spaces
            yr6amt(17),                                 " year 6 amount
            sp15(2),                                        " spaces
            closing(17),                                " closing balanc
          END OF filine.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 24(27) text-003.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK box1.

SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME.
SELECT-OPTIONS: s_bukrs  FOR anla-bukrs OBLIGATORY
                         NO INTERVALS DEFAULT 'UGL'.
SELECTION-SCREEN END OF BLOCK box2.

SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME.
SELECT-OPTIONS s_anlkl   FOR anla-anlkl.
SELECT-OPTIONS s_anln1   FOR anla-anln1.
SELECT-OPTIONS s_anln2   FOR anla-anln2.
SELECT-OPTIONS s_werks   FOR anlz-werks.
SELECT-OPTIONS s_kostl   FOR anlz-kostl.
SELECT-OPTIONS s_vyear   FOR anlb-vyear.
SELECT-OPTIONS s_gjahr   FOR anlc-gjahr OBLIGATORY
                         DEFAULT sy-datum(4) TO sy-datum(4).
SELECTION-SCREEN END OF BLOCK box3.

SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME TITLE text-040.
PARAMETERS:     p_rprt RADIOBUTTON GROUP rbcr,            "PRINT REPORT
                p_file RADIOBUTTON GROUP rbcr.              "EXCEL FILE
SELECTION-SCREEN END OF BLOCK box4.

SELECTION-SCREEN END OF BLOCK box.

*   End of selection screen.
************************************************************************
* initialize
REFRESH: reptab.
CLEAR:   reptab.

* set up the printing of report headers
TOP-OF-PAGE.
  PERFORM write_header.

* extract required data
START-OF-SELECTION.

* verify fiscal year range is a maximum of six years and setup headings
  year1 = s_gjahr-low + 5.
  IF s_gjahr-high > year1.
    MESSAGE e699 WITH 'Maximum range for Fiscal year is 6'.
  ENDIF.

* establish years report is to cover and allocate appropriate headings
  year1        = s_gjahr-low.
  year2        = s_gjahr-low + 1.
  year3        = s_gjahr-low + 2.
  year4        = s_gjahr-low + 3.
  year5        = s_gjahr-low + 4.
  year6        = s_gjahr-low + 5.
  yr1_hdg+5(4) = year1.
  yr2_hdg+5(4) = year2.
  yr3_hdg+5(4) = year3.
  yr4_hdg+5(4) = year4.
  yr5_hdg+5(4) = year5.
  yr6_hdg+5(4) = year6.
  IF NOT year2 LE s_gjahr-high.
    year2      = space.
    yr2_hdg    = space.
  ENDIF.
  IF NOT year3 LE s_gjahr-high.
    year3      = space.
    yr3_hdg    = space.
  ENDIF.
  IF NOT year4 LE s_gjahr-high.
    year4      = space.
    yr4_hdg    = space.
  ENDIF.
  IF NOT year5 LE s_gjahr-high.
    year5      = space.
    yr5_hdg    = space.
  ENDIF.
  IF NOT year6 LE s_gjahr-high.
    year6      = space.
    yr6_hdg    = space.
  ENDIF.


  SELECT * FROM anla                           " get required assets
  WHERE bukrs IN s_bukrs
  AND   anln1 IN s_anln1
  AND   anln2 IN s_anln2
  AND   anlkl IN s_anlkl.

    CHECK: anla-anln1+5(7) LT '9800000'.       "exclude group assets
     reptab-menge    = anla-menge.                       " quantity " SKAPSE 05/04/2015

    SELECT * FROM anlb                         " get the vintages
    WHERE bukrs EQ anla-bukrs
    AND   anln1 EQ anla-anln1
    AND   anln2 EQ anla-anln2
    AND   vyear IN s_vyear.

      SELECT SINGLE * FROM anlz                " check for valid plant
      WHERE bukrs EQ anla-bukrs
      AND   anln1 EQ anla-anln1
      AND   anln2 EQ anla-anln2
      AND   bdatu EQ anlb-bdatu
      AND   kostl IN s_kostl
      AND   werks IN s_werks.
      CHECK sy-subrc = 0.

      CHECK sy-subrc = 0.
      rowcnt = 0.

      SELECT * FROM anlc                       " count entries to read
      WHERE bukrs EQ anla-bukrs
      AND   anln1 EQ anla-anln1
      AND   anln2 EQ anla-anln2
      AND   gjahr IN s_gjahr
      AND   afabe EQ anlb-afabe.
      ENDSELECT.
      totcnt = sy-dbcnt.

      SELECT * FROM anlc                       " get the asset amounts
      WHERE bukrs EQ anla-bukrs
      AND   anln1 EQ anla-anln1
      AND   anln2 EQ anla-anln2
      AND   gjahr IN s_gjahr
      AND   afabe EQ anlb-afabe.

        rowcnt          = rowcnt + 1.                   " count rows
        reptab-bukrs    = anla-bukrs.                   " company code
        reptab-anlkl    = anla-anlkl.                   " asset class
        reptab-vyear    = anlb-vyear.                   " vintage year
        reptab-werks    = anlz-werks.                   " plant code
        reptab-kostl    = anlz-kostl.                   " cost centre
        reptab-anln1    = anlb-anln1.                   " main asset
        reptab-anln2    = anlb-anln2.                       " sub asset
        reptab-txt50    = anla-txt50.                   " description
       "" reptab-menge    = anla-menge.                       " quantity  "skapse 05/04/2015
        IF rowcnt       = 1.                            " only first row
          reptab-opening  = anlc-kansw + anlc-kinvz.    " opening balanc
          opening       = reptab-opening.
        ENDIF.
        IF anlc-gjahr   = year1.                        " year 1 amount
          reptab-yr1amt = anlc-answl.
        ENDIF.
        IF anlc-gjahr   = year2.                        " year 2 amount
          reptab-yr2amt = anlc-answl.
        ENDIF.
        IF anlc-gjahr   = year3.                        " year 3 amount
          reptab-yr3amt = anlc-answl.
        ENDIF.
        IF anlc-gjahr   = year4.                        " year 4 amount
          reptab-yr4amt = anlc-answl.
        ENDIF.
        IF anlc-gjahr   = year5.                        " year 5 amount
          reptab-yr5amt = anlc-answl.
        ENDIF.
        IF anlc-gjahr   = year6.                        " year 6 amount
          reptab-yr6amt = anlc-answl.
        ENDIF.
        reptab-closing  = anlc-answl.                   " closing balanc
        IF rowcnt       = totcnt.                       " only last row
          reptab-closing  = reptab-closing + opening.   " closing balanc
        ENDIF.
*        IF reptab-opening <> 0                       "Change by M. Khan
        IF opening <> 0
        OR reptab-closing <> 0.
          APPEND reptab.
          CLEAR reptab.
        ENDIF.

      ENDSELECT.
    ENDSELECT.
  ENDSELECT.

* sort the report table.
  SORT reptab BY bukrs anlkl vyear werks anln1 anln2.

* Process the table, outputing the report
  IF p_rprt = 'X'.
    LOOP AT reptab.
      AT NEW bukrs.
        NEW-PAGE.
      ENDAT.
      AT NEW anlkl.
        NEW-PAGE.
      ENDAT.
      AT NEW vyear.
        rvyear = reptab-vyear.
      ENDAT.
      AT NEW werks.
        rwerks = reptab-werks.
      ENDAT.
      AT END OF anln2.
        SUM.
        PERFORM write_detail.
      ENDAT.
      AT END OF werks.
        SUM.
        PERFORM write_plant_total.
      ENDAT.
      AT END OF vyear.
        SUM.
        PERFORM write_vintage_total.
        SKIP 2.
      ENDAT.
      AT END OF anlkl.
        SUM.
        PERFORM write_class_total.
      ENDAT.
      AT END OF bukrs.
        SUM.
        PERFORM write_comp_total.
      ENDAT.
      AT LAST.
        SUM.
        PERFORM write_report_total.
      ENDAT.
    ENDLOOP.
  ENDIF.
  IF p_file = 'X'.
    PERFORM write_file_header.
    LOOP AT reptab.
      AT NEW anlkl.
        FORMAT INTENSIFIED ON.
        WRITE:  /003 text-004, reptab-anlkl.
        FORMAT INTENSIFIED OFF.
      ENDAT.
      AT NEW vyear.
        rvyear = reptab-vyear.
      ENDAT.
      AT NEW werks.
        rwerks = reptab-werks.
      ENDAT.
      AT END OF anln2.
        SUM.
        PERFORM write_detail.
      ENDAT.
      AT END OF werks.
        SUM.
        PERFORM write_plant_total.
      ENDAT.
      AT END OF vyear.
        SUM.
        PERFORM write_vintage_total.
        SKIP.
      ENDAT.
      AT END OF anlkl.
        SUM.
        PERFORM write_class_total.
        SKIP 2.
      ENDAT.
      AT END OF bukrs.
        SUM.
        PERFORM write_comp_total.
      ENDAT.
      AT LAST.
        SUM.
        PERFORM write_report_total.
      ENDAT.
    ENDLOOP.
  ENDIF.

END-OF-SELECTION.

************************************************************************
*  Listed below are subroutines used by the program.
************************************************************************

FORM write_header.
  SELECT SINGLE * FROM t001
  WHERE  bukrs EQ reptab-bukrs.
  FORMAT INTENSIFIED OFF.
  SKIP 2.
  IF p_rprt = 'X'.
    WRITE:  /003 sy-datum
           , 128 t001-butxt
           , 243 text-002,  sy-pagno
           , /003 sy-uzeit
           , 126 text-003
    ,     sy-repid UNDER text-002
    , /003 text-clt, sy-mandt, sy-sysid.

    SKIP.
    WRITE:  /003 text-004, reptab-anlkl.
    ULINE.
    FORMAT INTENSIFIED ON.
    WRITE:   /03 text-008
           , 048 text-006
           , 055 text-005
           , 062 text-007
           , 077 text-034
           , 086 text-033
*           , 097 text-009
*           , 115 text-010
           , 096 text-009
           , 114 text-010
           , 130 yr1_hdg
           , 148 yr2_hdg
           , 166 yr3_hdg
           , 184 yr4_hdg
           , 202 yr5_hdg
           , 220 yr6_hdg
           , 239 text-017.
    FORMAT INTENSIFIED OFF.
    ULINE.
    SKIP.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_FILE_HEADER                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_file_header.
  WRITE: /003 text-003
             , /003 sy-datum
             , /003 sy-repid
             , /003 text-clt, sy-mandt, sy-sysid.
  ULINE.
  FORMAT INTENSIFIED ON.
  WRITE:   /03 text-008
         , 048 text-006
         , 055 text-005
         , 062 text-007
         , 077 text-034
         , 086 text-033
*         , 097 text-009
*         , 115 text-010
         , 096 text-009
         , 114 text-010
         , 130 yr1_hdg
         , 148 yr2_hdg
         , 166 yr3_hdg
         , 184 yr4_hdg
         , 202 yr5_hdg
         , 220 yr6_hdg
         , 239 text-017.
  FORMAT INTENSIFIED OFF.
  ULINE.
  SKIP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_DETAIL                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_detail.
  FORMAT INTENSIFIED OFF.
  WRITE: /     reptab-txt50   UNDER text-008
         ,     rvyear         UNDER text-006
         ,     rwerks         UNDER text-005
         ,     reptab-anln1   UNDER text-007
         ,     reptab-anln2   UNDER text-034
         ,     reptab-kostl   UNDER text-033
         ,     reptab-menge   UNDER text-009
         ,     reptab-opening UNDER text-010
         ,     reptab-yr1amt  UNDER yr1_hdg
         ,     reptab-yr2amt  UNDER yr2_hdg
         ,     reptab-yr3amt  UNDER yr3_hdg
         ,     reptab-yr4amt  UNDER yr4_hdg
         ,     reptab-yr5amt  UNDER yr5_hdg
         ,     reptab-yr6amt  UNDER yr6_hdg
         ,     reptab-closing UNDER text-017.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_PLANT_TOTAL                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_plant_total.
  FORMAT INTENSIFIED ON.
  SKIP.
  WRITE:   /03 text-019,reptab-werks
         ,     reptab-menge   UNDER text-009
         ,     reptab-opening UNDER text-010
         ,     reptab-yr1amt  UNDER yr1_hdg
         ,     reptab-yr2amt  UNDER yr2_hdg
         ,     reptab-yr3amt  UNDER yr3_hdg
         ,     reptab-yr4amt  UNDER yr4_hdg
         ,     reptab-yr5amt  UNDER yr5_hdg
         ,     reptab-yr6amt  UNDER yr6_hdg
         ,     reptab-closing UNDER text-017.
  FORMAT INTENSIFIED OFF.
  SKIP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_VINTAGE_TOTAL                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_vintage_total.
  FORMAT INTENSIFIED ON.
  WRITE:   /03 text-019,reptab-vyear
         ,     reptab-menge   UNDER text-009
         ,     reptab-opening UNDER text-010
         ,     reptab-yr1amt  UNDER yr1_hdg
         ,     reptab-yr2amt  UNDER yr2_hdg
         ,     reptab-yr3amt  UNDER yr3_hdg
         ,     reptab-yr4amt  UNDER yr4_hdg
         ,     reptab-yr5amt  UNDER yr5_hdg
         ,     reptab-yr6amt  UNDER yr6_hdg
         ,     reptab-closing UNDER text-017.
  FORMAT INTENSIFIED OFF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_CLASS_TOTAL                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_class_total.
  FORMAT INTENSIFIED ON.
  WRITE:   /03 text-020,reptab-anlkl
         ,     reptab-menge   UNDER text-009
         ,     reptab-opening UNDER text-010
         ,     reptab-yr1amt  UNDER yr1_hdg
         ,     reptab-yr2amt  UNDER yr2_hdg
         ,     reptab-yr3amt  UNDER yr3_hdg
         ,     reptab-yr4amt  UNDER yr4_hdg
         ,     reptab-yr5amt  UNDER yr5_hdg
         ,     reptab-yr6amt  UNDER yr6_hdg
         ,     reptab-closing UNDER text-017.
  FORMAT INTENSIFIED OFF.
  WRITE: / sy-uline(24) UNDER text-020.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_COMP_TOTAL                                         *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_comp_total.
  SKIP.
  FORMAT INTENSIFIED ON.
  WRITE:   /03 text-021
         ,     reptab-menge   UNDER text-009
         ,     reptab-opening UNDER text-010
         ,     reptab-yr1amt  UNDER yr1_hdg
         ,     reptab-yr2amt  UNDER yr2_hdg
         ,     reptab-yr3amt  UNDER yr3_hdg
         ,     reptab-yr4amt  UNDER yr4_hdg
         ,     reptab-yr5amt  UNDER yr5_hdg
         ,     reptab-yr6amt  UNDER yr6_hdg
         ,     reptab-closing UNDER text-017.
  FORMAT INTENSIFIED OFF.
  WRITE: / sy-uline(13) UNDER text-020.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM WRITE_REPORT_TOTAL                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM write_report_total.
  SKIP.
  FORMAT INTENSIFIED ON.
  WRITE:   /03 text-022
         ,     reptab-menge   UNDER text-009
         ,     reptab-opening UNDER text-010
         ,     reptab-yr1amt  UNDER yr1_hdg
         ,     reptab-yr2amt  UNDER yr2_hdg
         ,     reptab-yr3amt  UNDER yr3_hdg
         ,     reptab-yr4amt  UNDER yr4_hdg
         ,     reptab-yr5amt  UNDER yr5_hdg
         ,     reptab-yr6amt  UNDER yr6_hdg
         ,     reptab-closing UNDER text-017.
  FORMAT INTENSIFIED OFF.
  WRITE: / sy-uline(12) UNDER text-020.

ENDFORM.

************************************************************************
