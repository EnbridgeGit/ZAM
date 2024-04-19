REPORT ZAAMR007 NO STANDARD PAGE HEADING LINE-SIZE 205
                LINE-COUNT 65.

************************************************************************
*
*   PROGRAM:    ZAAMR007
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Union Gas & Centra Gas
*   DATE:       November 1997.
*
*   The purpose of this program is to produce the Gross Cost of Plant
*   report.  The opening balance, additions, transfers, retirements and
*   closing balance is listed for each asset group within the FI account
*   to which it relates.  Totals are produced at the asset group and FI
*   account level as well as grand totals at the end of the report.
*
************************************************************************

TABLES:   T001,                           " Company codes
          ANLA,                           " Asset master record-segment
          ANLB,                           " Depreciation terms
          ANLC,                           " Asset value fields
          ANEP.                           " Asset line item

DATA:     FIACCT        LIKE ANLA-KTOGR,                " rpt FI account
          ASSET         LIKE ANLA-ANLN1.                " rpt grp asset

DATA:     BEGIN OF ASSTAB OCCURS 1000,
            BUKRS       LIKE ANLA-BUKRS,                " company code
            KTOGR       LIKE ANLA-KTOGR,                " FI account nbr
            ANLGR       LIKE ANLB-ANLGR,                " group asset
            TXT50       LIKE ANLA-TXT50,                " description
            ANLGR2      LIKE ANLB-ANLGR2,               " grp asset sub
            ANLN1       LIKE ANLB-ANLN1,                " main asset
            ANLN2       LIKE ANLB-ANLN2,                " sub asset
          END OF ASSTAB.

DATA:     BEGIN OF REPTAB OCCURS 1000,
            BUKRS       LIKE ANLA-BUKRS,                " company code
            KTOGR       LIKE ANLA-KTOGR,                " FI account nbr
            ANLGR       LIKE ANLB-ANLGR,                " group asset
            TXT50       LIKE ANLA-TXT50,                " description
            ANLGR2      LIKE ANLB-ANLGR2,               " sub grp asset
            OPENING     LIKE ANLC-KANSW,                " opening balanc
            ADDS        LIKE ANLC-KANSW,                " additions
            TRANS       LIKE ANLC-KANSW,                " transfers
            RETIRS      LIKE ANLC-KANSW,                " retirements
            OTHERS      LIKE ANLC-KANSW,                " others
            CLOSING     LIKE ANLC-KANSW,                " closing balanc
          END OF REPTAB.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 26(27) TEXT-003.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
      SELECT-OPTIONS: S_BUKRS  FOR ANLA-BUKRS OBLIGATORY
                               NO INTERVALS DEFAULT 'UGL'.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(27) TEXT-004.
      PARAMETERS: P_GJAHR      LIKE ANLC-GJAHR OBLIGATORY
                               DEFAULT SY-DATUM(4).
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-017.
      SELECT-OPTIONS S_ANLGR   FOR ANLB-ANLGR.
      SELECT-OPTIONS S_ANLN1   FOR ANLA-ANLN1.
      SELECT-OPTIONS S_ANLN2   FOR ANLA-ANLN2.
      SELECT-OPTIONS S_ANLKL   FOR ANLA-ANLKL OBLIGATORY
                               DEFAULT '40000' TO '49999'.
   SELECTION-SCREEN SKIP.
   SELECTION-SCREEN BEGIN OF BLOCK BOX3A WITH FRAME TITLE TEXT-020.
      SELECT-OPTIONS: S_ADDS   FOR ANEP-BWASL OBLIGATORY
                               NO INTERVALS.
      SELECT-OPTIONS: S_TRANS  FOR ANEP-BWASL OBLIGATORY
                               NO INTERVALS.
      SELECT-OPTIONS: S_RETIRS FOR ANEP-BWASL OBLIGATORY
                               NO INTERVALS.
   SELECTION-SCREEN END OF BLOCK BOX3A.
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
************************************************************************
* initialize
REFRESH: ASSTAB, REPTAB.
CLEAR:   ASSTAB, REPTAB.

* set up the printing of report headers
TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

* extract required data
START-OF-SELECTION.

  SELECT * FROM ANLB                           " get required grp assets
  WHERE BUKRS IN S_BUKRS
  AND   ANLGR IN S_ANLGR
  AND   ANLN1 IN S_ANLN1
  AND   ANLN2 IN S_ANLN2.

    SELECT * FROM ANLA                         " get the assets
    WHERE BUKRS EQ ANLB-BUKRS
    AND   ANLKL IN S_ANLKL
    AND   ANLN1 EQ ANLB-ANLN1
    AND   ANLN2 EQ ANLB-ANLN2.

      ASSTAB-BUKRS  = ANLA-BUKRS.
      ASSTAB-KTOGR  = ANLA-KTOGR.
      ASSTAB-ANLN1  = ANLA-ANLN1.
      ASSTAB-ANLN2  = ANLA-ANLN2.
      ASSTAB-TXT50  = ANLA-TXT50.
      ASSTAB-ANLGR  = ANLB-ANLGR.
      ASSTAB-ANLGR2 = ANLB-ANLGR2.
      APPEND ASSTAB.
      CLEAR ASSTAB.

    ENDSELECT.
  ENDSELECT.

  LOOP AT ASSTAB.

    SELECT * FROM ANLC                         " get open/close balances
    WHERE BUKRS EQ ASSTAB-BUKRS
    AND   ANLN1 EQ ASSTAB-ANLN1
    AND   ANLN2 EQ ASSTAB-ANLN2
    AND   GJAHR EQ P_GJAHR
    AND   AFABE EQ '01'.

      REPTAB-OPENING = REPTAB-OPENING
                     + ANLC-KANSW
                     + ANLC-KINVZ.
      REPTAB-CLOSING = REPTAB-CLOSING
                     + ANLC-KANSW
                     + ANLC-KINVZ
                     + ANLC-ANSWL.

    ENDSELECT.

    SELECT * FROM ANEP                         " adds/transfers/retirmnt
    WHERE BUKRS EQ ASSTAB-BUKRS
    AND   ANLN1 EQ ASSTAB-ANLN1
    AND   ANLN2 EQ ASSTAB-ANLN2
    AND   GJAHR EQ P_GJAHR
    AND   AFABE EQ '01'.

      IF ANEP-BWASL IN S_ADDS.
        REPTAB-ADDS   = REPTAB-ADDS
                      + ANEP-ANBTR.
      ELSEIF ANEP-BWASL IN S_TRANS.
        REPTAB-TRANS  = REPTAB-TRANS
                      + ANEP-ANBTR.
      ELSEIF ANEP-BWASL IN S_RETIRS.
        REPTAB-RETIRS = REPTAB-RETIRS
                      + ANEP-ANBTR.
      ELSE.
        REPTAB-OTHERS = REPTAB-OTHERS
                      + ANEP-ANBTR.
      ENDIF.

    ENDSELECT.

    IF  REPTAB-OPENING <> 0
    OR  REPTAB-ADDS    <> 0
    OR  REPTAB-TRANS   <> 0
    OR  REPTAB-RETIRS  <> 0
    OR  REPTAB-CLOSING <> 0.

      REPTAB-BUKRS  = ASSTAB-BUKRS.
      REPTAB-KTOGR  = ASSTAB-KTOGR.
      REPTAB-ANLGR  = ASSTAB-ANLGR.
      REPTAB-ANLGR2 = ASSTAB-ANLGR2.
      REPTAB-TXT50  = ASSTAB-TXT50.
      APPEND REPTAB.
      CLEAR REPTAB.

    ENDIF.

  ENDLOOP.
  REFRESH ASSTAB.

* sort the gross cost of plant report table.
  SORT REPTAB BY BUKRS KTOGR ANLGR ANLGR2.

* Process the table, outputing the report and do update if required
  LOOP AT REPTAB.
    AT NEW BUKRS.
      NEW-PAGE.
    ENDAT.
    AT NEW KTOGR.
      FIACCT = REPTAB-KTOGR.
    ENDAT.
    AT NEW ANLGR.
      ASSET  = REPTAB-ANLGR.
    ENDAT.
    AT END OF ANLGR2.
      SUM.
      PERFORM WRITE_DETAIL.
      FIACCT = SPACE.
      ASSET  = SPACE.
    ENDAT.
    AT END OF ANLGR.
      SUM.
      PERFORM WRITE_GROUP_TOTAL.
    ENDAT.
    AT END OF KTOGR.
      SUM.
      PERFORM WRITE_FIACCT_TOTAL.
    ENDAT.
    AT END OF BUKRS.
      SUM.
      PERFORM WRITE_COMP_TOTAL.
    ENDAT.
    AT LAST.
      SUM.
      PERFORM WRITE_REPORT_TOTAL.
    ENDAT.
  endloop.

END-OF-SELECTION.

************************************************************************
*  Listed below are subroutines used by the program.
************************************************************************

FORM WRITE_HEADER.
     SELECT SINGLE * FROM T001
     WHERE  BUKRS EQ REPTAB-BUKRS.
     FORMAT INTENSIFIED OFF.
     ULINE.
     WRITE:   /01 SY-VLINE
            , 003 SY-DATUM
            , 093 T001-BUTXT
            , 193 TEXT-002,  SY-PAGNO
            , 205 SY-VLINE.
     WRITE:   /01 SY-VLINE
            ,     SY-UZEIT UNDER SY-DATUM
            , 093 TEXT-003
            ,     SY-REPID UNDER TEXT-002
            , 205 SY-VLINE.
     WRITE:   /01 SY-VLINE
            , 094 TEXT-004, P_GJAHR
            , 205 SY-VLINE.
     ULINE.
     WRITE:   /03 TEXT-006
            , 014 TEXT-023
            , 029 TEXT-024
            , 089 TEXT-009
            , 109 TEXT-010
            , 129 TEXT-011
            , 149 TEXT-012
            , 169 TEXT-022
            , 189 TEXT-013.
     PERFORM SHOWVLINE.
     FORMAT INTENSIFIED ON.
     ULINE.
ENDFORM.

FORM WRITE_DETAIL.
     WRITE: /     FIACCT         UNDER TEXT-006
            ,     ASSET          UNDER TEXT-023
            ,     REPTAB-ANLGR2  UNDER TEXT-024
            ,     REPTAB-TXT50
            ,     REPTAB-OPENING UNDER TEXT-009
            ,     REPTAB-ADDS    UNDER TEXT-010
            ,     REPTAB-TRANS   UNDER TEXT-011
            ,     REPTAB-RETIRS  UNDER TEXT-012
            ,     REPTAB-OTHERS  UNDER TEXT-022
            ,     REPTAB-CLOSING UNDER TEXT-013.
     PERFORM SHOWVLINE.
ENDFORM.

FORM WRITE_GROUP_TOTAL.
     FORMAT INTENSIFIED OFF.
     WRITE:   /01 SY-VLINE
            , 012 SY-VLINE.
     ULINE AT 27.
     WRITE:   /14 TEXT-014
            ,     REPTAB-OPENING UNDER TEXT-009
            ,     REPTAB-ADDS    UNDER TEXT-010
            ,     REPTAB-TRANS   UNDER TEXT-011
            ,     REPTAB-RETIRS  UNDER TEXT-012
            ,     REPTAB-OTHERS  UNDER TEXT-022
            ,     REPTAB-CLOSING UNDER TEXT-013.
     PERFORM SHOWVLINE_ASSET.
     WRITE:   /01 SY-VLINE.
     ULINE AT 12.
     FORMAT INTENSIFIED ON.
ENDFORM.

FORM WRITE_FIACCT_TOTAL.
     FORMAT INTENSIFIED OFF.
     WRITE:   /03 TEXT-015
            ,     REPTAB-OPENING UNDER TEXT-009
            ,     REPTAB-ADDS    UNDER TEXT-010
            ,     REPTAB-TRANS   UNDER TEXT-011
            ,     REPTAB-RETIRS  UNDER TEXT-012
            ,     REPTAB-OTHERS  UNDER TEXT-022
            ,     REPTAB-CLOSING UNDER TEXT-013.
     PERFORM SHOWVLINE_ACCT.
     ULINE.
     FORMAT INTENSIFIED ON.
ENDFORM.

FORM WRITE_COMP_TOTAL.
     ULINE.
     FORMAT INTENSIFIED OFF.
     WRITE:   /03 TEXT-021
            ,     REPTAB-OPENING UNDER TEXT-009
            ,     REPTAB-ADDS    UNDER TEXT-010
            ,     REPTAB-TRANS   UNDER TEXT-011
            ,     REPTAB-RETIRS  UNDER TEXT-012
            ,     REPTAB-OTHERS  UNDER TEXT-022
            ,     REPTAB-CLOSING UNDER TEXT-013.
     PERFORM SHOWVLINE_ACCT.
     ULINE.
     FORMAT INTENSIFIED ON.
ENDFORM.

FORM WRITE_REPORT_TOTAL.
     SKIP.
     ULINE.
     SKIP.
     ULINE.
     FORMAT INTENSIFIED OFF.
     WRITE:   /03 TEXT-016
            ,     REPTAB-OPENING UNDER TEXT-009
            ,     REPTAB-ADDS    UNDER TEXT-010
            ,     REPTAB-TRANS   UNDER TEXT-011
            ,     REPTAB-RETIRS  UNDER TEXT-012
            ,     REPTAB-OTHERS  UNDER TEXT-022
            ,     REPTAB-CLOSING UNDER TEXT-013.
     PERFORM SHOWVLINE_ACCT.
     ULINE.
     FORMAT INTENSIFIED OFF.
ENDFORM.

FORM ShowVline.
     WRITE:   001 SY-VLINE
            , 012 SY-VLINE
            , 027 SY-VLINE
            , 085 SY-VLINE
            , 105 SY-VLINE
            , 125 SY-VLINE
            , 145 SY-VLINE
            , 165 SY-VLINE
            , 185 SY-VLINE
            , 205 SY-VLINE.
ENDFORM.

FORM SHOWVLINE_ASSET.
     WRITE:   001 SY-VLINE
            , 012 SY-VLINE
            , 085 SY-VLINE
            , 105 SY-VLINE
            , 125 SY-VLINE
            , 145 SY-VLINE
            , 165 SY-VLINE
            , 185 SY-VLINE
            , 205 SY-VLINE.
ENDFORM.

FORM SHOWVLINE_ACCT.
     WRITE:   001 SY-VLINE
            , 085 SY-VLINE
            , 105 SY-VLINE
            , 125 SY-VLINE
            , 145 SY-VLINE
            , 165 SY-VLINE
            , 185 SY-VLINE
            , 205 SY-VLINE.
ENDFORM.

************************************************************************
*  This is the end my freind
************************************************************************
