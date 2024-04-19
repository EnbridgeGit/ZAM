REPORT ZAAMR009 NO STANDARD PAGE HEADING LINE-SIZE 93
                LINE-COUNT 65 MESSAGE-ID PP.
************************************************************************
*
*   PROGRAM:    ZAAMR009
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Union Gas & Centra Gas
*   DATE:       January 1998.
*
*   The purpose of this program is to list all FI asset documents which
*   do not have a corresponding entry in AM.  This is to highlight any
*   discrepancies between the two systems.  FI entries are found in
*   BSEG, the report contains those entries that are not found in ANEP.
*   Totals are produced by asset, G/L acccount and company, as well as
*   final report totals.
*
*   Bsically this program is a copy of ZASKAB00 with additional info.
*
************************************************************************
*
*TR582  2009/11/18 Mohammad   IFRS: Add the Depreciation Area selection
*                             filed as a range field and make other
*                             changes accordingly.
*
************************************************************************

TABLES:   BKPF,                           " Accounting document header
          BSEG,                           " Accounting document segment
          ANEP,                           " Asset line item
          T001.                           " Company codes

DATA:     RHKONT        LIKE BSEG-HKONT,                " rpt GL account
          RANLN1        LIKE BSEG-ANLN1,                " rpt asset
          RANLN2        LIKE BSEG-ANLN2,                " rpt sub asset
          DOCNBR        LIKE ANEP-BELNR,                " ANEP doc nbr
          RECSFND(1)    VALUE 'N'.                      " records found

DATA:     BEGIN OF BSEGTAB OCCURS 0,
            BUKRS       LIKE BSEG-BUKRS,                " company code
            HKONT       LIKE BSEG-HKONT,                " GL account
            ANLN1       LIKE BSEG-ANLN1,                " main asset
            ANLN2       LIKE BSEG-ANLN2,                " sub asset
            ANBWA       LIKE BSEG-ANBWA,                " asset trx type
            BEWAR       LIKE BSEG-BEWAR,                " FI trx type
            BSCHL       LIKE BSEG-BSCHL,                " posting key
            BELNR       LIKE BSEG-BELNR,                " document nbr
            BUZEI       LIKE BSEG-BUZEI,                " line item
            DMBTR       LIKE ANEP-ANBTR,                " amount
          END OF BSEGTAB.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 24(27) TEXT-003.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(27) TEXT-018.
      PARAMETERS: P_BUKRS      LIKE BSEG-BUKRS OBLIGATORY
                               DEFAULT 'UGL'.
   SELECTION-SCREEN END OF LINE.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(27) TEXT-018.
      PARAMETERS: P_GJAHR      LIKE BSEG-GJAHR OBLIGATORY
                               DEFAULT SY-DATUM(4).
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
      SELECT-OPTIONS: S_HKONT  FOR BSEG-HKONT.
      SELECT-OPTIONS: S_ANLN1  FOR BSEG-ANLN1.
      SELECT-OPTIONS: S_ANLN2  FOR BSEG-ANLN2.
      SELECT-OPTIONS: S_ANBWA  FOR BSEG-ANBWA.
      SELECT-OPTIONS: S_BEWAR  FOR BSEG-BEWAR.
      SELECT-OPTIONS: S_AFABE  FOR ANEP-AFABE.                   "TR582
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN END OF BLOCK BOX.

*   End of selection screen.
************************************************************************
* initialize
REFRESH: BSEGTAB.
CLEAR:   BSEGTAB.

* set up the printing of report headers
TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

* extract required data
START-OF-SELECTION.


  SELECT * FROM BSEG                           " get the FI documents
  WHERE  BUKRS EQ P_BUKRS
  AND    GJAHR EQ P_GJAHR
  AND    HKONT IN S_HKONT
  AND    KOART EQ 'A'
  AND    ANLN1 IN S_ANLN1
  AND    ANLN2 IN S_ANLN2
  AND    ANBWA IN S_ANBWA
  AND    BEWAR IN S_BEWAR.

    SELECT SINGLE * FROM BKPF
    WHERE  BUKRS  EQ  BSEG-BUKRS
    AND    BELNR  EQ  BSEG-BELNR
    AND    GJAHR  EQ  BSEG-GJAHR.
    DOCNBR = BKPF-AWKEY(10).

    SELECT SINGLE * FROM ANEP
    WHERE  BUKRS  EQ  BSEG-BUKRS
    AND    ANLN1  EQ  BSEG-ANLN1
    AND    ANLN2  EQ  BSEG-ANLN2
    AND    GJAHR  EQ  BSEG-GJAHR
    AND    AFABE  IN  S_AFABE                                    "TR582
*    AND    AFABE  EQ  '01'                                      "TR582
    AND    BELNR  EQ  BKPF-AWKEY.

    IF SY-SUBRC <> 0.
      RECSFND       = 'Y'.                              " set indicator
      BSEGTAB-BUKRS = BSEG-BUKRS.                       " company code
      BSEGTAB-HKONT = BSEG-HKONT.                       " GL account
      BSEGTAB-ANLN1 = BSEG-ANLN1.                       " main asset
      BSEGTAB-ANLN2 = BSEG-ANLN2.                       " sub asset
      BSEGTAB-ANBWA = BSEG-ANBWA.                       " asset trx type
      BSEGTAB-BEWAR = BSEG-BEWAR.                       " FI trx type
      BSEGTAB-BSCHL = BSEG-BSCHL.                       " posting key
      BSEGTAB-BELNR = BSEG-BELNR.                       " document nbr
      BSEGTAB-BUZEI = BSEG-BUZEI.                       " line item
      BSEGTAB-DMBTR = BSEG-DMBTR.                       " amount
      IF BSEG-BSCHL = '75'.
        BSEGTAB-DMBTR = BSEGTAB-DMBTR * -1.
      ENDIF.
      APPEND BSEGTAB.
      CLEAR BSEGTAB.
    ENDIF.

  ENDSELECT.

* sort the report table.
* sort bsegtab by bukrs hkont anln1 anln2 belnr buzei.
  SORT BSEGTAB BY BUKRS HKONT ANLN1 ANLN2 BELNR BUZEI.

* Process the table, outputing the report
  LOOP AT BSEGTAB.
    AT NEW BUKRS.
      NEW-PAGE.
    ENDAT.
    AT NEW HKONT.
      RHKONT = BSEGTAB-HKONT.
    ENDAT.
    AT NEW ANLN2.
      RANLN1 = BSEGTAB-ANLN1.
      RANLN2 = BSEGTAB-ANLN2.
    ENDAT.
    AT END OF BUZEI.
      SUM.
      PERFORM WRITE_DETAIL.
      RHKONT = SPACE.
      RANLN1 = SPACE.
      RANLN2 = SPACE.
    ENDAT.
    AT END OF ANLN2.
      SUM.
      PERFORM WRITE_ASSET_TOTAL.
    ENDAT.
    AT END OF HKONT.
      SUM.
      PERFORM WRITE_ACCOUNT_TOTAL.
    ENDAT.
    AT END OF BUKRS.
      SUM.
      PERFORM WRITE_COMP_TOTAL.
    ENDAT.
    AT LAST.
      SUM.
      PERFORM WRITE_REPORT_TOTAL.
    ENDAT.
  ENDLOOP.

  IF RECSFND = 'N'.
     ULINE.
     WRITE:   001 SY-VLINE
            , 003 TEXT-023
            , 093 SY-VLINE.
     ULINE.
  ENDIF.

END-OF-SELECTION.

************************************************************************
*  Listed below are subroutines used by the program.
************************************************************************

FORM WRITE_HEADER.
     SELECT SINGLE * FROM T001
     WHERE  BUKRS EQ P_BUKRS.
     FORMAT INTENSIFIED OFF.
     ULINE.
     WRITE:   /01 SY-VLINE
            , 003 SY-DATUM
            , 037 T001-BUTXT
            , 081 TEXT-002,  SY-PAGNO
            , 093 SY-VLINE.
     WRITE:   /01 SY-VLINE
            ,     SY-UZEIT UNDER SY-DATUM
            , 033 TEXT-003
            ,     SY-REPID UNDER TEXT-002
            , 093 SY-VLINE.
     WRITE:   /01 SY-VLINE
            , 039 TEXT-004, P_GJAHR
            , 093 SY-VLINE.
     ULINE.
     WRITE:   /03 TEXT-005
            , 016 TEXT-006
            , 029 TEXT-007
            , 036 TEXT-008
            , 047 TEXT-009
            , 054 TEXT-010
            , 064 TEXT-011
            , 070 TEXT-012
            , 090 TEXT-022.
     PERFORM SHOWVLINE.
     FORMAT INTENSIFIED ON.
     ULINE.
ENDFORM.

FORM WRITE_DETAIL.
     WRITE: /     RHKONT         UNDER TEXT-005
            ,     RANLN1         UNDER TEXT-006
            ,     RANLN2         UNDER TEXT-007
            ,     BSEGTAB-BELNR  UNDER TEXT-008
            ,     BSEGTAB-BUZEI  UNDER TEXT-009
            ,     BSEGTAB-ANBWA  UNDER TEXT-010
            ,     BSEGTAB-BEWAR  UNDER TEXT-011
            ,     BSEGTAB-DMBTR  UNDER TEXT-012
            ,     BSEGTAB-BSCHL  UNDER TEXT-022.
     PERFORM SHOWVLINE.
ENDFORM.

FORM WRITE_ASSET_TOTAL.
     FORMAT INTENSIFIED OFF.
     WRITE:   /01 SY-VLINE
            , 014 SY-VLINE.
     ULINE AT 34.
     WRITE:   /   TEXT-013       UNDER TEXT-006
            ,     BSEGTAB-DMBTR  UNDER TEXT-012.
     PERFORM SHOWVLINE_ASSET.
     WRITE:   /01 SY-VLINE.
     ULINE AT 14.
     FORMAT INTENSIFIED ON.
ENDFORM.

FORM WRITE_ACCOUNT_TOTAL.
     FORMAT INTENSIFIED OFF.
     WRITE:   /   TEXT-014       UNDER TEXT-005
            ,     BSEGTAB-DMBTR  UNDER TEXT-012.
     PERFORM SHOWVLINE_ACCOUNT.
     ULINE.
     FORMAT INTENSIFIED ON.
ENDFORM.

FORM WRITE_COMP_TOTAL.
     ULINE.
     FORMAT INTENSIFIED OFF.
     WRITE:   /   TEXT-015       UNDER TEXT-005
            ,     BSEGTAB-DMBTR  UNDER TEXT-012.
     PERFORM SHOWVLINE_ACCOUNT.
     ULINE.
     FORMAT INTENSIFIED ON.
ENDFORM.

FORM WRITE_REPORT_TOTAL.
     SKIP.
     SKIP.
     ULINE.
     FORMAT INTENSIFIED OFF.
     WRITE:   /   TEXT-016       UNDER TEXT-005
            ,     BSEGTAB-DMBTR  UNDER TEXT-012.
     PERFORM SHOWVLINE_ACCOUNT.
     ULINE.
     FORMAT INTENSIFIED OFF.
ENDFORM.

FORM ShowVline.
     WRITE:   001 SY-VLINE
            , 014 SY-VLINE
            , 034 SY-VLINE
            , 052 SY-VLINE
            , 068 SY-VLINE
            , 088 SY-VLINE
            , 093 SY-VLINE.
ENDFORM.

FORM SHOWVLINE_ASSET.
     WRITE:   001 SY-VLINE
            , 014 SY-VLINE
            , 068 SY-VLINE
            , 088 SY-VLINE
            , 093 SY-VLINE.
ENDFORM.

FORM SHOWVLINE_ACCOUNT.
     WRITE:   001 SY-VLINE
            , 068 SY-VLINE
            , 088 SY-VLINE
            , 093 SY-VLINE.
ENDFORM.

************************************************************************
*  This is the end my freind
************************************************************************
