REPORT ZAFAR005 LINE-SIZE 170 LINE-COUNT 58 NO STANDARD PAGE HEADING
       MESSAGE-ID ZA.

************************************************************************
*  Author:    Mohammad Khan
*  Date:      December, 2001.
*  Issue log: 863
*  Brief Description:
*     - The purpose of this report is to list an acquisition values for
*     - selected assets and subassets with the exlusion of group assets.
*
*
* Issue:  Date:       By:     Remarks:
* 1019  2003/07/23  mokhan    Add "deactivate date" field to the report
*                             option "Spread Sheet". Also, don't display
*                             0 balance rows for this option.
*
*TR582  2009/11/18  mokhan    IFRS: Change the Depreciation Area select-
*                             ion filed to a range field and make other
*                             changes accordingly.
*
************************************************************************
TABLES:   ANLC, ANLA, ANLZ.

DATA:     TOTAL        LIKE ANLC-KANSW,
          GROUPTOT     LIKE TOTAL,
          CLASSTOT     LIKE TOTAL,
          TOTAL1       LIKE TOTAL,
          TOTAL2       LIKE TOTAL,
          GROUPGRD     LIKE TOTAL,
          CLASSGRD     LIKE TOTAL,
          ACQVAL       LIKE TOTAL,
          BALANCE      LIKE TOTAL,
          KINGVAL      LIKE TOTAL,
          CLASS        LIKE TOTAL,
          DIVTOT       LIKE TOTAL,
          SUBTOT       LIKE TOTAL,
          COUNTER      TYPE I,
          ASSETCLS     LIKE ANLA-ANLKL,    "Asset Class
          ANLN1        LIKE ANLZ-ANLN1,    "Asset Number.
          COUNT        TYPE I,
          CURRENT      LIKE ANLA-ANLKL,
          PREVIOUS     LIKE ANLA-ANLKL,
          FIRST_TIME(1) VALUE 'Y'.

DATA: BEGIN OF ZTABLE     OCCURS 0,
                ASSETCLS    LIKE ANLA-ANLKL,    "Asset Class
                KOSTL       LIKE ANLZ-KOSTL,
                ANLN1       LIKE ANLA-ANLN1,    "Asset Class
                ANLN2       LIKE ANLA-ANLN2,    "Grp Asset Indicator
                FLAG        LIKE ANLA-XANLGR,
                GDLGRP      LIKE ANLA-GDLGRP,   "Station Id
                TEXT        LIKE ANLA-TXT50,
                FIRSTVAL    LIKE TOTAL,
                NEXTVAL     LIKE ANLC-ANSWL,
                LASTVAL     LIKE TOTAL,
                TXT50       LIKE ANLA-TXT50,
                TXA50       LIKE ANLA-TXA50,
                MEINS       LIKE ANLA-MEINS,
                MENGE       LIKE ANLA-MENGE,    "Quantity
                AKTIV       LIKE ANLA-AKTIV,
                DEAKT       LIKE ANLA-DEAKT,
                TOTAL       LIKE ANLC-KANSW,
                ANSWL       LIKE ANLC-ANSWL,
                ACQVAL      LIKE TOTAL,
                WERKS       LIKE ANLZ-WERKS.
DATA: END OF ZTABLE.

DATA: BEGIN OF ZTEMP   OCCURS 0.
        INCLUDE STRUCTURE ZTABLE.
DATA: END OF ZTEMP.
************************************************************************
*   Begin of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-023.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(2) TEXT-050.
SELECTION-SCREEN POSITION 4.
PARAMETERS:    CHECK1  RADIOBUTTON GROUP RGRP.
SELECTION-SCREEN COMMENT 6(23) TEXT-045.
SELECTION-SCREEN POSITION 29.
PARAMETERS:    CHECK2  RADIOBUTTON GROUP RGRP DEFAULT 'X'.
SELECTION-SCREEN COMMENT 31(21) TEXT-040.
SELECTION-SCREEN POSITION 53.
PARAMETERS:    CHECK3  RADIOBUTTON GROUP RGRP.
SELECTION-SCREEN COMMENT 55(21) TEXT-041.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(2) TEXT-051.
SELECTION-SCREEN POSITION 4.
PARAMETERS:    CHECK4  RADIOBUTTON GROUP ZGRP.
SELECTION-SCREEN COMMENT 6(23) TEXT-042.
SELECTION-SCREEN POSITION 29.
PARAMETERS:    CHECK5  RADIOBUTTON GROUP ZGRP DEFAULT 'X'.
SELECTION-SCREEN COMMENT 31(21) TEXT-043.
SELECTION-SCREEN POSITION 53.
PARAMETERS:    CHECK6  RADIOBUTTON GROUP ZGRP.
SELECTION-SCREEN COMMENT 55(17) TEXT-044.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
SELECTION-SCREEN SKIP 1.
PARAMETERS:     P_CCODE     LIKE   ANLA-BUKRS   DEFAULT 'UGL'.
SELECT-OPTIONS: S_ASSTNR    FOR ANLA-ANLN1,
                S_SUBNR     FOR ANLA-ANLN2.
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS: S_ACLASS   FOR ANLA-ANLKL,
                S_COSTCT   FOR ANLZ-KOSTL,
                S_PLANT    FOR ANLZ-WERKS,
                S_LOC      FOR ANLZ-STORT,
                S_AGROUP   FOR ANLA-ANLUE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-003.
PARAMETERS:     P_GJAHR     LIKE ANLC-GJAHR.
SELECT-OPTIONS: S_DEPARE    FOR  ANLC-AFABE.                     "TR582
*                P_DEPARE    LIKE RBADA-AFABE1  DEFAULT '01'.    "TR582

SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-004.
SELECT-OPTIONS: S_BALNCE   FOR ANLA-KTOGR,
                S_CAPDTE   FOR ANLA-AKTIV,
                S_ACQVAL   FOR ANLC-KANSW.
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN END OF BLOCK BOX.

AT SELECTION-SCREEN.
  IF ( CHECK1 = 'X' AND CHECK2 = ' ' AND CHECK3 = ' ' ) OR
     ( CHECK1 = ' ' AND CHECK2 = 'X' AND CHECK3 = ' ' ) OR
     ( CHECK1 = ' ' AND CHECK2 = ' ' AND CHECK3 = 'X' ).
* only 1 report can be produced at a time
  ELSE.
    MESSAGE E001.
  ENDIF.
************************************************************************

INITIALIZATION.
  P_GJAHR = SY-DATUM(4).

************************************************************************
TOP-OF-PAGE.
 FORMAT INTENSIFIED OFF.
 IF CHECK1 = 'X'.
  IF FIRST_TIME = 'Y'.
    WRITE: /1 TEXT-RPT, SY-REPID, 95 TEXT-001, 190 TEXT-DTE, SY-DATUM,
              TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
             TEXT-012 UNDER TEXT-001, P_GJAHR,
             TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
    WRITE: / TEXT-CPY UNDER TEXT-RPT, P_CCODE.
    PERFORM PRINTHEAD.
    MOVE 'N' TO FIRST_TIME.
*    WRITE: /.
  ENDIF.
 ELSE.
  WRITE: /1 TEXT-RPT, SY-REPID, 70 TEXT-001, 140 TEXT-DTE, SY-DATUM,
            TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
           TEXT-012 UNDER TEXT-001, P_GJAHR,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
  WRITE: / TEXT-CPY UNDER TEXT-RPT, P_CCODE.
   IF CHECK2 = 'X'.
     WRITE: /119 TEXT-054, 135 TEXT-027.
     WRITE: /1  TEXT-030, 130 TEXT-029, TEXT-028 UNDER TEXT-027,
           142 TEXT-025, 152 TEXT-026, 155 TEXT-024, 88 TEXT-055,
            98 TEXT-031, 116 TEXT-052, 123 TEXT-053.
*     WRITE: /.
   ELSE.
     WRITE: TEXT-037 UNDER TEXT-001.
     WRITE: /50 TEXT-032, 70 TEXT-033, 90 TEXT-034, 110 TEXT-035,
            130 TEXT-036.
*     WRITE: /.
   ENDIF.
 ENDIF.

*  IF CHECK3 = 'X'.
*    WRITE: TEXT-037 UNDER TEXT-001.
*  ENDIF.

*  WRITE: /.

*  IF CHECK1 = 'X'.
*    PERFORM PRINTHEAD.
*  ENDIF.

*  IF CHECK2 = 'X'.
*    WRITE: /119 TEXT-054, 135 TEXT-027.
*    WRITE: /1  TEXT-030, 130 TEXT-029, TEXT-028 UNDER TEXT-027,
*           142 TEXT-025, 152 TEXT-026, 155 TEXT-024, 88 TEXT-055,
*           98 TEXT-031, 116 TEXT-052, 123 TEXT-053.
*    WRITE /.
*  ENDIF.
*
*  IF CHECK3 = 'X'.
*    WRITE: TEXT-037 UNDER TEXT-001.
*    WRITE: /50 TEXT-032, 70 TEXT-033, 90 TEXT-034, 110 TEXT-035,
*           130 TEXT-036.
*    WRITE /.
*  ENDIF.
************************************************************************
START-OF-SELECTION.
  SELECT * FROM ANLZ
     WHERE BUKRS = P_CCODE
     AND ANLN1 IN S_ASSTNR
     AND ANLN2 IN S_SUBNR
     AND KOSTL IN S_COSTCT
     AND WERKS IN S_PLANT
     AND STORT IN S_LOC.

    SELECT * FROM ANLA
       WHERE BUKRS = ANLZ-BUKRS
       AND ANLN1 = ANLZ-ANLN1
       AND ANLN2 = ANLZ-ANLN2
       AND ANLKL IN S_ACLASS
       AND ANLUE IN S_AGROUP
       AND KTOGR IN S_BALNCE
       AND AKTIV IN S_CAPDTE
      ORDER BY ANLKL.                                     "4.6 B changes
*     ORDER BY ANLA-ANLKL.

      CLEAR: TOTAL, ACQVAL.
      MOVE:   ANLA-ANLN1   TO ZTABLE-ANLN1,
              ANLA-ANLN2   TO ZTABLE-ANLN2,
              ANLA-ANLKL   TO ZTABLE-ASSETCLS,
              ANLA-GDLGRP  TO ZTABLE-GDLGRP,
              ANLA-XANLGR  TO ZTABLE-FLAG,
              ANLA-TXT50   TO ZTABLE-TEXT,
              0            TO ZTABLE-NEXTVAL,
              TOTAL        TO ZTABLE-FIRSTVAL,
              TOTAL        TO ZTABLE-TOTAL,
              ACQVAL       TO ZTABLE-LASTVAL,
              ANLA-TXT50   TO ZTABLE-TXT50,
              ANLA-TXA50   TO ZTABLE-TXA50,
              ANLZ-KOSTL   TO ZTABLE-KOSTL,
              ANLZ-WERKS   TO ZTABLE-WERKS,
              ANLA-MENGE   TO ZTABLE-MENGE,
              ANLA-AKTIV   TO ZTABLE-AKTIV,
              ANLA-DEAKT   TO ZTABLE-DEAKT,
              0            TO ZTABLE-ANSWL,
              ANLA-MEINS   TO ZTABLE-MEINS,
              ACQVAL       TO ZTABLE-ACQVAL.
      PERFORM GET_ANLC_DATA.

      APPEND ZTABLE.
      CLEAR ZTABLE.
    ENDSELECT.            "End of ANLA
  ENDSELECT.              "End of ANLZ
*--------------------  PRINT REQUIRED REPORTS --------------------------
SORT ZTABLE BY ASSETCLS KOSTL FLAG DESCENDING ANLN1 ANLN2.

*Remove unwanted rows as per selection. Start of changes Issue Log 1019
IF CHECK5 = 'X'.
   LOOP AT ZTABLE.
        IF ZTABLE-TOTAL  = 0 AND ZTABLE-ANSWL = 0 AND
           ZTABLE-ACQVAL = 0 AND ZTABLE-MENGE = 0.
           DELETE ZTABLE.
        ENDIF.
   ENDLOOP.
ENDIF.

IF CHECK6 = 'X'.
   LOOP AT ZTABLE.
        IF ZTABLE-TOTAL  = 0 AND ZTABLE-ANSWL = 0 AND
           ZTABLE-ACQVAL = 0 AND ZTABLE-MENGE = 0.
        ELSE.
           DELETE ZTABLE.
        ENDIF.
   ENDLOOP.
ENDIF.
*                                          End of changes Issue Log 1019

IF CHECK1 = 'X'.
LOOP AT ZTABLE.
  AT FIRST.
     NEW-PAGE LINE-SIZE 230 LINE-COUNT 0.
  ENDAT.
WRITE: /2 ZTABLE-ANLN1, ZTABLE-ANLN2, ZTABLE-AKTIV, ZTABLE-DEAKT, "I1019
          ZTABLE-TXT50, ZTABLE-TXA50, ZTABLE-GDLGRP, ZTABLE-WERKS,
      158 ZTABLE-KOSTL, 166 ZTABLE-TOTAL,
      185(13) ZTABLE-ANSWL, 200 ZTABLE-ACQVAL,
      220(11) ZTABLE-MENGE DECIMALS 0.
ENDLOOP.
ULINE.
ENDIF.

  IF CHECK2 = 'X'.
    LOOP AT ZTABLE.

      AT NEW ASSETCLS.                                   "Asset Class
        WRITE: /1 TEXT-015 COLOR COL_HEADING INTENSIFIED ON,
                ZTABLE-ASSETCLS.
        FORMAT COLOR OFF.
      ENDAT.

      AT NEW FLAG.                                       "Group Asset
        IF ZTABLE-FLAG = 'X'.
          WRITE: /1 TEXT-016 COLOR COL_HEADING INTENSIFIED OFF.
        ENDIF.
      ENDAT.

*      IF  CHECK4 = 'X' OR ( CHECK5 = 'X' AND ZTABLE-ACQVAL <> 0 )
*                       OR ( CHECK6 = 'X' AND ZTABLE-ACQVAL = 0 ).
        FORMAT RESET.
        WRITE: / ZTABLE-ANLN1 UNDER TEXT-031, ZTABLE-ANLN2,
                 ZTABLE-GDLGRP UNDER TEXT-055,
                 ZTABLE-AKTIV+0(6) UNDER TEXT-052,
                 ZTABLE-DEAKT+0(6) UNDER TEXT-053,
            (48) ZTABLE-TXT50 UNDER TEXT-030, (37) ZTABLE-TXA50,
               ZTABLE-WERKS UNDER TEXT-029, ZTABLE-KOSTL UNDER TEXT-028,
            (11) ZTABLE-MENGE DECIMALS 0 UNDER TEXT-025,
                 ZTABLE-MEINS UNDER TEXT-026,
                 ZTABLE-ACQVAL UNDER TEXT-024.
*      ENDIF.

      IF ZTABLE-FLAG = 'X'.
        COMPUTE GROUPTOT = GROUPTOT + ZTABLE-ACQVAL.
        COMPUTE GROUPGRD = GROUPGRD + ZTABLE-ACQVAL.
      ELSE.
        COMPUTE CLASSTOT = CLASSTOT + ZTABLE-ACQVAL.
        COMPUTE CLASSGRD = CLASSGRD + ZTABLE-ACQVAL.
      ENDIF.

      AT END OF ANLN1.
        SUM.
*        IF  CHECK4 = 'X' OR ( CHECK5 = 'X' AND ZTABLE-ACQVAL <> 0 )
*                         OR ( CHECK6 = 'X' AND ZTABLE-ACQVAL = 0 ).
          FORMAT COLOR COL_TOTAL.
          WRITE: /61 TEXT-017,
                (11) ZTABLE-MENGE DECIMALS 0 UNDER TEXT-025,
                 ZTABLE-ACQVAL UNDER TEXT-024.              "MDEMEEST
*        ENDIF.
      ENDAT.

      AT END OF ANLN1+5(2).
        SUM.
        FORMAT COLOR COL_TOTAL.
        WRITE: /61 TEXT-018,
            (11) ZTABLE-MENGE DECIMALS 0 UNDER TEXT-025,
                ZTABLE-ACQVAL UNDER TEXT-024.               "MDEMEEST
        FORMAT  COLOR COL_NORMAL.
        WRITE: /69 SY-ULINE.
      ENDAT.

      AT END OF KOSTL.
        SUM.
        FORMAT COLOR COL_TOTAL.
        WRITE: /61 TEXT-019,
             (11) ZTABLE-MENGE DECIMALS 0 UNDER TEXT-025,
                  CLASSTOT UNDER TEXT-024.
        COMPUTE CLASSTOT = CLASSTOT - GROUPTOT.
        WRITE: /61 TEXT-020, CLASSTOT UNDER TEXT-024.
        FORMAT COLOR OFF.
        WRITE: /69 SY-ULINE.
        WRITE: /.
        CLEAR: CLASSTOT, GROUPTOT.
      ENDAT.

      AT LAST.
        WRITE: /.
        WRITE: /.
        WRITE: /1 TEXT-021, GROUPGRD.
        WRITE: /1 TEXT-022, CLASSGRD.
      ENDAT.

    ENDLOOP.
  ENDIF.
*---------------  REPORT 3 - TOTALS BY ASSET CLASS ---------------------
  IF CHECK3 = 'X'.
    LOOP AT ZTABLE.

      AT NEW ASSETCLS.                                   "Asset Class
        MOVE ZTABLE-ASSETCLS TO ASSETCLS.
      ENDAT.

      IF ZTABLE-FLAG = 'X'.
        MOVE ZTABLE-ANLN1 TO ANLN1.
        COMPUTE GROUPTOT = GROUPTOT + ZTABLE-ACQVAL.
        COMPUTE GROUPGRD = GROUPGRD + ZTABLE-ACQVAL.
        COMPUTE TOTAL1   = TOTAL1   + ZTABLE-ACQVAL.
      ELSE.
        COMPUTE CLASSTOT = CLASSTOT + ZTABLE-ACQVAL.
        COMPUTE CLASSGRD = CLASSGRD + ZTABLE-ACQVAL.
        COMPUTE TOTAL2   = TOTAL2   + ZTABLE-ACQVAL.
      ENDIF.

      AT END OF KOSTL.
        SUM.
        WRITE: / ASSETCLS UNDER TEXT-032,
                 ANLN1    UNDER TEXT-033,
                 CLASSTOT UNDER TEXT-034,
                 GROUPTOT UNDER TEXT-035.
        COMPUTE CLASSTOT = CLASSTOT - GROUPTOT.
        WRITE:   CLASSTOT UNDER TEXT-036.

        CLEAR: CLASSTOT, GROUPTOT, ANLN1.
      ENDAT.

      AT END OF ASSETCLS.
        SUM.
        WRITE: /90 SY-ULINE(56).
        WRITE: / ASSETCLS UNDER TEXT-032,
                 TOTAL2   UNDER TEXT-034,
                 TOTAL1   UNDER TEXT-035.
        COMPUTE TOTAL2 = TOTAL2 - TOTAL1.
        WRITE:  TOTAL2 UNDER TEXT-036.
        WRITE: /50 SY-ULINE(96).
        CLEAR: TOTAL1, TOTAL2.

      ENDAT.

      AT LAST.
        WRITE: /.
        WRITE: /.
        WRITE: /1 TEXT-021, GROUPGRD.
        WRITE: /1 TEXT-022, CLASSGRD.
      ENDAT.

    ENDLOOP.
  ENDIF.


************************************************************************
*   Subroutines used by a program:
************************************************************************
FORM GET_ANLC_DATA.
  SELECT * FROM ANLC
     WHERE BUKRS = ANLA-BUKRS
     AND ANLN1 = ANLA-ANLN1
     AND ANLN2 = ANLA-ANLN2
     AND GJAHR = P_GJAHR
     AND AFABE IN S_DEPARE.                                     "TR581
*     AND AFABE = P_DEPARE.                                     "TR581
*-----------------------------------------------------------------------
    CLEAR: TOTAL, ACQVAL.
    TOTAL = ANLC-KANSW + ANLC-KINVZ + ANLC-INVZM + ANLC-KAUFW +
            ANLC-AUFWB.
    ACQVAL = TOTAL + ANLC-ANSWL.
    CHECK ACQVAL IN S_ACQVAL.
*-----------------------------------------------------------------------
    MOVE:   TOTAL        TO ZTABLE-FIRSTVAL,
            TOTAL        TO ZTABLE-TOTAL,
            ANLC-ANSWL   TO ZTABLE-NEXTVAL,
            ACQVAL       TO ZTABLE-LASTVAL,
            ANLC-ANSWL   TO ZTABLE-ANSWL,
            ACQVAL       TO ZTABLE-ACQVAL.
  ENDSELECT.           "End of ANLC
ENDFORM.
************************************************************************
*   Subroutines used by a program:
************************************************************************

FORM PRINTHEAD.
  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE: / SY-ULINE.
*  WRITE: /001 SY-VLINE, 019 SY-VLINE, 030 SY-VLINE, 81 SY-VLINE,
*          132 SY-VLINE, 141 SY-VLINE, 146 SY-VLINE, 152 SY-VLINE,
*          170 SY-VLINE, 186 SY-VLINE,
*          204 SY-VLINE, 220 SY-VLINE.
  WRITE: /2(12) TEXT-005, 15(04) TEXT-006, 20(10) TEXT-007,
         31(10) TEXT-061, 42(15) TEXT-008, 93(15) TEXT-046,  "Issue 1019
         144 TEXT-055, 153 TEXT-029, 158 TEXT-060, 166(11) TEXT-009,
         184(13) TEXT-010, 198(15) TEXT-011, 221 TEXT-025.
  WRITE: / SY-ULINE.
ENDFORM.
