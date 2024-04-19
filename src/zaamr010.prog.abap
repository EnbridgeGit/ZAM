REPORT ZAAMR010 NO STANDARD PAGE HEADING LINE-SIZE  94.
*
************************************************************************
*  AUTHOR:      MOHAMMAD KHAN.
*  DATE:        DECEMBER 2002.
*  Description:
*     - The purpose of this program is to produce exceptions list
*        from Projects(PS) Actuals and Postings to Assets(AM).
*  NOTE: Due to the shoratge of time and year end closing, the report
*        produced is not a fency one and it can be improved later.
*Changes:
*
*Issue: Date:   By:            Description:
*
************************************************************************
* DB-Table
TABLES:   ANEP,               "Asset line item
          AUAK,               "Document header for settlement
          COBRA,              "Settlement rules
          COBRB,              "Distribution rules settlement rule
          PRPS,               "WBS Element master data
          COSP,               "CO object: Primary cost
          COSS.               "CO object: Secondary cost

DATA: FISCAL_YEAR   LIKE COSP-GJAHR,
      DIFF$         LIKE ANEP-ANBTR,
      TOTAMT        LIKE COSP-WKG001,
      WORK_DOLLAR   LIKE COSP-WKG001,
      G_ATINN       LIKE CABN-ATINN,
      G_ATINN_MP    LIKE CABN-ATINN,
      OBJECT        LIKE AUSP-OBJEK,
      CHARIC        LIKE CABN-ATNAM,
      WS_POSKI      LIKE PRPS-POSKI,
      NEW_OBJNR     LIKE PRPS-OBJNR,
      NEW_POSKI     LIKE PRPS-POSKI,
      MAJOR_PROJ(6).

DATA:     BEGIN OF AMTAB OCCURS 0,                      " AM data
            ANLN1       LIKE ANEP-ANLN1,                " asset number
            BWASL       LIKE ANEP-BWASL,                " trans. code
            OBJNR       LIKE AUAK-OBJNR,                " object number
            ANLN2       LIKE ANEP-ANLN2,                " sub asset no.
            POSKI       LIKE PRPS-POSKI,                " wbs
            EXPFLG      TYPE C,                         " excption flag
            AM$         LIKE ANEP-ANBTR,                " $ AM
            PS$         LIKE COSP-WKG001,               " $ PS
          END OF AMTAB.

DATA:     BEGIN OF MINITAB OCCURS 0,
            FYEAR       LIKE COSP-GJAHR,
            DOLLAR      LIKE COSP-WKG001,
          END OF MINITAB.

DATA: BEGIN OF PRPSTAB OCCURS 0,                     "PRPS DATA
        OBJNR         LIKE PRPS-OBJNR,            "Object Number
        POSKI         LIKE PRPS-POSKI,            "WBS
        STUFE         LIKE PRPS-STUFE,            "WBS Level
      END OF PRPSTAB.

DATA: BEGIN OF CHAR_TAB OCCURS 0.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.


* Report Selections
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME. " TITLE TEXT-012.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(53) TEXT-015.
PARAMETERS:    P_FULL  RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(53) TEXT-016.
PARAMETERS:    P_EXCP  RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
   PARAMETERS:     P_BUKRS  LIKE ANLA-BUKRS DEFAULT 'UGL',
                   P_YEAR   LIKE COSP-GJAHR.
   SELECT-OPTIONS: S_ANLN1  FOR  ANEP-ANLN1.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
   SELECT-OPTIONS S_KSTAR     FOR COSP-KSTAR
                              DEFAULT '491001' TO '491002'.
SELECTION-SCREEN END OF BLOCK BOX3.
SELECTION-SCREEN END OF BLOCK BOX.

START-OF-SELECTION.
  SELECT * FROM ANEP
   WHERE BUKRS = P_BUKRS
     AND ANLN1 IN S_ANLN1
     AND GJAHR = P_YEAR
     AND BWASL IN ('115', '116', '331', '336').

     IF SY-SUBRC = 0.
        SELECT SINGLE * FROM AUAK
        WHERE BELNR = ANEP-BELNR.
        IF SY-SUBRC = 0.
           MOVE AUAK-OBJNR  TO  AMTAB-OBJNR.
           MOVE ANEP-ANLN1  TO  AMTAB-ANLN1.
           MOVE ANEP-ANLN2  TO  AMTAB-ANLN2.
           MOVE ANEP-ANBTR  TO  AMTAB-AM$.
           IF  ANEP-BWASL = '115'.
               MOVE '116' TO AMTAB-BWASL.
           ELSE.
               MOVE ANEP-BWASL  TO  AMTAB-BWASL.
           ENDIF.
           COLLECT AMTAB.
        ENDIF.
      ENDIF.
   ENDSELECT.

IF AMTAB[] IS INITIAL.
   WRITE: /1 'NO DATA SELECTED'.
   STOP.
ENDIF.
   SORT AMTAB BY ANLN1 BWASL OBJNR.
   PERFORM LOAD_PRPS_DATA.
   LOOP AT AMTAB.
        CLEAR AMTAB-PS$.
        IF AMTAB-BWASL = '331'.
           PERFORM PROCES_TRANSACTION_331.
        ELSE.
           PERFORM PROCES_TRANSACTION_OTHERS.
        ENDIF.
   ENDLOOP.

   IF NOT AMTAB[] IS INITIAL.
      PERFORM MAJOR_PROJECT_AND_AVD_CHECK.
      WRITE: /1 TEXT-CLT, SY-MANDT, SY-SYSID,
             62 SY-DATUM, TEXT-AMP, SY-UZEIT.
   ENDIF.

   IF P_EXCP = 'X'.
      DELETE AMTAB WHERE EXPFLG = ' '.
      WRITE: /30 'EXCEPTIONS LIST FOR ', P_YEAR.
      WRITE: /30 '**************************'.
   ELSE.
      WRITE: /25 'FULL LIST WITH EXCEPTIONS FOR ', P_YEAR.
      WRITE: /25 '************************************'.
   ENDIF.
   WRITE: /1 'Trn       WBS             Asset     Sub',
          52 'AM$', 66 'PS$            DIFF'.

   LOOP AT AMTAB.
           WRITE: /1(3) AMTAB-BWASL, 7(16) AMTAB-POSKI, AMTAB-ANLN1,
                  AMTAB-ANLN2, 44(14) AMTAB-AM$, 59(14) AMTAB-PS$.
        IF AMTAB-AM$ <> AMTAB-PS$.
           DIFF$ = AMTAB-AM$ - AMTAB-PS$.
           WRITE 74(12) DIFF$.
           WRITE 87 '**'.
           CLEAR DIFF$.
        ELSEIF AMTAB-EXPFLG = 'M'.
           WRITE 74 'NO AVD-MAJ.PROJ'.
        ENDIF.
        AT END OF BWASL.
           SUM.
           DIFF$ = AMTAB-AM$ - AMTAB-PS$.
           WRITE: /1 'TRNS. TOTAL:', 44(14) AMTAB-AM$, 59(14) AMTAB-PS$,
                   74(12) DIFF$.
           CLEAR DIFF$.
           SKIP 1.
        ENDAT.
        AT END OF ANLN1.
           SUM.
           DIFF$ = AMTAB-AM$ - AMTAB-PS$.
           WRITE: /1 'ASSET TOTAL:', 44(14) AMTAB-AM$, 59(14) AMTAB-PS$,
                                     74(12) DIFF$.
           WRITE /20 '------------------------------------------------'.
           SKIP 1.
        ENDAT.

        AT LAST.
           SUM.
           DIFF$ = AMTAB-AM$ - AMTAB-PS$.
           WRITE: /1 'GRAND TOTAL:', 44(14) AMTAB-AM$, 59(14) AMTAB-PS$,
                                     74(12) DIFF$.
        ENDAT.
   ENDLOOP.

*---------------------------------------------------------------------*
*       FORM PERFORM PROCES_TRANSACTION_331                           *
*---------------------------------------------------------------------*
 FORM PROCES_TRANSACTION_331.
 DATA: GOBACK_NMBR(2) TYPE N,
       DOLLAR331  LIKE COSP-WKG001.
    REFRESH MINITAB.
    FISCAL_YEAR = P_YEAR.
    GOBACK_NMBR = P_YEAR - 1997.
    DO GOBACK_NMBR TIMES.
       FISCAL_YEAR = FISCAL_YEAR - 1.
       CLEAR TOTAMT.
       PERFORM GET_PS_DOLLARS.
       ADD TOTAMT TO DOLLAR331.
       IF  DOLLAR331 = AMTAB-AM$.
           MOVE DOLLAR331 TO AMTAB-PS$.
           MODIFY AMTAB.
           REFRESH MINITAB.
           CLEAR:  MINITAB, AMTAB, DOLLAR331.
           EXIT.
       ELSE.
           MOVE FISCAL_YEAR TO MINITAB-FYEAR.
           MOVE TOTAMT TO MINITAB-DOLLAR.
           APPEND MINITAB.
           CLEAR  MINITAB.
       ENDIF.
    ENDDO.

    IF NOT MINITAB[] IS INITIAL.
    CLEAR DOLLAR331.
       LOOP AT MINITAB.
           MOVE MINITAB-DOLLAR TO WORK_DOLLAR.
           PERFORM APPLY_PERCENT CHANGING WORK_DOLLAR.
           ADD WORK_DOLLAR  TO  DOLLAR331.
           IF DOLLAR331 = AMTAB-AM$.
              MOVE DOLLAR331 TO AMTAB-PS$.
              MODIFY AMTAB.
              REFRESH MINITAB.
              CLEAR:  MINITAB, AMTAB, DOLLAR331, WORK_DOLLAR.
              EXIT.
           ENDIF.
       ENDLOOP.
    ENDIF.
    IF NOT MINITAB[] IS INITIAL.
       MOVE DOLLAR331 TO WORK_DOLLAR.                 "*$changes
       PERFORM BUILD_EXCEPTION_RECORD.
       REFRESH MINITAB.
       CLEAR:  MINITAB, DOLLAR331.
    ENDIF.
 ENDFORM.

*---------------------------------------------------------------------*
*       FORM PERFORM PROCES_TRANSACTION_OTHERS                        *
*---------------------------------------------------------------------*
 FORM PROCES_TRANSACTION_OTHERS.
    FISCAL_YEAR = P_YEAR.
    CLEAR TOTAMT.
    PERFORM GET_PS_DOLLARS.
    IF TOTAMT <> AMTAB-AM$.
       MOVE TOTAMT  TO  WORK_DOLLAR.
       PERFORM APPLY_PERCENT CHANGING WORK_DOLLAR.
       IF WORK_DOLLAR <> AMTAB-AM$.
          PERFORM BUILD_EXCEPTION_RECORD.
       ELSE.
          MOVE WORK_DOLLAR TO AMTAB-PS$.
          MODIFY AMTAB.
       ENDIF.
    ELSE.
       MOVE TOTAMT TO AMTAB-PS$.
       MODIFY AMTAB.
    ENDIF.
    CLEAR: TOTAMT, WORK_DOLLAR.
 ENDFORM.
*---------------------------------------------------------------------*
*       FORM GET_PS_DOLLARS                                           *
*---------------------------------------------------------------------*
 FORM GET_PS_DOLLARS.
 DATA: AMT  LIKE COSP-WKG001.
  SELECT * FROM COSP
     WHERE OBJNR = AMTAB-OBJNR                "Matching objects
       AND GJAHR = FISCAL_YEAR                "Fiscal Year selected
       AND VERSN = '000'                      "Version
       AND WRTTP = '04'                       "Actuals $
       AND BEKNZ IN ('S','H','L')             "Debit/Credit Indicator
       AND KSTAR NOT IN S_KSTAR.

   IF SY-SUBRC = '0'.
      ADD  COSP-WKG001 FROM 1 TO 12 GIVING AMT.
      TOTAMT = TOTAMT + AMT.
      CLEAR AMT.
   ENDIF.

  ENDSELECT.

  SELECT * FROM COSS
     WHERE OBJNR = AMTAB-OBJNR                "Matching objects
       AND GJAHR = FISCAL_YEAR                "Fiscal Year selected
       AND VERSN = '000'                      "Version
       AND WRTTP = '04'                       "Actuals $
       AND BEKNZ IN ('S','H','L')             "Debit/Credit Indicator
       AND KSTAR NOT IN S_KSTAR.

   IF SY-SUBRC = '0'.
      ADD  COSS-WKG001 FROM 1 TO 12 GIVING AMT.
      TOTAMT = TOTAMT + AMT.
      CLEAR AMT.
   ENDIF.

  ENDSELECT.

ENDFORM.
*---------------------------------------------------------------------*
*       PERFORM APPLY_PERCENT.                                        *
*---------------------------------------------------------------------*
 FORM APPLY_PERCENT CHANGING WORK-DOLLAR.
 DATA: TOTAL_PROZS LIKE COBRB-PROZS.

  SELECT * FROM COBRB
   WHERE   OBJNR = AMTAB-OBJNR.
   IF SY-SUBRC = 0.
      IF COBRB-ANLN1+0(5) = AMTAB-ANLN1+0(5).
         ADD COBRB-PROZS TO TOTAL_PROZS.
      ENDIF.
   ENDIF.
  ENDSELECT.
  IF TOTAL_PROZS > 0.
     WORK_DOLLAR = WORK_DOLLAR * TOTAL_PROZS / 100.     " %
  ENDIF.

  CLEAR: TOTAL_PROZS.

*  SELECT SINGLE PROZS INTO WS_PROZS FROM COBRB
*   WHERE   OBJNR = AMTAB-OBJNR
*     AND   ANLN1 = AMTAB-ANLN1
*     AND   ANLN2 = AMTAB-ANLN2.
*  IF SY-SUBRC = 0.
*     WORK_DOLLAR = WORK_DOLLAR * WS_PROZS / 100.     " %
*  ENDIF.

 ENDFORM.
*---------------------------------------------------------------------*
*       PERFORM BUILD_EXCEPTION_RECORD.                               *
*---------------------------------------------------------------------*
 FORM BUILD_EXCEPTION_RECORD.

      MOVE WORK_DOLLAR  TO  AMTAB-PS$.
      MOVE 'Y' TO AMTAB-EXPFLG.
      PERFORM GET_PROJECT_NUMBER.
      MOVE WS_POSKI  TO  AMTAB-POSKI.
      MODIFY AMTAB.
      CLEAR AMTAB.

 ENDFORM.
**---------------------------------------------------------------------*
*                 LOAD_PRPS_DATA                                       *
*----------------------------------------------------------------------*
FORM LOAD_PRPS_DATA.

     SELECT OBJNR POSKI STUFE
       INTO TABLE PRPSTAB
       FROM PRPS
      WHERE LOEVM <> 'X'
      ORDER BY OBJNR.
ENDFORM.
**---------------------------------------------------------------------*
*                 GET_PROJECT_NUMBER                                   *
*----------------------------------------------------------------------*
FORM GET_PROJECT_NUMBER.

    READ TABLE PRPSTAB WITH KEY OBJNR = AMTAB-OBJNR BINARY SEARCH.
    IF SY-SUBRC EQ 0.
       MOVE PRPSTAB-POSKI TO WS_POSKI.
    ELSE.
       CLEAR WS_POSKI.
    ENDIF.

ENDFORM.
**---------------------------------------------------------------------*
*                 MAJOR_PROJECT_AND_AVD_CHECK                          *
*----------------------------------------------------------------------*
FORM MAJOR_PROJECT_AND_AVD_CHECK.
DATA: NEW_POSKI LIKE PRPS-POSKI.
  MOVE 'MAJORPROJECT' TO  CHARIC.           "Characteristics required
  PERFORM GET_ATINN.
  MOVE G_ATINN        TO   G_ATINN_MP.
  PERFORM BUILD_AUSP.

LOOP AT AMTAB.
 READ TABLE PRPSTAB WITH KEY OBJNR = AMTAB-OBJNR BINARY SEARCH.
  IF SY-SUBRC EQ 0.
     CLEAR NEW_POSKI.
     MOVE PRPSTAB-POSKI+0(9) TO NEW_POSKI.
     MOVE PRPSTAB-POSKI      TO AMTAB-POSKI.
     READ TABLE PRPSTAB WITH KEY POSKI = NEW_POSKI
                                 STUFE = 1.
     IF SY-SUBRC = 0.
        MOVE PRPSTAB-OBJNR TO NEW_OBJNR.
        PERFORM FIND_CHARACTERISTIC.
        IF MAJOR_PROJ CS 'Y'.
           PERFORM CHECK_AVD.
           CLEAR MAJOR_PROJ.
        ENDIF.
     ENDIF.
  ENDIF.
  MODIFY AMTAB.
  CLEAR: AMTAB.
ENDLOOP.

ENDFORM.

*-----------------------  GET_ATINN  -----------------------------------
* Routine used to get the internal character number for project control
*-----------------------------------------------------------------------
FORM GET_ATINN.
  CALL FUNCTION 'CTUT_FEATURE_CHECK'
       EXPORTING
            CLASS_TYPE                  = '014'
            FEATURE_NEUTRAL_NAME        = CHARIC
       IMPORTING
            FEATURE_ID                  = G_ATINN
       EXCEPTIONS
            INVALID_CLASS_TYPE          = 1
            MISSING_FEATURE_INFORMATION = 2
            NO_FEATURE_FOUND            = 3
            NO_FEATURE_VALID            = 4
            NO_LANGUAGE                 = 5
            OTHERS                      = 6.
  IF SY-SUBRC NE 0.
    WRITE: / 'UNABLE TO DETERMINE THE CHARACTERISTICS OF', CHARIC.
    WRITE: /.
  ENDIF.
ENDFORM.
*---------------------  BUILD_AUSP -------------------------------------
FORM BUILD_AUSP.

  REFRESH CHAR_TAB.
  SELECT * FROM AUSP INTO TABLE CHAR_TAB
         WHERE  ATINN = G_ATINN_MP
           AND  MAFID = 'O'
           AND  KLART = '014'.
  SORT CHAR_TAB BY OBJEK ATINN.
ENDFORM.
*--------------------------  FIND_CHARACTERISTIC  ----------------------
* Routine to get the value of the CHARACTER FIELDS
*-----------------------------------------------------------------------
FORM FIND_CHARACTERISTIC.

  CLEAR: MAJOR_PROJ.
  MOVE   'n/a'   TO MAJOR_PROJ.
  READ TABLE CHAR_TAB WITH KEY OBJEK = NEW_OBJNR
                               ATINN = G_ATINN_MP  BINARY SEARCH.
  IF SY-SUBRC EQ 0.
    MOVE CHAR_TAB-ATWRT+0(6) TO MAJOR_PROJ.
  ENDIF.

ENDFORM.
*-----------------------------------------------------------------------
*--------------------------CHECK_AVD------------------------------------
*-----------------------------------------------------------------------
FORM CHECK_AVD.
DATA: WS_BZDAT LIKE COBRA-BZDAT.
 SELECT SINGLE BZDAT INTO WS_BZDAT FROM COBRA
  WHERE OBJNR = AMTAB-OBJNR.
  IF SY-SUBRC = 0.
     IF WS_BZDAT = '00000000'.
        MOVE 'M' TO AMTAB-EXPFLG.
     ENDIF.
  ENDIF.
  CLEAR: WS_BZDAT.
ENDFORM.
