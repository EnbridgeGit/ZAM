REPORT ZAFAR004 NO STANDARD PAGE HEADING LINE-SIZE 170
                LINE-COUNT 58 MESSAGE-ID PP.

************************************************************************
*
*   PROGRAM:    zafar004.
*   PROGRAMMER: M. Khan.
*   CLIENT:     Union Gas
*   DATE:       MAY 2000.
*
*   The purpose of this program is to produce Special Group of Assets
*   Report.
*
************************************************************************
* CHANGES
* 20/04/10 TR314 M. Khan Change to handle the double depreciation area
*                        through variant screen.
*
* 21/04/09 TR314 M. Khan Change to select assets starting with 4 and 5
*                        instead of starting with 4 only.
*
************************************************************************

TABLES: ANLA,               " Asset master record-segment
        ANLC,               " Asset-value fields
        ANLZ,               " Time-dependent asset allocations
        ANEP,               " Asset line item
        ANKT,               " Asset Class Description
        T001W,              " Plants/Branches
        T001.               " Company code

DATA: BEGIN OF TABLE OCCURS 0,
        ANLKL         LIKE ANLA-ANLKL,              "Asset class
        WERKS         LIKE ANLZ-WERKS,              "Plant code
        ANLN1         LIKE ANEP-ANLN1,              "Main asset Number
        ANLN2         LIKE ANEP-ANLN2,              "Sub Asset Number
        DESCRP1(40),                                "anla-txt50
        DESCRP2(28),                                "anla-txa50
        KFZKZ         LIKE ANLZ-KFZKZ,              "Land file #
        ADDAMT(7)     TYPE P, " decimals 2,            "Additions
        TRNAMT(7)     TYPE P, " decimals 2,            "Transfers
        RETAMT(7)     TYPE P, " decimals 2,            "Retirements
        OPBAL(7)      TYPE P, " decimals 2,            "Opening balance
        CLOBAL1(7)    TYPE P, " decimals 2,     "Closing balance-system
        CLOBAL2(7)    TYPE P, " decimals 2,  "Closing balance-calculated
     END OF TABLE.

DATA: BEGIN OF EXCLUDE_TABLE OCCURS 0,
            ASSETNO(16),
      END OF EXCLUDE_TABLE.

DATA BIG_TABLE LIKE ANLC OCCURS 0 WITH HEADER LINE.

DATA: ADD_TOTAL(7)      TYPE P DECIMALS 2,
      REP_TOTAL(7)      TYPE P DECIMALS 2,
      TRN_TOTAL(7)      TYPE P DECIMALS 2,
      PREV_ANLKL        LIKE ANLA-ANLKL VALUE SPACE,
      FULL_ASSETNO(16),
      HEAD_PRINT(1)     VALUE 'Y',
      EXCLUDE_FLAG(1)   VALUE 'N',
      CURRENT_INDEX     TYPE I,
      MAINASSET_LINES   TYPE I,
      SUBASSET_LINES    TYPE I,
      ERL_COUNT         TYPE I.

*-----------------------------------------------------------------------
*    Start of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
PARAMETERS:
   P_CHKZRO AS CHECKBOX,                               "Print zero value
   P_REPTTL(40)  DEFAULT 'Special Group Of Assets Report',  "Heading
   P_FYEAR LIKE ANEP-GJAHR DEFAULT SY-DATUM+0(4),        "Fiscal Year
   P_CCODE LIKE ANEP-BUKRS DEFAULT 'UGL'.                "Company code

SELECT-OPTIONS:
        S_ANLKL  FOR ANLA-ANLKL,                         "Asset Class
        S_ANLN1  FOR ANEP-ANLN1,                         "Main Asset #
        S_ANLN2  FOR ANEP-ANLN2,                         "Sub  Asset #
        S_WERKS  FOR ANLZ-WERKS,                         "Plant code
        S_GDLGRP FOR ANLA-GDLGRP,                        "Station ID
        S_KFZKZ  FOR ANLZ-KFZKZ.                         "Land file #
PARAMETERS: P_AFABE LIKE RBADA-AFABE1 DEFAULT '01'.  "Dep. Area "TR314

SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
SELECT-OPTIONS:
        S_ADDTRN FOR ANEP-BWASL,                       "Add transactions
        S_REPTRN FOR ANEP-BWASL,                       "replace trans.
        S_TRFTRN FOR ANEP-BWASL.                       "Transfer trans.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-003.
SELECT-OPTIONS: S_EXCLD3 FOR ANEP-ANLN1+5(2) DEFAULT '98' TO '99',
                S_EXCLD1 FOR ANEP-ANLN1 NO INTERVALS,
                S_EXCLD2 FOR ANEP-ANLN2 NO INTERVALS.
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME TITLE TEXT-004.
PARAMETERS:     P_RPRT RADIOBUTTON GROUP RBCR,            "PRINT REPORT
                P_FILE RADIOBUTTON GROUP RBCR.            "EXCEL FILE
SELECTION-SCREEN END OF BLOCK BOX4.
*-----------------------  END of SELECTION SCREEN-----------------------

*-----------------------  INITIALIZATION     ---------------------------

INITIALIZATION.
 S_ADDTRN-SIGN   = 'I'.                     " Addition Transactions
 S_ADDTRN-OPTION = 'EQ'.
 S_ADDTRN-LOW    = '100'.
 APPEND S_ADDTRN.
 S_ADDTRN-LOW    = '101'.
 APPEND S_ADDTRN.
 S_ADDTRN-LOW    = '115'.
 APPEND S_ADDTRN.
 S_ADDTRN-LOW    = '331'.
 APPEND S_ADDTRN.
 S_ADDTRN-LOW    = '336'.
 APPEND S_ADDTRN.

 S_REPTRN-SIGN   = 'I'.                    "Replacement transactions
 S_REPTRN-OPTION = 'EQ'.
 S_REPTRN-LOW    = '200'.
 APPEND S_REPTRN.
 S_REPTRN-LOW    = '205'.
 APPEND S_REPTRN.
 S_REPTRN-LOW    = '250'.
 APPEND S_REPTRN.
 S_REPTRN-LOW    = '255'.
 APPEND S_REPTRN.

 S_TRFTRN-SIGN   = 'I'.                      "Transfer Transactions
 S_TRFTRN-OPTION = 'EQ'.
 S_TRFTRN-LOW    = '300'.
 APPEND S_TRFTRN.
 S_TRFTRN-LOW    = '310'.
 APPEND S_TRFTRN.
 S_TRFTRN-LOW    = '320'.
 APPEND S_TRFTRN.
 S_TRFTRN-LOW    = '330'.
 APPEND S_TRFTRN.

*-----------------------  END of INITIALIZATION-------------------------

************************************************************************
*-------------------------  ON VAOLUE REQUEST  -------------------------

AT SELECTION-SCREEN ON VALUE-REQUEST FOR P_AFABE.       "TR314
   PERFORM BEREICH_VALUE USING 'P_AFABE' P_AFABE.       "TR314

*-------------------------  START-OF-SELECTION -------------------------
START-OF-SELECTION.

DESCRIBE TABLE S_EXCLD1 LINES MAINASSET_LINES.
DESCRIBE TABLE S_EXCLD2 LINES SUBASSET_LINES.
IF SUBASSET_LINES <> MAINASSET_LINES.
   FORMAT COLOR COL_NEGATIVE INVERSE ON.
   WRITE: /50 '*************** PROGRAM TERMINATED ****************'.
   WRITE: /50 'Excludes for Main asset and Sub Asset must be equal'.
   PERFORM PRINT_VARIANT.
   STOP.
ENDIF.

SELECT SINGLE * FROM T001                "Company Code
  WHERE BUKRS = P_CCODE.

IF P_RPRT EQ 'X'.
   PERFORM PRINT_VARIANT.
ENDIF.

SELECT * FROM ANLC INTO TABLE BIG_TABLE
    WHERE BUKRS = P_CCODE    AND
          ANLN1 IN S_ANLN1   AND
          ANLN2 IN S_ANLN2   AND
          GJAHR = P_FYEAR    AND
          AFABE = P_AFABE.                                     "TR314

    IF SY-SUBRC = 0.
       LOOP AT BIG_TABLE.
*            IF BIG_TABLE-ANLN1+0(1) <> '4'.   "TR314
             IF NOT BIG_TABLE-ANLN1+0(1) BETWEEN '4' AND '5'.  "TR314
               DELETE BIG_TABLE.
               CONTINUE.
            ENDIF.
            IF S_EXCLD3-LOW  IS INITIAL  AND
               S_EXCLD3-HIGH IS INITIAL.
               CONTINUE.
            ELSEIF BIG_TABLE-ANLN1+5(2) IN S_EXCLD3.
               DELETE BIG_TABLE.
            ENDIF.
       ENDLOOP.

       LOOP AT BIG_TABLE.
            PERFORM GET_ANLA.
            IF SY-SUBRC = 0.
               PERFORM GET_ANLZ.
               IF SY-SUBRC = 0.
                  PERFORM GET_ANEP.             "Transaction data
                  PERFORM BUILD_TABLE.
               ENDIF.
            ENDIF.
       ENDLOOP.
    ENDIF.

 PERFORM BUILD_EXCLUDE_TABLE.
 SORT EXCLUDE_TABLE BY ASSETNO.

 LOOP AT TABLE.                                    "Exclude Transactions
      IF MAINASSET_LINES > 0.
         PERFORM EXCLDE_CHECK.
         IF EXCLUDE_FLAG = 'Y'.
            DELETE TABLE.
            CONTINUE.
         ENDIF.
      ENDIF.
      IF P_CHKZRO <> 'X'.
         IF TABLE-OPBAL    <> 0      OR
            TABLE-ADDAMT   <> 0      OR
            TABLE-TRNAMT   <> 0      OR
            TABLE-RETAMT   <> 0      OR
            TABLE-CLOBAL1  <> 0      OR
            TABLE-CLOBAL2  <> 0.
            CONTINUE.
         ELSE.
            DELETE TABLE.
         ENDIF.
      ENDIF.
 ENDLOOP.

    SORT TABLE BY ANLKL WERKS ANLN1 ANLN2.

 LOOP AT TABLE.
   AT NEW ANLKL.
      NEW-PAGE.
   ENDAT.
      WRITE: /1 TABLE-ANLN1,
             14 TABLE-ANLN2,
             19 TABLE-DESCRP1,
             60 TABLE-DESCRP2,
             90 TABLE-WERKS.
         PERFORM WRITE_AMOUNT.
         IF TABLE-CLOBAL1 <> TABLE-CLOBAL2.
            WRITE: '**'.
            ERL_COUNT = ERL_COUNT + 1.
         ENDIF.

   AT END OF WERKS.
     SUM.
     PERFORM GET_T001W.                              "Plant Description
     WRITE: /.
     WRITE: /95 SY-ULINE.
     WRITE: /2 TEXT-025, TABLE-WERKS, '-', T001W-NAME1.
     PERFORM WRITE_AMOUNT.
     WRITE: /95 SY-ULINE.
   ENDAT.

   AT END OF ANLKL.
     SUM.
     WRITE: /2 TEXT-026.
     PERFORM WRITE_AMOUNT.
     WRITE: /95 SY-ULINE.
   ENDAT.

   AT LAST.
     SUM.
     WRITE: /2 TEXT-027.
     PERFORM WRITE_AMOUNT.
     WRITE: /95 SY-ULINE.
      WRITE: / TEXT-024 UNDER TEXT-011,  ERL_COUNT.
   ENDAT.
 ENDLOOP.

************************************************************************
*                       SUBROUTINES
************************************************************************

*---------------------  BUILD_TABLE ------------------------------------
FORM BUILD_TABLE.
  TABLE-OPBAL   = BIG_TABLE-KANSW + BIG_TABLE-KINVZ + BIG_TABLE-INVZM +
                  BIG_TABLE-KAUFW + BIG_TABLE-AUFWB.
  TABLE-CLOBAL1 = TABLE-OPBAL + ADD_TOTAL + TRN_TOTAL + REP_TOTAL.
  TABLE-CLOBAL2 = TABLE-OPBAL + BIG_TABLE-ANSWL.

        TABLE-ANLKL   = ANLA-ANLKL.
        TABLE-ANLN1   = ANLA-ANLN1.
        TABLE-ANLN2   = ANLA-ANLN2.
        TABLE-WERKS   = ANLZ-WERKS.
        TABLE-DESCRP1 = ANLA-TXT50+0(40).
        IF ANLA-GDLGRP = SPACE.
           TABLE-DESCRP2 = ANLA-TXA50+0(28).
        ELSE.
           CONCATENATE: ANLA-TXA50+0(19) ANLA-GDLGRP
                            INTO TABLE-DESCRP2 SEPARATED BY SPACE.
        ENDIF.
        TABLE-KFZKZ   = ANLZ-KFZKZ.
        TABLE-ADDAMT  = ADD_TOTAL.
        TABLE-TRNAMT  = TRN_TOTAL.
        TABLE-RETAMT  = REP_TOTAL.
        APPEND TABLE.
  CLEAR: TABLE, ADD_TOTAL, TRN_TOTAL, REP_TOTAL.
ENDFORM.

*------------------- GET ANLA  -----------------------------------------
FORM GET_ANLA.
       SELECT SINGLE * FROM ANLA
          WHERE BUKRS = BIG_TABLE-BUKRS   AND
                ANLN1 = BIG_TABLE-ANLN1   AND
                ANLN2 = BIG_TABLE-ANLN2   AND
                ANLKL  IN S_ANLKL         AND
                GDLGRP IN S_GDLGRP.
ENDFORM.

*------------------- GET ANLZ  -----------------------------------------
FORM GET_ANLZ.
     SELECT SINGLE * FROM ANLZ
         WHERE BUKRS = BIG_TABLE-BUKRS   AND
               ANLN1 = BIG_TABLE-ANLN1   AND
               ANLN2 = BIG_TABLE-ANLN2   AND
               WERKS IN S_WERKS     AND
               KFZKZ IN S_KFZKZ.
ENDFORM.

*------------------- GET ANKT (Asset Class description)----------------
FORM GET_ANKT.
     SELECT SINGLE * FROM ANKT
         WHERE SPRAS = 'E'   AND
               ANLKL = TABLE-ANLKL.
ENDFORM.

*------------------- GET T001W (Plant code description)----------------
FORM GET_T001W.
     SELECT SINGLE * FROM T001W
         WHERE WERKS = TABLE-WERKS.
ENDFORM.

*------------------- GET ANEP (Transaction Data)------------------------

FORM GET_ANEP.
 CLEAR: ADD_TOTAL, REP_TOTAL, TRN_TOTAL.
 SELECT * FROM ANEP
   WHERE BUKRS = P_CCODE
     AND ANLN1 = BIG_TABLE-ANLN1
     AND ANLN2 = BIG_TABLE-ANLN2
     AND GJAHR = P_FYEAR
     AND AFABE = P_AFABE                                 "TR314
     AND ( BWASL IN S_ADDTRN     OR
           BWASL IN S_REPTRN     OR
           BWASL IN S_TRFTRN ).
   IF SY-SUBRC = 0.
      IF      ANEP-BWASL IN S_ADDTRN.
              ADD_TOTAL = ADD_TOTAL  +  ANEP-ANBTR.
      ELSEIF  ANEP-BWASL IN S_REPTRN.
              REP_TOTAL = REP_TOTAL  +  ANEP-ANBTR.
      ELSE.   TRN_TOTAL = TRN_TOTAL  +  ANEP-ANBTR.
      ENDIF.
   ENDIF.
 ENDSELECT.
ENDFORM.

*------------------- WRITE AMOUNT --------------------------------------

FORM WRITE_AMOUNT.

         IF TABLE-OPBAL <  0.
            FORMAT COLOR COL_NEGATIVE INVERSE ON.
            WRITE:  TABLE-OPBAL UNDER TEXT-011 NO-GAP.
            FORMAT COLOR OFF INVERSE OFF.
         ELSE.
            WRITE:  TABLE-OPBAL UNDER TEXT-011 NO-GAP.
         ENDIF.

         IF TABLE-ADDAMT <  0.
            FORMAT COLOR COL_NEGATIVE INVERSE ON.
            WRITE:  TABLE-ADDAMT  UNDER TEXT-012 NO-GAP.
            FORMAT COLOR OFF INVERSE OFF.
         ELSE.
            WRITE:  TABLE-ADDAMT UNDER TEXT-012 NO-GAP.
         ENDIF.

         IF TABLE-TRNAMT <  0.
            FORMAT COLOR COL_NEGATIVE INVERSE ON.
            WRITE: TABLE-TRNAMT  UNDER TEXT-014 NO-GAP.
            FORMAT COLOR OFF INVERSE OFF.
         ELSE.
            WRITE: TABLE-TRNAMT  UNDER TEXT-014 NO-GAP.
         ENDIF.

         IF TABLE-RETAMT <  0.
            FORMAT COLOR COL_NEGATIVE INVERSE ON.
            WRITE: TABLE-RETAMT UNDER TEXT-016 NO-GAP.
            FORMAT COLOR OFF INVERSE OFF.
         ELSE.
            WRITE: TABLE-RETAMT  UNDER TEXT-016 NO-GAP.
         ENDIF.

         IF TABLE-CLOBAL2 <  0.
            FORMAT COLOR COL_NEGATIVE INVERSE ON.
            WRITE: TABLE-CLOBAL2 UNDER TEXT-019 NO-GAP.
            FORMAT COLOR OFF INVERSE OFF.
         ELSE.
            WRITE: TABLE-CLOBAL2 UNDER TEXT-019 NO-GAP.
         ENDIF.
ENDFORM.

*------------------- PRINT VARIANT -------------------------------------

FORM PRINT_VARIANT.
   WRITE: /20 TEXT-100, 50 TEXT-101.
   WRITE: / SY-ULINE(19) UNDER TEXT-100.

   WRITE: / TEXT-102 UNDER  TEXT-100,
            P_FYEAR  UNDER  TEXT-101.

   WRITE: / TEXT-103 UNDER  TEXT-100,
            P_CCODE  UNDER  TEXT-101.


   WRITE: / TEXT-104 UNDER TEXT-100.
   LOOP AT S_ANLKL.
    IF S_ANLKL-OPTION = 'EQ'.
      WRITE: S_ANLKL-OPTION UNDER TEXT-101, S_ANLKL-LOW.
    ELSE.
      WRITE: S_ANLKL-OPTION UNDER TEXT-101, S_ANLKL-LOW, S_ANLKL-HIGH.
    ENDIF.
      SKIP.
  ENDLOOP.

   WRITE: / TEXT-105 UNDER TEXT-100.
   LOOP AT S_ANLN1.
    IF S_ANLN1-OPTION = 'EQ'.
      WRITE: S_ANLN1-OPTION UNDER TEXT-101, S_ANLN1-LOW.
    ELSE.
      WRITE: S_ANLN1-OPTION UNDER TEXT-101, S_ANLN1-LOW, S_ANLN1-HIGH.
    ENDIF.
      SKIP.
   ENDLOOP.

   WRITE: / TEXT-106 UNDER TEXT-100.
   LOOP AT S_ANLN2.
    IF S_ANLN2-OPTION = 'EQ'.
      WRITE: S_ANLN2-OPTION UNDER TEXT-101, S_ANLN2-LOW.
    ELSE.
      WRITE: S_ANLN2-OPTION UNDER TEXT-101, S_ANLN2-LOW, S_ANLN2-HIGH.
    ENDIF.
      SKIP.
   ENDLOOP.

   WRITE: / TEXT-107 UNDER TEXT-100.
   LOOP AT S_WERKS.
    IF S_WERKS-OPTION = 'EQ'.
      WRITE: S_WERKS-OPTION UNDER TEXT-101, S_WERKS-LOW.
    ELSE.
      WRITE: S_WERKS-OPTION UNDER TEXT-101, S_WERKS-LOW, S_WERKS-HIGH.
    ENDIF.
    SKIP.
   ENDLOOP.

   WRITE: / TEXT-010 UNDER TEXT-100.
   LOOP AT S_GDLGRP.
    IF S_GDLGRP-OPTION = 'EQ'.
      WRITE: S_GDLGRP-OPTION UNDER TEXT-101, S_GDLGRP-LOW.
    ELSE.
     WRITE: S_GDLGRP-OPTION UNDER TEXT-101, S_GDLGRP-LOW, S_GDLGRP-HIGH.
    ENDIF.
     SKIP.
   ENDLOOP.

   WRITE: / TEXT-109 UNDER TEXT-100.
   LOOP AT S_KFZKZ.
    IF S_KFZKZ-OPTION = 'EQ'.
      WRITE: S_KFZKZ-OPTION UNDER TEXT-101, S_KFZKZ-LOW.
    ELSE.
     WRITE: S_KFZKZ-OPTION UNDER TEXT-101, S_KFZKZ-LOW, S_KFZKZ-HIGH.
    ENDIF.
     SKIP.
   ENDLOOP.

   WRITE: / TEXT-110 UNDER TEXT-100.
 LOOP AT S_ADDTRN.
  IF S_ADDTRN-OPTION = 'EQ'.
     WRITE: / S_ADDTRN-OPTION UNDER TEXT-101, S_ADDTRN-LOW.
  ELSE.
   WRITE: / S_ADDTRN-OPTION UNDER TEXT-101, S_ADDTRN-LOW, S_ADDTRN-HIGH.
  ENDIF.
 ENDLOOP.

   WRITE: / TEXT-111 UNDER TEXT-100.
   LOOP AT S_REPTRN.
    IF S_REPTRN-OPTION = 'EQ'.
      WRITE: / S_REPTRN-OPTION UNDER TEXT-101, S_REPTRN-LOW.
    ELSE.
   WRITE: / S_REPTRN-OPTION UNDER TEXT-101, S_REPTRN-LOW, S_REPTRN-HIGH.
    ENDIF.
   ENDLOOP.

   WRITE: / TEXT-112 UNDER TEXT-100.
   LOOP AT S_TRFTRN.
    IF S_TRFTRN-OPTION = 'EQ'.
      WRITE: / S_TRFTRN-OPTION UNDER TEXT-101, S_TRFTRN-LOW.
    ELSE.
   WRITE: / S_TRFTRN-OPTION UNDER TEXT-101, S_TRFTRN-LOW, S_TRFTRN-HIGH.
    ENDIF.
   ENDLOOP.

   WRITE: / TEXT-003 UNDER TEXT-100.
   WRITE: / SY-ULINE(12) UNDER TEXT-100.
   WRITE: / TEXT-105 UNDER TEXT-100.
   LOOP AT S_EXCLD1.
    IF S_EXCLD1-OPTION = 'EQ'.
      WRITE: S_EXCLD1-OPTION UNDER TEXT-101, S_EXCLD1-LOW.
    ELSE.
     WRITE: S_EXCLD1-OPTION UNDER TEXT-101, S_EXCLD1-LOW, S_EXCLD1-HIGH.
    ENDIF.
    SKIP.
   ENDLOOP.

   WRITE: / TEXT-106 UNDER TEXT-100.
   LOOP AT S_EXCLD2.
    IF S_EXCLD2-OPTION = 'EQ'.
      WRITE: S_EXCLD2-OPTION UNDER TEXT-101, S_EXCLD2-LOW.
    ELSE.
     WRITE: S_EXCLD2-OPTION UNDER TEXT-101, S_EXCLD2-LOW, S_EXCLD2-HIGH.
    ENDIF.
     SKIP.
   ENDLOOP.
   NEW-PAGE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  EXCLDE_CHECK
*&---------------------------------------------------------------------*
FORM EXCLDE_CHECK.
    EXCLUDE_FLAG  = 'N'.
    CONCATENATE TABLE-ANLN1 TABLE-ANLN2 INTO FULL_ASSETNO.
    READ TABLE EXCLUDE_TABLE WITH KEY ASSETNO = FULL_ASSETNO BINARY
                   SEARCH.
         IF SY-SUBRC = 0.
            EXCLUDE_FLAG  = 'Y'.
         ENDIF.
ENDFORM.                    " EXCLDE_CHECK

*&---------------------------------------------------------------------*
*&      Form  BUILD_EXCLUDE_TABLE
*&---------------------------------------------------------------------*
FORM BUILD_EXCLUDE_TABLE.
     CURRENT_INDEX = 0.
     DO MAINASSET_LINES TIMES.
       CURRENT_INDEX = CURRENT_INDEX + 1.
       READ TABLE S_EXCLD1 INDEX CURRENT_INDEX.
       READ TABLE S_EXCLD2 INDEX CURRENT_INDEX.
       CONCATENATE S_EXCLD1-LOW S_EXCLD2-LOW INTO EXCLUDE_TABLE-ASSETNO.
       APPEND EXCLUDE_TABLE.
       CLEAR  EXCLUDE_TABLE.
     ENDDO.
ENDFORM.                    " BUILD_EXCLUDE_TABLE
*&---------------------------------------------------------------------*
*&      Form  BEREICH_VALUE      TR314
*&---------------------------------------------------------------------*
FORM BEREICH_VALUE USING VALUE(F_NAME)
                         AFABE LIKE RBADA-AFABE1.
*
DATA: L_DYNNR LIKE SY-DYNNR,
      L_REPID LIKE SY-REPID,
      L_AFABE LIKE T093-AFABER.
*
  L_REPID = SY-CPROG.
  L_DYNNR = SY-DYNNR.

CALL FUNCTION 'AM_AFABE_F4'
     EXPORTING
          I_DYNAME   = L_REPID
          I_DYNUMB   = L_DYNNR
          I_FN_BUK   = 'P_CCODE'
          I_FN_AFABE = F_NAME
     IMPORTING
          E_AFABE    = L_AFABE
     EXCEPTIONS
          OTHERS     = 1.
*
    IF NOT L_AFABE IS INITIAL.
      AFABE = L_AFABE.
    ENDIF.

*
ENDFORM.
*------------------- TOP-OF-PAGE ---------------------------------------
TOP-OF-PAGE.
IF HEAD_PRINT = 'Y'.
   IF TABLE-ANLKL <> PREV_ANLKL.
      PERFORM GET_ANKT.                       "Asset Class Description
      PREV_ANLKL = TABLE-ANLKL.
   ENDIF.
   WRITE: /1 TEXT-RPT, SY-REPID,  72 T001-BUTXT,             "Company
         140 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
   WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-SYSID, SY-MANDT,  "Title
          69 P_REPTTL,
            TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.
*  WRITE: /72 TEXT-TL2, P_FYEAR.                     "Fiscal Year TR314
   WRITE: / TEXT-015 UNDER TEXT-RPT, P_AFABE, 69 TEXT-TL2, P_FYEAR.

   WRITE: / TEXT-104 UNDER TEXT-RPT, ':',  TABLE-ANLKL NO-GAP,
                           '-' ,  ANKT-TXK50.       "Asset Class & Dscrp
   ULINE.
   WRITE: 1 TEXT-006, 19 TEXT-007, 60 TEXT-008, 88 TEXT-009,
          96 TEXT-011, 111 TEXT-012, 126 TEXT-014, 140 TEXT-016,
          156 TEXT-019.
   ULINE.
   IF P_FILE = 'X'.
      HEAD_PRINT = 'N'.
   ENDIF.
ENDIF.
*------------------------- End of Report Header ------------------------
