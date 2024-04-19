REPORT ZAAMR005 NO STANDARD PAGE HEADING LINE-SIZE 255 LINE-COUNT 65.
************************************************************************
* Program     : ZAAMR005
* Programmer  : M DeMeester
* Client      : Union Gas Ltd.
* Date        : May 1997.
*
* Description : Displays the ASSET MANAGEMENT/LAND FILE NUMBER QUERY
*               based on the selection criteria.
************************************************************************
* Date: 2003/07/25     By: Mohammad Khan    Issue# 1020
* Description: Add new fields to variants (cost center, plant code,
*              asset number, asset class & Asset Valuation Date).
*-----------------------------------------------------------------------
* 97/09/22 md7140 calculate current balance
************************************************************************
TABLES:  ANLA, ANLC, ANLZ, T001.

DATA: BEGIN OF TABLE1    OCCURS 0,
      GDLGRP               LIKE ANLA-GDLGRP,        "Station Id
*     anln1                like anla-anln1,         "Asset Number
      ANLN1(14) TYPE C,                            "Asset Number
      ANLN2                LIKE ANLA-ANLN2,         "Sub Number
      TXT50                LIKE ANLA-TXT50,         "Description 1
      TXA50                LIKE ANLA-TXA50,         "Description 2
      MENGE                LIKE ANLA-MENGE,         "Quantity
      MEINS                LIKE ANLA-MEINS,         "UOM
      AKTIV                LIKE ANLA-AKTIV,         "Capitalized Date
      POSNR                LIKE ANLA-POSNR,         "WBS Element
      KANSW                LIKE ANLC-KANSW,         "Acquisition Value
      CURBAL               LIKE ANLC-KANSW,         "Current Value
      ORD41                LIKE ANLA-ORD41,         "Pipe Size
      ORD42                LIKE ANLA-ORD42,         "Sector
      ORD43                LIKE ANLA-ORD43,         "Well Type
      SERNR                LIKE ANLA-SERNR,         "Rectifier Number
      WERKS                LIKE ANLZ-WERKS,         "Plant Code
      KFZKZ                LIKE ANLZ-KFZKZ,         "Land File Number
      END OF TABLE1.

*SELECTION-SCREEN SKIP.
*SELECTION-SCREEN COMMENT 1(80) TEXT-001.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME TITLE TEXT-001.
PARAMETERS:
      P_BUKRS    LIKE ANLA-BUKRS     OBLIGATORY.
SELECT-OPTIONS:
      S_KFZKZ  FOR ANLZ-KFZKZ,      "Land File Number
      S_KOSTL  FOR ANLZ-KOSTL,      "Cost Center          Issue 1020
      S_WERKS  FOR ANLZ-WERKS,      "Plant Code           Issue 1020
      S_ANLKL  FOR ANLA-ANLKL,      "Asset Class          Issue 1020
      S_ANLN1  FOR ANLZ-ANLN1,      "Asset Number         Issue 1020
      S_AKTIV  FOR ANLA-AKTIV.      "AVD                  Issue 1020
SELECTION-SCREEN SKIP 1.                                 "Issue 1020
PARAMETERS: P_ZERO  AS CHECKBOX DEFAULT ' '.             "Issue 1020
SELECTION-SCREEN END OF BLOCK BOX1.

*************************** MAIN **************************************
TOP-OF-PAGE.
WRITE: /1 TEXT-002, SY-REPID COLOR COL_NEGATIVE,
      110 TEXT-003 COLOR COL_HEADING,
      220 TEXT-004, SY-DATUM, TEXT-005, SY-UZEIT.
WRITE: / TEXT-006 UNDER TEXT-002, SY-MANDT UNDER SY-REPID,
      125 TEXT-021,
         TEXT-007 UNDER TEXT-004, SY-PAGNO UNDER SY-DATUM.
WRITE: /118 T001-BUTXT.
WRITE: /.
ULINE.
WRITE: / TEXT-034, TEXT-035.                                 "97/09/22
ULINE.

WRITE: /1 TEXT-009,  10 TEXT-026,  15 TEXT-028,  20 TEXT-030,
       28 TEXT-031,  45 TEXT-018,  65 TEXT-010,  81 TEXT-011,
      104 TEXT-013,                                  "Quantity
      121 TEXT-014,                                  "Unit of Measure
      125 TEXT-015,  88 TEXT-024, 137 TEXT-022,
      143 TEXT-012, 193 TEXT-020,                    "Description
      245 TEXT-017.
WRITE: / TEXT-019 UNDER TEXT-009,                    "Station Id
         TEXT-027 UNDER TEXT-026,                    "Pipe Size
         TEXT-029 UNDER TEXT-028,                    "Well Size
         TEXT-019 UNDER TEXT-031,                    "Land File #
         TEXT-019 UNDER TEXT-018,                    "Rectifier #
         TEXT-019 UNDER TEXT-010,                    "Asset #
         TEXT-019 UNDER TEXT-011,                    "Sub #
         TEXT-016 UNDER TEXT-015,                    "Capitalized Date
         TEXT-025 UNDER TEXT-024,                    "Acquisition Value
         TEXT-023 UNDER TEXT-022,                    "Plant Code
         TEXT-032 UNDER TEXT-017.                    "WSB Element

ULINE.

****************** Start of Selection **********************************
START-OF-SELECTION.
SELECT SINGLE * FROM T001              "Get Company Name
    WHERE BUKRS = P_BUKRS.

SELECT * FROM ANLZ                     "All other details for report
 WHERE   BUKRS =  P_BUKRS
   AND   ANLN1 IN S_ANLN1              "Issue 1020
   AND   KOSTL IN S_KOSTL              "Issue 1020
   AND   WERKS IN S_WERKS              "Issue 1020
   AND   KFZKZ IN S_KFZKZ.

    PERFORM CREATE_TABLE.
ENDSELECT.

SORT: TABLE1 BY KFZKZ ANLN1 ANLN2.

LOOP AT TABLE1.
    WRITE: / TABLE1-GDLGRP UNDER TEXT-009,          "Station ID
          TABLE1-ANLN1  UNDER TEXT-010 USING EDIT MASK '_____ __ _____',
             TABLE1-ANLN2  UNDER TEXT-011,          "Sub Number
             TABLE1-TXT50  UNDER TEXT-012,          "Description
             TABLE1-TXA50  UNDER TEXT-020,          "Description 2
             TABLE1-MENGE  UNDER TEXT-013,          "Quantity
             TABLE1-MEINS  UNDER TEXT-014,          "UOM
             TABLE1-AKTIV  UNDER TEXT-015,          "Capitalized Date
             TABLE1-POSNR  UNDER TEXT-017,          "WBS Element
             TABLE1-ORD41  UNDER TEXT-026,          "Pipe Size
             TABLE1-ORD43  UNDER TEXT-028,          "Well Type
             TABLE1-ORD42  UNDER TEXT-030,          "Sector
             TABLE1-KFZKZ  UNDER TEXT-031,          "Land File #
             TABLE1-SERNR  UNDER TEXT-018,          "Rectifier #
*            table1-kansw  under text-024,          "Acquisition Value
             TABLE1-WERKS  UNDER TEXT-022.          "Plant Code

    IF TABLE1-KANSW = TABLE1-CURBAL.                      "97/09/22
       WRITE: TABLE1-CURBAL UNDER TEXT-024.
    ELSE.
       WRITE: TABLE1-CURBAL UNDER TEXT-024 COLOR COL_TOTAL.
    ENDIF.

   WRITE: /.
ENDLOOP.

SKIP 4.
WRITE: / TEXT-033 UNDER TEXT-011.                   "END OF REPORT

******************** CREATE_TABLE **************************************
* This routine adds items meeting the selection criteria to TABLE1.
******************** CREATE_TABLE **************************************
FORM CREATE_TABLE.
   CLEAR TABLE1.
   MOVE ANLZ-WERKS   TO TABLE1-WERKS.             "Plant Code
   MOVE ANLZ-KFZKZ   TO TABLE1-KFZKZ.             "Land File Number

   SELECT SINGLE * FROM ANLA                       "Issue 1020
        WHERE BUKRS = P_BUKRS
          AND ANLN1 = ANLZ-ANLN1
          AND ANLN2 = ANLZ-ANLN2
          AND ANLKL IN S_ANLKL                     "Issue 1020
          AND AKTIV IN S_AKTIV.                    "Issue 1020
      MOVE ANLA-GDLGRP        TO TABLE1-GDLGRP.        "Station Id
      MOVE ANLA-ANLN1         TO TABLE1-ANLN1.         "Asset Number
      MOVE ANLA-ANLN2         TO TABLE1-ANLN2.         "Sub Number
      MOVE ANLA-TXT50         TO TABLE1-TXT50.         "Description 1
      MOVE ANLA-TXA50         TO TABLE1-TXA50.         "Description 2
      MOVE ANLA-MENGE         TO TABLE1-MENGE.         "Quantity
      MOVE ANLA-MEINS         TO TABLE1-MEINS.         "UOM
      MOVE ANLA-AKTIV         TO TABLE1-AKTIV.         "Capitalized Date
      MOVE ANLA-POSNR         TO TABLE1-POSNR.         "WBS Element
      MOVE ANLA-ORD41         TO TABLE1-ORD41.         "Pipe Size
      MOVE ANLA-ORD42         TO TABLE1-ORD42.         "Sector
      MOVE ANLA-ORD43         TO TABLE1-ORD43.         "Well Type
      MOVE ANLA-SERNR         TO TABLE1-SERNR.         "Rectifier #
*   ENDSELECT.                                    "Issue 1020
 IF SY-SUBRC = 0.                                 "Issue 1020
    SELECT * FROM ANLC
     WHERE BUKRS = P_BUKRS
       AND ANLN1 = ANLZ-ANLN1
       AND ANLN2 = ANLZ-ANLN2.

    MOVE ANLC-KANSW   TO TABLE1-KANSW.            "Acquisition Value
    TABLE1-CURBAL = ANLC-KANSW + ANLC-ANSWL             "97/09/22
                  + ANLC-KINVZ + ANLC-INVZM.
    ENDSELECT.
    IF P_ZERO = 'X'.                              "Issue 1020
       APPEND TABLE1.                             "Issue 1020
    ELSE.                                         "Issue 1020
       IF TABLE1-CURBAL <> 0.                     "Issue 1020
          APPEND TABLE1.                          "Issue 1020
       ENDIF.                                     "Issue 1020
    ENDIF.                                        "Issue 1020
 ENDIF.                                           "Issue 1020
ENDFORM.
