REPORT ZAFAI001 NO STANDARD PAGE HEADING LINE-SIZE 132
                LINE-COUNT 65 MESSAGE-ID AM.

************************************************************************
*
*   PROGRAM:    ZAFAI001
*   PROGRAMMER: MaryLou DeMeester
*   CLIENT:     Union Gas
*   DATE:       April 1999.
*
*   The purpose of this program is to capitalize Distribution, GSO,
*   NGV and A & G overhead costs at yearend.
*   Several steps are involved in the overhead allocation
*   A) Sum all the AUC's and ASSETS for the fiscal year
*   B) Cross reference each asset to its corresponding project
*   C) Based of the WBS element, group into one of four categories:
*      DST, GSO, NGV and OTHER
*   D) Calculate amount to be distributed
*   E) Create BDC session (ZAM_OVERHEAD) and post

************************************************************************
*  CHANGES
* 02/01/21 #988 Mokhan      - Reduce the run time:
*                             1- Cahnged the sequence of WHERE clause
*                                fields according to the sequence of
*                                key fields for DB tables COSS & COSP.
*                             2- Changed the subroutine "GET_ASSETS" to
*                                use the inner join for data extract
*                                from DB tables ANEP, ANLA and ANLZ,
*                                replacing select - endselects.
* 01/10/03 #--- Mokhan      - Add Asset Valu date in the variants and
*                             when entered, use it for Asset value date.
* 00/12/07 #--- M DeMeester - check the group asset indicator on ANLA
*                             and allow only subs to process
*                             ANLA-XANLGR = space.
*
* 00/11/29 #750 Mohammad Khan 1- Remove hard coded values for Asset
*                                Trans. Types and put them in Variants.
*                             2- Ignore the zero dollar amount rows.
*
* 99/06/24 #494 M DeMeester - fix the check boxes for the 4 groups
*                             DST,GST,NGV, A&G
*
* 99/06/07 #494 M DeMeester - added changes made in QA while Dev was
*                             closed because of upgrade to 3.1I
*
* 99/04/05 #494 M DeMeester - used ZPPMI001 as a base and changed
*                             according to above rules
************************************************************************

TABLES:  PROJ,            "Project Table
         PRPS,            "WBS Element Table
*        cobrb,           "Distribution Settlement Rule Table
         ANEP,            "Asset Line Items
*        anle,            "Asset Line Item Value (for cuurent year)
         ANLK,            "Asset origin by Cost Elements (for AUC)
         ANLZ,            "Union vs Centra profile cost centre
         ZOHDP,           " Distribution WBS elements
         ZOHGS,           " GSO WBS elements
         ZOHNP,           " NGV WBS elements
         ZOHAG,           " A & G WBS elements
         COSS,            " CO object: internal postings
         COSP,            " CO object: external postings
         T001,            " company codes
         ANLC,            " for AUC amounts
         ANLA.            " Asset master info

DATA:     TBLDAT        TYPE D,                         " reformat date
          TBUDAT        TYPE D,                         " reformat date
          ZBLDAT(10),                                   " reformat date
          ZBUDAT(10),                                   " reformat date
          BDC_AMT(13),                                  " BDC amount
          TRX_TYP       LIKE ANBZ-BWASL,                " BDC trans type
          TEMP_AMT      LIKE COSS-WKG001.

DATA:     WRK_AMT       LIKE COSP-WKG001,
          TEXTPCT(30)   TYPE C,
          PCT_AMT       TYPE P DECIMALS 7.

DATA:     DST_AMT       LIKE COSS-WKG001,               " distribution
          DST_WBS       LIKE COSS-WKG001,               " WBS total amt
          DST_CAP_AMT   LIKE COSP-WKG001,              " Capitalized Amt
          DST_NON_AMT   LIKE COSP-WKG001,              " Capitalized Amt
          DST_PCT       TYPE P DECIMALS 7,
          GSO_AMT       LIKE COSS-WKG001,               " GSO
          GSO_WBS       LIKE COSS-WKG001,               " WBS total amt
          GSO_CAP_AMT   LIKE COSP-WKG001,              " Capitalized Amt
          GSO_NON_AMT   LIKE COSP-WKG001,              " Capitalized Amt
          GSO_PCT       TYPE P DECIMALS 7,
          NGV_AMT       LIKE COSS-WKG001,               " NGV
          NGV_WBS       LIKE COSS-WKG001,               " WBS total amt
          NGV_CAP_AMT   LIKE COSP-WKG001,              " Capitalized Amt
          NGV_NON_AMT   LIKE COSP-WKG001,              " Capitalized Amt
          NGV_PCT       TYPE P DECIMALS 7,
          ANG_AMT       LIKE COSS-WKG001,               " A&G
          ANG_WBS       LIKE COSS-WKG001,               " WBS total amt
          ANG_CAP_AMT   LIKE COSP-WKG001,              " Capitalized Amt
          ANG_NON_AMT   LIKE COSP-WKG001,              " Capitalized Amt
          ANG_PCT       TYPE P DECIMALS 7.


DATA:     BEGIN OF TABLEWA OCCURS 5000,
             OBJNR      LIKE PRPS-OBJNR,
             POSID      LIKE PRPS-POSID,
             POST1      LIKE PRPS-POST1,
          END OF TABLEWA.

DATA:     BEGIN OF TABLEWC OCCURS 5000,
             ASSETCL(5) TYPE C,
             PSPID      LIKE PRPS-POSID,
             WBSEL      LIKE ZOHDP-WBSEL,
             ANLN1      LIKE ANLC-ANLN1,
             ANLN2      LIKE ANLC-ANLN2,
             OBJNR      LIKE PRPS-OBJNR,
             POSID      LIKE PRPS-POSID,
             POST1      LIKE PRPS-POST1,
             WBS_AMT    LIKE COSS-WKG001,              " WBS amount
             CAP_AMT    LIKE COSS-WKG001,              " capitalize amt
          END OF TABLEWC.

DATA:     BEGIN OF TABLEWD OCCURS 1000,
             ANLN1      LIKE ANLC-ANLN1,
             ANLN2      LIKE ANLC-ANLN2,
             WBS_AMT    LIKE COSS-WKG001,              " WBS amount
             CAP_AMT    LIKE COSS-WKG001,              " capitalize amt
          END OF TABLEWD.

DATA:     BEGIN OF DSTTAB OCCURS 2000.
           INCLUDE STRUCTURE TABLEWC.
DATA:     END OF DSTTAB.

DATA:     BEGIN OF GSOTAB OCCURS 2000.
           INCLUDE STRUCTURE TABLEWC.
DATA:     END OF GSOTAB.

DATA:     BEGIN OF NGVTAB OCCURS 2000.
           INCLUDE STRUCTURE TABLEWC.
DATA:     END OF NGVTAB.

DATA:     BEGIN OF ANGTAB OCCURS 2000.
           INCLUDE STRUCTURE TABLEWC.
DATA:     END OF ANGTAB.

DATA:     BEGIN OF NEWTAB OCCURS 2000,            "Issue 988
              ANLN1 LIKE ANEP-ANLN1,              "Issue 988
              ANLN2 LIKE ANEP-ANLN2,              "Issue 988
              ANBTR LIKE ANEP-ANBTR,              "Issue 988
          END OF NEWTAB.                          "Issue 988

DATA:     BEGIN OF BDCDATA OCCURS 100.
            INCLUDE STRUCTURE BDCDATA.
DATA:     END OF BDCDATA.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.               "Titles
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 26(27) TEXT-001.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

  SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN COMMENT 3(70) TEXT-002.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN COMMENT 3(70) TEXT-003.
  SELECTION-SCREEN END OF LINE.

  SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN COMMENT 3(70) TEXT-004.
  SELECTION-SCREEN END OF LINE.

*  selection-screen begin of line.
*   selection-screen comment 3(70) text-032.
*  selection-screen end of line.
                                         .

  PARAMETERS: P_RPT       AS CHECKBOX DEFAULT 'X'.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
   PARAMETERS: P_COMP   LIKE PRPS-PBUKR OBLIGATORY DEFAULT 'UGL'.
   PARAMETERS: P_YEAR   LIKE COSS-GJAHR OBLIGATORY DEFAULT SY-DATUM(4).
   SELECT-OPTIONS: S_PSPID FOR PROJ-PSPID,
                   S_VERNR FOR PROJ-VERNR,
                   S_BWASL FOR ANEP-BWASL.               "Issue log 750
   PARAMETERS: P_AVD     LIKE ANBZ-BZDAT,                "Issue log 840
               P_GKONT   LIKE RA01B-GKONT OBLIGATORY DEFAULT '115994',
               P_KOSTL   LIKE ANLZ-KOSTL  OBLIGATORY.
  SELECT-OPTIONS: S_KSTAR   FOR ANLK-KSTAR DEFAULT '491001' TO '491002'.


SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
  SELECTION-SCREEN BEGIN OF LINE.                          "DISTRIBUTION
      SELECTION-SCREEN COMMENT 1(17) TEXT-005.
      SELECTION-SCREEN POSITION 20.
      PARAMETERS: P_DST       AS CHECKBOX DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 33(24) TEXT-006.
      PARAMETERS: P_DSTWBS    LIKE PRPS-POSID
                              DEFAULT '37-97-081-5821'.
  SELECTION-SCREEN END OF LINE.

  PARAMETERS: P_DSTAMT  LIKE COSS-WKG001.
  SELECT-OPTIONS: S_DSTAC   FOR ANEP-ANLN1.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN ULINE 10(65).                    "End of DISTRIBUTION

  SELECTION-SCREEN BEGIN OF LINE.                                 "GSO
      SELECTION-SCREEN COMMENT 1(17) TEXT-005.
      SELECTION-SCREEN POSITION 20.
      PARAMETERS: P_GSO       AS CHECKBOX DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 33(24) TEXT-008.
      PARAMETERS: P_GSOWBS    LIKE PRPS-POSID
                              DEFAULT '37-97-081-5822'.
  SELECTION-SCREEN END OF LINE.

  PARAMETERS: P_GSOAMT    LIKE COSS-WKG001.
  SELECT-OPTIONS: S_GSOAC   FOR ANEP-ANLN1.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN ULINE 10(65).                             "End of GSO

  SELECTION-SCREEN BEGIN OF LINE.                                   "NGV
      SELECTION-SCREEN COMMENT 1(17) TEXT-005.
      SELECTION-SCREEN POSITION 20.
      PARAMETERS: P_NGV       AS CHECKBOX DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 33(24) TEXT-009.
      PARAMETERS: P_NGVWBS    LIKE PRPS-POSID
                              DEFAULT '37-97-081-5823'.
  SELECTION-SCREEN END OF LINE.

  PARAMETERS: P_NGVAMT    LIKE COSS-WKG001.
  SELECT-OPTIONS: S_NGVAC   FOR ANEP-ANLN1.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN ULINE 10(65).                             "End of NGV

  SELECTION-SCREEN BEGIN OF LINE.                                   "A&G
      SELECTION-SCREEN COMMENT 1(17) TEXT-005.
      SELECTION-SCREEN POSITION 20.
      PARAMETERS: P_ANG       AS CHECKBOX DEFAULT 'X'.
      SELECTION-SCREEN COMMENT 33(24) TEXT-010.
      PARAMETERS: P_ANGWBS    LIKE PRPS-POSID
                              DEFAULT '37-97-081-5824'.
  SELECTION-SCREEN END OF LINE.

  PARAMETERS: P_ANGAMT    LIKE COSS-WKG001.
  SELECT-OPTIONS: S_ANGAC   FOR ANEP-ANLN1.
  SELECTION-SCREEN SKIP 1.
  SELECTION-SCREEN ULINE 10(65).                           "End of A&G

SELECTION-SCREEN END OF BLOCK BOX3.

*-----------------------  END of SELECTION Screen ----------------------
*........................INITIALIZATION ...........Issue log 750
INITIALIZATION.
 S_BWASL-SIGN   = 'I'.                     " Asset Transaction Types
 S_BWASL-OPTION = 'EQ'.
 S_BWASL-LOW    = '115'.
 S_BWASL-HIGH   = '   '.
 APPEND S_BWASL.
 CLEAR  S_BWASL.
 S_BWASL-SIGN   = 'I'.
 S_BWASL-OPTION = 'EQ'.
 S_BWASL-LOW    = '331'.
 S_BWASL-HIGH   = '   '.
 APPEND S_BWASL.
 CLEAR  S_BWASL.
 S_BWASL-SIGN   = 'I'.
 S_BWASL-OPTION = 'EQ'.
 S_BWASL-LOW    = '336'.
 S_BWASL-HIGH   = '   '.
 APPEND S_BWASL.
 CLEAR  S_BWASL.
************************************************************************
AT SELECTION-SCREEN.
  SELECT SINGLE * FROM T001
     WHERE BUKRS = P_COMP.

*---------------------  START-OF-SELECTION  ----------------------------
START-OF-SELECTION.
 PERFORM GET_AUCS.     "Select all Dollars for Assets under construction
 PERFORM GET_ASSETS.   "Select all Dollars for Assets
 PERFORM CALCULATE_TOTAL_AMTS.
 PERFORM CALCULATE_DISBURSEMENTS.
 PERFORM WRITE_DST.
 PERFORM WRITE_GSO.
 PERFORM WRITE_NGV.
 PERFORM WRITE_ANG.
PERFORM TOTAL_PAGE.
PERFORM CREATE_BDC_SESSION.


*---------------------- SUBROOUTINES -----------------------------------
*
* --------------------- GET_OVERHEAD_COSTS -----------------------------
FORM GET_OVERHEAD_COSTS USING WBS IND AMT.
DATA: VALUE      LIKE COSP-WKG001.

  WRK_AMT = 0.
  SELECT SINGLE * FROM PRPS
    WHERE   POSID = WBS
      AND   PBUKR = P_COMP.

  IF SY-SUBRC = 0   AND
     IND      = 'X' AND
     AMT      > 0.
*Issue 988-where clause field's seq. changed according to the table key
    SELECT * FROM COSS                            " internal postings
     WHERE  OBJNR = PRPS-OBJNR
       AND  GJAHR = P_YEAR
       AND  WRTTP EQ '04'                         " Actuals only
       AND  VERSN = '000'                         " Version 0
       AND  NOT KSTAR IN S_KSTAR.
     ADD COSS-WKG001 FROM 1 TO 12 GIVING VALUE.
     WRK_AMT = WRK_AMT + VALUE.
   ENDSELECT.

*Issue 988-where clause field's seq. changed according to the table key
    SELECT * FROM COSP                            " external postings
     WHERE  OBJNR = PRPS-OBJNR
       AND  GJAHR = P_YEAR
       AND  WRTTP EQ '04'                        " Actuals only
       AND  VERSN = '000'                         " Version 0
       AND  NOT KSTAR IN S_KSTAR.
     ADD COSP-WKG001 FROM 1 TO 12 GIVING VALUE.
     WRK_AMT = WRK_AMT + VALUE.
   ENDSELECT.

   WRK_AMT = WRK_AMT + AMT.
 ENDIF.
ENDFORM.

* -------------------------------------------------------------------- *


* -------------------------------------------------------------------- *
* Process the individual tables one last time.  This will ouput all of
* the report lines including totals

FORM WRITE_DST.

SORT DSTTAB BY ASSETCL PSPID WBSEL ANLN1 ANLN2.
TEXTPCT = TEXT-024.
PCT_AMT  = DST_PCT.

NEW-PAGE.

LOOP AT DSTTAB.
  AT NEW PSPID(7).
     PERFORM WRITE_VERT.
     WRITE: DSTTAB-PSPID(7) UNDER TEXT-012.
  ENDAT.

  PERFORM WRITE_VERT.                          "Prints WBS Elements
  WRITE: DSTTAB-WBSEL    UNDER TEXT-013,
         DSTTAB-POST1    UNDER TEXT-014,
         DSTTAB-WBS_AMT  UNDER TEXT-015,
         DSTTAB-ANLN1    UNDER TEXT-016,
         DSTTAB-ANLN2    UNDER TEXT-017,
         DSTTAB-CAP_AMT  UNDER TEXT-018.

  AT END OF PSPID(7).
     SUM.
     PERFORM WRITE_VERT.
     WRITE: 12 SY-ULINE(120).
     PERFORM WRITE_VERT.
     WRITE: TEXT-019       UNDER TEXT-013.
     WRITE: DSTTAB-WBS_AMT UNDER TEXT-015,
            DSTTAB-CAP_AMT  UNDER TEXT-018.
     ULINE.
  ENDAT.

  AT END OF ASSETCL.
     SUM.
     PERFORM WRITE_VERT.
     WRITE: TEXT-041       UNDER TEXT-012.
     WRITE: DSTTAB-WBS_AMT UNDER TEXT-015,
            DSTTAB-CAP_AMT  UNDER TEXT-018.
     ULINE.
  ENDAT.

  AT LAST.
     SUM.
     ULINE.
     PERFORM WRITE_VERT.                          "Grand Total for DST
     WRITE: TEXT-020 UNDER TEXT-012,
            DSTTAB-WBS_AMT UNDER TEXT-015,
            DSTTAB-CAP_AMT  UNDER TEXT-018.
     ULINE.
  ENDAT.
ENDLOOP.
ENDFORM.

FORM WRITE_GSO.

SORT GSOTAB BY ASSETCL PSPID WBSEL ANLN1 ANLN2.
TEXTPCT = TEXT-025.
PCT_AMT  = GSO_PCT.

NEW-PAGE.

LOOP AT GSOTAB.
  AT NEW PSPID(7).
     PERFORM WRITE_VERT.
     WRITE: GSOTAB-PSPID(7) UNDER TEXT-012.
  ENDAT.

  PERFORM WRITE_VERT.                          "Prints WBS Elements
  WRITE: GSOTAB-WBSEL    UNDER TEXT-013,
         GSOTAB-POST1    UNDER TEXT-014,
         GSOTAB-WBS_AMT  UNDER TEXT-015,
         GSOTAB-ANLN1    UNDER TEXT-016,
         GSOTAB-ANLN2    UNDER TEXT-017,
         GSOTAB-CAP_AMT  UNDER TEXT-018.

  AT END OF PSPID(7).
     SUM.
     PERFORM WRITE_VERT.
     WRITE: 12 SY-ULINE(120).
     PERFORM WRITE_VERT.
     WRITE: TEXT-019       UNDER TEXT-013.
     WRITE: GSOTAB-WBS_AMT UNDER TEXT-015,
            GSOTAB-CAP_AMT  UNDER TEXT-018.
     ULINE.
  ENDAT.

  AT END OF ASSETCL.
     SUM.
     PERFORM WRITE_VERT.
     WRITE: TEXT-041       UNDER TEXT-012.
     WRITE: GSOTAB-WBS_AMT UNDER TEXT-015,
            GSOTAB-CAP_AMT  UNDER TEXT-018.
     ULINE.
  ENDAT.

  AT LAST.
     SUM.
     ULINE.
     PERFORM WRITE_VERT.                          "Grand Total for GSO
     WRITE: TEXT-021 UNDER TEXT-012,
            GSOTAB-WBS_AMT  UNDER TEXT-015,
            GSOTAB-CAP_AMT  UNDER TEXT-018.
     ULINE.
  ENDAT.
ENDLOOP.
ENDFORM.


FORM WRITE_NGV.

SORT NGVTAB BY ASSETCL PSPID WBSEL ANLN1 ANLN2.
TEXTPCT = TEXT-026.
PCT_AMT  = NGV_PCT.

NEW-PAGE.

LOOP AT NGVTAB.
  AT NEW PSPID(7).
     PERFORM WRITE_VERT.
     WRITE: NGVTAB-PSPID(7) UNDER TEXT-012.
  ENDAT.

  PERFORM WRITE_VERT.                          "Prints WBS Elements
  WRITE: NGVTAB-WBSEL    UNDER TEXT-013,
         NGVTAB-POST1    UNDER TEXT-014,
         NGVTAB-WBS_AMT  UNDER TEXT-015,
         NGVTAB-ANLN1    UNDER TEXT-016,
         NGVTAB-ANLN2    UNDER TEXT-017,
         NGVTAB-CAP_AMT  UNDER TEXT-018.

  AT END OF PSPID(7).
     SUM.
     PERFORM WRITE_VERT.
     WRITE: 12 SY-ULINE(120).
     PERFORM WRITE_VERT.
     WRITE: TEXT-019       UNDER TEXT-013,
            NGVTAB-WBS_AMT UNDER TEXT-015,
            NGVTAB-CAP_AMT  UNDER TEXT-018.
     ULINE.
  ENDAT.



  AT END OF ASSETCL.
     SUM.
     PERFORM WRITE_VERT.
     WRITE: TEXT-041       UNDER TEXT-012.
     WRITE: NGVTAB-WBS_AMT UNDER TEXT-015,
            NGVTAB-CAP_AMT  UNDER TEXT-018.
     ULINE.
  ENDAT.

  AT LAST.
     SUM.
     ULINE.
     PERFORM WRITE_VERT.                          "Grand Total for NGV
     WRITE: TEXT-022 UNDER TEXT-012,
            NGVTAB-WBS_AMT  UNDER TEXT-015,
            NGVTAB-CAP_AMT  UNDER TEXT-018.
     ULINE.
  ENDAT.
ENDLOOP.
ENDFORM.


FORM WRITE_ANG.

SORT ANGTAB BY ASSETCL PSPID WBSEL ANLN1 ANLN2.
TEXTPCT = TEXT-027.
PCT_AMT  = ANG_PCT.

NEW-PAGE.

LOOP AT ANGTAB.
  AT NEW PSPID(7).
     PERFORM WRITE_VERT.
     WRITE: ANGTAB-PSPID(7) UNDER TEXT-012.
  ENDAT.

  PERFORM WRITE_VERT.                          "Prints WBS Elements
  WRITE: ANGTAB-WBSEL    UNDER TEXT-013,
         ANGTAB-POST1    UNDER TEXT-014,
         ANGTAB-WBS_AMT  UNDER TEXT-015,
         ANGTAB-ANLN1    UNDER TEXT-016,
         ANGTAB-ANLN2    UNDER TEXT-017,
         ANGTAB-CAP_AMT  UNDER TEXT-018.

  AT END OF PSPID(7).
     SUM.
     PERFORM WRITE_VERT.
     WRITE: 12 SY-ULINE(120).
     PERFORM WRITE_VERT.
     WRITE: TEXT-019       UNDER TEXT-013,
            ANGTAB-WBS_AMT UNDER TEXT-015,
            ANGTAB-CAP_AMT  UNDER TEXT-018.
     ULINE.
  ENDAT.


  AT END OF ASSETCL.
     SUM.
     PERFORM WRITE_VERT.
     WRITE: TEXT-041       UNDER TEXT-012.
     WRITE: ANGTAB-WBS_AMT UNDER TEXT-015,
            ANGTAB-CAP_AMT  UNDER TEXT-018.
     ULINE.
  ENDAT.

  AT LAST.
     SUM.
     ULINE.
     PERFORM WRITE_VERT.                          "Grand Total for A & G
     WRITE: TEXT-023 UNDER TEXT-012,
            ANGTAB-WBS_AMT  UNDER TEXT-015,
            ANGTAB-CAP_AMT  UNDER TEXT-018.
     ULINE.
  ENDAT.
ENDLOOP.
ENDFORM.


*------------------------ WRITE_VERT -----------------------------------
FORM WRITE_VERT.
WRITE: /1 SY-VLINE, 12 SY-VLINE, 60 SY-VLINE, 85 SY-VLINE,
      109 SY-VLINE, 132 SY-VLINE.
ENDFORM.

FORM TOTAL_PAGE.
  CLEAR: TEXTPCT, PCT_AMT.
  NEW-PAGE.
  PERFORM TOTAL_DETAIL USING 'DISTRIBUTION'
                        DST_PCT DST_AMT DST_CAP_AMT DST_WBS DST_NON_AMT.
  PERFORM TOTAL_DETAIL USING 'GSO         '
                        GSO_PCT GSO_AMT GSO_CAP_AMT GSO_WBS GSO_NON_AMT.
  PERFORM TOTAL_DETAIL USING 'NGV         '
                        NGV_PCT NGV_AMT NGV_CAP_AMT NGV_WBS NGV_NON_AMT.
  PERFORM TOTAL_DETAIL USING 'A&G         '
                        ANG_PCT ANG_AMT ANG_CAP_AMT ANG_WBS ANG_NON_AMT.
ENDFORM.

FORM TOTAL_DETAIL USING DESC PCT_AMT AMT1 AMT2 AMT3 AMT4.

  PERFORM PRINT_VERT1.
  WRITE: 10 DESC.                                "Group Title
  PERFORM PRINT_VERT1.
  WRITE: 20 TEXT-028, 35 PCT_AMT.                "Percentage
  PERFORM PRINT_VERT1.
  WRITE: 20 TEXT-015, 35 AMT1.                   "WBS Amount
  PERFORM PRINT_VERT1.
  WRITE: 20 TEXT-018, 35 AMT2.                   "Amt Capitalized
  PERFORM PRINT_VERT1.
  WRITE: 20 TEXT-029, 35 AMT4.                   "Amt Not Capitalized
  PERFORM PRINT_VERT1.
  WRITE: 20 TEXT-030, 35 AMT3.                   "Total Overheads
  ULINE.

ENDFORM.

FORM PRINT_VERT1.
 WRITE: /1 SY-VLINE, 132 SY-VLINE.
ENDFORM.

FORM CREATE_BDC_SESSION.

IF P_RPT <> 'X'.
   PERFORM OPEN_BATCH_SESSION.
ENDIF.

LOOP AT DSTTAB.
  IF DSTTAB-ANLN1(1) CO '12'.
  ELSE.
     TABLEWD-ANLN1   = DSTTAB-ANLN1.
     TABLEWD-ANLN2   = DSTTAB-ANLN2.
*    tablewd-wbs_amt = dsttab-wbs_amt.
     TABLEWD-CAP_AMT = DSTTAB-CAP_AMT.
     COLLECT TABLEWD.
  ENDIF.
ENDLOOP.

LOOP AT GSOTAB.
  IF GSOTAB-ANLN1(1) CO '12'.
  ELSE.
     TABLEWD-ANLN1   = GSOTAB-ANLN1.
     TABLEWD-ANLN2   = GSOTAB-ANLN2.
*    tablewd-wbs_amt = gsotab-wbs_amt.
     TABLEWD-CAP_AMT = GSOTAB-CAP_AMT.
     COLLECT TABLEWD.
  ENDIF.
ENDLOOP.

LOOP AT NGVTAB.
  IF NGVTAB-ANLN1(1) CO '12'.
  ELSE.
     TABLEWD-ANLN1   = NGVTAB-ANLN1.
     TABLEWD-ANLN2   = NGVTAB-ANLN2.
*    tablewd-wbs_amt = ngvtab-wbs_amt.
     TABLEWD-CAP_AMT = NGVTAB-CAP_AMT.
     COLLECT TABLEWD.
  ENDIF.
ENDLOOP.

LOOP AT ANGTAB.
  IF ANGTAB-ANLN1(1) CO '12'.
  ELSE.
     TABLEWD-ANLN1   = ANGTAB-ANLN1.
     TABLEWD-ANLN2   = ANGTAB-ANLN2.
     TABLEWD-WBS_AMT = ANGTAB-WBS_AMT.
     TABLEWD-CAP_AMT = ANGTAB-CAP_AMT.
     COLLECT TABLEWD.
  ENDIF.
ENDLOOP.

SORT TABLEWD BY ANLN1 ANLN2.               "Summary by Asset Class/Asset
NEW-PAGE.
LOOP AT TABLEWD.
  PERFORM WRITE_VERT.
  WRITE: TABLEWD-ANLN1   UNDER TEXT-016,
         TABLEWD-ANLN2   UNDER TEXT-017,
         TABLEWD-WBS_AMT UNDER TEXT-015,
         TABLEWD-CAP_AMT UNDER TEXT-018.
  IF P_RPT <> 'X'.                                     "if BDC required
     IF TABLEWD-CAP_AMT <> 0.                          "Issue Log 750
        PERFORM POST_DISBURSE_AMT.
     ENDIF.
  ENDIF.

  AT END OF ANLN1(5).
     SUM.
     ULINE.
     PERFORM WRITE_VERT.
     WRITE: TABLEWD-ANLN1(5) UNDER TEXT-016,
            TABLEWD-WBS_AMT  UNDER TEXT-015,
            TABLEWD-CAP_AMT  UNDER TEXT-018.
     ULINE.
 ENDAT.

  AT LAST.
     SUM.
     ULINE.
     PERFORM WRITE_VERT.
     WRITE:
            TABLEWD-WBS_AMT  UNDER TEXT-015,
            TABLEWD-CAP_AMT  UNDER TEXT-018.
     ULINE.
     IF P_RPT <> 'X'.
        PERFORM CLOSE_BATCH_SESSION.
     ENDIF.
 ENDAT.

ENDLOOP.
ENDFORM.

*-----------------------  open_batch_session  --------------------------
*  Opens BDC session
*-----------------------------------------------------------------------

FORM OPEN_BATCH_SESSION.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT            = SY-MANDT
            GROUP             = 'ZAM_OVERHEAD'
            KEEP              = 'X'
            USER              = SY-UNAME
       EXCEPTIONS
            GROUP_INVALID     = 1
            GROUP_IS_LOCKED   = 2
            HOLDDATE_INVALID  = 3
            INTERNAL_ERROR    = 4
            QUEUE_ERROR       = 5
            RUNNING           = 6
            SYSTEM_LOCK_ERROR = 7
            USER_INVALID      = 8.
  IF SY-SUBRC <> 0.
    MESSAGE E699 WITH 'Could not open BDC session'.
  ENDIF.
  TBLDAT = SY-DATUM.
  CONCATENATE P_YEAR '1231' INTO TBUDAT.
  WRITE TBLDAT TO ZBLDAT DD/MM/YYYY.
  WRITE TBUDAT TO ZBUDAT DD/MM/YYYY.
ENDFORM.

*-------------------------  POST_DISBURSEMENT_AMT  ---------------------
*  Listed below is the subroutine which creates the update transactions
*  to post the disbursed overhead amounts to the sub-asset to which the
*  WBS element points via the mapping rules.
*-----------------------------------------------------------------------

FORM POST_DISBURSE_AMT.

 IF TABLEWD-CAP_AMT < 0.
    BDC_AMT = TABLEWD-CAP_AMT * -1.
    TRX_TYP = '101'.
 ELSE.
    BDC_AMT = TABLEWD-CAP_AMT.
    TRX_TYP = '100'.
 ENDIF.

  PERFORM SCREEN_HEADER USING 'SAPMA01B'     '100' 'X'.
  PERFORM SCREEN_FIELD  USING 'ANBZ-BUKRS'   P_COMP.
  PERFORM SCREEN_FIELD  USING 'ANBZ-ANLN1'   TABLEWD-ANLN1.
  PERFORM SCREEN_FIELD  USING 'ANBZ-ANLN2'   TABLEWD-ANLN2.
  PERFORM SCREEN_FIELD  USING 'ANEK-BLDAT'   ZBLDAT.
  PERFORM SCREEN_FIELD  USING 'ANEK-BUDAT'   ZBUDAT.
  PERFORM SCREEN_FIELD  USING 'ANBZ-PERID'   '13'.
  PERFORM SCREEN_FIELD  USING 'ANBZ-BWASL'   TRX_TYP.

  PERFORM SCREEN_HEADER USING 'SAPMA01B'     '110' 'X'.
  PERFORM SCREEN_FIELD  USING 'ANBZ-DMBTR'   BDC_AMT.
  IF P_AVD <> ''.                                         "Issue log 840
     PERFORM SCREEN_FIELD  USING 'ANBZ-BZDAT'   P_AVD.    "Issue log 840
  ENDIF.                                                  "Issue log 840
  PERFORM SCREEN_FIELD  USING 'RA01B-GKONT'  P_GKONT.
  PERFORM SCREEN_FIELD  USING 'ANEK-SGTXT'   'Overheads capitalized'.

* PERFORM SCREEN_FIELD  USING 'BDC_OKCODE'   '/11'.      "F11 - post
  PERFORM SCREEN_FIELD  USING 'BDC_OKCODE'   'UPDA'.     "4.6 B changes

  PERFORM INSERT_SESSION.
  CLEAR BDCDATA.
  REFRESH BDCDATA.

ENDFORM.

FORM Screen_Header using program screen indicator.
    CLEAR BDCDATA.
    BDCDATA-PROGRAM             = PROGRAM.
    BDCDATA-DYNPRO              = SCREEN.
    BDCDATA-DYNBEGIN            = INDICATOR.
    APPEND BDCDATA.
ENDFORM.

FORM Screen_Field using fnam fval.
    CLEAR BDCDATA.
    BDCDATA-FNAM                = FNAM.
    BDCDATA-FVAL                = FVAL.
    APPEND BDCDATA.
ENDFORM.

FORM INSERT_SESSION.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
              TCODE          = 'ABZO'
      TABLES
              DYNPROTAB      = BDCDATA
              EXCEPTIONS
              INTERNAL_ERROR = 1
              NOT_OPEN       = 2
              QUEUE_ERROR    = 3
              TCODE_INVALID  = 4.

 IF SY-SUBRC <> 0.
   MESSAGE E699 WITH 'Could not insert to the BDC session'.
 ENDIF.
ENDFORM.

FORM CLOSE_BATCH_SESSION.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN
            QUEUE_ERROR.
  IF SY-SUBRC <> 0.
    MESSAGE E699 WITH 'Could not close the BDC session'.
  ENDIF.
ENDFORM.

*------------------------ TOP-OF-PAGE ----------------------------------
TOP-OF-PAGE.
WRITE: /1 TEXT-RPT, SY-REPID, 50 T001-BUTXT,
      100 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
       TEXT-TTL UNDER T001-BUTXT,
       TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.
WRITE: / TEXTPCT UNDER TEXT-RPT, (10) PCT_AMT,
         TEXT-011 UNDER T001-BUTXT, P_YEAR.
ULINE.
PERFORM WRITE_VERT.
PERFORM WRITE_VERT.
WRITE: 2 TEXT-012, 13 TEXT-013, 18 TEXT-014, 61 TEXT-015,
      86 TEXT-016, 103 TEXT-017, 110 TEXT-018.
PERFORM WRITE_VERT.
ULINE.

*--------------------  GET_AUCS  ---------------------------------------
* Select all AUC records from ANLK table with dollars
*-----------------------------------------------------------------------
FORM GET_AUCS.

 SELECT * FROM ANLC
   WHERE BUKRS = P_COMP
     AND GJAHR = P_YEAR
     AND ANLN1 LIKE '1%'.

   COMPUTE TEMP_AMT = ANLC-KANSW + ANLC-KINVZ + ANLC-INVZM +
                      ANLC-KAUFW + ANLC-AUFWB + ANLC-ANSWL.

   IF TEMP_AMT <> 0.
      SELECT SINGLE * FROM ANLK
         WHERE BUKRS = ANLC-BUKRS
           AND ANLN1 = ANLC-ANLN1
           AND ANLN2 = ANLC-ANLN2.

      CLEAR TABLEWC.
      TABLEWC-ANLN1 = ANLC-ANLN1.
      TABLEWC-ANLN2 = ANLC-ANLN2.
      TABLEWC-WBS_AMT = TEMP_AMT.
      IF SY-SUBRC = 0.
         CONCATENATE ANLK-OBART ANLK-OBJID INTO TABLEWC-OBJNR.
      ENDIF.
      COLLECT TABLEWC.
   ENDIF.
 ENDSELECT.                                    "End of ANLK


 SELECT OBJNR POSID POST1 FROM PRPS INTO TABLEWA  "All WBS
   WHERE VERNR IN S_VERNR
     AND BELKZ = 'X'.
   IF TABLEWA-POSID+5(2) CO '0123456789'.
      APPEND TABLEWA.
   ENDIF.
 ENDSELECT.

 SORT TABLEWA BY OBJNR.

LOOP AT TABLEWC.                                "Determine WBS
 READ TABLE TABLEWA WITH KEY OBJNR = TABLEWC-OBJNR BINARY SEARCH.
  IF SY-SUBRC = '0'.
     TABLEWC-PSPID = TABLEWA-POSID(7).
     TABLEWC-WBSEL = TABLEWA-POSID+7(4).
     TABLEWC-POSID = TABLEWA-POSID.
     TABLEWC-POST1 = TABLEWA-POST1.
     MODIFY TABLEWC.
  ELSE.
     DELETE TABLEWC.
  ENDIF.
ENDLOOP.
*-----------------------------------------------------------------------
* category tablewc entries on tables zohdp, zohgs, zohnp, zohag.
*-----------------------------------------------------------------------
LOOP AT TABLEWC.

  IF P_DST = 'X' OR P_ANG = 'X'.
     SELECT SINGLE * FROM ZOHDP
       WHERE WBSEL = TABLEWC-WBSEL.
       IF SY-SUBRC = '0'.
          IF P_DST = 'X'.
             TABLEWC-ASSETCL = 'AUCDST'.
             COLLECT TABLEWC INTO DSTTAB.
          ENDIF.
          IF P_ANG = 'X' AND P_DST = 'X'.       "99/09/28
             TABLEWC-ASSETCL = 'AUCA&G'.
             COLLECT TABLEWC INTO ANGTAB.
          ENDIF.
       ENDIF.
  ENDIF.

  IF P_GSO = 'X' OR P_ANG = 'X'.
     SELECT SINGLE * FROM ZOHGS
       WHERE WBSEL = TABLEWC-WBSEL.
       IF SY-SUBRC = '0'.
          IF P_GSO = 'X'.
             TABLEWC-ASSETCL = 'AUCGSO'.
             APPEND TABLEWC TO GSOTAB.
          ENDIF.
          IF P_ANG = 'X' AND P_GSO = 'X'.       "99/09/28
             TABLEWC-ASSETCL = 'AUCA&G'.
             APPEND TABLEWC TO ANGTAB.
          ENDIF.
       ENDIF.
  ENDIF.

  IF P_NGV = 'X' OR P_ANG = 'X'.
     SELECT SINGLE * FROM ZOHNP
       WHERE WBSEL = TABLEWC-WBSEL.
       IF SY-SUBRC = '0'.
          IF P_NGV = 'X'.
             TABLEWC-ASSETCL = 'AUCNGV'.
             APPEND TABLEWC TO NGVTAB.
          ENDIF.
          IF P_ANG = 'X' AND P_NGV = 'X'.      "99/09/28
             TABLEWC-ASSETCL = 'AUCA&G'.
             APPEND TABLEWC TO ANGTAB.
          ENDIF.
       ENDIF.
  ENDIF.

  IF P_ANG = 'X'.
     SELECT SINGLE * FROM ZOHAG
       WHERE WBSEL = TABLEWC-WBSEL.
       IF SY-SUBRC = '0'.
          TABLEWC-ASSETCL = 'AUCA&G'.
          APPEND TABLEWC TO ANGTAB.
       ENDIF.
  ENDIF.

ENDLOOP.

ENDFORM.

*-------------------------  GET_ASSETS  --------------------------------
* Accumulate all asset dollars into appropriate tables - DST,GSO,NGV,A&G
*-----------------------------------------------------------------------
FORM GET_ASSETS.

*                                              Issue 988-changes
SELECT ANEP~ANLN1 ANEP~ANLN2 ANEP~ANBTR
  INTO TABLE NEWTAB
  FROM  ( ( ANEP
             INNER JOIN ANLA
             ON ANLA~BUKRS = ANEP~BUKRS AND
                ANLA~ANLN1 = ANEP~ANLN1 AND
                ANLA~ANLN2 = ANEP~ANLN2 )
             INNER JOIN ANLZ
             ON ANLZ~BUKRS = ANEP~BUKRS AND
                ANLZ~ANLN1 = ANEP~ANLN1 AND
                ANLZ~ANLN2 = ANEP~ANLN2 )
  WHERE ANEP~BUKRS = P_COMP
   AND  ANEP~ANLN1 LIKE '4%'
   AND  ANEP~GJAHR  = P_YEAR
   AND  ANEP~BWASL IN S_BWASL
   AND  ANLA~XANLGR = SPACE
   AND  ANLZ~KOSTL  = P_KOSTL.

*  SELECT * FROM ANEP
*     WHERE BUKRS = P_COMP
*       AND ANLN1 LIKE '4%'
*       AND GJAHR = P_YEAR
*       AND BWASL IN S_BWASL.                          "Issue log 750
**      AND BWASL IN ('115', '331', '336').
**      AND XAWBT = ' '.                                     "2000/09/20
*
*   select single * from anla                                "2000/12/07
*      where anln1  = anep-anln1                             "2000/12/07
*        and anln2  = anep-anln2                             "2000/12/07
*        and xanlgr = space.                                 "2000/12/07
*   if sy-subrc = '0'.                                       "2000/12/07
*   SELECT SINGLE * FROM ANLZ
*     WHERE BUKRS = P_COMP
*       AND ANLN1 = ANEP-ANLN1
*       AND ANLN2 = ANEP-ANLN2
*       AND KOSTL = P_KOSTL.
*    IF SY-SUBRC = '0'.
*    if anep-anln1 in s_dstac.                                "99/06/24

*         replace anep-xxxx fields with newtab-xxxx fields    "Issue 988
LOOP AT NEWTAB.
     IF NEWTAB-ANLN1 IN S_DSTAC AND P_DST = 'X'.              "99/06/24
        DSTTAB-ANLN1   = NEWTAB-ANLN1.
        DSTTAB-ANLN2   = NEWTAB-ANLN2.
        DSTTAB-WBS_AMT = NEWTAB-ANBTR.
        DSTTAB-ASSETCL = NEWTAB-ANLN1(5).
        COLLECT DSTTAB.
     ENDIF.
*    if anep-anln1 in s_gsoac.                                "99/06/24
     IF NEWTAB-ANLN1 IN S_GSOAC AND P_GSO = 'X'.              "99/06/24
        GSOTAB-ANLN1   = NEWTAB-ANLN1.
        GSOTAB-ANLN2   = NEWTAB-ANLN2.
        GSOTAB-WBS_AMT = NEWTAB-ANBTR.
        GSOTAB-ASSETCL = NEWTAB-ANLN1(5).
        COLLECT GSOTAB.
     ENDIF.
*    if anep-anln1 in s_ngvac.                                "99/06/24
     IF NEWTAB-ANLN1 IN S_NGVAC AND P_NGV = 'X'.              "99/06/24
        NGVTAB-ANLN1   = NEWTAB-ANLN1.
        NGVTAB-ANLN2   = NEWTAB-ANLN2.
        NGVTAB-WBS_AMT = NEWTAB-ANBTR.
        NGVTAB-ASSETCL = NEWTAB-ANLN1(5).
        COLLECT NGVTAB.
     ENDIF.
*    if anep-anln1 in s_dstac or                              "99/06/24
*       anep-anln1 in s_gsoac or                              "99/06/24
*       anep-anln1 in s_ngvac or                              "99/06/24
*       anep-anln1 in s_angac.                                "99/06/24
     IF ( NEWTAB-ANLN1 IN S_DSTAC AND P_DST = 'X' ) OR        "99/06/24
        ( NEWTAB-ANLN1 IN S_GSOAC AND P_GSO = 'X' ) OR        "99/06/24
        ( NEWTAB-ANLN1 IN S_NGVAC AND P_NGV = 'X' ) OR        "99/06/24
        ( NEWTAB-ANLN1 IN S_ANGAC AND P_ANG = 'X' ).          "99/06/24
        ANGTAB-ANLN1   = NEWTAB-ANLN1.
        ANGTAB-ANLN2   = NEWTAB-ANLN2.
        ANGTAB-WBS_AMT = NEWTAB-ANBTR.
        ANGTAB-ASSETCL = NEWTAB-ANLN1(5).
        COLLECT ANGTAB.
     ENDIF.
*  ENDIF.                 "End of ANLZ IF test
*  endif.                 "End of ANLA IF test               "2000/12/07
*  ENDSELECT.
ENDLOOP.
ENDFORM.

*---------------------------  CALCULATE_TOTAL_AMTS  --------------------
FORM CALCULATE_TOTAL_AMTS.
* Total amount to be distributed
CLEAR WRK_AMT.
LOOP AT DSTTAB.
 WRK_AMT = WRK_AMT + DSTTAB-WBS_AMT.
ENDLOOP.
DST_AMT = WRK_AMT.

CLEAR WRK_AMT.
LOOP AT GSOTAB.
 WRK_AMT = WRK_AMT + GSOTAB-WBS_AMT.
ENDLOOP.
GSO_AMT = WRK_AMT.

CLEAR WRK_AMT.
LOOP AT NGVTAB.
 WRK_AMT = WRK_AMT + NGVTAB-WBS_AMT.
ENDLOOP.
NGV_AMT = WRK_AMT.

CLEAR WRK_AMT.
LOOP AT ANGTAB.
 WRK_AMT = WRK_AMT + ANGTAB-WBS_AMT.
ENDLOOP.
ANG_AMT = WRK_AMT.

ENDFORM.

*-----------------------   CALCULATE_DISBURSEMENTS  --------------------
* get the total costs for each overhead that is to be disbursed and
* calculate the percent that each WBS element is to absorb
*-----------------------------------------------------------------------
FORM CALCULATE_DISBURSEMENTS.

PERFORM GET_OVERHEAD_COSTS USING P_DSTWBS P_DST P_DSTAMT.
DST_WBS = WRK_AMT.
IF DST_AMT <> 0.
   DST_PCT = DST_WBS / DST_AMT.
ENDIF.

PERFORM GET_OVERHEAD_COSTS USING P_GSOWBS P_GSO P_GSOAMT.
GSO_WBS = WRK_AMT.
IF GSO_AMT <> 0.
   GSO_PCT = GSO_WBS / GSO_AMT.
ENDIF.

PERFORM GET_OVERHEAD_COSTS USING P_NGVWBS P_NGV P_NGVAMT.
NGV_WBS = WRK_AMT.
IF NGV_AMT <> 0.
   NGV_PCT = NGV_WBS / NGV_AMT.
ENDIF.

PERFORM GET_OVERHEAD_COSTS USING P_ANGWBS P_ANG P_ANGAMT.
ANG_WBS = WRK_AMT.
IF ANG_AMT <> 0.
   ANG_PCT = ANG_WBS / ANG_AMT.
ENDIF.

*-----------------------------------------------------------------------
* Divide the amounts to be capitalized into 2 groups:
* - Non-capitalized ==> AUC ==>    Not posted to Overheads
* - Capitalized     ==> Assets ==> Posted as Overheads
*-----------------------------------------------------------------------
LOOP AT DSTTAB.
  COMPUTE DSTTAB-CAP_AMT = DSTTAB-WBS_AMT / DST_AMT * DST_WBS.
  MODIFY DSTTAB.
  IF DSTTAB-ANLN1(1) CO '1'.
     DST_NON_AMT = DST_NON_AMT + DSTTAB-CAP_AMT.
  ELSE.
     DST_CAP_AMT = DST_CAP_AMT + DSTTAB-CAP_AMT.
  ENDIF.
ENDLOOP.

LOOP AT GSOTAB.
  COMPUTE GSOTAB-CAP_AMT = GSOTAB-WBS_AMT / GSO_AMT * GSO_WBS.
  MODIFY GSOTAB.
  IF GSOTAB-ANLN1(1) CO '12'.
     GSO_NON_AMT = GSO_NON_AMT + GSOTAB-CAP_AMT.
  ELSE.
     GSO_CAP_AMT = GSO_CAP_AMT + GSOTAB-CAP_AMT.
  ENDIF.
ENDLOOP.

LOOP AT NGVTAB.
  COMPUTE NGVTAB-CAP_AMT = NGVTAB-WBS_AMT / NGV_AMT * NGV_WBS.
  MODIFY NGVTAB.
  IF NGVTAB-ANLN1(1) CO '12'.
     NGV_NON_AMT = NGV_NON_AMT + NGVTAB-CAP_AMT.
  ELSE.
     NGV_CAP_AMT = NGV_CAP_AMT + NGVTAB-CAP_AMT.
  ENDIF.
ENDLOOP.

LOOP AT ANGTAB.
  COMPUTE ANGTAB-CAP_AMT = ANGTAB-WBS_AMT / ANG_AMT * ANG_WBS.
  MODIFY ANGTAB.
  IF ANGTAB-ANLN1(1) CO '12'.
     ANG_NON_AMT = ANG_NON_AMT + ANGTAB-CAP_AMT.
  ELSE.
     ANG_CAP_AMT = ANG_CAP_AMT + ANGTAB-CAP_AMT.
  ENDIF.
ENDLOOP.

*-----------------------------------------------------------------------
* These routines make adjustments for round-off error. (s/b pennies)
* An AUC is adjusted
*-----------------------------------------------------------------------
COMPUTE WRK_AMT = ( DST_NON_AMT + DST_CAP_AMT ) - DST_WBS. "Distribution
LOOP AT DSTTAB.
 IF WRK_AMT <> 0.
    IF ( DSTTAB-CAP_AMT > WRK_AMT AND DSTTAB-ANLN1(1) CO '12' ).
       COMPUTE DSTTAB-CAP_AMT = DSTTAB-CAP_AMT - WRK_AMT.
       MODIFY DSTTAB.
       COMPUTE DST_NON_AMT = DST_NON_AMT - WRK_AMT.
       WRK_AMT = 0.
    ENDIF.
 ENDIF.
ENDLOOP.

COMPUTE WRK_AMT = ( GSO_NON_AMT + GSO_CAP_AMT ) - GSO_WBS.         "GSO
LOOP AT GSOTAB.
 IF WRK_AMT <> 0.
    IF ( GSOTAB-CAP_AMT > WRK_AMT AND GSOTAB-ANLN1(1) CO '12' ).
       COMPUTE GSOTAB-CAP_AMT = GSOTAB-CAP_AMT - WRK_AMT.
       MODIFY GSOTAB.
       COMPUTE GSO_NON_AMT = GSO_NON_AMT - WRK_AMT.
       WRK_AMT = 0.
    ENDIF.
 ENDIF.
ENDLOOP.

COMPUTE WRK_AMT = ( NGV_NON_AMT + NGV_CAP_AMT ) - NGV_WBS.         "NGV
LOOP AT NGVTAB.
 IF WRK_AMT <> 0.
    IF ( NGVTAB-CAP_AMT > WRK_AMT AND NGVTAB-ANLN1(1) CO '12' ).
       COMPUTE NGVTAB-CAP_AMT = NGVTAB-CAP_AMT - WRK_AMT.
       MODIFY NGVTAB.
       COMPUTE NGV_NON_AMT = NGV_NON_AMT - WRK_AMT.
       WRK_AMT = 0.
    ENDIF.
 ENDIF.
ENDLOOP.

COMPUTE WRK_AMT = ( ANG_NON_AMT + ANG_CAP_AMT ) - ANG_WBS.        "A & G
LOOP AT ANGTAB.
 IF WRK_AMT <> 0.
    IF ( ANGTAB-CAP_AMT > WRK_AMT AND ANGTAB-ANLN1(1) CO '12' ).
       COMPUTE ANGTAB-CAP_AMT = ANGTAB-CAP_AMT - WRK_AMT.
       MODIFY ANGTAB.
       COMPUTE ANG_NON_AMT = ANG_NON_AMT - WRK_AMT.
       WRK_AMT = 0.
    ENDIF.
 ENDIF.
ENDLOOP.

ENDFORM.
