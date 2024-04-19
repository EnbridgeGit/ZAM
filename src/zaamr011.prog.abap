REPORT ZAAMR011 NO STANDARD PAGE HEADING LINE-SIZE 250 LINE-COUNT 65.
*
************************************************************************
*  AUTHOR:      MOHAMMAD KHAN.
*  DATE:        DECEMBER 2002.
*  Description:
*     - The purpose of this program is to produce ACTUAL MONTHLY RATE
*        BASE REPORT. It takes data from AM and PS modules. The process
*        starts from the AM system and then it goes to PS system to
*        accomplish the results. The basic logic is to get the dollars
*        in AM system and then determine that when these dollars were
*        spent in the PS system and what percentage was posted to
*        different assets. This program can optionally submit ZAAMR010
*        program  to print exception report. the exceptio report shows
*        the difference of dollars in both systems and also reports the
*        projects that are major and don't have AVD.
*        The transaction 100, 101, 115, 116, 331 and 336 are selected
*        from the AM system.
* Trans. 100 and 101: Are the overheades and are applied to each period
*        dollars of PS at the sub total line of Asset Class.
*
* Trans. 331 is for the money spent in the previous years. So the
*        dollars from PS are not taken from the current year. Instead,
*        it goes back one year and the dollars for that year are
*        compared with AM system dollars, if they are not matching
*        then it will go back one year more and add up the amout and
*        then it's comared with AM system dollars and so on till 1997.
*        Please note that when going back year by year, the percentage
*        of settlement rule is applied as well.
*        All the dollars for this transaction will be printed under
*        period 1 irrespective of that when they were spent, but this
*        rule doesn't apply to Major projects. For major projects all
*        money will be printed in the AVD month.
*
* Trans. 336, 115, 116. The dollars in these trans. are the current
*        year dollars that are printed after applying the settlement
*        rule percentage. If it's a major project, then all the amount
*        of periods which are equal to and less than the AVD month is
*        added and printed in the AVD month.
*        It goes normal for the months greater than AVD month.
*
***********************************************************************
*Changes:                                                             *
*                                                                     *
*Ticket: Date:       By:       Description:                           *
*39303  29/12/2013  M. Khan    Remove duplicate lines and amount. Also*
*                              include logic for Tolerance Limit $.   *
*                                                                     *
*                                                                     *
*                                                                     *
***********************************************************************
*
* DB-Table
TABLES:   ANEP,               "Asset line item
          AUAK,               "Document header for settlement
          COBRA,              "Settlement rules
          COBRB,              "Distribution rules settlement rule
          T001,               "Company Code
          PRPS,               "WBS Element master data
          COSP,               "CO object: Primary cost
          COSS,               "CO object: Secondary cost
          ANKT,               "Asset class description
          PRI_PARAMS.

FIELD-SYMBOLS: <FS1>, <FS2>.
DATA: FISCAL_YEAR   LIKE COSP-GJAHR,

      TOTAMT        LIKE COSP-WKG001,
      WORK_DOLLAR   LIKE COSP-WKG001,
      G_ATINN       LIKE CABN-ATINN,
      G_ATINN_MP    LIKE CABN-ATINN,
      OBJECT        LIKE AUSP-OBJEK,
      CHARIC        LIKE CABN-ATNAM,
      NEW_OBJNR     LIKE PRPS-OBJNR,
      DOLLAR331     LIKE COSP-WKG001,
      LEN           TYPE P VALUE 11,
      ACTEXT(50)    TYPE C,
      MAJOR_PROJ(6) TYPE C,
      SMRY_FLAG     TYPE C VALUE 'N',
      ONE_TIME_HEAD TYPE C VALUE 'N',
      WRK_PERIOD(2) TYPE N,
      KOUNT(2)      TYPE N,
      STATUS(40)    TYPE C,                "All valid status of object
      WRK_TEXT(25)  TYPE C,
      WRK_TOTAL     LIKE COSS-WKG001,
      WRK_REMARKS(2) TYPE C,
      PARAMS        LIKE PRI_PARAMS,
      PREV_ANLKL    LIKE ANKT-ANLKL.

* Internal Tables
DATA:     BEGIN OF REPTAB OCCURS 1000,                  " report data
            ANLKL       LIKE ANKT-ANLKL,                " asset class
            BWASL       LIKE ANEP-BWASL,                " trans. type
            PROJ(11)    TYPE C,                         " proj. number
            STATUS(5)   TYPE C,                         " status
            BZDAT       LIKE COBRA-BZDAT,               " inservice date
            MAJOR       TYPE C,                         " major project
            WKG001      LIKE COSS-WKG001,               " rpt amt per 01
            WKG002      LIKE COSS-WKG002,               " rpt amt per 02
            WKG003      LIKE COSS-WKG003,               " rpt amt per 03
            WKG004      LIKE COSS-WKG004,               " rpt amt per 04
            WKG005      LIKE COSS-WKG005,               " rpt amt per 05
            WKG006      LIKE COSS-WKG006,               " rpt amt per 06
            WKG007      LIKE COSS-WKG007,               " rpt amt per 07
            WKG008      LIKE COSS-WKG008,               " rpt amt per 08
            WKG009      LIKE COSS-WKG009,               " rpt amt per 09
            WKG010      LIKE COSS-WKG010,               " rpt amt per 10
            WKG011      LIKE COSS-WKG011,               " rpt amt per 11
            WKG012      LIKE COSS-WKG012,               " rpt amt per 12
            WKGTOT      LIKE COSS-WKG012,               " rpt amt total
            AM$         LIKE ANEP-ANBTR,                " rpt amt - AM
            REMARKS(2)  TYPE C,                         " Remarks ($, A)
          END OF REPTAB.

DATA:     BEGIN OF SMRYTAB OCCURS 0,                    " report summary
            ANLKL       LIKE ZPWBS-ANLKL,               " asset class
            WKG001      LIKE COSS-WKG001,               " rpt amt per 01
            WKG002      LIKE COSS-WKG002,               " rpt amt per 02
            WKG003      LIKE COSS-WKG003,               " rpt amt per 03
            WKG004      LIKE COSS-WKG004,               " rpt amt per 04
            WKG005      LIKE COSS-WKG005,               " rpt amt per 05
            WKG006      LIKE COSS-WKG006,               " rpt amt per 06
            WKG007      LIKE COSS-WKG007,               " rpt amt per 07
            WKG008      LIKE COSS-WKG008,               " rpt amt per 08
            WKG009      LIKE COSS-WKG009,               " rpt amt per 09
            WKG010      LIKE COSS-WKG010,               " rpt amt per 10
            WKG011      LIKE COSS-WKG011,               " rpt amt per 11
            WKG012      LIKE COSS-WKG012,               " rpt amt per 12
            WKGTOT      LIKE COSS-WKG012,               " rpt amt total
          END OF SMRYTAB.

DATA:     BEGIN OF OHTAB OCCURS 0,                      " Overhead data
            ASTCLS      LIKE ANKT-ANLKL,                " asset class
            OHEAD$      LIKE ANEP-ANBTR,                " overhead $
          END OF OHTAB.

DATA:     BEGIN OF AMTAB OCCURS 0,                      " AM data
*           ANLN1       LIKE ANEP-ANLN1,        " asset number SDP39303
            ANLKL       LIKE ANKT-ANLKL,        " asset class  SDP39303
            BWASL       LIKE ANEP-BWASL,                " trans. code
            OBJNR       LIKE AUAK-OBJNR,                " object number
*            ANLN2       LIKE ANEP-ANLN2,       " sub asset no SDP39303
            POSKI       LIKE PRPS-POSKI,                " wbs
            AM$         LIKE ANEP-ANBTR,                " $ AM
          END OF AMTAB.

DATA: BEGIN OF STRUCAA,
            WKG001      LIKE COSP-WKG001,
            WKG002      LIKE COSP-WKG001,
            WKG003      LIKE COSP-WKG001,
            WKG004      LIKE COSP-WKG001,
            WKG005      LIKE COSP-WKG001,
            WKG006      LIKE COSP-WKG001,
            WKG007      LIKE COSP-WKG001,
            WKG008      LIKE COSP-WKG001,
            WKG009      LIKE COSP-WKG001,
            WKG010      LIKE COSP-WKG001,
            WKG011      LIKE COSP-WKG001,
            WKG012      LIKE COSP-WKG001,
      END OF STRUCAA.

DATA: BEGIN OF STRUCBB.
       INCLUDE STRUCTURE STRUCAA.
DATA: END OF STRUCBB.

DATA: BEGIN OF PRPSTAB OCCURS 0,                  "PRPS DATA
        OBJNR         LIKE PRPS-OBJNR,            "Object Number
        POSKI         LIKE PRPS-POSKI,            "WBS
        STUFE         LIKE PRPS-STUFE,            "WBS Level
      END OF PRPSTAB.

DATA:     BEGIN OF MINITAB OCCURS 0,
            FYEAR       LIKE COSP-GJAHR,
            WKG001      LIKE COSP-WKG001,
            WKG002      LIKE COSP-WKG001,
            WKG003      LIKE COSP-WKG001,
            WKG004      LIKE COSP-WKG001,
            WKG005      LIKE COSP-WKG001,
            WKG006      LIKE COSP-WKG001,
            WKG007      LIKE COSP-WKG001,
            WKG008      LIKE COSP-WKG001,
            WKG009      LIKE COSP-WKG001,
            WKG010      LIKE COSP-WKG001,
            WKG011      LIKE COSP-WKG001,
            WKG012      LIKE COSP-WKG001,
            TOTAL$      LIKE COSP-WKG001,
          END OF MINITAB.

DATA: BEGIN OF CHAR_TAB OCCURS 0.
        INCLUDE STRUCTURE AUSP.
DATA: END OF CHAR_TAB.


* Report Selections
SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(53) TEXT-014.
PARAMETERS:    P_FULL  RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(53) TEXT-012.
PARAMETERS:    P_EXCP  RADIOBUTTON GROUP SGRP.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN: COMMENT 1(53) TEXT-016.
PARAMETERS:    P_REPT  RADIOBUTTON GROUP SGRP DEFAULT 'X'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME.
PARAMETERS: P_REPTTL(50)  DEFAULT 'ACTUAL MONTHLY RATE BASE REPORT '.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME.
   PARAMETERS:     P_BUKRS  LIKE ANLA-BUKRS DEFAULT 'UGL',
                   P_YEAR   LIKE COSP-GJAHR.
   SELECT-OPTIONS: S_ANLN1  FOR  ANEP-ANLN1.
       PARAMETERS:     P_TOLRNS  TYPE I.        "SDP39303
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
   SELECT-OPTIONS S_POSKI   FOR PRPS-POSKI.
   SELECTION-SCREEN SKIP 1.
   SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN: COMMENT 1(63) TEXT-064.
   SELECTION-SCREEN END OF LINE.
   SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN: COMMENT 1(60) TEXT-065.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN BEGIN OF BLOCK BOX5 WITH FRAME.
   SELECT-OPTIONS S_KSTAR     FOR COSP-KSTAR
                              DEFAULT '491001' TO '491002'.
SELECTION-SCREEN END OF BLOCK BOX5.

SELECTION-SCREEN BEGIN OF BLOCK BOX6 WITH FRAME TITLE TEXT-067.
PARAMETERS:     P_RPRT RADIOBUTTON GROUP RBCR,            "PRINT REPORT
                P_FILE RADIOBUTTON GROUP RBCR.              "EXCEL FILE
SELECTION-SCREEN END OF BLOCK BOX6.

SELECTION-SCREEN END OF BLOCK BOX.

*----------------------------------------------------------------------

AT SELECTION-SCREEN ON P_BUKRS.
  SELECT SINGLE * FROM T001                        "Get Company Name
    WHERE BUKRS = P_BUKRS.

START-OF-SELECTION.
IF P_REPT <> 'X'.
   IF SY-BATCH = 'X'.
      CALL FUNCTION 'GET_PRINT_PARAMETERS'
      EXPORTING
         COPIES                 = 1
         DEPARTMENT             = 'System'
         DESTINATION            = 'CHEY'
         LINE_COUNT             = 65
         LINE_SIZE              = 94
         LIST_NAME              = 'TEST'
         LIST_TEXT              = 'SUBMIT TO SAP SPOOL'
         IMMEDIATELY            = ' '
         NEW_LIST_ID            = 'X'
         EXPIRATION             = 2
         NO_DIALOG              = 'X'
         RECEIVER               = 'SAP*'
         RELEASE                = 'X'
     IMPORTING
         OUT_PARAMETERS         = PARAMS.

   SUBMIT ZAAMR010 TO SAP-SPOOL                 "Submit Exception Report
          SPOOL PARAMETERS PARAMS
          WITHOUT SPOOL DYNPRO
     WITH  P_FULL  = P_FULL
     WITH  P_EXCP  = P_EXCP
     WITH  P_BUKRS = P_BUKRS
     WITH  P_YEAR  = P_YEAR
     WITH  S_ANLN1 IN S_ANLN1
     WITH  S_KSTAR IN S_KSTAR
     AND   RETURN.
     STOP.
ELSE.
   SUBMIT ZAAMR010                              "Submit Exception Report
     WITH  P_FULL  = P_FULL
     WITH  P_EXCP  = P_EXCP
     WITH  P_BUKRS = P_BUKRS
     WITH  P_YEAR  = P_YEAR
     WITH  S_ANLN1 IN S_ANLN1
     WITH  S_KSTAR IN S_KSTAR
     AND   RETURN.
     STOP.
ENDIF.
ENDIF.

  SELECT * FROM ANEP
   WHERE BUKRS = P_BUKRS
     AND ANLN1 IN S_ANLN1
     AND GJAHR = P_YEAR
     AND BWASL IN ('100', '101', '115', '116', '331', '336').

  IF SY-SUBRC = 0.
     IF ANEP-BWASL = '100' OR ANEP-BWASL = '101'.
        PERFORM PROCES_TRANSACTION_OVHEAD.
     ELSE.
        SELECT SINGLE * FROM AUAK
        WHERE BELNR = ANEP-BELNR.
        IF SY-SUBRC = 0.
           MOVE AUAK-OBJNR  TO  AMTAB-OBJNR.
*           MOVE ANEP-ANLN1  TO  AMTAB-ANLN1.                 "SDP39303
          CONCATENATE '000' ANEP-ANLN1+0(5) INTO AMTAB-ANLKL. "SDP39303
*          MOVE ANEP-ANLN2  TO  AMTAB-ANLN2.                  "SDP39303
           MOVE ANEP-ANBTR  TO  AMTAB-AM$.
           IF  ANEP-BWASL = '115'.
               MOVE '116' TO AMTAB-BWASL.
           ELSE.
               MOVE ANEP-BWASL  TO  AMTAB-BWASL.
           ENDIF.
           COLLECT AMTAB.
        ENDIF.
      ENDIF.
   ENDIF.
   ENDSELECT.

IF AMTAB[] IS INITIAL.
   WRITE: /1 'NO DATA SELECTED'.
   STOP.
ENDIF.
*   SORT AMTAB BY ANLN1 BWASL OBJNR.            "SDP39303
   SORT AMTAB BY ANLKL BWASL OBJNR.             "SDP39303
   PERFORM LOAD_PRPS_DATA.
   PERFORM BUILD_AUSP.
   LOOP AT AMTAB.
*        CONCATENATE '000' AMTAB-ANLN1+0(5) INTO REPTAB-ANLKL. "SDP39303
        MOVE AMTAB-ANLKL TO REPTAB-ANLKL.                      "SDP39303
        MOVE AMTAB-BWASL      TO REPTAB-BWASL.
        MOVE AMTAB-AM$        TO REPTAB-AM$.
        PERFORM GET_STATUS.
        IF AMTAB-BWASL = '331'.
           PERFORM PROCES_TRANSACTION_331.
        ELSE.
           PERFORM PROCES_TRANSACTION_OTHERS.
        ENDIF.
   ENDLOOP.
   SORT REPTAB BY ANLKL BWASL PROJ.
   SORT OHTAB  BY ASTCLS.

   IF NOT S_POSKI IS INITIAL.
      LOOP AT REPTAB.
           IF NOT REPTAB-PROJ+0(7) IN S_POSKI.
              DELETE REPTAB.
           ENDIF.
      ENDLOOP.
   ENDIF.

   LOOP AT REPTAB.
        AT NEW ANLKL.
           MOVE REPTAB-ANLKL+3(5) TO PREV_ANLKL.
           PERFORM GET_ASSET_CLASS_TEXT.
           IF P_RPRT = 'X'.
              NEW-PAGE.
           ENDIF.
           ENDAT.

        PERFORM WRITE_DETAIL_LINE.

        AT END OF ANLKL.
           SUM.
           PERFORM WRITE_TOTAL_LINE.
           IF S_POSKI IS INITIAL.
              PERFORM WRITE_OVERHEAD_LINE.
           ENDIF.
           IF P_RPRT = 'X'.
              ULINE.
              SKIP 1.
           ENDIF.
        ENDAT.

   ENDLOOP.

   CLEAR ACTEXT.
   CONCATENATE TEXT-063 P_REPTTL INTO P_REPTTL SEPARATED BY SPACE.
   MOVE 'Y' TO SMRY_FLAG.
   IF S_POSKI IS INITIAL.
      NEW-PAGE.
      PERFORM WRITE_SUMMARY_LINE.
   ENDIF.
*---------------------------------------------------------------------*
*                         TOP-OF-PAGE                                 *
*---------------------------------------------------------------------*
TOP-OF-PAGE.
IF ONE_TIME_HEAD = 'Y'  AND SMRY_FLAG <> 'Y'.
ELSE.
  WRITE: /1 TEXT-RPT, SY-REPID,                           "Report Id
         107 T001-BUTXT,                                  "Company Name
         215 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.      "Date/Time
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT, SY-SYSID,   "Client
         100 P_REPTTL,                                    "Report Title
             TEXT-PGE UNDER TEXT-DTE, SY-PAGNO.           "Page Number
IF SMRY_FLAG = 'Y'.
  WRITE: / TEXT-021 UNDER T001-BUTXT, P_YEAR.             "Fiscal year
ELSEIF P_FILE = 'X'.
  WRITE: / TEXT-021 UNDER T001-BUTXT, P_YEAR.             "Fiscal year
  MOVE 'Y' TO ONE_TIME_HEAD.
ELSE.
  WRITE: / TEXT-023 UNDER TEXT-RPT, REPTAB-ANLKL, ACTEXT, "Asset Class
           TEXT-021 UNDER T001-BUTXT, P_YEAR.             "Fiscal year
ENDIF.
  ULINE.
  WRITE: /1 TEXT-025, 7   TEXT-027, 23  TEXT-028,  32 TEXT-029,
         45 TEXT-031, 53  TEXT-033, 67  TEXT-035,  81 TEXT-037,
         95 TEXT-039, 109  TEXT-041, 123 TEXT-043, 137 TEXT-045,
        151 TEXT-046, 165 TEXT-047, 179 TEXT-049, 193 TEXT-051,
        207 TEXT-053, 231 TEXT-055.
  ULINE.
ENDIF.
*---------------------------------------------------------------------*
*                      WRITE_DETAIL_LINE                              *
*---------------------------------------------------------------------*
FORM WRITE_DETAIL_LINE.
     DATA: WRK_PROJ(14)   TYPE C,
           ABSAM$(16)     TYPE N,           "SDP39303
           ABSTOTAL(13)   TYPE N,           "SDP39303
           REPDIFF(13)    TYPE N.           "SDP39303

     ADD REPTAB-WKG001 FROM 1 TO 12 GIVING WRK_TOTAL.

        ABSAM$ = ABS( REPTAB-AM$ ).         "SDP39303
        ABSTOTAL = ABS( WRK_TOTAL ).        "SDP39303
     IF REPTAB-REMARKS = '  ' AND
*        REPTAB-AM$     <> WRK_TOTAL.                    "SDP39303
        ABSAM$ <> ABSTOTAL.
        REPDIFF = ABSTOTAL - ABSAM$.          "SDP39303
        IF ABS( REPDIFF ) > ABS( P_TOLRNS ).  "SDP39303
           MOVE '*$' TO WRK_REMARKS.
        ENDIF.                                "SDP39303
     ELSE.
        MOVE REPTAB-REMARKS TO WRK_REMARKS.
     ENDIF.
     CONCATENATE REPTAB-PROJ+0(2) REPTAB-PROJ+2(2) REPTAB-PROJ+4(3)
                 REPTAB-PROJ+7(4) INTO WRK_PROJ SEPARATED BY SPACE.
     WRITE: / REPTAB-BWASL  UNDER TEXT-025,
              WRK_PROJ   UNDER TEXT-027,
              REPTAB-STATUS UNDER TEXT-028,
              REPTAB-BZDAT  UNDER TEXT-029,
              REPTAB-MAJOR  UNDER TEXT-031.
     WRITE AT (LEN): REPTAB-WKG001 UNDER TEXT-033 DECIMALS 0,
              REPTAB-WKG002 UNDER TEXT-035 DECIMALS 0,
              REPTAB-WKG003 UNDER TEXT-037 DECIMALS 0,
              REPTAB-WKG004 UNDER TEXT-039 DECIMALS 0,
              REPTAB-WKG005 UNDER TEXT-041 DECIMALS 0,
              REPTAB-WKG006 UNDER TEXT-043 DECIMALS 0,
              REPTAB-WKG007 UNDER TEXT-045 DECIMALS 0,
              REPTAB-WKG008 UNDER TEXT-046 DECIMALS 0,
              REPTAB-WKG009 UNDER TEXT-047 DECIMALS 0,
              REPTAB-WKG010 UNDER TEXT-049 DECIMALS 0,
              REPTAB-WKG011 UNDER TEXT-051 DECIMALS 0,
              REPTAB-WKG012 UNDER TEXT-053 DECIMALS 0,
*             WRK_TOTAL UNDER TEXT-055 USING EDIT MASK 'LLV___________'.
              WRK_TOTAL     UNDER TEXT-055 DECIMALS 0.
  IF P_FILE = 'X'.
     WRITE: WRK_REMARKS, (5) REPTAB-ANLKL.
  ELSE.
     WRITE  WRK_REMARKS.
  ENDIF.
  CLEAR WRK_REMARKS.

ENDFORM.
*---------------------------------------------------------------------*
*                      WRITE_TOTAL_LINE                               *
*---------------------------------------------------------------------*
FORM WRITE_TOTAL_LINE.

    ADD REPTAB-WKG001 FROM 1 TO 12 GIVING WRK_TOTAL.
    WRITE: /1 TEXT-057.
    WRITE AT (LEN): REPTAB-WKG001 UNDER TEXT-033 DECIMALS 0,
              REPTAB-WKG002 UNDER TEXT-035 DECIMALS 0,
              REPTAB-WKG003 UNDER TEXT-037 DECIMALS 0,
              REPTAB-WKG004 UNDER TEXT-039 DECIMALS 0,
              REPTAB-WKG005 UNDER TEXT-041 DECIMALS 0,
              REPTAB-WKG006 UNDER TEXT-043 DECIMALS 0,
              REPTAB-WKG007 UNDER TEXT-045 DECIMALS 0,
              REPTAB-WKG008 UNDER TEXT-046 DECIMALS 0,
              REPTAB-WKG009 UNDER TEXT-047 DECIMALS 0,
              REPTAB-WKG010 UNDER TEXT-049 DECIMALS 0,
              REPTAB-WKG011 UNDER TEXT-051 DECIMALS 0,
              REPTAB-WKG012 UNDER TEXT-053 DECIMALS 0,
              WRK_TOTAL     UNDER TEXT-055 DECIMALS 0.
  IF P_FILE = 'X'.
*WRK_REMARKS will not print anything here, it's here as space holder.
     WRITE:   WRK_REMARKS, (5) REPTAB-ANLKL.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*                      WRITE_OVERHEAD_LINE                            *
*---------------------------------------------------------------------*
FORM WRITE_OVERHEAD_LINE.
CLEAR: OHTAB, STRUCAA.
READ TABLE OHTAB WITH KEY ASTCLS = PREV_ANLKL BINARY SEARCH.
IF SY-SUBRC = 0.
   IF WRK_TOTAL <> 0.
      IF REPTAB-WKG001 <> 0.
         STRUCAA-WKG001 = REPTAB-WKG001 / WRK_TOTAL * OHTAB-OHEAD$.
      ENDIF.
      IF REPTAB-WKG002 <> 0.
         STRUCAA-WKG002 = REPTAB-WKG002 / WRK_TOTAL * OHTAB-OHEAD$.
      ENDIF.
      IF REPTAB-WKG003 <> 0.
         STRUCAA-WKG003 = REPTAB-WKG003 / WRK_TOTAL * OHTAB-OHEAD$.
      ENDIF.
      IF REPTAB-WKG004 <> 0.
         STRUCAA-WKG004 = REPTAB-WKG004 / WRK_TOTAL * OHTAB-OHEAD$.
      ENDIF.
      IF REPTAB-WKG005 <> 0.
         STRUCAA-WKG005 = REPTAB-WKG005 / WRK_TOTAL * OHTAB-OHEAD$.
      ENDIF.
      IF REPTAB-WKG006 <> 0.
         STRUCAA-WKG006 = REPTAB-WKG006 / WRK_TOTAL * OHTAB-OHEAD$.
      ENDIF.
      IF REPTAB-WKG007 <> 0.
         STRUCAA-WKG007 = REPTAB-WKG007 / WRK_TOTAL * OHTAB-OHEAD$.
      ENDIF.
      IF REPTAB-WKG008 <> 0.
         STRUCAA-WKG008 = REPTAB-WKG008 / WRK_TOTAL * OHTAB-OHEAD$.
      ENDIF.
      IF REPTAB-WKG009 <> 0.
         STRUCAA-WKG009 = REPTAB-WKG009 / WRK_TOTAL * OHTAB-OHEAD$.
      ENDIF.
      IF REPTAB-WKG010 <> 0.
         STRUCAA-WKG010 = REPTAB-WKG010 / WRK_TOTAL * OHTAB-OHEAD$.
      ENDIF.
      IF REPTAB-WKG011 <> 0.
         STRUCAA-WKG011 = REPTAB-WKG011 / WRK_TOTAL * OHTAB-OHEAD$.
      ENDIF.
      IF REPTAB-WKG012 <> 0.
         STRUCAA-WKG012 = REPTAB-WKG012 / WRK_TOTAL * OHTAB-OHEAD$.
      ENDIF.
   ENDIF.
ENDIF.
   CLEAR WRK_TOTAL.
   ADD STRUCAA-WKG001 FROM 1 TO 12 GIVING WRK_TOTAL.
    WRITE: /1 TEXT-059.
    WRITE AT (LEN): STRUCAA-WKG001 UNDER TEXT-033 DECIMALS 0,
              STRUCAA-WKG002 UNDER TEXT-035 DECIMALS 0,
              STRUCAA-WKG003 UNDER TEXT-037 DECIMALS 0,
              STRUCAA-WKG004 UNDER TEXT-039 DECIMALS 0,
              STRUCAA-WKG005 UNDER TEXT-041 DECIMALS 0,
              STRUCAA-WKG006 UNDER TEXT-043 DECIMALS 0,
              STRUCAA-WKG007 UNDER TEXT-045 DECIMALS 0,
              STRUCAA-WKG008 UNDER TEXT-046 DECIMALS 0,
              STRUCAA-WKG009 UNDER TEXT-047 DECIMALS 0,
              STRUCAA-WKG010 UNDER TEXT-049 DECIMALS 0,
              STRUCAA-WKG011 UNDER TEXT-051 DECIMALS 0,
              STRUCAA-WKG012 UNDER TEXT-053 DECIMALS 0,
              WRK_TOTAL      UNDER TEXT-055 DECIMALS 0.
  IF P_FILE = 'X'.
*WRK_REMARKS will not print anything here, it's here as space holder.
     WRITE:   WRK_REMARKS, (5) REPTAB-ANLKL.
  ENDIF.

* CLASS TOTAL LINE
    CLEAR WRK_TOTAL.
    STRUCBB-WKG001 = REPTAB-WKG001 + STRUCAA-WKG001.
    STRUCBB-WKG002 = REPTAB-WKG002 + STRUCAA-WKG002.
    STRUCBB-WKG003 = REPTAB-WKG003 + STRUCAA-WKG003.
    STRUCBB-WKG004 = REPTAB-WKG004 + STRUCAA-WKG004.
    STRUCBB-WKG005 = REPTAB-WKG005 + STRUCAA-WKG005.
    STRUCBB-WKG006 = REPTAB-WKG006 + STRUCAA-WKG006.
    STRUCBB-WKG007 = REPTAB-WKG007 + STRUCAA-WKG007.
    STRUCBB-WKG008 = REPTAB-WKG008 + STRUCAA-WKG008.
    STRUCBB-WKG009 = REPTAB-WKG009 + STRUCAA-WKG009.
    STRUCBB-WKG010 = REPTAB-WKG010 + STRUCAA-WKG010.
    STRUCBB-WKG011 = REPTAB-WKG011 + STRUCAA-WKG011.
    STRUCBB-WKG012 = REPTAB-WKG012 + STRUCAA-WKG012.
    ADD STRUCBB-WKG001 FROM 1 TO 12 GIVING WRK_TOTAL.

 IF P_FILE = 'X'.
    WRITE: /1 TEXT-061, (5) REPTAB-ANLKL, ACTEXT+0(28).
 ELSE.
    WRITE: /1 TEXT-061.
 ENDIF.
*    WRITE: /1 TEXT-061.
    WRITE AT (LEN): STRUCBB-WKG001 UNDER TEXT-033 DECIMALS 0,
              STRUCBB-WKG002 UNDER TEXT-035 DECIMALS 0,
              STRUCBB-WKG003 UNDER TEXT-037 DECIMALS 0,
              STRUCBB-WKG004 UNDER TEXT-039 DECIMALS 0,
              STRUCBB-WKG005 UNDER TEXT-041 DECIMALS 0,
              STRUCBB-WKG006 UNDER TEXT-043 DECIMALS 0,
              STRUCBB-WKG007 UNDER TEXT-045 DECIMALS 0,
              STRUCBB-WKG008 UNDER TEXT-046 DECIMALS 0,
              STRUCBB-WKG009 UNDER TEXT-047 DECIMALS 0,
              STRUCBB-WKG010 UNDER TEXT-049 DECIMALS 0,
              STRUCBB-WKG011 UNDER TEXT-051 DECIMALS 0,
              STRUCBB-WKG012 UNDER TEXT-053 DECIMALS 0,
              WRK_TOTAL      UNDER TEXT-055 DECIMALS 0.
 IF P_FILE = 'X'.
*WRK_REMARKS will not print anything here, it's here as space holder.
     WRITE:   WRK_REMARKS, (5) REPTAB-ANLKL.
 ENDIF.
 MOVE-CORRESPONDING STRUCBB TO SMRYTAB.
 MOVE PREV_ANLKL   TO SMRYTAB-ANLKL.
 MOVE WRK_TOTAL    TO SMRYTAB-WKGTOT.
 APPEND SMRYTAB.
 CLEAR SMRYTAB.

ENDFORM.
*---------------------------------------------------------------------*
*                      WRITE_SUMMARY_LINE                             *
*---------------------------------------------------------------------*
FORM WRITE_SUMMARY_LINE.
 LOOP AT SMRYTAB.
     CLEAR WRK_TEXT.
     CONCATENATE 'Asset Class' SMRYTAB-ANLKL+0(5) INTO WRK_TEXT
                  SEPARATED BY SPACE.
     PERFORM WRITE_THE_DATA_LINE.
    AT LAST.
       SUM.
       MOVE 'TOTAL :' TO WRK_TEXT.
       ULINE.
       PERFORM WRITE_THE_DATA_LINE.
    ENDAT.
 ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*                      WRITE_THE_DATA_LINE                            *
*---------------------------------------------------------------------*
FORM WRITE_THE_DATA_LINE.
     WRITE: /1 WRK_TEXT.
     WRITE AT (LEN): SMRYTAB-WKG001 UNDER TEXT-033 DECIMALS 0,
              SMRYTAB-WKG002 UNDER TEXT-035 DECIMALS 0,
              SMRYTAB-WKG003 UNDER TEXT-037 DECIMALS 0,
              SMRYTAB-WKG004 UNDER TEXT-039 DECIMALS 0,
              SMRYTAB-WKG005 UNDER TEXT-041 DECIMALS 0,
              SMRYTAB-WKG006 UNDER TEXT-043 DECIMALS 0,
              SMRYTAB-WKG007 UNDER TEXT-045 DECIMALS 0,
              SMRYTAB-WKG008 UNDER TEXT-046 DECIMALS 0,
              SMRYTAB-WKG009 UNDER TEXT-047 DECIMALS 0,
              SMRYTAB-WKG010 UNDER TEXT-049 DECIMALS 0,
              SMRYTAB-WKG011 UNDER TEXT-051 DECIMALS 0,
              SMRYTAB-WKG012 UNDER TEXT-053 DECIMALS 0,
              SMRYTAB-WKGTOT UNDER TEXT-055 DECIMALS 0.
ENDFORM.
*---------------------------------------------------------------------*
*                      GET_ASSET_CLASS_TEXT                           *
*---------------------------------------------------------------------*
 FORM GET_ASSET_CLASS_TEXT.
   CLEAR ACTEXT.
   SELECT SINGLE TXK50 INTO ACTEXT
     FROM ANKT
    WHERE SPRAS = 'EN'
      AND ANLKL = REPTAB-ANLKL.
   IF SY-SUBRC <> 0.
      MOVE 'NOT FOUND ' TO ACTEXT.
   ENDIF.
 ENDFORM.
*---------------------------------------------------------------------*
*       FORM PERFORM PROCES_TRANSACTION_331                           *
*---------------------------------------------------------------------*
 FORM PROCES_TRANSACTION_331.
 DATA: GOBACK_NMBR(2) TYPE N.
    REFRESH MINITAB.
    CLEAR STRUCBB.
    FISCAL_YEAR = P_YEAR.
    GOBACK_NMBR = P_YEAR - 1997.
    DO GOBACK_NMBR TIMES.
       FISCAL_YEAR = FISCAL_YEAR - 1.
       CLEAR: TOTAMT, STRUCAA.
       PERFORM GET_PS_DOLLARS.
       ADD: STRUCAA-WKG001 TO STRUCBB-WKG001,
            STRUCAA-WKG002 TO STRUCBB-WKG002,
            STRUCAA-WKG003 TO STRUCBB-WKG003,
            STRUCAA-WKG004 TO STRUCBB-WKG004,
            STRUCAA-WKG005 TO STRUCBB-WKG005,
            STRUCAA-WKG006 TO STRUCBB-WKG006,
            STRUCAA-WKG007 TO STRUCBB-WKG007,
            STRUCAA-WKG008 TO STRUCBB-WKG008,
            STRUCAA-WKG009 TO STRUCBB-WKG009,
            STRUCAA-WKG010 TO STRUCBB-WKG010,
            STRUCAA-WKG011 TO STRUCBB-WKG011,
            STRUCAA-WKG012 TO STRUCBB-WKG012,
            TOTAMT TO DOLLAR331.
       IF  ABS( DOLLAR331 ) = ABS( AMTAB-AM$ ).    "SDP39303
           REFRESH MINITAB.
           CLEAR:  MINITAB, DOLLAR331.
           PERFORM BUILD_REPORT_TABLE.
           EXIT.
       ELSE.
           MOVE FISCAL_YEAR TO MINITAB-FYEAR.
           MOVE DOLLAR331 TO MINITAB-TOTAL$.
           MOVE-CORRESPONDING STRUCBB TO MINITAB.
           APPEND MINITAB.
           CLEAR  MINITAB.
       ENDIF.
    ENDDO.

    IF NOT MINITAB[] IS INITIAL.
    CLEAR: STRUCBB.
       LOOP AT MINITAB.
           MOVE MINITAB-TOTAL$ TO WORK_DOLLAR.
           MOVE-CORRESPONDING MINITAB TO STRUCBB.
           PERFORM APPLY_PERCENT.
           IF ABS( WORK_DOLLAR ) = ABS( AMTAB-AM$ ).     "SDP39303
              REFRESH MINITAB.
              PERFORM BUILD_REPORT_TABLE.
              CLEAR:  MINITAB, AMTAB, WORK_DOLLAR.
              REFRESH MINITAB.
              EXIT.
           ENDIF.
       ENDLOOP.
    ENDIF.

    IF NOT MINITAB[] IS INITIAL.
       PERFORM BUILD_REPORT_TABLE.
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
    MOVE-CORRESPONDING STRUCAA TO STRUCBB.
    IF ABS( TOTAMT ) <> ABS( AMTAB-AM$ ).
       MOVE TOTAMT  TO  WORK_DOLLAR.
       PERFORM APPLY_PERCENT.
    ENDIF.
    PERFORM BUILD_REPORT_TABLE.
    CLEAR: TOTAMT, WORK_DOLLAR.
 ENDFORM.

*---------------------------------------------------------------------*
*       FORM PERFORM PROCES_TRANSACTION_OVHEAD                        *
*---------------------------------------------------------------------*
FORM PROCES_TRANSACTION_OVHEAD.
 MOVE ANEP-ANLN1+0(5) TO OHTAB-ASTCLS.
 MOVE ANEP-ANBTR      TO OHTAB-OHEAD$.
 COLLECT OHTAB.
 CLEAR OHTAB.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM BUILD_REPORT_TABLE                                       *
*---------------------------------------------------------------------*
 FORM BUILD_REPORT_TABLE.
 DATA: WRK_AMT LIKE COSP-WKG001.
      PERFORM CHECK_MAJOR_PROJECT_INDICATOR.
      PERFORM CHECK_AVD.
      WRK_PERIOD = 1.
      IF REPTAB-MAJOR = 'Y' AND REPTAB-REMARKS = '  '.
         CLEAR: WRK_PERIOD.
         MOVE REPTAB-BZDAT+4(2) TO WRK_PERIOD.
         ASSIGN COMPONENT WRK_PERIOD OF STRUCTURE STRUCBB TO <FS1>.
         IF REPTAB-BWASL =  '331'.
            MOVE 12 TO WRK_PERIOD.
         ENDIF.
         CLEAR WRK_AMT.
         KOUNT = 0.
         WHILE KOUNT < WRK_PERIOD.
           KOUNT = KOUNT + 1.
           ASSIGN COMPONENT KOUNT OF STRUCTURE STRUCBB TO <FS2>.
           WRK_AMT = WRK_AMT + <FS2>.
           CLEAR <FS2>.
         ENDWHILE.
         MOVE WRK_AMT TO <FS1>.
      ENDIF.
   IF REPTAB-MAJOR = ' ' AND REPTAB-BWASL = '331'.
      ADD STRUCBB-WKG001 FROM 1 TO 12 GIVING REPTAB-WKG001.
   ELSE.
      MOVE: STRUCBB-WKG001 TO REPTAB-WKG001,
            STRUCBB-WKG002 TO REPTAB-WKG002,
            STRUCBB-WKG003 TO REPTAB-WKG003,
            STRUCBB-WKG004 TO REPTAB-WKG004,
            STRUCBB-WKG005 TO REPTAB-WKG005,
            STRUCBB-WKG006 TO REPTAB-WKG006,
            STRUCBB-WKG007 TO REPTAB-WKG007,
            STRUCBB-WKG008 TO REPTAB-WKG008,
            STRUCBB-WKG009 TO REPTAB-WKG009,
            STRUCBB-WKG010 TO REPTAB-WKG010,
            STRUCBB-WKG011 TO REPTAB-WKG011,
            STRUCBB-WKG012 TO REPTAB-WKG012.
   ENDIF.
      ADD STRUCBB-WKG001 FROM 1 TO 12 GIVING REPTAB-WKGTOT.

      APPEND REPTAB.
      CLEAR: STRUCAA, STRUCBB, REPTAB.
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
      STRUCAA-WKG001 = STRUCAA-WKG001 + COSP-WKG001.
      STRUCAA-WKG002 = STRUCAA-WKG002 + COSP-WKG002.
      STRUCAA-WKG003 = STRUCAA-WKG003 + COSP-WKG003.
      STRUCAA-WKG004 = STRUCAA-WKG004 + COSP-WKG004.
      STRUCAA-WKG005 = STRUCAA-WKG005 + COSP-WKG005.
      STRUCAA-WKG006 = STRUCAA-WKG006 + COSP-WKG006.
      STRUCAA-WKG007 = STRUCAA-WKG007 + COSP-WKG007.
      STRUCAA-WKG008 = STRUCAA-WKG008 + COSP-WKG008.
      STRUCAA-WKG009 = STRUCAA-WKG009 + COSP-WKG009.
      STRUCAA-WKG010 = STRUCAA-WKG010 + COSP-WKG010.
      STRUCAA-WKG011 = STRUCAA-WKG011 + COSP-WKG011.
      STRUCAA-WKG012 = STRUCAA-WKG012 + COSP-WKG012.
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
        STRUCAA-WKG001 = STRUCAA-WKG001 + COSS-WKG001.
        STRUCAA-WKG002 = STRUCAA-WKG002 + COSS-WKG002.
        STRUCAA-WKG003 = STRUCAA-WKG003 + COSS-WKG003.
        STRUCAA-WKG004 = STRUCAA-WKG004 + COSS-WKG004.
        STRUCAA-WKG005 = STRUCAA-WKG005 + COSS-WKG005.
        STRUCAA-WKG006 = STRUCAA-WKG006 + COSS-WKG006.
        STRUCAA-WKG007 = STRUCAA-WKG007 + COSS-WKG007.
        STRUCAA-WKG008 = STRUCAA-WKG008 + COSS-WKG008.
        STRUCAA-WKG009 = STRUCAA-WKG009 + COSS-WKG009.
        STRUCAA-WKG010 = STRUCAA-WKG010 + COSS-WKG010.
        STRUCAA-WKG011 = STRUCAA-WKG011 + COSS-WKG011.
        STRUCAA-WKG012 = STRUCAA-WKG012 + COSS-WKG012.
   ENDIF.

  ENDSELECT.

ENDFORM.

*---------------------------------------------------------------------*
*          FORM APPLY_PERCENT.                                        *
*---------------------------------------------------------------------*
 FORM APPLY_PERCENT.
 DATA: TOTAL_PROZS LIKE COBRB-PROZS.

  SELECT * FROM COBRB
   WHERE   OBJNR = AMTAB-OBJNR
     AND   LETJA GE P_YEAR.                             "SDP39303
*     AND   GBISJ = P_YEAR.                                 "SDP39303

*     AND   GABJA <= P_YEAR                                "SDP39303
*     AND   GBISJ >= P_YEAR.                               "SDP39303
   IF SY-SUBRC = 0.
      IF COBRB-ANLN1+0(5) = AMTAB-ANLKL+3(5).
         ADD COBRB-PROZS TO TOTAL_PROZS.
      ENDIF.
   ENDIF.
  ENDSELECT.
  IF TOTAL_PROZS > 0.
     WORK_DOLLAR = WORK_DOLLAR * TOTAL_PROZS / 100.           " %
     STRUCBB-WKG001 = STRUCBB-WKG001 * TOTAL_PROZS / 100.
     STRUCBB-WKG002 = STRUCBB-WKG002 * TOTAL_PROZS / 100.
     STRUCBB-WKG003 = STRUCBB-WKG003 * TOTAL_PROZS / 100.
     STRUCBB-WKG004 = STRUCBB-WKG004 * TOTAL_PROZS / 100.
     STRUCBB-WKG005 = STRUCBB-WKG005 * TOTAL_PROZS / 100.
     STRUCBB-WKG006 = STRUCBB-WKG006 * TOTAL_PROZS / 100.
     STRUCBB-WKG007 = STRUCBB-WKG007 * TOTAL_PROZS / 100.
     STRUCBB-WKG008 = STRUCBB-WKG008 * TOTAL_PROZS / 100.
     STRUCBB-WKG009 = STRUCBB-WKG009 * TOTAL_PROZS / 100.
     STRUCBB-WKG010 = STRUCBB-WKG010 * TOTAL_PROZS / 100.
     STRUCBB-WKG011 = STRUCBB-WKG011 * TOTAL_PROZS / 100.
     STRUCBB-WKG012 = STRUCBB-WKG012 * TOTAL_PROZS / 100.
  ENDIF.

  CLEAR: TOTAL_PROZS.

 ENDFORM.
**---------------------------------------------------------------------*
*                 LOAD_PRPS_DATA                                       *
*----------------------------------------------------------------------*
FORM LOAD_PRPS_DATA.

     SELECT OBJNR POSKI STUFE
       INTO TABLE PRPSTAB
       FROM PRPS
      WHERE POSKI IN S_POSKI
        AND LOEVM <> 'X'
      ORDER BY OBJNR.
ENDFORM.
**---------------------------------------------------------------------*
*                 BUILD_AUSP.                                          *
*----------------------------------------------------------------------*
FORM BUILD_AUSP.
  MOVE 'MAJORPROJECT' TO  CHARIC.           "Characteristics required
  PERFORM GET_ATINN.
  MOVE G_ATINN        TO   G_ATINN_MP.
  REFRESH CHAR_TAB.
  SELECT * FROM AUSP INTO TABLE CHAR_TAB
         WHERE  ATINN = G_ATINN_MP
           AND  MAFID = 'O'
           AND  KLART = '014'.
  SORT CHAR_TAB BY OBJEK ATINN.
ENDFORM.

**---------------------------------------------------------------------*
*                CHECK_MAJOR_PROJECT_INDICATOR                         *
*----------------------------------------------------------------------*
FORM CHECK_MAJOR_PROJECT_INDICATOR.
DATA: NEW_POSKI LIKE PRPS-POSKI.
READ TABLE PRPSTAB WITH KEY OBJNR = AMTAB-OBJNR BINARY SEARCH.
 IF SY-SUBRC EQ 0.
    CLEAR NEW_POSKI.
    MOVE PRPSTAB-POSKI+0(9) TO NEW_POSKI.
    CONCATENATE PRPSTAB-POSKI+0(2) PRPSTAB-POSKI+3(2) PRPSTAB-POSKI+6(3)
                PRPSTAB-POSKI+10(4) INTO REPTAB-PROJ.
     READ TABLE PRPSTAB WITH KEY POSKI = NEW_POSKI
                                 STUFE = 1.
     IF SY-SUBRC = 0.
        MOVE PRPSTAB-OBJNR TO NEW_OBJNR.
        PERFORM FIND_CHARACTERISTIC.
        IF MAJOR_PROJ CS 'Y'.
           MOVE 'Y' TO REPTAB-MAJOR.
           CLEAR MAJOR_PROJ.
        ENDIF.
     ENDIF.
  ENDIF.
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
     IF WS_BZDAT = '00000000' AND REPTAB-MAJOR = 'Y'.
        MOVE '*A'     TO REPTAB-REMARKS.
     ENDIF.
     MOVE WS_BZDAT TO REPTAB-BZDAT.
  ENDIF.
  CLEAR: WS_BZDAT.

ENDFORM.
*
*-------------------------------  GET_STATUS  --------------------------
* Routine to get the value of the PROJECT STATUS
*-----------------------------------------------------------------------
FORM GET_STATUS.
 CALL FUNCTION 'AIP9_STATUS_READ'                  "Retrieve Status Info
        EXPORTING
             I_OBJNR = AMTAB-OBJNR
             I_SPRAS = SY-LANGU
        IMPORTING
             E_SYSST = STATUS
        EXCEPTIONS
             OTHERS = 1.

  IF STATUS CS 'CLSD'.
     REPTAB-STATUS = ' CLSD'.
  ELSEIF STATUS CS 'TECO'.
     REPTAB-STATUS = ' TECO'.
  ELSEIF STATUS CS 'REL'.
     REPTAB-STATUS = ' REL'.
  ELSEIF STATUS CS 'CRTD'.
     REPTAB-STATUS = ' CRTD'.
  ELSEIF STATUS CS 'DLIN'.
     REPTAB-STATUS = ' DLIN'.
  ELSE.
     MOVE STATUS+0(4) TO REPTAB-STATUS.
  ENDIF.

ENDFORM.
