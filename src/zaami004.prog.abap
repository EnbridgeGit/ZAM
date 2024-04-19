REPORT ZAAMI004 NO STANDARD PAGE HEADING LINE-SIZE 116
                LINE-COUNT 65.

************************************************************************
*
*   PROGRAM:    ZAAMI004
*   PROGRAMMER: MOHAMMAD KHAN
*   CLIENT:     Union Gas & Centra Gas
*   DATE:       July 2002.
*
*   This program is intended to be a ONE TIME program. The purpose of
*   this program is to change or clear out capitalized on date for the
*   selected assets/sub assets.
*
************************************************************************
*  MODIFICATIONS:
*
*
************************************************************************

TABLES:   T001,                           " Company codes
          ANLA,                           " Asset master record-segment
          ANLC.                           " Asset value fields

DATA:     TRX_MODE(1)   VALUE 'N',                      " show errors
          NET_VAL       LIKE ANLC-KANSW,                " net book value
          AMOUNT1       LIKE ANLC-KANSW,                " work field
          AMOUNT2       LIKE ANLC-KANSW,                " work field
          AMOUNT3       LIKE ANLC-KANSW,                " work field
          AMOUNT4       LIKE ANLC-KANSW,                " work field
          AMOUNT5       LIKE ANLC-KANSW,                " work field
          AMOUNT6       LIKE ANLC-KANSW,                " work field
          AMOUNT7       LIKE ANLC-KANSW,                " work field
          AMOUNT8       LIKE ANLC-KANSW,                " work field
          AMOUNT9       LIKE ANLC-KANSW,                " work field
          AMOUNT0       LIKE ANLC-KANSW.                " work field

DATA:     BEGIN OF REPTAB OCCURS 100,
            BUKRS       LIKE ANLA-BUKRS,                " company code
            ANLN1       LIKE ANLA-ANLN1,                " main asset
            ANLN2       LIKE ANLA-ANLN2,                " sub asset
            AKTIV       LIKE ANLA-AKTIV,                " cap. date
            NEW_AKTIV   LIKE ANLA-AKTIV,                " new cap.date
            TXT50       LIKE ANLA-TXT50,                " description
            NET_VAL     LIKE ANLC-KANSW,                " net book value
            STATUS(6),                                  " update status
          END OF REPTAB.

DATA:     BEGIN OF BDCDATA OCCURS 100.
            INCLUDE STRUCTURE BDCDATA.
DATA:     END OF BDCDATA.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
      SELECT-OPTIONS S_COMP   FOR ANLA-BUKRS NO INTERVALS.
      SELECT-OPTIONS S_ANLN1   FOR ANLA-ANLN1.
      SELECT-OPTIONS S_ANLN2   FOR ANLA-ANLN2 OBLIGATORY
                               DEFAULT '2001' TO '2002'.

   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(27) TEXT-008.
      PARAMETERS: P_NO        RADIOBUTTON GROUP RADI.
      SELECTION-SCREEN COMMENT 33(05) TEXT-010.
      PARAMETERS: P_YES       RADIOBUTTON GROUP RADI.
      SELECTION-SCREEN COMMENT 43(05) TEXT-009.
   SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK BOX2.


*   End of selection screen.
************************************************************************
* initialize
REFRESH: REPTAB, BDCDATA.
CLEAR:   REPTAB, BDCDATA.

* set up the printing of report headers
TOP-OF-PAGE.
  PERFORM WRITE_HEADER.

* extract required data
START-OF-SELECTION.

  SELECT * FROM ANLA                           " get the required assets
  WHERE BUKRS IN S_COMP
  AND   ANLN1 IN S_ANLN1
  AND   ANLN2 IN S_ANLN2.

  IF SY-SUBRC = 0.
   IF ANLA-ANLN2 = S_ANLN2-LOW  OR
      ANLA-ANLN2 BETWEEN S_ANLN2-LOW AND S_ANLN2-HIGH.
      IF ANLA-AKTIV <> '00000000'.
         IF ANLA-ANLN2 <> ANLA-AKTIV+0(4).
            PERFORM GET_AMOUNT.
            IF NET_VAL = 0.
               REPTAB-NEW_AKTIV = '        '.
            ELSE.
               CONCATENATE ANLA-ANLN2 '1214' INTO REPTAB-NEW_AKTIV.
            ENDIF.
            PERFORM BUILD_REPTAB.
         ENDIF.
      ENDIF.
    ENDIF.
   ENDIF.


**   IF ANLA-ANLN2 = '2001' OR ANLA-ANLN2 = '2002'.
**      IF ANLA-AKTIV <> '00000000'.
**         IF ANLA-ANLN2 = '2001'.
**            IF ANLA-AKTIV+0(4) <> '2001'.
**               PERFORM GET_AMOUNT.
**               IF NET_VAL = 0.
**                  REPTAB-NEW_AKTIV = '        '.
**               ELSE.
**                  REPTAB-NEW_AKTIV = '20011214'.
**               ENDIF.
**              PERFORM BUILD_REPTAB.
**            ENDIF.
**         ELSE.
**            IF ANLA-AKTIV+0(4) <> '2002'.
**               PERFORM GET_AMOUNT.
**               IF NET_VAL = 0.
**                  REPTAB-NEW_AKTIV = '        '.
**               ELSE.
**                  REPTAB-NEW_AKTIV = '20021214'.
**               ENDIF.
**               PERFORM BUILD_REPTAB.
**            ENDIF.
**         ENDIF.
**      ENDIF.
**   ENDIF.
**  ENDIF.

  ENDSELECT.

* sort the cost center reposting amount table.
  SORT REPTAB BY BUKRS ANLN2 ANLN1.

  IF REPTAB[] IS INITIAL.
     WRITE: /15 'NO DATA SELECTED'.
     STOP.
  ELSE.
     IF P_YES = 'X'.
        PERFORM OPEN_BDC.
     ENDIF.
  ENDIF.


* Process the table, outputing the report and do update if required
  LOOP AT REPTAB.
      PERFORM WRITE_DETAIL.
      IF P_YES = 'X'.
         PERFORM CHANGE_CAP_DATE.
      ENDIF.
    AT END OF BUKRS.
      ULINE.
      NEW-PAGE.
    ENDAT.
  ENDLOOP.

     IF P_YES = 'X'.
        PERFORM CLOSE_BDC.
     ENDIF.


FORM GET_AMOUNT.
*   note that the following calculation is located in pgm SAPMA03W
    NET_VAL = 0.
    SELECT * FROM ANLC                         " determine net book valu
    WHERE BUKRS EQ ANLA-BUKRS
    AND   ANLN1 EQ ANLA-ANLN1
    AND   ANLN2 EQ ANLA-ANLN2.

      AMOUNT1 = ANLC-KANSW                     " book value at year strt
              + ANLC-KAUFW
              + ANLC-KMAFA
              + ANLC-KNAFA
              + ANLC-KSAFA
              + ANLC-KAAFA
              + ANLC-KAUFN.
      AMOUNT2 = ANLC-KANSW                     " current aquisition valu
              + ANLC-KAUFW
              + ANLC-KMAFA
              + ANLC-ANSWL
              + ANLC-MAFAG
              + ANLC-AUFWB
              + ANLC-AUFWV
              + ANLC-AUFWL.
      AMOUNT3 = ANLC-KANSW                     " aquisition valu yr strt
              + ANLC-KAUFW
              + ANLC-KMAFA.
      AMOUNT4 = ANLC-ZUSNA                     " write up at year end
              + ANLC-ZUSSA
              + ANLC-ZUSAA.
      AMOUNT5 = ANLC-NAFAG.                    " posted ordinary deprec
      AMOUNT6 = ANLC-SAFAG.                    " posted special deprec
      AMOUNT7 = ANLC-AAFAG.                    " posted unplanned deprec
      AMOUNT8 = ANLC-MAFAG.                    " transferred reserves
      AMOUNT9 = ANLC-AAFAG.                    " posted appr of ord depr
      AMOUNT0 = ANLC-NAFAL                     " value adjustment yr end
              + ANLC-SAFAL
              + ANLC-AAFAL
              + ANLC-NAFAV
              + ANLC-SAFAV
              + ANLC-AAFAV
              + ANLC-AUFNV.
      NET_VAL = NET_VAL                        " accumulate book value
              + AMOUNT1
              + AMOUNT2
              - AMOUNT3
              + AMOUNT4
              + AMOUNT5
              + AMOUNT6
              + AMOUNT7
              + AMOUNT8
              + AMOUNT9
              + AMOUNT0.
    ENDSELECT.
ENDFORM.

FORM BUILD_REPTAB.
      REPTAB-BUKRS   = ANLA-BUKRS.
      REPTAB-ANLN1   = ANLA-ANLN1.
      REPTAB-ANLN2   = ANLA-ANLN2.
      REPTAB-AKTIV   = ANLA-AKTIV.
      REPTAB-TXT50   = ANLA-TXT50.
      REPTAB-NET_VAL  = NET_VAL.
      APPEND REPTAB.
      CLEAR REPTAB.

ENDFORM.

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
            , 049 T001-BUTXT
            , 094 TEXT-011,  SY-PAGNO
            , 116 SY-VLINE.
     WRITE:   /01 SY-VLINE
            ,     SY-UZEIT UNDER SY-DATUM
            , 047 TEXT-001
            ,     SY-REPID UNDER TEXT-011
            , 116 SY-VLINE.
     ULINE.
     WRITE:   /01 TEXT-015
            , 052 TEXT-012
            , 065 TEXT-013
            , 071 TEXT-014
            , 083 TEXT-016
            , 100 TEXT-017.
     FORMAT INTENSIFIED ON.
     ULINE.
ENDFORM.

FORM WRITE_DETAIL.
     WRITE: /     REPTAB-TXT50   UNDER TEXT-015
            ,     REPTAB-ANLN1   UNDER TEXT-012
            ,     REPTAB-ANLN2   UNDER TEXT-013
            ,     REPTAB-AKTIV   UNDER TEXT-014
            ,     REPTAB-NEW_AKTIV  UNDER TEXT-016
            ,     REPTAB-NET_VAL UNDER TEXT-017.
ENDFORM.

************************************************************************
*  Listed below is the subroutine which creates the update transactions
************************************************************************

FORM CHANGE_CAP_DATE.

    PERFORM BDC_SCREEN USING 'SAPLAIST'    '100'.
    PERFORM BDC_FIELD  USING 'ANLA-ANLN1' REPTAB-ANLN1.
    PERFORM BDC_FIELD  USING 'ANLA-ANLN2' REPTAB-ANLN2.
    PERFORM BDC_FIELD  USING 'ANLA-BUKRS' REPTAB-BUKRS.
    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/7'.

    PERFORM BDC_SCREEN USING 'SAPLAIST'    '1000'.
    PERFORM BDC_FIELD  USING 'ANLA-AKTIV' REPTAB-NEW_AKTIV.

    PERFORM BDC_FIELD  USING 'BDC_OKCODE' '/11'.        " <save>

    PERFORM INSERT_BDC.
    CLEAR BDCDATA.
    REFRESH BDCDATA.


ENDFORM.

*-----------------------------------------------------------------------
*     FORM BDC_SCREEN
*-----------------------------------------------------------------------

FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM = PROGRAM.
  BDCDATA-DYNPRO = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.

*-----------------------------------------------------------------------
*     FORM BDC_FIELD
*-----------------------------------------------------------------------

FORM BDC_FIELD USING FNAM FVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM Open_BDC                                                 *
*---------------------------------------------------------------------*
FORM Open_BDC.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT              = sy-mandt
            GROUP               = 'ZAM_CAP_DATE'
            KEEP                = 'X'
            USER                = sy-uname
       EXCEPTIONS
            CLIENT_INVALID      = 1
            DESTINATION_INVALID = 2
            GROUP_INVALID       = 3
            GROUP_IS_LOCKED     = 4
            HOLDDATE_INVALID    = 5
            INTERNAL_ERROR      = 6
            QUEUE_ERROR         = 7
            RUNNING             = 8
            SYSTEM_LOCK_ERROR   = 9
            USER_INVALID        = 10
            OTHERS              = 11.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM Insert_BDC                                               *
*---------------------------------------------------------------------*
FORM Insert_BDC.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'AS02'
       TABLES
            DYNPROTAB      = BDCData
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4
            OTHERS         = 5.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM Close_BDC                                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM Close_BDC.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN    = 1
            QUEUE_ERROR = 2
            OTHERS      = 3.
ENDFORM.


