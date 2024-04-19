REPORT ZAAMI003 NO STANDARD PAGE HEADING LINE-SIZE 115 LINE-COUNT 60.

*---------------------------------------------------------------------*
*       REPORT ZAAMI003                                               *
*       AUTHOR M. Khan                                                *
*       DATE   NOVEMBER, 2001.                                        *
*---------------------------------------------------------------------*
*   This program creates a BDC session for AM to post mass partial    *
*   retirements using transaction ABAV.                               *
*                                                                     *
*---------------------------------------------------------------------*
*CHANGES:                                                             *
*                                                                     *
* 2012/08/08 M Khan   - TR995 Change C: drive to H: drive with        *
*                             directory, file selection using F4 and  *
*                                                                     *
*                                                                     *
*---------------------------------------------------------------------*

DATA:
    BEGIN OF ITAB   OCCURS 50,
    BUKRS LIKE PROJ-VBUKR,             "Company code
    ANLN1 LIKE ANLA-ANLN1,             "Asset
    ANLN2 LIKE ANLA-ANLN2,             "Sub Asset
    DDATE  LIKE RAIFP1-BLDAT,          "Doc. Date
    PDATE  LIKE RAIFP1-BLDAT,          "Posting Date
    ADATE  LIKE RAIFP1-BLDAT,          "AVD
    SGTXT(32) TYPE C,                  "Text
    QUANTY(9) TYPE N,                  "Quantity
    AMOUNT  LIKE COSP-WKG001,          "Amount Posted
  END OF ITAB.

*DATA: EXCELTAB TYPE KCDE_CELLS OCCURS 0 WITH HEADER LINE. "4.6C Upgrade
DATA: EXCELTAB TYPE ALSMEX_TABLINE OCCURS 0 WITH HEADER LINE.

* Batch input data
DATA: BEGIN OF BDCDATA OCCURS 500.
        INCLUDE STRUCTURE BDCDATA.
DATA: END OF BDCDATA.

DATA: W_DOLLARS   LIKE COSP-WKG001,
      LAST_COLUMN LIKE EXCELTAB-COL.
*------------------------ Selection Screen  ---------------------------

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
PARAMETERS: FNAME LIKE RLGRAP-FILENAME
*                  DEFAULT 'C:\retire.xls'.        "TR995
                  DEFAULT 'H:\retire.xls'.         "TR995
SELECTION-SCREEN BEGIN OF LINE.
   SELECTION-SCREEN COMMENT  1(31) TEXT-016.
      PARAMETERS: P_TEST  AS CHECKBOX DEFAULT ' '.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

*----------------------------------------------------------------------*
*       AT SELECTION-SCREEN
*----------------------------------------------------------------------*
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR FNAME.
data: wif_window_title        type string value 'Please Select File',
      wif_initial_directory   type string value 'h:\',
      wit_filename_tab        type filetable with header line,
      wif_rc                  type i.

  CALL METHOD CL_GUI_FRONTEND_SERVICES=>FILE_OPEN_DIALOG
    EXPORTING
      WINDOW_TITLE            = WIF_WINDOW_TITLE
*     DEFAULT_EXTENSION       =
*      default_filename        = wif_default_filename
*      FILE_FILTER             = WIF_FILE_FILTER
      INITIAL_DIRECTORY       = WIF_INITIAL_DIRECTORY
*     MULTISELECTION          =
    CHANGING
      FILE_TABLE              = WIT_FILENAME_TAB[]
      RC                      = WIF_RC
*     USER_ACTION             =
    EXCEPTIONS
      FILE_OPEN_DIALOG_FAILED = 1
      CNTL_ERROR              = 2
      ERROR_NO_GUI            = 3
      NOT_SUPPORTED_BY_GUI    = 4
      OTHERS                  = 5.

  IF ( SY-SUBRC = 0 ).
*Return user selection
    READ TABLE WIT_FILENAME_TAB INDEX 1.
    IF SY-SUBRC IS INITIAL AND WIF_RC > 0.
      FNAME = WIT_FILENAME_TAB.
    ELSE.
      CLEAR FNAME.
    ENDIF.
  ENDIF.
*End of TR995 changes
*----------------------------- MAINLINE --------------------------------
START-OF-SELECTION.

 CALL FUNCTION 'ALSM_EXCEL_TO_INTERNAL_TABLE'
      EXPORTING
           FILENAME                = FNAME
           I_BEGIN_COL             = 1
           I_BEGIN_ROW             = 2
           I_END_COL               = 9
           I_END_ROW               = 999
      TABLES
           INTERN                  = EXCELTAB
     EXCEPTIONS
          INCONSISTENT_PARAMETERS = 1
          UPLOAD_OLE              = 2
          OTHERS                  = 3
           .
 IF SY-SUBRC <> 0.
    SKIP 2.
    WRITE: / 'SY-SUBRC = ', SY-SUBRC, ' UNSUCCESSFUL EXCEL CALL '.
 ENDIF.

**************** Code for testing, please don't remove *********
** LOOP  AT EXCELTAB.
**       WRITE /1 EXCELTAB.
** ENDLOOP.
**************** End of testing code                   *********

 CLEAR LAST_COLUMN.

 LOOP AT EXCELTAB.
      IF EXCELTAB-COL < LAST_COLUMN.
         CLEAR LAST_COLUMN.
         IF ITAB-ANLN1 > 0.
            APPEND ITAB.
            CLEAR  ITAB.
         ENDIF.
      ELSE.
         MOVE EXCELTAB-COL TO LAST_COLUMN.
      ENDIF.

      IF EXCELTAB-COL = 1.
         MOVE EXCELTAB-VALUE TO ITAB-BUKRS.
      ELSEIF EXCELTAB-COL = 2.
         MOVE EXCELTAB-VALUE TO ITAB-ANLN1.
      ELSEIF EXCELTAB-COL = 3.
         MOVE EXCELTAB-VALUE TO ITAB-ANLN2.
      ELSEIF EXCELTAB-COL = 4.
         MOVE EXCELTAB-VALUE TO ITAB-DDATE.
      ELSEIF EXCELTAB-COL = 5.
         MOVE EXCELTAB-VALUE TO ITAB-PDATE.
      ELSEIF EXCELTAB-COL = 6.
         MOVE EXCELTAB-VALUE TO ITAB-ADATE.
      ELSEIF EXCELTAB-COL = 7.
         MOVE EXCELTAB-VALUE TO ITAB-SGTXT.
      ELSEIF EXCELTAB-COL = 8.
         MOVE EXCELTAB-VALUE TO ITAB-QUANTY.
      ELSE.
         MOVE EXCELTAB-VALUE TO ITAB-AMOUNT.
         CLEAR LAST_COLUMN.
         IF ITAB-ANLN1 > 0.
            APPEND ITAB.
            CLEAR  ITAB.
         ENDIF.

      ENDIF.
 ENDLOOP.

 LOOP AT ITAB.
      WRITE: /1 ITAB-BUKRS, ITAB-ANLN1, ITAB-ANLN2, ITAB-DDATE,
      ITAB-PDATE, ITAB-ADATE, ITAB-QUANTY, (12) ITAB-AMOUNT, ITAB-SGTXT.
 ENDLOOP.

*----------------------------- MAINLINE --------------------------------

  IF P_TEST = 'X'.
    PERFORM OPEN_BDC.
    LOOP AT ITAB.
      REFRESH BDCDATA.
      PERFORM BDC_SCREEN USING 'SAPMA01B'     '0100'.
      PERFORM BDC_FIELD  USING 'ANBZ-BUKRS'   ITAB-BUKRS.
      PERFORM BDC_FIELD  USING 'ANBZ-ANLN1'   ITAB-ANLN1.
      PERFORM BDC_FIELD  USING 'ANBZ-ANLN2'   ITAB-ANLN2.
      PERFORM BDC_FIELD  USING 'ANEK-BLDAT'   ITAB-DDATE.
      PERFORM BDC_FIELD  USING 'ANEK-BUDAT'   ITAB-PDATE.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE'   '/00'.
      PERFORM BDC_SCREEN USING 'SAPMA01B'     '0120'.
      PERFORM BDC_FIELD  USING 'ANBZ-BZDAT'   ITAB-ADATE.
      IF ITAB-AMOUNT > 0.
         PERFORM BDC_FIELD_N  USING 'ANBZ-DMBTR' ITAB-AMOUNT.
      ENDIF.
      IF ITAB-QUANTY > 0.
         PERFORM BDC_FIELD_N  USING 'ANBZ-MENGE' ITAB-QUANTY.
      ENDIF.
      PERFORM BDC_FIELD  USING 'ANEK-SGTXT'   ITAB-SGTXT.
      PERFORM BDC_FIELD  USING 'BDC_OKCODE'   'UPDA'.
    PERFORM INSERT_BDC.
    ENDLOOP.
    PERFORM CLOSE_BDC.
  ENDIF.
*
**---------------------------------------------------------------
**     FORM BDC_SCREEN
**----------------------------------------------------------------------
FORM BDC_SCREEN USING PROGRAM DYNPRO.

  CLEAR BDCDATA.
  BDCDATA-PROGRAM = PROGRAM.
  BDCDATA-DYNPRO = DYNPRO.
  BDCDATA-DYNBEGIN = 'X'.
  APPEND BDCDATA.

ENDFORM.

**--------------------------------------------------------------------
**     FORM BDC_FIELD
**--------------------------------------------------------------------
FORM BDC_FIELD USING FNAM FVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  APPEND BDCDATA.

ENDFORM.

**--------------------------------------------------------------------
**     FORM BDC_FIELD_N
**--------------------------------------------------------------------
FORM BDC_FIELD_N USING FNAM FVAL.

  CLEAR BDCDATA.
  BDCDATA-FNAM = FNAM.
  BDCDATA-FVAL = FVAL.
  SHIFT BDCDATA-FVAL LEFT DELETING LEADING SPACE.
  APPEND BDCDATA.

ENDFORM.

**---------------------------------------------------------------------*
**       FORM Open_BDC                                                 *
**---------------------------------------------------------------------*
FORM Open_BDC.
  CALL FUNCTION 'BDC_OPEN_GROUP'
       EXPORTING
            CLIENT              = sy-mandt
            GROUP               = 'ZAM_ASST_RET'
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
*
**---------------------------------------------------------------------*
**       FORM Insert_BDC                                               *
**---------------------------------------------------------------------*
FORM Insert_BDC.
  CALL FUNCTION 'BDC_INSERT'
       EXPORTING
            TCODE          = 'ABAV'
       TABLES
            DYNPROTAB      = BDCDATA
       EXCEPTIONS
            INTERNAL_ERROR = 1
            NOT_OPEN       = 2
            QUEUE_ERROR    = 3
            TCODE_INVALID  = 4
            OTHERS         = 5.
ENDFORM.

**---------------------------------------------------------------------*
**       FORM Close_BDC                                                *
**---------------------------------------------------------------------*
FORM Close_BDC.
  CALL FUNCTION 'BDC_CLOSE_GROUP'
       EXCEPTIONS
            NOT_OPEN    = 1
            QUEUE_ERROR = 2
            OTHERS      = 3.
ENDFORM.
**------------  TOP-OF-PAGE -----------------------------
TOP-OF-PAGE.
WRITE: /1 TEXT-RPT, SY-REPID COLOR COL_GROUP INTENSIFIED ON INVERSE ON,
            40 TEXT-TTL,
            85 TEXT-DTE, SY-DATUM, TEXT-AMP, SY-UZEIT.
  WRITE: / TEXT-CLT UNDER TEXT-RPT, SY-MANDT UNDER SY-REPID, SY-SYSID,
           TEXT-PGE UNDER TEXT-DTE, SY-PAGNO UNDER SY-DATUM.

  ULINE.
  WRITE:/1 TEXT-004.
