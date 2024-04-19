REPORT ZAFAR006.
TYPE-POOLS: SLIS.
************************************************************************
*  Author:    Mohammad Khan
*  Date:      January, 2010.
*  Issue log: 314
*  Brief Description:
*     - The purpose of this report is to list an acquisition values for
*     - selected assets and subassets with the exlusion of group assets.
*       Note: It's a copy of program ZAFAR005 and changes are to display
*             report in ALV and selection screen and some fields changes
*             are made.
*
************************************************************************
*CHANGES:
*
*
************************************************************************

TABLES:   ANLA, ANLB, ANLC, ANLZ.

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
          FIRST_TIME(1) VALUE 'Y',
          W_HEAD01(60) TYPE C,
          W_HEAD02(60) TYPE C,
          ES_VARIANT   LIKE DISVARIANT,
          IS_VARIANT LIKE DISVARIANT.

DATA: BEGIN OF ZTABLE     OCCURS 0,
                ANLKL       LIKE ANLA-ANLKL,  "Asset Class
                ANLGR       LIKE ANLB-ANLGR,  "Group asset
                TXT50       LIKE ANLA-TXT50,  "Asset Description
                TXA50       LIKE ANLA-TXA50,  "Additional Asset Descrp
                GDLGRP      LIKE ANLA-GDLGRP, "Station Id
                ANLN1       LIKE ANLA-ANLN1,  "Asset Number
                ANLN2       LIKE ANLA-ANLN2,  "Sub Asset
                AKTIV       LIKE ANLA-AKTIV,  "Asset capitalization date
                DEAKT       LIKE ANLA-DEAKT,  "Deactivation date
                VYEAR       LIKE ANLB-VYEAR,  "Acquisition year of asset
                WERKS       LIKE ANLZ-WERKS,  "Plant code
                KOSTL       LIKE ANLZ-KOSTL,
                MEINS       LIKE ANLA-MEINS,  "Base Unit of Measure
                MENGE       LIKE ANLA-MENGE,  "Quantity
                ACQVAL      LIKE ANLC-KANSW.
DATA: END OF ZTABLE.

************************************************************************
*   Begin of the selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME TITLE TEXT-001.
PARAMETERS:     P_CCODE     LIKE   ANLA-BUKRS   DEFAULT 'UGL'.
SELECT-OPTIONS: S_ASSTNR    FOR ANLA-ANLN1,
                S_SUBNR     FOR ANLA-ANLN2.
SELECT-OPTIONS: S_ACLASS   FOR ANLA-ANLKL,
                S_COSTCT   FOR ANLZ-KOSTL,
                S_PLANT    FOR ANLZ-WERKS,
                S_LOC      FOR ANLZ-STORT,
                S_AGROUP   FOR ANLA-ANLUE.
PARAMETERS:     P_GJAHR     LIKE ANLC-GJAHR,
                P_DEPARE    LIKE RBADA-AFABE1  DEFAULT '01'.
SELECT-OPTIONS: S_BALNCE   FOR ANLA-KTOGR,
                S_CAPDTE   FOR ANLA-AKTIV,
                S_ACQVAL   FOR ANLC-KANSW.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-003.
PARAMETERS PVARIANT LIKE DISVARIANT-VARIANT.         "Display Variant
PARAMETERS CHECK1 AS CHECKBOX.
PARAMETERS CHECK2 AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK BOX2.
SELECTION-SCREEN END OF BLOCK BOX.

************************************************************************

INITIALIZATION.
  P_GJAHR = SY-DATUM(4).

*Select Display variant
AT SELECTION-SCREEN ON VALUE-REQUEST FOR PVARIANT.
  IS_VARIANT-REPORT = 'ZAFAR006'.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
       EXPORTING
            IS_VARIANT          = IS_VARIANT
*           I_TABNAME_HEADER    =
*           I_TABNAME_ITEM      =
*           IT_DEFAULT_FIELDCAT =
            I_SAVE              = 'A'
        IMPORTING
*           E_EXIT              =
            ES_VARIANT          = ES_VARIANT
       EXCEPTIONS
            NOT_FOUND           = 1
            PROGRAM_ERROR       = 2
            OTHERS              = 3.
  PVARIANT = ES_VARIANT-VARIANT.

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
      ORDER BY ANLKL.
*     ORDER BY ANLA-ANLKL.

      CLEAR: TOTAL, ACQVAL.
      MOVE:   ANLA-ANLN1   TO ZTABLE-ANLN1,
              ANLA-ANLN2   TO ZTABLE-ANLN2,
              ANLA-ANLKL   TO ZTABLE-ANLKL,
              ANLA-GDLGRP  TO ZTABLE-GDLGRP,
              ANLA-TXT50   TO ZTABLE-TXT50,
              ANLA-TXA50   TO ZTABLE-TXA50,
              ANLZ-KOSTL   TO ZTABLE-KOSTL,
              ANLZ-WERKS   TO ZTABLE-WERKS,
              ANLA-MENGE   TO ZTABLE-MENGE,
              ANLA-AKTIV   TO ZTABLE-AKTIV,
              ANLA-DEAKT   TO ZTABLE-DEAKT,
*              0            TO ZTABLE-ANSWL,
              ANLA-MEINS   TO ZTABLE-MEINS,
              ACQVAL       TO ZTABLE-ACQVAL.
      PERFORM GET_ANLB_DATA.
      PERFORM GET_ANLC_DATA.
      IF CHECK2 = 'X' AND ANLA-XANLGR = 'X'.
         CLEAR ZTABLE.
      ELSE.
         APPEND ZTABLE.
         CLEAR ZTABLE.
      ENDIF.
    ENDSELECT.            "End of ANLA
  ENDSELECT.              "End of ANLZ
*--------------------  PRINT REQUIRED REPORTS --------------------------
IF CHECK1 = 'X'.
   DELETE ZTABLE WHERE ACQVAL = 0 AND MENGE = 0.
ENDIF.
SORT ZTABLE BY ANLKL ANLN1 ANLN2.
PERFORM DISPLAY_ALV_GRID_DATA.

************************************************************************
*   Subroutines used by a program:
************************************************************************
FORM GET_ANLB_DATA.
  SELECT SINGLE VYEAR ANLGR INTO (ANLB-VYEAR, ANLB-ANLGR)
    FROM ANLB
   WHERE BUKRS EQ ANLA-BUKRS
     AND ANLN1 EQ ANLA-ANLN1
     AND ANLN2 EQ ANLA-ANLN2.
    IF SY-SUBRC = 0.
       MOVE ANLB-VYEAR TO ZTABLE-VYEAR.
       MOVE ANLB-ANLGR TO ZTABLE-ANLGR.
    ELSE.
       MOVE '****'     TO: ZTABLE-VYEAR, ZTABLE-ANLGR.
    ENDIF.
ENDFORM.
************************************************************************
FORM GET_ANLC_DATA.
  SELECT * FROM ANLC
     WHERE BUKRS = ANLA-BUKRS
     AND ANLN1 = ANLA-ANLN1
     AND ANLN2 = ANLA-ANLN2
     AND GJAHR = P_GJAHR
     AND AFABE = P_DEPARE.
*-----------------------------------------------------------------------
    CLEAR: TOTAL, ACQVAL.
    TOTAL = ANLC-KANSW + ANLC-KINVZ + ANLC-INVZM + ANLC-KAUFW +
            ANLC-AUFWB.
    ACQVAL = TOTAL + ANLC-ANSWL.
    CHECK ACQVAL IN S_ACQVAL.
     MOVE   ACQVAL       TO ZTABLE-ACQVAL.
  ENDSELECT.           "End of ANLC
ENDFORM.
*-----------------------------------------------------------------------
*-----------------------------------------------------------------------
*
**----------------------------------------------------------------------
 FORM DISPLAY_ALV_GRID_DATA.

DATA: FIELDCAT TYPE SLIS_T_FIELDCAT_ALV,
      FC_STR   TYPE SLIS_FIELDCAT_ALV,
      LAYOUT   TYPE SLIS_LAYOUT_ALV,
      TITLE    TYPE LVC_TITLE,
      REPID    LIKE SY-REPID,
      VARIANT  LIKE DISVARIANT,
      SORT     TYPE SLIS_T_SORTINFO_ALV,
      SORT_STR TYPE SLIS_SORTINFO_ALV.

  CONCATENATE TEXT-062 P_CCODE TEXT-063 P_GJAHR INTO W_HEAD01
                                         SEPARATED BY SPACE.

  MOVE TEXT-CLT  TO W_HEAD02+0(7).
  MOVE SY-SYSID  TO W_HEAD02+8(5).
  MOVE SY-MANDT  TO W_HEAD02+14(4).
  MOVE TEXT-DTE  TO W_HEAD02+21(5).
  WRITE SY-DATUM TO W_HEAD02+27(10).
  MOVE TEXT-TME  TO W_HEAD02+40(5).
  WRITE SY-UZEIT TO W_HEAD02+46(10).
  REPID = SY-REPID.
  LAYOUT-COLWIDTH_OPTIMIZE = 'X'.
  LAYOUT-ZEBRA = 'X'.
  VARIANT-REPORT = REPID.
  VARIANT-VARIANT = PVARIANT.
  REFRESH FIELDCAT.
  CLEAR:  FIELDCAT, FC_STR.

* create field catalog
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
       I_PROGRAM_NAME         = REPID
       I_INTERNAL_TABNAME     = 'ZTABLE'
       I_INCLNAME             = REPID
    CHANGING
       CT_FIELDCAT            = FIELDCAT
    EXCEPTIONS
       INCONSISTENT_INTERFACE = 1
       PROGRAM_ERROR          = 2
       OTHERS                 = 3.

* update field catalog (hide/reposition/etc)
LOOP AT FIELDCAT INTO FC_STR.

CASE FC_STR-FIELDNAME.
     WHEN 'ANLN1'.
           FC_STR-KEY    = ' '.                 " Key columns-not first
     WHEN 'ANLN2'.
           FC_STR-KEY    = ' '.                 " Key columns-not first
     WHEN 'VYEAR'.
          FC_STR-SELTEXT_L = TEXT-VYR.          " Alternative col header
          FC_STR-DDICTXT = 'L'.
     WHEN OTHERS.
** fc_str-no_out = 'X'.           " hide column
ENDCASE.

MODIFY FIELDCAT FROM FC_STR.
ENDLOOP.

* Display ALV
CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
     EXPORTING
           IT_FIELDCAT  = FIELDCAT
           IS_LAYOUT    = LAYOUT
           I_CALLBACK_TOP_OF_PAGE = 'ALV_TOP_OF_PAGE'
          I_CALLBACK_PROGRAM      = repid
          I_SAVE       = 'A'
          IS_VARIANT   = variant
          IT_SORT        = sort
*           I_GRID_TITLE = TITLE
*          I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
           T_OUTTAB = ZTABLE
    EXCEPTIONS
           PROGRAM_ERROR = 1
    OTHERS               = 2.

ENDFORM.

*************************************************************
*                        TOP OF PAGE                        *
*************************************************************

FORM ALV_TOP_OF_PAGE.
  DATA: LS_LINE TYPE SLIS_LISTHEADER.
  DATA: LT_TOP_OF_PAGE TYPE SLIS_T_LISTHEADER.

*1- HEADING LINE: TYPE H
  CLEAR LS_LINE.
  LS_LINE-TYP  = 'H'.
  LS_LINE-INFO = SY-TITLE.             "sy-title.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*3- ACTION LINE:  TYPE A
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD01.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

*2- ACTION LINE:  TYPE A
  CLEAR LS_LINE.
  LS_LINE-TYP   = 'A'.
  LS_LINE-KEY   = ''.
  LS_LINE-INFO = W_HEAD02.
  APPEND LS_LINE TO LT_TOP_OF_PAGE.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
       EXPORTING
            IT_LIST_COMMENTARY = LT_TOP_OF_PAGE.
ENDFORM.
