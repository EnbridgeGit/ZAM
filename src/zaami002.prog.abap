REPORT ZAAMI002 NO STANDARD PAGE HEADING LINE-SIZE 116
                LINE-COUNT 65.

************************************************************************
*
*   PROGRAM:    ZAAMI002
*   PROGRAMMER: Marv Radsma
*   CLIENT:     Union Gas & Centra Gas
*   DATE:       November 1997.
*
*   The purpose of this program is to do a mass creation of subassets.
*   If a subasset for the specified source year exists and has a non-
*   zero dollar value, then a new subasset will be created for the
*   specified target year.  A report listing the created subassets is
*   provided along with the subassets that could not be created.  This
*   is an annual process run at yearend.
*
************************************************************************
*  MODIFICATIONS:
*
* 2001/02/19 mokhan   Isuue log - 865. Screen logic changes for 4.6 b.
*
* 2000/03/10 mdemeest Added screen 0181 as requested by T. Laframboise
************************************************************************

TABLES:   T001,                           " Company codes
          ANLA,                           " Asset master record-segment
          ANLC.                           " Asset value fields

DATA:     ASSET_OLD(6)  TYPE N,                         " assets read
          ASSET_NEW(6)  TYPE N,                         " assets created
          ASSET_ERR(6)  TYPE N,                         " asset errors
          ASSET_SKP(6)  TYPE N,                         " assets skipped
          TRX_MODE(1)   VALUE 'N',                      " show errors
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
            SOURCE      LIKE ANLA-ANLN2,                " old sub asset
            TARGET      LIKE ANLA-ANLN2,                " new sub asset
            TXT50       LIKE ANLA-TXT50,                " description
            NET_VAL     LIKE ANLC-KANSW,                " net book value
            STATUS(6),                                  " update status
          END OF REPTAB.

DATA:     BEGIN OF BDCDATA OCCURS 100.
            INCLUDE STRUCTURE BDCDATA.
DATA:     END OF BDCDATA.

************************************************************************
*   Beginning of selection screen

SELECTION-SCREEN BEGIN OF BLOCK BOX1 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 26(27) TEXT-001.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX1.

SELECTION-SCREEN BEGIN OF BLOCK BOX WITH FRAME.
SELECTION-SCREEN BEGIN OF BLOCK BOX2 WITH FRAME TITLE TEXT-002.
      SELECT-OPTIONS S_COMP   FOR ANLA-BUKRS OBLIGATORY NO INTERVALS.
      SELECT-OPTIONS S_SRCE   FOR ANLA-ANLN2 OBLIGATORY NO INTERVALS.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(27) TEXT-005.
      PARAMETERS: P_TRGT      LIKE ANLA-ANLN2 OBLIGATORY.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX2.

SELECTION-SCREEN BEGIN OF BLOCK BOX3 WITH FRAME TITLE TEXT-006.
      SELECT-OPTIONS S_ASSET  FOR ANLA-ANLN1.
SELECTION-SCREEN END OF BLOCK BOX3.

SELECTION-SCREEN BEGIN OF BLOCK BOX4 WITH FRAME.
   SELECTION-SCREEN BEGIN OF LINE.
      SELECTION-SCREEN COMMENT 3(27) TEXT-008.
      PARAMETERS: P_no        RADIOBUTTON GROUP RADI.
      SELECTION-SCREEN COMMENT 33(05) TEXT-010.
      PARAMETERS: P_yes       RADIOBUTTON GROUP RADI.
      SELECTION-SCREEN COMMENT 43(05) TEXT-009.
   SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK BOX4.

SELECTION-SCREEN END OF BLOCK BOX.

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
  AND   ANLN1 IN S_ASSET
  AND   ANLN2 IN S_SRCE.

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

    IF NET_VAL <> 0.
      ASSET_OLD      = ASSET_OLD + 1.
      REPTAB-BUKRS   = ANLA-BUKRS.
      REPTAB-ANLN1   = ANLA-ANLN1.
      REPTAB-SOURCE  = ANLA-ANLN2.
      REPTAB-TARGET  = P_TRGT.
      REPTAB-TXT50   = ANLA-TXT50.
      REPTAB-NET_VAL = NET_VAL.
      REPTAB-STATUS  = SPACE.
      APPEND REPTAB.
      CLEAR REPTAB.
    ENDIF.

  ENDSELECT.

* sort the cost center reposting amount table.
  SORT REPTAB BY BUKRS ANLN1.

* delete any duplicate sub-assets from the table
  DELETE ADJACENT DUPLICATES FROM REPTAB COMPARING BUKRS ANLN1.

* Process the table, outputing the report and do update if required
  LOOP AT REPTAB.
    SELECT SINGLE * FROM ANLA                         " does asset exist
    WHERE BUKRS EQ REPTAB-BUKRS
    AND   ANLN1 EQ REPTAB-ANLN1
    AND   ANLN2 EQ P_TRGT.
    IF SY-SUBRC = 0.
      ASSET_SKP = ASSET_SKP + 1.
      REPTAB-STATUS = 'Exists'.
    ENDIF.
    IF P_NO = 'X'
    OR REPTAB-STATUS = 'Exists'.
      IF REPTAB-STATUS <> 'Exists'.
        ASSET_NEW = ASSET_NEW + 1.
      ENDIF.
      PERFORM WRITE_DETAIL.
    ELSE.
      PERFORM CREATE_SUBASSET.
      PERFORM WRITE_DETAIL.
    ENDIF.
    AT END OF BUKRS.
      ULINE.
      NEW-PAGE.
    ENDAT.
  endloop.

  PERFORM WRITE_TOTALS.

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
            , 104 TEXT-011,  SY-PAGNO
            , 116 SY-VLINE.
     WRITE:   /01 SY-VLINE
            ,     SY-UZEIT UNDER SY-DATUM
            , 047 TEXT-001
            ,     SY-REPID UNDER TEXT-011
            , 116 SY-VLINE.
     ULINE.
     WRITE:   /03 TEXT-012
            , 018 TEXT-013
            , 027 TEXT-014
            , 036 TEXT-015
            , 091 TEXT-016
            , 109 TEXT-017.
     PERFORM SHOWVLINE.
     FORMAT INTENSIFIED ON.
     ULINE.
ENDFORM.

FORM WRITE_DETAIL.
     WRITE: /     REPTAB-ANLN1   UNDER TEXT-012
            ,     REPTAB-SOURCE  UNDER TEXT-013
            ,     REPTAB-TARGET  UNDER TEXT-014
            ,     REPTAB-TXT50   UNDER TEXT-015
            ,     REPTAB-NET_VAL UNDER TEXT-016
            ,     REPTAB-STATUS  UNDER TEXT-017.
     PERFORM SHOWVLINE.
ENDFORM.

FORM ShowVline.
     WRITE:   001 SY-VLINE
            , 016 SY-VLINE
            , 025 SY-VLINE
            , 034 SY-VLINE
            , 087 SY-VLINE
            , 107 SY-VLINE
            , 116 SY-VLINE.
ENDFORM.

FORM WRITE_TOTALS.
     ULINE.
     FORMAT INTENSIFIED OFF.
     WRITE:   /01  SY-VLINE
            , 003  TEXT-018
            , 050  ASSET_OLD
            , 116 SY-VLINE.
     WRITE:   /01  SY-VLINE
            , 003  TEXT-019
            , 050  ASSET_NEW
            , 116 SY-VLINE.
     WRITE:   /01  SY-VLINE
            , 003  TEXT-020
            , 050  ASSET_ERR
            , 116 SY-VLINE.
     WRITE:   /01  SY-VLINE
            , 003  TEXT-021
            , 050  ASSET_SKP
            , 116 SY-VLINE.
     ULINE.
     FORMAT INTENSIFIED OFF.
ENDFORM.

************************************************************************
*  Listed below is the subroutine which creates the update transactions
************************************************************************

FORM CREATE_SUBASSET.

     PERFORM SCREEN_HEADER USING 'SAPLAIST'   '0110'   'X'.
     PERFORM SCREEN_FIELD  USING 'ANLA-ANLN1' REPTAB-ANLN1. " main asset
     PERFORM SCREEN_FIELD  USING 'ANLA-BUKRS' REPTAB-BUKRS. " company cd
     PERFORM SCREEN_FIELD  USING 'RA02S-NASSETS' '1'. " Similar Assets

*   PERFORM SCREEN_HEADER USING 'SAPLAIST'   '0140'   'X'. "Issuelog 865
     PERFORM SCREEN_HEADER USING 'SAPLAIST'   '1000'   'X'.
     PERFORM SCREEN_FIELD  USING 'ANLA-ANLN2' P_TRGT.       " sub asset
     PERFORM SCREEN_FIELD USING 'ANLA-AKTIV' '          '. "Issuelog 865
     PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' '/6'.         "next screen
     PERFORM SCREEN_HEADER USING 'SAPLAIST'   '1000'   'X'.
     PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' '/6'.         "next screen
     PERFORM SCREEN_HEADER USING 'SAPLAIST'   '1000'   'X'.
     PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' '/6'.         "next screen
     PERFORM SCREEN_HEADER USING 'SAPLAIST'   '1000'   'X'.
     PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' '/6'.         "next screen

*   PERFORM SCREEN_HEADER USING 'SAPLAIST'   '0145'   'X'. "Issuelog 865

*   PERFORM SCREEN_HEADER USING 'SAPLAIST'   '0160'   'X'. "Issuelog 865

*   PERFORM SCREEN_HEADER USING 'SAPLAIST'   '0181'   'X'. "Issuelog 865

*   PERFORM SCREEN_HEADER USING 'SAPLAIST'   '0190'   'X'. "Issuelog 865
     PERFORM SCREEN_HEADER USING 'SAPLAIST'   '1000'   'X'.
     PERFORM SCREEN_FIELD  USING 'BDC_CURSOR' 'ANLB-AFASL(1)'.
     PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' '/2'.         " <choose>

     PERFORM SCREEN_HEADER USING 'SAPLAIST'   '0195'   'X'.
     PERFORM SCREEN_FIELD  USING 'ANLB-VYEAR' P_TRGT.       " vintage yr
     PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' '/11'.        " <save>

     CALL TRANSACTION 'AS11' USING  BDCDATA         " data source
                             MODE   TRX_MODE        " N - no display
                             UPDATE 'S'.            " S - synchronous up
     if sy-subrc <> 0.
       ASSET_ERR = ASSET_ERR + 1.
       REPTAB-STATUS = 'ERROR'.
     ELSE.
       ASSET_NEW = ASSET_NEW + 1.
       REPTAB-STATUS = 'Create'.
     endif.
     refresh BDCData.
     clear BDCData.
ENDFORM.

************************************************************************
*  Listed below are subroutines to open, close and process BDC data
************************************************************************

FORM Screen_Header using program screen indicator.
     clear BDCData.
     BDCData-program             = program.
     BDCData-dynpro              = screen.
     BDCData-dynbegin            = indicator.
     append BDCData.
ENDFORM.

FORM Screen_Field using fnam fval.
     clear BDCData.
     BDCData-fnam                = fnam.
     BDCData-fval                = fval.
     append BDCData.
ENDFORM.
