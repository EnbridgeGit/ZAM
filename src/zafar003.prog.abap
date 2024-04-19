REPORT zafar003 LINE-SIZE 170 LINE-COUNT 58 NO STANDARD PAGE HEADING
       MESSAGE-ID za.

************************************************************************
* Author:    M DeMeester
* Brief Description:
*      This report will list all projects remaining as Work-in-Progress
*      at the time this report is run.
*  For performance reasons, this report was written in 3 sections.
*  - get the Project & WBS data
*  - get the Asset Management data
*  - display report by matching Asset key to Project key
*
*  Items highlighted in red require action
*  - Status - this item should be an asset
*  - Division - doesn't match division in WBS - should be corrected
************************************************************************
* 99/02/03 md7140 #---  Initial request
*
* 01/02/26 mokhan (Issue # 839)
*          Selecting the file option is printing more than one
*          heading and first detail line is not prined.
* 2010/07/08  btboundy  TR842 - Adding depreciation area to selection
************************************************************************
TABLES: anla,                          "Asset
        anlc,                          "Value of Asset
        anlk,                          "Xref between Asset & WBS
        prps,                          "WBS
        proj,                          "Project
        t095.                          "XREF between Asset Class & GL

*------------------------ TEMPORARY VALUES -----------------------------
DATA:   total   LIKE anlc-kansw,       "Accumulated value of AUC
        objnr   LIKE prps-objnr,       "Object key for PRPS
        status(40)  TYPE c,            "All valid status of object
        reportline(190)  TYPE c,                            " TR842
        headline(190)    TYPE c,       "issue log 839                          TR842
*        reportline(170)  type c,                                              TR842
*        headline(170)    type c,       "issue log 839                         TR842
        first_time.
*-------------------------- INTERNAL TABLE -----------------------------
DATA: BEGIN OF amtable     OCCURS 50000,
         bukrs       LIKE anlc-bukrs,             "Company Code
         anlkl       LIKE anla-anlkl,             "Asset Class
         anln1       LIKE anlc-anln1,             "Asset Number
         anln2       LIKE anlc-anln2,             "Group Asset Indicator
         objnr       LIKE prps-objnr,
         afabe       LIKE anlc-afabe,             "Depretiation                TR842
         total       LIKE anlc-kansw.             "Sum of AUC/Asset
DATA: END OF amtable.

DATA: BEGIN OF pstable     OCCURS 50000,          "PROJECT INFO
         objnr       LIKE prps-objnr,             "Object key
         posid       LIKE prps-posid,             "WBS
         wbspost1    LIKE prps-post1,             "WBS Description
         prart       LIKE prps-prart,             "Project Type
         pspri       LIKE prps-pspri,             "Priority
         pspnr       LIKE proj-pspnr,             "Project Number
         post1       LIKE proj-post1,             "Project Description
         vernr       LIKE prps-vernr,             "Division
         stat(4)     TYPE c.                      "Status
DATA: END OF pstable.

*----------------------- SELECTION SCREEN ------------------------------
SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME TITLE text-001.
PARAMETERS: check1 AS CHECKBOX.
SELECTION-SCREEN END OF BLOCK box4.

SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-002.
SELECTION-SCREEN SKIP 1.
PARAMETERS:     p_ccode like anep-bukrs default 'UGL'.
SELECT-OPTIONS: s_anln1 FOR  anlc-anln1
                   DEFAULT '100000000000' TO '199999999999',
                s_anln2 FOR  anlc-anln2,
                s_anlkl FOR  anla-anlkl.
PARAMETERS:     p_afabe LIKE rbada-afabe1 DEFAULT '01'.             "Depretiation            TR842
SELECT-OPTIONS: s_stat  FOR  pstable-stat.
PARAMETERS:     p_gjahr LIKE anlc-gjahr OBLIGATORY DEFAULT sy-datum(4).
SELECTION-SCREEN END OF BLOCK box.
*-------------------------  ON VALUE REQUEST  -------------------------
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_afabe.           "TR314
  PERFORM bereich_value USING 'P_AFABE' p_afabe.            "TR314
*--------------------- START-OF-SELECTION ------------------------------
START-OF-SELECTION.
* GET PROJECT & WBS INFO

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = 'Retrieving WBS Info'
    EXCEPTIONS
      OTHERS = 1.

  SELECT * FROM proj.
    IF proj-pspid+5(2) CO '0123456789'.
      SELECT * FROM prps
        WHERE psphi = proj-pspnr
          AND belkz = 'X'.
        PERFORM build_pstable.
      ENDSELECT.
    ENDIF.
  ENDSELECT.

  SORT pstable BY objnr.

* GET ASSET MANAGEMENT INFO
  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      text   = 'Retrieving Asset Info'
    EXCEPTIONS
      OTHERS = 1.

  SELECT * FROM anlc
   WHERE bukrs = p_ccode
     AND anln1 IN s_anln1
     AND anln2 IN s_anln2
     AND afabe = p_afabe                                    " TR842
     AND gjahr = p_gjahr.
    CLEAR total.
    total = anlc-kansw + anlc-kinvz + anlc-invzm + anlc-kaufw
                       + anlc-aufwb + anlc-answl.
    IF total <> 0.
      SELECT * FROM anla
        WHERE bukrs = anlc-bukrs
          AND anln1 = anlc-anln1
          AND anln2 = anlc-anln2
          AND anlkl IN s_anlkl.
        SELECT SINGLE * FROM anlk
           WHERE bukrs = anlc-bukrs
             AND anln1 = anlc-anln1
             AND anln2 = anlc-anln2.
        CONCATENATE: anlk-obart anlk-objid INTO objnr.
        PERFORM build_amtable.
      ENDSELECT.
    ENDIF.
  ENDSELECT.

  IF check1 = 'X'.
    PERFORM display_file.
    WRITE: / text-021 UNDER text-012.
  ELSE.
    PERFORM display_table.
    WRITE: / text-021 UNDER text-012.
  ENDIF.

*-------------------------- SUBROUTINES --------------------------------
*-------------------------- BUILD_AMTABLE ------------------------------
FORM build_amtable.
  CLEAR amtable.
  amtable-anlkl = anla-anlkl.
  amtable-anln1 = anlc-anln1.
  amtable-anln2 = anlc-anln2.
  amtable-afabe = anlc-afabe.
  amtable-total = total.
  amtable-objnr = objnr.
  amtable-bukrs = anlc-bukrs.
  APPEND amtable.
ENDFORM.                    "build_amtable

*-------------------------- BUILD_PSTABLE ------------------------------
FORM build_pstable.
  CLEAR pstable.
  pstable-objnr     = prps-objnr.
  pstable-posid     = prps-posid.
  pstable-wbspost1  = prps-post1.
  pstable-prart     = prps-prart.
  pstable-pspri     = prps-pspri.
  pstable-post1     = proj-post1.
  pstable-vernr     = prps-vernr.

  CALL FUNCTION 'AIP9_STATUS_READ'                  "Retrieve Status Info
        EXPORTING
             i_objnr = pstable-objnr
             i_spras = sy-langu
        IMPORTING
             e_sysst = status
        EXCEPTIONS
             OTHERS = 1.

  IF status CS 'DLFL'.
    pstable-stat = 'DLFL'.
  ELSEIF status CS 'CLSD'.
    pstable-stat = 'CLSD'.
  ELSEIF status CS 'TECO'.
    pstable-stat = 'TECO'.
  ELSEIF status CS 'REL'.
    pstable-stat = 'REL'.
  ENDIF.
  APPEND pstable.
ENDFORM.                    "build_pstable

*------------------------- GET_WBS_INFO --------------------------------

*------------------------- DISPLAY_TABLE -------------------------------
FORM display_table.
  SORT amtable BY bukrs anlkl anln1 anln2 afabe.            " TR842

  LOOP AT amtable.
    AT NEW anlkl.
      NEW-PAGE.
    ENDAT.
    READ TABLE pstable WITH KEY objnr = amtable-objnr BINARY SEARCH.
    IF sy-subrc = '0'.
      IF pstable-stat IN s_stat.
        FORMAT COLOR COL_BACKGROUND.
        PERFORM print_vert.                                "ASSET INFO
        WRITE:  amtable-anln1 UNDER text-010 ,       "Asset Number
                text-dsh , amtable-anln2,            "Sub-number
                amtable-afabe UNDER text-023,       "Depreciation           TR842
                amtable-total UNDER text-014.              "Dollars in Acct

        WRITE: (16) pstable-posid    UNDER text-011,       "WBS Element
                    pstable-wbspost1 UNDER text-012,       "WBS Descript
                    pstable-post1    UNDER text-015,
                    pstable-prart    UNDER text-018,
                    pstable-pspri    UNDER text-019.

*   highlights in red any VERNR not equal to division in WBS
        IF pstable-posid(2) <>  pstable-vernr.                 "DIVISION
          FORMAT COLOR COL_NEGATIVE INVERSE.
        ENDIF.
        WRITE:      pstable-vernr    UNDER text-017.
        FORMAT COLOR COL_BACKGROUND INVERSE OFF.

*   highlights in red any non-REL status
        IF pstable-stat <> 'REL'.                            "STATUS
          FORMAT COLOR COL_NEGATIVE INVERSE.
        ENDIF.
        WRITE:      pstable-stat     UNDER text-016.
        FORMAT COLOR COL_BACKGROUND INVERSE OFF.
      ELSE.
        amtable-total = 0.
        MODIFY amtable.
      ENDIF.                        "==> end of STATUSes selected
    ENDIF.                         "==> end of WBS info exists

    AT END OF anlkl.                                    "ASSET CLASS TOTAL
      SELECT SINGLE * FROM t095 WHERE ktogr = amtable-anlkl.
      SUM.
      PERFORM print_vert.
      WRITE: sy-uline.
      PERFORM print_vert.
      WRITE: 2 text-020, 13 amtable-anlkl, 23 t095-ktansw+4(6),
                            amtable-total UNDER text-014.
      WRITE: sy-uline.
    ENDAT.

    AT LAST.                                            "Company Total
      SUM.
      PERFORM print_vert.
      WRITE: sy-uline.
      PERFORM print_vert.
      WRITE: 2 text-022, amtable-total UNDER text-014.
      WRITE: sy-uline.
    ENDAT.
  ENDLOOP.
ENDFORM.                    "display_table
*------------------------- DISPLAY_file --------------------------------
FORM display_file.
  SORT amtable BY bukrs anlkl anln1 anln2 afabe.            " TR842

  LOOP AT amtable.
    CLEAR reportline.
    reportline(4)      = amtable-bukrs.
    reportline+5(8)    = amtable-anlkl.
    reportline+14(12)  = amtable-anln1.
    reportline+27(4)   = amtable-anln2.
    reportline+32(4)  = amtable-afabe.                      " TR842
    reportline+37(11)  = amtable-total.                     " TR842
*  reportline+32(15)  = amtable-total.                                         TR842

    READ TABLE pstable WITH KEY objnr = amtable-objnr BINARY SEARCH.
    IF sy-subrc = '0'.
      reportline+49(11) = pstable-posid.                    " TR842
      reportline+61(40) = pstable-wbspost1.                 " TR842
      reportline+102(2) = pstable-prart.                    " TR842
      reportline+105(1) = pstable-pspri.                    " TR842
      reportline+107(8) = pstable-pspnr.                    " TR842
      reportline+116(40) = pstable-post1.                   " TR842
      reportline+157(8)  = pstable-vernr.                   " TR842
      reportline+166(4)  = pstable-stat.                    " TR842
*     reportline+50(15) = pstable-posid.                                       TR842
*     reportline+65(40) = pstable-wbspost1.                                    TR842
*     reportline+105(2) = pstable-prart.                                       TR842
*     reportline+107(1) = pstable-pspri.                                       TR842
*     reportline+108(10) = pstable-pspnr.                                      TR842
*     reportline+118(40) = pstable-post1.                                      TR842
*     reportline+158(8)  = pstable-vernr.                                      TR842
*     reportline+166(4)  = pstable-stat.                                       TR842
    ENDIF.
    WRITE: / reportline.
  ENDLOOP.
ENDFORM.                    "display_file
*------------------------- PRINT_VERT ----------------------------------
FORM print_vert.
  WRITE:  /1 sy-vline,  19 sy-vline,  36 sy-vline, 77 sy-vline, " TR842
          82 sy-vline, 99 sy-vline, 106 sy-vline, 147 sy-vline, " TR842
         156 sy-vline, 165 sy-vline, 170 sy-vline.          " TR842
ENDFORM.                    "print_vert
*-------------------------- PRINT_HEADINGS -----------------------------
FORM print_headings.
  PERFORM print_vert.
  WRITE: 2 text-010, 20 text-011,  37 text-012, 78 text-023, " TR842
        83 text-014,  100 text-016, 107 text-015, 148 text-017, " TR842
       157 text-018, 166 text-019.                          " TR842
  PERFORM print_vert.
  WRITE: 78 text-024, 166 text-025.                                           " TR842              " TR842
  WRITE: sy-uline.
ENDFORM.                    "print_headings
************************************************************************
*  top-of-page is not written, if only a file-->list is required.
TOP-OF-PAGE.
  IF check1 = ' '.
    FORMAT INTENSIFIED OFF.
    WRITE: /1 text-rpt, sy-repid, 70 text-ttl, 140 text-dte, sy-datum,
              text-amp, sy-uzeit.
    WRITE: / text-clt UNDER text-rpt, sy-mandt UNDER sy-repid, sy-sysid,
             text-tt2 UNDER text-ttl, p_gjahr,
             text-pge UNDER text-dte, sy-pagno UNDER sy-datum.
    WRITE: / text-cpy UNDER text-rpt, amtable-bukrs.
    SKIP 1.
    WRITE: sy-uline.
    FORMAT COLOR COL_BACKGROUND.
    PERFORM print_vert.
    WRITE: 2 text-013, amtable-anlkl.
    WRITE: sy-uline.
    PERFORM print_headings.
  ELSE.
    IF first_time = ' '.                               "issue log 839
      CLEAR headline.
      headline(4)      = 'CCODE'.
      headline+5(8)    = 'ASSET CL'.
      headline+14(12)  = text-010.
      headline+32(4)  = text-023.                           " TR842
      headline+37(15)  = text-014.                          " TR842
      headline+70(40)  = text-011.                          " TR842
      headline+123(40) = text-015.                          " TR842
      headline+163(8)  = text-017.                          " TR842
      headline+171(4)  = text-016.                          " TR842
*  headline+32(15)  = text-014.                                                TR842
*  headline+65(40)  = text-011.                                                TR842
*  headline+118(40) = text-015.                                                TR842
*  headline+158(8)  = text-017.                                                TR842
*  headline+166(4)  = text-016.                                                TR842
      WRITE: /1 headline.

*  clear reportline.                               "issue lof 839
*  reportline(4)      = 'CCODE'.
*  reportline+5(8)    = 'ASSET CL'.
*  reportline+14(12)  = text-010.
*  reportline+32(15)  = text-014.
*  reportline+65(40)  = text-011.
*  reportline+118(40) = text-015.
*  reportline+158(8)  = text-017.
*  reportline+166(4)  = text-016.
      first_time = 'X'.                                 "issue log 839
    ENDIF.                                             "issue log 839
  ENDIF.

*&---------------------------------------------------------------------*
*&      Form  BEREICH_VALUE      TR314
*&---------------------------------------------------------------------*
FORM bereich_value USING value(f_name)
                         afabe LIKE rbada-afabe1.
*
  DATA: l_dynnr LIKE sy-dynnr,
        l_repid LIKE sy-repid,
        l_afabe LIKE t093-afaber.
*
  l_repid = sy-cprog.
  l_dynnr = sy-dynnr.

  CALL FUNCTION 'AM_AFABE_F4'
    EXPORTING
      i_dyname   = l_repid
      i_dynumb   = l_dynnr
      i_fn_buk   = 'P_CCODE'
      i_fn_afabe = f_name
    IMPORTING
      e_afabe    = l_afabe
    EXCEPTIONS
      OTHERS     = 1.
*
  IF NOT l_afabe IS INITIAL.
    afabe = l_afabe.
  ENDIF.

*
ENDFORM.                    "BEREICH_VALUE
