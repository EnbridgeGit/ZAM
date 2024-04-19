******************************************************************
*                                                                *
*   PROGRAM: ZAPR_VENDOR_MASTER                                  *
*   AUTHOR:  Larry Ritchie                                       *
*   CREATED: 2010/01/11                                          *
*                                                                *
*   DESCRIPTION: This is a vendor report that can be ALV or      *
*                downloaded to EXCEL.                            *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YYYY/MM/DD - USERID - MOD# - DESCRIPTION                       *
* -------------------------------------------------------------- *
* 2010/01/11 LRITCHIE TR 787 New program for DBossy specs        *
*CHANGES:                                                        *
* 2012/07/31 M Khan   TR995 Change C: drive to H: drive with     *
*                           directory, file selection using F4 & *
*                           move the hard-coded file path/name to*
*                           variant.                             *
* 2014/12/16 GYmana SDP64937 Change report to allow multi-company*
*                            codes and remove a few columns      *
* 2015/10/02 SAHMAD SDP64937 Remove Bank Country                 *
* 2019/07/04 SHAFFES CHG0148817 Changes done as per CHG0148817   *
*                                to add new fields to the report *
*                                output and Selection Screen.    *
******************************************************************
******************************************************************
*CHG0199849 - Add Tax Number 5 Field(LFA1-STCD5) to the Input
*and Output display on the vendor master data report
******************************************************************

REPORT zapr_vendor_master_temp LINE-SIZE 256
NO STANDARD PAGE HEADING LINE-COUNT 90.

******************************************************************
*                   SAP TABLES                                   *
******************************************************************

TABLES: adr4,                       "Teletex
        adr6,                       "E-mail address
        lfbk,                       "Vendor banks
        lfa1,                       "Vendor master
        lfb1,                       "Vendor by company
        lfm1.                       "Vendor by purchase organization

TYPE-POOLS  slis.

******************************************************************
*                   INTERNAL TABLES                              *
******************************************************************

* vendor master - LFA1
DATA: BEGIN OF tbl_lfa1 OCCURS 0,
         lifnr      LIKE lfa1-lifnr,          "vendor
         land1      LIKE lfa1-land1,          "country
         name1      LIKE lfa1-name1,          "name
         name2      LIKE lfa1-name2,          "name
         ort01      LIKE lfa1-ort01,          "city
         pstlz      LIKE lfa1-pstlz,          "postal code
         regio      LIKE lfa1-regio,          "state or province
         sortl      LIKE lfa1-sortl,          "sort value
         stras      LIKE lfa1-stras,          "street
         adrnr      LIKE lfa1-adrnr,          "internal adress
         mcod1      LIKE lfa1-mcod1,          "name matchcode
         mcod3      LIKE lfa1-mcod3,          "city matchcode
         konzs      LIKE lfa1-konzs,          "group key
         ktokk      LIKE lfa1-ktokk,          "account group
         loevm      LIKE lfa1-loevm,          "deletion indicator
         sperr      LIKE lfa1-sperr,          "central block
         sperm      LIKE lfa1-sperm,          "purchasing block
         stcd1      LIKE lfa1-stcd1,          "tax number 1
         stcd2      LIKE lfa1-stcd2,          "tax number 2
         telf1      LIKE lfa1-telf1,          "telephone number
         telfx      LIKE lfa1-telfx,          "fax
         stcd3      LIKE lfa1-stcd3,          "tax number 3
         stcd4      LIKE lfa1-stcd4,          "tax number 4
         stcd5      TYPE lfa1-stcd5,          "tax number 5  "CHG0199849
         telx1      LIKE lfa1-telx1,          "Telex
      END OF tbl_lfa1.

* vendor by company code - LFB1
DATA: BEGIN OF tbl_lfb1 OCCURS 0,
         lifnr      LIKE lfb1-lifnr,     "vendor
         bukrs      LIKE lfb1-bukrs,     "company code
         sperr      LIKE lfb1-sperr,     "central block
         loevm      LIKE lfb1-loevm,     "deletion indicator
         akont      LIKE lfb1-akont,     "reconciliation g/l account
         zwels      LIKE lfb1-zwels,     "payment methods
         zahls      LIKE lfb1-zahls,     "block key for payment
         zterm      LIKE lfb1-zterm,     "payment terms
         busab      LIKE lfb1-busab,     "accounting clerk
         lnrzb      LIKE lfb1-lnrzb,     "alternative payee acct num
         xpore      LIKE lfb1-xpore,     "pay all items sep indicator
         altkn      LIKE lfb1-altkn,     "previous number
         xedip      LIKE lfb1-xedip,     "send payment advice by EDI
         intad      LIKE lfb1-intad,     "clerk's internet address
      END OF tbl_lfb1.

* vendor by purchasing organization - LFM1
DATA: BEGIN OF tbl_lfm1 OCCURS 0,
         lifnr      LIKE lfm1-lifnr,
         ekorg      LIKE lfm1-ekorg,
      END OF tbl_lfm1.

* vendor bank - LFBK
DATA: BEGIN OF tbl_lfbk OCCURS 0,
         lifnr      LIKE lfbk-lifnr,          "vendor
         banks      LIKE lfbk-banks,          "country
         bankl      LIKE lfbk-bankl,          "bank key
         bankn      LIKE lfbk-bankn,          "bank account number
      END OF tbl_lfbk.

**-- Start of Changes by SHAFFES CHG0148817
* Structure for ADRC
TYPES: BEGIN OF ty_adrc,
        addrnumber TYPE ad_addrnum,           "Address Number
        sort1      TYPE ad_sort1,             "Search Term 1
        sort2      TYPE ad_sort2,             "Search Term 2
      END OF ty_adrc.

DATA : gt_adrc TYPE STANDARD TABLE OF ty_adrc."Int.Table for ADRC
**-- End of Changes by SHAFFES CHG0148817

* E-mail address
DATA: BEGIN OF tbl_adr6 OCCURS 0,
         addrnumber LIKE adr6-addrnumber,     "internal address
         date_from  LIKE adr6-date_from,      "from date
         consnumber LIKE adr6-consnumber,     "sequence number
         smtp_addr  LIKE adr6-smtp_addr,      "E-mail address
      END OF tbl_adr6.

* teletex
DATA: BEGIN OF tbl_adr4 OCCURS 0,
         addrnumber LIKE adr4-addrnumber,     "internal address
         date_from  LIKE adr4-date_from,      "from date
         consnumber LIKE adr4-consnumber,     "sequence number
         ttx_number LIKE adr4-ttx_number,     "teletex number
      END OF tbl_adr4.

* temporary tables
DATA: BEGIN OF tbl_lifnr OCCURS 0,
         lifnr      LIKE lfa1-lifnr,
      END OF tbl_lifnr.

DATA: BEGIN OF tbl_lifnr2 OCCURS 0,
         lifnr      LIKE lfa1-lifnr,
         bukrs      LIKE lfb1-bukrs,
      END OF tbl_lifnr2.

DATA: BEGIN OF tbl_adrnr OCCURS 0,
         adrnr      LIKE lfa1-adrnr,          "internal address
      END OF tbl_adrnr.

* table to collect all the info for the report
DATA: BEGIN OF tbl_report OCCURS 0,
         ktokk(8)   TYPE c,                   "account group
         lifnr      LIKE lfa1-lifnr,          "vendor number
         name1      LIKE lfa1-name1,          "name
         name2      LIKE lfa1-name2,          "name
         stras      LIKE lfa1-stras,          "street
         ort01      LIKE lfa1-ort01,          "city
         regio(6)   TYPE c,                   "state or province
         land1(7)   TYPE c,                   "country
         pstlz(11)  TYPE c,                   "postal code
         telf1      LIKE lfa1-telf1,          "telephone number
         telfx      LIKE lfa1-telfx,          "fax number
         smtp_addr  LIKE adr6-smtp_addr,      "E-mail address
         ttx_number(60) TYPE c,               "teletex
         telx1      LIKE lfa1-telx1,          "Telex
         konzs      LIKE lfa1-konzs,          "corporate group
         stcd1      LIKE lfa1-stcd1,          "tax number 1
         stcd2      LIKE lfa1-stcd2,          "tax number 2
         stcd3      LIKE lfa1-stcd3,          "tax number 3
         stcd4      LIKE lfa1-stcd4,          "tax number 4
         stcd5      LIKE lfa1-stcd5,          "tax number 5  "CHG0199849
         banks(9)   TYPE c,                   "bank country
         bankl      LIKE lfbk-bankl,          "bank key
         bankn      LIKE lfbk-bankn,          "bank account
         purch_org(19) TYPE c,                "up to 4 purchase orgs
         bukrs      LIKE lfb1-bukrs,          "company code
         lnrzb      LIKE lfb1-lnrzb,          "alternate payee
         akont      LIKE lfb1-akont,          "reconciliation account
         altkn      LIKE lfb1-altkn,          "previous account number
         zterm(9)   TYPE c,                   "pay terms
         zwels      LIKE lfb1-zwels,          "payment method
         zahls(13)  TYPE c,                   "payment block key
         xedip(3)   TYPE c,                   "send by EDI
         busab(9)   TYPE c,                   "A/P clerk
         intad      LIKE lfb1-intad,          "clerk's internet address
         sperr(7)   TYPE c,                   "central block
         sperm(8)   TYPE c,                   "purchasing block
         sperr_co(7) TYPE c,                  "company block
         loevm(7)   TYPE c,                   "vendor deleted
         loevm_co(7) TYPE c,                  "company deleted
**-- Start of Changes by SHAFFES CHG0148817
         sort1       TYPE ad_sort1,           "Search Term 1
         sort2       TYPE ad_sort2,           "Search Term 2
**-- End of Changes by SHAFFES CHG0148817
       END OF tbl_report.

* EXCEL table header
DATA:  BEGIN OF tbl_excel_header OCCURS 1,
         spaltenname(20)  TYPE c,
         ddic_table(5)    TYPE c,
         ddic_field(5)    TYPE c,
         key              TYPE c,
       END OF tbl_excel_header.

*  internal table for field catalog.
DATA : tbl_fieldtab TYPE slis_t_fieldcat_alv WITH HEADER LINE,
       tbl_fieldcat TYPE slis_fieldcat_alv.

TYPES: BEGIN OF ty_lfa1,
         lifnr      LIKE lfa1-lifnr,          "vendor
         land1      LIKE lfa1-land1,          "country
         name1      LIKE lfa1-name1,          "name
         name2      LIKE lfa1-name2,          "name
         ort01      LIKE lfa1-ort01,          "city
         pstlz      LIKE lfa1-pstlz,          "postal code
         regio      LIKE lfa1-regio,          "state or province
         sortl      LIKE lfa1-sortl,          "sort value
         stras      LIKE lfa1-stras,          "street
         adrnr      LIKE lfa1-adrnr,          "internal adress
         mcod1      LIKE lfa1-mcod1,          "name matchcode
         mcod3      LIKE lfa1-mcod3,          "city matchcode
         konzs      LIKE lfa1-konzs,          "group key
         ktokk      LIKE lfa1-ktokk,          "account group
         loevm      LIKE lfa1-loevm,          "deletion indicator
         sperr      LIKE lfa1-sperr,          "central block
         sperm      LIKE lfa1-sperm,          "purchasing block
         stcd1      LIKE lfa1-stcd1,          "tax number 1
         stcd2      LIKE lfa1-stcd2,          "tax number 2
         telf1      LIKE lfa1-telf1,          "telephone number
         telfx      LIKE lfa1-telfx,          "fax
         stcd3      LIKE lfa1-stcd3,          "tax number 3
         stcd4      LIKE lfa1-stcd4,          "tax number 4
         stcd5      LIKE lfa1-stcd5,          "tax number 5 "CHG0199849
         telx1      LIKE lfa1-telx1,          "Telex
      END OF ty_lfa1.

DATA : git_lfa1_auth_chk TYPE STANDARD TABLE OF ty_lfa1,
       gwa_lfa1_auth_chk TYPE ty_lfa1.

RANGES : r_ktokk_excl FOR lfa1-ktokk.

******************************************************************
*                   VARIABLES                                    *
******************************************************************

DATA: v_prev_addrnumber  LIKE lfa1-adrnr,
      v_prev_date_from   LIKE adr6-date_from,
      v_tabix            LIKE sy-tabix
      .

* ALV stuff
DATA: st_layout TYPE slis_layout_alv,
      st_sort  TYPE slis_sortinfo_alv OCCURS 0,
      st_events TYPE slis_t_event,
      v_repid LIKE sy-repid.

DATA: st_line TYPE slis_listheader.
DATA: tbl_top_of_page TYPE slis_t_listheader.
DATA: v_head01(100) TYPE c,
      v_head02(100) TYPE c.

******************************************************************
*                   SELECTION SCREENS                            *
******************************************************************

SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS s_lifnr FOR lfa1-lifnr.              "vendor
**-- Start of Changes by SHAFFES CHG0148817
SELECT-OPTIONS s_stcd3 FOR lfa1-stcd3.              "EASL Vendor Number
**-- End of Changes by SHAFFES CHG0148817
SELECT-OPTIONS s_stcd5 FOR lfa1-stcd5.                      "CHG0199849
SELECT-OPTIONS s_mcod1 FOR lfa1-mcod1.              "name
SELECT-OPTIONS s_mcod3 FOR lfa1-mcod3.              "city
SELECT-OPTIONS s_land1 FOR lfa1-land1.              "vendor country
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS s_bukrs FOR lfb1-bukrs OBLIGATORY.   "comp code SDP64937
*PARAMETER      p_banks LIKE lfbk-banks OBLIGATORY   "bank country
*                       DEFAULT 'CA'.
PARAMETER      p_ekorg LIKE lfm1-ekorg.             "purchasing org
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN COMMENT 15(56) text-009.
SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS s_ktokk FOR lfa1-ktokk.               "account group
SELECT-OPTIONS s_konzs FOR lfa1-konzs.               "corporate group
SELECTION-SCREEN SKIP 1.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(29) text-008.       "vendor blocked
PARAMETER p_block1 RADIOBUTTON GROUP r3.
SELECTION-SCREEN COMMENT 36(10) text-005.      "all
PARAMETER p_block2 RADIOBUTTON GROUP r3.
SELECTION-SCREEN COMMENT 50(10) text-006.      "blocked - on
PARAMETER p_block3 RADIOBUTTON GROUP r3.
SELECTION-SCREEN COMMENT 64(11) text-007.      "not blocked - off
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(29) text-011.       "purchasing blocked
PARAMETER p_block4 RADIOBUTTON GROUP r4.
SELECTION-SCREEN COMMENT 36(10) text-005.      "all
PARAMETER p_block5 RADIOBUTTON GROUP r4.
SELECTION-SCREEN COMMENT 50(10) text-006.      "blocked - on
PARAMETER p_block6 RADIOBUTTON GROUP r4.
SELECTION-SCREEN COMMENT 64(11) text-007.      "not blocked - off
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(29) text-012.       "company blocked
PARAMETER p_block7 RADIOBUTTON GROUP r5.
SELECTION-SCREEN COMMENT 36(10) text-005.      "all
PARAMETER p_block8 RADIOBUTTON GROUP r5.
SELECTION-SCREEN COMMENT 50(10) text-006.      "blocked - on
PARAMETER p_block9 RADIOBUTTON GROUP r5.
SELECTION-SCREEN COMMENT 64(11) text-007.      "not blocked - off
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(29) text-004.       "vendor deletion flag
PARAMETER p_delet1 RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT 36(10) text-005.      "all
PARAMETER p_delet2 RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT 50(10) text-006.      "deleted - on
PARAMETER p_delet3 RADIOBUTTON GROUP r1.
SELECTION-SCREEN COMMENT 64(11) text-007.      "not deleted - off
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 3(29) text-010.       "company deletion flag
PARAMETER p_delet4 RADIOBUTTON GROUP r2.
SELECTION-SCREEN COMMENT 36(10) text-005.      "all
PARAMETER p_delet5 RADIOBUTTON GROUP r2.
SELECTION-SCREEN COMMENT 50(10) text-006.      "deleted - on
PARAMETER p_delet6 RADIOBUTTON GROUP r2.
SELECTION-SCREEN COMMENT 64(11) text-007.      "not deleted - off
SELECTION-SCREEN: END OF LINE.
SELECTION-SCREEN SKIP 1.

PARAMETER p_clean AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN END OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-003.
SELECTION-SCREEN SKIP 1.
PARAMETER  p_alv RADIOBUTTON GROUP r7.        "Print Report
PARAMETER  p_excel RADIOBUTTON GROUP r7.       "Excel Spreadsheet
PARAMETER p_file LIKE rlgrap-filename DEFAULT 'h:\'.     "TR995
SELECTION-SCREEN END OF BLOCK b3.

******************************************************************
*                   INITIALIZATION                               *
******************************************************************
INITIALIZATION.

*AT SELECTION-SCREEN.
*Start of TR995 changes
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_file.
  DATA: wif_window_title        TYPE string VALUE 'Please Select File',
        wif_initial_directory   TYPE string VALUE 'h:\',
        wit_filename_tab        TYPE filetable WITH HEADER LINE,
        wif_rc                  TYPE i.

  CALL METHOD cl_gui_frontend_services=>file_open_dialog
    EXPORTING
      window_title            = wif_window_title
*     DEFAULT_EXTENSION       =
*     default_filename        = wif_default_filename
*     FILE_FILTER             = WIF_FILE_FILTER
      initial_directory       = wif_initial_directory
*     MULTISELECTION          =
    CHANGING
      file_table              = wit_filename_tab[]
      rc                      = wif_rc
*     USER_ACTION             =
    EXCEPTIONS
      file_open_dialog_failed = 1
      cntl_error              = 2
      error_no_gui            = 3
      not_supported_by_gui    = 4
      OTHERS                  = 5.

  IF ( sy-subrc = 0 ).
*Return user selection
    READ TABLE wit_filename_tab INDEX 1.
    IF sy-subrc IS INITIAL AND wif_rc > 0.
      p_file = wit_filename_tab.
    ELSE.
      CLEAR p_file.
    ENDIF.
  ENDIF.

AT SELECTION-SCREEN ON p_file.                         "TR995
  PERFORM check_file_path.                             "TR995
*End of TR995 changes

******************************************************************
*                   START OF SELECTION                           *
******************************************************************
START-OF-SELECTION.

  PERFORM load_vendor_data.

  PERFORM create_report_table.

END-OF-SELECTION.

  IF tbl_report[] IS INITIAL.
    SKIP 5.
    WRITE:/15 'NO DATA TO REPORT'.
  ELSE.
    IF p_alv = 'X'.
      PERFORM build_fieldcat.
      PERFORM build_events USING st_events[].
      PERFORM display_grid.
    ENDIF.
    IF p_excel = 'X'.
      PERFORM build_excel_table.
    ENDIF.
  ENDIF.

******************************************************************
*                   SUBROUTINES                                  *
******************************************************************

*&---------------------------------------------------------------------*
*&      Form  load_vendor_data
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM load_vendor_data .

**-- Start of Changes by SHAFFES CHG0148817
* Local Data Declaration
  DATA : lt_lfa1 LIKE TABLE OF tbl_lfa1.        "Local Table for LFA1
**-- End of Changes by SHAFFES CHG0148817

  REFRESH: tbl_lfa1, tbl_lfb1, tbl_lfbk, tbl_adr6, tbl_lifnr, tbl_adrnr.
  CLEAR:   tbl_lfa1, tbl_lfb1, tbl_lfbk, tbl_adr6, tbl_lifnr, tbl_adrnr.

* get the vendor master

  SELECT lifnr land1 name1 name2 ort01 pstlz regio sortl stras adrnr mcod1 mcod3
         konzs ktokk loevm sperr sperm stcd1 stcd2 telf1 telfx stcd3 stcd4 stcd5 telx1
         INTO TABLE tbl_lfa1
         FROM lfa1
         WHERE lifnr IN s_lifnr
           AND land1 IN s_land1
           AND mcod1 IN s_mcod1
           AND mcod3 IN s_mcod3
           AND konzs IN s_konzs
           AND ktokk IN s_ktokk
**-- Start of Changes by SHAFFES CHG0148817
           AND stcd3 IN s_stcd3
           AND stcd5 IN s_stcd5.                            "CHG0199849
**-- End of Changes by SHAFFES CHG0148817

  SORT tbl_lfa1 BY lifnr.

**-- Start of Changes by SHAFFES CHG0148817
  IF tbl_lfa1[] IS NOT INITIAL.
    lt_lfa1[] = tbl_lfa1[].
    SORT lt_lfa1 BY adrnr.
    DELETE ADJACENT DUPLICATES FROM lt_lfa1 COMPARING adrnr.

* Fetch the data from ADRC Table
    SELECT addrnumber               "Address Number
           sort1                    "Search Term 1
           sort2                    "Search Term 2
      FROM adrc                     "Addresses (Business Address Services)
      INTO TABLE gt_adrc
      FOR ALL ENTRIES IN lt_lfa1
      WHERE addrnumber EQ lt_lfa1-adrnr.
    IF sy-subrc IS INITIAL.
      SORT gt_adrc BY addrnumber.
    ENDIF.
  ENDIF.
**-- End of Changes by SHAFFES CHG0148817

* Check for Account Group authorization.
  PERFORM account_grp_auth_chk.

  LOOP AT tbl_lfa1.
    tbl_lifnr-lifnr = tbl_lfa1-lifnr.
    APPEND tbl_lifnr.
  ENDLOOP.

* get the vendor by company

  IF NOT tbl_lifnr[] IS INITIAL.
    SELECT lifnr bukrs sperr loevm akont zwels zahls
           zterm busab lnrzb xpore altkn xedip intad
           INTO TABLE tbl_lfb1
           FROM lfb1
           FOR ALL ENTRIES IN tbl_lifnr
           WHERE lifnr = tbl_lifnr-lifnr
             AND bukrs IN s_bukrs.                          "SDP64937
    SORT tbl_lfb1 BY lifnr bukrs.
  ENDIF.

* get the vendor banks

  IF NOT tbl_lifnr[] IS INITIAL.
    SELECT lifnr banks bankl bankn
           INTO TABLE tbl_lfbk
           FROM lfbk
           FOR ALL ENTRIES IN tbl_lifnr
           WHERE lifnr = tbl_lifnr-lifnr.
*             AND banks = p_banks.
    SORT tbl_lfbk BY lifnr.
  ENDIF.

* get the purchasing organizations

  IF p_ekorg <> ' '.
    SELECT lifnr ekorg
           INTO TABLE tbl_lfm1
           FROM lfm1
           WHERE ekorg = p_ekorg.
  ELSE.
    SELECT lifnr ekorg
        INTO TABLE tbl_lfm1
        FROM lfm1.
  ENDIF.

  SORT tbl_lfm1 BY lifnr ekorg.

* get the E-mail addresses

  SELECT addrnumber date_from consnumber smtp_addr
         INTO TABLE tbl_adr6
         FROM adr6
         WHERE persnumber = ' '.

  SORT tbl_adr6 BY addrnumber ASCENDING date_from DESCENDING consnumber ASCENDING.

* remove old E-mail addresses but keep multiple values

  CLEAR: v_prev_addrnumber, v_prev_date_from.
  LOOP AT tbl_adr6.
    IF sy-tabix <> 1.
      IF tbl_adr6-addrnumber = v_prev_addrnumber AND tbl_adr6-date_from <> v_prev_date_from.
        DELETE tbl_adr6.
      ENDIF.
    ENDIF.
    v_prev_addrnumber = tbl_adr6-addrnumber.
    v_prev_date_from = tbl_adr6-date_from.
  ENDLOOP.

* get teletex
  SELECT addrnumber date_from consnumber ttx_number
         INTO TABLE tbl_adr4
         FROM adr4
         WHERE persnumber = ' '.
  SORT tbl_adr4 BY addrnumber ASCENDING date_from DESCENDING consnumber ASCENDING.

* remove old teletex but keep multiple values

  CLEAR: v_prev_addrnumber, v_prev_date_from.
  LOOP AT tbl_adr4.
    IF sy-tabix <> 1.
      IF tbl_adr4-addrnumber = v_prev_addrnumber AND tbl_adr4-date_from <> v_prev_date_from.
        DELETE tbl_adr4.
      ENDIF.
    ENDIF.
    v_prev_addrnumber = tbl_adr4-addrnumber.
    v_prev_date_from = tbl_adr4-date_from.
  ENDLOOP.

  FREE: tbl_lifnr, tbl_adrnr.

ENDFORM.                    " load_vendor_data

*&---------------------------------------------------------------------*
*&      Form  CREATE_REPORT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_report_table .

**-- Start of Changes by SHAFFES CHG0148817
* Local Data Declaration
  DATA : lv_adrc TYPE ty_adrc.                  "Local Variable for ADRC
**-- End of Changes by SHAFFES CHG0148817

  REFRESH tbl_report.

  LOOP AT tbl_lfa1.

    IF p_delet2 = 'X' AND tbl_lfa1-loevm = ' '.
      CONTINUE.
    ENDIF.
    IF p_delet3 = 'X' AND tbl_lfa1-loevm = 'X'.
      CONTINUE.
    ENDIF.

    IF p_block2 = 'X' AND tbl_lfa1-sperr = ' '.
      CONTINUE.
    ENDIF.
    IF p_block3 = 'X' AND tbl_lfa1-sperr = 'X'.
      CONTINUE.
    ENDIF.

    IF p_block5 = 'X' AND tbl_lfa1-sperm = ' '.
      CONTINUE.
    ENDIF.
    IF p_block6 = 'X' AND tbl_lfa1-sperm = 'X'.
      CONTINUE.
    ENDIF.

    CLEAR tbl_lfb1.
    LOOP AT tbl_lfb1 WHERE lifnr = tbl_lfa1-lifnr.

      IF p_delet5 = 'X' AND tbl_lfb1-loevm = ' '.
        CONTINUE.
      ENDIF.
      IF p_delet6 = 'X' AND tbl_lfb1-loevm = 'X'.
        CONTINUE.
      ENDIF.
      IF p_block8 = 'X' AND tbl_lfb1-sperr = ' '.
        CONTINUE.
      ENDIF.
      IF p_block9 = 'X' AND tbl_lfb1-sperr = 'X'.
        CONTINUE.
      ENDIF.

      tbl_lifnr2-lifnr = tbl_lfb1-lifnr.                    "SDP64937
      tbl_lifnr2-bukrs = tbl_lfb1-bukrs.                    "SDP64937
      APPEND tbl_lifnr2.                                    "SDP64937

    ENDLOOP.
  ENDLOOP.

* Build report detail body.

  SORT tbl_lifnr2 BY lifnr bukrs.                           "SDP64937

  LOOP AT tbl_lifnr2.                                       "SDP64937
    CLEAR: tbl_lfa1, tbl_lfb1, tbl_lfbk, tbl_report.        "SDP64937
                                                            "SDP64937
    READ TABLE tbl_lfa1 WITH KEY lifnr = tbl_lifnr2-lifnr   "SDP64937
                                   BINARY SEARCH.           "SDP64937
    READ TABLE tbl_lfb1 WITH KEY lifnr = tbl_lifnr2-lifnr   "SDP64937
                                 bukrs = tbl_lifnr2-bukrs   "SDP64937
                                   BINARY SEARCH.           "SDP64937
    READ TABLE tbl_lfbk WITH KEY lifnr = tbl_lifnr2-lifnr   "SDP64937
                                 BINARY SEARCH.             "SDP64937

    CLEAR tbl_adr6.
    READ TABLE tbl_adr6 WITH KEY addrnumber = tbl_lfa1-adrnr
                                 consnumber = 1
                                 BINARY SEARCH.
    IF sy-subrc = 0.
      tbl_report-smtp_addr = tbl_adr6-smtp_addr.
      DO.
        v_tabix = sy-tabix + 1.
        READ TABLE tbl_adr6 INDEX v_tabix.
        IF sy-subrc = 0 AND tbl_adr6-addrnumber = tbl_lfa1-adrnr.
          CONCATENATE tbl_report-smtp_addr '^' tbl_adr6-smtp_addr
             INTO tbl_report-smtp_addr.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

    CLEAR tbl_adr4.
    READ TABLE tbl_adr4 WITH KEY addrnumber = tbl_lfa1-adrnr
                                 consnumber = 1
                                 BINARY SEARCH.
    IF sy-subrc = 0.
      tbl_report-ttx_number = tbl_adr4-ttx_number.
      DO.
        v_tabix = sy-tabix + 1.
        READ TABLE tbl_adr4 INDEX v_tabix.
        IF sy-subrc = 0 AND tbl_adr4-addrnumber = tbl_lfa1-adrnr.
          CONCATENATE tbl_report-ttx_number '^' tbl_adr4-ttx_number
             INTO tbl_report-ttx_number.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

    CLEAR tbl_lfm1.
    READ TABLE tbl_lfm1 WITH KEY lifnr = tbl_lfa1-lifnr
                                 BINARY SEARCH.
    IF sy-subrc = 0.
      tbl_report-purch_org = tbl_lfm1-ekorg.
      DO.
        v_tabix = sy-tabix + 1.
        READ TABLE tbl_lfm1 INDEX v_tabix.
        IF sy-subrc = 0 AND tbl_lfm1-lifnr = tbl_lfa1-lifnr.
          CONCATENATE tbl_report-purch_org '^' tbl_lfm1-ekorg
                 INTO tbl_report-purch_org.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.
    ENDIF.

**-- Start of Changes by SHAFFES CHG0148817
    CLEAR lv_adrc.
    READ TABLE gt_adrc INTO lv_adrc
                       WITH KEY addrnumber = tbl_lfa1-adrnr
                                              BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      tbl_report-sort1 = lv_adrc-sort1.
      tbl_report-sort2 = lv_adrc-sort2.
    ENDIF.
**-- End of Changes by SHAFFES CHG0148817

    tbl_report-telx1 = tbl_lfa1-telx1.
    tbl_report-ktokk = tbl_lfa1-ktokk.
    tbl_report-lifnr = tbl_lfa1-lifnr.
    tbl_report-name1 = tbl_lfa1-name1.
    tbl_report-name2 = tbl_lfa1-name2.
    tbl_report-stras = tbl_lfa1-stras.
    tbl_report-ort01 = tbl_lfa1-ort01.
    tbl_report-regio = tbl_lfa1-regio.
    tbl_report-land1 = tbl_lfa1-land1.
    tbl_report-pstlz = tbl_lfa1-pstlz.
    tbl_report-telf1 = tbl_lfa1-telf1.
    tbl_report-telfx = tbl_lfa1-telfx.
    tbl_report-konzs = tbl_lfa1-konzs.
    tbl_report-stcd1 = tbl_lfa1-stcd1.
    tbl_report-stcd2 = tbl_lfa1-stcd2.
    tbl_report-stcd3 = tbl_lfa1-stcd3.
    tbl_report-stcd4 = tbl_lfa1-stcd4.
    tbl_report-stcd5 = tbl_lfa1-stcd5.                      "CHG0199849
    tbl_report-banks = tbl_lfbk-banks.
    tbl_report-bankl = tbl_lfbk-bankl.
    tbl_report-bankn = tbl_lfbk-bankn.
    tbl_report-bukrs = tbl_lfb1-bukrs.
    tbl_report-lnrzb = tbl_lfb1-lnrzb.
    tbl_report-akont = tbl_lfb1-akont.
    tbl_report-altkn = tbl_lfb1-altkn.
    tbl_report-zterm = tbl_lfb1-zterm.
    tbl_report-zwels = tbl_lfb1-zwels.
    tbl_report-zahls = tbl_lfb1-zahls.
    tbl_report-xedip = tbl_lfb1-xedip.
    tbl_report-busab = tbl_lfb1-busab.
    tbl_report-intad = tbl_lfb1-intad.
    tbl_report-sperr = tbl_lfa1-sperr.
    tbl_report-sperm = tbl_lfa1-sperm.
    tbl_report-sperr_co = tbl_lfb1-sperr.
    tbl_report-loevm = tbl_lfa1-loevm.
    tbl_report-loevm_co = tbl_lfb1-loevm.

    IF p_clean = 'X'.
      TRANSLATE tbl_report USING '" '.         "remove double quote for EXCEL
      TRANSLATE tbl_report USING ', '.         "remove comma for EXCEL
    ENDIF.

    APPEND tbl_report.

  ENDLOOP.                                                  "LFA1

ENDFORM.                    " CREATE_REPORT_TABLE

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       field length L = 40, M = 20, S = 10
*----------------------------------------------------------------------*
FORM build_fieldcat .

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'KTOKK'.
  tbl_fieldcat-seltext_s = 'Acct Grp'.
  tbl_fieldcat-seltext_m = 'Account Group'.
  tbl_fieldcat-seltext_l = 'Account Group'.
  tbl_fieldcat-reptext_ddic = 'Account Group'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'LIFNR'.
  tbl_fieldcat-seltext_s = 'Vendor'.
  tbl_fieldcat-seltext_m = 'Vendor Number'.
  tbl_fieldcat-seltext_l = 'Vendor Number'.
  tbl_fieldcat-reptext_ddic = 'Vendor Number'.
  tbl_fieldcat-just = 'R'.
  tbl_fieldcat-no_zero = 'X'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'NAME1'.
  tbl_fieldcat-seltext_s = 'Name 1'.
  tbl_fieldcat-seltext_m = 'Vendor Name 1'.
  tbl_fieldcat-seltext_l = 'Vendor Name 1'.
  tbl_fieldcat-reptext_ddic = 'Vendor Name 1'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'NAME2'.
  tbl_fieldcat-seltext_s = 'Name 2'.
  tbl_fieldcat-seltext_m = 'Vendor Name 2'.
  tbl_fieldcat-seltext_l = 'Vendor Name 2'.
  tbl_fieldcat-reptext_ddic = 'Vendor Name 2'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'STRAS'.
  tbl_fieldcat-seltext_s = 'Street'.
  tbl_fieldcat-seltext_m = 'Street/house'.
  tbl_fieldcat-seltext_l = 'Street/house'.
  tbl_fieldcat-reptext_ddic = 'Street/house'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'ORT01'.
  tbl_fieldcat-seltext_s = 'City'.
  tbl_fieldcat-seltext_m = 'City'.
  tbl_fieldcat-seltext_l = 'City'.
  tbl_fieldcat-reptext_ddic = 'City'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'REGIO'.
  tbl_fieldcat-seltext_s = 'Region'.
  tbl_fieldcat-seltext_m = 'Region'.
  tbl_fieldcat-seltext_l = 'Region'.
  tbl_fieldcat-reptext_ddic = 'Region'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'LAND1'.
  tbl_fieldcat-seltext_s = 'Ctry'.
  tbl_fieldcat-seltext_m = 'Country'.
  tbl_fieldcat-seltext_l = 'Country'.
  tbl_fieldcat-reptext_ddic = 'Coutry'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'PSTLZ'.
  tbl_fieldcat-seltext_s = 'Postl Code'.
  tbl_fieldcat-seltext_m = 'Postal Code'.
  tbl_fieldcat-seltext_l = 'Postal Code'.
  tbl_fieldcat-reptext_ddic = 'Postal Code'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'TELF1'.
  tbl_fieldcat-seltext_s = 'Telephone'.
  tbl_fieldcat-seltext_m = 'Telephone Number'.
  tbl_fieldcat-seltext_l = 'Telephone Number'.
  tbl_fieldcat-reptext_ddic = 'Telephone Number'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'TELFX'.
  tbl_fieldcat-seltext_s = 'Fax'.
  tbl_fieldcat-seltext_m = 'Fax'.
  tbl_fieldcat-seltext_l = 'Fax'.
  tbl_fieldcat-reptext_ddic = 'Fax'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'SMTP_ADDR'.
  tbl_fieldcat-seltext_s = 'E-mail'.
  tbl_fieldcat-seltext_m = 'E-mail Address'.
  tbl_fieldcat-seltext_l = 'E-mail Address'.
  tbl_fieldcat-reptext_ddic = 'E-mail Address'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'TTX_NUMBER'.
  tbl_fieldcat-seltext_s = 'Teletex'.
  tbl_fieldcat-seltext_m = 'Teletex'.
  tbl_fieldcat-seltext_l = 'Teletex'.
  tbl_fieldcat-reptext_ddic = 'Teletex'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.
  "LFA1-TELX1
  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'TELX1'.
  tbl_fieldcat-seltext_s = 'Vndr.Telex'.
  tbl_fieldcat-seltext_m = 'Vendor Telex'.
  tbl_fieldcat-seltext_l = 'Vendor Telex'.
  tbl_fieldcat-reptext_ddic = 'Vndr.Telex'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'KONZS'.
  tbl_fieldcat-seltext_s = 'Corp Grp'.
  tbl_fieldcat-seltext_m = 'Corporate Group'.
  tbl_fieldcat-seltext_l = 'Corporate Group'.
  tbl_fieldcat-reptext_ddic = 'Corporate Group'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

*  tbl_fieldcat-tabname   = 'TBL_REPORT'.                  "SDP64937
*  tbl_fieldcat-fieldname = 'STCD1'.                       "SDP64937
*  tbl_fieldcat-seltext_s = 'Tax Num 1'.                   "SDP64937
*  tbl_fieldcat-seltext_m = 'Tax Number 1'.                "SDP64937
*  tbl_fieldcat-seltext_l = 'Tax Number 1'.                "SDP64937
*  tbl_fieldcat-reptext_ddic = 'Tax Number 1'.             "SDP64937
*  append tbl_fieldcat to tbl_fieldtab.                    "SDP64937
*  clear tbl_fieldcat.                                     "SDP64937

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'STCD2'.
  tbl_fieldcat-seltext_s = 'Tax Num 2'.
  tbl_fieldcat-seltext_m = 'Tax Number 2'.
  tbl_fieldcat-seltext_l = 'Tax Number 2'.
  tbl_fieldcat-reptext_ddic = 'Tax Number 2'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'STCD3'.
  tbl_fieldcat-seltext_s = 'Tax Num 3'.
  tbl_fieldcat-seltext_m = 'Tax Number 3'.
  tbl_fieldcat-seltext_l = 'Tax Number 3'.
  tbl_fieldcat-reptext_ddic = 'Tax Number 3'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'STCD4'.
  tbl_fieldcat-seltext_s = 'Tax Num 4'.
  tbl_fieldcat-seltext_m = 'Tax Number 4'.
  tbl_fieldcat-seltext_l = 'Tax Number 4'.
  tbl_fieldcat-reptext_ddic = 'Tax Number 4'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.


  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'STCD5'.
  tbl_fieldcat-seltext_s = 'Tax Num 5'.
  tbl_fieldcat-seltext_m = 'Tax Number 5'.
  tbl_fieldcat-seltext_l = 'Tax Number 5'.
  tbl_fieldcat-reptext_ddic = 'Tax Number 5'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

*  tbl_fieldcat-tabname   = 'TBL_REPORT'.                  "SDP64937
*  tbl_fieldcat-fieldname = 'BANKS'.                       "SDP64937
*  tbl_fieldcat-seltext_s = 'Bank Ctry'.                   "SDP64937
*  tbl_fieldcat-seltext_m = 'Bank Country'.                "SDP64937
*  tbl_fieldcat-seltext_l = 'Bank Country'.                "SDP64937
*  tbl_fieldcat-reptext_ddic = 'Bank Country'.             "SDP64937
*  tbl_fieldcat-just = 'C'.                                "SDP64937
*  append tbl_fieldcat to tbl_fieldtab.                    "SDP64937
*  clear tbl_fieldcat.                                     "SDP64937
                                                            "SDP64937
*  tbl_fieldcat-tabname   = 'TBL_REPORT'.                  "SDP64937
*  tbl_fieldcat-fieldname = 'BANKL'.                       "SDP64937
*  tbl_fieldcat-seltext_s = 'Bank Key'.                    "SDP64937
*  tbl_fieldcat-seltext_m = 'Bank Key'.                    "SDP64937
*  tbl_fieldcat-seltext_l = 'Bank Key'.                    "SDP64937
*  tbl_fieldcat-reptext_ddic = 'Bank Key'.                 "SDP64937
*  append tbl_fieldcat to tbl_fieldtab.                    "SDP64937
*  clear tbl_fieldcat.                                     "SDP64937
                                                            "SDP64937
*  tbl_fieldcat-tabname   = 'TBL_REPORT'.                  "SDP64937
*  tbl_fieldcat-fieldname = 'BANKN'.                       "SDP64937
*  tbl_fieldcat-seltext_s = 'Bank Acct'.                   "SDP64937
*  tbl_fieldcat-seltext_m = 'Bank Account'.                "SDP64937
*  tbl_fieldcat-seltext_l = 'Bank Account Number'.         "SDP64937
*  tbl_fieldcat-reptext_ddic = 'Bank Acount Number'.       "SDP64937
*  append tbl_fieldcat to tbl_fieldtab.                    "SDP64937
*  clear tbl_fieldcat.                                     "SDP64937

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'PURCH_ORG'.
  tbl_fieldcat-seltext_s = 'Purch Org'.
  tbl_fieldcat-seltext_m = 'Purchasing Org'.
  tbl_fieldcat-seltext_l = 'Purchasing Organization'.
  tbl_fieldcat-reptext_ddic = 'Purchasing Organization'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'BUKRS'.
  tbl_fieldcat-seltext_s = 'Company'.
  tbl_fieldcat-seltext_m = 'Company Code'.
  tbl_fieldcat-seltext_l = 'Company Code'.
  tbl_fieldcat-reptext_ddic = 'Company Code'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'LNRZB'.
  tbl_fieldcat-seltext_s = 'Alt Payee'.
  tbl_fieldcat-seltext_m = 'Alternate Payee'.
  tbl_fieldcat-seltext_l = 'Alternate Payee'.
  tbl_fieldcat-reptext_ddic = 'Alternate Payee'.
  tbl_fieldcat-no_zero = 'X'.
  tbl_fieldcat-just = 'R'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'AKONT'.
  tbl_fieldcat-seltext_s = 'Rec Acct'.
  tbl_fieldcat-seltext_m = 'Reconciliation Acct'.
  tbl_fieldcat-seltext_l = 'Reconciliation Account'.
  tbl_fieldcat-reptext_ddic = 'Reconciliation Account'.
  tbl_fieldcat-no_zero = 'X'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'ALTKN'.
  tbl_fieldcat-seltext_s = 'Prev Acct'.
  tbl_fieldcat-seltext_m = 'Previous Account'.
  tbl_fieldcat-seltext_l = 'Previous Account Number'.
  tbl_fieldcat-reptext_ddic = 'Previous Account Number'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'ZTERM'.
  tbl_fieldcat-seltext_s = 'Pay Terms'.
  tbl_fieldcat-seltext_m = 'Payment Terms'.
  tbl_fieldcat-seltext_l = 'Payment Terms'.
  tbl_fieldcat-reptext_ddic = 'Payment Terms'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'ZWELS'.
  tbl_fieldcat-seltext_s = 'Pay Method'.
  tbl_fieldcat-seltext_m = 'Payment Method'.
  tbl_fieldcat-seltext_l = 'Payment Method'.
  tbl_fieldcat-reptext_ddic = 'Payment Method'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'ZAHLS'.
  tbl_fieldcat-seltext_s = 'Block Key'.
  tbl_fieldcat-seltext_m = 'Payment Block Key'.
  tbl_fieldcat-seltext_l = 'Payment Block Key'.
  tbl_fieldcat-reptext_ddic = 'Payment Block Key'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'XEDIP'.
  tbl_fieldcat-seltext_s = 'EDI'.
  tbl_fieldcat-seltext_m = 'EDI'.
  tbl_fieldcat-seltext_l = 'EDI'.
  tbl_fieldcat-reptext_ddic = 'EDI'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'BUSAB'.
  tbl_fieldcat-seltext_s = 'A/P Clerk'.
  tbl_fieldcat-seltext_m = 'A/P Clerk'.
  tbl_fieldcat-seltext_l = 'A/P Clerk'.
  tbl_fieldcat-reptext_ddic = 'A/P Clerk'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'INTAD'.
  tbl_fieldcat-seltext_s = 'Internet'.
  tbl_fieldcat-seltext_m = 'Clerks Internet'.
  tbl_fieldcat-seltext_l = 'Clerks Internet'.
  tbl_fieldcat-reptext_ddic = 'Clerks Internet Address'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'SPERR'.
  tbl_fieldcat-seltext_s = 'Blck Vend'.
  tbl_fieldcat-seltext_m = 'Blocked for Vendor'.
  tbl_fieldcat-seltext_l = 'Blocked for Vendor'.
  tbl_fieldcat-reptext_ddic = 'Blocked for Vendor'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'SPERM'.
  tbl_fieldcat-seltext_s = 'Blck Purch'.
  tbl_fieldcat-seltext_m = 'Blocked by Purch'.
  tbl_fieldcat-seltext_l = 'Blocked by Purchasing'.
  tbl_fieldcat-reptext_ddic = 'Blocked by Purchasing'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'SPERR_CO'.
  tbl_fieldcat-seltext_s = 'Blck Comp'.
  tbl_fieldcat-seltext_m = 'Blocked by Company'.
  tbl_fieldcat-seltext_l = 'Blocked by Company'.
  tbl_fieldcat-reptext_ddic = 'Blocked by Company'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'LOEVM'.
  tbl_fieldcat-seltext_s = 'DF Vendor'.
  tbl_fieldcat-seltext_m = 'Deletion Flag Vendor'.
  tbl_fieldcat-seltext_l = 'Deletion Flag by Vendor'.
  tbl_fieldcat-reptext_ddic = 'Deletion Flag by Vendor'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'LOEVM_CO'.
  tbl_fieldcat-seltext_s = 'DF Company'.
  tbl_fieldcat-seltext_m = 'Deletion Flag Comp'.
  tbl_fieldcat-seltext_l = 'Deletion Flag by Company'.
  tbl_fieldcat-reptext_ddic = 'Deletion Flag by Company'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

**-- Start of Changes by SHAFFES CHG0148817
  tbl_fieldcat-tabname   = text-013.
  tbl_fieldcat-fieldname = text-014.
  tbl_fieldcat-seltext_s = text-015.
  tbl_fieldcat-seltext_m = text-015.
  tbl_fieldcat-seltext_l = text-015.
  tbl_fieldcat-reptext_ddic = text-015.
  tbl_fieldcat-just = text-018.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = text-013.
  tbl_fieldcat-fieldname = text-016.
  tbl_fieldcat-seltext_s = text-017.
  tbl_fieldcat-seltext_m = text-017.
  tbl_fieldcat-seltext_l = text-017.
  tbl_fieldcat-reptext_ddic = text-017.
  tbl_fieldcat-just = text-018.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.
**-- End of Changes by SHAFFES CHG0148817

ENDFORM.                    " BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  BUILD_EVENTS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_st_events[]  text
*----------------------------------------------------------------------*
FORM build_events USING rt_events TYPE slis_t_event.

* Get all the events into itab rt_events
  CALL FUNCTION 'REUSE_ALV_EVENTS_GET'
    EXPORTING
      i_list_type = 0
    IMPORTING
      et_events   = rt_events.

ENDFORM.                    " BUILD_EVENTS
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_GRID
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_grid .

* Populate the layout
  v_repid = sy-repid.
  st_layout-colwidth_optimize = 'X'.
  st_layout-detail_popup = 'X'.
  st_layout-zebra = 'X'.
  st_layout-no_keyfix = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = v_repid
*     i_callback_top_of_page = gv_top_of_page
      i_callback_top_of_page = 'ALV_TOP_OF_PAGE'
      i_grid_title           = ''
      is_layout              = st_layout
      it_fieldcat            = tbl_fieldtab[]
      it_sort                = st_sort
      i_save                 = 'A'
      i_default              = 'X'
    TABLES
      t_outtab               = tbl_report[]
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.

ENDFORM.                    " DISPLAY_GRID

*&---------------------------------------------------------------------*
*&      Form  alv_top_of_page
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM alv_top_of_page.

* if you are printing the ALV, do not setup the header again
  IF tbl_top_of_page[] IS INITIAL.

    MOVE text-dte TO v_head01+0(7).
    WRITE sy-datum TO v_head01+8(10).
    MOVE text-amp  TO v_head01+19(5).
    WRITE sy-uzeit TO v_head01+25(10).

    MOVE text-clt  TO v_head02+0(7).
    MOVE sy-mandt  TO v_head02+8(4).
    MOVE sy-sysid  TO v_head02+14(5).

*1- HEADING LINE: TYPE H
    CLEAR st_line.
    st_line-typ  = 'H'.
    st_line-info = sy-title.             "sy-title.
    APPEND st_line TO tbl_top_of_page.

*2- SELECTION LINE: TYPE S
    CLEAR st_line.
    st_line-typ   = 'A'.
    st_line-key   = ''.
    st_line-info  = v_head01.
    APPEND st_line TO tbl_top_of_page.

*3- ACTION LINE:  TYPE A
    CLEAR st_line.
    st_line-typ   = 'A'.
    st_line-key   = ''.
    st_line-info = v_head02.
    APPEND st_line TO tbl_top_of_page.

  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = tbl_top_of_page.

ENDFORM.                               " ALV_TOP_OF_PAGE

*&---------------------------------------------------------------------*
*&      Form  BUILD_EXCEL_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_excel_table .

  PERFORM setup_excel_header.

  CALL FUNCTION 'MS_EXCEL_OLE_STANDARD_DAT'
    EXPORTING
*     file_name                 = 'C:\SAPTEMP' "TR995
      file_name                 = p_file               "TR995
      create_pivot              = 0
    TABLES
      data_tab                  = tbl_report
      fieldnames                = tbl_excel_header
    EXCEPTIONS
      file_not_exist            = 1
      filename_expected         = 2
      communication_error       = 3
      ole_ojbect_method_error   = 4
      ole_object_property_block = 5
      invalid_filename          = 6
      invalid_pivot_fields      = 7
      download_problem          = 8
      OTHERS                    = 9.

  IF sy-subrc <> 0.
    WRITE: /1 'table download unsuccessful - reason = ', sy-subrc.
  ENDIF.


ENDFORM.                    " BUILD_EXCEL_TABLE
*&---------------------------------------------------------------------*
*&      Form  SETUP_EXCEL_HEADER
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM setup_excel_header .

  MOVE 'ACCT GRP'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'VENDOR'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'VENDOR NAME 1'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'VENDOR NAME 2'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'STREET/HOUSE'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'CITY'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'REGION'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'COUNTRY'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'POSTAL CODE'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'TELEPHONE'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'FAX'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'E-MAIL ADDRESS'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'TELETEX'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'VENDOR TELEX'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'CORP GROUP'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
*  move 'TAX NUM 1'  to tbl_excel_header-spaltenname.      "SDP64937
*  append tbl_excel_header.                                "SDP64937
  MOVE 'TAX NUM 2'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'TAX NUM 3'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'TAX NUM 4'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'TAX NUM 5'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
*  move 'BANK CTRY'  to tbl_excel_header-spaltenname.      "SDP64937
*  append tbl_excel_header.                                "SDP64937
*  move 'BANK KEY'  to tbl_excel_header-spaltenname.       "SDP64937
*  append tbl_excel_header.                                "SDP64937
*  move 'BANK ACCOUNT'  to tbl_excel_header-spaltenname.   "SDP64937
*  append tbl_excel_header.                                "SDP64937
  MOVE 'PURCHASE ORG'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'COMPANY'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'ALT PAYEE'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'REC ACCT'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'PREV ACCT'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'PAY TERMS'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'PAY METHOD'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'PAY BLOCK KEY'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'EDI'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'A/P CLERK'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'INTERNET'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'BL VEND'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'BL PURCH'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'BL COMP'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'DF VEND'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'DF COMP'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.

ENDFORM.                    " SETUP_EXCEL_HEADER

*&---------------------------------------------------------------------*
*&      Ceck the validity of the Path    TR995
*&---------------------------------------------------------------------*
FORM check_file_path.
  DATA: sep_file TYPE string,
        sep_path TYPE string,
        lv_bol TYPE c.        "abap_bool.

*Separate Path and file
  CALL FUNCTION 'TRINT_SPLIT_FILE_AND_PATH'
    EXPORTING
      full_name     = p_file
    IMPORTING
      stripped_name = sep_file
      file_path     = sep_path
    EXCEPTIONS
      x_error       = 1
      OTHERS        = 2.
  IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  IF sep_path CS 'C:' OR sep_path CS 'c:'.
    MESSAGE ID 'ZS' TYPE 'E' NUMBER '019' WITH text-098.
  ELSE.
*Check if directory path exist or not.
    CALL METHOD cl_gui_frontend_services=>directory_exist
      EXPORTING
        directory            = sep_path
      RECEIVING
        result               = lv_bol
      EXCEPTIONS
        cntl_error           = 1
        error_no_gui         = 2
        wrong_parameter      = 3
        not_supported_by_gui = 4
        OTHERS               = 5.
    IF lv_bol IS INITIAL.
      CONCATENATE text-099 sep_path sep_file INTO sep_path.
      MESSAGE ID 'ZACC' TYPE 'E' NUMBER '101' WITH sep_path.
    ENDIF.
  ENDIF.
ENDFORM.                    "CHECK_FILE_PATH
*&---------------------------------------------------------------------*
*&      Form  ACCOUNT_GRP_AUTH_CHK
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM account_grp_auth_chk .
  IF tbl_lfa1[] IS NOT INITIAL.
    REFRESH : git_lfa1_auth_chk,
              r_ktokk_excl[].
    CLEAR   : gwa_lfa1_auth_chk,
              r_ktokk_excl.

    git_lfa1_auth_chk[] = tbl_lfa1[].
    SORT git_lfa1_auth_chk BY ktokk.
    DELETE ADJACENT DUPLICATES FROM git_lfa1_auth_chk COMPARING ktokk.

    LOOP AT git_lfa1_auth_chk INTO gwa_lfa1_auth_chk.

      IF gwa_lfa1_auth_chk-ktokk IS NOT INITIAL.
        AUTHORITY-CHECK OBJECT 'F_LFA1_GRP'
                 ID 'KTOKK' FIELD gwa_lfa1_auth_chk-ktokk
                 ID 'ACTVT' FIELD '03'.

        IF sy-subrc NE 0.
          r_ktokk_excl-low     = gwa_lfa1_auth_chk-ktokk.
          r_ktokk_excl-option  = 'EQ'.
          r_ktokk_excl-sign    = 'I'.
          APPEND r_ktokk_excl.
          CLEAR: r_ktokk_excl.
        ENDIF.

      ENDIF.

    ENDLOOP.

    IF r_ktokk_excl[] IS NOT INITIAL.
      REFRESH : git_lfa1_auth_chk.
      CLEAR   : gwa_lfa1_auth_chk.

      DELETE tbl_lfa1 WHERE ktokk IN r_ktokk_excl[].
    ENDIF.
  ENDIF.
ENDFORM.                    " ACCOUNT_GRP_AUTH_CHK
