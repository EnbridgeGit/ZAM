******************************************************************
*                                                                *
*   PROGRAM: ZAPR_VENDOR_MASTER2                                 *
*   AUTHOR:  Glenn Ymana                                         *
*   CREATED: 2014/12/16                                          *
*                                                                *
*   DESCRIPTION: This is a vendor report that can be ALV or      *
*                downloaded to EXCEL.                            *
******************************************************************
*        M  O  D  I  F  I  C  A  T  I  O  N       L  O  G        *
* YYYY/MM/DD USERID  MOD#     DESCRIPTION                        *
* -------------------------------------------------------------- *
* 2014/12/16 GYMANA  SDP64937 New program                        *
* 2016/07/07 JRHARTU ACR-1806 D30K927040, D30K927042             *
*                             Fix report - clear structure       *
******************************************************************

REPORT zapr_vendor_master2 LINE-SIZE 256
NO STANDARD PAGE HEADING LINE-COUNT 90.

******************************************************************
*                   SAP TABLES                                   *
******************************************************************

TABLES: adr4,                       "Teletex
        adr6,                       "E-mail address
        adcp,
        lfbk,                       "Vendor banks
        lfa1,                       "Vendor master
        lfb1,                       "Vendor by company
        lfm1,                       "Vendor by purchase organization
        tiban,
        bnka,
        lfza,
        lfbw,
        knvk.

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
         qland      LIKE lfb1-qland,     "Withholding tax country key
         xedip      LIKE lfb1-xedip,     "send payment advice by EDI
         intad      LIKE lfb1-intad,     "clerk's internet address
      END OF tbl_lfb1.

* vendor bank - LFBK
DATA: BEGIN OF tbl_lfbk OCCURS 0,
         lifnr      LIKE lfbk-lifnr,     "vendor
         banks      LIKE lfbk-banks,     "country
         bankl      LIKE lfbk-bankl,     "bank key
         bankn      LIKE lfbk-bankn,     "bank acct no
         koinh      LIKE lfbk-koinh,     "acct holder
         bkont      LIKE lfbk-bkont,     "acct key
         iban       LIKE tiban-iban,     "IBAN value
         bvtyp      LIKE lfbk-bvtyp,     "Partner Bank Type
         banka      LIKE bnka-banka,     "Bank Name
         bkref      LIKE lfbk-bkref,     "Reference details
      END OF tbl_lfbk.

* Permitted Payee - payee
DATA: BEGIN OF tbl_payee OCCURS 0,
         lifnr      LIKE lfb1-lifnr,     "Vendor
         bukrs      LIKE lfb1-bukrs,     "Company code
         empfk      LIKE lfza-empfk,     "Permitted Payee - Payee
         payname    LIKE lfa1-name1,     "Permitted Payee - Name
         ort01      LIKE lfa1-ort01,     "Permitted Payee - City
      END OF tbl_payee.

DATA: tbl_payee_temp LIKE tbl_payee OCCURS 0 WITH HEADER LINE.

* Withholding Tax - LFBW
DATA: BEGIN OF tbl_lfbw OCCURS 0,
         lifnr      LIKE lfbw-lifnr,        "Vendor
         bukrs      LIKE lfbw-bukrs,        "Company Code
         qland      LIKE lfb1-qland,        "Withholding Tax Cntry
         witht      LIKE lfbw-witht,        "Withholding Tax type
         wt_withcd  LIKE lfbw-wt_withcd,    "Withholding Tax code
         wt_subjct  LIKE lfbw-wt_subjct,    "Withholding Tax liable
         qsrec      LIKE lfbw-qsrec,        "Withholding Tax recipient
         wt_wtstcd  LIKE lfbw-wt_wtstcd,    "Withholding Tax ID
         wt_exdt    LIKE lfbw-wt_exdt,      "Exempt to
      END OF tbl_lfbw.

DATA: tbl_lfbw_temp LIKE tbl_lfbw OCCURS 0 WITH HEADER LINE.

* Contact Persons - knvk
DATA: BEGIN OF tbl_knvk OCCURS 0,
         lifnr      LIKE knvk-lifnr,        "Vendor number
         adrnp_2    LIKE knvk-adrnp_2,      "Address number
         prsnr      LIKE knvk-prsnr,        "Person number
         parnr      LIKE knvk-parnr,        "Number of Contact Person
         abtnr      LIKE knvk-abtnr,        "Contact Persons Dept
         pafkt      LIKE knvk-pafkt,        "Contact Persons Function
         namev      LIKE knvk-namev,        "Contact Persons First Name
         name1      LIKE knvk-name1,        "Contact Persons Last Name
         telf1      LIKE knvk-telf1,        "Contact Persons Telephone
      END OF tbl_knvk.

* Contact Persons - contper
DATA: BEGIN OF tbl_contper OCCURS 0,
         lifnr      LIKE knvk-lifnr,        "Vendor number
         adrnp_2    LIKE knvk-adrnp_2,      "Address number
         prsnr      LIKE knvk-prsnr,        "Person number
         parnr      LIKE knvk-parnr,        "Number of Contact Person
         abtnr      LIKE knvk-abtnr,        "Contact Persons Dept
         pafkt      LIKE knvk-pafkt,        "Contact Persons Function
         namev      LIKE knvk-namev,        "Contact Persons First Name
         contname1  LIKE knvk-name1,        "Contact Persons Last Name
         telf1      LIKE knvk-telf1,        "Contact Persons Telephone
         fax_number LIKE adcp-fax_number,
         smtp_addr  LIKE adr6-smtp_addr,
         deflt_comm LIKE adcp-deflt_comm,
      END OF tbl_contper.

DATA: tbl_contper_temp LIKE tbl_contper OCCURS 0 WITH HEADER LINE.

* Fax Number, Default Comm Method
DATA: BEGIN OF tbl_adcp OCCURS 0,
         persnumber LIKE adcp-persnumber,     "person number
         fax_number LIKE adcp-fax_number,     "fax number
         deflt_comm LIKE adcp-deflt_comm,     "Default Comm Method
      END OF tbl_adcp.

* Fax Number, Default Comm Method
DATA: BEGIN OF tbl_adr6 OCCURS 0,
         persnumber LIKE adr6-persnumber,     "person number
         smtp_addr  LIKE adr6-smtp_addr,      "E-mail address
      END OF tbl_adr6.

* temporary tables
DATA: BEGIN OF tbl_lifnr OCCURS 0,
         lifnr      LIKE lfa1-lifnr,
      END OF tbl_lifnr.

DATA: BEGIN OF tbl_lifnr2 OCCURS 0,
         lifnr      LIKE lfa1-lifnr,
         bukrs      LIKE lfb1-bukrs,
      END OF tbl_lifnr2.

* table to collect partial vendor info for the report
DATA: BEGIN OF tbl_temp_rpt OCCURS 0,
         lifnr      LIKE lfa1-lifnr,        "vendor number
         name1      LIKE lfa1-name1,        "name
         name2      LIKE lfa1-name2,        "name
         bukrs      LIKE lfb1-bukrs,        "company code
         ktokk      TYPE lfa1-ktokk,        "account group
         banks      LIKE lfbk-banks,        "Bank Details Bank Country
         bankl      LIKE lfbk-bankl,        "Bank Details Bank Key
         bankn      LIKE lfbk-bankn,        "Bank Details Bank Account
         koinh      LIKE lfbk-koinh,        "Bank Details Acct Holder
         bkont      LIKE lfbk-bkont,        "Bank Details Acct Key
         iban       LIKE tiban-iban,        "Bank Details IBAN Value
         bvtyp      LIKE lfbk-bvtyp,        "Bank Details BCat
         banka      LIKE bnka-banka,        "Bank Details Bank Name
         bkref      LIKE lfbk-bkref,        "Reference Details
       END OF tbl_temp_rpt.

* table to collect all the info for the report
DATA: BEGIN OF tbl_report OCCURS 0,
         lifnr      LIKE lfa1-lifnr,        "vendor number
         name1      LIKE lfa1-name1,        "name
         name2      LIKE lfa1-name2,        "name
         bukrs      LIKE lfb1-bukrs,        "company code
         ktokk      TYPE lfa1-ktokk,        "account group
         banks      LIKE lfbk-banks,        "Bank Details Bank Country
         bankl      LIKE lfbk-bankl,        "Bank Details Bank Key
         bankn      LIKE lfbk-bankn,        "Bank Details Bank Account
         koinh      LIKE lfbk-koinh,        "Bank Details Acct Holder
         bkont      LIKE lfbk-bkont,        "Bank Details Acct Key
         iban       LIKE tiban-iban,        "Bank Details IBAN Value
         bvtyp      LIKE lfbk-bvtyp,        "Bank Details BCat
         banka      LIKE bnka-banka,        "Bank Details Bank Name
         bkref      LIKE lfbk-bkref,        "Reference Details
         empfk      LIKE lfza-empfk,        "Permitted Payee - Payee
         payname    LIKE lfa1-name1,        "Permitted Payee - Name
         ort01      LIKE lfa1-ort01,        "Permitted Payee - City
         qland      LIKE lfb1-qland,        "Withholding Tax country
         witht      LIKE lfbw-witht,        "Withholding Tax type
         wt_withcd  LIKE lfbw-wt_withcd,    "Withholding Tax code
         wt_subjct  LIKE lfbw-wt_subjct,    "Withholding Tax liable
         qsrec      LIKE lfbw-qsrec,        "Withholding Tax recipient
         wt_wtstcd  LIKE lfbw-wt_wtstcd,    "Withholding Tax ID
         wt_exdt    LIKE lfbw-wt_exdt,      "Exempt to
         abtnr      LIKE knvk-abtnr,        "Contact Persons Dept
         pafkt      LIKE knvk-pafkt,        "Contact Persons Function
         namev      LIKE knvk-namev,        "Contact Persons First Name
         contname1  LIKE knvk-name1,        "Contact Persons Last Name
         telf1      LIKE knvk-telf1,        "Contact Persons Telephone
         fax_number LIKE adcp-fax_number,   "Contact Persons Fax
         smtp_addr  LIKE adr6-smtp_addr,    "Contact Persons Email
         deflt_comm LIKE adcp-deflt_comm,   "Contact Persons comm Methd
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
      END OF ty_lfa1.

DATA : git_lfa1_auth_chk TYPE STANDARD TABLE OF ty_lfa1,
       gwa_lfa1_auth_chk TYPE ty_lfa1.

RANGES : r_ktokk_excl FOR lfa1-ktokk.

******************************************************************
*                   VARIABLES                                    *
******************************************************************

DATA: v_prev_addrnumber  LIKE lfa1-adrnr,
      v_prev_date_from   LIKE adr6-date_from,
      v_tabix            LIKE sy-tabix,
      v_payee_rows       LIKE sy-tabix,
      v_lfbw_rows        LIKE sy-tabix,
      v_knvk_rows        LIKE sy-tabix,
      v_rpt_row          LIKE sy-tabix,
      v_lfbk_rows        LIKE sy-tabix,
      v_empfk            LIKE lfza-empfk,
      v_knvk_flag(1)     TYPE c VALUE 'N'
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
SELECT-OPTIONS s_mcod1 FOR lfa1-mcod1.              "name
SELECT-OPTIONS s_mcod3 FOR lfa1-mcod3.              "city
SELECT-OPTIONS s_land1 FOR lfa1-land1.              "vendor country
SELECTION-SCREEN SKIP 1.
SELECT-OPTIONS: s_bukrs FOR lfb1-bukrs OBLIGATORY,   "comp code SDP64937
*PARAMETER      p_banks LIKE lfbk-banks OBLIGATORY   "bank country
*                       DEFAULT 'CA'.
                s_banks FOR lfbk-banks OBLIGATORY
                        DEFAULT 'CA'.
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

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE text-017.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(29) text-013.       "Bank Details
PARAMETER p_bnkdet AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(29) text-014.       "Permitted Payee
PARAMETER p_payee AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(29) text-015.       "Withholding Tax
PARAMETER p_wtax  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN: BEGIN OF LINE.
SELECTION-SCREEN COMMENT 1(29) text-016.       "Contact Persons
PARAMETER p_person  AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN: END OF LINE.

SELECTION-SCREEN END OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE text-003.
SELECTION-SCREEN SKIP 1.
PARAMETER  p_alv RADIOBUTTON GROUP r7.        "Print Report
PARAMETER  p_excel RADIOBUTTON GROUP r7.       "Excel Spreadsheet
PARAMETER p_file LIKE rlgrap-filename DEFAULT 'h:\'.     "TR995
SELECTION-SCREEN END OF BLOCK b4.

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

  REFRESH: tbl_lfa1, tbl_lfb1, tbl_lfbk, tbl_lfbw, tbl_payee,
           tbl_knvk, tbl_lifnr, tbl_lifnr2.
  CLEAR:   tbl_lfa1, tbl_lfb1, tbl_lfbk, tbl_lfbw, tbl_payee,
           tbl_knvk, tbl_lifnr, tbl_lifnr2.
  data: lt_lfbk  TYPE TABLE OF lfbk,
        ls_lfbk  TYPE lfbk,
        lt_tiban TYPE TABLE OF tiban,
        ls_tiban TYPE tiban,
        lt_bnka  TYPE TABLE OF bnka,
        ls_bnka  TYPE bnka,
        ls_adcp  TYPE adcp,
        ls_adr6  TYPE adr6.
* get the vendor master

  SELECT lifnr land1 name1 name2 ort01 pstlz regio sortl stras adrnr
         mcod1 mcod3 konzs ktokk loevm sperr sperm stcd1 stcd2 telf1
         telfx stcd3 stcd4
         INTO TABLE tbl_lfa1
         FROM lfa1
         WHERE lifnr IN s_lifnr
           AND land1 IN s_land1
           AND mcod1 IN s_mcod1
           AND mcod3 IN s_mcod3
           AND konzs IN s_konzs
           AND ktokk IN s_ktokk.

  SORT tbl_lfa1 BY lifnr.

* Check for Account Group authorization.
  PERFORM account_grp_auth_chk.

  LOOP AT tbl_lfa1.
    tbl_lifnr-lifnr = tbl_lfa1-lifnr.
    APPEND tbl_lifnr.
  ENDLOOP.

* get the vendor by company

  IF NOT tbl_lifnr[] IS INITIAL.
    SELECT lifnr bukrs sperr loevm akont zwels zahls
           zterm busab lnrzb xpore altkn qland xedip intad
           INTO TABLE tbl_lfb1
           FROM lfb1
           FOR ALL ENTRIES IN tbl_lifnr
           WHERE lifnr = tbl_lifnr-lifnr
             AND bukrs IN s_bukrs.
    SORT tbl_lfb1 BY lifnr bukrs.
  ENDIF.
***Begin of Comment
*  LOOP AT tbl_lifnr.
*    SELECT *
*      FROM lfbk
*     WHERE lifnr = tbl_lifnr-lifnr
*       AND banks in s_banks. "= p_banks.
*    ENDSELECT.
*
*    IF sy-subrc = 0.
*      MOVE-CORRESPONDING lfbk TO tbl_lfbk.
*
*      SELECT *
*       FROM tiban
*      WHERE banks = tbl_lfbk-banks
*        AND bankl = tbl_lfbk-bankl
*        AND bankn = tbl_lfbk-bankn.
*      ENDSELECT.
*
*      MOVE tiban-iban TO tbl_lfbk-iban.
*
*      SELECT *
*        FROM bnka
*       WHERE banks = tbl_lfbk-banks
*         AND bankl = tbl_lfbk-bankl.
*      ENDSELECT.
*
*      MOVE bnka-banka TO tbl_lfbk-banka.
*      APPEND tbl_lfbk.
*    ENDIF.
*  ENDLOOP.
*  SORT tbl_lfbk BY lifnr.
*****End of Comment
  CLEAR: lt_lfbk,
         lt_tiban,
         lt_bnka.
  IF tbl_lifnr[] IS NOT INITIAL.
    SELECT * FROM lfbk INTO TABLE lt_lfbk
                       FOR ALL ENTRIES IN tbl_lifnr
                            WHERE lifnr = tbl_lifnr-lifnr
                              AND banks in s_banks.
    IF lt_lfbk[] IS NOT INITIAL.
*    SELECT *
*       FROM tiban INTO TABLE lt_tiban
*       FOR ALL ENTRIES IN lt_lfbk
*      WHERE banks = lt_lfbk-banks
*        AND bankl = lt_lfbk-bankl
*        AND bankn = lt_lfbk-bankn.
      SELECT *
          FROM bnka INTO TABLE lt_bnka
          FOR ALL ENTRIES IN lt_lfbk
         WHERE banks = lt_lfbk-banks
           AND bankl = lt_lfbk-bankl.
      LOOP AT lt_lfbk INTO ls_lfbk.
        CLEAR: ls_tiban,
               ls_bnka.
        MOVE-CORRESPONDING ls_lfbk TO tbl_lfbk.
*         READ TABLE lt_tiban INTO ls_tiban
*                             with key banks = ls_lfbk-banks
*                                      bankl = ls_lfbk-bankl
*                                      bankn = ls_lfbk-bankn.
        SELECT SINGLE * FROM tiban INTO ls_tiban
                        WHERE banks = tbl_lfbk-banks
                          AND bankl = tbl_lfbk-bankl
                          AND bankn = tbl_lfbk-bankn.
        tbl_lfbk-iban = ls_tiban-iban.
        READ TABLE lt_bnka INTO ls_bnka
                           WITH KEY banks = ls_lfbk-banks
                                    bankl = ls_lfbk-bankl.
        tbl_lfbk-banka = ls_bnka-banka.
        APPEND tbl_lfbk.
      ENDLOOP.
    ENDIF.
  ENDIF.
  SORT tbl_lfbk BY lifnr.
* Permitted Payee detail
  IF NOT tbl_lfb1[] IS INITIAL.
    LOOP AT tbl_lfb1.

      MOVE tbl_lfb1-lifnr TO tbl_payee-lifnr.
      MOVE tbl_lfb1-bukrs TO tbl_payee-bukrs.
      READ TABLE tbl_lfa1 WITH KEY lifnr = tbl_lfb1-lifnr
                                   BINARY SEARCH.
      MOVE tbl_lfa1-name1 TO tbl_payee-payname.
      MOVE tbl_lfa1-ort01 TO tbl_payee-ort01.

      SELECT *
        FROM lfza
       WHERE lifnr = tbl_lfb1-lifnr
         AND bukrs = tbl_lfb1-bukrs.

        IF sy-subrc = 0.
          MOVE lfza-empfk TO tbl_payee-empfk.
          APPEND tbl_payee.
        ENDIF.
      ENDSELECT.

    ENDLOOP.

    SORT tbl_lfbw BY lifnr bukrs.
  ENDIF.

* Withholding Tax Data

  IF NOT tbl_lfb1[] IS INITIAL.
    LOOP AT tbl_lfb1.

      SELECT *
        FROM lfbw
       WHERE lifnr = tbl_lfb1-lifnr
         AND bukrs = tbl_lfb1-bukrs.

        IF sy-subrc = 0.
          MOVE-CORRESPONDING lfbw TO tbl_lfbw.
          MOVE tbl_lfb1-qland TO tbl_lfbw-qland.
          APPEND tbl_lfbw.
        ENDIF.
      ENDSELECT.

    ENDLOOP.

    SORT tbl_lfbw BY lifnr bukrs.
  ENDIF.

* Get Contact Person Data

  IF NOT tbl_lfb1[] IS INITIAL.
    SELECT lifnr adrnp_2 prsnr parnr
           abtnr pafkt namev name1 telf1
           INTO TABLE tbl_knvk
           FROM knvk
           FOR ALL ENTRIES IN tbl_lfb1
           WHERE lifnr = tbl_lfb1-lifnr.
    SORT tbl_knvk BY lifnr.
  ENDIF.

  IF NOT tbl_knvk[] IS INITIAL.

    LOOP AT tbl_knvk.

      CLEAR                                tbl_contper.
      MOVE-CORRESPONDING tbl_knvk       TO tbl_contper.
      MOVE               tbl_knvk-name1 TO tbl_contper-contname1.

      CLEAR    ls_adcp.
      SELECT * FROM adcp
       WHERE persnumber = tbl_knvk-prsnr.
        CLEAR            ls_adcp.
        MOVE     adcp TO ls_adcp.
      ENDSELECT.
      IF     ( sy-subrc EQ 0 ).
        MOVE-CORRESPONDING ls_adcp TO tbl_contper.
      ELSE.
        CLEAR  ls_adcp.
      ENDIF.

      CLEAR    ls_adr6.
      SELECT * FROM adr6
       WHERE persnumber = tbl_knvk-prsnr.
        CLEAR            ls_adr6.
        MOVE     adr6 TO ls_adr6.
      ENDSELECT.
      IF     ( sy-subrc EQ 0 ).
        MOVE-CORRESPONDING ls_adr6 TO tbl_contper.
      ELSE.
        CLEAR  ls_adr6.
      ENDIF.

      APPEND tbl_contper.

    ENDLOOP.
    SORT tbl_contper BY lifnr.

  ENDIF.

  FREE: tbl_lifnr.

ENDFORM.                    " load_vendor_data

*&---------------------------------------------------------------------*
*&      Form  CREATE_REPORT_TABLE
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM create_report_table .

  DATA: lt_lfbk_temp like TABLE OF tbl_lfbk,
        ls_lfbk_temp LIKE LINE OF tbl_lfbk.

  REFRESH: tbl_report, tbl_lifnr2, tbl_temp_rpt, tbl_payee_temp,
           tbl_lfbw_temp, tbl_contper_temp.

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

      tbl_lifnr2-lifnr = tbl_lfb1-lifnr.
      tbl_lifnr2-bukrs = tbl_lfb1-bukrs.
      APPEND tbl_lifnr2.

    ENDLOOP.
  ENDLOOP.

* Build report detail body.

  SORT tbl_lifnr2 BY lifnr bukrs.
  CLEAR tbl_temp_rpt.

  LOOP AT tbl_lifnr2.
    CLEAR: tbl_lfa1, tbl_lfb1, tbl_lfbk.

    READ TABLE tbl_lfa1 WITH KEY lifnr = tbl_lifnr2-lifnr
                                   BINARY SEARCH.
    READ TABLE tbl_lfb1 WITH KEY lifnr = tbl_lifnr2-lifnr
                                 bukrs = tbl_lifnr2-bukrs
                                   BINARY SEARCH.
*    READ TABLE tbl_lfbk WITH KEY lifnr = tbl_lifnr2-lifnr
*                                 BINARY SEARCH.
    MOVE-CORRESPONDING tbl_lfa1 TO tbl_temp_rpt.
    MOVE-CORRESPONDING tbl_lfb1 TO tbl_temp_rpt.
*    IF p_bnkdet = 'X'.
*      MOVE-CORRESPONDING tbl_lfbk TO tbl_temp_rpt.
*    ENDIF.
    MOVE-CORRESPONDING tbl_lifnr2 TO tbl_temp_rpt.
    APPEND tbl_temp_rpt.
  ENDLOOP.

  SORT tbl_temp_rpt BY lifnr bukrs.

  CLEAR tbl_report.

  LOOP AT tbl_temp_rpt.

    REFRESH: tbl_payee_temp, tbl_lfbw_temp, tbl_contper_temp.
    CLEAR:   tbl_payee_temp, tbl_lfbw_temp, tbl_contper_temp.

* Extract payee, lfbw, & knvk rows pertaining to vendor.
* Determine number of rows in payee, lfbw and knvk to determine number
* of report rows needed to display all rows from LFBW & KNVK.

    MOVE 0  TO v_payee_rows.
    MOVE 0  TO v_lfbw_rows.
    MOVE 0  TO v_knvk_rows.
    MOVE 0  TO v_lfbk_rows.
    MOVE +1 TO v_tabix.

    IF p_payee = 'X'.
      LOOP AT tbl_payee WHERE lifnr = tbl_temp_rpt-lifnr
                          AND bukrs = tbl_temp_rpt-bukrs.
        v_payee_rows = v_payee_rows + 1.
        MOVE-CORRESPONDING tbl_payee TO tbl_payee_temp.
        APPEND tbl_payee_temp.
      ENDLOOP.
      SORT tbl_payee_temp BY lifnr bukrs.
    ENDIF.

    IF v_payee_rows > v_tabix.
      v_tabix = v_payee_rows.
    ENDIF.

    IF p_wtax = 'X'.
      LOOP AT tbl_lfbw WHERE lifnr = tbl_temp_rpt-lifnr
                         AND bukrs = tbl_temp_rpt-bukrs.
        v_lfbw_rows = v_lfbw_rows + 1.
        MOVE-CORRESPONDING tbl_lfbw TO tbl_lfbw_temp.
        APPEND tbl_lfbw_temp.
      ENDLOOP.
      SORT tbl_lfbw_temp BY lifnr bukrs.
    ENDIF.

    IF v_lfbw_rows > v_tabix.
      v_tabix = v_lfbw_rows.
    ENDIF.

    IF p_person = 'X'.
      LOOP AT tbl_contper WHERE lifnr = tbl_temp_rpt-lifnr.
        v_knvk_rows = v_knvk_rows + 1.
        MOVE-CORRESPONDING tbl_contper TO tbl_contper_temp.
        APPEND tbl_contper_temp.
      ENDLOOP.
      SORT tbl_contper_temp BY lifnr.
    ENDIF.

    IF v_knvk_rows > v_tabix.
      v_tabix = v_knvk_rows.
    ENDIF.
*Bank
    IF p_bnkdet = 'X'.
      clear lt_lfbk_temp.
      LOOP AT tbl_lfbk WHERE lifnr = tbl_temp_rpt-lifnr.
        v_lfbk_rows = v_lfbk_rows + 1.
        append tbl_lfbk to lt_lfbk_temp.
      ENDLOOP.
      IF v_lfbk_rows > v_tabix.
        v_tabix = v_lfbk_rows.
      ENDIF.
    ENDIF.
*end bank
* Build report rows.
    MOVE 0 TO v_rpt_row.

    DO.
      CLEAR tbl_report.

      ADD +1 TO v_rpt_row.
      IF v_rpt_row > v_tabix.
        EXIT.
      ENDIF.

      MOVE-CORRESPONDING tbl_temp_rpt TO tbl_report.

      READ TABLE tbl_payee_temp INDEX v_rpt_row.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING tbl_payee_temp TO tbl_report.
      ENDIF.

      READ TABLE tbl_lfbw_temp INDEX v_rpt_row.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING tbl_lfbw_temp TO tbl_report.
      ENDIF.

      READ TABLE tbl_contper_temp INDEX v_rpt_row.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING tbl_contper_temp TO tbl_report.
      ENDIF.
*Bank
      CLEAR ls_lfbk_temp.
      READ TABLE lt_lfbk_temp INTO ls_lfbk_temp INDEX v_rpt_row.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING ls_lfbk_temp TO tbl_report.
      ENDIF.
*End Bank
      APPEND tbl_report.
    ENDDO.

  ENDLOOP.

  IF p_clean = 'X'.
    TRANSLATE tbl_report USING '" '.         "remove double quote for EXCEL
    TRANSLATE tbl_report USING ', '.         "remove comma for EXCEL
  ENDIF.

ENDFORM.                    " CREATE_REPORT_TABLE

*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCAT
*&---------------------------------------------------------------------*
*       field length L = 40, M = 20, S = 10
*----------------------------------------------------------------------*
FORM build_fieldcat .

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
  tbl_fieldcat-fieldname = 'BUKRS'.
  tbl_fieldcat-seltext_s = 'Company'.
  tbl_fieldcat-seltext_m = 'Company Code'.
  tbl_fieldcat-seltext_l = 'Company Code'.
  tbl_fieldcat-reptext_ddic = 'Company Code'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'KTOKK'.
  tbl_fieldcat-seltext_s = 'Acct Grp'.
  tbl_fieldcat-seltext_m = 'Account Group'.
  tbl_fieldcat-seltext_l = 'Account Group'.
  tbl_fieldcat-reptext_ddic = 'Account Group'.
  tbl_fieldcat-just = 'C'.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

* Bank Details

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'BANKS'.
  tbl_fieldcat-seltext_s = 'Bank Ctry'.
  tbl_fieldcat-seltext_m = 'Bank Country'.
  tbl_fieldcat-seltext_l = 'Bank Country'.
  tbl_fieldcat-reptext_ddic = 'Bank Country'.
  tbl_fieldcat-just = 'C'.
  IF p_bnkdet = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'BANKL'.
  tbl_fieldcat-seltext_s = 'Bank Key'.
  tbl_fieldcat-seltext_m = 'Bank Key'.
  tbl_fieldcat-seltext_l = 'Bank Key'.
  tbl_fieldcat-reptext_ddic = 'Bank Key'.
  IF p_bnkdet = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'BANKN'.
  tbl_fieldcat-seltext_s = 'Bank Acct'.
  tbl_fieldcat-seltext_m = 'Bank Account'.
  tbl_fieldcat-seltext_l = 'Bank Account Number'.
  tbl_fieldcat-reptext_ddic = 'Bank Acount Number'.
  IF p_bnkdet = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'BKREF'.
  tbl_fieldcat-seltext_s = 'Bank Ref.dtl'.
  tbl_fieldcat-seltext_m = 'Bank Ref.Dtls'.
  tbl_fieldcat-seltext_l = 'Bank Reference Details'.
  tbl_fieldcat-reptext_ddic = 'Bank Reference Details'.
  IF p_bnkdet = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'KOINH'.
  tbl_fieldcat-seltext_s = 'Acct Holder'.
  tbl_fieldcat-seltext_m = 'Acct Holder'.
  tbl_fieldcat-seltext_l = 'Acct Holder'.
  tbl_fieldcat-reptext_ddic = 'Acct Holder'.
  IF p_bnkdet = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'BKONT'.
  tbl_fieldcat-seltext_s = 'Key'.
  tbl_fieldcat-seltext_m = 'Acct Key'.
  tbl_fieldcat-seltext_l = 'Acct Key'.
  tbl_fieldcat-reptext_ddic = 'Acct Key'.
  IF p_bnkdet = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'IBAN'.
  tbl_fieldcat-seltext_s = 'IBAN'.
  tbl_fieldcat-seltext_m = 'IBAN Value'.
  tbl_fieldcat-seltext_l = 'IBAN Value'.
  tbl_fieldcat-reptext_ddic = 'IBAN Value'.
  IF p_bnkdet = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'BVTYP'.
  tbl_fieldcat-seltext_s = 'BCat'.
  tbl_fieldcat-seltext_m = 'Bank Type'.
  tbl_fieldcat-seltext_l = 'Bank Type'.
  tbl_fieldcat-reptext_ddic = 'Bank Type'.
  IF p_bnkdet = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'BANKA'.
  tbl_fieldcat-seltext_s = 'Bank Name'.
  tbl_fieldcat-seltext_m = 'Bank Name'.
  tbl_fieldcat-seltext_l = 'Bank Name'.
  tbl_fieldcat-reptext_ddic = 'Bank Name'.
  IF p_bnkdet = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

* Permitted Payee Details

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'EMPFK'.
  tbl_fieldcat-seltext_s = 'Payee'.
  tbl_fieldcat-seltext_m = 'Payee'.
  tbl_fieldcat-seltext_l = 'Payee'.
  tbl_fieldcat-reptext_ddic = 'Payee'.
  tbl_fieldcat-just = 'R'.
  tbl_fieldcat-no_zero = 'X'.
  IF p_payee = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'PAYNAME'.
  tbl_fieldcat-seltext_s = 'Pay Name'.
  tbl_fieldcat-seltext_m = 'Payee Name'.
  tbl_fieldcat-seltext_l = 'Payee Name'.
  tbl_fieldcat-reptext_ddic = 'Payee Name'.
  IF p_payee = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'ORT01'.
  tbl_fieldcat-seltext_s = 'City'.
  tbl_fieldcat-seltext_m = 'City'.
  tbl_fieldcat-seltext_l = 'City'.
  tbl_fieldcat-reptext_ddic = 'City'.
  IF p_payee = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

* Withholding Tax Detail

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'QLAND'.
  tbl_fieldcat-seltext_s = 'Wth Tax Cntry'.
  tbl_fieldcat-seltext_m = 'With Tax Country'.
  tbl_fieldcat-seltext_l = 'With Tax Country'.
  tbl_fieldcat-reptext_ddic = 'With Tax Country'.
  IF p_wtax = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'WITHT'.
  tbl_fieldcat-seltext_s = 'Wth Tax Typ'.
  tbl_fieldcat-seltext_m = 'With Tax Type'.
  tbl_fieldcat-seltext_l = 'With Tax Type'.
  tbl_fieldcat-reptext_ddic = 'With Tax Type'.
  IF p_wtax = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'WT_WITHCD'.
  tbl_fieldcat-seltext_s = 'Wth Tax CD'.
  tbl_fieldcat-seltext_m = 'With Tax Code'.
  tbl_fieldcat-seltext_l = 'With Tax Code'.
  tbl_fieldcat-reptext_ddic = 'With Tax Code'.
  IF p_wtax = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'WT_SUBJCT'.
  tbl_fieldcat-seltext_s = 'Wth Tax Liable'.
  tbl_fieldcat-seltext_m = 'With Tax Liable'.
  tbl_fieldcat-seltext_l = 'With Tax Liable'.
  tbl_fieldcat-reptext_ddic = 'With Tax Liable'.
  IF p_wtax = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'QSREC'.
  tbl_fieldcat-seltext_s = 'Rec Type'.
  tbl_fieldcat-seltext_m = 'Recip Type'.
  tbl_fieldcat-seltext_l = 'Recip Type'.
  tbl_fieldcat-reptext_ddic = 'Recip Type'.
  IF p_wtax = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'WT_WTSTCD'.
  tbl_fieldcat-seltext_s = 'Wth Tax ID'.
  tbl_fieldcat-seltext_m = 'With Tax ID'.
  tbl_fieldcat-seltext_l = 'With Tax ID'.
  tbl_fieldcat-reptext_ddic = 'With Tax ID'.
  IF p_wtax = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'WT_EXDT'.
  tbl_fieldcat-seltext_s = 'WT Exempt To'.
  tbl_fieldcat-seltext_m = 'With Tax Exempt To'.
  tbl_fieldcat-seltext_l = 'With Tax Exempt To'.
  tbl_fieldcat-reptext_ddic = 'With Tax Exempt To'.
  IF p_wtax = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

* Contact Persons Detail

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'ABTNR'.
  tbl_fieldcat-seltext_s = 'Dept'.
  tbl_fieldcat-seltext_m = 'Department'.
  tbl_fieldcat-seltext_l = 'Department'.
  tbl_fieldcat-reptext_ddic = 'Department'.
  IF p_person = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'PAFKT'.
  tbl_fieldcat-seltext_s = 'Function'.
  tbl_fieldcat-seltext_m = 'Function'.
  tbl_fieldcat-seltext_l = 'Function'.
  tbl_fieldcat-reptext_ddic = 'Function'.
  IF p_person = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'NAMEV'.
  tbl_fieldcat-seltext_s = 'First Name'.
  tbl_fieldcat-seltext_m = 'First Name'.
  tbl_fieldcat-seltext_l = 'First Name'.
  tbl_fieldcat-reptext_ddic = 'First Name'.
  IF p_person = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'CONTNAME1'.
  tbl_fieldcat-seltext_s = 'Name 1'.
  tbl_fieldcat-seltext_m = 'Last Name 1'.
  tbl_fieldcat-seltext_l = 'Last Name 1'.
  tbl_fieldcat-reptext_ddic = 'Last Name 1'.
  IF p_person = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'TELF1'.
  tbl_fieldcat-seltext_s = 'Telephone'.
  tbl_fieldcat-seltext_m = 'Telephone Number'.
  tbl_fieldcat-seltext_l = 'Telephone Number'.
  tbl_fieldcat-reptext_ddic = 'Telephone Number'.
  IF p_person = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'FAX_NUMBER'.
  tbl_fieldcat-seltext_s = 'Fax'.
  tbl_fieldcat-seltext_m = 'Fax'.
  tbl_fieldcat-seltext_l = 'Fax'.
  tbl_fieldcat-reptext_ddic = 'Fax'.
  IF p_person = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'SMTP_ADDR'.
  tbl_fieldcat-seltext_s = 'E-mail'.
  tbl_fieldcat-seltext_m = 'E-mail Address'.
  tbl_fieldcat-seltext_l = 'E-mail Address'.
  tbl_fieldcat-reptext_ddic = 'E-mail Address'.
  IF p_person = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.

  tbl_fieldcat-tabname   = 'TBL_REPORT'.
  tbl_fieldcat-fieldname = 'DEFLT_COMM'.
  tbl_fieldcat-seltext_s = 'Deflt Comm'.
  tbl_fieldcat-seltext_m = 'Default Comm'.
  tbl_fieldcat-seltext_l = 'Deflt Comm Method'.
  tbl_fieldcat-reptext_ddic = 'Teletex'.
  IF p_person = ' '.
    tbl_fieldcat-no_out = 'X'.
  ENDIF.
  APPEND tbl_fieldcat TO tbl_fieldtab.
  CLEAR tbl_fieldcat.


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
  MOVE 'COMP CODE'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.

* Bank Details

  MOVE 'BANK COUNTRY'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'BANK KEY'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'BANK ACCT'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'ACCT HOLDER'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'ACCT KEY'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'IBAN VALUE'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'BCAT'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'BANK NAME'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.

* PERMITTED PAYEE

  MOVE 'PAYEE'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'PAYEE NAME'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'CITY'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.

* WITHHOLDING TAX

  MOVE 'WITH TAX CNTRY'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'WITH TAX TYPE'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'WITH TAX CODE'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'LIABLE'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'RECIP TYPE'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'WITH TAX ID'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.

* CONTACT PERSONS

  MOVE 'DEPARTMENT'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'FUNCTION'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'FIRST NAME'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'LAST NAME'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'TELEPHONE'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'FAX'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'E-MAIL ADDRESS'  TO tbl_excel_header-spaltenname.
  APPEND tbl_excel_header.
  MOVE 'COMM. METHOD'  TO tbl_excel_header-spaltenname.
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
