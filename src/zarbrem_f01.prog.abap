*&---------------------------------------------------------------------*
*&  Include           ZARBREM_F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZARBREM_F01                                    *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 07-Jan-2014                                    *
*& Object ID          : SDP46414 Remittance Advice (ZARBREM) Batch     *
*&                      Program                                        *
*& Application Area   : MM                                             *
*& Description        : This report generates the Remittance Advice for*
*&                      Ariba.                                         *
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Form  GET_pymt_DETAILS
*&---------------------------------------------------------------------*
*       Get Payment details
*----------------------------------------------------------------------*
FORM get_pymt_details .
  DATA: lt_reguh_temp TYPE STANDARD TABLE OF ty_reguh.

* Get Settlement data from payment program
  SELECT laufd
         laufi
         xvorl
         zbukr
         lifnr
         vblnr
         zaldt
         rzawe  "(+)PANUSURI Ticket 46414
         FROM reguh
         INTO TABLE lt_reguh
         WHERE laufd IN s_laufd
         AND   xvorl EQ space
         AND   lifnr IN s_lifnr.
  IF sy-subrc = 0.
    SORT lt_reguh BY laufd laufi zbukr vblnr.
  ELSE.
    MESSAGE 'No data selected'(002) TYPE 'S' DISPLAY LIKE 'E'.
  ENDIF.

  IF lt_reguh IS NOT INITIAL.
    lt_reguh_temp[] = lt_reguh[].
    SORT lt_reguh_temp BY lifnr.
    DELETE ADJACENT DUPLICATES FROM lt_reguh_temp COMPARING lifnr.
*   Get Ariba Supplier ID and Vendor name
    SELECT lifnr
           name1
           emnfr
           FROM lfa1
           INTO TABLE lt_lfa1
           FOR ALL ENTRIES IN lt_reguh_temp
           WHERE lifnr = lt_reguh_temp-lifnr.
    IF sy-subrc = 0.
      SORT lt_lfa1 BY lifnr.
    ENDIF.

    CLEAR lt_reguh_temp.
    REFRESH lt_reguh_temp.

*   Get Processed items from payment program
    SELECT laufd
           laufi
           xvorl
           zbukr
           lifnr
           vblnr
           bukrs
           belnr
           gjahr
           xblnr
           blart "(+)SKAPSE Ticket 78045
           budat
           dmbtr
           sknto
           FROM regup
           INTO TABLE lt_regup
           FOR ALL ENTRIES IN lt_reguh
           WHERE laufd = lt_reguh-laufd
           AND   laufi = lt_reguh-laufi
           AND   xvorl = lt_reguh-xvorl
           AND   zbukr = lt_reguh-zbukr
           AND   lifnr = lt_reguh-lifnr
           AND   vblnr = lt_reguh-vblnr
*           AND   blart = 'ZR'."(-)SKAPSE Ticket 78045
           AND   blart IN ('ZR', 'ZN')."(+)SKAPSE Ticket 78045

    IF sy-subrc = 0.
*      SORT lt_regup BY laufd laufi zbukr vblnr bukrs belnr gjahr DESCENDING. "(-)PANUSURI ticket 46414
      SORT lt_regup BY laufi DESCENDING AS TEXT. "(+)PANUSURI ticket 46414
*      DELETE ADJACENT DUPLICATES FROM lt_regup COMPARING laufd laufi zbukr vblnr bukrs belnr gjahr.  "(-)PANUSURI ticket 46414
      SORT lt_regup BY vblnr belnr.   "(+)PANUSURI ticket 46414
      DELETE ADJACENT DUPLICATES FROM lt_regup COMPARING vblnr belnr.  "(+)PANUSURI ticket 46414
    ELSE.
      MESSAGE 'No data selected'(002) TYPE 'S' DISPLAY LIKE 'E'.
    ENDIF.

    IF lt_regup IS NOT INITIAL.
*     Check no duplicate IDOCS are sent
      SELECT *
             FROM zeftdup
             INTO TABLE lt_zeftdup
             FOR ALL ENTRIES IN lt_regup
             WHERE laufd = lt_regup-laufd
             AND   laufi = lt_regup-laufi
             AND   belnr = lt_regup-belnr.
      IF sy-subrc = 0.
        SORT lt_zeftdup BY laufd laufi belnr.
        LOOP AT lt_zeftdup INTO lwa_zeftdup.
          DELETE lt_regup WHERE laufd = lwa_zeftdup-laufd
                          AND   laufi = lwa_zeftdup-laufi
                          AND   belnr = lwa_zeftdup-belnr.
          CLEAR lwa_zeftdup.
        ENDLOOP.
        REFRESH lt_zeftdup.
      ENDIF.
      IF lt_regup IS NOT INITIAL.
*BOI by PANUSURI Ticket 46414
*To avoid performance issues, BSEG is queried twice
        SELECT bukrs
               belnr
               gjahr
               FROM bseg
               INTO TABLE lt_bseg_pmt
               FOR ALL ENTRIES IN lt_regup
               WHERE bukrs = lt_regup-bukrs
               AND   belnr = lt_regup-vblnr
               AND   gjahr = lt_regup-gjahr
               AND   buzei = '1'.
        IF sy-subrc = 0.
          SORT lt_bseg_pmt BY bukrs belnr gjahr.
        ENDIF.
*EOI by PANUSURI Ticket 46414
*       Get ISR Reference Number
        SELECT bukrs
               belnr
               gjahr
               esrre
               FROM bseg
               INTO TABLE lt_bseg
               FOR ALL ENTRIES IN lt_regup
               WHERE bukrs = lt_regup-bukrs
               AND   belnr = lt_regup-belnr
               AND   gjahr = lt_regup-gjahr
               AND   buzei = '1'.
        IF sy-subrc = 0.
          SORT lt_bseg BY bukrs belnr gjahr.
        ENDIF.

*BOI by PANUSURI Ticket 46414
*       Get check number
        SELECT zbukr
               rzawe
               chect
               laufd
               lifnr
               vblnr
               FROM payr
               INTO TABLE lt_payr
               FOR ALL ENTRIES IN lt_regup
               WHERE zbukr = lt_regup-zbukr
               AND   laufd = lt_regup-laufd
               AND   lifnr = lt_regup-lifnr
               AND   vblnr = lt_regup-vblnr.
        IF sy-subrc = 0.
          SORT lt_payr BY zbukr laufd lifnr vblnr.
        ENDIF.
*EOI by PANUSURI Ticket 46414
      ENDIF.
    ENDIF.

  ENDIF.

ENDFORM.                    " GET_PYMT_DETAILS
*&---------------------------------------------------------------------*
*&      Form  GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*       Output data
*----------------------------------------------------------------------*
FORM get_output_data .
  DATA: lv_paid_amt  TYPE dmbtr.
*BOI by PANUSURI Ticket 46414
  SORT lt_reguh BY laufd laufi xvorl zbukr lifnr vblnr.
  SORT lt_regup BY laufd laufi xvorl zbukr lifnr vblnr.
*EOI by PANUSURI Ticket 46414

  LOOP AT lt_regup INTO lwa_regup.
*BOI by PANUSURI Ticket 46414
*   Check if payment is made
*    CLEAR lwa_bseg.
*    READ TABLE lt_bseg INTO lwa_bseg WITH KEY bukrs = lwa_regup-bukrs
*                                              belnr = lwa_regup-vblnr
*                                              gjahr = lwa_regup-gjahr
*                                              BINARY SEARCH.
*    IF sy-subrc <> 0.
*      CONTINUE.
*    ENDIF.
    CLEAR lwa_bseg_pmt.
    READ TABLE lt_bseg_pmt INTO lwa_bseg_pmt WITH KEY bukrs = lwa_regup-bukrs
                                                      belnr = lwa_regup-vblnr
                                                      gjahr = lwa_regup-gjahr
                                                      BINARY SEARCH.
    IF sy-subrc <> 0.
      CONTINUE.
    ENDIF.
*EOI by PANUSURI Ticket 46414
    CLEAR lwa_reguh.
    READ TABLE lt_reguh INTO lwa_reguh WITH KEY laufd = lwa_regup-laufd
                                                laufi = lwa_regup-laufi
                                                xvorl = lwa_regup-xvorl
                                                zbukr = lwa_regup-zbukr
                                                lifnr = lwa_regup-lifnr
                                                vblnr = lwa_regup-vblnr
                                                BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_output-lifnr = lwa_reguh-lifnr.
      lwa_output-zaldt = lwa_reguh-zaldt.
      READ TABLE lt_lfa1 INTO lwa_lfa1 WITH KEY lifnr = lwa_reguh-lifnr
                                                BINARY SEARCH.
      IF sy-subrc = 0.
        lwa_output-name1 = lwa_lfa1-name1.
        lwa_output-emnfr = lwa_lfa1-emnfr.
      ENDIF.
*BOI by PANUSURI Ticket 46414
      IF lwa_reguh-rzawe IS INITIAL.
        READ TABLE lt_payr INTO lwa_payr WITH KEY zbukr = lwa_regup-zbukr
                                           laufd = lwa_regup-laufd
                                           lifnr = lwa_regup-lifnr
                                           vblnr = lwa_regup-vblnr
                                           BINARY SEARCH.
        IF sy-subrc = 0.
*          IF lwa_payr-chect IS NOT INITIAL.
          IF lwa_payr-rzawe CA 'C' OR lwa_payr-rzawe CA 'O'
             OR lwa_payr-rzawe CA 'P'.
            lwa_output-rzawe = 'C'.
            lwa_output-chect = lwa_payr-chect.
          ELSE.  "(-)PANUSURI Ticket 46414
            lwa_output-rzawe = 'T'.  "(-)PANUSURI Ticket 46414
          ENDIF.
*        ELSE. "(+)PANUSURI Ticket 46414
*          lwa_output-rzawe = 'T'. "(+)PANUSURI Ticket 46414
        ENDIF.
*      ELSE.
*        IF lwa_reguh-rzawe CA 'C' OR lwa_reguh-rzawe CA 'O'
*                                  OR lwa_reguh-rzawe CA 'P'.
*          lwa_output-rzawe = 'C'.
*        ELSE.
*          lwa_output-rzawe = 'T'.
*        ENDIF.
      ELSEIF lwa_reguh-rzawe CA 'C' OR lwa_reguh-rzawe CA 'O'
         OR lwa_reguh-rzawe CA 'P'.
        lwa_output-rzawe = 'C'.
        READ TABLE lt_payr INTO lwa_payr WITH KEY zbukr = lwa_regup-zbukr
                                           laufd = lwa_regup-laufd
                                           lifnr = lwa_regup-lifnr
                                           vblnr = lwa_regup-vblnr
                                           BINARY SEARCH.
        IF sy-subrc = 0.
          lwa_output-chect = lwa_payr-chect.
        ENDIF.
      ELSE.
        lwa_output-rzawe = 'T'.
      ENDIF.
*EOI by PANUSURI Ticket 46414
    ENDIF.

    CLEAR lwa_bseg.
    READ TABLE lt_bseg INTO lwa_bseg WITH KEY bukrs = lwa_regup-bukrs
                                              belnr = lwa_regup-belnr
                                              gjahr = lwa_regup-gjahr
                                              BINARY SEARCH.
    IF sy-subrc = 0.
      lwa_output-esrre = lwa_bseg-esrre.
    ENDIF.

    lwa_output-laufd = lwa_regup-laufd.
    lwa_output-laufi = lwa_regup-laufi.
    lwa_output-vblnr = lwa_regup-vblnr.
    lwa_output-belnr = lwa_regup-belnr.
    lwa_output-gjahr = lwa_regup-gjahr.
    lwa_output-xblnr = lwa_regup-xblnr.
    lwa_output-blart = lwa_regup-blart."(+)SKAPSE Ticket 78045
    lwa_output-budat = lwa_regup-budat.
*   Calculate Gross amount
    IF lwa_regup-dmbtr < 0.
      lwa_output-gross_amt  = lwa_regup-dmbtr * - 1.
    ELSE.
      lwa_output-gross_amt  = lwa_regup-dmbtr.
    ENDIF.
*   Calculate Paid amount
    lv_paid_amt = lwa_regup-dmbtr - lwa_regup-sknto.
    IF lv_paid_amt < 0.
      lwa_output-paid_amt  = lv_paid_amt * - 1.
    ELSE.
      lwa_output-paid_amt  = lv_paid_amt.
    ENDIF.
*   Calculate discount amount
    IF lwa_regup-sknto < 0.
      lwa_output-disc_amt  = lwa_regup-sknto * -1.
    ELSE.
      lwa_output-disc_amt  = lwa_regup-sknto.
    ENDIF.

    APPEND lwa_output TO lt_output.

*   Fill output data for ALV
    lwa_output_alv-laufd = lwa_regup-laufd.
    lwa_output_alv-laufi = lwa_regup-laufi.
    lwa_output_alv-lifnr = lwa_regup-lifnr.
    lwa_output_alv-belnr = lwa_regup-belnr.

    APPEND lwa_output_alv TO lt_output_alv.

    CLEAR: lwa_output,
           lwa_output_alv,
           lwa_regup,
           lwa_reguh,
           lwa_lfa1,
           lwa_bseg,
           lwa_bseg_pmt,"(+)PANUSURI Ticket 46414
           lwa_payr.  "(+)PANUSURI Ticket 46414
  ENDLOOP.
  REFRESH: lt_reguh,
           lt_regup,
           lt_lfa1,
           lt_bseg,
           lt_bseg_pmt,"(+)PANUSURI Ticket 46414
           lt_payr. "(+)PANUSURI Ticket 46414

* Process the IDOC function modules
  IF lt_output IS NOT INITIAL.
    LOOP AT lt_output INTO lwa_output.
*     Ariba Status IDOC
      CALL FUNCTION 'Z_IDOC_CREATE_ZARBIST01'
        EXPORTING
*         im_doc_typ                   = 'ZR' "(-)SKAPSE Ticket 78045
          im_doc_typ                   = lwa_output-blart "(+)SKAPSE Ticket 78045
          im_inv_doc_no                = lwa_output-belnr
          im_vnd_no                    = lwa_output-lifnr
          im_arib_com_sup              = lwa_output-emnfr
          im_sup_inv_no                = lwa_output-xblnr
          im_inv_status                = 'PAID'
          im_ztext                     = ''
          im_inv_amt                   = ''
          im_year                      = lwa_output-gjahr
          im_port                      = 'SONIC'
          im_partno                    = 'SONIC'
          im_parttype                  = 'LS'
        EXCEPTIONS
          error_idoc_control           = 1
          error_idoc_status            = 2
          error_idoc_data              = 3
          error_logical_system_unknown = 4
          error_other                  = 5
          OTHERS                       = 6.

*     Ariba Remittance IDoc
      CALL FUNCTION 'Z_IDOC_CREATE_ZARBREM01'
        EXPORTING
          im_inv_doc_no                = lwa_output-belnr
          im_vnd_no                    = lwa_output-lifnr
          im_vnd_name1                 = lwa_output-name1
          im_sup_inv_no                = lwa_output-xblnr
          im_arib_com_sup              = lwa_output-emnfr
          im_po                        = lwa_output-esrre
          im_gross_amt                 = lwa_output-gross_amt
          im_paid_amt                  = lwa_output-paid_amt
          im_pymt_date                 = lwa_output-zaldt
          im_disc_amt                  = lwa_output-disc_amt
*         im_pymt_ref                  = 'T' "(-)PANUSURI Ticket 46414
*         im_pymt_type                 = lwa_output-vblnr   "(-)PANUSURI Ticket 46414
          im_pymt_ref                  = lwa_output-rzawe   "(+)PANUSURI Ticket 46414
          im_pymt_type                 = lwa_output-chect   "(+)PANUSURI Ticket 46414
          im_doc_date                  = lwa_output-budat
          im_pymt_id                   = lwa_output-vblnr
          im_port                      = 'SONIC'
          im_partno                    = 'SONIC'
          im_parttype                  = 'LS'
        EXCEPTIONS
          error_idoc_control           = 1
          error_idoc_status            = 2
          error_idoc_data              = 3
          error_logical_system_unknown = 4
          error_other                  = 5
          OTHERS                       = 6.

*     Update ZEFTDUP Table
      CLEAR lwa_zeftdup.
      lwa_zeftdup-laufd = lwa_output-laufd.
      lwa_zeftdup-laufi = lwa_output-laufi.
      lwa_zeftdup-belnr = lwa_output-belnr.
      INSERT zeftdup FROM lwa_zeftdup.
      COMMIT WORK.

      CLEAR lwa_output.
    ENDLOOP.
    REFRESH lt_output.
  ENDIF.

  SORT lt_output_alv BY laufd laufi lifnr belnr.

ENDFORM.                    " GET_OUTPUT_DATA
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*       Display output
*----------------------------------------------------------------------*
FORM display_output .
* Build fieldcatalog
  PERFORM build_fieldcatalog.

  lwa_layout-colwidth_optimize = 'X'.

  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program     = 'ZARBREM'
      i_callback_top_of_page = 'DISPLAY_TOP_OF_PAGE'
      is_layout              = lwa_layout
      it_fieldcat            = lt_fieldcat[]
*     it_sort                = lit_sort
    TABLES
      t_outtab               = lt_output_alv[]
    EXCEPTIONS
      program_error          = 1
      OTHERS                 = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

  ENDIF.

ENDFORM.                    " DISPLAY_OUTPUT
*&---------------------------------------------------------------------*
*&      Form  BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*       Build Fieldcatalog
*----------------------------------------------------------------------*
FORM build_fieldcatalog .
  PERFORM create_catalog USING : 'LAUFD' 'Run Date'(003).
  PERFORM create_catalog USING : 'LAUFI' 'RunID'(004).
  PERFORM create_catalog USING : 'LIFNR' 'Vendor'(005).
  PERFORM create_catalog USING : 'BELNR' 'DocumnetNo'(006).

ENDFORM.                    " BUILD_FIELDCATALOG
*&---------------------------------------------------------------------*
*&      Form  CREATE_CATALOG
*&---------------------------------------------------------------------*
*       Create catalog
*----------------------------------------------------------------------*
FORM create_catalog  USING    iv_fieldname TYPE slis_fieldname
                              iv_seltext   TYPE scrtext_m.
  DATA: lwa_fieldcat TYPE slis_fieldcat_alv.

  CLEAR lwa_fieldcat.
  lwa_fieldcat-fieldname = iv_fieldname.
  lwa_fieldcat-seltext_m = iv_seltext.
  APPEND lwa_fieldcat TO lt_fieldcat.

ENDFORM.                    " CREATE_CATALOG
*&---------------------------------------------------------------------*
*&      Form  DISPLAY_TOP_OF_PAGE
*&---------------------------------------------------------------------*
*       Display Top of page
*----------------------------------------------------------------------*
FORM display_top_of_page .
  IF lt_listheader[] IS INITIAL.
    lwa_listheader-typ               ='H'.
    lwa_listheader-info             = 'Ariba Remittance Advice Batch Program'(007).
    APPEND lwa_listheader TO lt_listheader.
    CLEAR lwa_listheader.
*   Write List header
    CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
      EXPORTING
        it_list_commentary = lt_listheader.
  ENDIF.

ENDFORM.                    " DISPLAY_TOP_OF_PAGE
