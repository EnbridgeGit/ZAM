REPORT zaamr013 LINE-SIZE 80 LINE-COUNT 65.
TYPE-POOLS: slis.

************************************************************************
*                                                                      *
*   PROGRAM:    ZAAMR013                                               *
*   PROGRAMMER: Mohammad Khan                                          *
*   DATE:       May, 2006.                                             *
*   Track #:    TR66                                                   *
*   The purpose of this program is to produce the Asset History by     *
*   Vintage report. The transactions data of selected year is displayed*
*   with the opening and closing balace for each main/sub asset.       *
*                                                                      *
************************************************************************

TABLES:   anla,               " Asset master record-segment
          anlb,               " Depreciation terms
          anlc,               " Asset value fields
          anlz,               " asset allocations (by time)
          anea,               " Asset Line Items for Proportional Values
          anep.               " Asset Line Items

DATA:     BEGIN OF indata OCCURS 0,
            bukrs       LIKE anla-bukrs,       " Company code
            gjahr       LIKE anep-gjahr,       " Fiscal Year
            anlkl       LIKE anla-anlkl,       " Asset class
            txt50       LIKE anla-txt50,       " Description
            vyear       LIKE anlb-vyear,       " Vintage year
            werks       LIKE anlz-werks,       " Plant code
            anln1       LIKE anlb-anln1,       " Main asset
            anln2       LIKE anlb-anln2,       " Sub asset
            kostl       LIKE anlz-kostl,       " Cost centre
            menge       LIKE anla-menge,       " Quantity
            kansw       LIKE anlc-kansw,       " Cumulative acquisition
            kinvz       LIKE anlc-kinvz,       " Cumulative invst grants
            answl       LIKE anlc-answl,       " For closing balance
            afabe       LIKE anlc-afabe,
          END OF indata.

DATA:     BEGIN OF reptab OCCURS 0,
            gjahr       LIKE anep-gjahr,       " Fiscal Year
            anlkl       LIKE anla-anlkl,       " Asset class
            txt50       LIKE anla-txt50,       " Description
            afabe       like anlc-afabe,       " Depreciation Area
            vyear       LIKE anlb-vyear,       " Vintage year
            werks       LIKE anlz-werks,       " Plant code
            anln1       LIKE anlb-anln1,       " Main asset
            anln2       LIKE anlb-anln2,       " Sub asset
            kostl       LIKE anlz-kostl,       " Cost centre
            menge       LIKE anla-menge,       " Quantity
            opening     LIKE cosp-wkg001,      " Opening balanc
            sysclos     LIKE cosp-wkg001,      " System Closing Balance
            additions   LIKE cosp-wkg001,      " $-addition Trans
            retirements LIKE cosp-wkg001,      " $-retirement Trans
            transfers   LIKE cosp-wkg001,      " $-transfer Trans
            resads      LIKE cosp-wkg001,      " $-RESADS Trans
            abands      LIKE cosp-wkg001,      " $-ABAND Trans
            others      LIKE cosp-wkg001,      " $-other Trans
            total       LIKE cosp-wkg001,      " $-total of all Trans
            closing     LIKE cosp-wkg001,      " Closing balanc
            status      TYPE c,                " Closing balances eq/ne
          END OF reptab.

DATA:     w_head01(60)  TYPE c,
          w_head03(60)  TYPE c,
          prev_anln1    LIKE anla-anln1,
          prev_anln2    LIKE anla-anln2,
          w_anbtr       LIKE anep-anbtr.

DATA:
   es_variant    LIKE disvariant,
   is_variant    LIKE disvariant.

RANGES: r_bwasl  FOR anep-bwasl.
FIELD-SYMBOLS: <fs1>.

************************************************************************
* Beginning of selection screen                                        *
************************************************************************

SELECTION-SCREEN BEGIN OF BLOCK box WITH FRAME TITLE text-003.
SELECTION-SCREEN SKIP 1.
SELECTION-SCREEN BEGIN OF BLOCK box1 WITH FRAME TITLE text-040.
SELECT-OPTIONS: s_bukrs  FOR anla-bukrs OBLIGATORY
                    NO INTERVALS DEFAULT 'UGL'.
SELECT-OPTIONS: s_anlkl   FOR anla-anlkl,
                s_anln1   FOR anla-anln1,
                s_anln2   FOR anla-anln2,
                s_afabe   FOR anlc-afabe,
                s_werks   FOR anlz-werks,
                s_kostl   FOR anlz-kostl,
                s_vyear   FOR anlb-vyear,
                s_gjahr   FOR anlc-gjahr OBLIGATORY
                    DEFAULT sy-datum(4).
SELECTION-SCREEN END OF BLOCK box1.

SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-041.
SELECT-OPTIONS: s1_bwasl   FOR anep-bwasl,      "Addition Trans.
                s2_bwasl   FOR anep-bwasl,      "Retirement Trans.
                s3_bwasl   FOR anep-bwasl,      "Transfer Trans
                s4_bwasl   FOR anep-bwasl,      "Res Ads
                s5_bwasl   FOR anep-bwasl,      "Aband
                s6_bwasl   FOR anep-bwasl.      "Other Trans
SELECTION-SCREEN END OF BLOCK box2.

SELECTION-SCREEN BEGIN OF BLOCK box3 WITH FRAME TITLE text-042.
SELECT-OPTIONS ex_anln1   FOR anla-anln1.
SELECTION-SCREEN END OF BLOCK box3.

SELECTION-SCREEN BEGIN OF BLOCK box4 WITH FRAME TITLE text-043.
PARAMETERS:     pvariant LIKE disvariant-variant,
                p_notran AS CHECKBOX DEFAULT ' '.
SELECTION-SCREEN END OF BLOCK box4.

SELECTION-SCREEN END OF BLOCK box.

*   End of selection screen.

************************************************************************
***                       INITIALIZATION                            ****
************************************************************************
INITIALIZATION.
  PERFORM fillout_transaction_types.
  REFRESH: reptab.
  CLEAR:   reptab.

* To select display variant
AT SELECTION-SCREEN ON VALUE-REQUEST FOR pvariant.
  is_variant-report = 'ZAAMR013'.
  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant          = is_variant
*     I_TABNAME_HEADER    =
*     I_TABNAME_ITEM      =
*     IT_DEFAULT_FIELDCAT =
      i_save              = 'A'
    IMPORTING
*     E_EXIT              =
      es_variant          = es_variant
    EXCEPTIONS
      not_found           = 1
      program_error       = 2
      OTHERS              = 3.
  IF sy-subrc = 0.
    pvariant = es_variant-variant.
  ENDIF.

* Extract the required data and display report
START-OF-SELECTION.
  PERFORM get_transaction_range.
  PERFORM collect_db_data.
  PERFORM display_alv_grid_data.

END-OF-SELECTION.

************************************************************************
***                       COLLECT_DB_DATA                           ****
************************************************************************
FORM collect_db_data.

  SELECT anlc~bukrs anlc~gjahr anla~anlkl anla~txt50 anlb~vyear
         anlz~werks anlb~anln1 anlb~anln2 anlz~kostl anla~menge
         anlc~kansw anlc~kinvz anlc~answl anlc~afabe
    INTO TABLE indata
    FROM ( ( ( anla INNER JOIN anlb
                      ON anlb~bukrs = anla~bukrs AND
                         anlb~anln1 = anla~anln1 AND
                         anlb~anln2 = anla~anln2 )
                      INNER JOIN anlc
                      ON anlc~bukrs = anlb~bukrs AND
                         anlc~anln1 = anlb~anln1 AND
                         anlc~anln2 = anlb~anln2 AND
                         anlc~afabe = anlb~afabe )
                      INNER JOIN anlz
                      ON anlz~bukrs = anlc~bukrs AND
                         anlz~anln1 = anlc~anln1 AND
                         anlz~anln2 = anlc~anln2 AND
                         anlz~bdatu = anlb~bdatu )
    WHERE  anla~bukrs IN s_bukrs
      AND  anla~anln1 IN s_anln1
      AND  anla~anln2 IN s_anln2
      AND  anla~anlkl IN s_anlkl
      AND  anlb~vyear IN s_vyear
      AND  anlc~gjahr IN s_gjahr
      AND  anlz~kostl IN s_kostl
      AND  anlz~werks IN s_werks
      and  anlc~afabe in s_afabe.

  SORT indata BY gjahr anlkl vyear werks anln1 anln2.

  IF NOT ex_anln1[] IS INITIAL.
    DELETE indata WHERE anln1 IN ex_anln1.
  ENDIF.

*DELETE INDATA WHERE ANLN1+5(7) GE '9800000'.  "exclude group assets

  LOOP AT indata.
    CLEAR   reptab.
    IF ( indata-anln1 <> prev_anln1 ) OR
       ( indata-anln2 <> prev_anln2 ).
      MOVE indata-anln1 TO prev_anln1.
      MOVE indata-anln2 TO prev_anln2.
      reptab-opening = indata-kansw + indata-kinvz.   " opening balance
      reptab-sysclos = reptab-opening + indata-answl. " CLOSING BALANCE
    ENDIF.
    MOVE-CORRESPONDING indata TO reptab.

    SELECT bwasl lnran SUM( anbtr )
      INTO (anep-bwasl, anep-lnran, anep-anbtr)
      FROM anep
     WHERE bukrs = indata-bukrs
       AND anln1 = indata-anln1
       AND anln2 = indata-anln2
       AND gjahr = indata-gjahr
       AND afabe = indata-afabe
       AND bwasl IN r_bwasl
     GROUP BY bwasl lnran.

      MOVE anep-anbtr TO w_anbtr.

      IF ( NOT s4_bwasl[] IS INITIAL AND anep-bwasl IN s4_bwasl ) OR
         ( NOT s5_bwasl[] IS INITIAL AND anep-bwasl IN s5_bwasl ).

*   IF ANEP-BWASL IN S4_BWASL  OR  ANEP-BWASL IN S5_BWASL.
        SELECT SINGLE nafav INTO w_anbtr
          FROM anea
         WHERE bukrs = indata-bukrs
           AND anln1 = indata-anln1
           AND anln2 = indata-anln2
           AND gjahr = indata-gjahr
           AND lnran = anep-lnran.
      ENDIF.

      IF NOT s1_bwasl[] IS INITIAL.
        IF anep-bwasl  IN s1_bwasl.
          MOVE w_anbtr TO: reptab-additions, reptab-total.
        ENDIF.
      ENDIF.

      IF NOT s2_bwasl[] IS INITIAL.
        IF anep-bwasl  IN s2_bwasl.
          MOVE w_anbtr TO: reptab-retirements, reptab-total.
        ENDIF.
      ENDIF.

      IF NOT s3_bwasl[] IS INITIAL.
        IF anep-bwasl  IN s3_bwasl.
          MOVE w_anbtr TO: reptab-transfers, reptab-total.
        ENDIF.
      ENDIF.

      IF NOT s4_bwasl[] IS INITIAL.
        IF anep-bwasl  IN s4_bwasl.
          MOVE w_anbtr TO: reptab-resads, reptab-total.
        ENDIF.
      ENDIF.

      IF NOT s5_bwasl[] IS INITIAL.
        IF anep-bwasl  IN s5_bwasl.
          MOVE w_anbtr TO: reptab-abands, reptab-total.
        ENDIF.
      ENDIF.

      IF NOT s6_bwasl[] IS INITIAL.
        IF anep-bwasl  IN s6_bwasl.
          MOVE w_anbtr TO: reptab-others, reptab-total.
        ENDIF.
      ENDIF.

      reptab-closing = reptab-opening + w_anbtr. " closing balance
      CLEAR reptab-status.
      COLLECT reptab.

      CLEAR: reptab-opening, reptab-sysclos, reptab-additions,
             reptab-retirements, reptab-transfers, reptab-resads,
             reptab-abands, reptab-others, reptab-total, reptab-closing,
             reptab-status.
      clear reptab-menge. "skapse 05/07/2015

    ENDSELECT.
    IF sy-subrc <> 0.
      IF p_notran = 'X'.
        IF reptab-opening <> 0  OR  reptab-sysclos <> 0.
          COLLECT reptab.
          CLEAR: reptab-opening, reptab-sysclos, reptab-status.
        ENDIF.
      ENDIF.
    ENDIF.

  ENDLOOP.

*END of no transaction data routine
  LOOP AT reptab.
    IF reptab-sysclos <> reptab-closing.
      MOVE text-005 TO reptab-status.
      MODIFY reptab.
    ENDIF.
  ENDLOOP.

ENDFORM.                    "COLLECT_DB_DATA

************************************************************************
***                  FILLOUT_TRANSACTION_TYPES                      ****
************************************************************************
FORM fillout_transaction_types.
  s1_bwasl-sign   = 'I'.              "Transaction code (Additions)
  s1_bwasl-option = 'EQ'.
  s1_bwasl-low    = '100'.
  APPEND s1_bwasl.
  s1_bwasl-low    = '101'.
  APPEND s1_bwasl.
  s1_bwasl-low    = '115'.
  APPEND s1_bwasl.
  s1_bwasl-low    = '116'.
  APPEND s1_bwasl.
  s1_bwasl-low    = '331'.
  APPEND s1_bwasl.
  s1_bwasl-low    = '336'.
  APPEND s1_bwasl.
  CLEAR  s1_bwasl.
*
  s2_bwasl-sign   = 'I'.              "Transaction code (Retirements)
  s2_bwasl-option = 'EQ'.
  s2_bwasl-low    = '200'.
  APPEND s2_bwasl.
  s2_bwasl-low    = '204'.
  APPEND s2_bwasl.
  s2_bwasl-low    = '205'.
  APPEND s2_bwasl.
  s2_bwasl-low    = '250'.
  APPEND s2_bwasl.
  s2_bwasl-low    = '254'.
  APPEND s2_bwasl.
  s2_bwasl-low    = '255'.
  APPEND s2_bwasl.
  CLEAR  s2_bwasl.
*
  s3_bwasl-sign   = 'I'.              "Transaction code (Transfers)
  s3_bwasl-option = 'EQ'.
  s3_bwasl-low    = '300'.
  APPEND s3_bwasl.
  s3_bwasl-low    = '310'.
  APPEND s3_bwasl.
  s3_bwasl-low    = '320'.
  APPEND s3_bwasl.
  s3_bwasl-low    = '330'.
  APPEND s3_bwasl.
  CLEAR  s3_bwasl.
*
  s4_bwasl-sign   = 'I'.              "Transaction code (ABANDS)
  s4_bwasl-option = 'EQ'.
  s4_bwasl-low    = '705'.
  APPEND s4_bwasl.
  CLEAR  s4_bwasl.
*
  s5_bwasl-sign   = 'I'.              "Transaction code (ABANDS)
  s5_bwasl-option = 'EQ'.
  s5_bwasl-low    = '702'.
  APPEND s5_bwasl.
  CLEAR  s5_bwasl.

ENDFORM.                    "FILLOUT_TRANSACTION_TYPES

************************************************************************
*                 FORM GET_TRANSACTION_RANGE                           *
************************************************************************

FORM get_transaction_range.
  r_bwasl[] = s1_bwasl[].
  APPEND LINES OF s2_bwasl TO r_bwasl.
  APPEND LINES OF s3_bwasl TO r_bwasl.
  APPEND LINES OF s4_bwasl TO r_bwasl.
  APPEND LINES OF s5_bwasl TO r_bwasl.
  APPEND LINES OF s6_bwasl TO r_bwasl.
ENDFORM.                    "GET_TRANSACTION_RANGE

************************************************************************
***                    DISPLAY_ALV_GRID_DATA                         ***
************************************************************************
*
FORM display_alv_grid_data.

  DATA: fieldcat TYPE slis_t_fieldcat_alv,
        fc_str   TYPE slis_fieldcat_alv,
        layout   TYPE slis_layout_alv,
*      TITLE    TYPE LVC_TITLE,
        repid    LIKE sy-repid,
        variant  LIKE disvariant,
        sort     TYPE slis_t_sortinfo_alv.
*      SORT_STR TYPE SLIS_SORTINFO_ALV.

  MOVE text-clt  TO w_head01+0(7).
  MOVE sy-sysid  TO w_head01+8(5).
  MOVE sy-mandt  TO w_head01+14(4).
  MOVE text-dte  TO w_head01+21(5).
  WRITE sy-datum TO w_head01+27(10).
  MOVE text-tme  TO w_head01+40(5).
  WRITE sy-uzeit TO w_head01+46(10).

*Build Report Heading
  IF s_gjahr-high = space.
    CONCATENATE text-001 s_gjahr-low INTO w_head03 SEPARATED BY space.
  ELSE.
    CONCATENATE text-001 s_gjahr-low text-002 s_gjahr-high
                         INTO w_head03 SEPARATED BY space.
  ENDIF.

  repid = sy-repid.
  layout-colwidth_optimize = 'X'.
  layout-get_selinfos = 'X'.
  layout-zebra = 'X'.
  variant-report = repid.
  variant-variant = pvariant.

* create field catalog
  REFRESH fieldcat.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = repid
      i_internal_tabname     = 'REPTAB'
      i_inclname             = repid
    CHANGING
      ct_fieldcat            = fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

* update field catalog (hide/reposition/etc)
  LOOP AT fieldcat INTO fc_str.

    CASE fc_str-fieldname.
      WHEN 'GJAHR'.
        fc_str-seltext_l = text-c00.          " Alternative colheader
        fc_str-ddictxt = 'L'.
        fc_str-key     = ' '.                 " Key columns -not first
      WHEN 'ANLKL'.
        fc_str-seltext_l = text-c01.          " Alternative colheader
        fc_str-ddictxt = 'L'.
        fc_str-key     = ' '.                 " Key columns -not first
      WHEN 'TEXT50'.
        fc_str-seltext_l = text-c02.          " Alternative colheader
        fc_str-ddictxt = 'L'.
        fc_str-key     = ' '.                 " Key columns -not first
*          FC_STR-NO_OUT  = 'X'.                 " hide column
      WHEN 'AFABE'.
        fc_str-seltext_l = text-c19.          " Alternative col header
        fc_str-ddictxt = 'L'.
        fc_str-key     = ' '.                 " Key columns -not first
      WHEN 'VYEAR'.
        fc_str-seltext_l = text-c03.          " Alternative col header
        fc_str-ddictxt = 'L'.
        fc_str-key     = ' '.                 " Key columns -not first
      WHEN 'WERKS'.
        fc_str-seltext_l = text-c04.          " Alternative colheader
        fc_str-ddictxt = 'L'.
        fc_str-key     = ' '.                 " Key columns -not first
      WHEN 'ANLN1'.
        fc_str-seltext_l = text-c05.          " Alternative colheader
        fc_str-ddictxt = 'L'.
        fc_str-key     = ' '.                 " Key columns -not first
      WHEN 'ANLN2'.
        fc_str-seltext_l = text-c06.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
        fc_str-key     = ' '.                 " Key columns -not first
      WHEN 'KOSTL'.
        fc_str-seltext_l = text-c07.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
        fc_str-key     = ' '.                 " Key columns -not first
      WHEN 'MENGE'.
        fc_str-seltext_l = text-c08.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
      WHEN 'OPENING'.
        fc_str-seltext_l = text-c09.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
        fc_str-do_sum  = 'X'.                 " Do Sum

      WHEN 'SYSCLOS'.
        fc_str-seltext_l = text-c9a.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
        fc_str-do_sum  = 'X'.                 " Do Sum

      WHEN 'ADDITIONS'.
        fc_str-seltext_l = text-c11.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
        fc_str-seltext_m = text-c11.          " Alternative colheader
        fc_str-seltext_s = text-c11.          " Alternative colheader
        fc_str-reptext_ddic = text-c11.
        fc_str-do_sum  = 'X'.                 " Do Sum
      WHEN 'RETIREMENTS'.
        fc_str-seltext_l = text-c12.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
        fc_str-seltext_m = text-c12.          " Alternative colheader
        fc_str-seltext_s = text-c12.          " Alternative colheader
        fc_str-reptext_ddic = text-c12.
        fc_str-do_sum  = 'X'.                 " Do Sum
      WHEN 'TRANSFERS'.
        fc_str-seltext_l = text-c13.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
        fc_str-seltext_m = text-c13.          " Alternative colheader
        fc_str-seltext_s = text-c13.          " Alternative colheader
        fc_str-reptext_ddic = text-c13.
        fc_str-do_sum  = 'X'.                 " Do Sum
      WHEN 'RESADS'.
        fc_str-seltext_l = text-c17.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
        fc_str-seltext_m = text-c17.          " Alternative colheader
        fc_str-seltext_s = text-c17.          " Alternative colheader
        fc_str-reptext_ddic = text-c17.
        fc_str-do_sum  = 'X'.                 " Do Sum
      WHEN 'ABANDS'.
        fc_str-seltext_l = text-c18.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
        fc_str-seltext_m = text-c18.          " Alternative colheader
        fc_str-seltext_s = text-c18.          " Alternative colheader
        fc_str-reptext_ddic = text-c18.
        fc_str-do_sum  = 'X'.                 " Do Sum
      WHEN 'OTHERS'.
        fc_str-seltext_l = text-c14.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
        fc_str-seltext_m = text-c14.          " Alternative colheader
        fc_str-seltext_s = text-c14.          " Alternative colheader
        fc_str-reptext_ddic = text-c14.
        fc_str-do_sum  = 'X'.                 " Do Sum
      WHEN 'TOTAL'.
        fc_str-seltext_l = text-c15.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
        fc_str-seltext_m = text-c15.          " Alternative colheader
        fc_str-seltext_s = text-c15.          " Alternative colheader
        fc_str-reptext_ddic = text-c15.
        fc_str-do_sum  = 'X'.                 " Do Sum
      WHEN 'CLOSING'.
        fc_str-seltext_l = text-c10.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
        fc_str-do_sum  = 'X'.                 " Do Sum
      WHEN 'STATUS'.
        fc_str-seltext_l = text-c16.          " Alternative colheader
        fc_str-ddictxt = 'L'.                 " Use Large system text
      WHEN OTHERS.
* fc_str-no_out = 'X'.           " hide column
    ENDCASE.

    MODIFY fieldcat FROM fc_str.
  ENDLOOP.

* Display ALV
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      it_fieldcat             = fieldcat
      is_layout               = layout
      i_callback_top_of_page  = 'ALV_TOP_OF_PAGE'
      i_callback_program      = repid
      i_save                  = 'A'
      is_variant              = variant
      it_sort                 = sort
*     I_GRID_TITLE            = TITLE
*     I_CALLBACK_USER_COMMAND = 'OUTPUTALV_DETAILS'
    TABLES
      t_outtab                = reptab
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.

ENDFORM.                    "DISPLAY_ALV_GRID_DATA

************************************************************************
***                    ALV_TOP_OF_PAGE                               ***
************************************************************************

FORM alv_top_of_page.
  DATA: ls_line TYPE slis_listheader.
  DATA: lt_top_of_page TYPE slis_t_listheader.
  DATA: f_length     TYPE i,
        kount        TYPE  i.

  DATA: BEGIN OF hdata01,
        head03(60) TYPE c,
        head04(60) TYPE c,
        head05(60) TYPE c,
        END OF hdata01.

  kount = 1.
  ASSIGN COMPONENT kount OF STRUCTURE hdata01 TO <fs1>.
  MOVE text-090 TO <fs1>.
  LOOP AT r_bwasl.
    f_length = STRLEN( <fs1> ) + 1.
    MOVE r_bwasl-low TO <fs1>+f_length(3).
    IF r_bwasl-option = 'BT'.
      f_length = STRLEN( <fs1> ).
      MOVE '-' TO <fs1>+f_length(1).
      ADD 1 TO f_length.
      MOVE r_bwasl-high TO <fs1>+f_length(3).
    ENDIF.
    f_length = STRLEN( <fs1> ).
    MOVE ',' TO <fs1>+f_length(1).
    f_length = STRLEN( <fs1> ).
    IF f_length > 57.
      kount = kount + 1.
      IF kount < 4.
        ASSIGN COMPONENT kount OF STRUCTURE hdata01 TO <fs1>.
      ELSE.
        EXIT.
      ENDIF.
    ENDIF.
    AT LAST.
      f_length = f_length - 1.
      MOVE '.' TO <fs1>+f_length(1).
    ENDAT.
  ENDLOOP.



*1- HEADING LINE: TYPE H
  CLEAR ls_line.
  ls_line-typ   = 'H'.
  ls_line-key   = ''.
  ls_line-info  = w_head03.
  APPEND ls_line TO lt_top_of_page.

*line 2:
  CLEAR ls_line.
  ls_line-typ   = 'A'.
  ls_line-key   = ''.
  ls_line-info  = w_head01.
  APPEND ls_line TO lt_top_of_page.

*Transaction code line 1.
  IF hdata01-head03 <> space.
    CLEAR ls_line.
    ls_line-typ   = 'A'.
    ls_line-key   = ''.
    ls_line-info = hdata01-head03.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

*Transaction code line 2
  IF hdata01-head04 <> space.
    CLEAR ls_line.
    ls_line-typ   = 'A'.
    ls_line-key   = ''.
    ls_line-info = hdata01-head04.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

*Transaction code line 3
  IF hdata01-head05 <> space.
    CLEAR ls_line.
    ls_line-typ   = 'A'.
    ls_line-key   = ''.
    ls_line-info = hdata01-head05.
    APPEND ls_line TO lt_top_of_page.
  ENDIF.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_top_of_page.

ENDFORM.                    "ALV_TOP_OF_PAGE
************************************************************************
***                    END OF PROGRAM                                ***
************************************************************************
