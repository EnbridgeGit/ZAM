*&---------------------------------------------------------------------*
*&  Include           ZARBREM_TOP
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Include Name       : ZARBREM_TOP                                    *
*& Author             : Praveena Anusuri                               *
*& Creation Date      : 07-Jan-2014                                    *
*& Object ID          : SDP46414 Remittance Advice (ZARBREM) Batch     *
*&                      Program                                        *
*& Application Area   : MM                                             *
*& Description        : This report generates the Remittance Advice for*
*&                      Ariba.                                         *
*&---------------------------------------------------------------------*
TYPE-POOLS: slis.
TABLES: regup.

************************************************************************
* DATA DECLARATION
************************************************************************
TYPES: BEGIN OF ty_reguh,
       laufd  TYPE laufd,
       laufi  TYPE laufi,
       xvorl  TYPE xvorl,
       zbukr  TYPE dzbukr,
       lifnr  TYPE lifnr,
       vblnr  TYPE vblnr,
       zaldt  TYPE dzaldt_zhl,
       rzawe  TYPE rzawe, "(+)PANUSURI Ticket 46414
       END OF ty_reguh,
       BEGIN OF ty_regup,
       laufd  TYPE laufd,
       laufi  TYPE laufi,
       xvorl  TYPE xvorl,
       zbukr  TYPE dzbukr,
       lifnr  TYPE lifnr,
       vblnr  TYPE vblnr,
       bukrs  TYPE bukrs,
       belnr  TYPE belnr_d,
       gjahr  TYPE gjahr,
       xblnr  TYPE xblnr1,
       blart  TYPE blart, "(+)SKAPSE Ticket 78045
       budat  TYPE budat,
       dmbtr  TYPE dmbtr,
       sknto  TYPE sknto,
       END OF ty_regup,
       BEGIN OF ty_lfa1,
       lifnr  TYPE lifnr,
       name1  TYPE name1_gp,
       emnfr  TYPE emnfr,
       END OF ty_lfa1,
*BOI by PANUSURI Ticket 46414
       BEGIN OF ty_bseg_pmt,
       bukrs  TYPE bukrs,
       belnr  TYPE belnr_d,
       gjahr  TYPE gjahr,
       END OF ty_bseg_pmt,
*EOI by PANUSURI Ticket 46414
       BEGIN OF ty_bseg,
       bukrs  TYPE bukrs,
       belnr  TYPE belnr_d,
       gjahr  TYPE gjahr,
       esrre  TYPE esrre,
       END OF ty_bseg,
*BOI by PANUSURI Ticket 46414
       BEGIN OF ty_payr,
       zbukr  TYPE dzbukr,
       rzawe  TYPE dzlsch,
       chect  TYPE chect,
       laufd  TYPE laufd,
       lifnr  TYPE lifnr,
       vblnr  TYPE vblnr,
       END OF ty_payr,
*EOI by PANUSURI Ticket 46414
       BEGIN OF ty_output,
       laufd     TYPE laufd,
       laufi     TYPE laufi,
       lifnr     TYPE lifnr,
       vblnr     TYPE vblnr,
       belnr     TYPE belnr_d,
       gjahr     TYPE gjahr,
       xblnr     TYPE xblnr1,
       budat     TYPE budat,
       gross_amt TYPE dmbtr,
       paid_amt  TYPE dmbtr,
       disc_amt  TYPE sknto,
       name1     TYPE name1_gp,
       emnfr     TYPE emnfr,
       esrre     TYPE esrre,
       zaldt     TYPE dzaldt_zhl,
       rzawe     TYPE dzlsch, "(+)PANUSURI Ticket 46414
       chect     TYPE chect,  "(+)PANUSURI Ticket 46414
blart     TYPE blart,  "(+)SKAPSE Ticket 78045
       END OF ty_output,
       BEGIN OF ty_output_alv,
       laufd     TYPE laufd,
       laufi     TYPE laufi,
       lifnr     TYPE lifnr,
       belnr     TYPE belnr_d,
       END OF ty_output_alv.

DATA: lt_reguh       TYPE STANDARD TABLE OF ty_reguh,
      lwa_reguh      TYPE ty_reguh,
      lt_regup       TYPE STANDARD TABLE OF ty_regup,
      lwa_regup      TYPE ty_regup,
      lt_zeftdup     TYPE STANDARD TABLE OF zeftdup,
      lwa_zeftdup    TYPE zeftdup,
      lt_lfa1        TYPE STANDARD TABLE OF ty_lfa1,
      lwa_lfa1       TYPE ty_lfa1,
      lt_bseg_pmt    TYPE STANDARD TABLE OF ty_bseg_pmt,"(+)PANUSURI Ticket 46414
      lwa_bseg_pmt   TYPE ty_bseg_pmt,                  "(+)PANUSURI Ticket 46414
      lt_bseg        TYPE STANDARD TABLE OF ty_bseg,
      lwa_bseg       TYPE ty_bseg,
      lt_payr        TYPE STANDARD TABLE OF ty_payr,  "(+)PANUSURI Ticket 46414
      lwa_payr       TYPE ty_payr,                    "(+)PANUSURI Ticket 46414
      lt_output      TYPE STANDARD TABLE OF ty_output,
      lwa_output     TYPE ty_output,
      lt_output_alv  TYPE STANDARD TABLE OF ty_output_alv,
      lwa_output_alv TYPE ty_output_alv,
      lwa_listheader TYPE slis_listheader,
      lt_listheader  TYPE slis_t_listheader,
      lt_fieldcat    TYPE slis_t_fieldcat_alv,
      lwa_layout     TYPE slis_layout_alv.

************************************************************************
*SELECTION-SCREEN
************************************************************************
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_laufd FOR regup-laufd OBLIGATORY, "Date on Which the Program Is to Be Run
                s_lifnr FOR regup-lifnr.            "Account Number of Vendor or Creditor
SELECTION-SCREEN END OF BLOCK b1.
