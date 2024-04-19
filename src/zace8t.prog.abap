REPORT ZACE8T MESSAGE-ID AD.

*======================================================================
*                ACE-ABAP 8 for 4.7, ECC 5.0 and higher
*
*            Copyright © 1996 - 2016 PricewaterhouseCoopers.
*                         All rights reserved.
*    PricewaterhouseCoopers refers to the network of member firms of
*    PricewaterhouseCoopers International Limited, each of which is
*               a separate and independent legal entity.
*======================================================================

DATA: FILENAM(400), MESS(400), TEXT(400), ZEILE(400), TREC(4000),
      DOWNDIR(400),
      NEWTRANSBYTE(10) TYPE N, NEWTRANSREC(10) TYPE N,
      CURTRANSBYTE(10) TYPE N, CURTRANSREC(10) TYPE N,
      NEWFILENUM(4) TYPE N,
      TRANSTXT(1) TYPE C VALUE '"',
      TRANSSEP(1) TYPE C VALUE ''''.

DATA : INVCHAR1 type x value '0A',
       INVCHAR2 type x value '0D',
       INVCONV type ref to CL_ABAP_CONV_IN_CE,
       INVXSTR type xstring,
       INVSTR1 type string,
       INVSTR2 type string.

*----------------------------------------------------------------------
PARAMETERS: PATH(100)     TYPE C  OBLIGATORY LOWER CASE
                                           DEFAULT 'D:\TEMP\ACE\'.
PARAMETERS: PARFLAG(1)    TYPE C  OBLIGATORY DEFAULT 'Y'.
PARAMETERS: ART(1)        TYPE C  OBLIGATORY DEFAULT 'I'.
PARAMETERS: NUM(4)        TYPE C  OBLIGATORY DEFAULT '9999'.
PARAMETERS: FILELENG      TYPE I  OBLIGATORY DEFAULT 010485760.
PARAMETERS: CDP(4)        TYPE C  OBLIGATORY DEFAULT '1101'.
PARAMETERS: REPL(1)       TYPE C  OBLIGATORY DEFAULT '`'.
PARAMETERS: SELDATE       TYPE D  OBLIGATORY DEFAULT '20050101'.
*----------------------------------------------------------------------

IF ART = 'I'.
  MESS = 'Do not start manually this program (use ZACE8M)!'.
  MESSAGE ID 'AD' TYPE 'S' NUMBER 10 WITH MESS.
  LEAVE PROGRAM.
ENDIF.

INVXSTR = INVCHAR1.

CALL METHOD CL_ABAP_CONV_IN_CE=>CREATE
  EXPORTING
    INPUT       = INVXSTR
    ENCODING    = 'UTF-8'
    REPLACEMENT = '?'
    IGNORE_CERR = ABAP_TRUE
  RECEIVING
    CONV        = INVCONV.

CALL METHOD INVCONV->READ
  IMPORTING
    DATA = INVSTR1.

INVXSTR = INVCHAR2.

CALL METHOD CL_ABAP_CONV_IN_CE=>CREATE
  EXPORTING
    INPUT       = INVXSTR
    ENCODING    = 'UTF-8'
    REPLACEMENT = '?'
    IGNORE_CERR = ABAP_TRUE
  RECEIVING
    CONV        = INVCONV.

CALL METHOD INVCONV->READ
  IMPORTING
    DATA = INVSTR2.

NEWFILENUM = NUM.
PERFORM PROC.

EXPORT NEWTRANSBYTE TO MEMORY ID 'ACEByte'.
EXPORT NEWTRANSREC  TO MEMORY ID 'ACERec'.
EXPORT NEWFILENUM  TO MEMORY ID 'ACEFNum'.

*---------------------------------------------------------------------*
*       FORM TRANS                                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM TRANS.
  DATA: REALTRANSBYTE TYPE I,
        RECLEN TYPE I.

  REALTRANSBYTE = CURTRANSBYTE + CURTRANSREC * 2.

  IF REALTRANSBYTE > FILELENG.
    CLOSE DATASET FILENAM.
    CURTRANSBYTE = 0.
    CURTRANSREC = 0.
    NEWFILENUM = NEWFILENUM + 1.

    CONCATENATE PATH ART NEWFILENUM '.QJF'  INTO FILENAM.

    OPEN DATASET FILENAM FOR OUTPUT IN LEGACY TEXT MODE
                                    CODE PAGE CDP
                                    REPLACEMENT CHARACTER REPL
                                    IGNORING CONVERSION ERRORS
                                    MESSAGE MESS.

    IF SY-SUBRC <> 0.
      MOVE SY-SUBRC            TO TEXT.
      CONCATENATE 'File-Open-Error:' FILENAM TEXT MESS
                    INTO ZEILE SEPARATED BY SPACE.
      WRITE / ZEILE.
      EXIT.
    ENDIF.
  ENDIF.

  REPLACE ALL OCCURRENCES OF INVSTR1 IN TREC WITH ' '.
  REPLACE ALL OCCURRENCES OF INVSTR2 IN TREC WITH ' '.

  RECLEN = STRLEN( TREC ).

  TRANSFER TREC TO FILENAM.

  NEWTRANSBYTE = NEWTRANSBYTE + RECLEN.
  CURTRANSBYTE = CURTRANSBYTE + RECLEN.

  NEWTRANSREC =  NEWTRANSREC + 1.
  CURTRANSREC =  CURTRANSREC + 1.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM proc                                                     *
*---------------------------------------------------------------------*
form proc.

endform.
