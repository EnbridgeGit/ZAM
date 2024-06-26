*&---------------------------------------------------------------------*
*& Report  ZAVOOBJNR
*&
*&---------------------------------------------------------------------*
*&SAPNote 663290 -Correction report: Operation with faulty object number
*&Repair operations with a faulty objnr
*&---------------------------------------------------------------------*

REPORT  ZAVOOBJNR.
TABLES: afko,
        afvc.
DATA: BEGIN OF lt_aufpl OCCURS 0,
        aufpl LIKE afko-aufpl,
      END OF lt_aufpl.
DATA: ls_aufpl LIKE lt_aufpl.
DATA: BEGIN OF lt_afvc OCCURS 0.
        INCLUDE STRUCTURE afvc.
DATA: END OF lt_afvc.
DATA: ls_afvc  LIKE lt_afvc.
DATA: lv_aufnr LIKE afko-aufnr.
DATA: one_found TYPE flag.

* Selection screen
SELECT-OPTIONS: p_aufnr FOR afko-aufnr OBLIGATORY.
PARAMETERS: testmode TYPE test_x AS CHECKBOX DEFAULT 'X'.

START-OF-SELECTION.
* get the AUFPL
  SELECT aufpl INTO TABLE lt_aufpl FROM afko
        WHERE aufnr IN p_aufnr.


  CHECK sy-subrc = 0.
* get the operations to the AUFPL
  LOOP AT lt_aufpl INTO ls_aufpl.
    SELECT * APPENDING TABLE lt_afvc FROM afvc
       WHERE aufpl = ls_aufpl.
  ENDLOOP.
* Headline
  IF testmode IS INITIAL.
    WRITE: 'UPDATEMODE'.
    SKIP 2.
  ELSE.
    WRITE: 'TESTMODE'.
    SKIP 2.
  ENDIF.
* check faulty operations
  LOOP AT lt_afvc INTO ls_afvc.
    IF ls_afvc-objnr+2(10) <> ls_afvc-aufpl.
* set flag for counter
      one_found = 'X'.
* repair the operations
      ls_afvc-objnr(2)    = 'OV'.
      ls_afvc-objnr+2(10) = ls_afvc-aufpl.
      ls_afvc-objnr+12(8) = ls_afvc-aplzl.
* get the order number for displaying purpose
      SELECT aufnr FROM afko INTO lv_aufnr
        WHERE aufpl = ls_afvc-aufpl.
      ENDSELECT.
      IF testmode IS INITIAL.
        WRITE: / 'Order', lv_aufnr, 'operation',
        ls_afvc-aplzl, 'corrected'.
        UPDATE afvc SET objnr = ls_afvc-objnr
          WHERE aufpl = ls_afvc-aufpl AND
                aplzl = ls_afvc-aplzl.
      ELSE.
        WRITE: / 'Order', lv_aufnr, 'operation',
                 ls_afvc-aplzl,'would be corrected'.
      ENDIF.

    ENDIF.
  ENDLOOP.
  IF one_found IS INITIAL.
    WRITE: / 'No affected operations found'.
  ENDIF.
