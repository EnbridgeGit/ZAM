*&--------------------------------------------------------------------*
*& Report  ZACORR153                                                  *
*&                                                                    *
*&--------------------------------------------------------------------*
*&                                                                    *
*& Initalisieren neue Felder in der Tabelle ANLZ                      *
*&                                                                    *
*& Voraussetzung:                                                     *
*&                                                                    *
*& In der DB-Tabelle ANLZ sind neue Felder dazugekommen. Durch das    *
*& Hinzufügen ist aber der Feldinhalt in verschiedenen Datenbank-     *
*& systemen nicht auf "INITIAL" gesetzt, sondern beinhaltet einen nicht*
*& umsetzbaren HEX-Wert. Dadurch liefern verschiedene Reports (z.B.   *
*& Anlagengitter) unterschiedliche Ergebnisse in der Einzel- und      *
*& Summensicht.                                                       *
*&                                                                    *
*& Fehlerbeseitigung:                                                 *
*& Durch Aufruf dieses Reports werden alle neuen Felder initalisiert  *
*& sofern noch kein Update erfolgte.                                  *
*&                                                                    *
*&                                                                    *
*& Der Report kann immer wieder neu aufgesetzt werden.                *
*&                                                                    *
*&                                                                    *
*&--------------------------------------------------------------------*

REPORT  ZACORR153                               .


TABLES: ANLZ,
        T000,
        T001.

DATA: ANZ             LIKE SY-DBCNT,
      BLOCKSIZE       LIKE SY-DBCNT VALUE 10000,
      FLG_PROB, FLG_PROB_GES.

SELECT-OPTIONS   SO_BUKRS FOR ANLZ-BUKRS.

DATA: BEGIN OF I_ANLZ OCCURS 0.
        INCLUDE STRUCTURE ANLZ.
DATA: END OF I_ANLZ.

DATA C TYPE CURSOR.


* es existieren schon Komplexe => Einzellesen ist notwendig.
           OPEN CURSOR WITH HOLD C
                             FOR SELECT * FROM ANLZ
                             WHERE   BUKRS IN SO_BUKRS.
            DO.
              FETCH NEXT CURSOR C INTO TABLE I_ANLZ PACKAGE SIZE 10000.
              IF SY-SUBRC NE 0.
                CLOSE CURSOR C.
                EXIT.
              ENDIF.
              UPDATE ANLZ CLIENT SPECIFIED FROM TABLE I_ANLZ.
              IF SY-SUBRC NE 0.
                FLG_PROB = '1'.
              ENDIF.
              CALL FUNCTION 'DB_COMMIT'.
            ENDDO.

 WRITE: / 'Anzahl aktualisierte Sätze:',
          SY-DBCNT.
