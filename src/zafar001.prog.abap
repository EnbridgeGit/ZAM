REPORT RABEST01 MESSAGE-ID AB
                LINE-SIZE 130
                NO STANDARD PAGE HEADING.

TABLES: ANLH,
        ANLA0,
        ANLAV,
        ANLB,
        ANEK,
        ANEPV,
        ANLCV.

TABLES RAPAR.
* FIELDS-Anweisungen.
INCLUDE RASORT00.

* Allgemeine DATA-, TABLES-, ... Anweisungen.
INCLUDE RASORT04.

FIELD-GROUPS: HEADER, DATEN.

DATA:
*     Anzahl der im Anforderungsbild erlaubten AfA-Bereiche.
      SAV_ANZBE(1) TYPE C VALUE '1',
*     Flag: Postenausgabe Ja='1'/Nein='0'.
      FLG_POSTX(1) TYPE C VALUE '0',
*     Summenbericht: Maximale Anzahl Wertfelder/Zeile.
      CON_WRTZL(2) TYPE P VALUE 3.

* Ausgabe-Wertfelder.
DATA: BEGIN OF X,
*       Kumulierter Anschaffungswert einschliesslich Inv-Zus und Aufw.
        KANSW       LIKE ANLCV-ANSW_GJE,
*       Kumulierte Gesamt-AfA einschliesslich Aufw N-AfA.
        KUMAFA      LIKE RAREP-KUMAFA,
*       Restbuchwert einschliesslich Inv-Zus und Aufw.
        BCHWRT      LIKE ANLCV-BCHWRT_GJE,
      END OF X.

* Sortier-Wertfelder.
DATA: BEGIN OF SORT,
*       Kumulierter Anschaffungswert einschliesslich Inv-Zus und Aufw.
        KANSW       LIKE ANLCV-ANSW_GJE,
*       Kumulierte Gesamt-AfA einschliesslich Aufw N-AfA.
        KUMAFA      LIKE RAREP-KUMAFA,
*       Restbuchwert einschliesslich Inv-Zus und Aufw.
        BCHWRT      LIKE ANLCV-BCHWRT_GJE,
      END OF SORT.

SELECTION-SCREEN BEGIN OF BLOCK BL1                        "VCL
                 WITH FRAME                                "VCL
                 TITLE TEXT-BL1.                           "VCL
  SELECT-OPTIONS:
*                 Anlagenbestandkonto.
                  SO_KTANW FOR ANLAV-KTANSW NO DATABASE SELECTION,
*                 Aktivierungsdatum.
                  SO_AKTIV FOR ANLAV-AKTIV,
*                 Aktueller Anschaffungswert.
                  SO_KANSW FOR X-KANSW,
*                 Kumulierte gesamte AfA.
                  SO_KUMAF FOR X-KUMAFA,
*                 Aktueller Buchwert.
                  SO_BCHWR FOR X-BCHWRT.
SELECTION-SCREEN END   OF BLOCK BL1.                       "VCL


SELECTION-SCREEN SKIP.                                     "VCL
SELECTION-SCREEN BEGIN OF BLOCK BL3                        "AB
                 WITH FRAME                                "AB
                 TITLE TEXT-C02.                           "AB
  PARAMETERS:
*             Keine zusaetzlich Sortierung.                "VCL
              PA_SRTW0 LIKE RAREP-SRTWRT                   "VCL
                       RADIOBUTTON GROUP RAD1,             "VCL
*             Zusaetzlich Sortierung nach Anschaffungswert.
              PA_SRTW1 LIKE RAREP-SRTWRT
                       RADIOBUTTON GROUP RAD1,             "VCL
*             Zusaetzliche Sortierung nach kumulierter AfA.
              PA_SRTW2 LIKE RAREP-SRTWRT
                       RADIOBUTTON GROUP RAD1,             "VCL
*             Zusaetzlich Sortierung nach Buchwert.
              PA_SRTW3 LIKE RAREP-SRTWRT
                       RADIOBUTTON GROUP RAD1,             "VCL
*             Hitliste: Top nnnnn.
              PA_HITLI LIKE RAREP-HITLI DEFAULT '00000'.
* Pro forma.
  DATA: PA_SRTW4  LIKE RAREP-SRTWRT,
        PA_SRTW5  LIKE RAREP-SRTWRT,
        PA_SRTW6  LIKE RAREP-SRTWRT,
        PA_SRTW7  LIKE RAREP-SRTWRT,
        PA_SRTW8  LIKE RAREP-SRTWRT,
        PA_SRTW9  LIKE RAREP-SRTWRT,
        PA_SRTW10 LIKE RAREP-SRTWRT.
SELECTION-SCREEN END   OF BLOCK BL3.                       "AB


SELECTION-SCREEN SKIP.
SELECTION-SCREEN BEGIN OF BLOCK BL4                       "AB
                 WITH FRAME                                "AB
                 TITLE TEXT-C03.                           "AB
  PARAMETERS:
*             Zusatzueberschrift.
              PA_TITEL LIKE RAREP-TITEL DEFAULT SPACE,
*             Flag: Listseparation gemaess Tabelle TLSEP.
              PA_LSSEP LIKE BHDGD-SEPAR,
*             Flag: Mikrofichezeile ausgeben.
              PA_MIKRO LIKE BHDGD-MIFFL.
SELECTION-SCREEN END   OF BLOCK BL4.                       "VCL


INITIALIZATION.
* Titel setzen bei Workflow.
  IF SY-TCODE = 'AR01'.              " Erzeugen Arbeitsvorrat
    SET TITLEBAR '001'.
  ENDIF.

* Sortiervariante vorschlagen.
  MOVE: '0001' TO SRTVR.



* Report wird nicht von außen aufgerufen. Lesen der PickUp-Informationen
* aus dem Memory d.h. der ursprünglich eingegebenen Programmabgrenzungen
IMPORT FLG_NOT_FIRST FROM MEMORY ID 'flg'.

* Allgemeine Verarbeitung der PA/SO-Eingaben.
INCLUDE RASORT08.
*---------------------------------------------------------------------*


START-OF-SELECTION.

* Sichern der Selektionsoptionen bzw. Einlesen der Sortierwerte bei
* PickUp.
  PERFORM INFO_PICK_UP.

* PF-Status setzen.
* SET PF-STATUS 'LIST'.

* Summenbericht: Ueberschriften der Wertfelder.
IF SUMMB NE SPACE.
  WRITE  TEXT-W01 TO SFLD-FNAME.
  APPEND SFLD.
  WRITE  TEXT-W02 TO SFLD-FNAME.
  APPEND SFLD.
  WRITE  TEXT-W03 TO SFLD-FNAME.
  APPEND SFLD.

ENDIF.

* Bestimmung des Sortierfeldes auf unterster Gruppenstufe.
ASSIGN SAV_DUMMY TO <B>.
* Zusaetzlich Sortierung nach Wertfeld auf unterster Gruppenstufe.
IF PA_SRTW1 NE SPACE.
  ASSIGN SORT-KANSW  TO <B>.
ENDIF.
IF PA_SRTW2 NE SPACE.
  ASSIGN SORT-KUMAFA TO <B>.
ENDIF.
IF PA_SRTW3 NE SPACE.
  ASSIGN SORT-BCHWRT TO <B>.
ENDIF.

* Hitliste: Wertfelder fuer Summe ueber Hit-Anlagen.
IF NOT PA_HITLI IS INITIAL.
  ASSIGN X-KANSW  TO <H1>.
  ASSIGN X-KUMAFA TO <H2>.
  ASSIGN X-BCHWRT TO <H3>.
* Nicht benoetigte Summenfelder.
  ASSIGN SAV_WDUMMY TO: <H4>, <H5>, <H6>, <H7>, <H8>, <H9>, <H10>,
    <H11>, <H12>, <H13>, <H14>, <H15>, <H16>, <H17>, <H18>, <H19>,
    <H20>.
ENDIF.

* Bestimmung des Sortierfeldes fuer Gitterposition oder Einzelposten.
ASSIGN SAV_DUMMY TO <P>.
ASSIGN SAV_DUMMY TO <Q>.

* Allgemeines Coding nach START-OF-SELECTION. Aufbau des HEADERs.
INCLUDE RASORT10.

INSERT
*        Daten zur Anlage.
         ANLAV-ANLN0
*        ANLAV-ANLN1        ANLAV-ANLN2        ANLAV-WERKS
         ANLAV-ANLN1        ANLAV-ANLN2
*        ANLAV-TXT50        ANLAV-KOSTL        ANLAV-MENGE
         ANLAV-TXT50
*        ANLAV-POSNR        ANLAV-AKTIV        ANLAV-LIFNR
                            ANLAV-AKTIV        ANLAV-LIFNR
*        ANLAV-MEINS        ANLAV-STORT        ANLAV-TXA50
                                               ANLAV-TXA50
         ANLAV-ANLHTXT      ANLAV-XANLGR
*        Daten zum AfA-Bereich.
                                               SAV_WAER1
*        Wertfelder.
         X-KANSW            X-KUMAFA           X-BCHWRT
         INTO DATEN.


* Steuerungskennzeichen für LDB setzen
GET ANLA0.

* Angabe der NICHT-KEY-Felder aus der Datenbank
GET ANLAV FIELDS AKTIV KTOGR  TXT50
                 TXA50 ZUGDT  DEAKT XANLGR.


  CHECK SELECT-OPTIONS.
* Nur Anlagen seleketieren, die aktiviert wurden ...
  CHECK NOT ANLAV-ZUGDT IS INITIAL.
* ... und zwar vor dem Berichtsdatum.
  CHECK     ANLAV-ZUGDT LE BERDATUM.

* Verarbeitungen ON CHANGE OF ANLAV-XXXXX.
  INCLUDE RASORT14.

* Im VJ deaktivierte Anlagen nicht selektieren.
  IF NOT ANLAV-DEAKT IS INITIAL.
    CHECK ANLAV-DEAKT GE SAV_GJBEG.
    CHECK ANLAV-DEAKT GT BERDATUM.
  ENDIF.

  ON CHANGE OF ANLAV-BUKRS.
*   Individueller Teil des Headers
    WRITE: '-'       TO HEAD-INFO1,
           BEREICH1  TO HEAD-INFO2,
           SAV_AFBE1 TO HEAD-INFO3.
*
    CONDENSE HEAD.
  ENDON.

* keine Bereichsdaten erforderlich
GET ANLB FIELDS ANLN1.


GET ANLCV.

  CHECK SELECT-OPTIONS.

  PERFORM FEHLER_AUSGEBEN.

* Werte berechnen.
  PERFORM WERTE_BERECHNEN.

  CHECK SO_KANSW.
  CHECK SO_KUMAF.
  CHECK SO_BCHWR.

* Daten gegen Sortierwerte beim PickUp checken.
  PERFORM SORT_CHECK.
* Keine Unterteilung der extrahierten Saetze ==> Jeder hat Rang '1'.
  RANGE = '1'.
* DATEN extrahieren.
  EXTRACT DATEN.

END-OF-SELECTION.

*---------------------------------------------------------------------*

* Bestand sortieren.
INCLUDE RASORT20.

LOOP.
* AT NEW - Allgemeine Steuerungen.
  INCLUDE RASORT24.
* Kein Summenbereicht.
  IF SUMMB EQ SPACE.
    AT DATEN.
*     Hitliste: Hit-Summe hochzaehlen, Hit-Zaehler hochsetzen.
      PERFORM HITSUMME_BILDEN.
*
      PERFORM DATEN_AUSGEBEN.
    ENDAT.
  ENDIF.
* AT END OF - Allgemeine Steuerungen, Summenausgaben.
  INCLUDE RASORT28.
ENDLOOP.

PERFORM NO_RECORDS.

*---------------------------------------------------------------------*

TOP-OF-PAGE.

* Allgemeine TOP-OF-PAGE-Verarbeitung.
  INCLUDE RASORT30.

*---------------------------------------------------------------------*

* PickUp auf Anlage oder Beleg (AT LINE-SELECTION).
INCLUDE RASORT50 .

* Allgemeine FORM-Routinen.
*$*$------- Start of INCLUDE RASORT40 -------( NO UPDATE )-------$*$*



*---------------------------------------------------------------------*
* Merken der benoetigten Feldattribute der Sortierfelder (aus DDIC)   *
* in der internen Tabelle FELD.                                       *
*---------------------------------------------------------------------*
* <-- FELD       Tabelle mit Feldattributen der Sortierfelder         *
* <-- FLG_XANLN1 Kennzeichen "Summe je Hauptnummer ausgeben"          *
*---------------------------------------------------------------------*
FORM FELDATTRIBUTE_MERKEN.

* Keine Hitliste.
  IF PA_HITLI IS INITIAL.

*   T086 einlesen.
    SELECT SINGLE * FROM T086
      WHERE SRTVAR EQ SRTVR.
*
    IF SY-SUBRC EQ 0.
*     Summe je Anlage ausgeben.
      MOVE T086-XANLN1 TO FLG_XANLN1.
*     Für Add-Fields eine Initialisierung durchführen
      IF T086-ANZUNTNR IS INITIAL.
        T086-ANZUNTNR = 2.
      ENDIF.
    ELSE.
*     Keine Sortiervariante angegeben.
      CLEAR FLG_XANLN1.
    ENDIF.

*   Tabelle Feld fuellen.
    REFRESH FELD.
    PERFORM FELD_FUELLEN USING T086-TABLN1  T086-FELDN1
                               T086-OFFSET1 T086-LAENGE1
                               T086-XSUMM1  T086-XAFLG1.
    PERFORM FELD_FUELLEN USING T086-TABLN2  T086-FELDN2
                               T086-OFFSET2 T086-LAENGE2
                               T086-XSUMM2  T086-XAFLG2.
    PERFORM FELD_FUELLEN USING T086-TABLN3  T086-FELDN3
                               T086-OFFSET3 T086-LAENGE3
                               T086-XSUMM3  T086-XAFLG3.
    PERFORM FELD_FUELLEN USING T086-TABLN4  T086-FELDN4
                               T086-OFFSET4 T086-LAENGE4
                               T086-XSUMM4  T086-XAFLG4.
    PERFORM FELD_FUELLEN USING T086-TABLN5  T086-FELDN5
                               T086-OFFSET5 T086-LAENGE5
                               T086-XSUMM5  T086-XAFLG5.

* Hitliste.
  ELSE.

*   Tabelle Feld mit dem einzigen Eintrag ANLAV-BUKRS fuellen.
    REFRESH FELD.
    CLEAR FELD.
*
    MOVE: 'ANLAV' TO FELD-TABLN,
          'BUKRS' TO FELD-FELDN,
          'X'     TO FELD-XSUMM.
*
    PERFORM GET_FIELD(RDDFIE00)
      USING FELD-TABLN  FELD-FELDN  SY-LANGU
      CHANGING DFIES SY-SUBRC.
    MOVE: DFIES-SCRTEXT_M TO FELD-FTEXT,
          DFIES-HEADLEN   TO FELD-LAENG,
          DFIES-REPTEXT   TO FELD-SPALT.
*
    APPEND FELD.
*
  ENDIF.

* Muss Kostenstellenstamm/Bilanzversion gelesen werden?
  LOOP AT FELD
    WHERE TABLN EQ 'ANLAV'
    AND ( FELDN EQ 'KHINR' OR
          FELDN EQ 'ERGSO' OR
          FELDN EQ 'ERGHB' ).
    CASE FELD-FELDN.
      WHEN 'KHINR'.
         *ANLA0-XKOST = 'X'.
      WHEN 'ERGSO'.
         *ANLA0-XBILV = 'X'.
      WHEN 'ERGHB'.
         *ANLA0-XBILV = 'X'.
    ENDCASE.
  ENDLOOP.

* Summenbericht + keine Stufe zum Summieren vorgesehen ...
  IF SUMMB NE SPACE.
    DESCRIBE TABLE FELD LINES CNT_COUNT.
    IF CNT_COUNT GT 0.
      LOOP AT FELD
        WHERE XSUMM NE SPACE.
      ENDLOOP.
    ENDIF.
    IF CNT_COUNT = 0  OR
     ( CNT_COUNT > 0  AND
       SY-SUBRC NE 0 ).
*          ... Fehler!
      MESSAGE E036 WITH SRTVR.
    ENDIF.
  ENDIF.

* Anzahl Sortierfelder in Sortierstufe merken.
  DESCRIBE TABLE FELD LINES CNT_COUNT.
  MOVE CNT_COUNT TO CON_SRTST.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM KONSTANTEN_ERMITTELN                                     *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM KONSTANTEN_ERMITTELN.

* (1) CON_TXPOS: Offset nach dem der Gruppenstufentext beginnt.
  CNT_OFFST = 1.
  LOOP AT FELD.
    CNT_OFFST = CNT_OFFST + FELD-LAENG + 1.
  ENDLOOP.
  CON_TXPOS = CNT_OFFST.
* Nicht mal genug Platz fuer Sortierstufe + Text --> Abbruch.
  CNT_COUNT = CON_TXPOS + CON_LGMIN + 1.
  IF CNT_COUNT GT SY-LINSZ.
    STOP.
  ENDIF.

* (2) CON_WRTZL: Anzahl Wertfelder je Zeile im Summenbericht
*                (wird vom individuellen Report uebergeben).
  DO.
    CNT_COUNT = 1 + 16 * CON_WRTZL.
*   Gewuenschte Anzahl Wertfelder passt in Liste, jubel ...
    IF CNT_COUNT LE SY-LINSZ.
      EXIT.
*   ... andernfalls kuerzen!
    ELSE.
      CON_WRTZL = CON_WRTZL - 1.
    ENDIF.
  ENDDO.

* (3) CON_LGTXT: Optimale Laenge des Gruppenstufentextes
*                zwischen CON_LGMIN und CON_LGMAX.
*     CON_SUPOS: Beginnposition des ersten Wertfeldes.
  CNT_COUNT = CON_TXPOS + CON_LGMIN + 1 + 16 * CON_WRTZL.
* Wertfelder gehen noch in ertse Zeile.
  IF CNT_COUNT LE SY-LINSZ.
    CON_LGTXT = SY-LINSZ - CON_TXPOS - 1 - 16 * CON_WRTZL.
    IF CON_LGTXT GT CON_LGMAX.
      CON_LGTXT = CON_LGMAX.
    ENDIF.
    CON_SUPOS = CON_TXPOS + CON_LGTXT + 1.
* Wertfelder gehen nicht mehr in erste Zeile.
  ELSE.
    CNT_OFFST = 1 + 16 * CON_WRTZL.
    CNT_COUNT = CNT_OFFST - CON_TXPOS - 1.
    IF CNT_COUNT GT CON_LGMIN.
      CON_LGTXT = CNT_COUNT.
    ELSE.
      CON_LGTXT = CON_LGMIN.
    ENDIF.
    CON_SUPOS = 1.
  ENDIF.

* (4) CON_ENPOS: Position der letzten Spalte.
  IF CON_SUPOS NE 1.
    CON_ENPOS = CON_SUPOS + 16 * CON_WRTZL.
  ELSE.
    CON_ENPOS = CON_TXPOS + CON_LGTXT + 1.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM FELD_FUELLEN                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_TABLN                                                       *
*  -->  F_FELDN                                                       *
*  -->  F_OFFSET                                                      *
*  -->  F_LAENGE                                                      *
*  -->  F_XSUMM                                                       *
*  -->  F_XAFLG                                                       *
*---------------------------------------------------------------------*
FORM FELD_FUELLEN USING F_TABLN  F_FELDN
                        F_OFFSET F_LAENGE
                        F_XSUMM  F_XAFLG.

  DATA: L_PACK(6)      TYPE P,
        L_CHAR(10)     TYPE C,
        L_FTEXT        LIKE DFIES-REPTEXT,
        L_POSITION     LIKE SY-FDPOS.

  IF F_TABLN NE SPACE.

    CLEAR FELD.
*
    MOVE: F_TABLN TO FELD-TABLN,
          F_FELDN TO FELD-FELDN,
          F_OFFSET TO FELD-OFFSET,
          F_LAENGE TO FELD-LAENGE,
          F_XSUMM TO FELD-XSUMM,
          F_XAFLG TO FELD-XAFLG.
*
    PERFORM GET_FIELD(RDDFIE00)
      USING F_TABLN F_FELDN SY-LANGU
      CHANGING DFIES SY-SUBRC.
*
    MOVE: DFIES-SCRTEXT_M TO FELD-FTEXT,
          DFIES-HEADLEN   TO FELD-LAENG,
          DFIES-REPTEXT   TO FELD-SPALT.
*   Bei Offset und Laenge: FTEXT korrigieren.
    IF NOT F_OFFSET IS INITIAL.
      MOVE: '+'        TO L_CHAR(1),
            F_OFFSET   TO L_PACK.
      WRITE L_PACK     TO L_CHAR+1(2).
    ENDIF.
    IF NOT F_LAENGE IS INITIAL.
      MOVE: '(  )'     TO L_CHAR+3(4),
            F_LAENGE   TO L_PACK.
      WRITE L_PACK     TO L_CHAR+4(2).
    ENDIF.
    CONDENSE L_CHAR NO-GAPS.
    IF L_CHAR CA ' '.
    ENDIF.
    L_POSITION = 20 - SY-FDPOS.
    L_FTEXT = FELD-FTEXT.
    MOVE L_FTEXT+L_POSITION TO  L_CHAR.
*   PERFORM FIELD_ASSIGN USING L_FTEXT+L_POSITION L_CHAR.
    CONDENSE L_FTEXT NO-GAPS.
    FELD-FTEXT = L_FTEXT.
*   Bei Offset und Laenge: LAENG korrigieren.
    IF NOT F_LAENGE IS INITIAL.
      FELD-LAENG = F_LAENGE.
    ENDIF.
    IF NOT F_OFFSET IS INITIAL AND
           F_LAENGE IS INITIAL .
      FELD-LAENG = FELD-LAENG - F_OFFSET.
    ENDIF.
*
    APPEND FELD.

  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM FIELD_ASSIGN                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  F_SOURCE                                                      *
*  -->  F_TARGET                                                      *
*---------------------------------------------------------------------*
FORM FIELD_ASSIGN USING F_SOURCE F_TARGET.

  F_SOURCE = F_TARGET.

ENDFORM.


*---------------------------------------------------------------------*
* Zuweisung der Sortierfelder aus T086 zu den Feldsymbolen.           *
*---------------------------------------------------------------------*
FORM FELDSYMBOLE_ZUWEISEN.

* 1. Feldsymbol.
  READ TABLE FELD INDEX 1.
  IF SY-SUBRC EQ 0.
    PERFORM FLST_AUFBAUEN.
    ASSIGN (FLST) TO <S1>.
  ELSE.
    ASSIGN SAV_DUMMY TO <S1>.
  ENDIF.

* 2. Feldsymbol.
  READ TABLE FELD INDEX 2.
  IF SY-SUBRC EQ 0.
    PERFORM FLST_AUFBAUEN.
    ASSIGN (FLST) TO <S2>.
  ELSE.
    ASSIGN SAV_DUMMY TO <S2>.
  ENDIF.

* 3. Feldsymbol.
  READ TABLE FELD INDEX 3.
  IF SY-SUBRC EQ 0.
    PERFORM FLST_AUFBAUEN.
    ASSIGN (FLST) TO <S3>.
  ELSE.
    ASSIGN SAV_DUMMY TO <S3>.
  ENDIF.

* 4. Feldsymbol.
  READ TABLE FELD INDEX 4.
  IF SY-SUBRC EQ 0.
    PERFORM FLST_AUFBAUEN.
    ASSIGN (FLST) TO <S4>.
  ELSE.
    ASSIGN SAV_DUMMY TO <S4>.
  ENDIF.

* 5. Feldsymbol.
  READ TABLE FELD INDEX 5.
  IF SY-SUBRC EQ 0.
    PERFORM FLST_AUFBAUEN.
    ASSIGN (FLST) TO <S5>.
  ELSE.
    ASSIGN SAV_DUMMY TO <S5>.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
*       FORM FLST_AUFBAUEN                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FLST_AUFBAUEN.

  CLEAR FLST.

  MOVE: FELD-TABLN TO FLST-TABLN,
        '-'        TO FLST-STRIC,
        FELD-FELDN TO FLST-FELDN.
* Offset?
  IF NOT FELD-OFFSET IS INITIAL.
    WRITE: '+'         TO FLST-OFFSET(1),
           FELD-OFFSET TO FLST-OFFSET+1(2).
  ELSE.
    CLEAR FLST-OFFSET.
  ENDIF.
* Laenge?
  IF NOT FELD-LAENGE IS INITIAL.
    WRITE: '(  )'      TO FLST-LAENGE,
           FELD-LAENGE TO FLST-LAENGE+1(2).
  ELSE.
    CLEAR FLST-LAENGE.
  ENDIF.
*
  CONDENSE FLST NO-GAPS.

ENDFORM.


*---------------------------------------------------------------------*
* Einlesen des Kurztextes zu AfA-Bereich AKL_AFABE.                   *
*---------------------------------------------------------------------*
* --> AKL_AFABE  AfA-Bereich                                          *
* <-- AKL_AFKTX  Kurztext zu AfA-Bereich                              *
*---------------------------------------------------------------------*
FORM AFABEKTX_LESEN USING AKL_AFABE AKL_AFKTX.

* Default: Text = SPACE.
  CLEAR AKL_AFKTX.
* Zuerst Bewertungsplan zu Buchungskreis lesen.
  SELECT SINGLE * FROM T093C
    WHERE BUKRS EQ ANLAV-BUKRS.
* Kurztext zu AfA-Bereich lesen.
  IF SY-SUBRC EQ 0.
    SELECT SINGLE * FROM T093T
      WHERE SPRAS  EQ SY-LANGU
      AND   AFAPL  EQ T093C-AFAPL
      AND   AFABER EQ AKL_AFABE.
    IF SY-SUBRC EQ 0.
      AKL_AFKTX = T093T-AFBKTX.
    ENDIF.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
* Ermittlung des Waehrungsschluessels zu AfA-Bereich WER_AFABE.       *
*---------------------------------------------------------------------*
* --> WER_AFABE  AfA-Bereich                                          *
* <-- WER_WAERS  Waehrungsschluessel                                  *
*---------------------------------------------------------------------*
FORM WAEHRUNG_ERMITTELN USING WER_AFABE WER_WAERS.

  CLEAR WER_WAERS.
  IF NOT UMVAR IS INITIAL.
    SELECT SINGLE * FROM T091C WHERE UMVAR = UMVAR.
    IF SY-SUBRC = 0 AND NOT T091C-WAERS IS INITIAL.
      WER_WAERS = T091C-WAERS.
      EXIT.
    ENDIF.
  ENDIF.

* Waehrungsschluessel aus T093B.
  SELECT SINGLE * FROM T093B
    WHERE BUKRS EQ ANLAV-BUKRS
    AND   AFABE EQ WER_AFABE.
  IF SY-SUBRC EQ 0.
    WER_WAERS = T093B-WAERS.
* Nix gefunden?
  ELSE.
*   Dann nimm die Hauswaehrung.
    SELECT SINGLE * FROM T001
      WHERE BUKRS EQ ANLAV-BUKRS.
    WER_WAERS = T001-WAERS.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
* Ausgabe der Gruppensumme bei Summenbericht.                         *
*---------------------------------------------------------------------*
* --> SWRT       Tabelle mit den auszugebenden Summen.                *
*---------------------------------------------------------------------*
FORM GRUSUMME_AUSGEBEN.

  DATA: L_AUFRISS(1) TYPE C VALUE '0',
        L_TABIX      LIKE SY-TABIX.

  FORMAT RESET.

* Kein Summenbericht ==> nix machen.
  IF SUMMB EQ SPACE.
    EXIT.
  ENDIF.

  RESERVE CON_RESRV LINES.

* Ausgabe fuer Aufriss einer Gruppenstufe? Merken!
  IF HLP_STUFE NE SPACE.
    L_AUFRISS = ON.
  ENDIF.

* Naechste Zeile.
  NEW-LINE.
  WRITE SY-VLINE NO-GAP.
* Aufriss --> Mitgegebenen Text statt automatischer Gruppenstufe.
  IF L_AUFRISS EQ ON.
    POSITION 2.
    WRITE HLP_STUFE.
* Kein Aufriss --> Automatisch aufgebaute Gruppenstufe.
  ELSE.
* Sichern der Sortierfeldwerte im Hide-Bereich einer Zeile für ein
* PickUp
    MOVE: HLP_LEVEL      TO Z_FELD_IND,
          'X'            TO FLG_PICK_UP.
    CNT_OFFST = 2.
*    Sortierfeld 1.
    READ TABLE FELD INDEX 1.
    IF SY-SUBRC EQ 0.
      IF HLP_LEVEL GE '1'.
        POSITION CNT_OFFST.
        WRITE <S1> COLOR COL_KEY INTENSIFIED ON.
      ELSE.
        POSITION CNT_OFFST.
        WRITE (132) SPACE.
        POSITION CNT_OFFST.
        WRITE '*'.
      ENDIF.
      CNT_OFFST = CNT_OFFST + FELD-LAENG + 1.
    ENDIF.
*    Sortierfeld 2.
    READ TABLE FELD INDEX 2.
    IF SY-SUBRC EQ 0.
      IF HLP_LEVEL GE '2'.
        POSITION CNT_OFFST.
        WRITE <S2> COLOR COL_KEY INTENSIFIED ON.
      ELSE.
        POSITION CNT_OFFST.
        WRITE (132) SPACE.
        POSITION CNT_OFFST.
        WRITE '*'.
      ENDIF.
      CNT_OFFST = CNT_OFFST + FELD-LAENG + 1.
    ENDIF.
*    Sortierfeld 3.
    READ TABLE FELD INDEX 3.
    IF SY-SUBRC EQ 0.
      IF HLP_LEVEL GE '3'.
        POSITION CNT_OFFST.
        WRITE <S3> COLOR COL_KEY INTENSIFIED ON.
      ELSE.
        POSITION CNT_OFFST.
        WRITE (132) SPACE.
        POSITION CNT_OFFST.
        WRITE '*'.
      ENDIF.
      CNT_OFFST = CNT_OFFST + FELD-LAENG + 1.
    ENDIF.
*    Sortierfeld 4.
    READ TABLE FELD INDEX 4.
    IF SY-SUBRC EQ 0.
      IF HLP_LEVEL GE '4'.
        POSITION CNT_OFFST.
        WRITE <S4> COLOR COL_KEY INTENSIFIED ON.
      ELSE.
        POSITION CNT_OFFST.
        WRITE (132) SPACE.
        POSITION CNT_OFFST.
        WRITE '*'.
      ENDIF.
      CNT_OFFST = CNT_OFFST + FELD-LAENG + 1.
    ENDIF.
*    Sortierfeld 5.
    READ TABLE FELD INDEX 5.
    IF SY-SUBRC EQ 0.
      IF HLP_LEVEL GE '5'.
        POSITION CNT_OFFST.
        WRITE <S5> COLOR COL_KEY INTENSIFIED ON.
      ELSE.
        POSITION CNT_OFFST.
        WRITE (132) SPACE.
        POSITION CNT_OFFST.
        WRITE '*'.
      ENDIF.
      CNT_OFFST = CNT_OFFST + FELD-LAENG + 1.
    ENDIF.

*
    HIDE: FLG_PICK_UP, <S1>, <S2>, <S3>, <S4>, <S5>, Z_FELD_IND.
    CLEAR FLG_PICK_UP.

*    Sortierstufentext.
    POSITION CON_TXPOS.
    WRITE (132) SPACE.
    CNT_OFFST = CON_TXPOS + 1.
    POSITION CNT_OFFST.
    WRITE HLP_FBEZ COLOR COL_KEY INTENSIFIED OFF.
  ENDIF.
  CNT_OFFST = CON_TXPOS + CON_LGTXT + 1.
  POSITION CNT_OFFST.
  WRITE (132) SPACE.
  POSITION CNT_OFFST.
  WRITE SY-VLINE NO-GAP.
* Kein Platz mehr fuer wenigstens ein Wertfeld -->
* Neue Zeile, durch einen Unterstrich abgetrennt.
  IF CON_SUPOS EQ 1.
    CLEAR HLP_CHAR.
    WRITE SY-ULINE TO HLP_CHAR+0(CON_ENPOS).
    NEW-LINE.
    WRITE HLP_CHAR.
    NEW-LINE.
    WRITE SY-VLINE NO-GAP.
  ENDIF.
* Tabelle mit den Wertfeldern ausgeben.
  CNT_OFFST = CON_SUPOS + 1.
  CNT_COUNT = 0.
  LOOP AT SWRT.
*   Maximale Anzahl Wertfelder/Zeile nicht ueberschritten?
    CNT_COUNT = CNT_COUNT + 1.
    IF CNT_COUNT LE CON_WRTZL.
*     Dann dieses Wertfeld in diese Zeile.
      POSITION CNT_OFFST.
      IF L_AUFRISS EQ OFF.
        WRITE (15) SWRT-BETRG COLOR COL_NORMAL INTENSIFIED OFF.
      ELSE.
        WRITE (15) SWRT-BETRG.
      ENDIF.
      CNT_OFFST = CNT_OFFST + 16.
*   Maximale Anzahl Wertfelder/Zeile ueberschritten?
    ELSE.
*      Dann neue Zeile anfangen.
      CNT_OFFST = CON_ENPOS.
      POSITION CNT_OFFST.
      WRITE SY-VLINE NO-GAP.
      NEW-LINE.
      WRITE SY-VLINE NO-GAP.
      IF CON_SUPOS NE 1.
        CNT_OFFST = CON_TXPOS + CON_LGTXT + 1.
        POSITION CNT_OFFST.
        WRITE SY-VLINE NO-GAP.
      ENDIF.
      CNT_OFFST = CON_SUPOS + 1.
      POSITION CNT_OFFST.
      IF L_AUFRISS EQ OFF.
        WRITE (15) SWRT-BETRG COLOR COL_NORMAL INTENSIFIED OFF.
      ELSE.
        WRITE (15) SWRT-BETRG.
      ENDIF.
      CNT_OFFST = CNT_OFFST + 16.
      CNT_COUNT = 1.
    ENDIF.
  ENDLOOP.

* Alles mit VLINE und neuem Unterstrich abschliessen.
  POSITION CON_ENPOS.
  WRITE SY-VLINE NO-GAP.
* Unterstrich,
* -  wenn kein Aufriss gemacht wird oder
* -  wenn Aufriss gemacht wird, aber Wertfelder in eigener Zeile.
  IF L_AUFRISS EQ OFF OR
     CON_SUPOS EQ 1   .
    CLEAR HLP_CHAR.
    WRITE SY-ULINE TO HLP_CHAR+0(CON_ENPOS).
    NEW-LINE.
    WRITE HLP_CHAR.
  ENDIF.

  FORMAT RESET.
* Fuellen der Tabellen zur Datenuebergabe an EXCEL.
  IF XXL_NICHT_DA IS INITIAL.
    PERFORM XXL_DATEN.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
* Ausgabe der Gruppensumme bei Summenbericht.                         *
*---------------------------------------------------------------------*
*     SWRT       Tabelle mit den auszugebenden Summen (Format C).     *
*---------------------------------------------------------------------*
* --> GSA_STUFE  Gruppenstufe: Feldinhalte Sortierfelder.             *
*---------------------------------------------------------------------*
FORM GRUSUMFRAME_AUSGEBEN USING GSA_STUFE GSA_ULINE.
  DATA: LD_RESRV LIKE CON_RESRV,
        LD_LINE  LIKE SY-LINNO.

  FORMAT COLOR COL_BACKGROUND.
  LD_RESRV = 2 * CON_RESRV + 1.
  RESERVE LD_RESRV LINES.
* SUMMARY/DETAIL-Steuerung erste Zeile.
*  IF FLG_DETAIL EQ OFF.
*    SUMMARY.
*  ELSE.
*    DETAIL.
*  ENDIF.
  LD_LINE = SY-LINNO - 1.
  SKIP TO LINE LD_LINE.
* Gruppenstufe mit Inhalt der Sortierfelder.
  WRITE: 001 GSA_ULINE,
          /001 SY-VLINE NO-GAP,
           002 GSA_STUFE COLOR COL_GROUP INTENSIFIED.
* Sollen Wertfelder u n t e r  (nicht  n e b e n )  Sortierstufe?
  IF CON_SUPOS EQ 0.
    WRITE / GSA_ULINE.
    NEW-LINE.
  ENDIF.
* Offset erste Summe.
  CNT_OFFST = CON_SUPOS + 1.
* Wertfelder  u n t e r  die Sortierstufe ...
  IF CON_SUPOS EQ 0.
*   ... dann auf jeden Fall mit DETAIL weiter.
*   DETAIL.
  ENDIF.
* Gar kein Platz mehr fuer Wertfeld ==> Abbrechen.
  CNT_COUNT = CON_SUPOS + 14.
  IF CNT_COUNT GT SY-LINSZ.
    STOP.
  ENDIF.

* Tabelle mit den Wertfeldern.
  LOOP AT SWRT.
*   Wertfeld ...
    CNT_COUNT = CNT_OFFST + 14.
*   ... noch in Zeile
    IF CNT_COUNT LE SY-LINSZ  AND
*   und maximale Anzahl Wertfelder/Zeile nicht ueberschritten?
       CNT_COUNT LE CON_ENPOS .
*     Dann dieses Wertfeld in diese Zeile.
      POSITION CNT_OFFST.
      WRITE SY-VLINE NO-GAP.
      WRITE SWRT-BETRG COLOR COL_TOTAL NO-GAP.
      WRITE SY-VLINE NO-GAP.
*   ... nicht mehr in dieser Zeile unterbringbar?
    ELSE.
*     Dann neue Zeile anfangen.
      CNT_OFFST = CON_SUPOS + 1.
*     Wertfeld in naechste Zeile.
      NEW-LINE.
      WRITE SY-VLINE NO-GAP.
      POSITION CNT_OFFST.
      WRITE SY-VLINE NO-GAP.
      WRITE SWRT-BETRG COLOR COL_TOTAL NO-GAP.
      WRITE SY-VLINE NO-GAP.
    ENDIF.
*   Offset naechstes Feld.
    CNT_OFFST = CNT_OFFST + 16.
  ENDLOOP.

  NEW-LINE.
  WRITE  001 GSA_ULINE.

ENDFORM.
*---------------------------------------------------------------------*
* Ausgabe der Gruppensumme bei Summenbericht.                         *
*---------------------------------------------------------------------*
*     SWRT       Tabelle mit den auszugebenden Summen (Format C).     *
*---------------------------------------------------------------------*
* --> GSA_STUFE  Gruppenstufe: Feldinhalte Sortierfelder.             *
*---------------------------------------------------------------------*
*ORM GRUSUMME_AUSGEBEN USING GSA_STUFE.
*
* RESERVE CON_RESRV LINES.
* Gruppenstufe mit Inhalt der Sortierfelder.
* IF FLG_DETAIL NE ON.
*   SUMMARY.
* ELSE.
*   DETAIL.
* ENDIF.
* WRITE /001     GSA_STUFE.
* Offset erste Summe.
* CNT_OFFST = CON_SUPOS + 1.
* Gar kein Platz mehr fuer Wertfeld ==> Abbrechen.
* CNT_COUNT = CON_SUPOS + 14.
* IF CNT_COUNT GT SY-LINSZ.
*   STOP.
* ENDIF.
* Tabelle mit den Wertfeldern.
* LOOP AT SWRT.
*   Wertfeld ...
*   CNT_COUNT = CNT_OFFST + 14.
*   ... noch in Zeile
*   IF CNT_COUNT LE SY-LINSZ  AND
*   und maximale Anzahl Wertfelder/Zeile nicht ueberschritten?
*      CNT_COUNT LE CON_ENPOS .
*     Dann dieses Wertfeld in diese Zeile.
*     POSITION CNT_OFFST.
*     WRITE SWRT-BETRG.
*   ... nicht mehr in dieser Zeile unterbringbar?
*   ELSE.
*     Dann neue Zeile anfangen.
*     CNT_OFFST = CON_SUPOS + 1.
*     Wertfeld in naechste Zeile.
*     NEW-LINE.
*     POSITION CNT_OFFST.
*     WRITE SWRT-BETRG.
*   ENDIF.
*   Offset naechstes Feld.
*   CNT_OFFST = CNT_OFFST + 16.
* ENDLOOP.
*
*NDFORM.

*---------------------------------------------------------------------*
* Summenbericht: Spaltenueberschriften ausgeben.                      *
*---------------------------------------------------------------------*
FORM S_TOP_AUSGEBEN.

  FORMAT RESET.

* Kein Summenbericht ==> nix machen.
  IF SUMMB EQ SPACE.
    EXIT.
  ENDIF.

* Anzahl der zu reservierenden Zeilen ermitteln: Mit 0 anfangen
* und bei jeder neuen Zeile um 1 hochzaehlen.
  CON_RESRV = 0.

* Unterstrich unter Seitenkopf.
  CLEAR HLP_CHAR.
  WRITE SY-ULINE TO HLP_CHAR+0(CON_ENPOS).
  NEW-LINE.
  WRITE HLP_CHAR.

* Zeile mit ...
  NEW-LINE. CON_RESRV = CON_RESRV + 1.
  WRITE SY-VLINE NO-GAP.
* ... Schluesselfeldern zu Sortierstufenfeldern ...
  CNT_OFFST = 2.
  LOOP AT FELD.
    POSITION CNT_OFFST.
    WRITE FELD-SPALT COLOR COL_HEADING INTENSIFIED ON.
    CNT_OFFST = CNT_OFFST + FELD-LAENG.
    POSITION CNT_OFFST.
    WRITE (1) SPACE.
    CNT_OFFST = CNT_OFFST + 1.
  ENDLOOP.
* ... und Platz fuer Sortierstufentext.
  POSITION CON_TXPOS.
  WRITE SPACE NO-GAP.
  CNT_OFFST = CON_TXPOS + 1.
  POSITION CNT_OFFST.
  WRITE (132) TXT_TXT50 COLOR COL_HEADING INTENSIFIED OFF.
  CNT_OFFST = CON_TXPOS + CON_LGTXT + 1.
  POSITION CNT_OFFST.
  WRITE (132) SPACE.
  POSITION CNT_OFFST.
  WRITE SY-VLINE NO-GAP.
* Kein Platz mehr fuer wenigstens ein Wertfeld -->
* Neue Zeile, durch einen Unterstrich abgetrennt.
  IF CON_SUPOS EQ 1.
    CLEAR HLP_CHAR.
    WRITE SY-ULINE TO HLP_CHAR+0(CON_ENPOS).
    NEW-LINE. CON_RESRV = CON_RESRV + 1.
    WRITE HLP_CHAR.
    NEW-LINE. CON_RESRV = CON_RESRV + 1.
    WRITE SY-VLINE NO-GAP.
  ENDIF.
* Ueberschriften der Wertfelder in Spaltenueberschriften einarbeiten.
  CNT_OFFST = CON_SUPOS + 1.
  CNT_COUNT = 0.
  LOOP AT SFLD.
*   Maximale Anzahl Wertfelder/Zeile nicht ueberschritten?
    CNT_COUNT = CNT_COUNT + 1.
    IF CNT_COUNT LE CON_WRTZL.
*      Dann dieses Wertfeld in diese Zeile.
      POSITION CNT_OFFST.
      WRITE SFLD-FNAME(14) COLOR COL_HEADING INTENSIFIED ON.
      CNT_OFFST = CNT_OFFST + 14.
      POSITION CNT_OFFST.
      WRITE (2) SPACE.
      CNT_OFFST = CNT_OFFST + 2.
*   Maximale Anzahl Wertfelder/Zeile ueberschritten?
    ELSE.
*      Dann neue Zeile anfangen.
      CNT_OFFST = CON_ENPOS.
      POSITION CNT_OFFST.
      WRITE SY-VLINE NO-GAP.
      NEW-LINE. CON_RESRV = CON_RESRV + 1.
      WRITE SY-VLINE NO-GAP.
      IF CON_SUPOS NE 1.
        CNT_OFFST = CON_TXPOS + CON_LGTXT + 1.
        POSITION CNT_OFFST.
        WRITE SY-VLINE NO-GAP.
      ENDIF.
      CNT_OFFST = CON_SUPOS + 1.
      POSITION CNT_OFFST.
      WRITE SFLD-FNAME COLOR COL_HEADING INTENSIFIED ON.
      CNT_OFFST = CNT_OFFST + 14.
      POSITION CNT_OFFST.
      WRITE (2) SPACE.
      CNT_OFFST = CNT_OFFST + 2.
      CNT_COUNT = 1.
    ENDIF.
  ENDLOOP.

* Alles mit VLINE und neuem Unterstrich abschliessen.
  POSITION CON_ENPOS.
  WRITE SY-VLINE NO-GAP.
*
  CLEAR HLP_CHAR.
  WRITE SY-ULINE TO HLP_CHAR+0(CON_ENPOS).
  NEW-LINE. CON_RESRV = CON_RESRV + 1.
  WRITE HLP_CHAR.

  FORMAT RESET.

ENDFORM.


*---------------------------------------------------------------------*
* Fehlerhafte Anlagen vor Ausgabe des Reports anlisten.               *
*---------------------------------------------------------------------*
FORM FEHLER_AUSGEBEN.

  TABLES: T100.

  DATA: HLP_MSGOR(4) TYPE C.

  FORMAT RESET.

* READ TABLE YANFM INDEX 1.
* IF SY-SUBRC NE 0. EXIT. ENDIF.

* Fehlertabelle lesen (wird von logischer Datenbank bereitgestellt).
  READ TABLE YANFM INDEX 1.
* Ueberhaupt Eintraege vorhanden ...
  IF SY-SUBRC EQ 0.
*   ... dann merke: fehlerhafte Anlagen vorhanden.
    FLG_XFEHL = ON.
*   Fehlerhafte Anlage.
    WRITE: /001     SY-VLINE NO-GAP,
            002(12) ANLAV-ANLN1 NO-GAP
                    COLOR COL_KEY INTENSIFIED ON,
            015(04) ANLAV-ANLN2 NO-GAP
                    COLOR COL_KEY INTENSIFIED ON,
            019     SY-VLINE NO-GAP,
            020(50) ANLAV-TXT50 NO-GAP
                    COLOR COL_KEY INTENSIFIED OFF,
            070(23) ANLAV-TXA50 NO-GAP
                    COLOR COL_KEY INTENSIFIED OFF,
            093     SY-VLINE NO-GAP,
           /001(93) SY-ULINE.
*   Informationen fuer PickUp.
    FLG_PICK_UP = 'X'.
    RANGE = '1'.
    HIDE: RANGE,
          FLG_PICK_UP,
          ANLAV-BUKRS,
          ANLAV-ANLN1,
          ANLAV-ANLN2.
    CLEAR FLG_PICK_UP.
*   Auflistung der einzelnen Fehler.
    LOOP AT YANFM.
*
      SELECT SINGLE * FROM T100
        WHERE SPRSL EQ SY-LANGU
          AND ARBGB EQ 'AA'
          AND MSGNR EQ YANFM-NR.
*
      IF SY-SUBRC EQ 0.
*
        IF YANFM-V1 GT SPACE.
          REPLACE '$' WITH YANFM-V1 INTO T100-TEXT.
          CONDENSE T100-TEXT.
        ENDIF.
        IF YANFM-V2 GT SPACE.
          REPLACE '$' WITH YANFM-V2 INTO T100-TEXT.
          CONDENSE T100-TEXT.
        ENDIF.
        IF YANFM-V3 GT SPACE.
          REPLACE '$' WITH YANFM-V3 INTO T100-TEXT.
          CONDENSE T100-TEXT.
        ENDIF.
        IF YANFM-V4 GT SPACE.
          REPLACE '$' WITH YANFM-V4 INTO T100-TEXT.
          CONDENSE T100-TEXT.
        ENDIF.
*
        MOVE: YANFM-KZ TO HLP_MSGOR(1),
              YANFM-NR TO HLP_MSGOR+1(3).
*       Fehler rausschreiben.
        DETAIL.
        WRITE: /001     SY-VLINE,
                015(04) HLP_MSGOR
                        COLOR COL_NORMAL INTENSIFIED ON,
                019     SY-VLINE,
                020(73) T100-TEXT
                        COLOR COL_NORMAL INTENSIFIED OFF,
                093     SY-VLINE.
*       Konvertierung zur Vermeidung von Typkonflikten in FB.
        MOVE  '0'           TO RANGE.
        MOVE: T100-TEXT     TO MSGH-MELDUNG,
              YANFM-NR      TO MSGH-MELD_NR,
              'AA'          TO MSGH-MELD_ID.
        WRITE TEXT-F10      TO MSGH-TITEL.
        MOVE: YANFM-V1      TO MSGH-MSGV1,
              YANFM-V2      TO MSGH-MSGV2,
              YANFM-V3      TO MSGH-MSGV3,
              YANFM-V4      TO MSGH-MSGV4.
*       Informationen fuer PickUp auf Fehlermeldung.
        FLG_PICK_UP = 'X'.
        HIDE: RANGE,
              FLG_PICK_UP,
              MSGH-MELDUNG,
              MSGH-MELD_ID,
              MSGH-MELD_NR,
              MSGH-TITEL,
              MSGH-MSGV1,
              MSGH-MSGV2,
              MSGH-MSGV3,
              MSGH-MSGV4.
        CLEAR FLG_PICK_UP.
        WRITE  /001(93) SY-ULINE.
      ENDIF.
    ENDLOOP.
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM HITSUMME_BILDEN                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM HITSUMME_BILDEN.

* Hitliste?
  IF PA_HITLI IS INITIAL.
*   Nein, dann nix machen.
    EXIT.
  ENDIF.

* Zaehler hochzaehlen.
  CNT_HITLI = CNT_HITLI + 1.
* Hit-Datensatz?
  IF CNT_HITLI LE PA_HITLI.
*   Ja, dann in Hit-Summe addieren.
    HITSUM-BETRG1  = HITSUM-BETRG1  + <H1>.
    HITSUM-BETRG2  = HITSUM-BETRG2  + <H2>.
    HITSUM-BETRG3  = HITSUM-BETRG3  + <H3>.
    HITSUM-BETRG4  = HITSUM-BETRG4  + <H4>.
    HITSUM-BETRG5  = HITSUM-BETRG5  + <H5>.
    HITSUM-BETRG6  = HITSUM-BETRG6  + <H6>.
    HITSUM-BETRG7  = HITSUM-BETRG7  + <H7>.
    HITSUM-BETRG8  = HITSUM-BETRG8  + <H8>.
    HITSUM-BETRG9  = HITSUM-BETRG9  + <H9>.
    HITSUM-BETRG10 = HITSUM-BETRG10 + <H10>.
    HITSUM-BETRG11 = HITSUM-BETRG11 + <H11>.
    HITSUM-BETRG12 = HITSUM-BETRG12 + <H12>.
    HITSUM-BETRG13 = HITSUM-BETRG13 + <H13>.
    HITSUM-BETRG14 = HITSUM-BETRG14 + <H14>.
    HITSUM-BETRG15 = HITSUM-BETRG15 + <H15>.
    HITSUM-BETRG16 = HITSUM-BETRG16 + <H16>.
    HITSUM-BETRG17 = HITSUM-BETRG17 + <H17>.
    HITSUM-BETRG18 = HITSUM-BETRG18 + <H18>.
    HITSUM-BETRG19 = HITSUM-BETRG19 + <H19>.
    HITSUM-BETRG20 = HITSUM-BETRG20 + <H20>.
  ENDIF.

ENDFORM.


* Wenn keine Sätze selektiert wurden, Meldung ausgeben
FORM NO_RECORDS.

  IF RAREP-XNORCD = 'X'.
    MESSAGE S020(AB).
  ENDIF.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM FLG_EINZEL_SETZEN                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM FLG_EINZEL_SETZEN.

  DATA: L_CNT  LIKE SY-TFILL,
        L_CNT1 LIKE SY-TFILL,
        L_CNT2 LIKE SY-TFILL.

* Default-Annahme: Keine Einzelsatzverarbeitung.
  FLG_EINZEL = OFF.

* Mehrere Buchungskreise ==> Keine Einzelsatzverarbeitung.
  DESCRIBE TABLE BUKRS LINES L_CNT.
  CHECK L_CNT EQ 1.
  READ TABLE BUKRS INDEX 1.
  CHECK BUKRS-SIGN  EQ 'I'                       AND
        ( BUKRS-OPTION  EQ 'EQ'              OR
          ( BUKRS-OPTION  EQ 'BT'        AND
            BUKRS-LOW     EQ BUKRS-HIGH  )   )   .

* Verarbeitung von InvProgrammen oder Projekten ==>
* Keine Einzelsatzverarbeitung.
  CHECK PA_XINVP IS INITIAL AND
        PA_XPROJ IS INITIAL .

* Verarbeitung von Anlagen  u n d  Aufträgen ==>
* Keine Einzelsatzverarbeitung.
  CHECK   PA_XANLG IS INITIAL OR
          PA_XAUFT IS INITIAL .

* Verarbeitung von Anlagen.
  IF NOT PA_XANLG IS INITIAL.
    DESCRIBE TABLE ANLAGE   LINES L_CNT.
    CHECK L_CNT EQ 1.
    DESCRIBE TABLE UNTNR    LINES L_CNT.
    CHECK L_CNT EQ 1.
    READ TABLE ANLAGE INDEX 1.
    READ TABLE UNTNR  INDEX 1.
    CHECK ANLAGE-SIGN EQ 'I'                       AND
          ( ANLAGE-OPTION EQ 'EQ'              OR
            ( ANLAGE-OPTION EQ 'BT'        AND
              ANLAGE-LOW    EQ ANLAGE-HIGH )   )   .
    CHECK UNTNR-SIGN EQ 'I'                      AND
          ( UNTNR-OPTION EQ 'EQ'             OR
            ( UNTNR-OPTION EQ 'BT'       AND
              UNTNR-LOW    EQ UNTNR-HIGH )   )   .
    FLG_EINZEL = ON.
  ENDIF.

* Verarbeitung von Aufträgen.
  IF NOT PA_XAUFT IS INITIAL.
*   Genau eine Abgrenzung Auftraege, sonst sicher keine
*   Einzelsatzverarbeitung.
    DESCRIBE TABLE SO_EAUFN LINES L_CNT1.
    CHECK L_CNT1 EQ 1.
    IF L_CNT1 EQ 1.
      READ TABLE SO_EAUFN INDEX 1.
      CHECK SO_EAUFN-SIGN EQ 'I'                         AND
            ( SO_EAUFN-OPTION EQ 'EQ'                OR
              ( SO_EAUFN-OPTION EQ 'BT'          AND
                SO_EAUFN-LOW    EQ SO_EAUFN-HIGH )   )   .
      FLG_EINZEL = ON.
    ENDIF.
  ENDIF.

ENDFORM.


*---------------------------------------------------------------------*
* Ermitteln der Bezeichnung zu einer Sortierstufe.                    *
*---------------------------------------------------------------------*
* --> FBE_FIELDNAME  Technischer Feldname aus ANLAV.                  *
* --> FBE_CONTENT    Auspraegung des Feldes.                          *
* <-- FBE_BEZ.       Zugehoerige Bezeichnung.                         *
*---------------------------------------------------------------------*
FORM HLP_FBEZ_ERMITTELN USING FBE_FIELDNAME FBE_CONTENT FBE_BEZ.

  DATA: L_KOKRS     LIKE TKA01-KOKRS,
        L_KTEXT     LIKE CSKT-KTEXT,
        L_AFABE     LIKE ANLB-AFABE,
        L_FIELDNAME LIKE ANLA-TXT50,
        L_CONTENT   LIKE ANLA-TXT50,
        L_BEZ       LIKE ANLA-TXT50.

  DATA: BEGIN OF L_011Q OCCURS 200.
          INCLUDE STRUCTURE RF011Q.
  DATA: END OF L_011Q.

  CLEAR FBE_BEZ.
*
  CASE FBE_FIELDNAME.
* Buchungskreis.
    WHEN 'BUKRS'.
      SELECT SINGLE * FROM T001
        WHERE BUKRS EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE T001-BUTXT TO FBE_BEZ.
      ENDIF.
* Geschaeftsbereich.
    WHEN 'GSBER'.
      SELECT SINGLE * FROM TGSBT
        WHERE SPRAS EQ SY-LANGU
        AND   GSBER EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE TGSBT-GTEXT TO FBE_BEZ.
      ENDIF.
* Ergebnisschluessel Soll/Haben.
    WHEN 'ERGSO'.
      SELECT SINGLE * FROM T093B
         WHERE BUKRS EQ ANLAV-BUKRS
         AND   AFABE EQ BEREICH1.
      IF SY-SUBRC EQ 0.
        CALL FUNCTION 'FI_IMPORT_BALANCE_SHEET_TEXT'
             EXPORTING
                  SPRACHE        = SY-LANGU
                  VERSION        = T093B-VERSN_011
             TABLES
                  X011Q          = L_011Q
             EXCEPTIONS
                  TEXT_NOT_FOUND = 1.
        IF SY-SUBRC EQ 0.
          LOOP AT L_011Q
            WHERE ERGSL EQ FBE_CONTENT
            AND   TXTYP EQ 'K'.
            EXIT.
          ENDLOOP.
          MOVE L_011Q-TXT45 TO FBE_BEZ.
        ENDIF.
      ENDIF.
* Bestandskonto.
    WHEN 'KTANSW'.
      SELECT SINGLE * FROM T001
        WHERE BUKRS EQ ANLAV-BUKRS.
      SELECT SINGLE * FROM SKAT
        WHERE SPRAS EQ SY-LANGU
        AND   KTOPL EQ T001-KTOPL
        AND   SAKNR EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        IF CON_LGTXT LE 30      OR
           SUMMB     IS INITIAL .
          MOVE SKAT-TXT20 TO FBE_BEZ.
        ELSE.
          MOVE SKAT-TXT50 TO FBE_BEZ.
        ENDIF.
      ENDIF.
* Anlagenklasse.
    WHEN 'ANLKL'.
      SELECT SINGLE * FROM ANKT
        WHERE SPRAS EQ SY-LANGU
        AND   ANLKL EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        IF CON_LGTXT LE 30      OR
           SUMMB     IS INITIAL .
          MOVE ANKT-TXK20 TO FBE_BEZ.
        ELSE.
          MOVE ANKT-TXK50 TO FBE_BEZ.
        ENDIF.
      ENDIF.
* Werk.
    WHEN 'WERKS'.
      SELECT SINGLE * FROM T001W
        WHERE WERKS EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE T001W-NAME1 TO FBE_BEZ.
      ENDIF.
* Kostenstelle.
    WHEN 'KOSTL'.
      CALL FUNCTION 'RK_KOKRS_FIND'
           EXPORTING
                BUKRS  = ANLAV-BUKRS
                GSBER  = ANLAV-GSBER
           IMPORTING
                KOKRS  = L_KOKRS
           EXCEPTIONS
                OTHERS = 1.
      IF SY-SUBRC EQ 0.
        CALL FUNCTION 'RK_KOSTL_READ'
             EXPORTING
                  DATUM  = BERDATUM
                  KOKRS  = L_KOKRS
                  KOSTL  = FBE_CONTENT
                  SPRAS  = SY-LANGU
             IMPORTING
                  KTEXT  = L_KTEXT
             EXCEPTIONS
                  OTHERS = 1.
        IF SY-SUBRC EQ 0.
          MOVE L_KTEXT TO FBE_BEZ.
        ENDIF.
      ENDIF.
* Vermoegensgliederungsschluessel.
    WHEN 'VMGLI'.
      SELECT SINGLE * FROM T092T
        WHERE SPRAS EQ SY-LANGU
        AND   VMGLI EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE T092T-VMTXT TO FBE_BEZ.
      ENDIF.
* Versicherungsart.
    WHEN 'VSART'.
      SELECT SINGLE * FROM T099T
        WHERE SPRAS EQ SY-LANGU
        AND   VSART EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE T099T-VRSTX TO FBE_BEZ.
      ENDIF.
* Versicherungsgesellschaft.
    WHEN 'VSGES'.
      SELECT SINGLE * FROM T099U
        WHERE SPRAS EQ SY-LANGU
        AND   VSGES EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE T099U-VSGTX TO FBE_BEZ.
      ENDIF.
* Versicherungsindex.
    WHEN 'VSIND'.
      SELECT SINGLE * FROM T094T
        WHERE SPRAS EQ SY-LANGU
        AND   WBIND EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE T094T-INDTXT TO FBE_BEZ.
      ENDIF.
* Leasinggeber.
    WHEN 'LEAFI'.
      SELECT SINGLE * FROM LFA1
        WHERE LIFNR EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE LFA1-NAME1 TO FBE_BEZ.
      ENDIF.
* AfA-Schluessel.
    WHEN 'AFASL'.
      SELECT SINGLE * FROM T093C
        WHERE BUKRS EQ ANLAV-BUKRS.
      SELECT SINGLE * FROM T090T
        WHERE SPRAS EQ SY-LANGU
        AND   AFAPL EQ T093C-AFAPL
        AND   AFASL EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE T090T-AFATXT TO FBE_BEZ.
      ENDIF.
* Ordnungsbegriff 1
    WHEN 'ORD41'.
      SELECT SINGLE * FROM T087T
        WHERE SPRAS EQ SY-LANGU
        AND   ORDNR EQ '1'
        AND   ORD4X EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE T087T-ORDTX  TO FBE_BEZ.
      ENDIF.
* Ordnungsbegriff 2
    WHEN 'ORD42'.
      SELECT SINGLE * FROM T087T
        WHERE SPRAS EQ SY-LANGU
        AND   ORDNR EQ '2'
        AND   ORD4X EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE T087T-ORDTX  TO FBE_BEZ.
      ENDIF.
* Ordnungsbegriff 3
    WHEN 'ORD43'.
      SELECT SINGLE * FROM T087T
        WHERE SPRAS EQ SY-LANGU
        AND   ORDNR EQ '3'
        AND   ORD4X EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE T087T-ORDTX  TO FBE_BEZ.
      ENDIF.
* Ordnungsbegriff 4
    WHEN 'ORD44'.
      SELECT SINGLE * FROM T087T
        WHERE SPRAS EQ SY-LANGU
        AND   ORDNR EQ '4'
        AND   ORD4X EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE T087T-ORDTX  TO FBE_BEZ.
      ENDIF.
* Ordnungsbegriff 5
    WHEN 'GDLGRP'.
      SELECT SINGLE * FROM T087S
        WHERE SPRAS EQ SY-LANGU
        AND  GDLGRP EQ FBE_CONTENT.
      IF SY-SUBRC EQ 0.
        MOVE T087S-GDLGRP_TXT TO FBE_BEZ.
      ENDIF.
    WHEN OTHERS.
      CLEAR: L_FIELDNAME, L_CONTENT, L_BEZ.
      MOVE: FBE_FIELDNAME TO L_FIELDNAME,
            FBE_CONTENT   TO L_CONTENT,
            BEREICH1      TO L_AFABE.
      CALL FUNCTION 'GRUPPENSTUFEN_TEXTE_LESEN'
           EXPORTING
                I_BUKRS     = ANLAV-BUKRS
                I_AFABE     = L_AFABE
                I_CONTENT   = L_CONTENT
                I_FIELDNAME = L_FIELDNAME
                I_OFFSET    = FELD-OFFSET
                I_LAENGE    = FELD-LAENGE
           IMPORTING
                E_BEZ       = L_BEZ.
      MOVE L_BEZ TO FBE_BEZ.
  ENDCASE.


ENDFORM.

*---------------------------------------------------------------------*
*       FORM XXL_DATEN                                                *
*---------------------------------------------------------------------*
*       der Anschluß an EXCEL wird über den Funktionsbaustein         *
*       xxl_full_api realisiert. Hierzu müssen folgende interne       *
*       Tabellen übergeben werden:                                    *
*       xxl_d: Tabelle, die alle Datenfelder wie z.B. Wertfelder oder *
*              Textfelder und Währungsfelder enthält. Die Struktur    *
*              wurde frei definiert, ohne auf DDIC-Felder zu verweisen*
*              Aus Performancegründen wurden zwei Datentabellen       *
*              definiert: xxl_d mit 6 Wertfeldern und xxl_d2 mit 25   *
*              Wertfeldern.                                           *
*       xxl_v: Tabelle, welche die vertikalen Feldbezeichnungen       *
*              enthält z.B. BuKr, GsBe, BestKoAHK, Klasse, Bezeichnung*
*       xxl_h: Die Tabelle enthält die horizontalen Feldbezeichnungen.*
*              i.d.R. sind dies Wertfelder wie z.B. Anschw.           *
*              Da XXL_H-ROW grundsätzlich 1 ist, liegt immer eine     *
*              eindimensionale Hierarchie vor.                        *
*       xxl_s: xxl_s-col_no : Spaltennummer in der EXCEL-Tabelle      *
*                             (Zielspalte)                            *
*              xxl_s-col_src: Spaltennummer der Tabelle xxl_d, aus der*
*                             die Daten geholt werden(Herkunftsspalte)*
*              xxl_s-col_typ: gibt den Datentyp fuer EXCEL an         *
*                                                                     *
*                                                                     *
*---------------------------------------------------------------------*
FORM XXL_DATEN.


* Zaehler fuer swrt loop.
  DATA: XXL_ZAEHLER TYPE I.
* Zaehler fuer XXL_S-COL_SRC.
  DATA: XXL_SRC_ZAEHLER TYPE I.
* Hilfsfeld zur Umwandlung von type c in type p.
  DATA: XXL_BETRG(15) TYPE C.
* Zaehler der angezeigten Spalten in EXCEL (aus DATA selektiert)
  DATA: XXL_SPALTE TYPE I.
* Anzahl der Dezimalstellen der Währung.
  DATA: XXL_CURRDEC TYPE I.
* 'neue Zeile auf 0 setzen'
  DATA: XXL_SPALTE_7 TYPE I.
* gitterspalte für xxl_h-col_no.
  DATA: XXL_GITSP TYPE I.
* gitterzeile für xxl_h-row_no.
  DATA: XXL_GITZL TYPE I VALUE 1.
* Hilfsfeld für condense
  DATA XXL_HLP(30) TYPE C.
* Zaehler
  DATA XXL_ZAEHLER3 TYPE I.

  IF XXL_FLAG2 IS INITIAL.
    XXL_FLAG2 = 'X'.
* Einmaliges auswählen der Tabelle xxl_d oder xxl_d2
* wenn xxl_d : xxl_d_nr = 1
* wenn xxl_d2: xxl_d_nr = 2
    DESCRIBE TABLE SWRT LINES XXL_D_NR.
    IF XXL_D_NR LT 7.
      XXL_D_NR = 1.
    ELSE.
      XXL_D_NR = 2.
    ENDIF.
  ENDIF.

* --------------------------------------------------------------------*
* Der Tabelle xxl_d wird nach jeder ermittelten Gruppensumme eine     *
* Zeile angehängt.                                                    *
* --------------------------------------------------------------------*

*  zählen der Wertfelder der Tabelle xxl_d von 1 bis max. 25
  XXL_ZAEHLER = 0.
*  löschen der Kopfzeile, damit Währung nur einmal in EXCEL erscheint
  CLEAR XXL_D.
  CASE XXL_D_NR.
    WHEN 1.
* kleine Datentabelle xxl_d fuellen.
      MOVE SAV_WAER1 TO XXL_D-CURR.
      MOVE <S1> TO XXL_D-SORT1.
      MOVE <S2> TO XXL_D-SORT2.
      MOVE <S3> TO XXL_D-SORT3.
      MOVE <S4> TO XXL_D-SORT4.
      MOVE <S5> TO XXL_D-SORT5.
      IF NOT HLP_STUFE IS INITIAL.
        MOVE HLP_STUFE TO XXL_D-TEXT.
        TRANSLATE XXL_D-TEXT USING '; '.
      ELSE.
        MOVE HLP_FBEZ TO XXL_D-TEXT.
      ENDIF.

      LOOP AT SWRT.
        IF FLG_GITTER = '1' AND CON_GITSP NE 0
                            AND XXL_ZAEHLER = CON_GITSP.
          XXL_ZAEHLER = 1.
          APPEND XXL_D.
        ELSE.
          XXL_ZAEHLER = XXL_ZAEHLER + 1.
        ENDIF.
        MOVE SWRT-BETRG TO XXL_BETRG.
        TRANSLATE XXL_BETRG USING '. , '.
        CONDENSE XXL_BETRG NO-GAPS.
        IF  NOT XXL_BETRG(1) BETWEEN '0' AND '9'.
          CLEAR XXL_BETRG.
        ENDIF.
        CASE XXL_ZAEHLER.
          WHEN 1.
            MOVE XXL_BETRG TO XXL_D-WERT1.
          WHEN 2.
            MOVE XXL_BETRG TO XXL_D-WERT2.
          WHEN 3.
            MOVE XXL_BETRG TO XXL_D-WERT3.
          WHEN 4.
            MOVE XXL_BETRG TO XXL_D-WERT4.
          WHEN 5.
            MOVE XXL_BETRG TO XXL_D-WERT5.
          WHEN 6.
            MOVE XXL_BETRG TO XXL_D-WERT6.
        ENDCASE.
      ENDLOOP.
      APPEND XXL_D.
    WHEN 2.
* grosse Datentabelle xxl_d2 fuellen.
      MOVE SAV_WAER1 TO XXL_D2-CURR.
      MOVE <S1> TO XXL_D2-SORT1.
      MOVE <S2> TO XXL_D2-SORT2.
      MOVE <S3> TO XXL_D2-SORT3.
      MOVE <S4> TO XXL_D2-SORT4.
      MOVE <S5> TO XXL_D2-SORT5.
      IF NOT HLP_STUFE IS INITIAL.
        MOVE HLP_STUFE TO XXL_D2-TEXT.
      ELSE.
        MOVE HLP_FBEZ TO XXL_D2-TEXT.
      ENDIF.


      LOOP AT SWRT.
        IF FLG_GITTER = '1' AND CON_GITSP NE 0
                            AND XXL_ZAEHLER = CON_GITSP.
          XXL_ZAEHLER = 1.
          APPEND XXL_D2.
        ELSE.
          XXL_ZAEHLER = XXL_ZAEHLER + 1.
        ENDIF.
        MOVE SWRT-BETRG TO XXL_BETRG.
        TRANSLATE XXL_BETRG USING '. , '.
        CONDENSE XXL_BETRG NO-GAPS.
        IF  NOT XXL_BETRG(1) BETWEEN '0' AND '9'.
          CLEAR XXL_BETRG.
        ENDIF.
        CASE XXL_ZAEHLER.
          WHEN 1.
            MOVE XXL_BETRG TO XXL_D2-WERT1.
          WHEN 2.
            MOVE XXL_BETRG TO XXL_D2-WERT2.
          WHEN 3.
            MOVE XXL_BETRG TO XXL_D2-WERT3.
          WHEN 4.
            MOVE XXL_BETRG TO XXL_D2-WERT4.
          WHEN 5.
            MOVE XXL_BETRG TO XXL_D2-WERT5.
          WHEN 6.
            MOVE XXL_BETRG TO XXL_D2-WERT6.
          WHEN 7.
            MOVE XXL_BETRG TO XXL_D2-WERT7.
          WHEN 8.
            MOVE XXL_BETRG TO XXL_D2-WERT8.
          WHEN 9.
            MOVE XXL_BETRG TO XXL_D2-WERT9.
          WHEN 10.
            MOVE XXL_BETRG TO XXL_D2-WERT10.
          WHEN 11.
            MOVE XXL_BETRG TO XXL_D2-WERT11.
          WHEN 12.
            MOVE XXL_BETRG TO XXL_D2-WERT12.
          WHEN 13.
            MOVE XXL_BETRG TO XXL_D2-WERT13.
          WHEN 14.
            MOVE XXL_BETRG TO XXL_D2-WERT14.
          WHEN 15.
            MOVE XXL_BETRG TO XXL_D2-WERT15.
          WHEN 16.
            MOVE XXL_BETRG TO XXL_D2-WERT16.
          WHEN 17.
            MOVE XXL_BETRG TO XXL_D2-WERT17.
          WHEN 18.
            MOVE XXL_BETRG TO XXL_D2-WERT18.
          WHEN 19.
            MOVE XXL_BETRG TO XXL_D2-WERT19.
          WHEN 20.
            MOVE XXL_BETRG TO XXL_D2-WERT20.
          WHEN 21.
            MOVE XXL_BETRG TO XXL_D2-WERT21.
          WHEN 22.
            MOVE XXL_BETRG TO XXL_D2-WERT22.
          WHEN 23.
            MOVE XXL_BETRG TO XXL_D2-WERT23.
          WHEN 24.
            MOVE XXL_BETRG TO XXL_D2-WERT24.
          WHEN 25.
            MOVE XXL_BETRG TO XXL_D2-WERT25.
        ENDCASE.
      ENDLOOP.

      APPEND XXL_D2.
  ENDCASE.

* Tabellen xxl_v, xxl_h, xxl_s werden in einem Durchlauf komplett
* gefuellt.
  IF XXL_FLAG IS INITIAL.
* xxl_spalte zählt die Spalten für xxl_s-col_no - also der Zielspalte
* in der EXCEL-Tabelle - von 1 bis max. 32 in Einerschritten hoch.
    XXL_FLAG = 'X'.
    XXL_SPALTE  = 0.

* --------------------------------------------------------------------*
* Tabelle XXL_V fuellen.                                              *
* --------------------------------------------------------------------*

*  zählen der Spaltenzahl für xxl_v-col_no von 1 bis 6
    XXL_ZAEHLER = 0.
*  feststellen der Herkunftssplate in xx_d fuer xxl_s-col_src
    XXL_SRC_ZAEHLER = 1.
    LOOP AT FELD.
      ADD 1 TO XXL_ZAEHLER.
      MOVE FELD-SPALT TO XXL_V-COL_NAME.
      MOVE XXL_ZAEHLER TO XXL_V-COL_NO.
      APPEND XXL_V.
* Tabelle xxl_s fuellen.
      ADD 1 TO XXL_SPALTE.
      MOVE XXL_SPALTE TO XXL_S-COL_NO.
      ADD 1 TO XXL_SRC_ZAEHLER.
      MOVE XXL_SRC_ZAEHLER TO XXL_S-COL_SRC.
      MOVE 'STR'  TO XXL_S-COL_TYP.
      MOVE 'DFT'  TO XXL_S-COL_OPS.
      APPEND XXL_S.
    ENDLOOP.
    ADD 1 TO XXL_ZAEHLER.
    MOVE XXL_ZAEHLER TO XXL_V-COL_NO.
    MOVE TXT_TXT50   TO XXL_V-COL_NAME.
    APPEND XXL_V.
* Tabelle xxl_s fuellen.
    ADD 1 TO XXL_SPALTE.
    MOVE XXL_SPALTE      TO XXL_S-COL_NO.
    XXL_SRC_ZAEHLER = 7.
    MOVE XXL_SRC_ZAEHLER TO XXL_S-COL_SRC.
    MOVE 'STR'           TO XXL_S-COL_TYP.
    MOVE 'DFT'           TO XXL_S-COL_OPS.
    APPEND XXL_S.
* Anzahl (hierarchischer) Schluesselspalten.
    XXL_N_VRT_KEYS = XXL_SPALTE.

* --------------------------------------------------------------------*
* Tabelle XXL_H fuellen.                                              *
* --------------------------------------------------------------------*

*  zählen der Spaltenzahl für xxl_h-col_no von 1 bis max. 25
    XXL_ZAEHLER = 1.
    XXL_GITSP = 1.
* Währungsspalte
    MOVE XXL_ZAEHLER TO XXL_H-COL_NO.
    MOVE 1           TO XXL_H-ROW_NO.
    MOVE ' '         TO XXL_H-COL_NAME.
    APPEND XXL_H.

    IF FLG_GITTER = '1'.
      DO CON_GITZL TIMES.
        ADD 1 TO XXL_ZAEHLER3.
        IF XXL_ZAEHLER3 > 1.
          MOVE 1            TO XXL_H-COL_NO.
          MOVE XXL_ZAEHLER3 TO XXL_H-ROW_NO.
          MOVE ' '          TO XXL_H-COL_NAME.
          APPEND XXL_H.
        ENDIF.
      ENDDO.
    ENDIF.
* Übergabe der Währungsspalte durch xxl_s-col_src = 1
* In der 1. Spalte von xxl_d steht die Währung.
    ADD 1            TO XXL_SPALTE.
    MOVE XXL_SPALTE  TO XXL_S-COL_NO.
    MOVE 1           TO XXL_S-COL_SRC.
    MOVE 'STR'       TO XXL_S-COL_TYP.
    MOVE 'DFT'       TO XXL_S-COL_OPS.
    APPEND XXL_S.
    LOOP AT SFLD.
      ADD 1 TO XXL_ZAEHLER.
      ADD 1 TO XXL_SRC_ZAEHLER.
      ADD 1 TO XXL_SPALTE.
      MOVE SFLD-FNAME  TO XXL_H-COL_NAME.
      IF FLG_GITTER = '1'.
        IF XXL_GITSP GT CON_GITSP.
          XXL_GITSP = 1.
          ADD 1 TO XXL_GITZL.
        ENDIF.
        ADD 1 TO XXL_GITSP.
        MOVE XXL_GITSP TO XXL_H-COL_NO.
        MOVE XXL_GITZL TO XXL_H-ROW_NO.
      ELSE.
        MOVE XXL_ZAEHLER TO XXL_H-COL_NO.
        MOVE 1           TO XXL_H-ROW_NO.
      ENDIF.
      APPEND XXL_H.
* Tabelle XXL_S fuellen.
      MOVE XXL_SPALTE      TO XXL_S-COL_NO.
      MOVE XXL_SRC_ZAEHLER TO XXL_S-COL_SRC.
      IF SY-REPID NE 'RAUSMQ10'.
        MOVE 1               TO XXL_S-COL_CUR.
      ELSE.
        IF XXL_ZAEHLER EQ 3 OR XXL_ZAEHLER EQ 5.
          MOVE 0               TO XXL_S-COL_CUR.
        ELSE.
          MOVE 1               TO XXL_S-COL_CUR.
        ENDIF.
      ENDIF.
* --------------------------------------------------------------------*
* Decimals der Waehrung feststellen                                   *
* --------------------------------------------------------------------*
      SELECT SINGLE * FROM  TCURX
             WHERE  CURRKEY     = SAV_WAER1.

      IF SY-SUBRC NE 0.
        MOVE 2 TO XXL_CURRDEC.
      ELSE.
        MOVE TCURX-CURRDEC TO XXL_CURRDEC.
      ENDIF.

      CASE XXL_CURRDEC.
        WHEN 0.
          MOVE 'N00'           TO XXL_S-COL_TYP.
        WHEN 1.
          MOVE 'N01'           TO XXL_S-COL_TYP.
        WHEN 3.
          MOVE 'N03'           TO XXL_S-COL_TYP.
        WHEN OTHERS.
          MOVE 'N02'           TO XXL_S-COL_TYP.
      ENDCASE.
      MOVE 'ADD'           TO XXL_S-COL_OPS.
      IF SY-REPID = 'RAUSMQ10'.
        IF XXL_ZAEHLER EQ 3 OR XXL_ZAEHLER EQ 5.
          MOVE 'N01'           TO XXL_S-COL_TYP.
        ENDIF.
      ENDIF.
      XXL_SPALTE_7 = XXL_SPALTE - XXL_N_VRT_KEYS - 1.
      IF FLG_GITTER = '1'.
        IF XXL_SPALTE_7 LE CON_GITSP.
          APPEND XXL_S.
        ENDIF.
      ELSE.
        APPEND XXL_S.
      ENDIF.
    ENDLOOP.
    IF FLG_GITTER = '1'.
      XXL_N_ATT_COLS = CON_GITSP + 1.
      XXL_N_HRZ_KEYS = CON_GITZL.
    ELSE.
      XXL_N_ATT_COLS = XXL_SPALTE - XXL_N_VRT_KEYS.
    ENDIF.
* --------------------------------------------------------------------*
* Header1 fuellen                                                     *
* --------------------------------------------------------------------*
    WRITE TEXT-U01            TO XXL_HEADER1+001(18).
    WRITE BERDATUM DD/MM/YYYY TO XXL_HEADER1+020(10).
    WRITE HEAD                TO XXL_HEADER1+033.
* --------------------------------------------------------------------*
* Header2 fuellen
* --------------------------------------------------------------------*
    WRITE TEXT-U02            TO XXL_HEADER2+001(18).
    WRITE SY-DATUM DD/MM/YYYY TO XXL_HEADER2+020(10).
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM XXL_AUFRUFEN                                             *
*---------------------------------------------------------------------*
*       xxl_d  (6 Wertfelder)  ----> call funktion mit xxl_d          *
*       xxl_d2 (25 Wertfelder) ----> call funktion mit xxl_d2         *
*---------------------------------------------------------------------*
FORM XXL_AUFRUFEN.
  CASE XXL_D_NR.
    WHEN 1.
      PERFORM CALL_XXL TABLES XXL_D.
* kleine Datentabelle übergeben.
    WHEN 2.
* große Datentabelle übergeben.
      PERFORM CALL_XXL TABLES XXL_D2.
  ENDCASE.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM CALL_XXL                                                 *
*---------------------------------------------------------------------*
*       call function 'xxl_full_api' mit Fehlerbehandlung             *
*---------------------------------------------------------------------*
*  -->  XXL_D                                                         *
*---------------------------------------------------------------------*
FORM CALL_XXL TABLES XXL_D.

  CALL FUNCTION 'XXL_FULL_API'
       EXPORTING
            DATA_ENDING_AT    = -1
            DATA_STARTING_AT  = 1
            FILENAME          = 'XXL_1ST '
            HEADER_1          = XXL_HEADER1
            HEADER_2          = XXL_HEADER2
            NO_DIALOG         = ' '
            NO_START          = ' '
            N_ATT_COLS        = XXL_N_ATT_COLS
            N_HRZ_KEYS        = XXL_N_HRZ_KEYS
            N_VRT_KEYS        = XXL_N_VRT_KEYS
            SEMA_TYPE         = ' '
            SO_TITLE          = ' '
       TABLES
            DATA              = XXL_D
            HKEY              = XXL_H
            ONLINE_TEXT       = XXL_O
            PRINT_TEXT        = XXL_P
            SEMA              = XXL_S
            VKEY              = XXL_V
       EXCEPTIONS
            CANCELLED_BY_USER = 01
            DATA_TOO_BIG      = 02
            DIM_MISMATCH_DATA = 03
            DIM_MISMATCH_SEMA = 04
            DIM_MISMATCH_VKEY = 05
            ERROR_IN_HKEY     = 06
            ERROR_IN_SEMA     = 07
            FILE_OPEN_ERROR   = 08
            FILE_WRITE_ERROR  = 09
            INV_DATA_RANGE    = 10
            INV_WINSYS        = 11
            INV_XXL           = 12.

  CASE SY-SUBRC.
    WHEN 01.
      MESSAGE I403.
    WHEN 02.
      MESSAGE I404.
    WHEN 08.
      MESSAGE I405.
    WHEN 09.
      MESSAGE I406.
    WHEN 11.
      MESSAGE I400.
    WHEN 12.
      MESSAGE I401.
    WHEN 03.
      MESSAGE I402 WITH SY-SUBRC.
    WHEN 04.
      MESSAGE I402 WITH SY-SUBRC.
    WHEN 05.
      MESSAGE I402 WITH SY-SUBRC.
    WHEN 06.
      MESSAGE I402 WITH SY-SUBRC.
    WHEN 07.
      MESSAGE I402 WITH SY-SUBRC.
    WHEN 10.
      MESSAGE I402 WITH SY-SUBRC.
  ENDCASE.
ENDFORM.


* Form Routinen zur Realisierung des PickUp
* Routine, die den Report nach einem PickUp erneut, aber mittels
* Einzelauflistung aufruft.
FORM SUBMIT_ANLAGENREPORT
                      USING VALUE(U_REPORT)      " Aufzurufender Report
                      VALUE(FLG_CALL_SUM).

* Informationen zum Empfängerbericht.
  DATA: BEGIN OF L_REC.
          INCLUDE STRUCTURE RSTIREC.
  DATA: END OF L_REC.

* Tabelle mit den Selektionen.
  DATA: BEGIN OF XSOPT OCCURS 0.
          INCLUDE STRUCTURE RSPARAMS.
  DATA: END   OF XSOPT.

* Dynamische Abgrenzungen werden beim PickUp noch nicht unterstützt
  PERFORM DYN_SEL_CHECK.
* Sortierwerte müssen für den Aufruf gesichert werden.
  PERFORM SORTIERWERTE_SICHERN.
* Der Report wird nicht zum ersten Mal aufgerufen.
  FLG_NOT_FIRST = 1.
  EXPORT FLG_NOT_FIRST TO MEMORY ID 'flg'.

  IF NOT U_REPORT IS INITIAL.
*   Empfängerbercht bestimmen.
    L_REC-RTOOL = TOOL.
    L_REC-RAPPL = SPACE.
    L_REC-RONAM = U_REPORT.
  ELSE.
    CLEAR L_REC.
  ENDIF.

* Aufruf des Empfängerberichts über die BBS-Schnittstelle.
  PERFORM SEND_SEL TABLES XSOPT
                   USING  L_REC
                          FLG_CALL_SUM
                          'I'.

* Rückkehr zum ursprünglichen Report.
  FLG_NOT_FIRST = 0.
  EXPORT FLG_NOT_FIRST TO MEMORY ID 'flg'.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM SUBMIT_FREMDREPORT                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM SUBMIT_FREMDREPORT.

  DATA: FLG_CALL_SUM.
* Informationen zum Empfängerbericht.
  DATA: BEGIN OF L_REC.
          INCLUDE STRUCTURE RSTIREC.
  DATA: END OF L_REC.

* Tabelle mit den Selektionen.
  DATA: BEGIN OF XSOPT OCCURS 0.
          INCLUDE STRUCTURE RSPARAMS.
  DATA: END   OF XSOPT.

* Dynamische Abgrenzungen werden beim PickUp noch nicht unterstützt
  PERFORM DYN_SEL_CHECK.


* Aufruf des Empfängerberichts über die BBS-Schnittstelle.
  PERFORM SEND_SEL TABLES XSOPT
                   USING  L_REC
                          FLG_CALL_SUM
                          'E'.

ENDFORM.


* Beim ersten Aufruf Selektionsoptionen sichern.
* Beim Aufruf aus einem PickUp werden die im Memory gespeicherten
* Selektionsoptionen wieder eingelesen.
FORM INFO_PICK_UP.
  IF FLG_NOT_FIRST <> 0.
    PERFORM SORTIERWERTE_EINLESEN.
  ENDIF.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM SORTIERWERTE_SICHERN                                     *
*---------------------------------------------------------------------*
*       Sichern der Sortierwerte aus der Zeile im Memory              *
*---------------------------------------------------------------------*
FORM SORTIERWERTE_SICHERN.
  REFRESH: SO_SEL1, SO_SEL2, SO_SEL3, SO_SEL4, SO_SEL5.
  CLEAR: SO_SEL1, SO_SEL2, SO_SEL3, SO_SEL4, SO_SEL5.
* Sichern des HelpLevels d.h. Anzahl der Sortierwerte der Zeile
  EXPORT Z_FELD_IND TO MEMORY ID 'ind'.
* Ab hier Sicherung der Sortierwerte
  IF Z_FELD_IND >= 1.                  "Sortierstufe 1
    WRITE <S1> TO SO_SEL1-LOW.
    MOVE 'EQ' TO SO_SEL1-OPTION.
    MOVE 'I' TO SO_SEL1-SIGN.
    APPEND SO_SEL1.
    EXPORT SO_SEL1 TO MEMORY ID 'so_sel1'.
  ENDIF.
  IF Z_FELD_IND >= 2.                  "Sortierstufe 2
    WRITE <S2> TO SO_SEL2-LOW.
    MOVE 'EQ' TO SO_SEL2-OPTION.
    MOVE 'I' TO SO_SEL2-SIGN.
    APPEND SO_SEL2.
    EXPORT SO_SEL2 TO MEMORY ID 'so_sel2'.
  ENDIF.
  IF Z_FELD_IND >= 3.                  "Sortierstufe 3
    WRITE <S3> TO SO_SEL3-LOW.
    MOVE 'EQ' TO SO_SEL3-OPTION.
    MOVE 'I' TO SO_SEL3-SIGN.
    APPEND SO_SEL3.
    EXPORT SO_SEL3 TO MEMORY ID 'so_sel3'.
  ENDIF.
  IF Z_FELD_IND >= 4.                  "Sortierstufe 4
    WRITE <S4> TO SO_SEL4-LOW.
    MOVE 'EQ' TO SO_SEL4-OPTION.
    MOVE 'I' TO SO_SEL4-SIGN.
    APPEND SO_SEL4.
    EXPORT SO_SEL4 TO MEMORY ID 'so_sel4'.
  ENDIF.
  IF Z_FELD_IND >= 5.                  "Sortierstufe 5
    WRITE <S5> TO SO_SEL5-LOW.
    MOVE 'EQ' TO SO_SEL5-OPTION.
    MOVE 'I' TO SO_SEL5-SIGN.
    APPEND SO_SEL5.
    EXPORT SO_SEL5 TO MEMORY ID 'so_sel5'.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SORTIERWERTE_EINLESEN                                    *
*---------------------------------------------------------------------*
*       Einlesen der Sortierwerte aus dem Memory                      *
*---------------------------------------------------------------------*
FORM SORTIERWERTE_EINLESEN.
  IMPORT Z_FELD_IND FROM MEMORY ID 'ind'.
  IF Z_FELD_IND >= 1.
    IMPORT SO_SEL1 FROM MEMORY ID 'so_sel1'.
  ENDIF.
  IF Z_FELD_IND >= 2.
    IMPORT SO_SEL2 FROM MEMORY ID 'so_sel2'.
  ENDIF.
  IF Z_FELD_IND >= 3.
    IMPORT SO_SEL3 FROM MEMORY ID 'so_sel3'.
  ENDIF.
  IF Z_FELD_IND >= 4.
    IMPORT SO_SEL4 FROM MEMORY ID 'so_sel4'.
  ENDIF.
  IF Z_FELD_IND >= 5.
    IMPORT SO_SEL5 FROM MEMORY ID 'so_sel5'.
  ENDIF.
ENDFORM.

* Bei Aufruf aus einem PickUp einer Zeile werden die Sortierwerte der
* aufgepickten Zeile gegen die aktuellen Daten gecheckt.
* Entsprechen die aktuellen Daten diesen nicht werden sie auch nicht
* in den Datenbestand extrahiert.
* Weiterhin werden alle Anlagen nicht angelistet, die einem Komplex
* zugeordnet sind.
FORM SORT_CHECK.
  IF FLG_NOT_FIRST = 1.
    PERFORM SORTIERWERTE_CHECKEN USING FLG_CHK_SUCCESS.
    IF FLG_CHK_SUCCESS NE 1.
* Daten erfüllen nicht die Sortierkriterien.
      REJECT.
    ENDIF.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM SORTIERWERTE_CHECKEN                                     *
*---------------------------------------------------------------------*
*       Die Sortierwerte der aufgepickten  Zeile werden gegen         *
*       die aktuellen Werte gecheckt                                  *
*---------------------------------------------------------------------*
*  -->  FLG_CHK_SUCCESS                                               *
*       gibt an, ob aktueller Wert den Check bestanden hat            *
*---------------------------------------------------------------------*
FORM SORTIERWERTE_CHECKEN USING FLG_CHK_SUCCESS.
* BREAK-POINT.
  FLG_CHK_SUCCESS = 0.
  IF Z_FELD_IND >= 1.                  "Sortierstufe 1
    WRITE <S1> TO HLP_FELD.
    CHECK HLP_FELD IN SO_SEL1.
  ENDIF.
  IF Z_FELD_IND >= 2.                  "Sortierstufe 2
    WRITE <S2> TO HLP_FELD.
    CHECK HLP_FELD IN SO_SEL2.
  ENDIF.
  IF Z_FELD_IND >= 3.                  "Sortierstufe 3
    WRITE <S3> TO HLP_FELD.
    CHECK HLP_FELD IN SO_SEL3.
  ENDIF.
  IF Z_FELD_IND >= 4.                  "Sortierstufe 4
    WRITE <S4> TO HLP_FELD.
    CHECK HLP_FELD IN SO_SEL4.
  ENDIF.
  IF Z_FELD_IND >= 5.                  "Sortierstufe 5
    WRITE <S5> TO HLP_FELD.
    CHECK HLP_FELD IN SO_SEL5.
  ENDIF.
  FLG_CHK_SUCCESS = 1.                 "Daten entsprechen Sortierwerten
ENDFORM.

* Select-Option-Tabelle für Pick-Up aufbauen bzw. zurücksetzen.
FORM SO_TAB_ERNEUERN TABLES SO_NAME SAV_SO_NAME.
* SO-Tabelle erfrischen
  CLEAR   SO_NAME.
  REFRESH SO_NAME.
* SO-Tabelle in Anfangszustand zurücksetzen
  LOOP AT SAV_SO_NAME.
    MOVE SAV_SO_NAME TO SO_NAME.
    APPEND SO_NAME.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM KOMPL_TESTEN                                             *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM KOMPL_TESTEN.
  DATA: FLG_KOMPL.
  IF SUMMB IS INITIAL.
    IF *ANLA0-XAKPL IS INITIAL.
      SELECT * FROM  T093B
        WHERE  BUKRS  IN BUKRS
        AND  (  AFABE = BEREICH1 OR AFABE = BEREICH2 OR
                  AFABE = BEREICH3 )
        ORDER BY PRIMARY KEY.

        IF NOT T093B-XAKPL IS INITIAL.
          FLG_KOMPL = 'X'.
          EXIT.
        ENDIF.
      ENDSELECT.
    ENDIF.
  ENDIF.
  IF FLG_KOMPL IS INITIAL.
    EXCLKEY-FUNKTION = 'KOMP'.
    APPEND EXCLKEY.
  ENDIF.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM KOMPLEX_AUFLOESEN                                        *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM KOMPLEX_AUFLOESEN.
  RANGES: HLP_ANLAGE FOR ANLAV-ANLN1,
          HLP_BUKRS  FOR ANLAV-BUKRS.

  IF ANLAV-XANLGR IS INITIAL.
    MESSAGE E421.
*    Die selektierte Anlage ist kein Komplex.
  ENDIF.
************************************************************************
  SUBMIT ZAKOMP01 AND RETURN
************************************************************************
                  WITH BUKRS EQ ANLAV-BUKRS SIGN 'I'
                  WITH ANLAGE EQ ANLAV-ANLN1 SIGN 'I'
                  WITH BEREICH1 EQ BEREICH1
                  WITH BERDATUM EQ BERDATUM
                  WITH SRTVR EQ SRTVR.
ENDFORM.

* Arbeitsvorrat erzeugen.
FORM ARBVOR_AUFBAUEN.
* Tabellen für Popup bekanntmachen.
  TABLES: SWZDUMRES, SWWWIHEAD.
*
  INCLUDE <CNTAIN>.
  INCLUDE RSWUINCL.

* Spezifische Daten zum Aufbauen eines Arbeitsvorrats.
*     Hilfsstrukur zum Aufbauen der Objektliste. (Anlagenaufbau)
  DATA: BEGIN OF HLP_OBJECT,
          LOGSYS  LIKE SWOTOBJID-LOGSYS,
          OBJTYPE LIKE SWOTOBJID-OBJTYPE,
          BUKRS   LIKE ANLA-BUKRS,
          ANLN1   LIKE ANLA-ANLN1,
          ANLN2   LIKE ANLA-ANLN2,
        END    OF HLP_OBJECT.

*     Agenten.
  DATA: BEGIN OF AGENTS OCCURS 0.
          INCLUDE STRUCTURE SWHACTOR.
  DATA: END   OF AGENTS.

*     Deadline-Agenten.
  DATA: BEGIN OF DEADLINE_AGENTS OCCURS 0.
          INCLUDE STRUCTURE SWHACTOR.
  DATA: END   OF DEADLINE_AGENTS.

*     Excluded-Agenten.
  DATA: BEGIN OF EXCLUDED_AGENTS OCCURS 0.
          INCLUDE STRUCTURE SWHACTOR.
  DATA: END   OF EXCLUDED_AGENTS.

*     Notification-Agenten.
  DATA: BEGIN OF NOTIFICATION_AGENTS OCCURS 0.
          INCLUDE STRUCTURE SWHACTOR.
  DATA: END   OF NOTIFICATION_AGENTS.

*     Item-Agenten.
  DATA: BEGIN OF ITEM_AGENTS OCCURS 0.
          INCLUDE STRUCTURE SWHACTOR.
  DATA: END   OF ITEM_AGENTS.

*     Objektliste des Arbeitsvorrats.
  DATA: BEGIN OF OBJECT_LIST OCCURS 0.
          INCLUDE STRUCTURE SWZAIELEM.
  DATA: END   OF OBJECT_LIST.

*     Sekundärmethoden.
  DATA: BEGIN OF SEC_METHODS OCCURS 0.
          INCLUDE STRUCTURE SWWMETHODS.
  DATA: END   OF SEC_METHODS.

*     Completion Events.
  DATA: BEGIN OF COMP_EVENTS OCCURS 0.
          INCLUDE STRUCTURE SWWCOMPEVT.
  DATA: END   OF COMP_EVENTS.

*     Container zur Werte- und Parameterübergabe.
  DATA: BEGIN OF WI_CONTAINER OCCURS 0.
          INCLUDE STRUCTURE SWCONT.
  DATA: END   OF WI_CONTAINER.


*    Vorfüllen des Containers mit den Anlagenwerten.
  DATA BEGIN OF OBJECT.
          INCLUDE STRUCTURE SWOTOBJID.
  DATA END OF OBJECT.

*     Zu füllende Felder.
  DATA:

        DEF_TASK LIKE   SWZAIENTRY-TASK_ID,      " Def-Aufgabe
        APPL     LIKE SWZAI-APPL       " Erz. Applikation
                 VALUE 'AMAA',
        CREATOR  LIKE SWWWIHEAD-WI_CREATOR,    " Erzeuger undef. Eingabe
        WI_TEXT  LIKE SWWWIHEAD-WI_TEXT,       " Kurztext des AV
        WI_ID    LIKE SWWWIHEAD-WI_ID, " Kennung des AV
        PRIO     LIKE SWWWIHEAD-WI_PRIO,       " Ausführungspriorität
        RELE     LIKE SWZAI-RELEASED.  " Sofortige Ausführung

* Flags.
  DATA: FLG_ERL(1).                    " Abgang mit/ohne Erlös.
  DATA: AI_RETURN(1).                  " AV nicht anlegen.

* Konstanten.
  DATA: C_OBJ_TYPE LIKE SWOTOBJID-OBJTYPE VALUE 'AM_AI'.  " Objekttyp.

* return value.
  DATA  RETURNCODE(1)  TYPE C.
*

  DO.
*   Notwendige Felder, die nicht automatisch gesetzt werden können.
    CALL FUNCTION 'AM_AI_POPUP_GET_CREATE_VAL'
         IMPORTING
              E_ORGTASK  = DEF_TASK
              E_WI_TEXT  = WI_TEXT
              ABBRUCH_KZ = RETURNCODE.

*   Wenn Abbrechen angewählt wurde, DO-Schleife verlassen.
    IF RETURNCODE = 'A'.
      MESSAGE ID 'AB' TYPE 'S' NUMBER '050'.
      EXIT.
    ENDIF.

*   Kennzeichen zum Nichtanlegen löschen.
    CLEAR AI_RETURN.
*   Abhängig von der Aufgabe weitere Daten erfragen.
    CASE DEF_TASK.
*     bei Abgang mit Erlös.
      WHEN 'TS00007957'.
        FLG_ERL = 'X'.
        PERFORM ABGANG_POPUP_SCHICKEN TABLES WI_CONTAINER
                                      USING  FLG_ERL AI_RETURN.
        OBJECT_LIST-BATCH = 'X'.
*     bei Abgang ohne Erlös.
      WHEN 'TS00007956'.
        CLEAR FLG_ERL.
        PERFORM ABGANG_POPUP_SCHICKEN TABLES WI_CONTAINER
                                      USING  FLG_ERL AI_RETURN.
        OBJECT_LIST-BATCH = 'X'.
*     bei Massenänderung.
      WHEN 'TS00007958'.
        OBJECT_LIST-BATCH = 'X'.
        PERFORM MASS_POPUP_SCHICKEN TABLES WI_CONTAINER
                                    USING  AI_RETURN.
*     bei Anlagen phys. löschen.
      WHEN 'TS00008043'.
        OBJECT_LIST-BATCH = 'X'.
*     bei Stammdatenänderung.
      WHEN OTHERS.
        FLG_ERL = 'M'.
    ENDCASE.

*   AV nicht anlegen, wenn vorher Popup abgebrochen wurde.
    IF AI_RETURN IS INITIAL.
      " DO-Schleife verlassen, da alle Daten sauber eingegeben
                                       " wurden.
      EXIT.
    ENDIF.
  ENDDO.

* Wenn Abbrechen angewählt wurde, kein AV anlegen.
  IF RETURNCODE = 'A'.
    EXIT.
  ENDIF.

* Kennzeichen: Art des Abgangs
  SWC_SET_ELEMENT WI_CONTAINER 'erlkz' FLG_ERL. " Erlöskennzeichen

* Vorbereitungen zum Anlegen des AV.
* HR_ORG spezifisch. Benutzernamen für Zuordnung eintragen.
  AGENTS-OTYPE = 'US'.
  AGENTS-OBJID = 'XXXXX'.
  APPEND AGENTS.

*    Agenten für die Benachrichtigung.
  " NOTIFICATION_AGENTS-OTYPE = 'US'.
  " NOTIFICATION_AGENTS-OBJID = SY-UNAME.
                                       " APPEND NOTIFICATION_AGENTS.

* Felder vorfüllen, die nicht eingegeben werden sollen.
  CREATOR = SY-UNAME.
  PRIO    = 5.

* Arbeitsvorrat mit selektierten Anlagen des Reports füllen.
  LOOP.
    HLP_OBJECT-LOGSYS = SY-SYSID.
    HLP_OBJECT-OBJTYPE = 'BUS1022'.
    HLP_OBJECT-BUKRS = ANLAV-BUKRS.
    HLP_OBJECT-ANLN1 = ANLAV-ANLN1.
    HLP_OBJECT-ANLN2 = ANLAV-ANLN2.
    OBJECT_LIST-OBJECTID = HLP_OBJECT.
    "      Def_Task wird verwendet, da sonst beim Hinzufügen Probleme
    "      auftreten können, daß Anlage zweimal im AV ist,
    "      einmal mit Aufgabe und einmal mit Def_Task.
    "      OBJECT_LIST-TASK_ID = DEF_TASK.
    APPEND OBJECT_LIST.
  ENDLOOP.

* prepare wi container: set the only one
* object for slot WI_LEADING_OBJECT
  CLEAR OBJECT.
  OBJECT-LOGSYS = SY-SYSID.
  OBJECT-OBJTYPE = 'BUS1022'.
  SWC_SET_ELEMENT WI_CONTAINER WI_LEADING_OBJECT OBJECT.

* Funktion zum Erzeugen des Arbeitsvorrats rufen.
  CALL FUNCTION 'SWZ_AI_CREATE'
       EXPORTING
            APPLICATION           = APPL
*           CALLBACK_FB           = ' '
*           COMPLETION_EVENT      = ' '
            CREATOR               = CREATOR
            DEF_TASK              = DEF_TASK
*           DESIRED_END_ACTION    = 'SWW_DI_CREATE'
*           DESIRED_END_DATE      = '00000000'
*           DESIRED_END_TIME      = '000000'
*           DESIRED_START_DATE    = '00000000'
*           DESIRED_START_TIME    = '000000'
*           LANGUAGE              = SY-LANGU
*           LATEST_END_ACTION     = 'SWW_DI_CREATE'
*           LATEST_END_DATE       = '00000000'
*           LATEST_END_ESCALATION = '0'
*           LATEST_END_TIME       = '000000'
*           LATEST_START_ACTION   = 'SWW_DI_CREATE'
*           LATEST_START_DATE     = '00000000'
*           LATEST_START_TIME     = '000000'
            PRIORITY              = PRIO
            RELEASED              = RELE
*           STATUS                = 'READY'
            TEXT                  = WI_TEXT
            OBJECT_TYPE           = C_OBJ_TYPE
       IMPORTING
            WI_ID                 = WI_ID
       TABLES
            AGENTS                = AGENTS
            DEADLINE_AGENTS       = DEADLINE_AGENTS
            EXCLUDED_AGENTS       = EXCLUDED_AGENTS
            ITEM_AGENTS           = ITEM_AGENTS
            NOTIFICATION_AGENTS   = NOTIFICATION_AGENTS
            OBJECT_LIST           = OBJECT_LIST
            SECONDARY_METHODS     = SEC_METHODS
            WI_CONTAINER          = WI_CONTAINER
            COMP_EVENTS           = COMP_EVENTS
       EXCEPTIONS
            ID_NOT_CREATED        = 01
            ID_NOT_EXECUTED       = 02.

  CASE SY-SUBRC.
*   Alles OK.
    WHEN 0.
                                       " Arb. vorrat wurde hinzugefügt.
      MESSAGE ID 'WL' TYPE 'S' NUMBER '879'
              WITH WI_ID.
*   Arb.vorrat wurde nicht erzeugt.
    WHEN 1.
      MESSAGE ID 'WL' TYPE 'E' NUMBER '888'.
*   Arb.vorrat erzeugt, aber nicht ausgeführt.
    WHEN 2.
      MESSAGE ID 'WL' TYPE 'E' NUMBER '886'.

  ENDCASE.

ENDFORM.


* Anlagen des Reports in bestehenden AV aufnehmen.
FORM ARBVOR_AUFNEHMEN.

* Spezifische Daten zum Aufbauen eines Arbeitsvorrats.

*     Im Arbeitsvorrat zu erledigende Aufgabe.
  DATA: DEF_TASK LIKE SWZAIENTRY-TASK_ID,      " Def-Aufgabe
      FLG_ERR(1) TYPE C VALUE '0',     " ERR-Flag, nicht alle Anl.
                                       " konnten eingefügt werden.
        BATCH    LIKE SWZAIELEM-BATCH, " Zusätzliche Felder AV
        TASK_ID  LIKE SWZAIELEM-TASK_ID.                    " AV

*     Struktur für Einzeleinfügen der Anlagen.
*     Hilfsstrukur zum Aufbauen der Objektliste. (Anlagenaufbau)
  DATA: BEGIN OF HLP_OBJECT,
          LOGSYS  LIKE SWOTOBJID-LOGSYS,
          OBJTYPE LIKE SWOTOBJID-OBJTYPE,
          BUKRS   LIKE ANLA-BUKRS,
          ANLN1   LIKE ANLA-ANLN1,
          ANLN2   LIKE ANLA-ANLN2,
        END    OF HLP_OBJECT.

*     Id des ausgewählten Arbeitsvorrats.
  DATA: SELECTED_AI LIKE SWWWIHEAD-WI_ID.

                                       " Popup-Informationen.
* description of fields for popup.
  DATA: BEGIN OF FIELDS OCCURS 1.
          INCLUDE STRUCTURE SVAL.
  DATA: END   OF FIELDS.

* return value.
  DATA  RETURNCODE(1)  TYPE C.
* Antwort
  DATA  ANSWER(1).

  CALL FUNCTION 'POPUP_TO_DECIDE'
       EXPORTING
            DEFAULTOPTION = '1'
            TEXTLINE1     = 'Welche Anlagen'(AV2)
            TEXTLINE2     = 'möchten Sie zum'(AV3)
            TEXTLINE3     = 'Arbeitsvorrat hinzufügen?'(AV4)
            TEXT_OPTION1  = 'Ausgew. Anlage'(AV5)
            TEXT_OPTION2  = 'Alle Anlagen'(AV6)
            TITEL         = 'Arbeitsvorrat ergänzen'(AV1)
*           START_COLUMN  = 25
*           START_ROW     = 6
       IMPORTING
            ANSWER        = ANSWER.

* Welche Alternative ausführen?
  IF ANSWER = 'A'.
*   Abbrechen.
                                       " Aktion wurde abgebrochen.
    MESSAGE ID 'AB' TYPE 'S' NUMBER '050'.
                                       " Verlassen der Form-Routine.
    EXIT.
  ELSEIF ANSWER = '1'.                 " Ausgew. Anlage.
*   Es darf nicht aufgepickt werden / Keine gültige Zeile markiert.
    IF FLG_PICK_UP EQ SPACE OR
       ANLAV-BUKRS EQ SPACE OR
       ANLAV-ANLN1 EQ SPACE.
      MESSAGE E031.
      CLEAR FLG_PICK_UP.
*     Routine verlassen.
      EXIT.
    ENDIF.
  ENDIF.

* Arbeitsvorrat auswählen, in den eingefügt werden soll.
  CALL FUNCTION 'SWZ_AI_VALUE_REQUEST'
       EXPORTING
*           WI_STAT    = ' '
*           WI_CD_FROM = '00000000'
*           WI_CD_TO   = '00000000'
*           WI_AAGENT  = ' '
*           WI_PRIO    = ' '
            APPL       = 'AMAA'
            RELEASED   = ' '
*           DIALOG     = 'X'
*           DISPLAY    = ' '
       IMPORTING
            SEL_AI     = SELECTED_AI.

  IF SELECTED_AI IS INITIAL.
                                       " Aktion wurde abgebrochen.
    MESSAGE ID 'AB' TYPE 'S' NUMBER '050'.
                                       " Verlassen der Form-Routine.
    EXIT.
  ENDIF.

* Batch-Kennzeichen anhand des AV ermitteln.
  PERFORM BATCHKZ_ERMITTELN USING SELECTED_AI BATCH.


* Einzelne Anlage hinzufügen.
  IF ANSWER = '1'.
    PERFORM AV_SINGLE_ABMISCHEN USING  SELECTED_AI BATCH.
* Alle Anlagen der Liste mit AV abmischen.
  ELSE.                                " answer = '2'.
    PERFORM AV_LISTE_ABMISCHEN  USING  SELECTED_AI  BATCH.
  ENDIF.


ENDFORM.


* Ausgewählte Einzelanlage in bestehenden AV hinzufügen.
FORM AV_SINGLE_ABMISCHEN USING SELECTED_AI BATCH.

*     Übereinstimmung der Buchungskreise.
  DATA: FLG_CHCK(1) TYPE C.


* Einzufügende Zeile.
  DATA: BEGIN OF INSERT_ASSET.
          INCLUDE STRUCTURE SWZAIELEM.
  DATA: END   OF INSERT_ASSET.

* Prüfung, ob Buchungskreis mit Buchungskreis des AV übereinstimmt.
  PERFORM AI_BUKRS_CHECK USING SELECTED_AI ANLAV-BUKRS
                               ANLAV-ANLN1 FLG_CHCK.

* Wenn Buchungskreise abweichen, Anlagen nicht einfügen und
* Routine verlassen.
  IF NOT FLG_CHCK IS INITIAL.
    EXIT.
  ENDIF.

* Einzelne Zeile des Arbeitsvorrats aufbauen.
  PERFORM AI_ZEILE_AUFBAUEN USING  ANLAV-BUKRS    ANLAV-ANLN1
                                   ANLAV-ANLN2    BATCH
                                   INSERT_ASSET.


* Hilfsobjekt einfügen mit FB.
  CALL FUNCTION 'SWZ_AI_ELEMENT_APPEND'
       EXPORTING
            OBJECT           = INSERT_ASSET
            WI_ID            = SELECTED_AI
       EXCEPTIONS
            EXECUTION_FAILED = 01
            READ_FAILED      = 02.
  CASE SY-SUBRC.
    WHEN '0'.
      " Anlage wurde in bestehenden AV hinzugefügt.
      MESSAGE ID 'AY' TYPE 'I' NUMBER '804'
              WITH ANLAV-ANLN1 ANLAV-ANLN2 SELECTED_AI.
    WHEN OTHERS.
      " Anlage konnte nicht hinzugefügt werden.
      MESSAGE ID 'AY' TYPE 'I' NUMBER '805'
              WITH ANLAV-ANLN1 ANLAV-ANLN2 SELECTED_AI.

  ENDCASE.

ENDFORM.



* Einzelne Zeile des Arbeitsvorrats aufbauen.
FORM AI_ZEILE_AUFBAUEN USING BUKRS    ANLN1
                             ANLN2    BATCH
                             INSERT_ASSET STRUCTURE SWZAIELEM.

*     Struktur für Einzeleinfügen der Anlagen.
*     Hilfsstrukur zum Aufbauen der Objektliste. (Anlagenaufbau)
  DATA: BEGIN OF HLP_OBJECT,
          LOGSYS  LIKE SWOTOBJID-LOGSYS,
          OBJTYPE LIKE SWOTOBJID-OBJTYPE,
          BUKRS   LIKE ANLA-BUKRS,
          ANLN1   LIKE ANLA-ANLN1,
          ANLN2   LIKE ANLA-ANLN2,
        END    OF HLP_OBJECT.

*   Hilfsobjekt Zeile eines AV's aufbauen.
  HLP_OBJECT-LOGSYS  = SY-SYSID.
  HLP_OBJECT-OBJTYPE = 'BUS1022'.
  HLP_OBJECT-BUKRS   = BUKRS.
  HLP_OBJECT-ANLN1   = ANLN1.
  HLP_OBJECT-ANLN2   = ANLN2.
*   Zeile für Insert aufbereiten.
  INSERT_ASSET-OBJECTID = HLP_OBJECT.
  "   Diese Zeile wurde entfernt, um doppelte Einträge zu vermeiden.
  "   Anlage einmal mit Task und einmal mit DEFAULT_TASK.
  "   INSERT_ASSET-TASK_ID = TASK_ID.
  INSERT_ASSET-BATCH   = BATCH.

ENDFORM.



* Alle Anlagen der Liste mit bestehendem AV abmischen.
FORM AV_LISTE_ABMISCHEN USING SELECTED_AI  BATCH.

* Einzufügende Zeile.
  DATA: BEGIN OF INSERT_ASSET.
          INCLUDE STRUCTURE SWZAIELEM.
  DATA: END   OF INSERT_ASSET.

  DATA: FLG_CHCK(1) TYPE C,            " Buchungskreis identisch.
       FLG_ERR(1) TYPE C VALUE '0'.    " ERR-Flag, nicht alle Anl.
                                       " konnten eingefügt werden.

* Prüfung, ob Buchungskreise identsich sind.
  LOOP.
    PERFORM AI_BUKRS_CHECK USING SELECTED_AI ANLAV-BUKRS
                                 ANLAV-ANLN1 FLG_CHCK.
*   Schleife verlassen.
    EXIT.
  ENDLOOP.

* Wenn Buchungskreise abweichen, Anlagen nicht einfügen und
* Routine verlassen.
  IF NOT FLG_CHCK IS INITIAL.
    EXIT.
  ENDIF.

* Anlagen ermitteln, die in AV zusätzlich aufzunehmen sind.
  LOOP.
*   Einzelne Zeile des Arbeitsvorrats aufbauen.
    PERFORM AI_ZEILE_AUFBAUEN USING ANLAV-BUKRS    ANLAV-ANLN1
                                    ANLAV-ANLN2    BATCH
                                    INSERT_ASSET.

*   Hilfsobjekt einfügen mit FB.
    CALL FUNCTION 'SWZ_AI_ELEMENT_APPEND'
         EXPORTING
              OBJECT           = INSERT_ASSET
              WI_ID            = SELECTED_AI
         EXCEPTIONS
              EXECUTION_FAILED = 01
              READ_FAILED      = 02.
    CASE SY-SUBRC.
      WHEN '0'.
        " Anlage wurde in bestehenden AV hinzugefügt.
      WHEN OTHERS.
        FLG_ERR = ON.
        " Anlage konnte nicht hinzugefügt werden.
        " Dieser Fehler wird hier nicht explizit aufgeführt.
    ENDCASE.
  ENDLOOP.

  IF FLG_ERR = ON.
    " Nicht alle Anlagen wurden in AV aufgenommen.
    MESSAGE S450 WITH SELECTED_AI.
  ELSE.
                                       " Alle Anlagen wurden eingefügt.
    MESSAGE S451 WITH SELECTED_AI.
  ENDIF.

ENDFORM.


*&---------------------------------------------------------------------*
*&      FORM AI_BUKRS_CHECK
*&---------------------------------------------------------------------*
*       Prüfung, ob die neu aufzunehmende Anlage im gleichen           *
*       Buchungskreis liegt wie die bestehenden Anlagen.
*----------------------------------------------------------------------*
*  -->  U_AI      Arbeitsvorrat
*  -->  U_BUKRS   Buchungskreis
*----------------------------------------------------------------------*
FORM AI_BUKRS_CHECK USING U_AI U_BUKRS U_ANLN1 U_RESULT.

* Zeilen des Arbeitsvorrats.
  DATA: BEGIN OF XASSETS OCCURS 0.
          INCLUDE STRUCTURE ASSET.
  DATA: END OF XASSETS.

* Anlagen des Arbeitsvorrats lesen.
  CALL FUNCTION 'AM_ASSETS_GET_FROM_AI'
       EXPORTING
            WI_ID               = U_AI
       TABLES
            T_ASSETS            = XASSETS
       EXCEPTIONS
            WI_ID_NOT_FOUND     = 01
            FALSE_OBJECTS_IN_AI = 02.

* Alle Zeilen des Arbeitsvorrats haben denselben Buchungskreis.
* gemäß Definition.
  READ TABLE XASSETS INDEX 1.
  IF XASSETS-BUKRS <> U_BUKRS.
*   Aufzunehmende Anlage ist im falschen Buchungskreis.
    U_RESULT = 'A'.
    MESSAGE ID 'AY' TYPE 'I' NUMBER '810'
            WITH U_ANLN1 XASSETS-BUKRS U_AI U_BUKRS.
  ENDIF.

ENDFORM.                               " AI_BUKRS_CHECK


*---------------------------------------------------------------------*
*       FORM ABGANG_POPUP_SCHICKEN                                    *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  CONTAINER                                                     *
*  -->  FLG_ERL                                                       *
*  -->  AI_RETURN                                                     *
*---------------------------------------------------------------------*
FORM ABGANG_POPUP_SCHICKEN TABLES CONTAINER STRUCTURE SWCONT
                           USING  FLG_ERL AI_RETURN.

* Catt-Ablauf komplett dunkel steuern.
  DATA: CON_DUNKEL LIKE RSCAT-BDCMODE  VALUE 'N'.
* return value.
  DATA  RETURNCODE(1)  TYPE C.
* Felder des Abgangs,
  DATA:
    L_BUDAT LIKE RA01B-BUDAT,
    L_BLDAT LIKE RA01B-BLDAT,
    L_BWASL LIKE ANBZ-BWASL,
    L_ERBDM LIKE ANBZ-ERBDM,
    L_BZDAT LIKE ANBZ-BZDAT,
* Verteilkennzeichen.
    L_VERKZ(1).

* Popup aufrufen.
  CALL FUNCTION 'POPUP_TO_GET_AI_VALUES'
       EXPORTING
            I_ERLKZ    = FLG_ERL
       IMPORTING
            E_BUDAT    = L_BUDAT
            E_BLDAT    = L_BLDAT
            E_BWASL    = L_BWASL
            E_ERBDM    = L_ERBDM
            E_BZDAT    = L_BZDAT
            ABBRUCH_KZ = RETURNCODE
            E_VERKZ    = L_VERKZ.


  IF RETURNCODE = 'A'.
    " Kennzeichen setzen, AV nicht anzulegen.
    AI_RETURN = 'X'.
                                       " Verlassen der Form-Routine.
    EXIT.
  ENDIF.

* Übertragen der eingegebenen Werte in den Daten-Container.
  SWC_SET_ELEMENT CONTAINER 'bldat'    L_BLDAT.    " Belegdatum
  SWC_SET_ELEMENT CONTAINER 'bwasl'    L_BWASL.    " Bewegungsart
  SWC_SET_ELEMENT CONTAINER 'bzdat'    L_BZDAT.    " Bezugsdatum
  SWC_SET_ELEMENT CONTAINER 'budat'    L_BUDAT.    " Buchungsdatum
  SWC_SET_ELEMENT CONTAINER 'erbdm'    L_ERBDM.    " Erlösbetrag
  SWC_SET_ELEMENT CONTAINER 'verkz'    L_VERKZ.    " Verteilkennzeichen.
  SWC_SET_ELEMENT CONTAINER 'mode'     CON_DUNKEL. " CATT-Ablauf dunkel.

ENDFORM.

* Popup für Massenänderungen schicken.
FORM MASS_POPUP_SCHICKEN TABLES CONTAINER STRUCTURE SWCONT
                         USING  AI_RETURN.
* return value.
  DATA  RETURNCODE(1)  TYPE C.
* Zurückgegebene Substitution.
  DATA: L_SUBSTID LIKE T093SB-SUBSTID.


* Popup aufrufen.
  CALL FUNCTION 'POPUP_TO_GET_AI_MASS_VALUES'
       IMPORTING
            E_SUBSTID  = L_SUBSTID
            ABBRUCH_KZ = RETURNCODE
       TABLES
            SEL_BUKRS  = BUKRS.

  IF RETURNCODE = 'A'.
    " Kennzeichen setzen, AV nicht anzulegen.
    AI_RETURN = 'X'.
    " Es wurde keine gültige Substitution ausgewählt.
    MESSAGE ID 'AB' TYPE 'S' NUMBER '051'.
                                       " Verlassen der Form-Routine.
    EXIT.
  ENDIF.

* Übertragen der eingegebenen Werte in den Daten-Container.
  SWC_SET_ELEMENT CONTAINER 'substid'  L_SUBSTID.  " Substitution.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM DYN_SEL_CHECK                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM DYN_SEL_CHECK.

  DATA: HLP_REPID LIKE RSVAR-REPORT,
        HLP_FLG,
        HLP_RANGE TYPE RSDS_TRANGE.

  HLP_REPID = SY-REPID.
  CALL FUNCTION 'RS_REFRESH_FROM_DYNAMICAL_SEL'
       EXPORTING
            CURR_REPORT        = HLP_REPID
            MODE_WRITE_OR_MOVE = HLP_FLG
       IMPORTING
            P_TRANGE           = HLP_RANGE
       EXCEPTIONS
            NOT_FOUND          = 01.

  IF SY-SUBRC EQ 0.
    MESSAGE E422.
  ENDIF.
ENDFORM.


* Ermitteln des Batch-Kennzeichens anhand der Zeilen des AV.
FORM BATCHKZ_ERMITTELN USING AI BATCH.

* Zeileninformationen des AV.
  DATA: BEGIN OF OL OCCURS 0.
          INCLUDE STRUCTURE SWZELES.
  DATA: END OF OL.

* Lesen der Zeilen des Arbeitsvorrats.
  CALL FUNCTION 'SWZ_AI_SHOW'
       EXPORTING
            WI_ID            = AI
       TABLES
            OBJECT_LIST      = OL
       EXCEPTIONS
            EXECUTION_FAILED = 01.

* This error shoul never occur.
  IF SY-SUBRC = 0.
*   1. Eintrag lesen und dieses Batch-Kz übernehmen.
    READ TABLE OL INDEX 1.
    IF SY-SUBRC = 0.
      BATCH = OL-BATCH.
    ENDIF.
  ENDIF.

ENDFORM.

* Reports über mehr als einen Buchungskreis -> keine AV's erzeugen.
FORM AV_BUKRS_CHECK.

  DESCRIBE TABLE BUKRS LINES SY-TABIX.
  IF SY-TABIX > 1.
*   AV's erzeugen.
    EXCLKEY-FUNKTION = 'AICR'.
    APPEND EXCLKEY.
*   AV's ergänzen.
    EXCLKEY-FUNKTION = 'AIME'.
    APPEND EXCLKEY.
  ELSE.
*   Buchungskreis mit Between angegeben.
    READ TABLE BUKRS INDEX 1.
    IF BUKRS-HIGH > BUKRS-LOW AND
       BUKRS-SIGN <> 'EQ'.
*     AV's erzeugen.
      EXCLKEY-FUNKTION = 'AICR'.
      APPEND EXCLKEY.
*     AV's ergänzen.
      EXCLKEY-FUNKTION = 'AIME'.
      APPEND EXCLKEY.
    ENDIF.
  ENDIF.

ENDFORM.


* Selektionsdaten in die Bericht-Bericht-Schnittstelle schreiben
* und Aufruf des Communicationhandler.
FORM SEND_SEL TABLES SEL_OPTS STRUCTURE RSPARAMS
              USING  U_REC
                     VALUE(FLG_CALL_SUM)
                     VALUE(U_FLG).


* Selektionsdaten an die BBS-Schnittstelle senden.
  PERFORM BBS_VERSORGEN USING FLG_CALL_SUM U_FLG.

* Aufruf des Berichts über den Communication-Handler.
  PERFORM COMM_HANDLER_RUFEN  USING U_REC U_FLG.

ENDFORM.


* Ermitteln der aktuellen Select-Options des Reports.
FORM SELOPTIONS_ERMITTELN  USING  VALUE(REPORT).

* Select-Options des Reports.
  CALL FUNCTION 'RSTI_REPORT_FIELDS_FIND'
       EXPORTING
            E_REPID   = REPORT
            E_TYPE    = 'R'
       TABLES
            IT_SEL    = IT_SELR
            IT_FIELDS = IT_FIELDR.


ENDFORM.


*---------------------------------------------------------------------*
*       FORM SELOPTIONS_UEBERTRAGEN                                   *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  SEL_OPTS                                                      *
*  -->  VALUE(REPORT)                                                 *
*  -->  VALUE(VARIANT)                                                *
*---------------------------------------------------------------------*


*---------------------------------------------------------------------*
*       FORM BBS_VERSORGEN                                            *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  SEL_OPTS                                                      *
*---------------------------------------------------------------------*
FORM BBS_VERSORGEN USING VALUE(FLG_CALL_SUM)
                         VALUE(V_FLG).
* Tabelle Feldkatalog initialisieren.
  CLEAR IT_SEL.
  REFRESH IT_SEL.

* Tabelle Selektionsdaten initialisieren.
  CLEAR IT_FIELDS.
  REFRESH IT_FIELDS.

* 2. Feldkatalog aufstellen.
  CALL FUNCTION 'RSTI_REPORT_FIELDS_FIND'
       EXPORTING
            E_REPID   = REPORT
            E_TYPE    = 'S'            " Sender
       TABLES
            IT_FIELDS = IT_FIELDS
            IT_SEL    = IT_SEL.

* Selektionen manipulieren. Aus dem Summenbericht wird die Einzelliste
* erzeugt.
  PERFORM SELOPTIONS_MANIPULIEREN TABLES IT_SEL
                                         IT_FIELDS
                                  USING  REPORT
                                         FLG_CALL_SUM
                                         V_FLG.

* 3. Beides senden.
  CALL FUNCTION 'RSTI_SELECTION_EXPORT'
       TABLES
            IT_SEL    = IT_SEL
            IT_FIELDS = IT_FIELDS.


ENDFORM.

*
FORM COMM_HANDLER_RUFEN USING U_REC STRUCTURE RSTIREC   " Empfänger
                              VALUE(U_FLG).
*     Senderbericht.
  DATA: SREPORT LIKE TRSTI-SONAM,
*     Aufruftcode
        FCODE  LIKE SY-TCODE,
*     Einzelbericht.
* Alle Berichte.
         FCCLS(2).

  DATA: L_EXIT.

  IF U_FLG = 'I'.
    FCCLS = '1'.
  ELSE.
    FCCLS = '9A'.
  ENDIF.

* Senderbericht ist immer der Report selber.
  SREPORT = SY-REPID.
* Sendetransaktion.
  FCODE  = SY-TCODE.

* Aufruf eines Berichts über BBS.
  CALL FUNCTION 'RSTI_COMMUNICATION_HANDLER'
       EXPORTING
*            E_SONAM                    = SREPORT
            E_FCODE                    = FCODE
            E_REXEC                    = 'X'
            E_FCCLS                    = FCCLS
            E_REC                      = U_REC
       IMPORTING
            I_EXIT                     = L_EXIT
       EXCEPTIONS
            APPL_STACK_NOT_INITIALIZED = 01
            NO_LINES                   = 02
            NO_LINE_PICKED             = 03.

* Report verlassen wegen Navigationsbaustein
  IF NOT L_EXIT IS INITIAL.
    LEAVE.
  ENDIF.
ENDFORM.

* Bei Pick-Up auf Summenliste wird eine Einzelliste.
FORM SELOPTIONS_MANIPULIEREN TABLES IT_SEL    STRUCTURE RSTISEL
                                    IT_FIELDS STRUCTURE RSTIFIELDS
                             USING  VALUE(U_REPORT)
                                    VALUE(FLG_CALL_SUM)
                                    VALUE(V_FLG).

* Lokaler Index.
  DATA: L_TABIX LIKE SY-TABIX.

* Die <Sortiert nach> Parameter müssen eliminiert werden
  LOOP AT IT_SEL.
    IF IT_SEL-FIELD CS 'PA_SRT'.
      DELETE IT_SEL.
    ENDIF.
  ENDLOOP.
  LOOP AT IT_FIELDS.
    IF IT_FIELDS-FIELD CS 'PA_SRT'.
      DELETE IT_FIELDS.
    ENDIF.
  ENDLOOP.

  IF V_FLG = 'I'.
* Bisher Summenliste und nun Einzelliste angefordert.
    IF NOT SUMMB IS INITIAL AND
      FLG_CALL_SUM IS INITIAL.
*   Summenberichtsflag ausschalten. -> it_sel
*   Einzelliste einschalten.

* Sortiervariantenparameter als Selektionskriterium mitgeben.
      PERFORM SORTIERVARIANTE_ERGAENZEN TABLES IT_SEL
                                               IT_FIELDS
                                        USING T086.

      READ TABLE IT_SEL WITH KEY FIELD = 'SUMMB'.
      IF SY-SUBRC = 0.
        L_TABIX = SY-TABIX.
        IT_SEL-LOW = ' '.
        MODIFY IT_SEL INDEX L_TABIX.
*     Explizit setzen.
        IT_SEL-FIELD = 'XEINZEL'.
        IT_SEL-LOW   = 'X'.
        APPEND IT_SEL.
      ENDIF.
      READ TABLE IT_FIELDS WITH KEY FIELD = 'SUMMB'.
      IF SY-SUBRC = 0.
        IT_FIELDS-FIELD  = 'XEINZEL'.
        APPEND IT_FIELDS.
      ENDIF.
*   IF SY-SUBRC = 0.
*     L_TABIX = SY-TABIX.
*     IT_SEL-FIELD = 'XEINZL'.
*     MODIFY IT_SEL INDEX L_TABIX.
*   ENDIF.
*   READ TABLE IT_FIELDS WITH KEY FIELD = 'SUMMB'.
*   IF SY-SUBRC = 0.
*     IT_FIELDS-FIELD  = 'XEINZL'.
*     MODIFY IT_FIELDS INDEX SY-TABIX.
*   ENDIF.


    ENDIF.

* Bisher Summenliste und nun auch Summenliste.
    IF NOT SUMMB IS INITIAL AND
       NOT FLG_CALL_SUM IS INITIAL.
*   Einzelliste ausschalten.
      READ TABLE IT_SEL WITH KEY FIELD = 'SUMMB'.
      IF SY-SUBRC = 0.
*     Explizit setzen.
        IT_SEL-FIELD = 'XEINZEL'.
        IT_SEL-LOW   = ' '.
        APPEND IT_SEL.
      ENDIF.
      READ TABLE IT_FIELDS WITH KEY FIELD = 'SUMMB'.
      IF SY-SUBRC = 0.
        IT_FIELDS-FIELD  = 'XEINZEL'.
        APPEND IT_FIELDS.
      ENDIF.

    ENDIF.

* Bisher Hauptnummerliste und nun auch Hauptnummernliste
    IF NOT XUNTNR IS INITIAL.
*   Einzelliste ausschalten.
      READ TABLE IT_SEL WITH KEY FIELD = 'XUNTNR'.
      IF SY-SUBRC = 0.
*     Explizit setzen.
        IT_SEL-FIELD = 'XEINZEL'.
        IT_SEL-LOW   = ' '.
        APPEND IT_SEL.
      ENDIF.
      READ TABLE IT_FIELDS WITH KEY FIELD = 'XUNTNR'.
      IF SY-SUBRC = 0.
        IT_FIELDS-FIELD  = 'XEINZEL'.
        APPEND IT_FIELDS.
      ENDIF.

    ENDIF.
  ELSE.
    LOOP AT IT_SEL.
      IF IT_SEL-FIELD NE 'BUKRS'
         AND IT_SEL-FIELD NE 'ANLAGE'
         AND IT_SEL-FIELD NE 'UNTNR'.
        DELETE IT_SEL.
      ENDIF.
    ENDLOOP.
    LOOP AT IT_FIELDS.
      IF IT_FIELDS-FIELD NE 'BUKRS'
         AND IT_FIELDS-FIELD NE 'ANLAGE'
         AND IT_FIELDS-FIELD NE 'UNTNR'.
        DELETE IT_FIELDS.
      ENDIF.
    ENDLOOP.
  ENDIF.

* Bisher Einzelliste und Anlage ausgewählt.
  IF SUMMB IS INITIAL AND NOT FLG_PICK_UP IS INITIAL.
*   Anlage in Select-Options eintragen.
    PERFORM ANLAGE_FUELLEN TABLES IT_SEL
                                  IT_FIELDS.
    CLEAR FLG_PICK_UP.
  ENDIF.

* Reports mit Sonderbehandlung.
  CASE REPORT.
    WHEN 'RAUSAG01'.
      PERFORM SELOPTIONS_SPEZ_ANPASSEN(RAUSAG01)
              TABLES IT_SEL IT_FIELDS
              IF FOUND.
    WHEN 'RAUSMQ10'.
      PERFORM SELOPTIONS_SPEZ_ANPASSEN(RAUSMQ10)
              TABLES IT_SEL IT_FIELDS
              IF FOUND.
    WHEN 'RAHERK01'.
      PERFORM SELOPTIONS_SPEZ_ANPASSEN(RAHERK01)
              TABLES IT_SEL IT_FIELDS
              IF FOUND.
  ENDCASE.

ENDFORM.

*---------------------------------------------------------------------*
*       FORM SORTIERVARIANTE_ERGAENZEN                                *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  IT_SEL                                                        *
*  -->  IT_FIELDS                                                     *
*---------------------------------------------------------------------*
FORM SORTIERVARIANTE_ERGAENZEN TABLES IT_SEL    STRUCTURE RSTISEL
                                      IT_FIELDS STRUCTURE RSTIFIELDS
                                     USING VALUE(V_T086) STRUCTURE T086.


*--Felder aus Sortiervariante ergänzen -------------------------------*
  IF NOT V_T086 IS INITIAL.
    DO Z_FELD_IND TIMES VARYING V_T086-TABLN1 FROM V_T086-TABLN1
                                      NEXT V_T086-TABLN2
                        VARYING V_T086-FELDN1 FROM V_T086-FELDN1
                                      NEXT V_T086-FELDN2.
      IF V_T086-TABLN1 = 'ANLAV' AND NOT V_T086-FELDN1 IS INITIAL.
        READ TABLE IT_FIELDS WITH KEY FIELD = V_T086-FELDN1.

        IF SY-SUBRC <> 0.
          IT_FIELDS-FIELD = V_T086-FELDN1.
          IT_FIELDS-ROLLNAME = V_T086-FELDN1.
          CLEAR IT_FIELDS-DOMNAME.
          APPEND IT_FIELDS.
        ENDIF.
        READ TABLE IT_SEL WITH KEY FIELD = V_T086-FELDN1.

        IT_SEL-SIGN = 'I'.
        IT_SEL-OPTION = 'EQ'.
        CASE SY-INDEX.
          WHEN 1.
            IT_SEL-LOW = <S1>.
          WHEN 2.
            IT_SEL-LOW = <S2>.
          WHEN 3.
            IT_SEL-LOW = <S3>.
          WHEN 4.
            IT_SEL-LOW = <S4>.
          WHEN 5.
            IT_SEL-LOW = <S5>.
        ENDCASE.
        IF SY-SUBRC <> 0.
          IT_SEL-FIELD = V_T086-FELDN1.
          APPEND IT_SEL.
        ELSE.
          MODIFY IT_SEL INDEX SY-TABIX.
        ENDIF.
      ENDIF.
    ENDDO.
  ENDIF.
ENDFORM.



* Anlagennummer aus Hide-Bereich in die Sel-Optiontabelle füllen.
FORM ANLAGE_FUELLEN TABLES IT_SEL    STRUCTURE RSTISEL
                           IT_FIELDS STRUCTURE RSTIFIELDS.

*   Anlagenhauptnummer füllen.
  PERFORM SELOPTIONS_EINTRAG TABLES IT_SEL
                                    IT_FIELDS
                             USING  'ANLAGE' 'ANLAV'
                                    'ANLN1' ANLAV-ANLN1.
*   Anlagenunternummer.
  PERFORM SELOPTIONS_EINTRAG TABLES IT_SEL
                                    IT_FIELDS
                             USING  'UNTNR' 'ANLAV'
                                    'ANLN2' ANLAV-ANLN2.
*   Buchungskreis.
  PERFORM SELOPTIONS_EINTRAG TABLES IT_SEL
                                    IT_FIELDS
                             USING  'BUKRS' 'ANLAV'
                                    'BUKRS' ANLAV-BUKRS.

ENDFORM.



*---------------------------------------------------------------------*
*       FORM SELOPTIONS_EINTRAG                                       *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  SEL_OPTS                                                      *
*  -->  VALUE(SEL_NAM)                                                *
*  -->  VALUE(SEL_VAL)                                                *
*---------------------------------------------------------------------*
FORM SELOPTIONS_EINTRAG TABLES IT_SEL     STRUCTURE RSTISEL
                               IT_FIELDS  STRUCTURE RSTIFIELDS
                        USING  VALUE(SEL_OPTNAM)
                               VALUE(SEL_TABNAM)
                               VALUE(SEL_FNAM)
                               VALUE(SEL_VAL).

* Lokaler Tabellenindex.
  DATA: L_TABIX LIKE SY-TABIX.

* Eintrag aus Tabelle it_sel genau spezifizieren.
  READ TABLE IT_SEL WITH KEY FIELD = SEL_OPTNAM.
  IF SY-SUBRC = 0.
    CLEAR: IT_SEL-HIGH.
    IT_SEL-LOW    = SEL_VAL.
    IT_SEL-SIGN   = 'I'.
    IT_SEL-OPTION = 'EQ'.
    MODIFY IT_SEL INDEX SY-TABIX.
    L_TABIX = SY-TABIX + 1.
    LOOP AT IT_SEL FROM L_TABIX
        WHERE FIELD = SEL_OPTNAM.
      DELETE IT_SEL.
    ENDLOOP.
* Eintrag noch nicht vorhanden.
  ELSE.
    CLEAR: IT_SEL.
    IT_SEL-FIELD  = SEL_OPTNAM.
    IT_SEL-LOW    = SEL_VAL.
    IT_SEL-SIGN   = 'I'.
    IT_SEL-OPTION = 'EQ'.
    APPEND IT_SEL.
*   Namen einfügen
    READ TABLE IT_FIELDS WITH KEY FIELD = SEL_OPTNAM.
    IF SY-SUBRC <> 0.
      PERFORM GET_FIELD(RDDFIE00)
        USING SEL_TABNAM SEL_FNAM SY-LANGU
        CHANGING DFIES SY-SUBRC.
      IT_FIELDS-DOMNAME = DFIES-DOMNAME.                " Domäne
      IT_FIELDS-ROLLNAME = DFIES-ROLLNAME.              " Datenelement
      IT_FIELDS-FIELD = SEL_OPTNAM.    " Feldname
      IT_FIELDS-KIND  = 'S'.           " Selectoption
      APPEND IT_FIELDS.
    ENDIF.
  ENDIF.

ENDFORM.

* Navigationsbaustein der BBS.
FORM BERICHTSHISTORIE_AUFRUFEN.
  DATA: L_EXIT.

  CALL FUNCTION 'RSTI_NAVIGATE'
*      EXPORTING
*         E_LEVEL                    =
       IMPORTING
          I_EXIT                     =  L_EXIT.
*  EXCEPTIONS
*         APPL_STACK_NOT_INITIALIZED = 1
*         OTHERS                     = 2.
  IF NOT L_EXIT IS INITIAL.
    LEAVE.
  ENDIF.
ENDFORM.



* Zuordnung von anderen Berichten
FORM BERICHT_ZUORDNEN.
  DATA: BEGIN OF REPORT.
          INCLUDE STRUCTURE RSTIREC.
  DATA: END OF REPORT.

  REPORT-RTOOL = 'RT'.
  REPORT-RONAM = SY-REPID.

  CALL FUNCTION 'RSTI_REPORT_ADD'
       EXPORTING
            E_FCCLS  = '9'
            E_REPORT = REPORT
            E_RECSEN = 'S'.
*     exceptions
*          no_authority        = 1
*          wrong_function_call = 2
*          others              = 3.
ENDFORM.


*---------------------------------------------------------------------*
*       FORM BBS_VERARBEITEN                                          *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
FORM BBS_VERARBEITEN.
  DATA: HLP_LINES LIKE SY-INDEX.
  DATA: HLP_IONRA LIKE IONRA.


* Verarbeitung der übergebenen Projekte. Konvertierung von externen
* zur internen Projektnummer.
  REFRESH POSNR.

  LOOP AT POSID.
    POSNR-SIGN = POSID-SIGN.
    POSNR-OPTION = POSID-OPTION.

    CALL FUNCTION 'CONVERSION_EXIT_KONPR_INPUT'
         EXPORTING
              INPUT     = POSID-LOW
         IMPORTING
              OUTPUT    = POSNR-LOW
*         PRPSWA    =
         EXCEPTIONS
              NOT_FOUND = 1
              OTHERS    = 2.

    CALL FUNCTION 'CONVERSION_EXIT_KONPR_INPUT'
         EXPORTING
              INPUT     = POSID-HIGH
         IMPORTING
              OUTPUT    = POSNR-HIGH
*         PRPSWA    =
         EXCEPTIONS
              NOT_FOUND = 1
              OTHERS    = 2.

    APPEND POSNR.
  ENDLOOP.

* Verarbeitung von allgemeiner Objektnummer
  LOOP AT OBJNR.

    CALL FUNCTION 'OBJECT_IDENTIFICATION_GET'
         EXPORTING
              OBJNR   = OBJNR-LOW
         IMPORTING
              E_IONRA = HLP_IONRA.

    CASE HLP_IONRA-OBART.
      WHEN 'OR'.
        EAUFN-SIGN = 'I'.
        EAUFN-OPTION = 'EQ'.
        EAUFN-LOW = HLP_IONRA-AUFNR.
        APPEND EAUFN.

      WHEN 'PR'.
        POSNR-SIGN = 'I'.
        POSNR-OPTION = 'EQ'.
        POSNR-LOW = HLP_IONRA-PSPNR.
        APPEND POSNR.
    ENDCASE.
  ENDLOOP.

* Parameter und Selektionsoptionen für Simulation clearen - sonst doppel
  IF SY-SUBRC EQ 0.
    REFRESH SO_PSPNR.
    CLEAR PA_POSID.
  ENDIF.

  DESCRIBE TABLE EAUFN LINES HLP_LINES.
  IF HLP_LINES GT 0.
    REFRESH SO_EAUFN.
  ENDIF.

* Buchungskreis = SPACE eliminieren.
  LOOP AT BUKRS.
    IF BUKRS-SIGN = 'I' AND BUKRS-OPTION = 'EQ'
                        AND BUKRS-LOW IS INITIAL.
      DELETE BUKRS.
    ENDIF.
  ENDLOOP.
ENDFORM.

*---------------------------------------------------------------------*
*       FORM RSTI_SELECTION_EXIT                                      *
*---------------------------------------------------------------------*
*       ........                                                      *
*---------------------------------------------------------------------*
*  -->  IT_FIELDRM                                                    *
*  -->  FLG_OWN_LOGIC                                                 *
*---------------------------------------------------------------------*
FORM RSTI_SELECTION_EXIT TABLES IT_FIELDRM STRUCTURE RSTIFIELDS
                        USING FLG_OWN_LOGIC.

* Bereich für BBS
  IT_FIELDRM-RFIELD = 'BEREICH1'.
  IT_FIELDRM-TRFLG = 'I'.

  IT_FIELDRM-DOMNAME = 'AFABE'.
  APPEND IT_FIELDRM.
* SUMMB
  IT_FIELDRM-RFIELD = 'SUMMB'.
  IT_FIELDRM-TRFLG = 'I'.

  IT_FIELDRM-DOMNAME = 'SUMMB'.
  APPEND IT_FIELDRM.

* XEINZL
  IT_FIELDRM-RFIELD = 'XEINZEL'.
  IT_FIELDRM-TRFLG = 'I'.

  IT_FIELDRM-DOMNAME = 'XEINZL'.
  APPEND IT_FIELDRM.

ENDFORM.
*$*$--------- End of INCLUDE RASORT40 -------( NO UPDATE )-------$*$*


FORM DATEN_AUSGEBEN.

* Hitliste.
  IF NOT PA_HITLI IS INITIAL.
*   Datensatz-Nummer > angeforderte Anzahl ...
    IF CNT_HITLI GT PA_HITLI.
*     ... dann nix ausgeben.
      EXIT.
    ENDIF.
  ENDIF.

  RESERVE 2 LINES.

  WRITE: /001      SY-VLINE NO-GAP,
          002(12)  ANLAV-ANLN0 NO-GAP
                   COLOR COL_KEY INTENSIFIED ON,
          014(01)  SPACE NO-GAP
                   COLOR COL_KEY INTENSIFIED ON,
          015(04)  ANLAV-ANLN2 NO-GAP
                   COLOR COL_KEY INTENSIFIED ON,
          019      SY-VLINE NO-GAP,
          020(10)  ANLAV-AKTIV DD/MM/YYYY NO-GAP
                   COLOR COL_KEY INTENSIFIED OFF,
          030(01)  SPACE NO-GAP
                   COLOR COL_KEY INTENSIFIED OFF,
          031(50)  ANLAV-TXT50 NO-GAP
                   COLOR COL_KEY INTENSIFIED OFF,
          081      SY-VLINE NO-GAP,
          082(15)  X-KANSW  CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED OFF,
          098(15)  X-KUMAFA CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED OFF,
          114(15)  X-BCHWRT CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED OFF,
          130      SY-VLINE NO-GAP.
* Informationen fuer PickUp.
  FLG_PICK_UP = 'X'.
* PickUp auf Anlage ist möglich.

  HIDE:           ANLAV-BUKRS,
                  ANLAV-ANLN1,
                  ANLAV-ANLN2,
                  ANLAV-XANLGR,
                  FLG_PICK_UP,
                  RANGE.

  CLEAR FLG_PICK_UP.

ENDFORM.


FORM SUMME_AUSGEBEN USING SUM_STARX
                          SUM_FTEXT
                          SUM_FINHA
                          SUM_FBEZ.

* Summenbericht?
  IF SUMMB NE SPACE.

*   Summentabelle erfrischen.
    REFRESH SWRT.
    CLEAR SWRT.
*   Uebergabe der Summen in der Reihenfolge ihrer Ausgabe.
    WRITE SUM(X-KANSW)  TO SWRT-BETRG
      CURRENCY SAV_WAER1.
    APPEND SWRT.
    WRITE SUM(X-KUMAFA) TO SWRT-BETRG
      CURRENCY SAV_WAER1.
    APPEND SWRT.
    WRITE SUM(X-BCHWRT) TO SWRT-BETRG
      CURRENCY SAV_WAER1.
    APPEND SWRT.
*
    PERFORM GRUSUMME_AUSGEBEN.

* Kein Summenbericht.
  ELSE.

*   Hitliste?
    IF NOT PA_HITLI IS INITIAL.

*     Dann Summe ueber Hitliste zuerst ausgeben.
      RESERVE 2 LINES.

      WRITE: /001      SY-VLINE,
              002(05)  TEXT-H02 NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              007(12)  SPACE NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              019      SY-VLINE,
              020(05)  PA_HITLI NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              025(16)  SPACE NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              041(20)  TEXT-H01 NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              061(15)  SPACE NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              076(05)  '    *' NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              081      SY-VLINE NO-GAP,
              082(15)  HITSUM-BETRG1 CURRENCY SAV_WAER1 NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              098(15)  HITSUM-BETRG2 CURRENCY SAV_WAER1 NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              114(15)  HITSUM-BETRG3 CURRENCY SAV_WAER1 NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              130      SY-VLINE NO-GAP.
       ULINE.
    ENDIF.

    RESERVE 2 LINES.

    WRITE: /001      SY-VLINE NO-GAP,
            002(17)  SUM_FTEXT NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            019      SY-VLINE NO-GAP,
            020(20)  SUM_FINHA NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            040(01)  SPACE NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            041(34)  SUM_FBEZ NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            075(01)  SPACE NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            076(05)  SUM_STARX NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            081      SY-VLINE NO-GAP,
            082(15)  SUM(X-KANSW)  CURRENCY SAV_WAER1 NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            098(15)  SUM(X-KUMAFA) CURRENCY SAV_WAER1 NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            114(15)  SUM(X-BCHWRT) CURRENCY SAV_WAER1 NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            130      SY-VLINE NO-GAP.
    ULINE.
  ENDIF.

ENDFORM.


FORM ANLN1_SUMME_AUSGEBEN.

* Feld für Sternchen bei Summe
DATA: STARORSPACE(1)  TYPE C    VALUE ' '.

  IF T086-XLEERZL = 'X'.
    STARORSPACE = '*'.
    RESERVE 3 LINES.
  ELSE.
    RESERVE 2 LINES.
  ENDIF.

  WRITE: /001      SY-VLINE NO-GAP,
          002(12)  ANLAV-ANLN0 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          014(01)  SPACE NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          015(04)  CON_ANLN1_SUM NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          019      SY-VLINE NO-GAP,
          020(11)  SPACE NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          031(50)  ANLAV-ANLHTXT NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          081      SY-VLINE NO-GAP,
          082(15)  SUM(X-KANSW)  CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          097      STARORSPACE NO-GAP,
          098(15)  SUM(X-KUMAFA) CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          113      STARORSPACE NO-GAP,
          114(15)  SUM(X-BCHWRT) CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          129      STARORSPACE NO-GAP,
          130      SY-VLINE NO-GAP.

  CHECK T086-XLEERZL = 'X'.

  WRITE: /001      SY-VLINE NO-GAP,
          002(12)  SPACE    NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          014(01)  SPACE    NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          015(04)  SPACE    NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          019      SY-VLINE NO-GAP,
          020(11)  SPACE    NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          031(50)  SPACE    NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          081      SY-VLINE NO-GAP,
          082(15)  SPACE    NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          098(15)  SPACE    NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          114(15)  SPACE    NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          130      SY-VLINE NO-GAP.

ENDFORM.


FORM UEBERSCHRIFTEN_AUSGEBEN.

  WRITE: /001      SY-VLINE NO-GAP,
          002(12)  TEXT-001 NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          014(01)  SPACE NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          015(04)  TEXT-002 NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          019      SY-VLINE NO-GAP,
          020(10)  TEXT-003 NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          030(01)  SPACE NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          031(50)  TEXT-004 NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          081      SY-VLINE NO-GAP,
          082(14)  TEXT-W01 NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          098(14)  TEXT-W02 NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          114(14)  TEXT-W03 NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          130      SY-VLINE NO-GAP.
  ULINE.

ENDFORM.


FORM WERTE_BERECHNEN.

* Kumulierter Anschaffungswert einschliesslich Inv-Zus und Aufw.
  X-KANSW       = ANLCV-ANSW_GJE.


* Kumulierte Gesamt-AfA einschliesslich Aufw N-AfA.
  X-KUMAFA      = ANLCV-AFA_GJE.

* Buchwert GJ-Ende.
  X-BCHWRT      = ANLCV-BCHWRT_GJE.

* Sortier-Wertfelder.
  SORT-KANSW    = X-KANSW.
  SORT-KUMAFA   = 0           - X-KUMAFA.
  SORT-BCHWRT   = X-BCHWRT.

ENDFORM.
