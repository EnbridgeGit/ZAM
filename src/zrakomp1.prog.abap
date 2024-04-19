REPORT RAKOMP01 MESSAGE-ID AB
                LINE-SIZE 131
                NO STANDARD PAGE HEADING.

TABLES: ANLH,
        ANLA0,
        ANLAV,
        ANLB,
        ANEK,
        ANEPV,
        ANLCV.

* FIELDS-Anweisungen.
INCLUDE RASORT00.

* Allgemeine DATA-, TABLES-, ... Anweisungen.
INCLUDE RASORT04.

FIELD-GROUPS: HEADER, DATEN.

DATA: LD_KANSW LIKE ANLC-KANSW.  "insert SAP

DATA:
*     Anzahl der im Anforderungsbild erlaubten AfA-Bereiche.
      SAV_ANZBE(1) TYPE C VALUE '1',
*     Flag: Postenausgabe Ja='1'/Nein='0'.
      FLG_POSTX(1) TYPE C VALUE '0',
*     Summenbericht: Maximale Anzahl Wertfelder/Zeile.
      CON_WRTZL(2) TYPE P VALUE 4.

* Ausgabe-Wertfelder.
DATA: BEGIN OF X,
*       Kumulierter Anschaffungswert einschliesslich Inv-Zus und Aufw.
        KANSW       LIKE ANLCV-ANSW_GJE,
*       Statistischer Anschaffungswert
        KSANS       LIKE ANLCV-KSANS,
*       Kumulierte Gesamt-AfA einschliesslich Aufw N-AfA.
        KUMAFA      LIKE RAREP-KUMAFA,
*       Laufende Afa des Jahres
        LFDAFA      LIKE RAREP-KUMAFA,
*       Restbuchwert einschliesslich Inv-Zus und Aufw.
        BCHWRT      LIKE ANLCV-BCHWRT_GJE,
      END OF X.

* Ausgabe-Wertfelder für Anlagenkomplex
DATA: BEGIN OF Y,
*       Kumulierter Anschaffungswert einschliesslich Inv-Zus und Aufw.
        KANSW       LIKE ANLCV-ANSW_GJE,
*       Kumulierte Gesamt-AfA einschliesslich Aufw N-AfA.
        KUMAFA      LIKE RAREP-KUMAFA,
*       Laufende Afa des Jahres
        LFDAFA      LIKE RAREP-KUMAFA,
*       Restbuchwert einschliesslich Inv-Zus und Aufw.
        BCHWRT      LIKE ANLCV-BCHWRT_GJE,
      END OF Y.

* Sortier-Wertfelder.
DATA: BEGIN OF SORT,
*       Kumulierter Anschaffungswert einschliesslich Inv-Zus und Aufw.
        KANSW       LIKE ANLCV-ANSW_GJE,
*       Statistischer Anschaffungswert
        KSANS       LIKE ANLCV-KSANS,
*       Kumulierte Gesamt-AfA einschliesslich Aufw N-AfA.
        KUMAFA      LIKE RAREP-KUMAFA,
*       Restbuchwert einschliesslich Inv-Zus und Aufw.
        BCHWRT      LIKE ANLCV-BCHWRT_GJE,
      END OF SORT.
* Hilfsfelder für Verteilung
DATA:
  SUM_KUMAFA  LIKE X-KUMAFA,
  SUM_LFDAFA  LIKE X-LFDAFA,
  SUM_BCHWRT  LIKE X-BCHWRT.

DATA: BEGIN OF KOMPL OCCURS 100,
         ANLGR LIKE ANLB-ANLGR,
         ANLGR2 LIKE ANLB-ANLGR2.
         INCLUDE STRUCTURE ANLAV.
DATA:    AFASL LIKE ANLB-AFASL,
         AFABG LIKE ANLB-AFABG,
         SAFBG LIKE ANLB-SAFBG,
         NDJAR LIKE ANLB-NDJAR,
         NDPER LIKE ANLB-NDPER,
         SAV_WAER1 LIKE SAV_WAER1,
         RANGE     LIKE RANGE.
         INCLUDE STRUCTURE Y.
DATA:    LAST TYPE C,
      END OF KOMPL.

SELECTION-SCREEN BEGIN OF BLOCK BL1                        "AB
                 WITH FRAME                                "AB
                 TITLE TEXT-BL1.                           "AB

  SELECT-OPTIONS:
*               Anlagenbestandkonto.
                SO_KTANW FOR ANLAV-KTANSW NO DATABASE SELECTION,
*               Aktivierungsdatum.
                SO_AKTIV FOR ANLAV-AKTIV NO DATABASE SELECTION,
*               Aktueller Anschaffungswert.
                SO_KANSW FOR X-KANSW,
*               Kumulierte gesamte AfA.
                SO_KUMAF FOR X-KUMAFA,
*               Aktueller Buchwert.
                SO_BCHWR FOR X-BCHWRT.
SELECTION-SCREEN END   OF BLOCK BL1.                       "AB

SELECTION-SCREEN SKIP.

SELECTION-SCREEN BEGIN OF BLOCK BL3                        "AB
                 WITH FRAME                                "AB
                 TITLE TEXT-C02.                           "AB
  PARAMETERS:
*             Keine zusaetzlich Sortierung.                "AB
              PA_SRTW0 LIKE RAREP-SRTWRT                   "AB
                       RADIOBUTTON GROUP RAD1,             "AB
*           Zusaetzlich Sortierung nach Anschaffungswert.
              PA_SRTW1 LIKE RAREP-SRTWRT
                       RADIOBUTTON GROUP RAD1,             "AB
*           Zusaetzliche Sortierung nach kumulierter AfA.
              PA_SRTW2 LIKE RAREP-SRTWRT
                       RADIOBUTTON GROUP RAD1,             "AB
*           Zusaetzlich Sortierung nach Buchwert.
              PA_SRTW3 LIKE RAREP-SRTWRT
                       RADIOBUTTON GROUP RAD1,             "AB
*           Hitliste: Top nnnnn.
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
*           Zusatzueberschrift.
            PA_TITEL LIKE RAREP-TITEL DEFAULT SPACE,
*           Flag: Listseparation gemaess Tabelle TLSEP.
            PA_LSSEP LIKE BHDGD-SEPAR,
*           Flag: Mikrofichezeile ausgeben.
            PA_MIKRO LIKE BHDGD-MIFFL.
SELECTION-SCREEN END   OF BLOCK BL4.                       "AB

INITIALIZATION.
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

* Summenbericht: Ueberschriften der Wertfelder.
IF SUMMB NE SPACE.
  WRITE  TEXT-W01 TO SFLD-FNAME.
  APPEND SFLD.
  WRITE  TEXT-W02 TO SFLD-FNAME.
  APPEND SFLD.
  WRITE  TEXT-W03 TO SFLD-FNAME.
  APPEND SFLD.
  WRITE  TEXT-W04 TO SFLD-FNAME.
  APPEND SFLD.
  WRITE  TEXT-W05 TO SFLD-FNAME.
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
  ASSIGN X-LFDAFA TO <H3>.
  ASSIGN X-BCHWRT TO <H4>.
  ASSIGN X-KSANS  TO <H5>.
* Nicht benoetigte Summenfelder.
  ASSIGN SAV_WDUMMY TO: <H6>, <H7>, <H8>, <H9>, <H10>,
    <H11>, <H12>, <H13>, <H14>, <H15>, <H16>, <H17>, <H18>, <H19>,
    <H20>.
ENDIF.

* Bestimmung des Sortierfeldes fuer Gitterposition oder Einzelposten.
ASSIGN ANLAV-ANLN1 TO <P>.
ASSIGN SAV_DUMMY TO <Q>.

* Steuerungskennzeichen für LDB setzen
     *ANLA0-XAKPL = 'X'.
* Allgemeines Coding nach START-OF-SELECTION. Aufbau des HEADERs.
INCLUDE RASORT10.

INSERT
*        Daten zur Anlage.
         ANLAV-ANLN0
         ANLAV-ANLN1        ANLAV-ANLN2        ANLAV-WERKS
         ANLAV-TXT50        ANLAV-KOSTL        ANLAV-MENGE
         ANLAV-POSNR        ANLAV-AKTIV        ANLAV-LIFNR
         ANLAV-MEINS        ANLAV-STORT        ANLAV-TXA50
         ANLAV-ANLHTXT      ANLAV-XANLGR
*        Daten zum AfA-Bereich.
         ANLB-AFASL         ANLB-AFABG         ANLB-SAFBG
         ANLB-NDJAR         ANLB-NDPER         SAV_WAER1
*        Wertfelder.
         X-KANSW            X-KUMAFA           X-BCHWRT
         X-KSANS            X-LFDAFA
         INTO DATEN.

*        Daten zur Anlage.

GET ANLA0.


GET ANLAV.
   IF ANLAV-MENGE IS INITIAL.
   CLEAR ANLAV-MEINS.
   ENDIF.
* Nur Komplexe abfragen
   IF NOT ANLAV-XANLGR IS INITIAL.
     CHECK SELECT-OPTIONS.
   ENDIF.
* Nur Anlagen seleketieren, die aktiviert wurden ...
  CHECK NOT ANLAV-ZUGDT IS INITIAL.
* ... und zwar vor dem Berichtsdatum.
  CHECK     ANLAV-ZUGDT LE BERDATUM.

* Verarbeitungen ON CHANGE OF ANLAV-XXXXX.
  INCLUDE RASORT14.

* Im VJ deaktivierte Anlagen nicht selektieren.
  IF NOT ANLAV-DEAKT IS INITIAL.
    CHECK ANLAV-DEAKT GE SAV_GJBEG.
  ENDIF.

  ON CHANGE OF ANLAV-BUKRS.
*   Individueller Teil des Headers
    WRITE: '-'       TO HEAD-INFO1,
           BEREICH1  TO HEAD-INFO2,
           SAV_AFBE1 TO HEAD-INFO3.
*
    CONDENSE HEAD.
  ENDON.


GET ANLB.

   IF ANLB-ANLGR IS INITIAL.
     CHECK SELECT-OPTIONS.
   ENDIF.

GET ANLCV.
   IF ANLB-ANLGR IS INITIAL.
     CHECK SELECT-OPTIONS.
   ENDIF.

  PERFORM FEHLER_AUSGEBEN.

* Werte berechnen.
  PERFORM WERTE_BERECHNEN.
  IF ANLB-ANLGR IS INITIAL.
    CHECK SO_KANSW.
    CHECK SO_KUMAF.
    CHECK SO_BCHWR.
  ENDIF.
* Daten gegen Sortierwerte beim PickUp checken.
  IF NOT ANLAV-XANLGR IS INITIAL.
    RANGE = '1'.
    PERFORM SORT_CHECK.
* DATEN extrahieren.
   EXTRACT DATEN.
  ELSEIF NOT ANLB-ANLGR IS INITIAL.
    MOVE-CORRESPONDING ANLAV TO KOMPL.
    MOVE-CORRESPONDING ANLB TO KOMPL.
    MOVE-CORRESPONDING Y TO KOMPL.
    KOMPL-RANGE = '1'.
    APPEND KOMPL.
  ENDIF.

END-OF-SELECTION.

*---------------------------------------------------------------------*

* Bestand sortieren.
INCLUDE RASORT20.
SORT KOMPL BY ANLGR ANLGR2 ANLN1.

* Die letzten Anlagen eines Komplexes als solche kennzeichnen
PERFORM LETZTE_ANLAGE_MARKIEREN TABLES KOMPL.

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
       READ TABLE KOMPL WITH KEY ANLGR = ANLAV-ANLN1
                                 ANLGR2 = ANLAV-ANLN2 BINARY SEARCH.
       IF SY-SUBRC EQ 0.
         CLEAR LD_KANSW.                "<<<< insert  SAP
         LOOP AT KOMPL FROM SY-INDEX
                       WHERE ANLGR = ANLAV-ANLN1
                       AND   ANLGR2 = ANLAV-ANLN2.
           LD_KANSW = LD_KANSW + KOMPL-KANSW.  "<<<< insert SAP
           PERFORM KOMPL_AUSGEBEN.
         ENDLOOP.
           WRITE:/ 'cumulated acquisition value:', LD_KANSW.  "<< SAP
       ENDIF.
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
INCLUDE RASORT40.


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
          031(35)  ANLAV-TXT50 NO-GAP
                   COLOR COL_KEY INTENSIFIED OFF,
          066      SY-VLINE NO-GAP,
          067(15)  X-KANSW  CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED OFF,
          083(15)  X-KUMAFA CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED OFF,
          099(15)  X-LFDAFA CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED OFF,
          115(15)  X-BCHWRT CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED OFF,
          131      SY-VLINE NO-GAP.
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
  WRITE: /001      SY-VLINE NO-GAP,
          002(12)  SPACE       NO-GAP
                   COLOR COL_KEY INTENSIFIED ON,
          014(01)  SPACE NO-GAP
                   COLOR COL_KEY INTENSIFIED ON,
          015(04)  SPACE       NO-GAP
                   COLOR COL_KEY INTENSIFIED ON,
          019      SY-VLINE NO-GAP,
          020(10)  SPACE                  NO-GAP
                   COLOR COL_KEY INTENSIFIED OFF,
          030(01)  SPACE NO-GAP
                   COLOR COL_KEY INTENSIFIED OFF,
          031(35)  SPACE       NO-GAP
                   COLOR COL_KEY INTENSIFIED OFF,
          066      SY-VLINE NO-GAP,
          067(15)  X-KSANS  CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED OFF,
          083(15)  SPACE    CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED OFF,
          099(15)  SPACE    CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED OFF,
          115(15)  SPACE    CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED OFF,
          131      SY-VLINE NO-GAP.
  ULINE.
ENDFORM.

FORM KOMPL_AUSGEBEN.
  DATA: HLP_QUOT TYPE F.

* Hitliste.
  IF NOT PA_HITLI IS INITIAL.
*   Datensatz-Nummer > angeforderte Anzahl ...
    IF CNT_HITLI GT PA_HITLI.
*     ... dann nix ausgeben.
      EXIT.
    ENDIF.
  ENDIF.
* Verteilung der Anlagen, die nicht die letzten sind anteilsmäßig
  IF KOMPL-LAST IS INITIAL.
    HLP_QUOT = KOMPL-KANSW / ( X-KANSW + X-KSANS ).
    KOMPL-KUMAFA = X-KUMAFA * HLP_QUOT.
    KOMPL-LFDAFA = X-LFDAFA * HLP_QUOT.
    KOMPL-BCHWRT = X-BCHWRT * HLP_QUOT.
    ADD KOMPL-KUMAFA TO SUM_KUMAFA.
    ADD KOMPL-LFDAFA TO SUM_LFDAFA.
    ADD KOMPL-BCHWRT TO SUM_BCHWRT.
  ELSE.
*  letze Anlage bekommt den Rest
    KOMPL-KUMAFA = X-KUMAFA - SUM_KUMAFA.
    KOMPL-LFDAFA = X-LFDAFA - SUM_LFDAFA.
    KOMPL-BCHWRT = X-BCHWRT - SUM_BCHWRT.
    CLEAR: SUM_KUMAFA, SUM_LFDAFA, SUM_BCHWRT.
  ENDIF.

  RESERVE 2 LINES.

  WRITE: /001      SY-VLINE NO-GAP,
          002(12)  KOMPL-ANLN0 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED ON,
          014(01)  SPACE NO-GAP
                   COLOR COL_NORMAL INTENSIFIED ON,
          015(04)  KOMPL-ANLN2 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED ON,
          019      SY-VLINE NO-GAP,
          020(10)  KOMPL-AKTIV DD/MM/YYYY NO-GAP
                   COLOR COL_NORMAL INTENSIFIED ON,
          030(01)  SPACE NO-GAP
                   COLOR COL_NORMAL INTENSIFIED ON,
          031(35)  KOMPL-TXT50 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED ON,
          066      SY-VLINE NO-GAP,
          067(15)  KOMPL-KANSW  CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED ON,
          083(15)  KOMPL-KUMAFA CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED ON,
          099(15)  KOMPL-LFDAFA CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED ON,
          115(15)  KOMPL-BCHWRT CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_NORMAL INTENSIFIED ON,
          131      SY-VLINE NO-GAP.
* Informationen fuer PickUp.
  FLG_PICK_UP = 'X'.
* PickUp auf Anlage ist möglich.
  MOVE KOMPL-BUKRS TO ANLAV-BUKRS.
  MOVE KOMPL-ANLN1 TO ANLAV-ANLN1.
  MOVE KOMPL-ANLN2 TO ANLAV-ANLN2.
  MOVE KOMPL-RANGE TO RANGE.
  CLEAR ANLAV-XANLGR.
  HIDE:           ANLAV-BUKRS,
                  ANLAV-ANLN1,
                  ANLAV-ANLN2,
                  ANLAV-XANLGR,
                  FLG_PICK_UP,
                  RANGE.

  CLEAR FLG_PICK_UP.

* Unterstrich am Ende.
  CHECK KOMPL-LAST EQ 'X'.
  ULINE.
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
    WRITE SUM(X-LFDAFA) TO SWRT-BETRG
      CURRENCY SAV_WAER1.
    APPEND SWRT.
    WRITE SUM(X-BCHWRT) TO SWRT-BETRG
      CURRENCY SAV_WAER1.
    APPEND SWRT.
    WRITE SUM(X-KSANS) TO SWRT-BETRG
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
              025(06)  SPACE NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              031(20)  TEXT-H01 NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              051(10)  SPACE                  NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              061(05)  '    *' NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              066      SY-VLINE NO-GAP,
              067(15)  HITSUM-BETRG1 CURRENCY SAV_WAER1 NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              083(15)  HITSUM-BETRG2 CURRENCY SAV_WAER1 NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              099(15)  HITSUM-BETRG3 CURRENCY SAV_WAER1 NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              115(15)  HITSUM-BETRG4 CURRENCY SAV_WAER1 NO-GAP
                       COLOR COL_TOTAL INTENSIFIED ON,
              131      SY-VLINE NO-GAP.
    WRITE: /001      SY-VLINE NO-GAP,
            002(12)  SPACE       NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            014(01)  SPACE NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            015(04)  SPACE       NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            019      SY-VLINE NO-GAP,
            020(10)  SPACE                  NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            030(01)  SPACE NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            031(35)  SPACE       NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            066      SY-VLINE NO-GAP,
            067(15)  HITSUM-BETRG5 CURRENCY SAV_WAER1 NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            083(15)  SPACE    CURRENCY SAV_WAER1 NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            099(15)  SPACE    CURRENCY SAV_WAER1 NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            115(15)  SPACE    CURRENCY SAV_WAER1 NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            131      SY-VLINE NO-GAP.
       ULINE.
    ENDIF.

    RESERVE 2 LINES.

    WRITE: /001      SY-VLINE NO-GAP,
            002(17)  SUM_FTEXT NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            019      SY-VLINE NO-GAP,
            020(10)  SUM_FINHA NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            030(01)  SPACE NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            031(29)  SUM_FBEZ NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            060(01)  SPACE NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            061(05)  SUM_STARX NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            066      SY-VLINE NO-GAP,
            067(15)  SUM(X-KANSW)  CURRENCY SAV_WAER1 NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            083(15)  SUM(X-KUMAFA)  CURRENCY SAV_WAER1 NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            099(15)  SUM(X-LFDAFA) CURRENCY SAV_WAER1 NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            115(15)  SUM(X-BCHWRT) CURRENCY SAV_WAER1 NO-GAP
                     COLOR COL_TOTAL INTENSIFIED ON,
            131      SY-VLINE NO-GAP.
  WRITE: /001      SY-VLINE NO-GAP,
          002(12)  SPACE       NO-GAP
                   COLOR COL_TOTAL INTENSIFIED ON,
          014(01)  SPACE NO-GAP
                   COLOR COL_TOTAL INTENSIFIED ON,
          015(04)  SPACE       NO-GAP
                   COLOR COL_TOTAL INTENSIFIED ON,
          019      SY-VLINE NO-GAP,
          020(10)  SPACE                  NO-GAP
                   COLOR COL_TOTAL INTENSIFIED ON,
          030(01)  SPACE NO-GAP
                   COLOR COL_TOTAL INTENSIFIED ON,
          031(35)  SPACE       NO-GAP
                   COLOR COL_TOTAL INTENSIFIED ON,
          066      SY-VLINE NO-GAP,
          067(15)  SUM(X-KSANS) CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED ON,
          083(15)  SPACE    CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED ON,
          099(15)  SPACE    CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED ON,
          115(15)  SPACE    CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED ON,
          131      SY-VLINE NO-GAP.
    ULINE.
  ENDIF.

ENDFORM.


FORM ANLN1_SUMME_AUSGEBEN.

* Feld für Sternchen bei Summe
DATA: STARORSPACE(1)  TYPE C    VALUE ' '.

IF T086-XLEERZL = 'X'.
  STARORSPACE = '*'.
ENDIF.

  RESERVE 3 LINES.

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
          031(35)  ANLAV-ANLHTXT NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          066      SY-VLINE NO-GAP,
          067(15)  SUM(X-KANSW)  CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          082      STARORSPACE NO-GAP,
          083(15)  SUM(X-KUMAFA)  CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          098      STARORSPACE NO-GAP,
          099(15)  SUM(X-LFDAFA) CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          114      STARORSPACE NO-GAP,
          115(15)  SUM(X-BCHWRT) CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          130      STARORSPACE NO-GAP,
          131      SY-VLINE NO-GAP.
  WRITE: /001      SY-VLINE NO-GAP,
          002(12)  SPACE       NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          014(01)  SPACE NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          015(04)  SPACE       NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          019      SY-VLINE NO-GAP,
          020(10)  SPACE                  NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          030(01)  SPACE NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          031(35)  SPACE       NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          066      SY-VLINE NO-GAP,
          067(15)  SUM(X-KSANS) CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          082      STARORSPACE NO-GAP,
          083(15)  SPACE    CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          099(15)  SPACE    CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          115(15)  SPACE    CURRENCY SAV_WAER1 NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          131      SY-VLINE NO-GAP.

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
          031(35)  SPACE    NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          066      SY-VLINE NO-GAP,
          067(15)  SPACE    NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          083(15)  SPACE    NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          099(15)  SPACE    NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          115(15)  SPACE    NO-GAP
                   COLOR COL_TOTAL INTENSIFIED OFF,
          131      SY-VLINE NO-GAP.

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
          031(35)  TEXT-004 NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          066(01)  SY-VLINE NO-GAP,
          067(14)  TEXT-W01 NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          083(14)  TEXT-W02 NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          099(14)  TEXT-W04 NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          115(14)  TEXT-W03 NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          131      SY-VLINE NO-GAP.
  WRITE: /001      SY-VLINE NO-GAP,
          002(12)  SPACE    NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          014(01)  SPACE NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          015(04)  SPACE    NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          019      SY-VLINE NO-GAP,
          020(10)  SPACE    NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          030(01)  SPACE NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          031(35)  SPACE    NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          066(01)  SY-VLINE NO-GAP,
          067(14)  TEXT-W05 NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          083(14)  SPACE    NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          099(14)  SPACE    NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          115(14)  SPACE    NO-GAP
                   COLOR COL_HEADING INTENSIFIED ON,
          131      SY-VLINE NO-GAP.


  ULINE.

ENDFORM.


FORM WERTE_BERECHNEN.

* Kumulierter Anschaffungswert einschliesslich Inv-Zus und Aufw.
  X-KANSW       = ANLCV-ANSW_GJE.

* Kumulierter statistischer Anschaffungswert
  X-KSANS       =  ANLCV-KSANS + ANLCV-SANSL.

* Kumulierte Gesamt-AfA einschliesslich Aufw N-AfA.
  X-KUMAFA      = ANLCV-AFA_GJA.

* Afa des lfd. Jahres
  X-LFDAFA      = ANLCV-AFA_LFDP.

* Buchwert GJ-Ende.
  X-BCHWRT      = ANLCV-BCHWRT_GJE.

* Sortier-Wertfelder.
  SORT-KANSW    = X-KANSW.
  SORT-KUMAFA   = 0           - X-KUMAFA - X-LFDAFA.
  SORT-BCHWRT   = X-BCHWRT.

* Y-KUMAFA = X-KUMAFA.
  Y-KANSW = X-KANSW.
* Y-BCHWRT = X-BCHWRT.
* y-lfdafa = x-lfdafa.
ENDFORM.


FORM LETZTE_ANLAGE_MARKIEREN TABLES KOMPL STRUCTURE KOMPL.
  DATA: HLP_ANLGR(16) TYPE C,
        HLP_ANLGR_NEU(16) TYPE C,
        HLP_LINE   LIKE SY-INDEX,
        HLP_FLG  TYPE C.

* READ TABLE KOMPL INDEX 1.
* HLP_ANLGR = KOMPL-ANLGR.
* HLP_ANLGR+13 = KOMPL-ANLGR2.
* LOOP AT KOMPL.
*   HLP_ANLGR_NEU = KOMPL-ANLGR.
*   HLP_ANLGR_NEU+13 = KOMPL-ANLGR2.
*   IF HLP_ANLGR NE HLP_ANLGR_NEU.
*     KOMPL-LAST = 'X'.
*     MODIFY KOMPL.
*     HLP_ANLGR = HLP_ANLGR_NEU.
*   ENDIF.
* ENDLOOP.
* DESCRIBE TABLE KOMPL LINES HLP_LINE.
* READ TABLE KOMPL INDEX HLP_LINE.
* KOMPL-LAST = 'X'.
* MODIFY KOMPL INDEX HLP_LINE.
  LOOP AT KOMPL.
    AT END OF ANLGR2.
       HLP_FLG = 'X'.
    ENDAT.
    IF HLP_FLG EQ 'X'.
      KOMPL-LAST = 'X'.
      MODIFY KOMPL.
      CLEAR HLP_FLG.
    ENDIF.
  ENDLOOP.
ENDFORM.
