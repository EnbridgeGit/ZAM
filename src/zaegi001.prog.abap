REPORT zaegi001 LINE-SIZE  79
                LINE-COUNT 59
                MESSAGE-ID zs.

*----------------------------------------------------------------------
* Program    : ZAEGI001
* Created  On: December 20, 1996.
* Created  By: Joanne Lee (Omnilogic Systems Group)
*         and: Marv Radsma (MR Consulting)
*
* Changed by: Mohammad T. Khan
* Changed on: August 16, 2001.
* Reason:  Accomodate diferent file format due to the new MISOS
*          system. (mainframe elimination).
*----------------------------------------------------------------------
* This programs uploads changes from a flat file on UNIX to update
* the station ID information on SAP.  The station ID will always be
* 8 characters and the description will be a maximum of 30 characters.
* The structure of the inputting data is the 8 character station ID,
* followed by a comma, followed by the 30 character description.
* i.e.  XXXXXXXX,YYYYYYYYYYYYYYYYYYYYYYYYYYYYYY
*
* The default file to get the data from is:
*     Logical File:  ZAEGI001_01
*     Physical File: /usr/sap/interfaces/D30/IFAM003/ZAEGI001.SAP (dev)
*     Physical File: /usr/sap/interfaces/P01/IFAM003/ZAEGI001.SAP (prod)
*
* This programs extracts the columns 1 to 8 as station ID, then
* extracts columns 9 to 39 as station description. Any other colummns
* are ignored.
*
* The procedure taken to update the data is to:
* (1) read a record from the input flat file.
* (2) select a record from the table with a matching station ID.
* (3) if the record is not found, then add it to the table.
* (4) if the record is found and the description is different, then
*     use the pull down menus 'Goto - other entry' and 'Edit - change
*     fld conts' to change the station description.
* (5) do nothing if the record is found and the descriptions match.
* (6) a dummy station id of 99999999 is maintained as the last record
*     of the group assets table.
*
* How To Run This Program:
*    Please make sure that this program is executed in BACKGROUND.
*
* Note on Centra data:
*    The data from centra arrives in Excel format which must be saved
*    in comma delimited form using the 'save as' function from the
*    'file' pulldown menu.  The title and all columns but the ID and
*    description must be deleted.  Now ftp the file (without the 'bin'
*    option to the directory specified above and rename the file to
*    ZAEGI001.SAP.  Now the program can be run to load Centra data.

*----------------------------------------------------------------------
* Program     : ZAEGI001
* Changed  By : Jyoti Sharma
* Changed on  : Sept 16 2014
* Reason:  Selection option Plant is required, removed the existing
*          Parameter.Station ID is updated based on Location now.
*          Existing code is not deleted, its commented
*----------------------------------------------------------------------
* data declaration
*----------------------------------------------------------------------
TABLES:  t499s ,t087s.     "Evaluation Group 8 places

* internal tables storing BDC entries for deleting old / creating new
* station id information
DATA: BEGIN OF bdcdata OCCURS 100.
        INCLUDE STRUCTURE bdcdata.
DATA: END OF bdcdata.

* other data declarations
*DATA: BEGIN OF itab OCCURS 0,                                     "EHP5
*              irec(39),                                           "EHP5
*      END OF itab.
TYPES : BEGIN OF ty_t499s,
    plant TYPE werks_d,
    station_id TYPE stort_t499s,
    station_desc TYPE ktext,
    END OF ty_t499s.

DATA : lt_t499s TYPE STANDARD TABLE OF ty_t499s,
       lt_t499s1 TYPE STANDARD TABLE OF ty_t499s,
       wa_t499s TYPE ty_t499s.                                                   "EHP5
DATA: line(39)    TYPE c.                  "line of data
DATA: mess(100)   TYPE c.                  "storing error message
DATA: zsubrc      TYPE i.                  "return code
DATA: p_file LIKE filename-fileextern.     "physical filename
DATA: station_id(8),                       "station identification
      station_desc(30),                    "station description
      any_updates(1)   VALUE 'n'.          "update indicator

*----------------------------------------------------------------------
* parameters
*----------------------------------------------------------------------
*SELECTION-SCREEN BEGIN OF BLOCK BLK1 WITH FRAME.
*PARAMETERS:   L_FILE LIKE FILENAME-FILEINTERN "logical filename
*              DEFAULT 'ZAEGI001_01' OBLIGATORY.
*SELECTION-SCREEN END OF BLOCK BLK1.
SELECTION-SCREEN BEGIN OF BLOCK box2 WITH FRAME TITLE text-003.   "EHP5
SELECT-OPTIONS : s_werks FOR t499s-werks.
*PARAMETERS: p_local RADIOBUTTON GROUP gr1,                        "EHP5
*            p_unix  RADIOBUTTON GROUP gr1 DEFAULT 'X'.            "EHP5
*                                                                  "EHP5
*SELECTION-SCREEN SKIP.                                            "EHP5
*PARAMETERS: p_locfil LIKE filename-fileextern,                    "EHP5
*            p_infile LIKE filename-fileextern.                    "EHP5
SELECTION-SCREEN END OF BLOCK box2.                               "EHP5
*----------------------------------------------------------------------
INITIALIZATION.
*  MOVE '/usr/sap/interfaces/P01/IFAM003/ZAEGI001.SAP'             "EHP5
*       TO p_infile.                                               "EHP5
*  MOVE 'H:\' TO p_locfil.                                         "EHP5
*----------------------------------------------------------------------
* Event: start-of-selection ... open file to read
*----------------------------------------------------------------------
START-OF-SELECTION.

  "EHP5
  DATA: l_file TYPE string.                                       "EHP5
  DATA: lt_itab TYPE STANDARD TABLE OF string.                    "EHP5
  "EHP5
** For Unix Files                                                  "EHP5
*  IF p_unix = 'X'.                                                "EHP5
*    OPEN DATASET p_infile FOR INPUT IN TEXT MODE.                 "EHP5
*    IF sy-subrc NE 0.                                             "EHP5
*      MESSAGE e002 WITH p_infile.                                 "EHP5
*      "EHP5
*      STOP.                                                       "EHP5
*    ELSE.                                                         "EHP5
*      DO.                                                         "EHP5
*        READ DATASET p_infile INTO itab-irec.                     "EHP5
*        IF sy-subrc NE 0.                                         "EHP5
*          EXIT.                                                   "EHP5
*        ELSE.                                                     "EHP5
*          APPEND itab.                                            "EHP5
*        ENDIF.                                                    "EHP5
*      ENDDO.                                                      "EHP5
*    ENDIF.                                                        "EHP5
*  ELSE.                                                           "EHP5
** For Local Files.                                                "EHP5
*    l_file = p_locfil.                                            "EHP5
*    "EHP5
*    CALL METHOD cl_gui_frontend_services=>gui_upload              "EHP5
*      EXPORTING                                                   "EHP5
*        filename                = l_file                          "EHP5
*        filetype                = 'ASC'                           "EHP5
*      CHANGING                                                    "EHP5
*        data_tab                = lt_itab                         "EHP5
*      EXCEPTIONS                                                  "EHP5
*        file_open_error         = 1                               "EHP5
*        file_read_error         = 2                               "EHP5
*        OTHERS                  = 18.                             "EHP5
*    "EHP5
*    IF sy-subrc NE 0.                                             "EHP5
*      MESSAGE e002 WITH p_locfil.                                 "EHP5
*    ELSE.                                                         "EHP5
*      itab[] = lt_itab[].                                         "EHP5
*    ENDIF.                                                        "EHP5
*    "EHP5
*  ENDIF.                                                          "EHP5
*
**   "get physical path from logical filename
**   CALL FUNCTION 'FILE_GET_NAME'
**        EXPORTING CLIENT           = SY-MANDT
**                  LOGICAL_FILENAME = L_FILE
**                  OPERATING_SYSTEM = SY-OPSYS
**        IMPORTING FILE_NAME        = P_FILE
**        EXCEPTIONS FILE_NOT_FOUND  = 1.
**   IF SY-SUBRC <> 0.
**    MESSAGE E368
**            WITH 'Error in locating physical path for logical file'
**                 L_FILE.
**    EXIT.
**   ENDIF.
**
**   "open file for input
**   OPEN DATASET P_FILE FOR INPUT IN TEXT MODE MESSAGE MESS.
**   IF SY-SUBRC <> 0.
**      MESSAGE E368 WITH MESS.
**   ENDIF.

*----------------------------------------------------------------------
* Event: start-of-selection ... main program
*----------------------------------------------------------------------

* Re-initialization
  REFRESH bdcdata.
  CLEAR bdcdata.

*  update with new station info                                  "Ehp5
*   DO.                                                           "Ehp5
*      READ DATASET P_FILE INTO LINE.                             "Ehp5
  "Ehp5


  SELECT werks stand ktext FROM t499s INTO TABLE lt_t499s
    WHERE werks IN s_werks.
  IF sy-subrc IS INITIAL AND lt_t499s[] IS NOT INITIAL.
    SORT lt_t499s BY station_id.

  ENDIF.
  CLEAR : wa_t499s,
          lt_t499s1.

  LOOP AT lt_t499s INTO wa_t499s.                                                   "Ehp5

    station_id  = wa_t499s-station_id.
    station_desc = wa_t499s-station_desc.
    READ TABLE lt_t499s1 WITH KEY station_id = station_id
                                  TRANSPORTING NO FIELDS.
    IF sy-subrc = 0.
      MOVE 'y' TO any_updates.                             "Ehp5
      PERFORM change_station USING station_id station_desc."Ehp5
    ELSE.
      SELECT SINGLE * FROM t087s                                 "Ehp5
      WHERE  gdlgrp = station_id                                 "Ehp5
      AND    spras  = sy-langu.         "4.6B upgrade            "Ehp5
      IF sy-subrc <> 0.                                          "Ehp5
        MOVE 'y' TO any_updates.                                "Ehp5
        PERFORM create_station USING station_id station_desc.   "Ehp5
      ELSE.                                                      "Ehp5
        IF t087s-gdlgrp_txt <> station_desc.                    "Ehp5
          MOVE 'y' TO any_updates.                             "Ehp5
          PERFORM change_station USING station_id station_desc."Ehp5
        ENDIF.                                                  "Ehp5
      ENDIF.
    ENDIF.
    APPEND wa_t499s TO lt_t499s1.                                                                                                      "Ehp5
  ENDLOOP.
  "Ehp5
*   ENDDO.                                                        "Ehp5
  "Ehp5
* PERFORM SCREEN_FIELD  USING 'BDC_OKCODE' '/11'.                 "Ehp5
  PERFORM screen_field  USING 'BDC_OKCODE' 'SAVE'.                "Ehp5
  PERFORM screen_header USING 'SAPL0A03' '0500' 'X'.              "Ehp5
  PERFORM screen_field  USING 'BDC_OKCODE' 'ENDE'.                "Ehp5

  IF any_updates = 'y'.
    PERFORM execute.
    IF zsubrc <> 0.
      MESSAGE e019 WITH 'Error in refreshing evaluation group.'."Ehp5
    ELSE.
      MESSAGE i019 WITH 'Evaluation Group Refreshed.'.          "Ehp5
    ENDIF.
  ELSE.
    MESSAGE i019 WITH 'No Evaluation Group Updates to Perform.'. "Ehp5
  ENDIF.

  "close data file
  CLOSE DATASET p_file.

*----------------------------------------------------------------------
* form change_station ... create BDC entries to update station desc
*----------------------------------------------------------------------
FORM change_station USING stn_id stn_desc.

  DATA:    old_stn_id(15)   TYPE c VALUE 'SVALD-VALUE(01)'.
  DATA:    new_stn_desc(14) TYPE c VALUE 'REPLACE-BUFFER'.

  "find table row to change using the menu pulldowns
  PERFORM screen_header USING 'SAPL0A03' '0500' 'X'.
  PERFORM screen_field  USING 'BDC_OKCODE' 'POSI'.        "go to other

*   PERFORM SCREEN_HEADER USING 'SAPLSPO4' '100' 'X'.
  PERFORM screen_header USING 'SAPLSPO4' '300' 'X'.              "4.6B
  PERFORM screen_field  USING old_stn_id stn_id.
  PERFORM screen_field  USING 'BDC_OKCODE' 'FURT'.           "continue

  PERFORM screen_header USING 'SAPL0A03' '0500' 'X'.
  PERFORM screen_field  USING 'BDC_CURSOR' 'V_T087G-GDLGRP(01)'. "4.6C
  PERFORM screen_field  USING 'BDC_OKCODE' '/9'.         "select entry

  PERFORM screen_header USING 'SAPL0A03' '0500' 'X'.
  PERFORM screen_field  USING 'BDC_OKCODE' 'REPL'.    "chg fld content
  PERFORM screen_header USING 'SAPLSVIX' '0700' 'X'.
  PERFORM screen_field  USING new_stn_desc stn_desc.
  PERFORM screen_header USING 'SAPL0A03' '0500' 'X'.          "4.6B

ENDFORM.                    "CHANGE_STATION

*----------------------------------------------------------------------
* form create_station ... create BDC entries to add station info
*----------------------------------------------------------------------
FORM create_station USING stn_id stn_desc.

  DATA:    add_stn_id(18)   TYPE c VALUE 'V_T087G-GDLGRP(01)'.
  DATA:    add_stn_desc(22) TYPE c VALUE 'V_T087G-GDLGRP_TXT(01)'.

  "create header record for BDC create table
  PERFORM screen_header USING 'SAPL0A03' '0500' 'X'.
  PERFORM screen_field  USING 'BDC_OKCODE' '/5'.          "add entries
  PERFORM screen_header USING 'SAPL0A03' '0500' 'X'.

  "create BDC entries to be updated
  PERFORM screen_field  USING add_stn_id   stn_id.
  PERFORM screen_field  USING add_stn_desc stn_desc.
  PERFORM screen_field  USING 'BDC_OKCODE' '/3'.                 "back

ENDFORM.                    "CREATE_STATION

*----------------------------------------------------------------------
* form screen_header ... create BDC header entry using parameters
*                        (1) program   ...  screen program
*                        (2) screen    ...  screen number
*                        (3) indicator ...  indicator
*----------------------------------------------------------------------
FORM screen_header USING program screen indicator.
  CLEAR bdcdata.
  bdcdata-program  = program.
  bdcdata-dynpro   = screen.
  bdcdata-dynbegin = indicator.
  APPEND bdcdata.
ENDFORM.                    "SCREEN_HEADER

*----------------------------------------------------------------------
* form screen_field ... create BDC entry using parameters
*                       (1) fnam ... field name to fill
*                       (2) fval ... value to fill in
*----------------------------------------------------------------------
FORM screen_field USING fnam fval.
  CLEAR bdcdata.
  bdcdata-fnam = fnam.
  bdcdata-fval = fval.
  APPEND bdcdata.
ENDFORM.                    "SCREEN_FIELD

*----------------------------------------------------------------------
* form execute ... execute transaction OAV8 using
*                  (1) bdcdata as data source
*                  (2) in no display mode
*                  (3) in synchronous update mode
*----------------------------------------------------------------------
FORM execute.
  CALL TRANSACTION 'OAV8' USING bdcdata         "data source
                          MODE 'N'              "N - no display
                          UPDATE 'S'.           "S - synchronous update
  MOVE sy-subrc TO zsubrc.
*  call function 'BDC_OPEN_GROUP'
*       exporting  client      = sy-mandt
*                  group       = 'MARVCHGS'
*                  user        = sy-uname.
*  call function 'BDC_INSERT'
*       exporting  tcode       = 'OAV8'
*       tables     dynprotab   = bdcdata.
*  call function 'BDC_CLOSE_GROUP'
*       exceptions not_open    = 1
*                  queue_error = 2
*                  others      = 3.
ENDFORM.                    "EXECUTE
*----------------------------------------------------------------------
