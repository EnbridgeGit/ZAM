

REPORT ZALE2 .
* template for IDoc outbound processing for the ALE workshop
*  " : insert the command in the correct way into the coding

* data base tables
  TABLES: ZALE2.

* IDoc segment
  TABLES: Z1MES02.

* parameters
  PARAMETERS: MESSAGE LIKE ZALE2-MESSAGE,
              MESKEY  LIKE ZALE2-MESKEY,
              SENDER  LIKE ZALE2-SENDER.

* internal tables
  DATA: BEGIN OF F_IDOC_HEADER.
          INCLUDE STRUCTURE EDIDC.
  DATA: END OF F_IDOC_HEADER.

  DATA: BEGIN OF T_IDOC_DATA OCCURS 0.
          INCLUDE STRUCTURE EDIDD.
  DATA: END OF T_IDOC_DATA.

  DATA: BEGIN OF T_COMMUNICATION_IDOC_CONTROL OCCURS 0.
          INCLUDE STRUCTURE EDIDC.
  DATA: END OF T_COMMUNICATION_IDOC_CONTROL.

* clear data
  CLEAR T_IDOC_DATA.
  REFRESH T_IDOC_DATA.
  CLEAR F_IDOC_HEADER.

* move paramters into field string
 " message -> z1mes##-message
  MOVE MESSAGE TO Z1MES02-MESSAGE.
 " meskey -> z1mes##-meskey
  MOVE MESKEY TO Z1MES02-MESKEY.

* add additional data to field string
 " 'GROUP_##' -> z1mes##-sender
  MOVE SENDER TO Z1MES02-SENDER.
 " sy-datum -> z1mes##-mdate
  Z1MES02-MDATE = SY-DATUM.
 " sy-uzeit -> z1mes##-mtime
  Z1MES02-MTIME = SY-UZEIT.

* move field string to IDoc-data
 " z1mes## -> t_idoc_data-sdata
  T_IDOC_DATA-SDATA = Z1MES02.

* fill segment name to t_idoc_data-segnam
 " 'Z1MES##' -> t_idoc_data-segnam
  T_IDOC_DATA-SEGNAM = 'Z1MES02'.

* append data
  APPEND T_IDOC_DATA.

* fill IDoc header
 " message type 'ZMES##' -> f_idoc_header-mestyp
  F_IDOC_HEADER-MESTYP = 'ZMES02'.
 " basic IDoc type 'ZALE2##' -> f_idoc_header-idoctp
  F_IDOC_HEADER-IDOCTP = 'ZALE202'.

* send IDoc
  CALL FUNCTION 'MASTER_IDOC_DISTRIBUTE'
     EXPORTING
          MASTER_IDOC_CONTROL            = F_IDOC_HEADER
     TABLES
          COMMUNICATION_IDOC_CONTROL     = T_COMMUNICATION_IDOC_CONTROL
          MASTER_IDOC_DATA               = T_IDOC_DATA
     EXCEPTIONS
          ERROR_IN_IDOC_CONTROL          = 1
          ERROR_WRITING_IDOC_STATUS      = 2
          ERROR_IN_IDOC_DATA             = 3
          SENDING_LOGICAL_SYSTEM_UNKNOWN = 4
          OTHERS                         = 5.

* check result
  IF SY-SUBRC = 0.
    WRITE: / 'IDoc created.'.
  ELSE.
    WRITE: / 'Error ', SY-SUBRC, ' occured.'.
  ENDIF.

* close LUW
  COMMIT WORK.
