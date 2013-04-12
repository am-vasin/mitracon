**************************************************************************
*                                                                        *
*                       Рассылка подписчикам                             *
*                                                                        *
**************************************************************************
PROCEDURE SS_Send
PARAMETERS ss_type, ss_mess

PRIVATE s_sav, i, tmp_ID, tmpTit

m.s_sav = SELECT()
SELECT 0
USE base_path+"LETT_OUT" SHARED AGAIN ALIAS OUT_5B13
SELECT 0
USE base_path+"LETT_IN" SHARED AGAIN ALIAS IN_5B13 ORDER TAG MESS_ID
SELECT 0
USE base_path+"SSCRPT_T" SHARED AGAIN ALIAS SST_5B13 ORDER TAG SS_CODE
SEEK m.ss_type
m.tmpTit = ALLTRIM(SS_NAME)
USE base_path+"SSCRPT" SHARED AGAIN ALIAS SS_5B13 ORDER TAG SS_CODE
SEEK STR(m.ss_type)

SCAN REST WHILE m.ss_type = SS_CODE
  m.tmp_ID = DOC_NUM("LETT_ID", "", m.arm, DATE())
  m.tmp_ID = SUBSTR(DTOS(DATE()), 3, 2)+STR(m.tmp_ID, 10)
  SELECT OUT_5B13
  APPEND BLANK
  REPLACE MESS_ID	WITH m.tmp_ID, ;
          TITLE		WITH m.tmpTit, ;
          MESS_TEXT	WITH m.ss_mess,;
          CR_DATE	WITH DATE(),   ;
          CR_TIME	WITH TIME(),   ;
          WHO		WITH m.user
  SELECT IN_5B13
  GO TOP
  IF .NOT. EMPTY(MESS_ID) .OR. BOF() .OR. EOF()
    APPEND BLANK
  ENDIF
  REPLACE MESS_ID	WITH m.tmp_ID, ;
          ADDRESS_T	WITH " ",      ;
          ADDRESS	WITH STR(SS_5B13.SS_USER, FSIZE("ADDRESS")), ;
          SHOW_DATE	WITH DATE()
  SELECT SS_5B13
ENDSCAN

USE IN IN_5B13
USE IN OUT_5B13
USE IN SS_5B13

SELECT (m.s_sav)

RETURN
