*********************************************************************
*                                                                   *
*       Формирование заявки на передачу из филиала                  *
*                                                                   *
*********************************************************************
PROCEDURE FOR_MOVE
PARAMETERS tmpDoc, tmpDate
PRIVATE  newDoc, newDate, t_id, t_tp
 newDoc = ""
 newDate = {} 
 t_id = ""
 t_tp = ""
*
*	tmpDoc	- MOVEST.DOC_NUM,
*	tmpDate	- MOVEST.DOC_DATE,
*	newDoc	- загружается номер сформированной заявки,
*	newDate	- загружается дата сформированной заявки,
*	t_id	- загружается номер технологической цепочки,
*	t_tp	- загружается дата технологической цепочки.
*
*	sup_own - глобальная переменная,которая должна быть определена в начале программы

PRIVATE s_sav
s_sav = SELECT()

SELECT 0
USE (base_store+"\STOCK") ORDER TAG CODE ALIAS ST_0910 SHARED AGAIN

SELECT 0
USE (Base_Compel+"\MOVEST") ORDER TAG DOC_NUM ALIAS MS_0910 SHARED AGAIN
= SEEK(LEFT(DTOS(tmpDate),4)+tmpDoc)

SELECT 0
USE (BASE_PATH+"\MATERS") ALIAS MT_0910 SHARED AGAIN

SELECT 0
USE (Base_Compel+"\MOVEST_D") ORDER TAG DOC_NUM ALIAS MSD_0910 SHARED AGAIN
IF .NOT. SEEK(LEFT(DTOS(tmpDate),4)+tmpDoc)
  = MessageBox("Заявка пуста!", 48, "Заявка на передачу из филиала")
  USE
  &&pvn
  SELECT MT_0910
  USE
  &&&pvn
  SELECT MS_0910
  USE
  SELECT ST_0910
  USE
  SELECT (s_sav)
  RETURN
ENDIF
SET RELATION TO STO_CODE INTO ST_0910

SELECT 0
USE (BASE_STORE+"\ORD_TIT") AGAIN
SET NOCPTRANS TO WHERE_ORD

SELECT 0
USE (BASE_STORE+"\ORDER") AGAIN

t_tp = "A"
newDate = DATE()
*!*	t_id = Doc_Num("COUNT", "", 0, newDate)
t_id = NEWCOUNT("COUNT", "","" , STR(YEAR(newDate),4))
t_id = STR(sup_own,8)+SUBSTR(DTOS(newDate), 3, 2)+STR(t_id,10)
*!*	newDoc = Doc_Num("ORDER", "", 0, newDate)
newDoc =  NEWCOUNT("ORDER", "", "",STR(YEAR(newDate),4))

newDoc = STR(newDoc,10)

SELECT MSD_0910
SCAN REST WHILE tmpDate = DOC_DATE .AND. tmpDoc = DOC_NUM
  SELECT ORDER
  APPEND BLANK
  REPLACE ORD_NUM  WITH newDoc,            ;
          ORD_DATE WITH newDate,           ;
          CODE     WITH MSD_0910.STO_CODE, ;
          PURSYS   WITH MT_0910.SUBSYS,    ;
          IDL      WITH MT_0910.IDL_MT,    ;
          PREFIX   WITH ST_0910.PREFIX,    ;
          NAME     WITH ST_0910.NAME,      ;
          PRODUCER WITH ST_0910.PRODUCER,  ;
          QNT      WITH MSD_0910.QTY
  SELECT MT_0910
  REPLACE IDL_MT WITH IDL_MT+1
  SELECT MSD_0910
ENDSCAN

PRIVATE tmpName, tmpAddr, tmpPhone
STORE "" TO tmpName, tmpAddr, tmpPhone
IF MS_0910.CUS_CODE # 0
  SELECT 0
  USE (base_store+"\ACCOUNT") SHARED AGAIN ALIAS ACC_0B01 ORDER TAG CUS_CODE
  SEEK MS_0910.CUS_CODE
  tmpName = CUS_NAME
  USE (base_store+"\CLI_INFO") SHARED AGAIN ALIAS ACC_0B01 ORDER TAG CUS_CODE
  IF SEEK(MS_0910.CUS_CODE)
    tmpAddr  = ADDRESS
    tmpPhone = PHONE
  ENDIF
  USE
ENDIF

SELECT ORD_TIT
APPEND BLANK
REPLACE ORD_NUM   WITH newDoc,          ;
        ORD_DATE  WITH newDate,         ;
        CUS_CODE  WITH sup_own,         ;
        SUP_CODE  WITH MS_0910.SUP_CODE, ;
        TECHNID   WITH t_id,            ;
        TECHNTP   WITH t_tp,            ;
        FOR_CODE  WITH MS_0910.CUS_CODE, ;
        FOR_NAME  WITH tmpName,         ;
        FOR_PHONE WITH tmpPhone,        ;
        FOR_ADDR  WITH tmpAddr,         ;
        WHO_ORD   WITH SYS.N_RM,          ;
        WHERE_ORD WITH CHR(SYS.N_S)

SELECT MS_0910
REPLACE TECHNID    WITH t_id,     ;
        TECHNTP    WITH t_tp,     ;
        START_TYPE WITH "O",      ;
        START_DOC  WITH newDoc,   ;
        START_DATE WITH newDate,  ;
        WHO        WITH SYS.N_RM

USE
SELECT MSD_0910
USE
SELECT ORDER
USE
SELECT ORD_TIT
USE
SELECT ST_0910
USE
SELECT MT_0910
USE
SELECT (s_sav)

RETURN
