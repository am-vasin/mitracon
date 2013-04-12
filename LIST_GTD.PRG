***********************************************************
*                                                         *
*                 Подбор ГТД для заготовки                *
*                                                         *
***********************************************************
PROCEDURE List_GTD

PRIVATE s_sav, ll_no, tmpNum, tmpLine, tmpCntr, tmpC

STORE "" TO m.tmpNum, m.tmpLine, m.tmpCntr, m.tmpC
m.ll_no = STR(RECNO("INC_TIT"), 5)
m.s_sav = SELECT()
SELECT 0
USE m.base_path+"INC_LIST" SHARED AGAIN ORDER TAG L_CODE DESCENDING ALIAS TMP_LIST
SEEK m.ll_no
SCAN REST WHILE LIST_NO = m.ll_no FOR CODE # 0 .AND. EMPTY(GTD_NUM)
  DO Suit_GTD WITH CODE, m.tmpNum, m.tmpLine, m.tmpCntr, m.tmpC
  REPLACE	GTD_NUM		WITH m.tmpNum,	;
  			GTD_LINE	WITH m.tmpLine,	;
  			COUNTRY		WITH m.tmpCntr, ;
  			COUNTRYC	WITH m.tmpC
ENDSCAN

USE
SELECT (m.s_sav)

RETURN
