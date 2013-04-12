********************************************************************************
*    Файл Terr_CSV       Разработчик Андрей Васин                11.12.2011    *
********************************************************************************
*                                                                              *
*                     Разборка CSV-файла Терра-Электроники                     *
*                                                                              *
********************************************************************************
PROCEDURE Terr_CSV
PARAMETERS fName
*
* fName		- Имя CSV-файла
*
PRIVATE s_sav, tmpName, tmpNote, d_t, w_t
m.d_t = LEFT(Table_D(),66)
m.w_t = LEFT(Table_W(),66)

m.s_sav = SELECT()
SELECT 0
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName)	;
	(	CODE	N(10),		;
		QNT_TER	N(10),		;
		PRICE_1	N(15, 2),	;
		NOTE	M			)
USE (m.tmpo_path+m.tmpName) ALIAS TMP_TERR
APPEND BLANK
SELECT 0
USE m.path_comm+"SUBJECT" SHARED AGAIN ORDER TAG STO_CODE ALIAS SBJ_TERR
SELECT 0
USE m.base_path+"CLASSES" SHARED AGAIN ORDER TAG CODE ALIAS CLS_TERR
SELECT 0
USE m.base_path+"STOCK" SHARED AGAIN ORDER TAG CODE ALIAS STO_TERR
SET RELATION TO CLASS INTO CLS_TERR
SELECT TMP_TERR

DO CSV_File WITH m.fName

USE IN TMP_TERR
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".fpt")
USE IN CLS_TERR
USE IN STO_TERR
USE IN SBJ_TERR
SELECT(m.s_sav)
RETURN

********************************************************************************
*** Добавление новой записи
********************************************************************************
PROCEDURE NewRec
PARAMETERS n_rec

IF m.n_rec % 1000 = 0
  WAIT WINDOW STR(m.n_rec) NOWAIT
ENDIF

RETURN

********************************************************************************
*** Запись поля
********************************************************************************
PROCEDURE AddField
PARAMETERS n_fld, v_fld
PRIVATE tmp_Val

DO CASE
CASE m.n_Fld = 1
  REPLACE CODE WITH VAL(m.v_fld)
CASE m.n_Fld = 8
  REPLACE QNT_TER WITH VAL(m.v_fld)
CASE m.n_Fld = 10
  REPLACE NOTE WITH m.v_fld
CASE m.n_Fld = 11
  REPLACE PRICE_1 WITH VAL(m.v_fld)
  IF CODE # 0 .AND. QNT_TER # 0
    IF SEEK(CODE, "STO_TERR")
      SELECT TMP_DMS
      APPEND BLANK
      REPLACE	CLASS		WITH STO_TERR.CLASS,	;
  				CLASS_DKO	WITH CLS_TERR.NAME,		;
  				CODE		WITH TMP_TERR.CODE,		;
	  			NAME		WITH STO_TERR.NAME,		;
  				PRODUCER	WITH STO_TERR.PRODUCER,	;
  				QNT			WITH TMP_TERR.QNT_TER,	;
  				PRICE1		WITH TMP_TERR.PRICE_1,	;
	  			SUP_DATE	WITH "1 день!"
      SELECT TMP_TERR
    ENDIF
  ENDIF
  IF CODE # 0 .AND. .NOT. EMPTY(NOTE)
    IF SEEK(CODE, "SBJ_TERR")
      IF EMPTY(SBJ_TERR.FUNCTION)
        m.tmpNote = NOTE
        SELECT SBJ_TERR
        REPLACE FUNCTION WITH CHRTRAN(m.tmpNote, w_t, d_t)
        SELECT TMP_TERR
      ENDIF
    ENDIF
  ENDIF
ENDCASE

RETURN
