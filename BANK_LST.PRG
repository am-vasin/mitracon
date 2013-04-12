********************************************************************************
*    Файл Bank_Lst       Разработчик Андрей Васин                10.01.2012    *
********************************************************************************
*                                                                              *
*                          Подборка банков по шаблону                          *
*                                                                              *
********************************************************************************
PROCEDURE Bank_Lst
PARAMETERS prmShab
*
* prmShab - Шаблон для поиска
*

PRIVATE s_sav, fLen, tmpShab, tmpName, n
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"CUS_BANK") SHARED AGAIN ALIAS BANK_ADD
SELECT 0
USE (m.base_path+"CUS_BIL") SHARED AGAIN ALIAS BANK_MAIN
m.fLen = MAX(FSIZE("BANK", "BANK_MAIN"), FSIZE("BANK", "BANK_ADD"))
SELECT 0
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName)	;
	(	LINK	N(10),	;
		IS_MAIN	L,		;
		MRK		C( 1),	;
		NAME	C(m.fLen)	)
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_BANKS
m.tmpShab = SYS(15, m.lwr, m.prmShab)
DO Wt_Mess WITH "Подборка"
SELECT BANK_ADD
m.n = 0
SCAN
  m.n = m.n+1
  IF m.n % 10 = 0
    WAIT WINDOW STR(m.n) NOWAIT
  ENDIF
  IF m.tmpShab $ SYS(15, m.lwr, BANK)
    SELECT TMP_BANKS
    APPEND BLANk
    REPLACE	LINK	WITH RECNO("BANK_ADD"),	;
    		NAME	WITH BANK_ADD.BANK
    SELECT BANK_ADD
  ENDIF
ENDSCAN
USE

SELECT BANK_MAIN
SCAN
  m.n = m.n+1
  IF m.n % 10 = 0
    WAIT WINDOW STR(m.n) NOWAIT
  ENDIF
  IF m.tmpShab $ SYS(15, m.lwr, BANK)
    SELECT TMP_BANKS
    APPEND BLANk
    REPLACE	LINK	WITH RECNO("BANK_MAIN"),	;
    		NAME	WITH BANK_MAIN.BANK,		;
    		IS_MAIN	WITH .T.
    SELECT BANK_MAIN
  ENDIF
ENDSCAN
USE
WAIT CLEAR

SELECT TMP_BANKS
INDEX ON SYS(15, m.lwr, NAME) TAG MARKED FOR .NOT. EMPTY(MRK)
INDEX ON SYS(15, m.lwr, NAME) TAG NAME UNIQUE

DO Wt_Mess

SELECT (m.s_sav)
