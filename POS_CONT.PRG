*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Pos_Cont     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Контекстный поиск в номенклатуре                     ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 27.10.2005 ══╝
PROCEDURE Pos_Cont
PARAMETERS flt_val, prmClass, for_del, repeate		&& Значение фильтра

PRIVATE s_sav, tmpShab, tmpFunc, tmpRet, tmpArt, i, artCode

m.tmpRet  = 0
m.tmpFunc = .F.
m.tmpArt  = .F.
m.tmpShab = SPACE(25)

IF m.repeate
  IF .NOT. File_O(m.tmpo_path+"POS_CONT.DBF")
    PRIVATE mss
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"Ничего не было найдено..."
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    RETURN m.tmpRet
  ENDIF
  m.s_sav = SELECT()
  SELECT 0
  USE (m.tmpo_path+"POS_CONT.DBF") ORDER TAG NAME ALIAS TMP_5A27
  IF RECCOUNT() = 0
    PRIVATE mss
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"Ничего не было найдено..."
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (m.s_sav)
    RETURN m.tmpRet
  ENDIF
  m.tmpRet = PosFound()
ELSE

  DO Pos_Shab

  IF EMPTY(m.tmpShab)
    RETURN m.tmpRet
  ENDIF

  m.s_sav = SELECT()
  IF m.tmpArt
    m.artCode = ""
    FOR m.i = 1 TO LEN(m.tmpShab)
      IF SUBSTR(m.tmpShab, m.i, 1) $ "0123456789"
        m.artCode = m.artCode+SUBSTR(m.tmpShab, m.i, 1)
      ENDIF
    ENDFOR
    m.artCode = VAL(m.artCode)
    IF EMPTY(m.artCode)
      PRIVATE mss
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Артикул указан некорректно..."
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      RETURN m.tmpRet
    ENDIF
    SELECT 0
    USE base_path+"STOCK" SHARED AGAIN ORDER TAG CODE ALIAS TMP_5A27
    IF .NOT. SEEK(m.artCode)
      IF TYPE("m.mode_s_a") = "L"
        IF m.mode_s_a
          USE (m.base_path+"ADD_STO") SHARED AGAIN ORDER TAG CODE ALIAS TMP_5A27
          IF SEEK(STR(m.artCode), "TMP_5A27")
            m.tmpRet = -RECNO("TMP_5A27")
            USE
            SELECT (m.s_sav)
            RETURN m.tmpRet
          ENDIF
        ENDIF
      ENDIF
      PRIVATE mss
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Артикул не найден.."
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      USE
      SELECT (m.s_sav)
      RETURN m.tmpRet
    ENDIF
    IF .NOT. for_del
      IF .NOT. EMPTY(TMP_5A27.DATE_OFF)
        PRIVATE mss
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"Позиция удалена..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        USE
        SELECT (m.s_sav)
        RETURN m.tmpRet
      ENDIF
    ELSE
      IF EMPTY(TMP_5A27.DATE_OFF)
        PRIVATE mss
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"Позиция не удалена..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        USE
        SELECT (m.s_sav)
        RETURN m.tmpRet
      ENDIF
    ENDIF
    IF .NOT. EMPTY(prmClass) .AND. prmClass # TMP_5A27.CLASS
      PRIVATE mss
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Артикул не найден.."
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      USE
      SELECT (m.s_sav)
      RETURN m.tmpRet
    ENDIF
    m.tmpRet = RECNO("TMP_5A27")
    USE
    SELECT (m.s_sav)
    RETURN m.tmpRet
  ELSE
    DO Prp_List		&& Подготовка списка

    IF RECCOUNT("TMP_5A27") = 0
      PRIVATE mss
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Ничего не найдено..."
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      SELECT TMP_5A27
      m.tmpRet = PosFound()
    ENDIF
  ENDIF
ENDIF

IF USED("TMP_5A27")
  USE IN TMP_5A27
ENDIF

SELECT (m.s_sav)

RETURN m.tmpRet


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_List     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Подготовка списка                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.10.2005 ──┘
PROCEDURE Prp_List

PRIVATE s_sav, cnt, tmpFlg, tmpVal

m.s_sav = SELECT()

SELECT 0
IF File_O(m.tmpo_path+"POS_CONT.DBF")
  USE (m.tmpo_path+"POS_CONT.DBF") ALIAS TMP_5A27 EXCLUSIVE
  ZAP
  DELETE TAG ALL
ELSE
  CREATE DBF (m.tmpo_path+"POS_CONT") ;
	(	LINK		N(10), ;
		CODE		N( 7), ;
		PREFIX		C( 4), ;
		NAME		C(35), ;
		PRODUCER	C( 8), ;
		DATE_OFF	D      )
  USE (m.tmpo_path+"POS_CONT") ALIAS TMP_5A27 EXCLUSIVE
ENDIF

SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS STO_5A27
IF .NOT. EMPTY(m.flt_val)
  SET FILTER TO &flt_val
ENDIF
IF m.tmpFunc
  USE (m.path_comm+"SUBJECT") SHARED AGAIN ALIAS SJ_5A27 ORDER TAG STO_CODE IN 0
  SET RELATION TO CODE INTO SJ_5A27
ENDIF

DO Wt_Mess WITH "Поиск..."
m.cnt = 0
m.tmpShab = SYS(15, m.lwr, ALLTRIM(m.tmpShab))
IF .NOT. EMPTY(prmClass)
  SET ORDER TO TAG CLASS
  SEEK prmClass
ENDIF
SCAN REST
  m.cnt = m.cnt+1
  IF m.cnt % 500 = 0
    @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(m.cnt)), WCOLS())
  ENDIF
  IF .NOT. EMPTY(prmClass)
    IF CLASS # prmClass
      EXIT
    ENDIF
  ENDIF
  m.tmpFlg = .F.
  IF m.tmpShab $ SYS(15, m.lwr, STO_5A27.PREFIX+STO_5A27.NAME+STO_5A27.PRODUCER)
    m.tmpFlg = .T.
  ELSE
    IF m.tmpFunc
      IF .NOT. EMPTY(SJ_5A27.FUNCTION)
        m.tmpVal = SJ_5A27.FUNCTION
        IF m.tmpShab $ SYS(15, m.lwr, m.tmpVal)
          m.tmpFlg = .T.
        ENDIF
      ENDIF
    ENDIF 
  ENDIF
  IF m.tmpFlg
    SELECT TMP_5A27
    APPEND BLANK
    REPLACE	LINK		WITH RECNO("STO_5A27"), ;
    		CODE		WITH STO_5A27.CODE,		;
			PREFIX		WITH STO_5A27.PREFIX,   ;
			NAME		WITH STO_5A27.NAME,     ;
			PRODUCER	WITH STO_5A27.PRODUCER, ;
			DATE_OFF	WITH STO_5A27.DATE_OFF
    SELECT STO_5A27
  ENDIF
ENDSCAN
DO Wt_Mess
USE IN STO_5A27
IF USED("SJ_5A27")
  USE IN SJ_5A27
ENDIF

SELECT TMP_5A27
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
GO TOP

SELECT (m.s_sav)

RETURN
