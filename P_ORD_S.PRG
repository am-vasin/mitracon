*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла P_Ord_S      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Печать суммарной заявки 2006.                      ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 24.11.2006 ══╝
PROCEDURE P_Ord_S
PARAMETERS prmList, prmOrd
*
* prmList - номер списка (C или N)
* prmOrd  - номер суммарной заявки (C или N)
*

PRIVATE tmpList, tmpOrd, sb
DIMENSION sb(8, 2)

m.tmpList = IIF(TYPE("m.prmList")="N", m.prmList, VAL(m.prmList))
m.tmpList = STR(m.tmpList, 6)
m.tmpOrd = IIF(TYPE("m.prmOrd")="N", m.prmOrd, VAL(m.prmOrd))

sb(1, 1) = "{№ док-та}"
sb(1, 2) = "          "

sb(2, 1) = "{Список заявок                         }"
sb(2, 2) = "                                        "

sb(3, 1) = "{Имя списка                    }"
sb(3, 2) = "                                "

sb(4, 1) = "{Продавец                      }"
sb(4, 2) = "                                "

sb(5, 1) = "{Кладовщик                     }"
sb(5, 2) = "                                "

sb(6, 1) = "{Раб. место    }"
sb(6, 2) = "                "

sb(7, 1) = "{Примечание                            }"
sb(7, 2) = "....:....!....:....!....:....!....:....!"

sb(8, 1) = "{Дата    }"
sb(8, 2) = "          "

IF .NOT. TmpOrdFile()	&& Подготовка временного файла и параметров заголовка
  RETURN
ENDIF

DO PrnOrdSum

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура TmpOrdFile   Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Создание временного файла печати заявки                 │
*│                                                                        │
*└────────────────────────────────────────────────────────── 24.11.2006 ──┘
PROCEDURE TmpOrdFile

PRIVATE s_sav, tmpOList, tmpNote, tmpSlM, tmpStM, tmpName
PRIVATE tmpDate, tmpVal, i, prc_00

m.tmpOList = ""
m.tmpNote = ""
m.tmpSlM  = 0
m.tmpStM  = 0
m.tmpDate = CTOD("")
m.tmpName = SYS(3)

m.s_sav = SELECT()

SELECT 0
IF FILE_O(m.base_path+"O_BOUND.DBF")
  USE (m.base_path+"O_BOUND") ORDER TAG H_BOUND
  i = 0
  SCAN
    i = i + 1
    DIMENSION prc_00(i,2)
    prc_00(i,1) = H_BOUND
    prc_00(i,2) = CHAR_C
  ENDSCAN  
ENDIF  
USE
CREATE DBF (m.tmpo_path+m.tmpName)	;
	(	CODE		N(10), ; 
		ADDRESS		C( 8), ;
		MEMO_ST		C(16), ;
		MEMO_ST_L	C( 1), ;
		ATTEN		C( 1), ;
		PREFIX		C( 4), ;
		NAME35		C(35), ;
		NAME29		C(29), ;
		NAME23		C(23), ;
		PRODUCER	C( 8), ;
		QNT			N( 6)  )
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TmpOrdFile
INDEX ON CODE TAG CODE
		
SELECT 0
USE m.base_path+"ORD_D" SHARED AGAIN ORDER TAG DOC_NUM ALIAS OD_6B24
USE m.base_path+"STOCK" SHARED AGAIN ORDER TAG CODE ALIAS ST_6B24 IN 0
SET RELATION TO CODE INTO ST_6B24
SELECT 0
USE m.base_path+"ORD_T" SHARED AGAIN ORDER TAG DOC_NUM ALIAS OT_6B24
SEEK m.tmpList

*sb(1, 1) = "{№ док-та}"
m.tmpVal = ALLTRIM(m.tmpList)+"/"+ALLTRIM(STR(m.tmpOrd))
sb(1, 2) = PADR(m.tmpVal, LEN(sb(1, 2)))

*sb(3, 1) = "{Имя списка                    }"
m.tmpVal = ALLTRIM(OT_6B24.LIST_NAME)
sb(3, 2) = PADR(m.tmpVal, LEN(sb(3, 2)))

*sb(6, 1) = "{Раб. место    }"
USE m.base_path+"LIST_TIT" SHARED AGAIN ORDER TAG LST_NUM ALIAS LST_6B24 IN 0
USE m.base_path+"ARM_LST" SHARED AGAIN ORDER TAG ARM_C ALIAS ARM_6B24 IN 0
IF SEEK(VAL(OT_6B24.LIST_NUM), "LST_6B24")
  = SEEK(LST_6B24.LST_ARM, "ARM_6B24")
ELSE
  = SEEK(OT_6B24.WHERE, "ARM_6B24")
ENDIF
m.tmpVal = ALLTRIM(ARM_6B24.ARM_N)
sb(6, 2) = PADR(m.tmpVal, LEN(sb(6, 2)))
USE IN ARM_6B24
USE IN LST_6B24

SCAN REST WHILE LIST_NUM = m.tmpList FOR VAL(CHECK_S) = m.tmpOrd
  m.tmpDate = MAX(m.tmpDate, ORD_DATE)
  m.tmpOList = m.tmpOList+ALLTRIM(STR(ORD_NUM))+","
  IF .NOT. EMPTY(NOTE)
    m.tmpNote  = NOTE
  ENDIF
  IF .NOT. EMPTY(WHO)
    m.tmpSlM   = WHO
  ENDIF
  IF .NOT. EMPTY(STO_MARK)
    m.tmpStM   = STO_MARK
  ENDIF
  SELECT OD_6B24
  SEEK OT_6B24.LIST_NUM+STR(OT_6B24.ORD_NUM, 3)
  SCAN REST WHILE OT_6B24.LIST_NUM = LIST_NUM .AND. OT_6B24.ORD_NUM = ORD_NUM
    SELECT TmpOrdFile
    IF .NOT. SEEK(OD_6B24.CODE)
      APPEND BLANK
      REPLACE	CODE		WITH OD_6B24.CODE,		; 
				ADDRESS		WITH ST_6B24.ADDRESS,	;
				MEMO_ST		WITH ST_6B24.MEMO_ST,	;
				MEMO_ST_L	WITH IIF(EMPTY(ST_6B24.MEMO_ST), "", "+"), ;
				ATTEN		WITH P_Bound(ST_6B24.PRICE),	;
				PREFIX		WITH ST_6B24.PREFIX,			;
				NAME35		WITH LEFT(ST_6B24.NAME, 35),	;
				NAME29		WITH LEFT(ST_6B24.PREFIX+ST_6B24.NAME, 29),	;
				NAME23		WITH LEFT(ST_6B24.PREFIX+ST_6B24.NAME, 23),	;
				PRODUCER	WITH ST_6B24.PRODUCER
    ENDIF
    REPLACE QNT WITH QNT+OD_6B24.QNT
    SELECT OD_6B24
  ENDSCAN
  SELECT OT_6B24
ENDSCAN

SELECT TmpOrdFile
DELETE ALL FOR QNT = 0

m.tmpVal = LEFT(m.tmpOList, LEN(m.tmpOList)-1)

*sb(2, 1) = "{Список заявок                         }"
sb(2, 2) = PADR(m.tmpVal, LEN(sb(2, 2)))

*sb(7, 1) = "{Примечание                            }"
sb(7, 2) = PADR(m.tmpNote, LEN(sb(7, 2)))

*sb(8, 1) = "{Дата    }"
sb(8, 2) = DTOC(m.tmpDate)

USE m.base_path+"PERSONS" SHARED AGAIN ORDER TAG CODE ALIAS PRS_6B24 IN 0
IF .NOT. EMPTY(m.tmpSlM)
  = SEEK(m.tmpSlM, "PRS_6B24")
  sb(4, 2) = PADR(PRS_6B24.FAMILY, LEN(sb(4, 2)))
ENDIF
IF .NOT. EMPTY(m.tmpStM)
  = SEEK(m.tmpStM, "PRS_6B24")
  sb(5, 2) = PADR(PRS_6B24.FAMILY, LEN(sb(5, 2)))
ENDIF
USE IN PRS_6B24

USE IN OT_6B24
USE IN OD_6B24
USE IN ST_6B24

SELECT TmpOrdFile
DELETE ALL FOR QNT = 0
GO TOP
IF EOF() .OR. BOF()
  USE
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
  SELECT (m.s_sav)
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Заявки не существует или она пуста..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF


SELECT (m.s_sav)

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура PrnOrdSum    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Собственно печать заявки                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.11.2006 ──┘
PROCEDURE PrnOrdSum

PRIVATE s_sav, sbDet
PRIVATE p_ln, fnt, n_cp, lft, ffeed, p_drctry
PRIVATE n, t_p, e_p, det, i, j, stw, cnt, delim, tmpName


DIMENSION sbDet(11, 2)

sbDet( 1, 1) = "{Адрес }"
sbDet( 1, 2) = "        "

sbDet( 2, 1) = "{Доп. адрес    }"
sbDet( 2, 2) = "                "

sbDet( 3, 1) = "{А}"
sbDet( 3, 2) = " "

sbDet( 4, 1) = "{!}"
sbDet( 4, 2) = " "

sbDet( 5, 1) = "{Пр}"
sbDet( 5, 2) = "    "

sbDet( 6, 1) = "{Наименование                     }"
sbDet( 6, 2) = "....:....!....:....!....:....!....:"

sbDet( 7, 1) = "{Наименование 29            }"
sbDet( 7, 2) = "....:....!....:....!....:...."

sbDet( 8, 1) = "{Наименование 23      }"
sbDet( 8, 2) = "....:....!....:....!..."

sbDet( 9, 1) = "{Фирма }"
sbDet( 9, 2) = "....:..."

sbDet(10, 1) = "{К-во}"
sbDet(10, 2) = "....:."

sbDet(11, 1) = "{Наименование 29 +количество      }"
sbDet(11, 2) = "....:....!....:....!....:....!....:"

m.delim = PADC(ALLTRIM(sb(1,2)), 72, "-")
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"DOC_FORM")
LOCATE FOR "ORDERSTO" == ALLTRIM(UPPER(DOC_NAME))
m.p_ln = DOC_FORM.PAGE_LEN
m.fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
m.n_cp = DOC_FORM.N_COPIES
m.lft  = DOC_FORM.LEFT_FIELD
m.ffeed = DOC_FORM.F_FEED
m.p_drctry = DOC_FORM.P_DIR

m.n = MEMLINES(DOC_H)
DIMENSION t_p(m.n)
FOR m.i = 1 TO m.n
  t_p(m.i) = MLINE(DOC_H, m.i)
ENDFOR

m.n = MEMLINES(DOC_F)
DIMENSION e_p(m.n)
FOR m.i = 1 TO m.n
  e_p(m.i) = MLINE(DOC_F,m.i)
ENDFOR

m.n = MEMLINES(DETAIL)
DIMENSION det(m.n)
FOR m.i = 1 TO m.n
  det(m.i) = MLINE(DETAIL,m.i)
ENDFOR
USE

FOR m.i = 1 TO ALEN(t_p)
  FOR m.j = 1 TO ALEN(sb, 1)
    t_p(m.i) = STRTRAN(t_p(m.i), sb(m.j, 1), sb(m.j, 2))
  ENDFOR
ENDFOR

FOR m.i = 1 TO ALEN(e_p)
  FOR m.j = 1 TO ALEN(sb, 1)
    e_p(m.i) = STRTRAN(e_p(m.i), sb(m.j, 1), sb(m.j, 2))
  ENDFOR
ENDFOR

DO Ini_Prn WITH "Заявка на склад N "+sb(1,2), m.p_ln, m.lft, m.n_cp, ;
                m.fnt, m.ffeed, m.p_drctry

FOR m.i = 1 TO ALEN(t_p)
  @ PROW()+1, 0 SAY t_p(m.i)
ENDFOR

SELECT TmpOrdFile
INDEX ON ADDRESS TAG ADDRESS
cnt = 0
SCAN
  
  *sbDet( 1, 1) = "{Адрес }"
  sbDet( 1, 2) = ADDRESS
  *sbDet( 2, 1) = "{Доп. адрес    }"
  sbDet( 2, 2) = MEMO_ST
  *sbDet( 3, 1) = "{А}"
  sbDet( 3, 2) = MEMO_ST_L
  *sbDet( 4, 1) = "{!}"
  sbDet( 4, 2) = ATTEN
  *sbDet( 5, 1) = "{Пр}"
  sbDet( 5, 2) = PREFIX
  *sbDet( 6, 1) = "{Наименование                     }"
  sbDet( 6, 2) = NAME35
  *sbDet( 7, 1) = "{Наименование 29            }"
  sbDet( 7, 2) = NAME29
  *sbDet( 8, 1) = "{Наименование 23      }"
  sbDet( 8, 2) = NAME23
  *sbDet( 9, 1) = "{Фирма }"
  sbDet( 9, 2) = PRODUCER
  *sbDet(10, 1) = "{К-во}"
  sbDet(10, 2) = STR(QNT, 6)
  *sbDet(11, 1) = "{Наименование 29 +количество      }"
  sbDet(11, 2) = PADR(ALLTRIM(sbDet( 7, 2))+sbDet(10, 2), LEN(sbDet(11, 2)))
  
  FOR m.i = 1 TO ALEN(det)
    stw = det(m.i)
    FOR m.j = 1 TO ALEN(sbDet, 1)
      stw = STRTRAN(stw, sbDet(m.j, 1), sbDet(m.j, 2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR
  cnt = (cnt+1)%5
  IF cnt = 0
    @ PROW()+1, 0 SAY delim
  ENDIF
ENDSCAN

m.tmpName = DBF()
USE
DELETE FILE (m.tmpName)
DELETE FILE (LEFT(m.tmpName, LEN(m.tmpName)-3)+"cdx")
SELECT (m.s_sav)

FOR m.i = 1 TO ALEN(e_p)
  @ PROW()+1, 0 SAY e_p(m.i)
ENDFOR

DO Term_Prn WITH "", .F., .F., "ORD_S", STR(m.tmpOrd, 3), m.tmpList, DATE()

RETURN
