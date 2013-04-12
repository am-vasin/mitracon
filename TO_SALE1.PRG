*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла To_Sale      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║               Вынесение помеченных позиций для продажи.                ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 02.05.2006 ══╝
PROCEDURE To_Sale
PARAMETERS n_list	&& номер списка (C)

PRIVATE s_sav, new_list, tmpName, tmpArr, mss, tmpZone, n1, n2

m.tmpArr = SYS(3)
m.new_list = ""
m.s_sav = SELECT()

SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS STO_6502
SELECT 0
CREATE DBF (m.tmpo_pat+m.tmpArr) ;
	(	LINK	N(10)  )
USE (m.tmpo_pat+m.tmpArr) EXCLUSIVE ALIAS TMP_LINK
m.tmpName = SYS(3)
DO WHILE m.tmpName = m.tmpArr
  m.tmpName = SYS(3)
ENDDO

SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ORDER TAG LIST ALIAS DET_6502

SEEK m.n_list
SCAN REST WHILE LIST_NUM = m.n_list FOR CODE # 0 .AND. .NOT. EMPTY(MARK_TMP) .AND. QNT > 0
  SELECT TMP_LINK
  APPEND BLANK
  REPLACE LINK WITH RECNO("DET_6502")
  SELECT DET_6502
ENDSCAN

IF RECCOUNT("TMP_LINK") = 0
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Вы не отметили ни одной складской позиции..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN TMP_LINK
  USE IN DET_6502
  USE IN STO_6502
  DELETE FILE (m.tmpo_pat+m.tmpArr+".dbf")
  RETURN
ENDIF

SELECT 0
CREATE DBF (m.tmpo_pat+m.tmpName) ;
	(	CODE	N( 7), ;
		TMP_ORD	N( 5), ;
		ZONE	C( 2), ;
		QNT		N(10), ;
		QNT_RES N(10), ;
		QNT_REQ N(10), ;
		QNT_BR	N(10)  )
USE (m.tmpo_pat+m.tmpName) EXCLUSIVE ALIAS TMP_BRON

USE (m.base_path+"LINK_ORD") SHARED AGAIN ORDER TAG LIST_CODE ALIAS LNK_6502 IN 0
SELECT DET_6502
SET ORDER TO
SELECT TMP_LINK
SCAN
  IF EMPTY(m.new_list)	&& список еще не создан
    SELECT 0
    USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS TIT_6502
    SEEK VAL(m.n_list)
    SCATTER TO tmpArr
    m.new_list = Doc_Num("LISTS", FIRM, m.arm, DATE())
    APPEND FROM ARRAY m.tmpArr
    m.tmpName = LEFT(LST_NAME, LEN(LST_NAME)-8)+"Пр"+STR(LST_NUM, 6)
    REPLACE	LST_NUM		WITH m.new_list, ;
    		CR_DAT		WITH DATE(),     ;
    		LST_NAME	WITH m.tmpName,  ;
    		WHO_CR		WITH m.user,     ;
    		WHERE_CR	WITH m.arm,      ;
    		ORD_NUM		WITH 0,          ;
    		ORD_COUNT	WITH 0,          ;
    		DATE_REQ	WITH DATE(),	 ;
    		TIME_REQ	WITH TIME()
    m.new_list = STR(m.new_list, 6)
  ENDIF
  SELECT DET_6502
  GO TMP_LINK.LINK
  IF EMPTY(QNT_DUE)
    REPLACE REPLACE LIST_NUM WITH m.new_list, MARK_TMP WITH ""
    SELECT LNK_6502
    DO WHILE SEEK(DET_6502.LIST_NUM+STR(DET_6502.CODE))
      REPLACE LNK_5A10.LIST_NUM WITH m.new_lst
    ENDDO
  ELSE
    SCATTER TO tmpArr
    REPLACE QNT WITH 0, QNT_REQ WITH 0, MARK_TMP WITH ""
    APPEND FROM ARRAY tmpArr
    REPLACE REPLACE LIST_NUM WITH m.new_list, MARK_TMP WITH "", QNT_DUE WITH 0
  ENDIF
  = SEEK(DET_6502.CODE, "STO_6502")
  m.tmpZone = WhatZone(STO_6502.ADDRESS)
  SELECT TMP_BRON
  APPEND BLANK
  REPLACE	CODE	WITH DET_6502.CODE, ;
			ZONE	WITH m.tmpZone,     ;
			QNT		WITH DET_6502.QNT,  ;
			QNT_RES WITH DET_6502.REQ
  SELECT TMP_LINK
ENDSCAN

USE IN LNK_6502

m.tmpName = DBF()
USE (m.base_path+"ORD_T") SHARED AGAIN ORDER TAG DOC_NUM ALIAS OT_6502
*
*  Проверяем: все заявки д. б. либо на бронирование, либо на резервирование
*
SEEK m.n_list
STORE 0 TO m.n1, m.n2
SCAN REST WHILE LIST_NUM = m.n_list
  IF EMPTY(REQ_DATE)
    m.n1 = m.n1+1
  ELSE
    m.n2 = m.n2+1
  ENDIF
ENDSCAN
IF m.n1 # 0 .AND. m.n2 # 0	&& Криминал! Исправляем!
  SEEK m.n_list
  SCAN REST WHILE LIST_NUM = m.n_list
    IF EMPTY(REQ_DATE)
      REPLACE REQ_DATE WITH DATE(), REQ_TIME WITH TIME()
    ENDIF
  ENDSCAN
ENDIF
DELETE FILE (m.tmpName)
SELECT TMP_BRON
INDEX ON CODE TAG CODE
SELECT 0
USE (m.base_path+"ORD_D") SHARED AGAIN ORDER TAG DOC_NUM ALIAS OD_6502
IF m.n2 = 0		&& если только резерв, не будем и смотреть
  SEEK m.n_list
  SCAN REST WHILE LIST_NUM = m.n_list
    IF .NOT. SEEK(CODE, "TMP_BRON")	&& нет в новом списке
      LOOP
    ENDIF
    = SEEK(LIST_NUM+STR(ORD_NUM,3), "OT_6502")
    SELECT TMP_BRON
  
    SELECT OD_6502
  ENDSCAN

&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
&&  Расставить тип брони в TMP_BRON
&&  и создать заявки на бронирование и разбронирование,
&&  расчитать сумму для нового списка
&&  КОЛ-ВО ЗАЯВОК, ТРЕБУЮЩИХ ПОДБОРКИ ДЛЯ НОВОГО И СТАРОГО СПИСКОВ