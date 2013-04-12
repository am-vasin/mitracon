*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Ord_Sto      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Заявки на склад по позиции                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 07.02.2007 ══╝
PROCEDURE Ord_Sto
PARAMETERS tmpCode	&& код позиции

PRIVATE FName, s_sav

m.FName = SYS(3)
m.s_sav = SELECT()
SELECT 0
DO TmpList
IF RECCOUNT("TMP_070207") # 0
  DO V_Tmp
ENDIF
USE IN TMP_070207
DELETE FILE (m.tmpo_path+m.FName+".dbf")
DELETE FILE (m.tmpo_path+m.FName+".cdx")
SELECT (m.s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура TmpList      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временного файла                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.02.2007 ──┘
PROCEDURE TmpList

PRIVATE tmpVal

CREATE DBF (m.tmpo_path+m.FName) ;
	(	LIST_NUM	C( 6),	;
		ORD_NUM		N( 5),	;
		SUM_ORD		C( 4),	;
		ORD_DATE	D,		;
		ORD_TIME	C( 8),	;
		QNT			N(10),	;
		WHO_ORD		C(40),	;
		WHO_COLL	C(40),	;
		SALE		C(30),	;
		CUSTOMER	C(50)	)
USE (m.tmpo_path+m.FName) EXCLUSIVE ALIAS TMP_070207

SELECT 0
USE (m.base_path+"ORD_D") SHARED AGAIN ALIAS OD_070207 ORDER TAG CODE
USE (m.base_path+"ORD_T") SHARED AGAIN ALIAS OT_070207 ORDER TAG DOC_NUM IN 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS LT_070207 ORDER TAG LST_NUM IN 0
USE (m.base_path+"PERSONS") SHARED AGAIN ALIAS P_070207 ORDER TAG CODE IN 0
USE (m.base_path+"SALE_TIT") SHARED AGAIN ALIAS ST_070207 ORDER TAG LIST_NUM IN 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS C_070207 ORDER TAG CUS_CODE IN 0
SET RELATION TO LIST_NUM+STR(ORD_NUM, 3) INTO OT_070207

IF SEEK(m.tmpCode)
  SCAN REST WHILE CODE = m.tmpCode
    SELECT TMP_070207
    APPEND BLANK
    REPLACE	LIST_NUM	WITH OD_070207.LIST_NUM,	;
			ORD_NUM		WITH OD_070207.ORD_NUM,		;
			SUM_ORD		WITH OT_070207.CHECK_S,		;
			ORD_DATE	WITH OT_070207.ORD_DATE,	;
			ORD_TIME	WITH OT_070207.ORD_TIME,	;
			QNT			WITH OD_070207.QNT
    IF SEEK(VAL(OD_070207.LIST_NUM), "LT_070207")	&& Если список еще жив...
      IF SEEK(LT_070207.CLI_CODE, "C_070207")		&&   берем там клиента
        REPLACE CUSTOMER WITH C_070207.CUS_NAME
      ENDIF
    ENDIF
    IF SEEK(OD_070207.LIST_NUM, "ST_070207")		&& Если была продажа...
      IF SEEK(ST_070207.CUS_CODE, "C_070207")		&&   берем там клиента
        REPLACE CUSTOMER WITH C_070207.CUS_NAME
      ENDIF
      m.tmpVal = LEFT(ST_070207.DOC_NUM, 4)+"-"+ALLTRIM(SUBSTR(ST_070207.DOC_NUM, 5))+ ;
                 "/"+ALLTRIM(ST_070207.FIRM)+ ;
                 " от "+DTOC(ST_070207.DOC_DATE)
      REPLACE SALE WITH m.tmpVal
    ENDIF
    IF SEEK(OT_070207.WHO, "P_070207")
      m.tmpVal = ALLTRIM(P_070207.FAMILY)+" "+LEFT(P_070207.NAME, 1)+" "+LEFT(P_070207.S_NAME, 1)
      REPLACE WHO_ORD WITH m.tmpVal
    ENDIF
    IF .NOT. EMPTY(OT_070207.STO_MARK)
      IF SEEK(OT_070207.STO_MARK, "P_070207")
        m.tmpVal = ALLTRIM(P_070207.FAMILY)+" "+LEFT(P_070207.NAME, 1)+" "+LEFT(P_070207.S_NAME, 1)
        REPLACE WHO_COLL WITH m.tmpVal
      ENDIF
    ENDIF
    SELECT OD_070207
  ENDSCAN
ENDIF

USE IN OT_070207
USE IN OD_070207
USE IN ST_070207
USE IN P_070207
USE IN LT_070207
USE IN C_070207

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Tmp        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Просмотр заявок                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.02.2007 ──┘
PROCEDURE V_Tmp


*
*  Переменные состояния для навигации
*

PRIVATE stat_type     && Тип навигации: 0 - нестандартный;
                                        1 - бланк;
                                        2 - BROWSE - таблица;
                                        3 - BROWSE - список.
PRIVATE what_do       && Имя режима.
PRIVATE menu_name     && Имя асинхронного меню.
PRIVATE last_mouse    && Время последнего нажатия звериной кнопки.
PRIVATE win_name      && Имя окна ( окон для BROWSE ).
PRIVATE s_sav         && Номер рабочей области для сохранения и восстановления!

*
*   Заполняем значениями переменные состояния...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD View OF (menu_name) PROMPT "Просмотр заявки" KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Print OF (menu_name) PROMPT "Печать сумм. заявки" KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd, mss

m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = 73

* Список───№──Сумм─────Дата─────Время──Кол-во─Продажа
*!999999 9999 9999 ДД.ММ.ГГГГ ЧЧ:ММ:СС 999999 A999-999999/FFF от ДД.ММ.ГГГГ!
*
*  ....:....!....:....!....:....!  ....:....!....:....!....:....!
*  Клиент ....:....!....:....!....:....!....:....!....:....!
DO D_Wins WITH m.ln, m.wd, "Заявки на склад по позиции", 1, 2
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS STO_070207 ORDER TAG CODE IN 0
= SEEK(tmpCode, "STO_070207")
@ 1, 2 SAY "Позиция: "+ALLTRIM(STO_070207.PREFIX)+" "+ALLTRIM(STO_070207.NAME)+" "+ ;
              IIF(EMPTY(STO_070207.PRODUCER), "", "/"+ALLTRIM(STO_070207.PRODUCER)+"/")
USE IN STO_070207
@ 2, 2 SAY "Список───№──Сумм─────Дата─────Время──Кол-во─Продажа"
@ WROWS()-4,  3 SAY "Заявку подал"
@ WROWS()-4, 35 SAY "Заявку обслужил"
 
m.what_do = "List"

SELECT TMP_070207
INDEX ON DTOS(ORD_DATE)+ORD_TIME TAG HRON DESCENDING
GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl-Enter DO Sw_Mode WITH "Print"

*
*    BROWSE - меню
*
    BROWSE FIELDS LIST_NUM:H="",			;
                  ORD_NUM:H="":P="9999",	;
                  SUM_ORD:H="",				;
                  ORD_DATE:H="",			;
                  ORD_TIME:H="",			;
                  QNT:H="":P="999999",		;
                  SALE:H="",				;
           DUMMY.F:H="" FREEZE DUMMY.F		;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр заявки

    DO V_Ord1 WITH LIST_NUM, ORD_NUM
    m.what_do = "List"

  CASE m.what_do = "Print"    && Печать сумм. заявки

    IF EMPTY(SUM_ORD)
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Суммарная заявка не создана!"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      DO P_Ord_S WITH LIST_NUM, SUM_ORD
    ENDIF
    m.what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
SELECT (m.s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-3,  3 SAY TMP_070207.WHO_ORD
@ WROWS()-3, 35 SAY TMP_070207.WHO_COLL
@ WROWS()-2,  3 SAY "Клиент: "+TMP_070207.CUSTOMER

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
