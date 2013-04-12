*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Pos_Ord      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Заявки на поставку по позиции.                     ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 06.10.2005 ══╝
PROCEDURE Pos_Ord
PARAMETERS prmCode


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
PRIVATE tmpName
m.tmpName = SYS(3)
DO P_Lst_Ord
IF RECCOUNT("TMP_LIST") = 0
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Эта позиция никогда не заказывалась..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN TMP_LIST
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
  RETURN
ENDIF
m.s_sav = SELECT()
PRIVATE tmpPosName
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS STO_5A06 ORDER TAG CODE
SEEK m.prmCode
m.tmpPosName = ALLTRIM(STO_5A06.PREFIX)
m.tmpPosName = m.tmpPosName+IIF(.NOT. EMPTY(m.tmpPosName), " ", "")+ ;
               ALLTRIM(STO_5A06.NAME)+ ;
               IIF(.NOT. EMPTY(STO_5A06.PRODUCER), " /"+ALLTRIM(STO_5A06.PRODUCER)+"/", "")
USE
DO Use_Dummy
SELECT TMP_LIST

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Orders OF (menu_name) PROMPT "Менеджеры" KEY Ctrl-A, "Enter"
ON SELECTION PAD Orders OF (menu_name) DO Sw_Mode WITH "Orders"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = MIN(WROWS("")-11, RECCOUNT())   && Количество видимых строк BROWSE
m.wd = 76

DO D_Wins WITH m.ln, m.wd, "Заказы на поставку по позиции", 1, 1
@ 1, 2 SAY "Позиция "+m.tmpPosName
@ 2, 2 SAY ;
 "───Заказ №──Дата──────Поставщик─────────────────────────────────────Кол-во─М"
* 1234567890 dd.mm.yyyy ....:....!....:....!....:....!....:....!....: 999999 M
* ....:....!....:....!....:....!..  ....:....!....:....!....:....!..
@ WROWS()-3,  3 SAY "Кто сформировал заявку"
@ WROWS()-3, 43 SAY "Примечание"

m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS ORD_NUM:H="",   ;
                  ORD_DATE:H="",  ;
                  F000 = LEFT(SUPPL, 45):H="", ;
                  QNT:H="":P="999999", ;
                  IS_CONT:H="",        ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Orders")
    ENDIF

  CASE m.what_do = "Orders"    && Просмотр списка

    DO Pos_Ord1 WITH ORD_FIRM, ORD_DATE, ORD_NUM, m.prmCode
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
USE IN TMP_LIST
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".cdx")

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
@ WROWS()-2,  3 SAY MAN COLOR SCHEME 1
@ WROWS()-2, 43 SAY NOTE COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура P_Lst_Ord    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│           Подготовка временного файла с заявками по позиции            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.10.2005 ──┘
PROCEDURE P_Lst_Ord

PRIVATE s_sav

m.s_sav = SELECT()
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	ORD_NUM		C(10), ;
		ORD_DATE	D,     ;
		ORD_FIRM	C( 3), ;
		SUPPL		C(50), ;
		MAN			C(32), ;
		NOTE		C(32), ;
		QNT			N( 6), ;
		IS_CONT		C( 1)  )
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_LIST

SELECT 0
USE (m.base_path+"LINK_ORD") SHARED AGAIN ALIAS LINK_5A06 ORDER TAG ORD_CODE

SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS SUP_5A06 ORDER TAG CUS_CODE

SELECT 0
USE (m.base_path+"PERSONS") SHARED AGAIN ALIAS MAN_5A06 ORDER TAG CODE

SELECT 0
USE (m.base_path+"ORD_TIT") SHARED AGAIN ALIAS TIT_5A06 ORDER TAG ORD_NUM
SET RELATION TO SUP_CODE INTO SUP_5A06, WHO_ORD INTO MAN_5A06

SELECT 0
USE (m.base_path+"ORDER") SHARED AGAIN ALIAS ORD_5A06 ORDER TAG CODE
SET RELATION TO FIRM+LEFT(DTOS(ORD_DATE), 4)+ORD_NUM INTO TIT_5A06,   ;
                FIRM+LEFT(DTOS(ORD_DATE), 4)+ORD_NUM+STR(CODE) INTO LINK_5A06

SEEK m.prmCode
SCAN REST WHILE CODE = m.prmCode
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE	ORD_NUM		WITH ORD_5A06.ORD_NUM, ;
			ORD_DATE	WITH ORD_5A06.ORD_DATE,;
			ORD_FIRM	WITH ORD_5A06.FIRM,    ;
			SUPPL		WITH SUP_5A06.CUS_NAME,;
			MAN			WITH ALLTRIM(MAN_5A06.FAMILY)+" "+ ;
							 LEFT(MAN_5A06.NAME, 1)  +" "+ ;
							 LEFT(MAN_5A06.S_NAME, 1),     ;
			QNT			WITH ORD_5A06.QNT,     ;
			NOTE		WITH TIT_5A06.NOTE,    ;
			IS_CONT		WITH IIF(FOUND("LINK_5A06"), "√", " ")
  SELECT ORD_5A06
ENDSCAN

USE IN ORD_5A06
USE IN MAN_5A06
USE IN SUP_5A06
USE IN TIT_5A06
USE IN LINK_5A06

SELECT TMP_LIST
INDEX ON DTOS(ORD_DATE) TAG HRON
SET ORDER TO TAG HRON DESCENDING
GO TOP

SELECT (m.s_sav)

RETURN
