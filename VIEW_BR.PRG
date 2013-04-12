*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла View_Br      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                            Бронь по позиции                            ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 28.09.2005 ══╝
PROCEDURE View_Br
PARAMETERS c_code
*
*  c_code - код позиции
*

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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
PRIVATE tmpName, tmpTit, qnt_tot
PRIVATE hRez, hWork, hBron
STORE 0 TO m.hRez, m.hWork, m.hBron
m.tmpName = SYS(3)
m.qnt_tot = 0
m.tmpTit = ""

s_sav = SELECT()
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
  ( QNT			N( 8), ;
  	QNT_REZ		N( 8), ;
  	QNT_WORK	N( 8), ;
  	QNT_BRON	N( 8), ;
    PRICE		C(14), ;
    CUS_NAME	C(50), ;
    LIST_N		C( 6), ;
    ARM_NAME	C(16), ;
    MAN_NAME	C(40)  )
USE (m.tmpo_path+m.tmpName) ALIAS TMP_BRON
DO Prp_Bron
SELECT TMP_BRON
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"По этой позиции ничего не забронировано!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  SELECT (m.s_sav)
  RETURN
ENDIF
GO TOP
  
SELECT 0
DO Use_Dummy
SELECT TMP_BRON

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(RECCOUNT(), WROWS("")-11)   && Количество видимых строк BROWSE
wd = 74

*    Бронь        Цена    Клиент
*!99999999 99999999999999 ....:....!....:....!....:....!....:....!....:....!!

*   Всего  Рабочее место    Менеджер                                 Список
*!99999999 ....:....!....:. ....:....!....:....!....:....!....:....! 999999 !
DO D_Wins WITH ln, wd, m.tmpTit, 0, 4
@ 1,  5 SAY "Бронь"
@ 1, 18 SAY "Цена"
@ 1, 26 SAY "Клиент"
@ WROWS()-6,  4 SAY "Всего"
@ WROWS()-6, 11 SAY "Рабочее место"
@ WROWS()-6, 28 SAY "Менеджер"
@ WROWS()-6, 69 SAY "Список"
@ WROWS()-5,  2 SAY STR(m.qnt_tot, 8) COLOR SCHEME 1
@ WROWS()-4,  3 SAY "Резерв:   Всего "
@ WROWS()-4, COL() SAY STR(m.hRez, 6) COLOR SCHEME 1
@ WROWS()-4, COL() SAY " в заказе"
@ WROWS()-3,  3 SAY "В работе: Всего "
@ WROWS()-3, COL() SAY STR(m.hWork, 6) COLOR SCHEME 1
@ WROWS()-3, COL() SAY " в заказе"
@ WROWS()-2,  3 SAY "Бронь:    Всего "
@ WROWS()-2, COL() SAY STR(m.hBron, 6) COLOR SCHEME 1
@ WROWS()-2, COL() SAY " в заказе"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS QNT:H="":P="99999999",  ;
                  PRICE:H="",             ;
                  CUS_NAME:H="",          ;
           DUMMY.F:H="" FREEZE DUMMY.F    ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

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
USE IN TMP_BRON
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
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
@ WROWS()-5, 11 SAY ARM_NAME COLOR SCHEME 1
@ WROWS()-5, 28 SAY MAN_NAME COLOR SCHEME 1
@ WROWS()-5, 69 SAY LIST_N   COLOR SCHEME 1
@ WROWS()-4, 35 SAY STR(QNT_REZ, 6) COLOR SCHEME 1
@ WROWS()-3, 35 SAY STR(QNT_WORK, 6) COLOR SCHEME 1
@ WROWS()-2, 35 SAY STR(QNT_BRON, 6) COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_Bron     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Подготовка списка брони                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 28.09.2005 ──┘
PROCEDURE Prp_Bron

PRIVATE s_sav, tmpPrice, tmpMan
PRIVATE tmpRez, tmpWork, tmpBron

m.s_sav = SELECT()

SELECT 0
USE m.base_path+"STOCK" SHARED AGAIN ORDER TAG CODE ALIAS STO_5928
SEEK m.c_code
m.tmpTit = ALLTRIM(PREFIX)+" "+ALLTRIM(NAME)+ ;
           IIF(EMPTY(PRODUCER), "", " /"+ALLTRIM(PRODUCER)+"/")

USE m.base_path+"ACCOUNT" SHARED AGAIN ORDER TAG CUS_CODE ALIAS CUS_5928

SELECT 0
USE m.base_path+"PERSONS" SHARED AGAIN ORDER TAG CODE ALIAS MAN_5928

SELECT 0
USE m.base_path+"ARM_LST" SHARED AGAIN ORDER TAG ARM_C ALIAS ARM_5928

SELECT 0
USE m.base_path+"ORD_T" SHARED AGAIN ORDER TAG DOC_NUM ALIAS OT_5928

SELECT 0
USE m.base_path+"ORD_D" SHARED AGAIN ORDER TAG DOC_NUM ALIAS OD_5928

SELECT 0
USE m.base_path+"LIST_TIT" SHARED AGAIN ORDER TAG LST_NUM ALIAS TIT_5928
SET RELATION TO WHO_CR   INTO MAN_5928, ;
                LST_ARM  INTO ARM_5928, ;
                CLI_CODE INTO CUS_5928

SELECT 0
USE m.base_path+"LIST_DET" SHARED AGAIN ORDER TAG BRON ALIAS DET_5928
SET RELATION TO VAL(LIST_NUM) INTO TIT_5928

SEEK c_code
SCAN REST WHILE CODE = m.c_code
  IF .NOT. FOUND("TIT_5928")
    LOOP
  ENDIF
  IF .NOT. EMPTY(TIT_5928.SAL_NUM)
    LOOP
  ENDIF
  m.tmpPrice = TRANSFORM(DET_5928.SAL_PRICE, "99 999 999.99") + ;
               IIF(TIT_5928.IS_USD, "$", "р")
  m.tmpMan = ALLTRIM(MAN_5928.FAMILY)+" "+LEFT(MAN_5928.NAME, 1)+" "+LEFT(MAN_5928.S_NAME, 1)
  STORE 0 TO m.tmpRez, m.tmpWork, m.tmpBron
  SELECT OD_5928
  SEEK DET_5928.LIST_NUM
  SCAN REST WHILE DET_5928.LIST_NUM = LIST_NUM FOR CODE = DET_5928.CODE
    IF SEEK(LIST_NUM+STR(ORD_NUM, 3), "OT_5928")
      DO CASE
      CASE .NOT. EMPTY(OT_5928.STO_MARK)
        m.tmpBron = m.tmpBron+QNT
        m.hBron = m.hBron+QNT
      CASE .NOT. EMPTY(OT_5928.REQ_DATE)
        m.tmpWork = m.tmpWork+QNT
        m.hWork = m.hWork+QNT
      OTHERWISE
        m.tmpRez = m.tmpRez+QNT
        m.hRez = m.hRez+QNT
      ENDCASE
    ENDIF
  ENDSCAN
  SELECT TMP_BRON
  APPEND BLANK
  REPLACE QNT		WITH DET_5928.QNT_REQ,  ;
  		  QNT_REZ	WITH m.tmpRez,			;
  		  QNT_WORK	WITH m.tmpWork,			;
  		  QNT_BRON	WITH m.tmpBron,			;
          PRICE		WITH m.tmpPrice,        ;
          CUS_NAME	WITH CUS_5928.CUS_NAME, ;
          LIST_N	WITH DET_5928.LIST_NUM, ;
          ARM_NAME	WITH ARM_5928.ARM_N,    ;
          MAN_NAME	WITH m.tmpMan

  SELECT DET_5928
  m.qnt_tot = m.qnt_tot+QNT_REQ
ENDSCAN

USE IN DET_5928
USE IN TIT_5928
USE IN MAN_5928
USE IN CUS_5928
USE IN ARM_5928
USE IN OT_5928
USE IN OD_5928

SELECT (m.s_sav)

RETURN
