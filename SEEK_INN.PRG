*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Seek_INN     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                         Поиск клиента по ИНН.                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 02.09.2005 ══╝
PROCEDURE Seek_INN
PARAMETERS prmINN, prmKPP

PRIVATE s_sav, tmpINN, tmpKPP, res, tmpKey, tmpRec, is_list

m.is_list = .F.
m.res = 0
m.tmpINN = ""
m.tmpKPP = ""
DO Get_INN WITH 0, m.tmpINN, m.tmpKPP
IF .NOT. Fill_Blank()
  RETURN m.res
ENDIF
m.tmpKey = m.tmpINN+IIF(.NOT. EMPTY(m.tmpKPP), m.tmpKPP, "")
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"CUS_BIL") SHARED AGAIN ALIAS CB_5902 ORDER TAG INN
IF .NOT. SEEK(m.tmpKey)
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Клинет не найден!"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN CB_5902
  SELECT (m.s_sav)
  RETURN m.res
ENDIF
m.tmpRec = RECNO()
SKIP
IF .NOT. EOF()
  m.is_list = CB_5902.INN+CB_5902.KPP = m.tmpKey
ENDIF
GO m.tmpRec

IF m.is_list
  m.res = Choose_C()
ELSE
  USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS CUS_5902 ORDER TAG CUS_CODE IN 0
  IF SEEK(CB_5902.CUS_CODE, "CUS_5902")
    IF EMPTY(CUS_5902.DATE_OFF)
      m.res = RECNO("CUS_5902")
    ELSE
      PRIVATE mss
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Клинет не найден!"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
    ENDIF
  ENDIF
  USE IN CUS_5902
ENDIF

USE IN CB_5902
SELECT (m.s_sav)

RETURN m.res

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Fill_Blank   Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Заполнение бланка.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.09.2005 ──┘
PROCEDURE Fill_Blank
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

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌─────────────────────────────────────────┐
*│                                         │
*│ ИНН ....:....!..   КПП ....:....!....:. │
*│                                         │
*│     < OK Ctrl-W > < Отказаться Esc >    │
*└─────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 45, "Что ищем?"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  3 SAY "ИНН" GET m.tmpINN
@ 3, 22 SAY "КПП" GET m.tmpKPP
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1 .AND. .NOT. EMPTY(m.tmpINN)

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Choose_c     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Выбор клиента из списка                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.09.2005 ──┘
PROCEDURE Choose_c

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
PRIVATE res, mss
DIMENSION mss[3]
mss[1] = ""
mss[2] = CHR(0)+"Этот клиент удален!"
mss[3] = ""

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()
m.res = 0

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS CUS_5902 ORDER TAG CUS_CODE
SELECT CB_5902
SET RELATION TO CUS_CODE INTO CUS_5902

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Main OF (menu_name) PROMPT "Головное предпр." KEY Ctrl-A, "Ins"
ON SELECTION PAD Main OF (menu_name) DO Sw_Mode WITH "Main"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd, r_sav
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 74

DO D_Wins WITH ln, wd, "Клиенты", 0, 0
what_do = "List"
@ 1, 2 SAY ;
"Г────Клиент──────────────────────────────────ИНН─────────КПП─────────────У"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Main"

*
*    BROWSE - меню
*
    BROWSE KEY m.tmpKey  ;
           FIELDS f001 = IIF(CB_5902.MAIN_FIRM, "√", " "):H="", ;
                  f000 = LEFT(CUS_5902.CUS_NAME,40):H="",   ;
                  CB_5902.INN:H="", ;
                  CB_5902.KPP:H="", ;
                  d000 = IIF(EMPTY(CUS_5902.DATE_OFF), " ", "√"):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Main"    && Головное предприятие

    IF CB_5902.MAIN_FIRM
      REPLACE CB_5902.MAIN_FIRM WITH .F.
    ELSE
      m.r_sav = RECNO("CB_5902")
      SELECT CB_5902
      SEEK m.tmpINN
      REPLACE REST WHILE INN = m.tmpINN MAIN_FIRM WITH .F.
      GO m.r_sav
      REPLACE CB_5902.MAIN_FIRM WITH .T.
    ENDIF
    what_do = "List"

  CASE what_do = "Choose"    && Просмотр списка

    IF EMPTY(CUS_5902.DATE_OFF)
      m.res = RECNO("CUS_5902")
      EXIT
    ENDIF
    DO Out_Mess WITH 7, "mss"
    what_do = "List"

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
USE IN CUS_5902
SELECT (s_sav)

RETURN m.res
