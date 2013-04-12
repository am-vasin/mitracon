*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Cls_Refr     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                         Сверка списка классов                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 09.12.2005 ══╝
PROCEDURE Cls_Refr
PARAMETERS prmCus, prmFile, nameMode

*
* prmCus	- код поставщика,
* prmFile	- название файла поставщика,
* nameMode  - 0 - оставляем свои имена (по умолчанию),
*             1 - устанавливаем имена поставщика,
*             2 - всякий раз спрашиваем.
*

* Структура файла поставщика
*	CODE - код класса
*   NAME - название класса
*

PRIVATE s_sav, keyCus, tmpCode, tmpMode, mss, tmpName, ret_v

m.tmpName = SYS(3)

IF TYPE("m.nameMode") = "N"
  m.tmpMode = IIF(BETWEEN(m.nameMode, 0, 2), m.nameMode, 0)
ELSE
  m.tmpMode = 0
ENDIF
m.keyCus = STR(prmCus)
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"CLASSES") SHARED AGAIN ORDER TAG CODE ALIAS CL_5C09
SELECT 0
USE (m.base_path+"TEACH_U") SHARED AGAIN ORDER TAG CODE ALIAS TC_5C09
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
  ( NAME C(FSIZE("NAME_CUS", "TC_5C09")) )
USE (m.tmpo_path+m.tmpName) ALIAS TMP_LIST
SELECT 0
USE (m.prmFile) SHARED AGAIN ALIAS SRC_5C09
SCAN
  m.tmpCode = IIF(TYPE("CODE")="C", CODE, STR(CODE, 15))
  IF SEEK("C"+m.keyCus+m.tmpCode, "TC_5C09")
    SELECT TC_5C09
    REPLACE NAME_CUS WITH ALLTRIM(SRC_5C09.NAME)
    IF m.tmpMode = 0
      SELECT SRC_5C09
      LOOP
    ENDIF
    = SEEK(TC_5C09.CODE_OWN, "CL_5C09")
    IF .NOT. ALLTRIM(TC_5C09.NAME_CUS) == ALLTRIM(CL_5C09.NAME)
      SELECT CL_5C09
      IF m.tmpMode = 1
        REPLACE NAME WITH TC_5C09.NAME_CUS
      ELSE
        DIMENSION mss[5]
        mss[1] = ""
        mss[2] = CHR(0)+"Класс поставщика: "+ALLTRIM(TC_5C09.NAME_CUS)
        mss[3] = CHR(0)+"Наше название: "+ALLTRIM(CL_5C09.NAME)
        mss[4] = CHR(0)+"  Переименовать класс? [ Да ] [ Нет ]"
        mss[5] = ""
        IF Out_Mess(5, "mss") = 1
          REPLACE NAME WITH TC_5C09.NAME_CUS
        ENDIF
      ENDIF
    ENDIF
    SELECT SRC_5C09
    LOOP
  ENDIF
  IF .NOT. Connect(m.tmpCode, NAME)
    SELECT TMP_LIST
    APPEND BLANK
    REPLACE NAME WITH ALLTRIM(SRC_5C09.NAME)
    SELECT SRC_5C09
  ENDIF
ENDSCAN

USE
USE IN TC_5C09
USE IN CL_5C09
m.ret_v = RECCOUNT("TMP_LIST") = 0
IF .NOT. m.ret_v
  DO View_Tmp
ENDIF
USE IN TMP_LIST
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
SELECT (m.s_sav)
RETURN m.ret_v

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Connect      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Привязка класса                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 09.12.2005 ──┘
PROCEDURE Connect
PARAMETERS pCode, pName

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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌────────────────────────────────────────────────────────────────────┐
*│ ┌─ Класс поставщика ─────────────────────────────────────────────┐ │
*│ │....:....!....:....!....:....!....:....!....:....!....:....!....│ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│[ ] ....:....!....:....!....:....!....:....!....:....!....:....!....│
*│                    < OK Ctrl-W > < Отказаться Esc >                │
*└────────────────────────────────────────────────────────────────────┘

PRIVATE ex, tmpCls, tmpNm, tmpSw, tmpRet, s_sav
m.ex = 1
m.tmpSw  = .F.
m.tmpRet = .F.
m.tmpCls = "            "
m.tmpNm  = SPACE(64)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 72, "Укажите соответствие"
DO Sun_Bord WITH  2,  3,  4, 68, " Класс поставщика "
@ 3,  4 SAY m.pName
@ WROWS()-1, 2 SAY PADC(" Ins - Коррекция списка классов ", WCOLS()-4, "─")

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 5,  2 GET m.tmpSw PICTURE "@*C " VALID C_Cls()
@ 5,  6 GET m.tmpNm WHEN .F.

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

ON KEY LABEL Ins DO Classes
READ CYCLE
ON KEY

IF m.ex = 1
  *
  * Отрабатываем бланк
  IF .NOT. EMPTY(m.tmpCls)
    m.s_sav = SELECT()
    SELECT TC_5C09
    APPEND BLANK
    REPLACE TEACH_TYPE	WITH "C",      ;
            CUS_CODE	WITH m.prmCus, ;
            CODE_CUS	WITH m.pCode,  ;
            CODE_OWN	WITH m.tmpCls, ;
            NAME_CUS	WITH m.pName
    SELECT (m.s_sav)
    m.tmpRet = .T.
  ENDIF
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.tmpRet

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура C_Cls        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                              Выбор класса                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 09.12.2005 ──┘
PROCEDURE C_Cls

DO Ch_Class WITH m.tmpCls, m.tmpNm
m.tmpSw = .F.
SHOW GET m.tmpSw
SHOW GET m.tmpNm

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_Tmp     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Просмотр временного файла                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 14.01.2006 ──┘
PROCEDURE View_Tmp

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
SELECT TMP_LIST
GO TOP

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
m.ln = MIN(RECCOUNT(), WROWS("")-11)   && Количество видимых строк BROWSE
m.wd = FSIZE("NAME")

DO D_Wins WITH m.ln, m.wd, "Вы не определили следующие классы поставщика", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

*  CASE m.what_do = "Choose"    && Просмотр списка

*    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    m.what_do = "List"

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
