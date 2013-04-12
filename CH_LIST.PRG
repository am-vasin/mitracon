*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Ch_List      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Выбор списка на рабочем месте                      ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 07.09.2006 ══╝
PROCEDURE Ch_List
PARAMETERS prmArmC


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

PRIVATE tmpArmN, retValue, seekShab, seekMode, tmpShab, tmpRec
m.seekShab = SPACE(40)
m.seekMode = 1
m.s_sav = SELECT()
m.retValue = "      "

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"ARM_LST") SHARED AGAIN ALIAS AL_6907 ORDER TAG ARM_C
SEEK m.prmArmC
m.tmpArmN = ALLTRIM(ARM_N)
USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS LT_6907 ORDER TAG LST_ARM
USE (m.base_path+"ACCOUNT") SHARED AGAIN ALIAS AC_6907 ORDER TAG CUS_CODE IN 0
SET RELATION TO CLI_CODE INTO AC_6907
PRIVATE mss
IF .NOT. SEEK(m.prmArmC)
  DIMENSION mss[4]
  mss[1] = ""
  mss[2] = CHR(0)+'За рабочим местом "'+m.tmpArmN+'"'
  mss[3] = CHR(0)+"не числится ни одного списка!"
  mss[4] = ""
  DO Out_Mess WITH 7, "mss"
  USE
  USE IN AC_6907
  SELECT (m.s_sav)
  RETURN m.retValue
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD View OF (menu_name) PROMPT "Содержимое" KEY Ctrl-A, "Tab"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-5   && Количество видимых строк BROWSE
m.wd = 76

DO D_Wins WITH m.ln, m.wd, 'Списки рабочего места "'+m.tmpArmN+'"', 0, 0
m.what_do = "List"
@ 1, 2 SAY ;
"Фрм──Номер────Дата─────Клиент───────────────────────────────────Сумма──────П"
*FFF 999999 99.99.9999 ....:....!....:....!....:....!....:. 9999999999.9999 П

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"
    ON KEY LABEL Tab   DO Sw_Mode WITH "View"
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE KEY m.prmArmC ;
           FIELDS FIRM:H="",		;
                  LST_NUM:H="",		;
                  CR_DAT:H="",		;
                  tmpCust = LEFT(AC_6907.CUS_NAME, 36):H="",;
                  R_TOTAL:H="",		;
                  tmpSaled = IIF(EMPTY(SAL_NUM), " ", "√"):1:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    m.retValue = STR(LST_NUM, 6)
    EXIT

  CASE m.what_do = "View"      && Содержимое списка

    DO ViewList WITH LST_NUM
    m.what_do = "List"

  CASE m.what_do = "Seek"      && Поиск

    DO Seek_Lst
    IF .NOT. EMPTY(m.seekShab) .AND. m.seekMode # 0
      m.tmpShab = ALLTRIM(m.seekShab)
      m.tmpShab = SYS(15, m.lwr, m.tmpShab)
      m.tmpRec = RECNO()
      IF m.seekMode = 1
        SEEK m.prmArmC
      ELSE
        SKIP
        IF EOF()
          GO BOTTOM
        ENDIF
      ENDIF
      LOCATE REST FOR m.tmpShab $ SYS(15, m.lwr, AC_6907.CUS_NAME)
      IF .NOT. FOUND()
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"Ничего не найдено..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        GO m.tmpRec
      ENDIF
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
USE IN AC_6907
USE IN LT_6907
SELECT (m.s_sav)

RETURN m.retValue

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_Lst     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Поиск списка по клиенту                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.09.2006 ──┘
PROCEDURE Seek_Lst
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

*┌────────────────────────────────────────────┐
*│ ┌─ Что ищем ─────────────────────────────┐ │
*│ │....:....!....:....!....:....!....:....!│ │
*│ └────────────────────────────────────────┘ │
*│   ( ) Искать с начала  ( ) Искать далее    │
*│     < OK Ctrl-W > < Отказаться Esc >       │
*└────────────────────────────────────────────┘

PRIVATE ex
m.ex = 1
m.seekMode = MAX(m.seekMode, 1)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 48, "Поиск списка по клиенту"
DO Sun_Bord WITH  2, 3,  4, 44, " Что ищем "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 4 GET m.seekShab PICTURE "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
@ 5, 5 GET m.seekMode PICTURE "@*RH Искать с начала;Искать далее"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF m.ex # 1
  m.seekMode = 0
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
