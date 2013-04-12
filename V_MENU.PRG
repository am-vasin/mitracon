*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла V_Menu       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                             Просмотр меню.                             ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 19.02.2002 ══╝
PROCEDURE V_Menu

PRIVATE tmpLevel, tmpName, tmpKey, tmpTitle

USE (base_path+"S_RIGHTS")
SET FILTER TO .NOT. EMPTY(NAME)

SELECT 0
USE (base_path+"MENU_LST")

tmpTitle = "Основное меню"
tmpLevel = ""
tmpName = SYS(3)
SELECT 0
CREATE DBF (tmpo_path+tmpName) (LINK N(10))
USE (tmpo_path+tmpName) ALIAS TMP_MENU EXCLUSIVE

DO Fil_Tmp

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
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT TMP_MENU

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Вернуться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = FSIZE("P_PROMPT", "MENU_LST")+2

DO D_Wins WITH ln, wd, "Меню системы", 1, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 0 SAY PADC(tmpTitle, WCOLS())
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    

*
*    BROWSE - меню
*
    BROWSE FIELDS MENU_LST.P_PROMPT:H="",  ;
                  P000 = IIF(EMPTY(MENU_LST.COMM_STR), "", " "), ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Просмотр списка

    IF EMPTY(MENU_LST.COMM_STR)
      tmpLevel = STR(MENU_LST.LEVEL1, 2)
      IF .NOT. EMPTY(MENU_LST.LEVEL2)
        tmpLevel = tmpLevel+STR(MENU_LST.LEVEL2, 2)
      ENDIF
      DO Fil_Tmp
    ELSE
      DO V_Rights
    ENDIF
    what_do = "List"

  OTHERWISE

    IF LEN(tmpLevel) = 0
      EXIT
    ELSE
      tmpLevel = LEFT(tmpLevel, LEN(tmpLevel)-2)
      DO Fil_Tmp
    ENDIF
    what_do = "List"

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
SELECT TMP_MENU
tmpName = DBF()
USE
DELETE FILE (tmpName)
SELECT MENU_LST
USE
SELECT S_RIGHTS
USE

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Fil_Tmp      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Заполнение временного файла.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 19.02.2002 ──┘
PROCEDURE Fil_Tmp

SELECT TMP_MENU
SET RELATION TO
ZAP

SELECT MENU_LST
SET ORDER TO TAG LEVELS
SEEK tmpLevel
IF LEN(tmpLevel) # 0
  tmpTitle = ALLTRIM(P_PROMPT)
  SKIP
ELSE
  tmpTitle = "Основное меню"
ENDIF

SCAN REST WHILE LEFT(STR(MENU_LST.LEVEL1,2)+STR(MENU_LST.LEVEL2,2)+  ;
                     STR(MENU_LST.LEVEL3,2), LEN(tmpLevel)) = tmpLevel
  IF LEN(tmpLevel) = 0 .AND. MENU_LST.LEVEL2 # 0
    LOOP
  ENDIF
  IF LEN(tmpLevel)  = 2 .AND. MENU_LST.LEVEL3 # 0
    LOOP
  ENDIF
  SELECT TMP_MENU
  APPEND BLANK
  REPLACE LINK WITH RECNO("MENU_LST")
  SELECT MENU_LST
ENDSCAN
SET ORDER TO

SELECT TMP_MENU
SET RELATION TO LINK INTO MENU_LST
GO TOP

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Rights     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Санкции доступа.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.02.2002 ──┘
PROCEDURE V_Rights

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
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT S_RIGHTS
GO TOP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Доступ" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Users OF (menu_name) PROMPT "Пользователи" KEY Ctrl-A, "Tab"
ON SELECTION PAD Users OF (menu_name) DO Sw_Mode WITH "Users"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = FSIZE("NAME")+2

DO D_Wins WITH ln, wd, ALLTRIM(MENU_LST.P_PROMPT), 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Users"

*
*    BROWSE - меню
*
    BROWSE FIELDS S000 = IIF(EMPTY(SUBSTR(MENU_LST.ACC_LABELS, RIGHT, 1)), "√", " "):1:H="", ;
                  NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Смена доступа
    
    SELECT MENU_LST
    IF EMPTY(SUBSTR(MENU_LST.ACC_LABELS, S_RIGHTS.RIGHT, 1))
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS, S_RIGHTS.RIGHT, 1, "*")
    ELSE
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS, S_RIGHTS.RIGHT, 1, " ")
    ENDIF
    SELECT S_RIGHTS

    what_do = "List"

  CASE what_do = "Users"    && Пользователи
    
    DO R_Users WITH S_RIGHTS.RIGHT
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
SELECT (s_sav)

RETURN
