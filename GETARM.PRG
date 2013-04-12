*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла GetArm       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                         Выбор рабочего места.                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 18.04.2000 ══╝
PROCEDURE GetArm
PARAMETERS startV

PRIVATE tmpPoint, armPoint, s_sav, retVal

s_sav = SELECT()
SELECT 0

tmpPoint = 1
DELETE FILE (tmpo_path+"SQUARES.DBF")
DELETE FILE (tmpo_path+"SQUARES.CDX")

CREATE DBF (tmpo_path+"SQUARES") ;
  ( SQ_CHAR C( 1), ;
    SQ_NAME C(12)  )
INDEX ON SQ_CHAR TAG SQ_CHAR
APPEND BLANK
REPLACE SQ_NAME WITH "Все площадки"

SELECT 0
DELETE FILE (tmpo_path+"GETARM.DBF")
DELETE FILE (tmpo_path+"GETARM.CDX")

CREATE DBF (tmpo_path+"GETARM") ;
  ( SQ_CHAR  C( 1), ;
    ARM_CODE N( 4), ;
    ARM_NAME C(16)  )
INDEX ON SQ_CHAR+SYS(15, lwr, ARM_NAME)  TAG SQ_CHAR
INDEX ON SYS(15, lwr, ARM_NAME) TAG ARM_NAME ADDITIVE

SELECT 0
USE (base_path+"ARM_LST") ALIAS ARMS_0418 AGAIN
SCAN
  IF .NOT. EMPTY(ARM_S)
    IF .NOT. SEEK(ARM_S, "SQUARES")
      SELECT SQUARES
      APPEND BLANK
      REPLACE SQ_CHAR WITH ARMS_0418.ARM_S, ;
              SQ_NAME WITH "Площадка "+ARMS_0418.ARM_S
      IF sys_char = SQ_CHAR
        tmpPoint = RECNO()
      ENDIF
    ENDIF
  ENDIF
  SELECT GETARM
  APPEND BLANK
  REPLACE SQ_CHAR  WITH ARMS_0418.ARM_S,    ;
          ARM_CODE WITH ARMS_0418.ARM_CODE, ;
          ARM_NAME WITH ARMS_0418.ARM_N
  IF TYPE("startV") = "N"
    IF startV = ARM_CODE
      armPoint = RECNO()
    ENDIF
  ENDIF
  SELECT ARMS_0418
ENDSCAN

USE
SELECT SQUARES
GO tmpPoint
SELECT GETARM
IF EMPTY(SQUARES.SQ_CHAR)
  SET ORDER TO TAG ARM_NAME
ELSE
  SET ORDER TO TAG SQ_CHAR
ENDIF

IF TYPE("armPoint") = "N"
  GO armPoint
  IF SQ_CHAR # SQUARES.SQ_CHAR
    GO 1 IN SQUARES
  ENDIF
ELSE
  GO TOP
ENDIF


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

SELECT 0
DO Use_Dummy
SELECT GETARM

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Square OF (menu_name) PROMPT "Площадка" KEY Ctrl-A, "F1"
ON SELECTION PAD Square OF (menu_name) DO Sw_Mode WITH "Square"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = FSIZE("ARM_NAME")

DO D_Wins WITH ln, wd, "Рабочие места", 1, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 2 SAY SQUARES.SQ_NAME
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1    DO Sw_Mode WITH "Square"

*
*    BROWSE - меню
*
    BROWSE KEY ALLTRIM(SQUARES.SQ_CHAR);
           FIELDS ARM_NAME:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбрать

    retVal = ARM_CODE
    EXIT

  CASE what_do = "Square"    && Площадка

    DO Get_Sq
    what_do = "List"

  OTHERWISE

    retVal = IIF(TYPE("startV") = "N", startV, -1)
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
USE
SELECT SQUARES
USE

DELETE FILE (tmpo_path+"SQUARES.DBF")
DELETE FILE (tmpo_path+"SQUARES.CDX")
DELETE FILE (tmpo_path+"GETARM.DBF")
DELETE FILE (tmpo_path+"GETARM.CDX")

SELECT (s_sav)

RETURN retVal

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Sq       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Выбор площадки.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.04.2000 ──┘
PROCEDURE Get_Sq

PRIVATE r_sav
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

s_sav = SELECT()

SELECT SQUARES

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выьрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-18   && Количество видимых строк BROWSE
wd = FSIZE("SQ_NAME")

DO D_Wins WITH ln, wd, "Площадки", 0, 0
what_do = "List"

r_sav = RECNO()
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS SQ_NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Просмотр списка

    IF r_sav # RECNO()
      SELECT GETARM
      IF EMPTY(SQUARES.SQ_CHAR)
        SET ORDER TO TAG ARM_NAME
      ELSE
        SET ORDER TO TAG SQ_CHAR
      ENDIF
      GO TOP IN GETARM
    ENDIF
    EXIT

  OTHERWISE

    GO r_sav
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
