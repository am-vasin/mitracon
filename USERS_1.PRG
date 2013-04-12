*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                  Список пользователей системы "Склад"                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.01.97 17:19:41 ═╝
PROCEDURE Users_1
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
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*     Список сотрудников
*
USE PERSONS ORDER TAG NAME
PRIVATE cli_c, cli_n, cli_q, cli_wd, mss
cli_q = 0
cli_wd = FSIZE("FAMILY")+6
SCAN
  cli_q = cli_q+1
  DIMENSION cli_c(cli_q), cli_n(cli_q)
  mss = ALLTRIM(FAMILY)
  IF .NOT. EMPTY(NAME)
    mss = mss+" "+LEFT(ALLTRIM(NAME),1)+". "
    IF .NOT. EMPTY(S_NAME)
      mss = mss+LEFT(ALLTRIM(S_NAME),1)+"."
    ENDIF
  ENDIF
  cli_c(cli_q) = CODE
  cli_n(cli_q) = PADR(mss, cli_wd)
ENDSCAN
RELEASE mss

USE
IF cli_q = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Список сотрудников пуст!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

RELEASE mss
USE RIGHTS 
PRIVATE rght_c, rght_n, rght_q, cnt
cnt = PADR("Нет доступа", FSIZE("NAME", "RIGHTS"))
rght_q = 1
DIMENSION rght_c(rght_q), rght_n(rght_q)
rght_c(rght_q) = 0
rght_n(rght_q) = cnt

SCAN
  rght_q = rght_q+1
  DIMENSION rght_c(rght_q), rght_n(rght_q)
  rght_c(rght_q) = RIGHT
  rght_n(rght_q) = NAME
ENDSCAN

IF rght_q = 1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Список прав доступа пуст!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

SET ORDER TO TAG RIGHT

DO Use_Dummy

SELECT 0
USE USERS
SCAN
  mss = ASCAN(cli_c, user_code)
  IF mss # 0
    REPLACE USER_NAME WITH cli_n(mss)
  ENDIF
ENDSCAN
SET ORDER TO TAG USER_NAME
SET RELATION TO RIGHTS INTO RIGHTS
GO TOP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

DEFINE PAD Add  OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = 100
wd = FSIZE("USER_NAME", "USERS")+FSIZE("NAME", "RIGHTS")+1

DO D_Wins WITH ln, wd, "Список пользователей программы СКЛАД", 1, 0
@ 1, 2 SAY PADR("Фамилия",FSIZE("USER_NAME", "USERS")+1)+PADR("Уровень доступа",FSIZE("NAME", "RIGHTS"))
what_do = IIF(BOF() .OR. EOF(), "Add", "list")

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "Add"
    ON KEY LABEL Del   Do Sw_Mode WITH "Del"

*
*    BROWSE - меню
*
    BROWSE FIELDS USER_NAME:H="",     ;
                  F001 = IIF(FOUND("RIGHTS"), RIGHTS.NAME, cnt):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && Коррекция

    DO Add_User WITH "Corr"
    what_do = "list"

  CASE what_do = "Add"     && Добавление

    DO Add_User WITH "Add"
    what_do = IIF(BOF() .OR. EOF(), "Exit", "list")

  CASE what_do = "Del"     && Удаление

    IF S_RIGHTS # 0
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+ALLTRIM(USER_NAME)+" имеет доступ к программе ПОДАЖИ!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      REPLACE DATE_OFF WITH DATE()
    ENDIF
    what_do = IIF(BOF() .OR. EOF(), "Exit", "list")

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║              Программа добавления/коррекции пользователя               ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.01.97 18:23:45 ═╝
PROCEDURE Add_User
PARAMETER mode

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

PRIVATE ex, wh, rg, pw

pw = SPACE(FSIZE("PASSWORD"))
IF mode = "Add"
  wh = 1
  rg = 1
ELSE
  wh = MAX(1, ASCAN(cli_c, USER_CODE))
  rg = MAX(1, ASCAN(rght_c, RIGHTS))
ENDIF
ex   = 1
*------------------------------------------------------------------------
*┌──────────────────────────────────────────────────────────────────────┐
*│      Пользователь                      Уровень доступа               │
*│┌────────────────────────────────┐┌──────────────────────────────────┐│
*││ ░░░░▓░░░░█░░░░▓░░░░█░░░░▓░░░░█ ││ ░░░░▓░░░░█░░░░▓░░░░█░░░░▓░░░░█░░ ││
*│└────────────────────────────────┘└──────────────────────────────────┘│
*│                           Пароль ░░░░▓░░░                            │
*│                   < OK Ctrl-W > < Отказаться Esc >                   │
*└──────────────────────────────────────────────────────────────────────┘

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 74, IIF(mode = "Add", "Добавление", "Коррекция")+" пользователя системы"
@ 2,  8 SAY "Пользователь"
@ 2, 42 SAY "Уровень доступа"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  2 GET wh PICTURE "@^" FROM cli_n  SIZE 3, 34 WHEN mode = "Add"
@ 3, 36 GET rg PICTURE "@^" FROM rght_n SIZE 3, 36
@ 6, 29 SAY "Пароль" GET pw
@ 7, 21 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_B()

IF ex = 1
  IF EMPTY(pw)
    pw = PASSWORD
  ELSE
    pw = SYS(15,lwr,pw)
    DO SetPrice WITH "superamv", pw
  ENDIF
  IF mode = "Add"
    APPEND BLANK
  ENDIF
  REPLACE USER_CODE WITH cli_c(wh),   ;
          USER_NAME WITH cli_n(wh),   ;
          PASSWORD  WITH pw,          ;
          RIGHTS    WITH rght_c(rg),  ;
          USER_SYS  WITH sys_char

ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Проверка выхода из бланка.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.01.97 22:46:46 ═╝
PROCEDURE Tst_B
PRIVATE mss, rc, rc_sv, w_pw

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

rc = -1
IF mode = "Add" .AND. EMPTY(pw)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Пароль должет быть!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ELSE
  rc = RECNO()
ENDIF

IF .NOT. EMPTY(pw)
  w_pw = SYS(15,lwr,pw)
  DO SetPrice WITH "superamv", w_pw
  rc_sv = RECNO()
  LOCATE FOR PASSWORD = w_pw .AND. RECNO() # rc
  IF rc_sv > 0 .AND. rc_sv <= RECCOUNT()
    GO rc_sv
  ENDIF
  IF FOUND()
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Вам придется максимально напрячь воображение и придумать другой пароль!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.
