*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                 Программа просмотра журнала ошибок.                   ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE View_Err

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

PRIVATE ex, nm_w     &&
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
nm_w = PADR(base_path,40)   &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 67, "Какой журнал смотрим..."
*
*┌───────────────────────────────────────────────────────────────┐
*│                                                               │
*│ Путь к файлу журнала ....:....1....:....2....:....3....:....4 │
*│                                                               │
*│               < OK Ctrl-W > < Отказаться Esc >                │
*│                                                               │
*└───────────────────────────────────────────────────────────────┘


*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  3 SAY "Путь к файлу журнала" GET nm_w PICTURE "@K"
@ 5, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "


READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF ex = 2 .OR. EMPTY(nm_w)
  RETURN
ENDIF
*--------------------------------------------------------------------------

nm_w = ALLTRIM(nm_w)

USE (nm_w+"USERS") ORDER TAG USER_CODE

DO Use_Dummy

SELECT 0
USE (nm_w+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (nm_w+"ERR_LOG") ORDER TAG ERR_DAT
SET RELATION TO ER_USE INTO USERS,  ;
                ER_ARM INTO ARM_LST

GO BOTT
DO Err_Brws

CLOSE DATABASES
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                                                                       ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Err_Brws

*
*  Переменные состояния для навигвции
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
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Hole_N OF (menu_name) PROMPT "Полное сообщение" KEY Ctrl-A, "F1"
ON SELECTION PAD Hole_N OF (menu_name) Do Sw_Mode WITH "hole_n"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(WROWS("")-10, RECCOUNT())   && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY")="ON", 73, 71)

DO D_Wins WITH ln, wd, "Журнал ошибок", 1, 1

@ 1, 2 SAY IIF(SET("CENTURY")="ON","   Дата    ","  Дата   ")+  ;
           " Время   Номер    Сообщение                   Программа Строка"
@ WROWS()-3, 3 SAY " Раб. место "
@ WROWS()-3, 4+FSIZE("ARM_N","ARM_LST") SAY " Пользователь "

what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F1 WAIT WINDOW ALLTRIM(ER_MES)

*
*    BROWSE - меню
*
    BROWSE FIELDS ER_DAT:H="", ;
                  ER_TIM:H="", ;
                  ER_NUM:H="", ;
                  m00 = IIF(LEN(ALLTRIM(ER_MES)) < 31, LEFT(ER_MES,30), LEFT(ER_MES,29)+""):30,  ;
                  m01 = LEFT(ER_PRO,10):10,  ;
                  ER_LIN:H="", ;
                  DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WHEN S_Foot()     ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "edit")
    ENDIF

  CASE what_do = "hole_n"

    WAIT WINDOW ALLTRIM(ER_MES)
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE MENU (menu_name) EXTENDED
RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")

RETURN

***********************************************************************
PROCEDURE S_Foot
PRIVATE u_n

IF ER_USE = 0
  u_n = PADR("САМ!", FSIZE("USER_NAME", "USERS"))
ELSE
  u_n = USERS.USER_NAME
ENDIF
ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-2, 3 SAY ARM_LST.ARM_N COLOR (SCHEME(1,2))
@ WROWS()-2, 4+FSIZE("ARM_N","ARM_LST") SAY u_n  COLOR (SCHEME(1,2))

ACTIVATE WINDOW (ALIAS())

RETURN .T.
