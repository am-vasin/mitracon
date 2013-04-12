*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл News         Разработчик Андрей Васин           16.01.98 10:24:47 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Программа информации от разработчика.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE News
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
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

IF .NOT. File_O(base_path+"NEWS.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет никакой информации!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

USE (base_path+"NEWS") ORDER TAG HRON
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет никакой информации!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF
DO Use_Dummy
SELECT NEWS
GO BOTTOM

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD View OF (menu_name) PROMPT "Просмотр" KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = FSIZE("MESS_TIT")+IIF(SET("CENTURY")="ON", 11, 9)

DO D_Wins WITH ln, wd, "Информация от разработчика", 0, 0
what_do = "List"
@ 1, 2 SAY ;
    IIF(SET("CENTURY")="ON", "── Дата ───", "─ Дата ──")+"Заголовок сообщения"

DO WHILE INKEY("M") # 0
ENDDO
SKIP -FLOOR((ln-1)/2)
KEYBOARD "{PgDn}"
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS MESS_DATE:H="", MESS_TIT:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "View")
    ENDIF

  CASE what_do = "View"    && Просмотр сообщения

    DO V_Memo WITH "MESS", 74, ALLTRIM(MESS_TIT)
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN
