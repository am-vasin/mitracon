*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла RusBrend     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Российские фирмы производители ЭК.                   ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 17.07.2000 ══╝
PROCEDURE RusBrend

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

SELECT 0
USE (base_path+"RUSBREND") ORDER TAG CODE ALIAS BREND_0717

SELECT 0
USE (base_path+"DEVELOP") ORDER TAG B_NAME AGAIN ALIAS PROD_0717
SET RELATION TO CODE INTO BREND_0717 

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mark OF (menu_name) PROMPT "Отметка" KEY Ctrl-A, "Enter"
ON SELECTION PAD Mark OF (menu_name) DO Sw_Mode WITH "Mark"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 71

DO D_Wins WITH ln, wd, "Фирмы производители ЭК", 0, 0
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
    BROWSE FIELDS MARK = IIF(FOUND("BREND_0717"), "√", " "):1:H="", ;
           B_NAME:8:H="", L_NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Mark")
    ENDIF

  CASE what_do = "Mark"    && Отметка

    IF FOUND("BREND_0717")
      REPLACE BREND_0717.CODE WITH 0
    ELSE
      SELECT BREND_0717
      GO TOP
      IF .NOT. EMPTY(CODE)
        APPEND BLANK
      ENDIF
      REPLACE BREND_0717.CODE WITH PROD_0717.CODE
      SELECT PROD_0717
    ENDIF
    SKIP
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
USE
SELECT BREND_0717
USE
SELECT (s_sav)

RETURN
