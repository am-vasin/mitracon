*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Ch_Chang     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Интерактивный вабор замены.                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 09/24/1999 ══╝
PROCEDURE Ch_Chang
PARAMETERS s_name, v_zero

PRIVATE s_sav, r

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_9924 AGAIN
SELECT 0
USE (tmpo_path+"CHANGES")
SET RELATION TO CODE INTO ST_9924
IF .NOT. v_zero
  SET FILTER TO ST_9924.QNT > 0 .AND. ST_9924.PRICE > 0
ENDIF
COUNT TO r
IF r = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одной замены!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT ST_9924
  USE
  SELECT (s_sav)
  RETURN 0
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
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(r,WROWS("")-9)   && Количество видимых строк BROWSE
wd = 76

DO D_Wins WITH ln, wd, "Аналоги"+IIF(.NOT. EMPTY(s_name), " для "+ALLTRIM(s_name), ""), 0, 0
@ 1, 2 SAY ;
"───Наименование───────────────────────────────────Кол-во───Цена У Е─Цена руб"
*PPPP nnnnnnnnnNnnnnnnnnnNnnnnnnnnnNnnnnn PPPPPPPP
what_do = "List"
GO TOP
PUSH KEY CLEAR
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS ST_9924.PREFIX:4:H="",  ;
                  ST_9924.NAME:35:H="",   ;
                  ST_9924.PRODUCER:8:H="",;
                  ST_9924.QNT:P="@Z 999999":H="",   ;
                  ST_9924.PRICE:P="@Z 99999.9999":H="",   ;
                  ST_9924.PRICE_R:P="@Z 99999.99":H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    r = ST_9924.CODE
    EXIT

  OTHERWISE

    r = 0
    EXIT

  ENDCASE

ENDDO

POP KEY
RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
SELECT CHANGES
USE
SELECT ST_9924
USE
SELECT (s_sav)

RETURN r
