PROCEDURE Tst_Seek

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

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

*DEFINE PAD <Pad name> OF (menu_name) PROMPT "<Prompt>" KEY Ctrl-A, "<Hot key>"
*ON SELECTION PAD <Pad name> OF (menu_name) DO Sw_Mode WITH "<Mode name>"

*DEFINE PAD <Pad name> OF (menu_name) PROMPT "<Prompt>"
*ON PAD <Pad name> OF (menu_name) ACTIVATE POPUP <Popup name>

*  DEFINE POPUP <Popup name> MARGIN RELATIVE SHADOW COLOR SCHEME 4

*  DEFINE BAR  1 OF <Popup name> PROMPT <Prompt> KEY Ctrl-A, "<Hot key>"
*  ON SELECTION BAR 1 OF <Popup name> DO Sw_Mode WITH "<Mode name>"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DELETE FILE (tmpo_path+"TST_SEEK.DBF")
DELETE FILE (tmpo_path+"TST_SEEK.CDX")

CREATE DBF (tmpo_path+"TST_SEEK.DBF")  ;
  ( CODE    N( 7),  ;
    QNT     N( 6),  ;
    PRICE_R N(8,2)  )
INDEX ON CODE TAG CODE

SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME
SET RELATION TO CODE INTO TST_SEEK
*
*   Расчет размеров окна
*
PRIVATE ln, wd, rc_no
ln = 16   && Количество видимых строк BROWSE
wd = 55

DO D_Wins WITH ln, wd, "Список", 1, 3
what_do = "List"

rc_no = RECNO()
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - меню
*
    BROWSE FIELDS P_N = P_Name(),  ;
                  STOCK.PRICE_R:P="99999.99", TST_SEEK.QNT:P="999999"  ;
           NOAPPEND NOWAIT NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    DO Seek_Pos WITH 1, 3, 25, ln, "STOCK", "", .T.
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

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

PROCEDURE P_Name
RETURN IIF(RECNO("STOCK") = rc_no, " ", "  ")+STOCK.PREFIX+" "+STOCK.NAME+" "+STOCK.PRODUCER
