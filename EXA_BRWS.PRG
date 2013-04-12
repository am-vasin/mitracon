*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                                                                       ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Cus_Corr

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

DEFINE PAD <Pad name> OF (menu_name) PROMPT "<Prompt>" KEY Ctrl-A, "<Hot key>"
ON SELECTION PAD <Pad name> OF (menu_name) DO Sw_Mode WITH "<Mode name>"

DEFINE PAD <Pad name> OF (menu_name) PROMPT "<Prompt>"
ON PAD <Pad name> OF (menu_name) ACTIVATE POPUP <Popup name>

  DEFINE POPUP <Popup name> MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF <Popup name> PROMPT <Prompt> KEY Ctrl-A, "<Hot key>"
  ON SELECTION BAR 1 OF <Popup name> DO Sw_Mode WITH "<Mode name>"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

-
-
-
-
-
-
-
-
-

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = FSIZE("<Field>", "<Alias>")+ ...

DO D_Wins WITH ln, wd, "<Browse title>", 0, 0
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
    BROWSE FIELDS < Поля меню >,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "exit", "edit")
    ENDIF

  CASE what_do = "edit"    && Просмотр списка

    DO Corr_Name
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
