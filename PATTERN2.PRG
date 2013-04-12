
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
* <Файл1 > (<Алиас>)
*  ├─ <Файл1 > (<Алиас>)
*  │  ├─ <Файл1 > (<Алиас>)
*  │  │  ├─ <Файл1 > (<Алиас>)
*  │  │  │  ├─ <Файл1 > (<Алиас>)
*  │  │  │  └─ <Файл1 > (<Алиас>)
*  │  │  └ <Файл1 > (<Алиас>)
*  │  ├─ <Файл1 > (<Алиас>)
*  │  └─ <Файл1 > (<Алиас>)
*  └─ <Файл1 > (<Алиас>)

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
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
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = IIF(SET("CENTURY") = "ON", !!!!!!!!!,  !!!!!!!!!!)

DO D_Wins WITH m.ln, m.wd, "<Browse title>", 0, 1
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS < Поля меню >,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
SELECT (m.s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
