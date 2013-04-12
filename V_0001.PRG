*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл V_0001       Разработчик Андрей Васин           15.06.99          ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║             Внесение позиций поставщика в свою номенклатуру.           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE V_0001

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

DEFINE PAD Mark OF (menu_name) PROMPT "Отметки"
ON PAD Mark OF (menu_name) ACTIVATE POPUP Mark

  DEFINE POPUP Mark MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Mark PROMPT "Уст./Снять" KEY Ctrl-A, "Пробел"
  ON SELECTION BAR 1 OF Mark DO Sw_Mode WITH "Mark"

  DEFINE BAR  2 OF Mark PROMPT "Инвертировать все" KEY Ctrl-A, "*"
  ON SELECTION BAR 2 OF Mark DO Sw_Mode WITH "Not"

  DEFINE BAR  3 OF Mark PROMPT "Очистить все" KEY Ctrl-A, "-"
  ON SELECTION BAR 3 OF Mark DO Sw_Mode WITH "Clear"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
PRIVATE r_sav, o_sav
r_sav = RECNO()
o_sav = TAG()
SET ORDER TO
SELECT LINK
SET RELATION TO LNK INTO IN_STOCK

*
*   Расчет размеров окна
*
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = 71

*
*┌Отм─────Наименование────────────────────────Дата созд.──Нал.─┐
*│MMM PPPP NNNNNNNNNNNNNNNNNNNNNNNNN PPPPPPPP DDDDDDDDDD 999999│
*└─────────────────────────────────────────────────────────────┘
*┌─ Цена поставщика ───────────────────────────────────────────┐
*│ 99999.99руб   99999.9999$                                   │
*└─────────────────────────────────────────────────────────────┘

DO D_Wins WITH ln, wd, ALLTRIM(s_name), 0, 1

@ 1, 2 SAY "Отм─────Наименование──────────────────────────────────Дата созд.──Нал."

@ WROWS()-3,  3 SAY " Цена поставщика "
@ WROWS()-2, 11 SAY "руб           $"
what_do = "List"

DO WHILE .T.
 
  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL SpaceBar DO Mrk
    ON KEY LABEL "*"      DO Inv_Mrk
    ON KEY LABEL "-"      DO Clr_Mrk
*
*    BROWSE - меню
*
    BROWSE FIELDS IN_STOCK.MRK:H="", ;
                  IN_STOCK.PREFIX:H="", ;
                  IN_STOCK.NAME:H="", ;
                  IN_STOCK.PRODUCER:H="", ;
                  IN_STOCK.DATE_ON:H="", ;
                  F000 = IIF(IN_STOCK.SUP_QNT < 0, " Есть ", TRANSFORM(IN_STOCK.SUP_QNT, "@Z 999999")):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_F() ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "Mark"     && Установить/Снять отметку

    DO Mrk
    what_do = "List"

  CASE what_do = "Clear"    && Очистить все отметки

    DO Clr_Mrk
    what_do = "List"

  CASE what_do = "Not"      && Ивертировать все отметки

    DO Inv_Mrk
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
SET RELATION TO
SELECT IN_STOCK
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
SET ORDER TO (o_sav)

RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Вывод информационной строки.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 14.06.97 14:49:18 ═╝
PROCEDURE Show_F

ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-2,  3 SAY IN_STOCK.PRICE_R PICTURE "@Z 99999.99" COLOR SCHEME 1
@ WROWS()-2, 17 SAY IN_STOCK.PRICE_D PICTURE "@Z 99999.9999" COLOR SCHEME 1

RETURN .T.
