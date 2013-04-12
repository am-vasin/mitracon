*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла V_Ord1       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                        Просмотр заявки на склад                        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 11.11.2005 ══╝
PROCEDURE V_Ord1
PARAMETERS prmList, prmNum

*
* prmList - номер списка;
* prmNum  - номер заявки.
*


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

m.s_sav = SELECT()

SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS ST_5B11
SELECT 0
USE (m.base_path+"ORD_D") SHARED AGAIN ORDER TAG DOC_NUM ALIAS OD_5B11
SET RELATION TO CODE INTO ST_5B11
SELECT 0
DO Use_Dummy
SELECT OD_5B11

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = 65

DO D_Wins WITH m.ln, m.wd, ;
"Заявка № "+ALLTRIM(m.prmList)+"/"+ALLTRIM(STR(m.prmNum)), 0, 0

@ 1, 2 SAY ;
"Адрес─────────Наименование─────────────────────────────────Кол-во"
*AAAAAAAA PPPP nnnnnnnnnNnnnnnnnnnNnnnnnnnnnNnnnnn FFFFFFFF 999999
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
    BROWSE KEY m.prmList+STR(m.prmNum, 3) ;
               FIELDS ST_5B11.ADDRESS:H="",   ;
                      ST_5B11.PREFIX:H="",    ;
                      ST_5B11.NAME:35:H="",   ;
                      ST_5B11.PRODUCER:H="",  ;
                      QNT:H="":P="999999",    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
*      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
      m.what_do = "Exit"
    ENDIF

*  CASE m.what_do = "Choose"    && Просмотр списка

*    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*    m.what_do = "List"

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
USE IN ST_5B11
USE IN OD_5B11

SELECT (m.s_sav)

RETURN
