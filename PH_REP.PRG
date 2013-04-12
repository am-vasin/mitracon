*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Ph_Rep       Разработчик Андрей Васин           30.01.97 00:32:26 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Телефонный справочник предприятия.                   ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Ph_Rep

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
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

*DEFINE PAD Seek1 OF (menu_name) PROMPT "Поиск по нач. симв." KEY Ctrl-A, "F7"
*ON SELECTION PAD Seek1 OF (menu_name) Do Sw_Mode WITH "Seek1"

*DEFINE PAD Seek2 OF (menu_name) PROMPT "Конт. поиск" KEY Ctrl-A, "Ctrl-F7"
*ON SELECTION PAD Seek2 OF (menu_name) Do Sw_Mode WITH "Seek2"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

USE DEPS ORDER TAG CODE

DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") ORDER TAG NAME
SET RELATION TO DEP_NO INTO DEPS

*
*   Расчет размеров окна
*
PRIVATE ln, wd, pic
ln = 100   && Количество видимых строк BROWSE
*
*┌── Сотрудник ───────────────────────────── Гор. тел. ────── Вн.т.┐
*│░░░░▒░░░░▓░░░░▒░░░░▓░░░░ ░░░░▒░░░░▓░░░░▒░ ░░░░▒░░░░▓░░░░▒░ ░░░░▒░│
*└─────────────────────────────────────────────────────────────────┘
*┌───── Отдел ──────────── Площадка ───────── Комната ─────────────┐
*│     ░░░░▒░░░░▓░░░░▒░   ░░░░▒░░░░▓░░░░▒░   ░░░░▒░░░░▓░░░░▒░      │
*└─────────────────────────────────────────────────────────────────┘

pic = FSIZE("NAME")+FSIZE("FAMILY")+1
wd = pic+FSIZE("W_PHONE1")+FSIZE("W_PHONE2")+2
pic = "@S"+ALLTRIM(STR(pic))

DO D_Wins WITH ln, wd, "Список сотрудников", 0, 1
@ 1, 4 SAY " Сотрудник "
@ 1, 44 SAY " Гор. тел. "
@ 1, 61 SAY " Вн.т."
@ WROWS()-3,  7 SAY " Отдел "
@ WROWS()-3, 26 SAY " Площадка "
@ WROWS()-3, 45 SAY " Комната "

what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    DO Key_Seek
*    ON KEY LABEL F7      Do Sw_Mode WITH "Seek1"
*    ON KEY LABEL Ctrl+F7 Do Sw_Mode WITH "Seek2"

*
*    BROWSE - меню
*
    BROWSE FIELDS F001 = ALLTRIM(FAMILY)+" "+ALLTRIM(NAME):H="":P=pic,  ;
                  W_PHONE1:H="",              ;
                  W_PHONE2:H="",              ;
                  DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_Lin()                    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = "exit"
    ENDIF

  CASE what_do = "Seek1"    && Поиск по нач. символам

    what_do = "list"

  CASE what_do = "Seek2"    && Контекстный поиск

    what_do = "list"

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
*║                      Вывод информационной строки                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 30.01.97 01:02:08 ═╝
PROCEDURE Show_Lin

ACTIVATE WINDOW (win_name+"_M") BOTTOM

@ WROWS()-2,  7 SAY DEPS.B_NAME COLOR SCHEME 1
@ WROWS()-2, 26 SAY PLACE COLOR SCHEME 1
@ WROWS()-2, 45 SAY ROOM COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
