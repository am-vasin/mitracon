*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл View_DBF     Разработчик Андрей Васин           01.10.98 15:24:58 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                          Просмотр DBF-файла.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE View_DBF
PARAMETERS f_name, tit

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

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
PRIVATE s_sav
s_sav = SELECT()
SELECT 0

USE (ALLTRIM(f_name)) ALIAS View

*
*   Расчет размеров окна
*
PRIVATE ln, wd, i, nm
ln = WROWS()-7   && Количество видимых строк BROWSE
i = 1
wd = 0
nm = FIELD(i)
DO WHILE .NOT. EMPTY(nm)
  DO CASE
  CASE TYPE(nm) = "M"
    wd = wd+MAX(4, LEN(nm))
  CASE TYPE(nm) = "D"
    wd = wd+MAX(IIF(SET("CENTURY") = "ON", 10, 8), LEN(nm))
  OTHERWISE
    wd = wd+MAX(FSIZE(nm), LEN(nm))
  ENDCASE
  i = i+1
  nm = FIELD(i)
ENDDO
wd = MIN(wd+i-2, WCOLS()-4)

DO D_Wins WITH ln, wd, ALLTRIM(tit), 0, 0
ZOOM WINDOW (win_name+"_I") NORM AT WLROW()+1, WLCOL() ;
                            SIZE WROWS()-1, WCOLS()-1
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE NOMODIFY NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
USE
SELECT (s_sav)
RELEASE MENU (menu_name) EXTENDED

RETURN
