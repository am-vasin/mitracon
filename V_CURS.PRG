*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                  Программа просмотра курса USD.                       ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDUR V_Curs
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
USE (base_path+"CURS_SET") ORDER TAG HRON DESCENDING ALIAS CS_9528 AGAIN

SELECT 0
USE (base_path+"COURSE") ALIAS C_9528 AGAIN
SET RELATION TO B_NAME INTO CS_9528
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
ln = MIN(RECCOUNT(),WROWS("")-11)   && Количество видимых строк BROWSE
wd = 32

DO D_Wins WITH ln, wd, "Текущее значение курса USD", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Del   DO Del_Firm
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"

*
*    BROWSE - меню
*
    BROWSE FIELDS L_NAME:24:H="", ;
                  CS_9528.CURS:H="":P="@Z 999.999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
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
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
USE
SELECT CS_9528
USE
SELECT (s_sav)

RETURN
