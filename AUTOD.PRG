*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла AutoD        Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                Список предприятий, для которых зарешена                ║
*║                      автоматическая конвертация.                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 02/15/2001 ══╝
PROCEDURE AutoD

PRIVATE tmpFlags

tmpFlags = Get_Sys("AutoDeps")

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
USE (base_path+"FIRMS") ORDER TAG ALL_NAMES
SET FILTER TO EMPTY(DATE_OFF)

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
wd = 52

DO D_Wins WITH ln, wd, "Запрет автомат. кновертации", 0, 0
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
    BROWSE FIELDS L_NAME:H="",     ;
                  M_000 = IIF(FIRM_CODE+"!" $ tmpFlags, "√", " "):1:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Mark")
    ENDIF

  CASE what_do = "Mark"    && Отметка

    wd = AT(FIRM_CODE+"!", tmpFlags)
    IF EMPTY(tmpFlags) .OR. wd = 0
      tmpFlags = ALLTRIM(tmpFlags)+FIRM_CODE+"!"
    ELSE
      tmpFlags = STUFF(tmpFlags, wd, 4, "")
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

PRIVATE mss
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"  Сохраним изменения? [ Да ] [ Нет ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 1
  DO Put_Sys WITH "AutoDeps", tmpFlags
ENDIF
USE

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
SELECT (s_sav)

RETURN
