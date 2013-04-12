*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Cli_Kind     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                          Выбор типа клиента.                           ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 03/20/2001 ══╝
PROCEDURE Cli_Kind
PARAMETERS kindCode, kindName

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
USE (base_path+"CLI_KIND") ORDER TAG CODE ALIAS LST_1320 AGAIN
IF EOF() .OR. BOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Список типов пуст!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
IF .NOT. EMPTY(kindCode)
  IF .NOT. SEEK(kindCode)
    kindCode = SPACE(FSIZE("KIND_CODE"))
    kindName = SPACE(FSIZE("KIND_NAME"))
    GO TOP
  ENDIF
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Clear OF (menu_name) PROMPT "Очистить" KEY Ctrl-A, "Пробел"
ON SELECTION PAD Clear OF (menu_name) DO Sw_Mode WITH "Clear"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Отказаться"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = FSIZE("KIND_NAME")

DO D_Wins WITH ln, wd, "Типы клиентов", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter    KEYBOARD CHR(23)
    ON KEY LABEL SpaceBar DO Sw_Mode WITH "Clear"

*
*    BROWSE - меню
*
    BROWSE FIELDS KIND_NAME:H="",      ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    kindCode = KIND_CODE
    kindName = KIND_NAME
    EXIT

  CASE what_do = "Clear"     && Очистка

    kindCode = SPACE(FSIZE("KIND_CODE"))
    kindName = SPACE(FSIZE("KIND_NAME"))
    EXIT

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
SELECT (s_sav)

RETURN
