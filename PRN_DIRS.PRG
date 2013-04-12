*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Prn_Dirs     Разработчик Андрей Васин           10.03.98 12:36:46 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Перечень направлений печати.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Prn_Dirs
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
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DO Use_Dummy

SELECT 0
IF .NOT. File_O(base_path+"PRN_DIRS.DBF")
  CREATE DBF (base_path+"PRN_DIRS.DBF") (NAME C(32))
ELSE
  USE (base_path+"PRN_DIRS.DBF")
ENDIF

IF BOF() .OR. EOF()
  DO Corr_Dir WITH .T.
ENDIF

IF BOF() .OR. EOF()
  CLOSE DATABASES
  RETURN
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = 10   && Количество видимых строк BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "Направления печати", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && Коррекция

    DO Corr_Dir
    what_do = "List"

  CASE what_do = "Add"    && Добавление

    DO Corr_Dir WITH .T.
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

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                   Коррекция/добавление направления.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.03.98 13:00:42 ═╝
PROCEDURE Corr_Dir
PARAMETERS is_new

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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌──────────────────────────────────────┐
*│ ┌─ Название ───────────────────────┐ │
*│ │ ....:....!....:....!....:....!.. │ │
*│ └──────────────────────────────────┘ │
*│   < OK Ctrl-W > < Отказаться Esc >   │
*└──────────────────────────────────────┘

PRIVATE ex, nm_w     &&
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
IF is_new
  nm_w = SPACE(FSIZE("NAME"))
ELSE
  nm_w = NAME
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 42, IIF(is_new, "Добавление", "Коррекция")
DO Sun_Bord WITH 2, 3, 4, 38, " Название "
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 5 GET nm_w
@ 5, 5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID T_Name()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          А не пустое ли имя?!                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.03.98 13:08:10 ═╝
PROCEDURE T_Name

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Название не может быть пустым!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF is_new
  APPEND BLANK
ENDIF

REPLACE NAME WITH nm_w

RETURN .T.
