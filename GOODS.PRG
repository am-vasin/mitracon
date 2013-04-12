*╔══════════════════════════════════════════════════════════════════════╗
*║                                                                      ║
*║                Программа работы со списком товаров.                  ║
*║                                                                      ║
*╚══════════════════════════════════════════════════════════════════════╝
PROCEDURE Goods

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
*  Список отделов предприятия
*
PRIVATE mss, sel_sav
sel_sav = SELECT()
SELECT 0
USE GOODS ORDER TAG NAME
GO TOP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "add"

DEFINE PAD Edit OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Edit OF (menu_name) Do Sw_Mode WITH "edit"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

SELECT GOODS

*
*   Расчет размеров окна
*
PRIVATE ln, wd, pic
ln = 100   && Количество видимых строк BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "Список товаров", 0, 0

IF EOF() .OR. BOF()
  what_do = "add"
ELSE
  what_do = "list"
ENDIF

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "add"

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E") COLOR SCHEME 13
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "edit")
    ENDIF

  CASE what_do = "add"    && Добавить

    DO Corr_God WITH "A"
    IF EOF() .OR. BOF()
      what_do = "exit"
    ELSE
      what_do = "list"
    ENDIF

  CASE what_do = "edit"   && Корректировать

    DO Corr_God WITH "C"
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
SELECT GOODS
USE
SELECT (sel_sav)
RELEASE MENU (menu_name) EXTENDED

RETURN


*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Программа коррекции товара.                        ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Corr_God
PARAMETER mode

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

PRIVATE ex, b_n, l_n, w
                                    &&   Объявляем и заполняем поля бланка
ex   = 1                            &&

IF mode = "A"
  l_n  = SPACE(FSIZE("NAME"))
ELSE
  l_n  = NAME
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 52, "Заполните карточку"

*┌────────────────────────────────────────────────┐
*│                                                │
*│  Наименование товара ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒  │
*│                                                │
*│        < OK Ctrl-W > < Отказаться Esc >        │
*│                                                │
*└────────────────────────────────────────────────┘

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3,  4 SAY "Наименование товара" GET l_n
@ 5, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  IF mode = "A"
    SET ORDER TO TAG CODE
    GO BOTTOM
    IF EOF() .OR. BOF()
      w = 1
    ELSE
      w = CODE+1
    ENDIF
    APPEND BLANK
    REPLACE CODE  WITH w
    SET ORDER TO TAG NAME
  ENDIF
  REPLACE NAME WITH l_n
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║             Программа проверки корректности полей.                    ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Tst_Blank

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF EMPTY(l_n)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет, как-то он (товар) все-таки должен называться!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
