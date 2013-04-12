*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла For_Cond     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Коррекция списка стандартных фраз.                   ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 26.03.2000 ══╝
PROCEDURE For_Cond
PARAMETERS cd, nm
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
IF EMPTY(cd)
  USE (base_path+"FOR_COND") ORDER TAG NAME ALIAS CND_0326 AGAIN
ELSE
  USE (base_path+"FOR_COND") ORDER TAG CODE ALIAS CND_0326 AGAIN
  IF SEEK(cd)
    SET ORDER TO TAG NAME
  ELSE
    SET ORDER TO TAG NAME
    GO TOP
  ENDIF
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Modify OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Tab"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "Выберите текст", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Modify"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Коррекция

    IF .NOT. BOF() .AND. .NOT. EOF()
      cd = CODE
      IF TYPE("nm") = "C"
        nm = NAME
      ENDIF
      EXIT
    ELSE
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Так не из чего выбирать!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      what_do = "List"
    ENDIF

  CASE what_do = "Modify"    && Коррекция

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Modi_Item
    ENDIF
    what_do = "List"

  CASE what_do = "Add"       && Добавление

    DO Modi_Item WITH .T.
    what_do = "List"

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

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_Item    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Коррекция/добавление записи.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.03.2000 ──┘
PROCEDURE Modi_Item
PARAMETERS new

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

*┌─────────────────────────────────────┐
*│                                     │
*│       Текст ....:....!....:.        │
*│                                     │
*│  < OK Ctrl-W > < Отказаться Esc >   │
*└─────────────────────────────────────┘

PRIVATE ex, nm_w
ex = 1
nm_w = IIF(new, SPACE(16), NAME)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 41, IIF(new, "Добавление", "Коррекция")+" текста"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 9 SAY "Текст" GET nm_w
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  IF new
    SET ORDER TO TAG CODE
    GO BOTT
    IF EOF() .OR. BOF()
      ex = "    1"
    ELSE
      ex = VAL(ALLTRIM(CODE))
      ex = STR(ex+1,5)
    ENDIF
    APPEND BLANK
    REPLACE CODE WITH ex
    SET ORDER TO TAG NAME
  ENDIF
  REPLACE NAME WITH nm_w
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Blank    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Правильность заполнения бланка.                     │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.03.2000 ──┘
PROCEDURE Tst_Blank

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Текст не может быть пустым!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
