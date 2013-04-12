*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла OperStru     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║           Формирование структуры разделов кассовых операций.           ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 10/22/1998 ══╝
PROCEDURE OperStru

PRIVATE fl_level, s_sav, what_do, titles
fl_level = 0
DIMENSION titles(1)   && Вот этот массив - не что иное, как стек заголовков
titles = "Основные разделы"

s_sav = SELECT()
DO Use_Dummy
SELECT 0
USE (base_path+"MM_TYPES") ORDER TAG GROUP ALIAS O8A22 AGAIN  && Операции
SELECT 0

USE (base_path+"OPERSTRU") ORDER TAG CODE ALIAS C8A22 AGAIN   && Источник кодов
SELECT 0
USE (base_path+"OPERSTRU") ORDER TAG PREV_LINE ALIAS V8A22 AGAIN && Собственно коррекция

what_do = "Level"
DO WHILE .T.
  DO CASE
  CASE what_do = "Level"
    what_do = Level()
  CASE what_do = "TermList"
    DO TermList
    what_do = "Level"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

USE
SELECT C8A22
USE
SELECT O8A22
USE

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Level        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│               Коррекция списка пунктов текущего уровня.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10/22/1998 ──┘
PROCEDURE Level

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
PRIVATE ret_v
*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "Корр." KEY Ctrl-A, "Tab"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "Доб." KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "Удал." KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Next OF (menu_name) PROMPT "След. уровень" KEY Ctrl-A, "Enter"
ON SELECTION PAD Next OF (menu_name) DO Sw_Mode WITH "Next"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd, mss, rc_sav, w
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, titles(ALEN(titles)), 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Corr"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Del"

*
*    BROWSE - меню
*
    BROWSE KEY fl_level FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Next")
    ENDIF

  CASE what_do = "Corr"    && Корректировать

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Modi_Line
    ENDIF
    what_do = "List"

  CASE what_do = "Add"     && Добавить

    DO Modi_Line WITH .T.
    what_do = "List"

  CASE what_do = "Del"     && Удалить

    IF EOF() .OR. BOF()
      what_do = "List"
      LOOP
    ENDIF

    IF IS_TERM
      IF SEEK(STR(CODE), "O8A22")
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Этот раздел содержит операции!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        LOOP
      ENDIF
    ELSE
      w = CODE
      rc_sav = RECNO()
      IF SEEK(w)
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Этот раздел содержит подразделы!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        GO rc_sav
        LOOP
      ENDIF
      GO rc_sav
    ENDIF
    REPLACE CODE WITH 0
    SEEK fl_level
    what_do = "List"

  CASE what_do = "Next"    && На следующий уровень

    IF .NOT. BOF() .AND. .NOT. EOF()
      fl_level = CODE
      DIMENSION titles(ALEN(titles)+1)
      titles(ALEN(titles)) = ALLTRIM(NAME)
      ret_v =IIF(IS_TERM, "TermList", "Level")
      EXIT
    ELSE
      what_do = "List"
    ENDIF

  OTHERWISE

    IF fl_level = 0
      ret_v = "Exit"
    ELSE
      = SEEK(fl_level,"C8A22")
      fl_level = C8A22.PREV_LINE
      DIMENSION titles(ALEN(titles)-1)
      ret_v = "Level"
    ENDIF
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

RETURN ret_v

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_Line    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Коррекция/добавление раздела.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10/22/1998 ──┘
PROCEDURE Modi_Line
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

*┌──────────────────────────────────┐
*│ ┌─ Заголовок раздела ──────────┐ │
*│ │   ....:....!....:....!....   │ │
*│ └──────────────────────────────┘ │
*│[ ] Больше не содержит подразделов│
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, nm, sw
IF new
  nm = SPACE(24)
  sw = .F.
ELSE
  nm = NAME
  sw = IS_TERM
ENDIF
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 38, IIF(new, "Создание", "Коррекция")+" раздела"
DO Sun_Bord WITH  2,  3,  4, 34, " Заголовок раздела "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 7 GET nm
@ 5, 2 GET sw PICTURE "@*C Больше не содержит подразделов"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_B()

IF ex = 1
  *
  * Отрабатываем бланк
  IF new
    GO TOP IN C8A22
    IF C8A22.CODE = 0 .AND. .NOT. BOF("C8A22") .AND. .NOT. EOF("C8A22")
      GO RECNO("C8A22")
    ELSE
      APPEND BLANK
    ENDIF
    GO BOTTOM IN C8A22
    REPLACE CODE WITH MAX(C8A22.CODE+1, 1)
  ENDIF
  REPLACE NAME      WITH nm,  ;
          PREV_LINE WITH fl_level, ;
          IS_TERM   WITH sw

ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_B        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Проверка правильности заполнения бланка.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10/22/1998 ──┘
PROCEDURE Tst_B

PRIVATE r_sav, mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Название раздела не может быть пустым!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF .NOT. new .AND. IS_TERM # sw
  IF .NOT. sw   && Стал не терм!
    IF SEEK(STR(CODE), "O8A22")
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Раздел содержит операции и его статус не может быть изменен!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      RETURN .F.
    ENDIF
  ELSE
    rc_sav = RECNO()
    IF SEEK(CODE)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Раздел содержит подразделы и его статус не может быть изменен!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      GO rc_sav
      RETURN .F.
    ENDIF
    GO rc_sav
  ENDIF
ENDIF

RETURN .T.


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура TermList     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Список операций в разделе.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.10.1998 ──┘
PROCEDURE TermList
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

SELECT O8A22
SET RELATION TO GROUP INTO C8A22
SET ORDER TO NAME

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Inc OF (menu_name) PROMPT "Внести в раздел" KEY Ctrl-A, "Enter"
ON SELECTION PAD Inc OF (menu_name) DO Sw_Mode WITH "Inc"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 54

DO D_Wins WITH ln, wd, ALLTRIM(V8A22.NAME), 0, 0
@ 1, 4 SAY " Операция ────────────────── Раздел "
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter DO Inc_Oper

*
*    BROWSE - меню
*
    BROWSE FIELDS F000 = IIF(GROUP = V8A22.CODE, "√", " "):1:H="",   ;
           NAME:H="", ;
           F001 = IIF(M_RUB, "Руб", "У Е"):H="":3,  ;
           F002 = IIF(M_DIR=0, "Приход" ,"Расход"):6:H="", ;
           F003 = IIF(GROUP = 0 .OR. .NOT. FOUND("C8A22"), "", C8A22.NAME):24:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"

  CASE what_do = "Inc"       && Вставить!

    DO Inc_Oper
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
SET RELATION TO
SET ORDER TO GROUP
SELECT V8A22
fl_level = PREV_LINE
DIMENSION titles(ALEN(titles)-1)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Inc_Oper     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Вставка операции в раздел.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.10.1998 ──┘
PROCEDURE Inc_Oper

REPLACE GROUP WITH V8A22.CODE
RETURN
