*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла S_Div        Разработчик Галина Дмитриенко                 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                             Суперразделы.                              ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 18.01.2000 ══╝
PROCEDURE S_Div

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
USE (base_path+"POPUL") ORDER TAG CODE ALIAS P0118 AGAIN

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Cont OF (menu_name) PROMPT "Разделы" KEY Ctrl-A, "Tab"
ON SELECTION PAD Cont OF (menu_name) DO Sw_Mode WITH "Cont"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "Суперразделы", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Cont"

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && Коррекция

    DO Modi_S
    what_do = "List"

  CASE what_do = "Cont"    && Разделы

    DO Cont WITH CODE, NAME
    what_do = "List"

  CASE what_do = "Add"     && Добавление

    DO Modi_S WITH .T.
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
*│   Процедура Modi_S       Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Коррекция/добавление суперраздела.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.01.2000 ──┘
PROCEDURE Modi_S
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
*│                                  │
*│     Название ....:....!....:     │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, nm
ex = 1
nm = IIF(new, SPACE(FSIZE("NAME")), NAME)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, IIF(new, "Добавление ", "Коррекция ")+"суперраздела"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 7 SAY "Название" GET nm
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * Отрабатываем бланк
  PRIVATE cd
  IF new
    GO BOTT
    IF BOF() .OR. EOF()
      cd = 1
    ELSE
      cd = CODE+1
    ENDIF
    APPEND BLANK
    REPLACE CODE WITH cd
  ENDIF
  REPLACE NAME WITH nm
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Blank    Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Проверка правильности заполнения бланка.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.01.2000 ──┘
PROCEDURE Tst_Blank

PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Имя не может быть пустым..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Cont         Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Содержимое суперраздела.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.01.2000 ──┘
PROCEDURE Cont
PARAMETERS s_cd, s_nm

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
USE (base_path+"POPUL") ORDER TAG CODE ALIAS P_0118 AGAIN
SELECT 0
USE (base_path+"DIV_DET") ORDER TAG DIV ALIAS L_0118 AGAIN
SELECT 0
USE (base_path+"DIVISION") ORDER TAG B_NAME ALIAS D_0118 AGAIN
SET RELATION TO CODE INTO L_0118
SELECT 0
DELETE FILE (tmpo_path+"DIV_TMP.DBF")
CREATE DBF (tmpo_path+"DIV_TMP") ;
  ( DIV      N( 3), ;
    DIV_NAME C(FSIZE("DIV_NAME", "L_0118")), ;
    SUP_DIV  N( 2) )
SELECT D_0118
SCAN
  SELECT DIV_TMP
  APPEND BLANK
  REPLACE DIV      WITH D_0118.CODE,   ;
          DIV_NAME WITH D_0118.B_NAME, ;
          SUP_DIV  WITH IIF(FOUND("L_0118"), L_0118.SUP_DIV, 99)
  SELECT D_0118
ENDSCAN
USE
SELECT DIV_TMP
SET RELATION TO SUP_DIV INTO P_0118
GO TOP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Insert OF (menu_name) PROMPT "Включить в суперраздел" KEY Ctrl-A, "Enter"
ON SELECTION PAD Insert OF (menu_name) DO Sw_Mode WITH "Insert"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = FSIZE("DIV_NAME")+FSIZE("NAME","P_0118")+3

DO D_Wins WITH ln, wd, ALLTRIM(s_nm), 0, 0
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
    BROWSE FIELDS DIV_NAME:H="",  ;
                  MARK0 = IIF(SUP_DIV = s_cd, "√", " "):1:H="", ;
                  P_0118.NAME:H="",    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Insert")
    ENDIF

  CASE what_do = "Insert"    && Вставить!

    REPLACE SUP_DIV WITH s_cd
    SKIP
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
PRIVATE mss
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"  Сохраним изменения? [ Да ] [ Нет ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 1
  SELECT L_0118
  SET ORDER TO
  GO TOP
  SELECT DIV_TMP
  GO TOP
  SCAN
    SELECT L_0118
    IF EOF() .OR. BOF()
      APPEND BLANK
    ENDIF
    REPLACE DIV      WITH DIV_TMP.DIV,  ;
            DIV_NAME WITH DIV_TMP.DIV_NAME, ;
            SUP_DIV  WITH DIV_TMP.SUP_DIV
    SKIP
    SELECT DIV_TMP
  ENDSCAN
  SELECT L_0118
  IF .NOT. BOF() .AND. .NOT. EOF()
    SKIP
    DELETE REST
  ENDIF
ENDIF
SELECT DIV_TMP
USE
SELECT P_0118
USE
SELECT L_0118
USE
DELETE FILE (tmpo_path+"DIV_TMP.DBF")
SELECT (s_sav)

RETURN
