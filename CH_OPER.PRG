*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Ch_Oper      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                        Выбор кассовой операции.                        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 10/23/1998 ══╝
PROCEDURE Ch_Oper
PARAMETERS o_code, o_name

PRIVATE w_code, w_name
PRIVATE fl_level, s_sav, what_do, titles
fl_level = 0
DIMENSION titles(1)   && Вот этот массив - не что иное, как стек заголовков
titles = "Основные разделы"

s_sav = SELECT()
DO Use_Dummy

SELECT 0
USE (base_path+"OPERSTRU") ORDER TAG CODE ALIAS C8A22 AGAIN   && Источник кодов

SELECT 0
USE (base_path+"OPERSTRU") ORDER TAG PREV_LINE ALIAS V8A22 AGAIN && Собственно коррекция

SELECT 0
USE (base_path+"MM_TYPES") ORDER TAG CODE ALIAS O8A22 AGAIN  && Операции

what_do = "Level"
IF SEEK(o_code)               && Строим стек подразделов
  PRIVATE tmp_arr, ok, nxt_w
  DIMENSION tmp_arr(1)
  nxt_w = GROUP
  ok = nxt_w # 0
  DO WHILE nxt_w # 0
    IF .NOT. SEEK(nxt_w, "C8A22")
      ok = .F.
      EXIT
    ENDIF
    DIMENSION tmp_arr(ALEN(tmp_arr)+1)
    tmp_arr(ALEN(tmp_arr)) = ALLTRIM(C8A22.NAME)
    nxt_w = C8A22.PREV_LINE
  ENDDO
  IF ok
    o_name = O8A22.NAME
    DIMENSION titles(ALEN(tmp_arr))
    FOR ok = 2 TO ALEN(tmp_arr)
      titles(ok) = tmp_arr(ALEN(tmp_arr)-ok+2)
    ENDFOR
    what_do = "TermList"
    fl_level = O8A22.GROUP
  ENDIF
ENDIF
SET ORDER TO TAG GROUP
SELECT V8A22

DO WHILE .T.
  DO CASE
  CASE what_do = "Level"
    what_do = Level()
  CASE what_do = "TermList"
    what_do = TermList()
    IF what_do = "Exit"
      o_code = O8A22.CODE
      o_name = O8A22.NAME
    ENDIF
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

SELECT V8A22
USE
SELECT C8A22
USE
SELECT O8A22
USE
SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Level        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│               Выбор из списка разделов текущего уровня.                │
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
      ret_v = RECNO("C8A22")
      GO ret_v
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
*│   Процедура TermList     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Список операций в разделе.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.10.1998 ──┘
PROCEDURE TermList
PRIVATE ret_v
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

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Внести в раздел" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 27

DO D_Wins WITH ln, wd, ALLTRIM(V8A22.NAME), 0, 0
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
    BROWSE KEY STR(fl_level) ;
           FIELDS NAME:H="", ;
                  F001 = IIF(M_RUB, "Руб", "У Е"):3:H="", ;
                  F002 = IIF(M_DIR = 0, "Приход", "Расход"):H="":6, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")

  CASE what_do = "Choose"       && Выбор!

    ret_v = "Exit"
    EXIT

  OTHERWISE

    ret_v = "Level"
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
SELECT V8A22
fl_level = PREV_LINE
DIMENSION titles(ALEN(titles)-1)

RETURN ret_v
