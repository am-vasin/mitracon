*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Ch_OperG     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║             Выбор терма в структуре кассовых операции.                 ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 10/23/1998 ══╝
PROCEDURE Ch_OperG
PARAMETERS g_code, g_name

PRIVATE w_code, w_name
PRIVATE fl_level, s_sav, what_do, titles, w
fl_level = 0
DIMENSION titles(1)   && Вот этот массив - не что иное, как стек заголовков
titles = "Основные разделы"

s_sav = SELECT()
DO Use_Dummy

SELECT 0
USE (base_path+"OPERSTRU") ORDER TAG PREV_LINE ALIAS V8A27 AGAIN && Собственно коррекция

SELECT 0
USE (base_path+"OPERSTRU") ORDER TAG CODE ALIAS C8A27 AGAIN   && Источник кодов

what_do = "Level"
IF SEEK(g_code)               && Строим стек подразделов
  w = RECNO()
  GO w IN V8A27
  g_name = C8A27.NAME
  fl_level = C8A27.PREV_LINE
  PRIVATE tmp_arr, ok, nxt_w
  DIMENSION tmp_arr(1)
  nxt_w = PREV_LINE
  ok = nxt_w # 0
  DO WHILE nxt_w # 0
    IF .NOT. SEEK(nxt_w, "C8A27")
      ok = .F.
      EXIT
    ENDIF
    DIMENSION tmp_arr(ALEN(tmp_arr)+1)
    tmp_arr(ALEN(tmp_arr)) = ALLTRIM(C8A27.NAME)
    nxt_w = C8A27.PREV_LINE
  ENDDO
  IF ok
    DIMENSION titles(ALEN(tmp_arr))
    FOR ok = 2 TO ALEN(tmp_arr)
      titles(ok) = tmp_arr(ALEN(tmp_arr)-ok+2)
    ENDFOR
  ENDIF
ENDIF
SELECT V8A27
DO WHILE .T.
  DO CASE
  CASE what_do = "Level"
    what_do = Level()
  CASE what_do = "Term"
    g_code = V8A27.CODE
    g_name = V8A27.NAME
    EXIT
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

SELECT V8A27
USE
SELECT C8A27
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
wd = FSIZE("NAME")+2

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
    BROWSE KEY fl_level FIELDS NAME:H="",  ;
           F000 = IIF(IS_TERM, "√"," "):H="",  ;
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
      ret_v =IIF(IS_TERM, "Term", "Level")
      EXIT
    ELSE
      what_do = "List"
    ENDIF

  OTHERWISE

    IF fl_level = 0
      ret_v = "Exit"
    ELSE
      = SEEK(fl_level,"C8A27")
      GO RECNO("C8A27") IN V8A27
      fl_level = C8A27.PREV_LINE
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
