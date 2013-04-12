*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Mss_List     Разработчик Андрей Васин           04.09.98 13:32:12 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                Список посланий разработчику по теме...                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Mss_List
PARAMETER topic_c, topic_n, p_who

PRIVATE s_sav, p_w

p_w = ""
IF TYPE("p_who") = "N"
  p_w = IIF(p_who > 0, STR(p_who), "")
ENDIF

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
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Add OF (menu_name) PROMPT "Добавить сообщение" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD View OF (menu_name) PROMPT "Просмотр"
ON PAD View OF (menu_name) ACTIVATE POPUP View

  DEFINE POPUP View MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF View PROMPT "Сообщения" KEY Ctrl-A, "Enter"
  ON SELECTION BAR 1 OF View DO Sw_Mode WITH "Mess"

  DEFINE BAR  2 OF View PROMPT "Ответа" KEY Ctrl-A, "Ctrl+Enter"
  ON SELECTION BAR 2 OF View DO Sw_Mode WITH "Answer"

  IF EMPTY(p_w)
    DEFINE BAR  3 OF View PROMPT "Ввод ответа" KEY Ctrl-A, "Tab"
    ON SELECTION BAR 3 OF View DO Sw_Mode WITH "Reply"
  ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "Вернуться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
s_sav = SELECT()

DO Use_Dummy
SELECT 0
USE (base_path+"MSS_LIST") ORDER TAG TOPIC

*
*   Расчет размеров окна
*

PRIVATE ln, wd
ln = 12   && Количество видимых строк BROWSE
wd = FSIZE("MSS_TITLE")+IIF(SET("CENTURY") = "ON", 32, 28)

DO D_Wins WITH ln, wd, ALLTRIM(topic_n), 0, IIF(EMPTY(p_w), 1, 0)
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter      KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "Answer"
    ON KEY LABEL Ins        DO Sw_Mode WITH "Add"
    IF EMPTY(p_w)
      ON KEY LABEL Tab DO Sw_Mode WITH "Reply"
    ENDIF

*
*    BROWSE - меню
*
    BROWSE KEY p_w+topic_c ;
           FIELDS MSS_DATE:H="",   ;
                  MSS_TIME:H="",   ;
                  MSS_TITLE:H="",  ;
                  ANSW_DATE:H="",  ;
                  ANSW_TIME:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Mess")
    ENDIF

  CASE what_do = "Mess"    && Просмотр сообщения

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO V_Mess
    ENDIF
    what_do = "List"

  CASE what_do = "Add"     && Ввод сообщения

    DO A_Mess WITH topic_c, topic_n
    what_do = "List"

  CASE what_do = "Answer"  && Просмотр ответа

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO V_Answer
    ENDIF
    what_do = "List"

  CASE what_do = "Reply"   && Ввод ответа

    DO A_Answer
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
SELECT MSS_LIST
USE
RELEASE MENU (menu_name) EXTENDED
SELECT (s_sav)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Программа ввода сообщения.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 04.09.98 17:32:16 ═╝
PROCEDURE A_Mess
PARAMETERS topic_c, topic_n

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

*┌────────────────────────────────────────────────────────────────────────┐
*│          Заголовок сообщения ....:....!....:....!....:....!..          │
*│ ┌─ Текст сообщения ──────────────────────────────────────────────────┐ │
*│ │                                                                    │ │
*│ │                                                                    │ │
*│ │                                                                    │ │
*│ │                                                                    │ │
*│ │                                                                    │ │
*│ │                                                                    │ │
*│ │                                                                    │ │
*│ │                                                                    │ │
*│ │                                                                    │ │
*│ │                                                                    │ │
*│ │                                                                    │ │
*│ │                                                                    │ │
*│ │                                                                    │ │
*│ │                                                                    │ │
*│ └────────────────────────────────────────────────────────────────────┘ │
*│                    < OK Ctrl-W > < Отказаться Esc >                    │
*└────────────────────────────────────────────────────────────────────────┘

PRIVATE ex, nm_mess, t_mess
ex   = 1
t_mess = ""
nm_mess = SPACE(32)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 21, 76, ALLTRIM(topic_n)
DO Sun_Bord WITH 3, 3, 18, 72, " Текст сообщения "
@ 18, (WCOLS()-50)/2 SAY " Выход из окна редактирования - Tab или Shift-Tab "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2, 12 SAY "Заголовок сообщения" GET nm_mess
@  4,  4 EDIT t_mess SIZE 14, 68
@ 19, 22 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID T_Blank()

IF ex = 1
  DO SetPrice WITH "Медведь! Бурбон! Монстр!!!", t_mess
  APPEND BLANK
  REPLACE TOPIC     WITH topic_c,  ;
          WHO       WITH p_who,    ;
          WHERE     WITH arm,      ;
          MSS_DATE  WITH DATE(),   ;
          MSS_TIME  WITH TIME(),   ;
          MSS_TITLE WITH nm_mess,  ;
          MSS_TEXT  WITH t_mess
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Проверка корректности бланка.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 04.09.98 18:31:41 ═╝
PROCEDURE T_Blank
PRIVATE mss

IF ex = 1
  IF EMPTY(nm_mess)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Заголовок сообщения не может быть пустым!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  IF EMPTY(t_mess)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Сообщение не может быть пустым!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Действительно, сохраняем? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 2
    RETURN .F.
  ENDIF
ELSE
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Действительно, отказываемся сохранять? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 2
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.
