*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Dossier      Разработчик Андрей Васин           25.05.97 20:14:01 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║             Программа просмотра и коррекции досье клиента.             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Dossier
PARAMETERS c_code, flags
PRIVATE let_add, mss, sel_sav, c_nam, emp_l

let_add = .F.
IF TYPE("flags") = "C"
  let_add = "C" $ flags
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
win_name   = "DOSS"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
sel_sav = SELECT()
SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC999 AGAIN
IF .NOT. SEEK(c_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Некорректный код клиента."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_sav)
  RETURN
ENDIF
c_nam = ALLTRIM(CUS_NAME)
USE (base_path+"DOSSIER") ORDER TAG CUS_HIST DESCENDING ALIAS DOSS999 AGAIN
emp_l = .F.
IF .NOT. SEEK(STR(c_code,8))
  IF .NOT. let_add
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"У клиента пустое досье."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (sel_sav)
    RETURN
  ENDIF
  emp_l = .T.
ENDIF
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PERS999 AGAIN
SELECT DOSS999
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Просмотр" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

IF let_add
  DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
  ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "Add"

  DEFINE PAD Importance OF (menu_name) PROMPT "Важность" KEY Ctrl-A, "Tab"
  ON SELECTION PAD Importance OF (menu_name) Do Sw_Mode WITH "Importance"

ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, tit
ln = 15   && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY") = "ON", 52, 50)

DO D_Wins WITH ln, wd, c_nam, 0, 1
tit = PADC("Дата",IIF(SET("CENTURY") = "ON", 10, 8),"─")+"─"+ ;
      PADC("Содержание",32,"─")+"─Важность"+ ;
what_do = IIF(emp_l, "Add", "List")

IF WEXIST("H_Wind")
  HIDE WINDOW H_Wind
ENDIF
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    IF let_add
      ON KEY LABEL Ins Do Sw_Mode WITH "Add"
      ON KEY LABEL Tab Do Sw_Mode WITH "Importance"
    ENDIF

*
*    BROWSE - меню
*
    BROWSE KEY STR(c_code,8)            ;
           FIELDS DOSS_DATE:H="",       ;
                  DOSS_TIT:32:H="",     ;
                  IMP_MARK = IIF(EMPTY(IMPORTANCE), "        ", "Высокая "):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Sh_User()              ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Add"    && Добавить запись в досье

    DO Cor_Doss WITH c_code, c_nam, "A"
    what_do = "List"
    IF emp_l
      IF .NOT. SEEK(STR(c_code,8))
        what_do = "Exit"
      ENDIF
    ENDIF
    emp_l = .F.
    
  CASE what_do = "Choose"    && Просмотр досье

    DO Cor_Doss WITH c_code, c_nam
    what_do = "List"
    
  CASE what_do = "Importance" && Важность

    REPLACE DOSS999.IMPORTANCE WITH IIF(DOSS999.IMPORTANCE=" ", "!", " ")
    what_do = "List"
    
  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
USE
SELECT PERS999
USE
SELECT (sel_sav)

RELEASE MENU (menu_name) EXTENDED
sel_sav = WOUTPUT()
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
IF WEXIST("H_Wind")
  SHOW WINDOW H_Wind
ENDIF
IF .NOT. EMPTY(sel_sav)
  ACTIVATE WINDOW (sel_sav)
ENDIF

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                  Программа добавления пункта в досье.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 25.05.97 20:55:00 ═╝
PROCEDURE Cor_Doss
PARAMETERS c_code, c_nam, flags
PRIVATE to_add, to_corr

mode = "V"
to_add  = .F.
to_corr = .F.
IF TYPE("flags") = "C"
  DO CASE
  CASE "A" $ flags
    to_add  = .T.
    to_corr = .T.
  CASE "C" $ flags
    to_corr = .T.
  ENDCASE
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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌────────────────────────────────────────────────────────────────┐
*│      Заголовок сообщения ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒      │
*│                      [ ] Высокая важность                      │
*│┌─ Текст сообщения (Tab - след., Shift-Tab - пред. поле ) ─────┐│
*││ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█ ││
*││ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█ ││
*││ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█ ││
*││ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█ ││
*││ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█ ││
*││ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█ ││
*││ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█ ││
*││ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█ ││
*│└──────────────────────────────────────────────────────────────┘│
*│                < OK Ctrl-W > < Отказаться Esc >                │
*│                             < OK >                             │
*└────────────────────────────────────────────────────────────────┘

PRIVATE d_tit, d_mess, d_imp, ex     &&
ex   = 1             &&
IF to_add
  d_tit  = SPACE(32)
  d_mess = ""
  d_imp  = .F.
ELSE
  d_tit  = DOSS_TIT
  d_mess = DOSS_MESS
  d_imp  = .NOT. EMPTY(IMPORTANCE)
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 16, 68, c_nam

DO Sun_Bord WITH 4, 2, 13, 65, " Текст сообщения (Tab - след., Shift-Tab - пред. поле ) "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2,  8 SAY "Заголовок сообщения" GET d_tit WHEN to_corr
@ 3, 24 GET d_imp PICTURE "@*C Высокая важность" WHEN to_corr

IF to_corr
  @ 5, 4 EDIT d_mess SIZE 8, 60
  @ 14, 18 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
ELSE
  @ 5, 4 EDIT d_mess SIZE 8, 60 NOMODIFY
  @ 14, 31 GET ex PICTURE "@*HT \ OK "
ENDIF

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Выход из формы.                             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 25.05.97 21:31:28 ═╝
PROCEDURE Tst_Blank
PRIVATE mss

IF READKEY() % 256 = 12 .OR. ex = 2 .OR. .NOT. to_corr
  RETURN .T.
ENDIF

IF EMPTY(d_tit)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Сообщение надо как-то назвать!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(d_mess)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Так о чем, собственно, Вы хотите поведать?"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF to_add
  APPEND BLANK
ENDIF
REPLACE CUS_CODE   WITH c_code,     ;
        DOSS_DATE  WITH DATE(),     ;
        DOSS_TIT   WITH d_tit,      ;
        DOSS_MESS  WITH d_mess,     ;
        IMPORTANCE WITH IIF(d_imp, "!", " "), ;
        WHO        WITH user,       ;
        WHERE      WITH arm
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Информационная строка.                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 25.05.97 22:16:20 ═╝
PROCEDURE Sh_User
PRIVATE u_name

ACTIVATE WINDOW (win_name+"_M")
IF .NOT. SEEK(WHO, "PERS999") .OR. WHO = 0
  u_name = IIF(WHO = 0, "САМ ввел!", "")
ELSE  
  u_name = ALLTRIM(PERS999.FAMILY)+" "+LEFT(PERS999.NAME,1)+"."+LEFT(PERS999.S_NAME,1)+"."
ENDIF
u_name = PADR(u_name, 29)
@ WROWS()-2, 3 SAY "Информацию ввел "
@ ROW(), COL() SAY u_name COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS())

RETURN .T.
