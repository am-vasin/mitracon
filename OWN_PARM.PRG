*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Own_Parm     Разработчик Андрей Васин           07.03.97 13:18:41 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Настройка параметров АРМа.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Own_Parm

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

PRIVATE ex, p00, p01, p02, p03, p04, scr
PRIVATE inv_list, nam_list, i, s_sav, point
scr = .F.
ex   = 1
p00  = m.showLett
p01  = prn_mode
p02  = own_mail
s_sav = SELECT()
SELECT 0
USE (base_path+"DOC_FORM") ALIAS D_9502 AGAIN && IN 0
i = 0
SCAN
  IF LEFT(DOC_NAME,7) # "INVOICE"
    LOOP
  ENDIF
  i = i+1
  DIMENSION inv_list(i), nam_list(i)
  inv_list(i) = SUBSTR(DOC_NAME,8,1)
  nam_list(i) = DOC_PROMPT
ENDSCAN
USE

IF choose_inv == "-"
  p03 = .T.
  p04 = SPACE(40)
  point = 1
ELSE
  p03 = .F.
  point = ASCAN(inv_list, choose_inv)
  p04 = nam_list(point)
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 12, 49, "Настройка АРМа"
DO Sun_Bord WITH  5,  2,  8, 46, " Формат накладной "
*
*┌─────────────────────────────────────────────┐
*│ [ ] Показывать сообщения внутренней почты   │
*│ [ ] Выдавать заставку при печати документов │
*│      [ ] Собственная электронная почта      │
*│┌─ Формат накладной ────────────────────────┐│
*││      [ ] Выбор формата при печати         ││
*││ ....:....!....:....!....:....!....:....!  ││
*│└───────────────────────────────────────────┘│
*│      < Подписка по внутренней почте >       │
*│      < OK Ctrl-W > < Отказаться Esc >       │
*└─────────────────────────────────────────────┘

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2, 3 GET p00 PICTURE "@*C Показывать сообщения внутренней почты"
@  3, 3 GET p01 PICTURE "@*C Выдавать заставку при печати документов"
@  3, 3 GET p01 PICTURE "@*C Выдавать заставку при печати документов"
@  4, 8 GET p02 PICTURE "@*C Собственная электронная почта"
@  6, 9 GET p03 PICTURE "@*C Выбор формата при печати" VALID C_Form()
@  7, 4 GET p04 WHEN .F.
@  9, 8 GET scr PICTURE "@*C Подписка по внутренней почте " VALID Ss()
*@  9, 8 GET scr PICTURE "@*H \ Подписка по внутренней почте " VALID Ss()
@ 10, 8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1 .AND. READKEY() % 256 # 12
  m.showLett = p00
  m.prn_mode = p01
  m.own_mail = p02
  IF p03
    m.choose_inv = "-"
  ELSE
    m.choose_inv = inv_list(point)
  ENDIF
  SELECT 0
  DELETE FILE (prmo_path+"OWN_PARM.DBF")
  CREATE DBF (prmo_path+"OWN_PARM.DBF")  ;
      ( PRN_MODE   L,    ;
        OWN_MAIL   L,    ;
        SHOW_LETT  L,    ;
        CHOOSE_INV C(1)  )
  APPEND BLANK
  REPLACE OWN_PARM.PRN_MODE   WITH m.prn_mode,  ;
          OWN_PARM.SHOW_LETT  WITH m.showLett,  ;
          OWN_PARM.OWN_MAIL   WITH m.own_mail,  ;
          OWN_PARM.CHOOSE_INV WITH m.choose_inv
  USE
ENDIF
SELECT (s_sav)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура C_Form       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Выбор формата из списка.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.06.1999 ──┘
PROCEDURE C_Form

IF p03
  p04 = SPACE(40)
  point = 1
  SHOW GET p04
  RETURN .T.
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

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH MIN(ALEN(nam_list)+3, WROWS("")-5), LEN(nam_list(1))+4, "Форматы накладных"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO WHILE .T.
  point = MIN(point, ALEN(nam_list))
  point = MAX(point, 1)
  @ 1, 1 MENU nam_list, ALEN(nam_list), WROWS()-3
  READ MENU TO point
  IF READKEY() = 12
    point = 1
    p03   = .T.
    EXIT
  ENDIF
  IF READKEY() = 15
    p04 = nam_list(point)
    EXIT
  ENDIF
ENDDO

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
SHOW GET p03
SHOW GET p04

RETURN .T.

*--------------------------------------------------------------------------
PROCEDURE Ss

DO Sscrpt
scr = .F.
SHOW GETS
RETURN .T.
