*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл P_Screen     Разработчик Андрей Васин           14.09.98 17:49:13 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Заставка при печати документа.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE P_Screen
PARAMETERS tit, is_dbf
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

*┌─────────────────────────────────────────────────────────────┐
*│        Поля слева 99      Кол-во экз 99                     │
*│ ┌─ Прогон в конце документа ───────────────┐  < Печать ^W > │
*│ │  ( ) Формата     ( ) Строк   Кол-во 999  │  < Просмотр  > │
*│ └──────────────────────────────────────────┘  < WordPad   > │
*│ ┌── Фрагмент ──────────────────────────────┐  < Файл TXT  > │
*│ │ [ ] Весь документ  Страницы с 999 по 999 │  < Файл DBF  > │
*│ └──────────────────────────────────────────┘  < Факс      > │
*│ ┌─ Ориентация ───────────────┐┌─ Шрифт ────┐  < E-Mail    > │
*│ │ ( ) Книжная  ( ) Альбомная ││ [ ] Сжатый │  < Отказ Esc > │
*│ └────────────────────────────┘└────────────┘                │
*│         ┌──────────────────────────────────┐                │
*│ Принтер │ ....:....!....:....!....:....!.. │                │
*│         └──────────────────────────────────┘                │
*└─────────────────────────────────────────────────────────────┘

PRIVATE ornt, is_con
ornt = IIF(EMPTY(SUBSTR(fnt,2,1)), 1, 2)
is_con = .NOT. EMPTY(LEFT(fnt,1))
PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 16, 65, tit
DO Sun_Bord WITH 3, 3, 5, 46, " Прогон в конце документа "
DO Sun_Bord WITH 6, 3, 8, 46, " Фрагмент "
DO Sun_Bord WITH 9, 3, 11, 32, " Ориентация "
DO Sun_Bord WITH 9, 33, 11, 46, " Шрифт "
@ 13, 3 SAY "Принтер"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3, 49 GET trm PICTURE "@*T \! Печать ^W ;\ Просмотр  ;\ Файл TXT  ;\"+  ;
  IIF(is_dbf, "", "\")+" Файл DBF  ;\ WordPad   ;\ Факс      ;\ E-Mail    ;"+ ;
  IIF(TYPE("winType") = "C", "\", "\\")+" Win_Sale ;\? Отказ ESC "
@  2, 10 SAY "Поля слева" GET l_f PICTURE "@Z 99" VALID V_Left()
@  2, 29  SAY "Кол-во экз" GET n_c PICTURE "@Z 99" VALID V_Copy()
@  4,  6 GET f_sw PICTURE "@*RH Формата    ;Строк" VALID V_Switch()
@  4, 34 SAY "Кол-во" GET f_num PICTURE "@Z 999" WHEN f_sw = 2
@  7,  5 GET frg PICTURE "@*C Весь документ" VALID V_Frag()
@  7, 24 SAY "Страницы с" GET p_s PICTURE "@Z 999" WHEN .NOT. frg VALID V_First()
@  7, 39 SAY "по" GET p_e PICTURE "@Z 999" WHEN .NOT. frg VALID V_Last()
@ 10,  5 GET ornt PICTURE "@*RH Книжная ;Альбомная"
@ 10, 35 GET is_con PICTURE "@*C Сжатый"
@ 12, 11 GET p_d0 PICTURE "@^" FROM p_list SIZE 1, 36 WHEN p_len > 1
READ CYCLE VALID V_Read()
fnt = STUFF(fnt, 2, 1, IIF(ornt=1, " ", "*"))
fnt = STUFF(fnt, 1, 1, IIF(is_con, "*", " "))
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN trm

*************************************************************************
*
*     Проверка правильности полей.
*
PROCEDURE V_Left

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF

IF l_f < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Таких полей в природе не бывает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*************************************************************************
*
*     Проверка правильности числа копий.
*
PROCEDURE V_Copy

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF

IF n_c < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет никакой возможности напечатать столько копий!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF n_c = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Если Вы не хотите печатать документ, не мудрите - нажмите Esc!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*************************************************************************
*
*     Проверка наличия фрагмента.
*
PROCEDURE V_Frag

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF

IF frg
  p_s = 0
  p_e = 0
  SHOW GET p_s
  SHOW GET p_e
ENDIF

RETURN .T.

*************************************************************************
*
*     Проверка переключателя прогона формата.
*
PROCEDURE V_Switch

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF

IF f_sw = 1
  f_num = 0
  SHOW GET f_num
ENDIF

RETURN .T.

*************************************************************************
*
*     Проверка номера начальной страницы.
*
PROCEDURE V_First

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF
IF frg
  RETURN .T.
ENDIF

IF p_s <= 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Таких номеров страниц в природе не бывает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.


*************************************************************************
*
*     Проверка номера конечной страницы.
*
PROCEDURE V_Last

PRIVATE s, mss
s = READKEY() % 256
IF s = 12
  RETURN .T.
ENDIF
IF frg
  RETURN .T.
ENDIF

IF p_e <= 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Таких номеров страниц в природе не бывает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF p_e < p_s
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Таких интервалов страниц в природе не бывает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*************************************************************************
*
*     Проверка выхода из бланка.
*
PROCEDURE V_Read

PRIVATE s, mss
s = READKEY() % 256
IF trm = 7 .OR. READKEY() = 12
  RETURN .T.
ENDIF

IF trm = 1
  IF .NOT. V_Left()
    RETURN .F.
  ENDIF
  IF .NOT. V_Copy()
    RETURN .F.
  ENDIF
  IF .NOT. V_Frag()
    RETURN .F.
  ENDIF
  IF .NOT. V_First()
    RETURN .F.
  ENDIF
  IF .NOT. V_Last()
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.
