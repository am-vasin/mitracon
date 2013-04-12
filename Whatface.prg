*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла WhatFace     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                              Тип клиента                               ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 18.01.2002 ══╝
PROCEDURE WhatFace
PARAMETER s_value, cli_code
PRIVATE s_sel, is_country, tmpINN, tmpKPP
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
m.tmpINN = ""
m.tmpKPP = ""
IF TYPE("m.cli_code") = "N"
  DO Get_INN WITH m.cli_code, m.tmpINN, m.tmpKPP
ENDIF
*------------------------------------------------------------------------
*    Содержательная часть программы:
*
*   Тип клиента
*┌─────────────────────────┐
*│ Страна ....:....1....:. │
*│  ( ) Юридическое лицо   │
*│  ( ) Предприниматель    │
*│  ( ) Частное лицо       │
*│   ИНН ....:....1..      │
*│   КПП ....:....1....:.  │
*│         < OK >          │
*└─────────────────────────┘

PRIVATE ex, c_type
IF TYPE("s_value") = "C"
  c_type = VAL(s_value)+1
ELSE
  c_type = 0
ENDIF

ex = 1
*------------------------------------------------------------------------
is_country = TYPE("cntr") = "C"
PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH IIF(is_country, 8, 7)+IIF(TYPE("m.cli_code") = "N", 2, 0), 29, "Тип клиента"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

IF is_country
  @ 2, 3 SAY "Страна" GET cntr
ENDIF
@ IIF(is_country, 3, 2), 3 GET c_type PICTURE "@*RV Юридическое лицо;Предприниматель;Частное лицо"
IF TYPE("m.cli_code") = "N"
  @ WROWS()-4, 5 SAY "ИНН" GET m.tmpINN
  @ WROWS()-3, 5 SAY "KPP" GET m.tmpKPP
ENDIF
@ WROWS()-2, 10 GET ex PICTURE "@*HT \! OK "

READ CYCLE VALID Tst_Type()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN TRANSFORM(c_type-1,"@Z 9")+" "+m.tmpINN+CHR(0)+m.tmpKPP

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Type     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Проверка корректности типа.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.01.2002 ──┘
PROCEDURE Tst_Type
PRIVATE mss

IF .NOT. INLIST(c_type, 1, 2, 3)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы так и не указали тип клиента!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF is_country
  IF EMPTY(cntr)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Вы не указали страну!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

IF TYPE("m.cli_code") = "N"
  RETURN Tst_INN(m.cli_code, m.tmpINN)
ENDIF
RETURN .T.
