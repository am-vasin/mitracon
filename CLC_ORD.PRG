*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Clc_Ord      Разработчик Андрей Васин           06.05.98 16:29:41 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Расчет рекомендуемого заказа.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Clc_Ord

*
*   Параметры заказа
*
PRIVATE is1, is2, is3, is4, is5, is6, is7
PRIVATE set_p1, set_p2, set_p3, set_p4, set_p5

STORE .F. TO is1, is2, is3, is4, is5, is6, is7
STORE .F. TO set_p1, set_p2, set_p3, set_p4, set_p5

IF G_Parm()
  DO Clc_Qnt
ENDIF

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Установка параметров заказа.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.05.98 18:49:31 ═╝
PROCEDURE G_Parm

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

*┌─────────────────────────────────────────────────┐
*│ ┌─ Состав заявки ─────────────────────────────┐ │
*│ │ Тип составляющей                  Параметры │ │
*│ │ [ ] Продажи                          [ ]    │ │
*│ │ [ ] Спрос                            [ ]    │ │
*│ │ [ ] Давно отсутствующие на складе    [ ]    │ │
*│ │ [ ] Новые                            [ ]    │ │
*│ │ [ ] Фирма-производитель           ........  │ │
*│ │ [ ] Долг                                    │ │
*│ │ [ ] Из файла ....:....!....:....!....:....! │ │
*│ └─────────────────────────────────────────────┘ │
*│ ┌─ Общие параметры ───────────────────────────┐ │
*│ │ [ ] Записать поверх                         │ │
*│ │ [ ] Учитывать флажок "не заказывать"        │ │
*│ └─────────────────────────────────────────────┘ │
*│        < OK Ctrl-W > < Отказаться Esc >         │
*└─────────────────────────────────────────────────┘

PRIVATE ex
ex   = 1             &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 18, 53, "Параметры заявки"
DO Sun_Bord WITH  2,  3, 11, 49, " Состав заявки "
DO Sun_Bord WITH 12,  3, 15, 49, " Общие параметры "
@ 3, 5 SAY "Тип составляющей                  Параметры"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  4,  5 GET is1 PICTURE "@*C Продажи" WHEN .NOT. EMPTY(d1) .AND. .NOT. EMPTY(d2)
@  4, 42 GET set_p1 PICTURE "@*C " WHEN is1 VALID S_P1()
@  5,  5 GET is2 PICTURE "@*C Спрос" WHEN .NOT. EMPTY(d1) .AND. .NOT. EMPTY(d2)
@  5, 42 GET set_p2 PICTURE "@*C " WHEN is2 VALID S_P2()
@  6,  5 GET is3 PICTURE "@*C Давно отсутствующие на складе" WHEN .NOT. EMPTY(d1) .AND. .NOT. EMPTY(d2)
@  6, 42 GET set_p3 PICTURE "@*C " WHEN is3 VALID S_P3()
@  7,  5 GET is4 PICTURE "@*C Новые"
@  7, 42 GET set_p4 PICTURE "@*C " WHEN is4 VALID S_P4()
@  8,  5 GET is5 PICTURE "@*C Фирма-производитель" VALID S_P5()
@  9,  5 GET is6 PICTURE "@*C Долг" WHEN .NOT. EMPTY(d1) .AND. .NOT. EMPTY(d2)
@ 10,  5 GET is7 PICTURE "@*C Из файла" VALID S_P7()
@ 13,  5 GET __over PICTURE "@*C Записать поверх"
@ 14,  5 GET __stop_ord PICTURE '@*C Учитывать флажок "не заказывать"'
@ 16, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║           Коррекция параметров для составляющей по продажам.           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.98 13:31:03 ═╝
PROCEDURE S_P1
EXTERNAL ARRAY __c_scale

IF .NOT. set_p1
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

*┌──────────────────────────────────┐
*│    Таблица градаций MIN и MAX    │
*│          запасов в днях          │
*│    ┌─ Цена от...──MIN──MAX──┐    │
*│    │  99999.999   999  999  │    │
*│    │  99999.999   999  999  │    │
*│    │  99999.999   999  999  │    │
*│    │  99999.999   999  999  │    │
*│    │  99999.999   999  999  │    │
*│    │  99999.999   999  999  │    │
*│    │  99999.999   999  999  │    │
*│    │  99999.999   999  999  │    │
*│    │  99999.999   999  999  │    │
*│    │  99999.999   999  999  │    │
*│    └────────────────────────┘    │
*│  MIN сумма по позиции 9999.999   │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, _0c_scale, _0min_p, i, j
DIMENSION _0c_scale(10,3)
FOR i = 1 TO 10
  FOR j = 1 To 3
   _0c_scale(i,j) = __c_scale(i,j)
  ENDFOR
ENDFOR
_0min_p   = __min_p
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 20, 38, "Составляющая по продажам"
@ 2,  6 SAY "Таблица градаций MIN и MAX"
@ 3, 12 SAY "запасов в днях"
DO Sun_Bord WITH 4, 6, 15, 31
@ 4, 8 SAY " Цена от...──MIN──MAX"


*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  5,  9 GET _0c_scale( 1,1) WHEN .F. PICTURE "@Z 99999.999"
@  5, 21 GET _0c_scale( 1,2) PICTURE "@Z 999" WHEN Enter2(1) VALID Exit2(1)
@  5, 26 GET _0c_scale( 1,3) PICTURE "@Z 999" WHEN Enter3(1) VALID Exit3(1)

@  6,  9 GET _0c_scale( 2,1) PICTURE "@Z 99999.999" WHEN Enter1(2) VALID Exit1(2)
@  6, 21 GET _0c_scale( 2,2) PICTURE "@Z 999" WHEN Enter2(2) VALID Exit2(2)
@  6, 26 GET _0c_scale( 2,3) PICTURE "@Z 999" WHEN Enter3(2) VALID Exit3(2)

@  7,  9 GET _0c_scale( 3,1) PICTURE "@Z 99999.999" WHEN Enter1(3) VALID Exit1(3)
@  7, 21 GET _0c_scale( 3,2) PICTURE "@Z 999" WHEN Enter2(3) VALID Exit2(3)
@  7, 26 GET _0c_scale( 3,3) PICTURE "@Z 999" WHEN Enter3(3) VALID Exit3(3)

@  8,  9 GET _0c_scale( 4,1) PICTURE "@Z 99999.999" WHEN Enter1(4) VALID Exit1(4)
@  8, 21 GET _0c_scale( 4,2) PICTURE "@Z 999" WHEN Enter2(4) VALID Exit2(4)
@  8, 26 GET _0c_scale( 4,3) PICTURE "@Z 999" WHEN Enter3(4) VALID Exit3(4)

@  9,  9 GET _0c_scale( 5,1) PICTURE "@Z 99999.999" WHEN Enter1(5) VALID Exit1(5)
@  9, 21 GET _0c_scale( 5,2) PICTURE "@Z 999" WHEN Enter2(5) VALID Exit2(5)
@  9, 26 GET _0c_scale( 5,3) PICTURE "@Z 999" WHEN Enter3(5) VALID Exit3(5)

@ 10,  9 GET _0c_scale( 6,1) PICTURE "@Z 99999.999" WHEN Enter1(6) VALID Exit1(6)
@ 10, 21 GET _0c_scale( 6,2) PICTURE "@Z 999" WHEN Enter2(6) VALID Exit2(6)
@ 10, 26 GET _0c_scale( 6,3) PICTURE "@Z 999" WHEN Enter3(6) VALID Exit3(6)

@ 11,  9 GET _0c_scale( 7,1) PICTURE "@Z 99999.999" WHEN Enter1(7) VALID Exit1(7)
@ 11, 21 GET _0c_scale( 7,2) PICTURE "@Z 999" WHEN Enter2(7) VALID Exit2(7)
@ 11, 26 GET _0c_scale( 7,3) PICTURE "@Z 999" WHEN Enter3(7) VALID Exit3(7)

@ 12,  9 GET _0c_scale( 8,1) PICTURE "@Z 99999.999" WHEN Enter1(8) VALID Exit1(8)
@ 12, 21 GET _0c_scale( 8,2) PICTURE "@Z 999" WHEN Enter2(8) VALID Exit2(8)
@ 12, 26 GET _0c_scale( 8,3) PICTURE "@Z 999" WHEN Enter3(8) VALID Exit3(8)

@ 13,  9 GET _0c_scale( 9,1) PICTURE "@Z 99999.999" WHEN Enter1(9) VALID Exit1(9)
@ 13, 21 GET _0c_scale( 9,2) PICTURE "@Z 999" WHEN Enter2(9) VALID Exit2(9)
@ 13, 26 GET _0c_scale( 9,3) PICTURE "@Z 999" WHEN Enter3(9) VALID Exit3(9)

@ 14,  9 GET _0c_scale(10,1) PICTURE "@Z 99999.999" WHEN Enter1(10) VALID Exit1(10)
@ 14, 21 GET _0c_scale(10,2) PICTURE "@Z 999" WHEN Enter2(10) VALID Exit2(10)
@ 14, 26 GET _0c_scale(10,3) PICTURE "@Z 999" WHEN Enter3(10) VALID Exit3(10)

@ 16,  4 SAY "MIN сумма по позиции" GET _0min_p PICTURE "9999.999" WHEN Clr_Tab()
@ 18,  3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc " WHEN Clr_Tab()

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF ex = 1
  FOR i = 1 TO 10
    FOR j = 1 To 3
      __c_scale(i,j) = _0c_scale(i,j)
    ENDFOR
  ENDFOR
  __min_p   = _0min_p
ENDIF

set_p1 = .F.
SHOW GET set_p1
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Вход/выход из полей таблицы.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.98 15:16:09 ═╝
************************* Граница ****************************************
PROCEDURE Enter1
PARAMETER lin
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
IF lin = 2 .OR. _0c_scale(lin-1,1) # 0
  RETURN .T.
ENDIF
RETURN .F.

PROCEDURE Exit1
PARAMETER lin
PRIVATE i
DO CASE
CASE _0c_scale(lin,1) < 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Отрицательная цена? Здесь что-то не так!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
CASE _0c_scale(lin,1) = 0
  FOR i = lin TO 10
    _0c_scale(i,1) = 0
    SHOW GET _0c_scale(i,1)
    _0c_scale(i,2) = 0
    SHOW GET _0c_scale(i,2)
    _0c_scale(i,3) = 0
    SHOW GET _0c_scale(i,3)
  ENDFOR
OTHERWISE
  IF _0c_scale(lin,1) < _0c_scale(lin-1,1)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Посмотрите на предыдущую границу. Здесь что-то не так!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDCASE
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
RETURN .T.

************************* MIN ****************************************
PROCEDURE Enter2
PARAMETER lin
IF _0c_scale(lin,1) = 0 .AND. lin # 1
  RETURN .F.
ENDIF
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
RETURN .T.

PROCEDURE Exit2
PARAMETER lin
IF _0c_scale(lin,2) < 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Здесь отрицательных чисел быть не может!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
RETURN .T.

************************* MAX ****************************************
PROCEDURE Enter3
PARAMETER lin
IF _0c_scale(lin,1) = 0 .AND. lin # 1
  RETURN .F.
ENDIF
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
RETURN .T.

PROCEDURE Exit3
PARAMETER lin
IF _0c_scale(lin,3) < 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Здесь отрицательных чисел быть не может!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
RETURN .T.

PROCEDURE Clr_Tab
@ 5, 7 FILL TO 14, 30 COLOR (SCHEME(13,1))
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║              Коррекция параметров составляющей по спросу.              ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 08.05.98 11:16:37 ═╝
PROCEDURE S_P2
*
*  Переменные состояния для навигации
*

IF .NOT. set_p2
  RETURN .T.
ENDIF

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

*┌──────────────────────────────────────────────┐
*│                                              │
*│  Коэффициент для количества деталей  999.99  │
*│                                              │
*│  Коэффициент для количества запросов 999.99  │
*│                                              │
*│       Порог по количеству запросов 999       │
*│                                              │
*│   Min сумма 999999999  Max сумма 999999999   │
*│                                              │
*│       < OK Ctrl-W > < Отказаться Esc >       │
*└──────────────────────────────────────────────┘

PRIVATE ex, k_1, k_2, p, m1, m2
k_1 = __koeff_q       && Коэффициент для количества деталей
k_2 = __koeff_n       && Коэффициент для количества запросов
p   = __porog         && Порог по кол-ву запросов
m1  = __min_s         && Минимальная стоимость заказа
m2  = __max_s         && Максимальная стоимость заказа
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 13, 50, "Параметры составляющей по спросу"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  4 SAY "Коэффициент для количества деталей " GET k_1 PICTURE "999.99"
@  5,  4 SAY "Коэффициент для количества запросов" GET k_2 PICTURE "999.99"
@  7,  9 SAY "Порог по количеству запросов" GET p PICTURE "999"
@  9,  5 SAY "Min сумма" GET m1 PICTURE "999999999"
@  9, 26 SAY "Max сумма" GET m2 PICTURE "999999999"
@ 11,  9 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  __koeff_q = k_1      && Коэффициент для количества деталей
  __koeff_n = k_2      && Коэффициент для количества запросов
  __porog   = p        && Порог по кол-ву запросов
  __min_s   = m1       && Минимальная стоитмость заказа
  __max_s   = m2       && Максимальная стоитмость заказа
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
set_p2 = .F.
SHOW GET set_p2
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║   Коррекция параметров составляющей по давно отсутствующим позициям.   ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 08.05.98 11:16:37 ═╝
PROCEDURE S_P3
*
*  Переменные состояния для навигации
*

IF .NOT. set_p3
  RETURN .T.
ENDIF

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

*┌──────────────────────────────────────────────┐
*│                                              │
*│   Сколько дней позиция не продавалась 9999   │
*│                                              │
*│              Количество 999999               │
*│                                              │
*│       < OK Ctrl-W > < Отказаться Esc >       │
*└──────────────────────────────────────────────┘

PRIVATE ex, k_1, k_2
k_1 = __abs_q   && Количество
k_2 = __abs_age && Срок
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 50, "Отсутствующие позиции"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 5 SAY "Сколько дней позиция не продавалась" GET k_2 PICTURE "9999"
@ 5,16 SAY "Количество" GET k_1 PICTURE "999999"
@ 7, 9 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  __abs_q   = k_1 && Количество
  __abs_age = k_2 && Срок
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
set_p3 = .F.
SHOW GET set_p3
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║           Коррекция параметров составляющей по новым позициям.         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 08.05.98 11:16:37 ═╝
PROCEDURE S_P4
*
*  Переменные состояния для навигации
*

IF .NOT. set_p4
  RETURN .T.
ENDIF

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

*┌───────────────────────────────────┐
*│                                   │
*│   "Возраст" позиций в днях 9999   │
*│                                   │
*│         Количество 999999         │
*│                                   │
*│ < OK Ctrl-W > < Отказаться Esc >  │
*└───────────────────────────────────┘

PRIVATE ex, k_1, k_2
k_1 = __new_q   && Количество
k_2 = __new_d   && "Возраст" в днях
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 39, "Новые позиции"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 5 SAY '"Возраст" позиций в днях' GET k_2 PICTURE "9999"
@ 5,11 SAY "Количество" GET k_1 PICTURE "999999"
@ 7, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  __new_q = k_1   && Количество
  __new_d = k_2   && "Возраст" в днях
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
set_p4 = .F.
SHOW GET set_p4
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                        Фирма - производитель.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 08.05.98 11:16:37 ═╝
PROCEDURE S_P5
*
*  Переменные состояния для навигации
*

IF .NOT. is5
  @ 8, 39 SAY "        "
  RETURN .T.
ENDIF

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
*Фирма-производитель
*┌────────────────────────────────┐
*│                                │
*│         Фирма ....!...         │
*│                                │
*│       Количество 999999        │
*│                                │
*│< OK Ctrl-W > < Отказаться Esc >│
*└────────────────────────────────┘

PRIVATE ex, k_1, point, i
s_sav = SELECT()
SELECT 0
USE (base_path+"DEVELOP") ORDER TAG CODE ALIAS W0003 AGAIN
IF SEEK(fir_code)
  point = LEFT(B_NAME,8)
ELSE
  point = SPACE(8)
ENDIF
USE
SELECT (s_sav)
k_1 = __fir_q   && Количество
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 36, "Фирма-производитель"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,11 SAY "Фирма" GET point VALID Develop(fir_code)
@ 5,  9 SAY "Количество" GET k_1 PICTURE "999999"
@ 7,  2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF ex = 1
  __fir_q = k_1
ELSE  
  is5 = .F.
  SHOW GET is5
ENDIF
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Список из файла.                              ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 08.05.98 11:16:37 ═╝
PROCEDURE S_P7

IF .NOT. is7
  @ 10, 18 SAY SPACE(30)
  RETURN .T.
ENDIF

__f_name = Int_File("", "*", "Укажите файл со списком")
IF .NOT. EMPTY(__f_name)
  PRIVATE s_sav, err_cod, err_lst
  DIMENSION err_lst(1)
  err_lst(1) = 15
  err_cod = 0
  s_sav = SELECT()
  SELECT 0
  USE (ALLTRIM(__f_name)) ALIAS w333
  IF err_cod = 0
    IF TYPE("CODE") # "N" .OR. ;
       TYPE("PREFIX") # "C" .OR. ;
       TYPE("NAME") # "C" .OR. ;
       TYPE("PRODUCER") # "C" .OR. ;
       TYPE("QNT") # "N"
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Структура файла не соответствует заказу!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      __f_name = ""
    ENDIF
  ELSE
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Это не DBF-файл!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    __f_name = ""
  ENDIF
  USE
  SELECT (s_sav)
ENDIF
IF EMPTY(__f_name)
  is7 = .F.
  SHOW GET is7
  @ 10, 18 SAY SPACE(30)
ELSE
  @ 10, 18 SAY PADR(__f_name,30) COLOR SCHEME 1
ENDIF

RETURN .T.
*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║              Расчет количеств для рекомендуемого заказа.               ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 12.05.98 13:52:43 ═╝
PROCEDURE Clc_Qnt

PRIVATE s_sav, ord_sav, rc_sav, q_w, p_own
PRIVATE p_sup_d, p_sup_r, own_v, i, q_n, q_p, cnt, sw

IF .NOT. is1 .AND. .NOT. is2 .AND. .NOT. is3 .AND. .NOT. is4 .AND.   ;
   .NOT. is5 .AND. .NOT. is6 .AND. .NOT. is7
  RETURN
ENDIF

s_sav = SELECT()

SELECT 0
USE (base_path+"PARMS") ALIAS P13598 AGAIN
own_v = VALUTA
USE
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST12598 AGAIN

SELECT ORD_TMP
rc_sav = RECNO()

IF is1 .OR. is2 .OR. is3 .OR. is4 .OR. is5 .OR. is6
  DO Wt_Mess WITH "Формирование заказа"
  cnt = 0
  SCAN
    cnt = cnt+1
    IF cnt % 100 = 0
      @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)),WCOLS())
    ENDIF
    q_w = 0
    p_own = 0
    p_sup_d = 0
    p_sup_r = 0
    IF OWN
      IF SEEK(CODE,"ST12598")
        IF .NOT. EMPTY(ST12598.WK_SW1) .AND. __stop_ord
          LOOP
        ENDIF
        p_own = IIF(own_v = "$", ST12598.PRICE, ST12598.PRICE_R)
      ENDIF
    ENDIF
    IF ORD_TMP.OFFER_REC # 0
      GO ORD_TMP.OFFER_REC IN OFFER
      p_sup_r = OFFER.PRICE_R
      p_sup_d = OFFER.PRICE_D
    ENDIF
    IF is1    && По продажам
      q_n = __c_scale(1,2)
      q_p = __c_scale(1,3)
      FOR i = 2 TO 10
        IF __c_scale(i,1) = 0 .OR. p_own <= __c_scale(i,1)
          EXIT
        ENDIF
        q_n = __c_scale(i,2)
        q_p = __c_scale(i,3)
      ENDFOR
      q_w = QNT_SALE/__srok   && Среднедневная продажа
      IF OWN .AND. FOUND("ST12598")
        q_w = IIF(q_w*q_n > ST12598.QNT, ROUND(q_w*q_p,0)-ST12598.QNT, 0)
      ENDIF
      *
      *   Добавляем до нужной суммы по позиции
      IF q_w > 0 .AND. __min_p > 0
        IF own_v = "$"
          IF p_sup_d > 0
            q_w = MAX(q_w, CEILING(__min_p/p_sup_d))
          ENDIF
        ELSE
          IF p_sup_r > 0
            q_w = MAX(q_w, CEILING(__min_p/p_sup_r))
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    IF is2    && Спрос
      sw = .T.
      IF OWN
        IF QNT_SALE > 0   && Эту позицию заказываем на основе продаж
          sw = .F.
        ENDIF
        IF FOUND("ST12598") .AND. ST12598.QNT > 0  && А эта и так уже есть
          sw = .F.
        ENDIF
      ENDIF
      IF QNT_DEM_N < __porog     && Недостаточно много раз спросили!
        sw = .F.
      ENDIF
      IF sw
        p_sup_r = 0
        p_sup_d = 0
        IF ORD_TMP.OFFER_REC # 0
          GO ORD_TMP.OFFER_REC IN OFFER
          p_sup_r = OFFER.PRICE_R
          p_sup_d = OFFER.PRICE_D
        ENDIF
        q_w = q_w+ROUND(__koeff_q*QNT_DEM_Q+__koeff_n*QNT_DEM_N, 0)
        IF own_v = "$"
          IF p_sup_d > 0 .AND. __min_s > 0
            q_w = MAX(q_w, CEILING(__min_s/p_sup_d))
          ENDIF
          IF p_sup_d > 0 .AND. __max_s > 0
            q_w = MIN(q_w, CEILING(__max_s/p_sup_d))
          ENDIF
        ELSE
          IF p_sup_r > 0 .AND. __min_s > 0
            q_w = MAX(q_w, CEILING(__min_s/p_sup_r))
          ENDIF
          IF p_sup_r > 0 .AND. __max_s > 0
            q_w = MIN(q_w, CEILING(__max_s/p_sup_r))
          ENDIF
        ENDIF
      ENDIF
    ENDIF
    IF is3    && Давно отсутствующая на складе
      IF OWN .AND. FOUND("ST12598")
        IF ST12598.QNT = 0
          q_w = q_w+IIF(DATE()-LAST_SALE > __abs_age .OR. LAST_SALE = {}, __abs_q, 0)
        ENDIF
      ENDIF
    ENDIF
    IF is4     && Новые
      IF ORD_TMP.OFFER_REC # 0
        GO ORD_TMP.OFFER_REC IN OFFER
        IF .NOT. EMPTY(OFFER.DATE_ON) .AND. DATE()-OFFER.DATE_ON < __new_d
          q_w = q_w+__new_q
        ENDIF
      ENDIF
    ENDIF
    IF is5     && Фирма
      IF FOUND("ST12598") .AND. fir_code = ST12598.PRO_CODE
        q_w = q_w+__fir_q
      ENDIF
    ENDIF
    IF is6     && Долг
      q_w = q_w+QNT_DUE
    ENDIF
    REPLACE QNT WITH q_w+IIF(__over, 0, QNT)
  ENDSCAN
  DO Wt_Mess
ENDIF

*
*  Заявка из файла
*
ord_sav = TAG()
SET ORDER TO TAG CODE
IF is7
  DO Wt_Mess WITH "Чтение заказа из файла"
  SELECT 0
  USE (__f_name) ALIAS IN
  SCAN FOR CODE # 0
    IF .NOT. SEEK(CODE, "ORD_TMP")
      LOOP
    ENDIF
    IF SEEK(CODE,"ST12598")
      IF .NOT. EMPTY(ST12598.WK_SW1) .AND. __stop_ord
        LOOP
      ENDIF
    ENDIF
    REPLACE ORD_TMP.QNT WITH ORD_TMP.QNT+QNT
  ENDSCAN

  USE
  DO Wt_Mess
ENDIF

SELECT ST12598
USE
SELECT ORD_TMP
SET ORDER TO TAG (ord_sav)
IF BETWEEN(rc_sav, 1, RECCOUNT())
  GO rc_sav
ENDIF

SELECT (s_sav)

RETURN
