*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Curs      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Выбор типа курса.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.02.1999 ──┘
PROCEDURE Ch_Curs
PARAMETERS c_code, c_name

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

*┌────────────────────────────────┐
*│  ┌──────────────────────────┐  │
*│  │                          │  │
*│  └──────────────────────────┘  │
*│< OK Ctrl-W > < Отказаться Esc >│
*└────────────────────────────────┘

PRIVATE ex, arr_c, arr_n, point, n, ln
ex = SELECT()
SELECT 0
USE (base_path+"COURSE") ALIAS CV_9218 AGAIN
n = 0
point = 1
SCAN
  n = n+1
  DIMENSION arr_c(n), arr_n(n)
  arr_c(n) = B_NAME
  arr_n(n) = L_NAME
  IF PADR(c_code,10) = arr_c(n)
    point = n
  ENDIF
ENDSCAN
USE
SELECT (ex)
IF n = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не создано ни одного типа..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

ex = 1
ln = MIN(n, WROWS("")-15)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH ln+6, 36, "Выберите курс..."

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 4 GET point PICTURE "@& " FROM arr_n SIZE ln+2, 28
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  c_code = arr_c(point)
  IF TYPE("c_name") = "C"
    c_name = arr_n(point)
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1
