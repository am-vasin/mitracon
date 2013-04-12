*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Bill_0       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Программа выписки и/или печати счета.                  ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 19.01.1999 ══╝
PROCEDURE Bill_0
PARAMETERS l_num

PRIVATE s_sav, d_num, d_dat, d_fir

d_num = ""
d_dat = {}
d_fir = ""
s_sav = SELECT()
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_9119 AGAIN
IF .NOT. SEEK(l_num)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Списка N "+ALLTRIM(l_num)+" не существует"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

IF .NOT. EMPTY(BILL_NUM)
  d_num = LEFT(BILL_NUM,4)+"-"+ALLTRIM(RIGHT(BILL_NUM,6))
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

*┌──────────────────────────────────────────────────────┐
*│                                                      │
*│ < Напечатать > < Выписать новый > < Отказаться Esc > │
*│                                                      │
*└──────────────────────────────────────────────────────┘
*┌─────────────────────────────────┐
*│                                 │
*│ < Выписать > < Отказаться Esc > │
*│                                 │
*└─────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
IF EMPTY(d_num)
  DO D_Win_N WITH 6, 37, "Счет"
ELSE
  DO D_Win_N WITH 6, 58, "Счет N "+d_num
ENDIF

*------------------------------------------------------------------------
*      Ввод полей бланка
*


IF EMPTY(d_num)
  @ 3, 3 GET ex PICTURE "@*HT \ Выписать ;\? Отказаться Esc "
ELSE
  @ 3, 3 GET ex PICTURE "@*HT \ Напечатать ;\ Выписать новый ;\? Отказаться Esc "
ENDIF
READ CYCLE
IF EMPTY(d_num)
  ex = ex+1
ENDIF

DO CASE
CASE ex = 1
  DO P_Bill WITH BILL_FIRM, BILL_NUM, BILL_YEAR
CASE ex = 2
  DO Bill WITH l_num
ENDCASE

USE
SELECT (s_sav)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
