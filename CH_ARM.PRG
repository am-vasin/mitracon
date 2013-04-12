*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Ch_Arm       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                         Выбор рабочего места.                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 03.01.2004 ══╝
PROCEDURE Ch_Arm
PARAMETERS tmpCode, tmpArm

PRIVATE c_arm, n_arm, cnt, tmpPoint, s_sav
cnt = 0
s_sav = SELECT()
SELECT 0
tmpPoint = 1
USE (base_path+"ARM_LST") SHARED AGAIN ORDER TAG NAME
SCAN
  cnt = cnt+1
  DIMENSION c_arm(cnt), n_arm(cnt)
  c_arm(cnt) = ARM_C
  n_arm(cnt) = ARM_N
  IF ARM_C == tmpCode
    tmpPoint = cnt
  ENDIF
ENDSCAN

USE
SELECT (s_sav)

IF EMPTY(cnt)
  tmpCode = ""
  RETURN
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
*│┌────────────────────────────────┐│
*││                                ││
*││                                ││
*││                                ││
*││                                ││
*││                                ││
*││                                ││
*││                                ││
*││                                ││
*││                                ││
*││                                ││
*││                                ││
*││                                ││
*││                                ││
*││                                ││
*││                                ││
*│└────────────────────────────────┘│
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH WROWS()-8, 38, "Выбор рабочего места"
*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2,  2 GET tmpPoint FROM n_arm SIZE WROWS()-4, WCOLS()-4 FUNCTION "&N"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF ex=1
  tmpCode = c_arm(tmpPoint)
  IF TYPE("tmpArm") = "C"
    tmpArm = n_arm(tmpPoint)
  ENDIF
ELSE
  tmpCode = "   "
ENDIF

RETURN
