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
*│                                  │
*│    [ ] Установить границу 99%    │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, disbound, yes
IF File_O(base_path+"DISBOUND.MEM")
  RESTORE FROM (base_path+"DISBOUND.MEM") ADDITIVE
  yes = .T.
ELSE
  disbound = 0
  yes = .F.
ENDIF
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, '"Большая" скидка'

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 32 SAY "%"
@ 3, 6 GET yes PICTURE "@*C Установить границу" VALID C_Val()
@ 3, 29 GET disbound WHEN yes PICTURE "@Z 99"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  IF yes
    SAVE ALL LIKE disbound TO (base_path+"DISBOUND.MEM")
  ELSE
    DELETE FILE (base_path+"DISBOUND.MEM")
 ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*******************************************************************
PROCEDURE C_Val
IF .NOT. yes
  disbound = 0
  SHOW GET disbound
ENDIF

RETURN .T.
