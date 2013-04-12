*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Sal_Flg      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║           Коррекция флажков непосредственно перед продажей.            ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 13.05.1999 ══╝
PROCEDURE Sal_Flg

*
*  Переменные состояния для навигации
*
PRIVATE fs_names
IF .NOT. File_O(base_path+"FS_FLAGS.MEM")
  RETURN
ENDIF
RESTORE FROM (base_path+"FS_FLAGS.MEM") ADDITIVE
IF EMPTY(fs_names(1))
  RETURN
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
*
*┌────────────────────────────────────────────┐
*│ [ ] ....:....!....:.  [ ] ....:....!....:. │
*│ [ ] ....:....!....:.  [ ] ....:....!....:. │
*│ [ ] ....:....!....:.  [ ] ....:....!....:. │
*│ [ ] ....:....!....:.  [ ] ....:....!....:. │
*│                                            │
*│      < OK Ctrl-W > < Отказаться Esc >      │
*└────────────────────────────────────────────┘

PRIVATE ex, v_flags, i, s_sav
ex = 1
DIMENSION v_flags(16)
FOR i = 1 TO 16
  v_flags(i) = .NOT. EMPTY(SUBSTR(LIST_TIT.LST_FLAGS,i,1))
ENDFOR

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 52, "Флажки"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
FOR i = 1 TO 4
  IF EMPTY(fs_names(i))
    @ i+1, 3 GET v_flags(i) PICTURE "@*C Резерв" DISABLE
  ELSE
    @ i+1, 3 GET v_flags(i) PICTURE "@*C "+fs_names(i)
  ENDIF
ENDFOR
FOR i = 1 TO 4
  IF EMPTY(fs_names(i+4))
    @ i+1, 25 GET v_flags(i+4) PICTURE "@*C Резерв" DISABLE
  ELSE
    @ i+1, 25 GET v_flags(i+4) PICTURE "@*C "+fs_names(i+4)
  ENDIF
ENDFOR
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  ex = ""
  FOR i = 1 TO 16
    ex = ex+IIF(v_flags(i), "*", " ")
  ENDFOR
  s_sav = SELECT()
  SELECT LIST_TIT
  REPLACE LST_FLAGS WITH ex
  SELECT (s_sav)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
