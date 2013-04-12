*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Set_LFS      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║       Процедура очистки/установки флажков для списка (продажи).        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 27.03.1999 ══╝
PROCEDURE Set_LFS
PRIVATE fs_names, n_flags, vl, i
DIMENSION vl(16)

IF File_O(base_path+"FS_FLAGS.MEM")
  RESTORE FROM (base_path+"FS_FLAGS.MEM") ADDITIVE
ELSE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ни один флажок не описан!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
n_flags = 0
FOR i = 1 TO 8
  IF EMPTY(fs_names(i))
    EXIT
  ELSE
    n_flags = n_flags+1
  ENDIF
ENDFOR
IF n_flags = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ни один флажок не описан!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

FOR i = 1 TO 16
  vl(i) = .NOT. EMPTY(SUBSTR(LIST_TIT.LST_FLAGS,i,1))
ENDFOR

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
*│      [ ] ....:....!....:.      │
*│      [ ] ....:....!....:.      │
*│      [ ] ....:....!....:.      │
*│      [ ] ....:....!....:.      │
*│< OK Ctrl-W > < Отказаться Esc >│
*└────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH n_flags+4, 36, "Флажки для списка"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
FOR i = 1 TO n_flags
  @ 1+i, 8 GET vl(i) PICTURE "@*C "+fs_names(i)
ENDFOR

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  ex = ""
  FOR i = 1 TO 16
    ex = ex +IIF(vl(i), "*", " ")
  ENDFOR
  REPLACE LIST_TIT.LST_FLAGS WITH ex
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
