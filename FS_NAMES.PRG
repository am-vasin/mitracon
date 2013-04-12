*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла FS_Names     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║      Программа создания описания флажков для списков (накладных).      ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 03/27/1999 ══╝
PROCEDURE FS_Names

PRIVATE n_flags, fs_names, i

IF File_O(base_path+"FS_FLAGS.MEM")
  RESTORE FROM (base_path+"FS_FLAGS.MEM") ADDITIVE
ELSE
  DIMENSION fs_names(16)
  FOR i = 1 TO 16
    fs_names(i) = SPACE(16)
  ENDFOR
ENDIF
n_flags = 0
FOR i = 1 TO 8
  IF EMPTY(fs_names(i))
    EXIT
  ELSE
    n_flags = n_flags+1
  ENDIF
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
*│      1. ....:....!....:.       │
*│      2. ....:....!....:.       │
*│      3. ....:....!....:.       │
*│      4. ....:....!....:.       │
*│      5. ....:....!....:.       │
*│      6. ....:....!....:.       │
*│      7. ....:....!....:.       │
*│      8. ....:....!....:.       │
*│                                │
*│< OK Ctrl-W > < Отказаться Esc >│
*└────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 13, 36, "Описание флажков"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@  2, 7 SAY " 1." GET fs_names( 1) VALID V_Flg( 1) WHEN n_flags >=  0
@  3, 7 SAY " 2." GET fs_names( 2) VALID V_Flg( 2) WHEN n_flags >=  1
@  4, 7 SAY " 3." GET fs_names( 3) VALID V_Flg( 3) WHEN n_flags >=  2
@  5, 7 SAY " 4." GET fs_names( 4) VALID V_Flg( 4) WHEN n_flags >=  3
@  6, 7 SAY " 5." GET fs_names( 5) VALID V_Flg( 5) WHEN n_flags >=  4
@  7, 7 SAY " 6." GET fs_names( 6) VALID V_Flg( 6) WHEN n_flags >=  5
@  8, 7 SAY " 7." GET fs_names( 7) VALID V_Flg( 7) WHEN n_flags >=  6
@  9, 7 SAY " 8." GET fs_names( 8) VALID V_Flg( 8) WHEN n_flags >=  7
*@ 10, 7 SAY " 9." GET fs_names( 9) VALID V_Flg( 9) WHEN n_flags >=  8
*@ 11, 7 SAY "10." GET fs_names(10) VALID V_Flg(10) WHEN n_flags >=  9
*@ 12, 7 SAY "11." GET fs_names(11) VALID V_Flg(11) WHEN n_flags >= 10
*@ 13, 7 SAY "12." GET fs_names(12) VALID V_Flg(12) WHEN n_flags >= 11
*@ 14, 7 SAY "13." GET fs_names(13) VALID V_Flg(13) WHEN n_flags >= 12
*@ 15, 7 SAY "14." GET fs_names(14) VALID V_Flg(14) WHEN n_flags >= 13
*@ 16, 7 SAY "15." GET fs_names(15) VALID V_Flg(15) WHEN n_flags >= 14
*@ 17, 7 SAY "16." GET fs_names(16) VALID V_Flg(16) WHEN n_flags >= 15

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  SAVE ALL LIKE fs_names TO (base_path+"FS_FLAGS.MEM")

ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Flg        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Выход из поля имени флажка.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/27/1999 ──┘
PROCEDURE V_Flg
PARAMETERS i0
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF i0 <= n_flags .AND. EMPTY(fs_names(i0))   && Флажок уже был заполнен 
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Это имя уже нельзя очистить!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF i0 = n_flags+1 .AND. .NOT. EMPTY(fs_names(i0))   && Флажок появился!
  n_flags = n_flags+1
ENDIF
RETURN .T.
