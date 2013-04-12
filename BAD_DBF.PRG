*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║          Программа формирования списка испорченых DBF-файлов.         ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Bad_Dbf

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

PRIVATE ex, nm_w     &&
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
nm_w = PADR(base_path,40)   &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 69, "Проверка DBF-файлов..."
*
*┌─────────────────────────────────────────────────────────────────┐
*│                                                                 │
*│ Проверяемая директория ....:....1....:....2....:....3....:....4 │
*│                                                                 │
*│                < OK Ctrl-W > < Отказаться Esc >                 │
*│                                                                 │
*└─────────────────────────────────────────────────────────────────┘


*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  3 SAY "Проверяемая директория" GET nm_w PICTURE "@K"
@ 5, 18 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "


READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF ex = 2 .OR. EMPTY(nm_w)
  RETURN
ENDIF
*--------------------------------------------------------------------------

nm_w = ALLTRIM(nm_w)

PRIVATE sav_int, bad_list, bad_len, dbf_list, err_c, dbf_len, mss
DIMENSION bad_list(1)
dbf_len = ADIR(dbf_list,nm_w+"*.DBF")
IF dbf_len = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Директория "+nm_w+" не содержит ни одного DBF-файла!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

sav_int = ON("ERROR")
err_c = 0
ON ERROR err_c = ERROR()
bad_len = 0
FOR ex = 1 TO dbf_len
  WAIT WINDOW dbf_list(ex, 1) NOWAIT
  err_c = 0
  USE (nm_w+dbf_list(ex, 1))
  IF err_c # 0
    bad_len = bad_len+1
    DIMENSION bad_list(bad_len)
    bad_list(bad_len) = PADR(dbf_list(ex, 1),13)+STR(err_c,5)
  ENDIF
ENDFOR
USE
ON ERROR &sav_int

IF bad_len = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ошибок нет!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  RETURN
ENDIF  

PRIVATE view_l

view_l = MIN(WROWS("")-8, bad_len)

DO D_Win_N WITH view_l+3, 22, "Ошибки..."

ex = 1
DO WHILE .T.
  ex = MAX(ex,1)
  ex = MIN(ex, bad_len)
  @ 1,1 MENU bad_list, bad_len, view_l
  READ MENU TO ex
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
ENDDO

RELEASE WINDOW (win_name)

RETURN
