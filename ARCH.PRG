*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                  Программа архивирования директории.                  ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Arch
PARAMETER pth, arch_n, ttl

*   pth    - архивируемая директория;
*   arch_n - имя создаваемого файла архива;
*   ttl    - надпись на форточке
*
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

PRIVATE ex, max_sz   &&
                     &&   Объявляем и заполняем поля бланка
ex     = 1           &&
max_sz = 1           &&
*------------------------------------------------------------------------
*
*┌─────────────────────────────────────────────────────────────┐
*│ ┌─ Максимальный размер архивного файла ───────────────────┐ │
*│ │ ( ) Неограничен  ( ) 1.4M  ( ) 1.2M  ( ) 700K  ( ) 360K │ │
*│ └─────────────────────────────────────────────────────────┘ │
*│              < OK Ctrl-W > < Отказаться Esc >               │
*└─────────────────────────────────────────────────────────────┘


PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 65, IIF(TYPE("ttl")="C", ttl, "Упаковка данных")
DO Sun_Bord WITH 2, 3, 4, 61
@ 2, 5 SAY " Максимальный размер архивного файла "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  5 GET max_sz PICTURE "@*RH Неограничен;1.4M;1.2M;700K;360K"
@ 5, 16 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF ex = 1
  PRIVATE a_dir, l_dir, i, n, w
  
  l_dir = ADIR(a_dir, pth+arch_n+".A*")   && Удаляем архивные файлы
  IF l_dir # 0                            &&         если они уже есть...
    FOR i = 1 TO l_dir
      w = RIGHT(a_dir(i,1), 2)
      IF w = "RJ"
        DELETE FILE (pth+a_dir(i,1))
      ENDIF
      IF RIGHT(w,1) $ "0123456789J" .AND. ;
         LEFT(w,1) $ "0123456789R"
        DELETE FILE (pth+a_dir(i,1))
      ENDIF
    ENDFOR
  ENDIF
  *
  * Упаковка...
  *
  PRIVATE l, c, ss, sr
  ACTIVATE SCREEN
  SAVE SCREEN TO ss
  cr = SET("CURSOR") = "ON"
  SET CURSOR OFF
  c = FLOOR((WCOLS("")-16)/2)
  l = CEILING((WROWS("")-3)/2)
  @ l+1, c+2 FILL TO l+3, c+17 COLOR (SCHEME(1,8))
  @ l,   c SAY "                " COLOR SCHEME 5
  @ l+1, c SAY "   Пакуем...    " COLOR SCHEME 5
  @ l+2, c SAY "                " COLOR SCHEME 5
  
  DO CASE
  CASE max_sz = 1
    w = ""
  CASE max_sz = 2
    w = " -V1400000"
  CASE max_sz = 3
    w = " -V1200000"
  CASE max_sz = 4
    w = " -V700000"
  CASE max_sz = 5
    w = " -V360000"
  ENDCASE
  w = "! ARJ A"+w+" -Y -E "+pth+arch_n+" "+pth+" > nul"
  &w
  RESTORE SCREEN FROM ss
  IF cr
    SET CURSOR ON
  ENDIF
ENDIF
*--------------------------------------------------------------------------

RETURN ex = 1
