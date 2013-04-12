*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Tst_Tag1     Разработчик Андрей Васин           20.11.97 19:00:04 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Выбор файла для проверки индекса.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Tst_Tag1

PRIVATE s_path, f_name, t_list, t_numb, t_choose, i, ln

DO Set_Lwr

s_path = base_path
DO WHILE .T.

  f_name = Int_File(s_path, "DBF", "Выберите файл для проверки")
  IF EMPTY(f_name)
    EXIT
  ENDIF
  USE (f_name)
  t_numb = 0
  s_path = LEFT(f_name,RAT("\",f_name))
  DO WHILE .T.
    IF EMPTY(TAG(t_numb+1))
      EXIT
    ENDIF
    t_numb = t_numb+1
    DIMENSION t_list(t_numb)
    t_list(t_numb) = TAG(t_numb)
  ENDDO
  IF t_numb = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Выбранный Вами файл не имеет ни одного индекса!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    LOOP
  ENDIF
  ln = 0
  FOR i = 1 TO t_numb
    ln = MAX(ln, LEN(t_list(i)))
  ENDFOR
  ln = ln+2
  FOR i = 1 TO t_numb
    t_list(i) = PADR(t_list(i),ln)
  ENDFOR
  t_choose = 1
  DO WHILE .T.
    t_choose = Get_Tag(t_choose,t_list)
    IF EMPTY(t_choose)
      EXIT
    ENDIF
    DO Tst_Tag WITH f_name, ALLTRIM(t_list(t_choose))
  ENDDO
ENDDO

CLOSE DATABASES
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                             Выбор индекса.                             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.11.97 11:33:23 ═╝
PROCEDURE Get_Tag
PARAMETER i_choose, lst
EXTERNAL ARRAY lst
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
*│     ┌──────────────────────┐     │
*│     │                      │     │
*│     │                      │     │
*│     │                      │     │
*│     │                      │     │
*│     │                      │     │
*│     │                      │     │
*│     └──────────────────────┘     │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, ln, wd, c1, c2
ex = 1
wd = MAX(38, LEN(lst(1))+6)
c1 = FLOOR((wd-LEN(lst(1))-2)/2)
c2 = FLOOR((wd-32)/2)
ln = MIN(ALEN(lst), 15)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH ln+6, wd, "Индексы"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, c1 GET i_choose FROM lst SIZE ln+2, LEN(lst(1))+2
@ WROWS()-2, c2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

i_choose = IIF(ex = 1, i_choose, 0)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN i_choose
