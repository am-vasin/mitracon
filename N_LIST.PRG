*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл N_List       Разработчик Андрей Васин           11.09.98 13:20:10 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Коррекция расписания регламентов.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE N_List

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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Modify OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
DO Use_Dummy
SELECT 0
USE (base_path+"NIGHT")

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-10   && Количество видимых строк BROWSE
wd = FSIZE("NAME")+2

DO D_Wins WITH ln, wd, "Список регламентов", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:H="", F000 = IIF(STOP, "X", " "):H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Modify"    && Коррекция

    IF .NOT. EOF() .AND. .NOT. BOF()
      DO Corr0
    ENDIF
    what_do = "List"

  CASE what_do = "Add"       && Добавление

    DO Corr0 WITH .T.
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Коррекция пункта регламента.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 11.09.98 13:33:02 ═╝
PROCEDURE Corr0
PARAMETER new

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

*┌──────────────────────────────────────────────┐
*│ ┌─ Название пункта ────────────────────────┐ │
*│ │ ....:....!....:....!....:....!....:....! │ │
*│ └──────────────────────────────────────────┘ │
*│ ┌─ Тип расписания ─────────────────────────┐ │
*│ │    ( ) Числа месяца    ( ) Дни недели    │ │
*│ └──────────────────────────────────────────┘ │
*│   [ ] Коррекция расписания  [ ] Блокировка   │
*│ ┌─ Командная строка ───────────────────────┐ │
*│ │ ....:....!....:....!....:....!....:....! │ │
*│ └──────────────────────────────────────────┘ │
*│       < OK Ctrl-W > < Отказаться Esc >       │
*└──────────────────────────────────────────────┘

PRIVATE ex, p_name, p_type, p_corr, p_stop, p_comm, p_list
ex = 1
IF new
  p_name = SPACE(40)
  p_type = 1
  p_corr = .F.
  p_stop = .F.
  p_comm = SPACE(40)
  p_list = SPACE(31)
ELSE
  p_name = NAME
  p_type = TYPE_LIST
  p_corr = .F.
  p_stop = STOP
  p_comm = COMM_STR
  p_list = LOG_LIST
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 14, 50, "Пункт регламента"

DO Sun_Bord WITH 2, 3,  4, 46, " Название пункта "
DO Sun_Bord WITH 5, 3,  7, 46, " Тип расписания "
DO Sun_Bord WITH 9, 3, 11, 46, " Командная строка "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  5 GET p_name
@  6,  8 GET p_type PICTURE "@*RH Числа месяца   ;Дни недели"
@  8,  5 GET p_corr PICTURE "@*C Коррекция расписания" VALID Corr_L()
@  8, 31 GET p_stop PICTURE "@*C Блокировка"
@ 10,  5 GET p_comm PICTURE "@S40"
@ 12,  9 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  IF new
    APPEND BLANK
  ENDIF
  REPLACE NAME      WITH p_name, ;
          TYPE_LIST WITH p_type, ;
          STOP      WITH p_stop, ;
          COMM_STR  WITH p_comm, ;
          LOG_LIST  WITH p_list
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                Коррекция расписания по числам месяца.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 11.09.98 13:33:02 ═╝
PROCEDURE Corr1

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
*│   [ ]  1     [ ] 11     [ ] 21   │
*│   [ ]  2     [ ] 12     [ ] 22   │
*│   [ ]  3     [ ] 13     [ ] 23   │
*│   [ ]  4     [ ] 14     [ ] 24   │
*│   [ ]  5     [ ] 15     [ ] 25   │
*│   [ ]  6     [ ] 16     [ ] 26   │
*│   [ ]  7     [ ] 17     [ ] 27   │
*│   [ ]  8     [ ] 18     [ ] 28   │
*│   [ ]  9     [ ] 19     [ ] 29   │
*│   [ ] 10     [ ] 20     [ ] 30   │
*│                         [ ] 31   │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, wk_list
ex = 1
DIMENSION wk_list(31)
FOR i = 1 TO 31
  wk_list(i) = .NOT. EMPTY(SUBSTR(p_list, i, 1))
ENDFOR

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 16, 38, "Расписание по числам месяца"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
FOR i = 1 TO 31
  DO CASE
  CASE i < 11
    @ 1+i, 5 GET wk_list(i) PICTURE "@*C "+STR(i,2)
  CASE i < 21
    @ i-9, 16 GET wk_list(i) PICTURE "@*C "+STR(i,2)
  OTHERWISE
    @ i-19, 27 GET wk_list(i) PICTURE "@*C "+STR(i,2)
  ENDCASE
ENDFOR
@ 14, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

p_list = ""
IF ex = 1
  FOR i = 1 TO 31
    p_list = p_list+IIF(wk_list(i), "X", " ")
  ENDFOR
ENDIF
p_list = PADR(p_list,31)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                Коррекция расписания по дням недели.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 11.09.98 13:33:02 ═╝
PROCEDURE Corr2

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
*│          [ ] Понедельник         │
*│          [ ] Вторник 2           │
*│          [ ] Среда 3             │
*│          [ ] Четверг 4           │
*│          [ ] Пятница 5           │
*│          [ ] Суббота 6           │
*│          [ ] Воскресенье         │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, wk_list
ex = 1
DIMENSION wk_list(7)
wk_list(1) = SUBSTR(p_list,2,1) # " "
wk_list(2) = SUBSTR(p_list,3,1) # " "
wk_list(3) = SUBSTR(p_list,4,1) # " "
wk_list(4) = SUBSTR(p_list,5,1) # " "
wk_list(5) = SUBSTR(p_list,6,1) # " "
wk_list(6) = SUBSTR(p_list,7,1) # " "
wk_list(7) = SUBSTR(p_list,1,1) # " "

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 12, 38, "Расписание по дням недели"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 12 GET wk_list(1) PICTURE "@*C Понедельник"
@ 3, 12 GET wk_list(2) PICTURE "@*C Вторник"
@ 4, 12 GET wk_list(3) PICTURE "@*C Среда"
@ 5, 12 GET wk_list(4) PICTURE "@*C Четверг"
@ 6, 12 GET wk_list(5) PICTURE "@*C Пятница"
@ 7, 12 GET wk_list(6) PICTURE "@*C Суббота"
@ 8, 12 GET wk_list(7) PICTURE "@*C Воскресенье"
@ 10, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

p_list = ""
IF ex = 1
  p_list = p_list+IIF(wk_list(7), "X", " ")
  p_list = p_list+IIF(wk_list(1), "X", " ")
  p_list = p_list+IIF(wk_list(2), "X", " ")
  p_list = p_list+IIF(wk_list(3), "X", " ")
  p_list = p_list+IIF(wk_list(4), "X", " ")
  p_list = p_list+IIF(wk_list(5), "X", " ")
  p_list = p_list+IIF(wk_list(6), "X", " ")
ENDIF
p_list = PADR(p_list,31)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Коррекция расписания.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 11.09.98 16:05:27 ═╝
PROCEDURE Corr_L

IF .NOT. p_corr
  RETURN .T.
ENDIF

IF p_type = 1
  DO Corr1
ELSE
  DO Corr2
ENDIF

p_corr = .F.
SHOW GET p_corr

RETURN .T.
