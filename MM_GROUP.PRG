*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл MM_Group     Разработчик Андрей Васин           07.09.98 12:27:42 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Группы кассовых операций.                        ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE MM_Group

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
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

*
*   Список операций
USE (base_path+"MM_TYPES") ORDER TAG NAME
PRIVATE oper_n, oper_c, oper_cnt
oper_cnt = 0
SCAN
  oper_cnt = oper_cnt+1
  DIMENSION oper_n(oper_cnt), oper_c(oper_cnt)
  oper_n(oper_cnt) = " "+NAME
  oper_c(oper_cnt) = CODE
ENDSCAN
USE
IF oper_cnt = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не определили ни одной кассовой операции!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
DO Use_Dummy
SELECT 0
USE (base_path+"MM_GROUP")

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Delete OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = FLOOR(WROWS("")*0.7)   && Количество видимых строк BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "Группы операций", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL dEL   DO Sw_Mode WITH "Delete"

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Modify"    && Коррекция

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Corr_Gr
    ENDIF
    what_do = "List"

  CASE what_do = "Add"       && Добавление

    DO Corr_Gr WITH .T.
    what_do = "List"

  CASE what_do = "Delete"   && Удаление

    IF .NOT. BOF() .AND. .NOT. EOF()
      DELETE
    ENDIF
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
*║                 Коррекция, добавдение группы операций.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.09.98 12:52:54 ═╝
PROCEDURE Corr_Gr
PARAMETER add

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
*│  ┌─ Название группы ────────┐  │
*│  │ ....:....!....:....!.... │  │
*│  └──────────────────────────┘  │
*│      [ ] Список операций       │
*│                                │
*│< OK Ctrl-W > < Отказаться Esc >│
*└────────────────────────────────┘

PRIVATE ex, nm_w, o_list, s_list
ex   = 1
o_list = .F.
IF add
  nm_w = SPACE(24)
  s_list = ""
ELSE
  nm_w = NAME
  s_list = OPER_LST
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 36, IIF(add,"Добавление ", "Коррекция ")+"группы"
DO Sun_Bord WITH 2, 4, 4, 31, " Название группы "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3, 6 GET nm_w
@ 5, 8 GET o_list PICTURE "@*C Список операций" VALID C_List()
@ 7, 2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  IF add
    APPEND BLANK
  ENDIF
  REPLACE NAME     WITH nm_w, ;
          OPER_LST WITH s_list
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Коррекция списка операций.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.09.98 14:46:07 ═╝
PROCEDURE C_List
PRIVATE i
IF .NOT. o_list
  RETURN .T.
ENDIF

FOR i = 1 TO oper_cnt
  IF STR(oper_c(i),3)+"!" $ s_list
    oper_n(i) = "√"+SUBSTR(oper_n(i),2)
  ELSE
    oper_n(i) = " "+SUBSTR(oper_n(i),2)
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

*  Операции
*┌─────────────────┐
*│                 │
*│                 │
*│                 │
*│                 │
*│                 │
*│                 │
*│                 │
*└─────────────────┘
* Enter - отметить
*   Esc - выход

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 12, 21, "Операции"
@ 1, 1 CLEAR TO WROWS()-1, WCOLS()-1
@ WROWS()-2, 0 SAY PADC("Enter - отметить", WCOLS())
@ WROWS()-1, 0 SAY PADC("Esc - выход", WCOLS())

*------------------------------------------------------------------------
*      Ввод полей бланка
*
i = 1
DO WHILE .T.
  i = MAX(i,1)
  i = MIN(i, oper_cnt)
  @ 1, 1 MENU oper_n, oper_cnt, 7
  READ MENU TO i
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
  IF LEFT(oper_n(i), 1) = " "
    oper_n(i) = "√"+SUBSTR(oper_n(i),2)
  ELSE
    oper_n(i) = " "+SUBSTR(oper_n(i),2)
  ENDIF
ENDDO
s_list = ""
FOR i = 1 TO oper_cnt
  IF EMPTY(LEFT(oper_n(i),1))
    LOOP
  ENDIF
  s_list = s_list+STR(oper_c(i),3)+"!"
ENDFOR
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
o_list = .F.
SHOW GET o_list
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Проверка бланка.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.09.98 16:32:40 ═╝
PROCEDURE Tst_Blank

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Имя не может быть пустым!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
