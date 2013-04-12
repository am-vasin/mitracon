*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Cus_Type     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                          Категории клиентов.                           ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 07/05/2000 ══╝
PROCEDURE Cus_Type

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
PRIVATE s_sav         && Номер рабочей области для сохранения и восстановления!

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"CUS_TYPE") ALIAS C_T_0705 AGAIN ORDER TAG CODE ASCENDING

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "Имя категории" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Corr OF (menu_name) PROMPT "Предприятия" KEY Ctrl-A, "Tab"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "Типы клиентов", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Corr"

*
*    BROWSE - меню
*
    BROWSE KEY STR(user_dep,3)   ;
           FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Modify"    && Коррекция имени

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Modi_Item
    ENDIF
    what_do = "List"

  CASE what_do = "Add"        && Добавление типа

    DO Modi_Item WITH .T.
    what_do = "List"

  CASE what_do = "Corr"        && Предприятия

    DO Modi_List
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
USE
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_Item    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│          Создание новой категории или коррекция наименования.          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07/05/2000 ──┘
PROCEDURE Modi_Item
PARAMETERS new

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
*│  ┌─ Название ───────────────┐  │
*│  │ ....:....!....:....!.... │  │
*│  └──────────────────────────┘  │
*│< OK Ctrl-W > < Отказаться Esc >│
*└────────────────────────────────┘

PRIVATE ex, nm
ex = 1
IF new
  nm = SPACE(24)
ELSE
  nm = NAME
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 36, IIF(new, "Добавление ", "Коррекция ")+"типа"
DO Sun_Bord WITH  2,  4,  4, 31, " Название "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3, 6 GET nm
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Name()

IF ex = 1
  *
  * Отрабатываем бланк
  IF new
    SET ORDER TO TAG CODE DESCENDING
    IF SEEK(STR(user_dep,3))
      ex = CODE+1
    ELSE
      ex = 0
    ENDIF
    SET ORDER TO TAG CODE ASCENDING
    APPEND BLANK
    REPLACE CODE WITH ex, DEP WITH STR(user_dep,3)
  ENDIF
  REPLACE NAME WITH nm
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Name     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Проверка корректности имени.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07/05/2000 ──┘
PROCEDURE Tst_Name

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Название не может быть пустым!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_List    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                  Список предприятий для типа клиента.                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.07.2000 ──┘
PROCEDURE Modi_List

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
PRIVATE s_sav, p_list, n_list, i
s_sav = SELECT()

SELECT 0
USE (base_path+"CUS_LINK") ORDER TAG TYPE ALIAS LNK_0705 AGAIN

SELECT 0
USE (base_path+"FIRMS") ORDER TAG NAME ALIAS FRM_0705 AGAIN

n_list = 0
SCAN
  n_list = n_list+1
  DIMENSION p_list(n_list)
  p_list(n_list) = IIF(.NOT. SEEK(STR(user_dep,3)+STR(C_T_0705.CODE,3)+FIRM_CODE, "LNK_0705"), "√", " ")+ ;
                   FIRM_CODE+" "+B_NAME
ENDSCAN
USE
SELECT LNK_0705

*
*┌─────────────────────┐
*│√CCC NNNNNNNNNNNNNNNN│
*└─────────────────────┘
*  Enter  - Отметить
*  Ctrl_W - Сохранить
*  Esc    - Отказаться

PRIVATE ex, ln, i, sw
ex = 1
ln = MIN(n_list+6, WROWS("")-4)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH ln, 25, ALLTRIM(C_T_0705.NAME)
@ 1, 0 CLEAR TO WROWS()-1, WCOLS()-1
@ WROWS()-3, 3 SAY "Enter  - Отметить"
@ WROWS()-2, 3 SAY "Ctrl_W - Сохранить"
@ WROWS()-1, 3 SAY "Esc    - Отказаться"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
ON KEY LABEL INS KEYBOARD "{Tab}"
DO WHILE .T.
  ex = MAX(ex, 1)
  ex = MIN(ex, n_list)
  @ 1, 1 MENU p_list, n_list, ln-6
  READ MENU TO ex
  IF READKEY() % 256 = 12
    ex = 2
    EXIT
  ENDIF
  IF READKEY() % 256 = 14
    IF C_T_0705.CODE = 0
      sw = .T.
      FOR i = 1 TO n_list
        IF .NOT. EMPTY(LEFT(p_list(i),1))
          sw = .F.
          EXIT
        ENDIF
      ENDFOR
      IF sw
        PRIVATE mss
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Для этого типа должно быть доступно хотя бы одно предприятие!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        LOOP
      ENDIF
    ENDIF
    ex = 1
    EXIT
  ENDIF
  IF READKEY() % 256 = 15
    IF EMPTY(LEFT(p_list(ex),1))
      p_list(ex) = "√"+SUBSTR(p_list(ex),2)
    ELSE
      p_list(ex) = " "+SUBSTR(p_list(ex),2)
    ENDIF
  ENDIF
ENDDO
*--------------------------------------------------------------------------
IF ex = 1
  DO WHILE SEEK(STR(user_dep,3)+STR(C_T_0705.CODE,3))
    REPLACE DEP WITH "", TYPE WITH "", FIRM WITH ""
  ENDDO
  FOR i = 1 TO n_list
    IF .NOT. EMPTY(LEFT(p_list(i),1))
      LOOP
    ENDIF
    GO TOP
    IF .NOT. EMPTY(TYPE) .OR. BOF() .OR. EOF()
      APPEND BLANK
    ENDIF
    REPLACE DEP  WITH STR(user_dep, 3),     ;
            TYPE WITH STR(C_T_0705.CODE,3), ;
            FIRM WITH SUBSTR(p_list(i),2,3)
  ENDFOR
ENDIF

POP KEY
USE
SELECT (s_sav)
RELEASE WINDOW (win_name)
RETURN
