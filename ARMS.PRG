*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Arms         Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                         Описание рабочих мест.                         ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 03.09.1999 ══╝
PROCEDURE Arms

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
USE (base_path+"PLACES")
PRIVATE p_names, p_codes, p_count, f_char, f_name
f_char = ""
f_name = "Все площадки            "
p_count = 0
SCAN
  p_count = p_count+1
  DIMENSION p_names(p_count), p_codes(p_count)
  p_names(p_count) = P_NAME
  p_codes(p_count) = P_CHAR
ENDSCAN
USE (base_path+"PERSONS") ORDER TAG CODE
SELECT 0
USE (base_path+"ARM_INFO") ORDER TAG ARM_C
SET RELATION TO WHO INTO PERSONS
SELECT 0
USE (base_path+"ARM_LST") ORDER TAG NAME
SET RELATION TO ARM_C INTO ARM_INFO

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "Коррекция" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-14   && Количество видимых строк BROWSE
wd = 47
*┌Раб. место───────Пользователь──────────────────┐
*│....:....!....:. ....:....!....:....!....:....!│
*└───────────────────────────────────────────────┘
*┌─Информация────────────────────────────────────┐
*│  Где нах. ....:....!....:....!                │
*│  Телефон  ....:....!....:.                    │
*│  Сет. имя ....:....!....:....!                │
*└───────────────────────────────────────────────┘
DO D_Wins WITH ln, wd, "", 0, 3
@ 1, 2 SAY "Раб. место───────Пользователь"
@ WROWS()-5, 3 SAY "Информация"
@ WROWS()-4, 4 SAY "Где нах."
@ WROWS()-3, 4 SAY "Телефон"
@ WROWS()-2, 4 SAY "Сет. имя"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"

*
*    BROWSE - меню
*
    BROWSE FIELDS ARM_N:H="", F000 = P_F():H="":30,  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Modify"    && Коррекция

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Arm_Info WITH ARM_C
    ENDIF
    what_do = "List"

  CASE what_do = "S_Filter"  && Установка фильтра

    DO S_Filter
    what_do = "List"

  CASE what_do = "C_Filter"  && Очистка фильтра

    f_char = ""
    f_name = "Все площадки            "
    SET FILTER TO
    what_do = "List"

  CASE what_do = "Add"       && Добавление рабочего места

      DO Arm_Info WITH "   "
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
CLOSE DATABASES
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-4, 13 CLEAR TO WROWS()-2, WCOLS()-3
IF FOUND("ARM_INFO")
  @ WROWS()-4, 13 SAY ARM_INFO.LOCATE
  @ WROWS()-3, 13 SAY ARM_INFO.PHONE
  @ WROWS()-2, 13 SAY ARM_INFO.NET_NAME
ENDIF

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура P_F          Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Площадка для рабочаго места.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.09.1999 ──┘
PROCEDURE P_F
PRIVATE i

RETURN ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура S_Filter     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Установка фильтра.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.09.1999 ──┘
PROCEDURE S_Filter

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

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH MIN(WROWS("")-10, p_count+3), 28, "Площадки"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
IF .NOT. EMPTY(f_char)
  ex = ASCAN(p_codes, f_char)
ENDIF
DO WHILE .T.
  ex = MIN(p_count, ex)
  ex = MAX(ex, 1)
  @ 1, 1 MENU p_names, p_count, WROWS()-3
  READ MENU TO ex
  IF READKEY() % 256 = 15
    f_char = p_codes(ex)
    f_name = p_names(ex)
    SET FILTER TO ARM_S = f_char
    EXIT
  ENDIF
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
ENDDO    
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Add_P        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Добавление рабочего места.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.09.1999 ──┘
PROCEDURE Add_P
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

*┌─────────────────────────────────────┐
*│      Название ....:....!....:.      │
*│          ┌────────────────────────┐ │
*│ Площадка │....:....!....:....!....│ │
*│          └────────────────────────┘ │
*│  < OK Ctrl-W > < Отказаться Esc >   │
*└─────────────────────────────────────┘

PRIVATE ex, p, nm
ex = 1
nm = SPACE(16)
p = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 41, "Создание рабочего места"
*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 4,  3 SAY "Площадка"
@ 2,  8 SAY "Название" GET nm
@ 3, 12 GET p PICTURE "@^" FROM p_names SIZE 3, 26
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Blank    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Выход из бланка.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.09.1999 ──┘
PROCEDURE Tst_Blank

PRIVATE r_sav, to_add

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Название площадки не может быть пустым!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 1
ENDIF

PRIVATE c_s
to_add = .T.
r_sav = RECNO()
SET ORDER TO ARM_C
IF .NOT. EMPTY(f_char)
  SET FILTER TO
ENDIF
GO BOTT
IF BOF() .OR. EOF()
  c_s = "  1"
ELSE
  c_s = RIGHT(STR(VAL(ARM_C)+1),3)
ENDIF
IF to_add
  APPEND BLANK
  REPLACE ARM_C    WITH c_s,        ;
          ARM_CODE WITH VAL(ALLTRIM(c_s)),   ;
          ARM_S    WITH p_codes(p), ;
          ARM_N    WITH nm
ENDIF

IF .NOT. EMPTY(f_char)
  IF f_char # ARM_S
    f_char = ""
    f_name = "Все площадки            "
  ELSE
    SET FILTER TO ARM_S = f_char
  ENDIF
ENDIF
SET ORDER TO TAG NAME

RETURN .T.
