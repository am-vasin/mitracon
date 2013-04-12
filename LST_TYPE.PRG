*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Lst_Type     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Стандартные типы списков.                        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 07.09.1999 ══╝
PROCEDURE Lst_Type

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
IF .NOT. File_O(prmo_path+"LST_TYPE.DBF")
  CREATE DBF (prmo_path+"LST_TYPE.DBF") ;
    ( FLAGS    C(16), ;
      CUS_CODE N( 8), ;
      LST_NAME C(16), ;
      IS_USD   L,     ;
      IS_CHECK L,     ;
      FIRM     C( 3), ;
      BANK     C( 3), ;
      NAME     C(24) )
ELSE
  USE (prmo_path+"LST_TYPE.DBF") EXCLUSIVE
ENDIF

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
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-7   && Количество видимых строк BROWSE
wd = 24

DO D_Wins WITH ln, wd, "Стандартные типы списков", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Delete"

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Modify"    && Корректировать

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Mody_St
    ENDIF
    what_do = "List"

  CASE what_do = "Add"       && Добавить

    DO Mody_St WITH .T.
    what_do = "List"

  CASE what_do = "Delete"    && Удалить
    IF .NOT. BOF() .AND. .NOT. EOF()
      DELETE
      PACK
    ENDIF
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
*│   Процедура Mody_St      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                 Коррекция/добавление описания списка.                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.09.1999 ──┘
PROCEDURE Mody_St
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
PRIVATE f_c000, f_n000, b_c000, b_n000, b_sw000, f_c_old
PRIVATE shab1, shab2, s_sav
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
PRIVATE c_code, c_sw, c_name, is_acc, nm, l_nm, fl, fl_sw, usd, chc

s_sav = SELECT()
IF new
  f_c000 = def_firm
  b_c000 = def_bank
  f_c_old = def_firm
  c_code  = 0
  c_sw    = .F.
  usd     = .F.
  chc     = .F.
  c_name  = SPACE(50)
  nm      = SPACE(24)
  l_nm    = SPACE(16)
  fl      = SPACE(16)
ELSE
  f_c000 = FIRM
  b_c000 = BANK
  f_c_old = FIRM
  c_code  = CUS_CODE
  c_sw    = c_code # 0
  c_name  = SPACE(50)
  nm      = NAME
  usd     = IS_USD
  chc     = IS_CHECK
  l_nm    = LST_NAME
  fl      = FLAGS
ENDIF
fl_sw = .F.

is_acc  = USED("ACCOUNT")
IF .NOT. is_acc
  SELECT 0
  USE (base_path+"ACCOUNT") AGAIN
ENDIF
SELECT ACCOUNT
IF c_code # 0
  SET ORDER TO TAG CUS_CODE
  SEEK c_code
  c_name = CUS_NAME
ELSE
  SET ORDER TO TAG CUS_NAME
  GO TOP
ENDIF
SELECT 0
DO Use_Dummy
SELECT 0
DO Use_Link

f_n000 = SPACE(16)
b_n000 = SPACE(16)
b_sw000= .F.

IF .NOT. EMPTY(f_c000)
  SELECT 0
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS TMP_9876 AGAIN
  IF SEEK(f_c000)
    f_n000 = B_NAME
    IF .NOT. EMPTY(b_c000)
      USE (base_path+"BANKS") ORDER TAG BANK ALIAS TMP_9876 AGAIN
      IF SEEK(f_c000+b_c000)
        b_n000 = B_NAME
      ELSE
        b_c000 = "   "
      ENDIF
    ENDIF
  ELSE
    f_c000 = "   "
    b_c000 = "   "
  ENDIF
  USE
  SELECT (s_sav)
ELSE
  f_c000 = "   "
  b_c000 = "   "
ENDIF

*┌──────────────────────────────────────────────────────┐
*│      Название заготовки ....:....!....:....!....     │
*│ ┌─ [ ] Постоянный клиент ──────────────────────────┐ │
*│ │....:....!....:....!....:....!....:....!....:....!│ │
*│ └──────────────────────────────────────────────────┘ │
*│           Название списка ....:....!....:.           │
*│                      [ ] Флажки                      │
*│         Преприятие 999 9999999999999999 (F1)         │
*│              [ ] Банк 9999999999999999               │
*│               [ ] Цены в USD   [ ] Чек               │
*│           < OK Ctrl-W > < Отказаться Esc >           │
*└──────────────────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 13, 58, "Описание заготовки "+IIF(new, "(добавление)", "(коррекция)")
DO Sun_Bord WITH  3,  3,  5, 54, "                       "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2,  8 SAY "Название заготовки" GET nm
@  3,  6 GET c_sw PICTURE "@*C Постоянный клиент" VALID G_Cust()
@  4,  4 GET c_name WHEN .F.
@  6, 13 SAY "Название списка" GET l_nm
@  7, 24 GET fl_sw PICTURE "@*C Флажки" VALID S_Flags()
DO Get_Firm WITH 8, 11, enab_firm
DO Get_Bank WITH 9, 16, enab_bank
@ 10, 17+IIF(kkm_exist, 0, 5) GET usd PICTURE "@*C Цены в USD"
IF kkm_exist
  @ 10, 34 GET chc PICTURE "@*C Чек"
ENDIF
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  SELECT (s_sav)
  IF new
    APPEND BLANK
  ENDIF
  REPLACE FLAGS    WITH fl,     ;
          CUS_CODE WITH c_code, ;
          LST_NAME WITH l_nm,   ;
          FIRM     WITH f_c000, ;
          BANK     WITH b_c000, ;
          IS_USD   WITH usd,    ;
          IS_CHECK WITH chc,    ;
          NAME     WITH nm
ENDIF
*--------------------------------------------------------------------------

POP KEY
IF .NOT. is_acc
  SELECT ACCOUNT
  USE
ENDIF
SELECT (s_sav)

RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Cust       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Выбор клиента.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.09.1999 ──┘
PROCEDURE G_Cust

IF .NOT. c_sw
  c_code = 0
  c_name = SPACE(50)
  SHOW GET c_name
ELSE
  SELECT ACCOUNT
  SET ORDER TO TAG CUS_NAME
  GO TOP
  IF Custs()
    c_code = CUS_CODE
    c_name = CUS_NAME
  ELSE
    c_sw = .F.
    c_code = 0
    c_name = SPACE(50)
  ENDIF
  SHOW GET c_sw
  SHOW GET c_name
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла S_Flags      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║       Процедура очистки/установки флажков для списка (продажи).        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 27.03.1999 ══╝
PROCEDURE S_Flags

IF .NOT. fl_sw
  RETURN .T.
ENDIF
fl_sw = .F.
SHOW GET fl_sw
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
  RETURN .T.
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
  RETURN .T.
ENDIF

FOR i = 1 TO 16
  vl(i) = .NOT. EMPTY(SUBSTR(fl,i,1))
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
  fl = ""
  FOR i = 1 TO 16
    fl = fl +IIF(vl(i), "*", " ")
  ENDFOR
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN .T.
