*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Set_F_B      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                Установка площадки и банка по умолчанию.                ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 15.01.1999 ══╝
PROCEDURE Set_F_B

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
PRIVATE flt, frm_n, bnk_n, p_list, shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
p_list = ""
STORE "" TO flt
STORE SPACE(16) TO frm_n, bnk_n
s_sav = SELECT()
SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"PLACES") ALIAS B_9115 AGAIN
SCAN
  p_list = p_list+p_char
ENDSCAN
USE (base_path+"BANKS") ORDER TAG BANK ALIAS B_9115 AGAIN
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9115 AGAIN
SELECT 0
USE (base_path+"PARMS") ALIAS P_9115 AGAIN

IF .NOT. EMPTY(SYS_FIRM)
  IF SEEK(SYS_FIRM, "F_9115")
    frm_n = F_9115.B_NAME
    IF .NOT. EMPTY(SYS_BANK)
      IF SEEK(SYS_FIRM+SYS_BANK, "B_9115")
        bnk_n = B_9115.B_NAME
      ELSE
        REPLACE SYS_BANK WITH "   "
      ENDIF
    ENDIF
  ELSE
    REPLACE SYS_FIRM WITH "   "
  ENDIF
ENDIF

SELECT 0
USE (base_path+"ARM_LST") ALIAS A_9115 AGAIN
SET RELATION TO ARM_FIRM INTO F_9115
SET RELATION TO ARM_FIRM+ARM_BANK INTO B_9115 ADDITIVE

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Square OF (menu_name) PROMPT "Площадка" KEY Ctrl-A, "Enter"
ON SELECTION PAD Square OF (menu_name) DO Sw_Mode WITH "Square"

DEFINE PAD Firm OF (menu_name) PROMPT "Предпр." KEY Ctrl-A, "Tab"
ON SELECTION PAD Firm OF (menu_name) DO Sw_Mode WITH "Firm"

DEFINE PAD Filter OF (menu_name) PROMPT "Фильтр" KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Main OF (menu_name) PROMPT "Осн. касса" KEY Ctrl-A, "F1"
ON SELECTION PAD Firm OF (menu_name) DO Sw_Mode WITH "Main"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
*wd = 60
wd = 58

DO D_Wins WITH ln, wd, "Список рабочих мест", 0, 1
*┌Раб. место───────Пл.─Предприятие──────Р──Банк────────────Р─О┐
*│9999999999999999 999 9999999999999999 9 9999999999999999 9 9│
*└────────────────────────────────────────────────────────────┘
*┌ Общие ───────────────────────────────────────────────────┐
*│  Предприятие 9999999999999999     Банк 9999999999999999  │
*└──────────────────────────────────────────────────────────┘
*@ 1, 2 SAY "Раб. место───────Пл.─Предприятие──────Р──Банк────────────Р─О"
@ 1, 2 SAY "Раб. место───────Пл.─Предприятие──────Р──Банк────────────Р"
@ WROWS()-3, 2 SAY " Общие "
@ WROWS()-2, 4 SAY "Предприятие "+frm_n+"     Банк "+bnk_n

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F6    DO Sw_Mode WITH "Filter"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Firm"
    ON KEY LABEL F1    DO Sw_Mode WITH "Main"

*
*    BROWSE - меню
*
    BROWSE FIELDS ARM_N:16:H="",  ;
                  F000 = PADC(ARM_S,3):3:H="", ;
                  F_9115.B_NAME:16:H="", ;
                  F001 = IIF(LET_FIRM, "√", " "):1:H="", ;
                  B_9115.B_NAME:16:H="", ;
                  F002 = IIF(LET_BANK, "√", " "):1:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

*                  F003 = IIF(MAIN_KASSA, "√", " "):1:H="", ;
*

    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Square")
    ENDIF

  CASE what_do = "Square"    && Умолчания для рабочего места

    DO Set_Def WITH .T.
    what_do = "List"

  CASE what_do = "Firm"      && Умолчания для предприятия

    DO Set_Def WITH .F.
    ACTIVATE WINDOW (win_name+"_M")
    @ WROWS()-2, 4 SAY "Предприятие "+frm_n+"     Банк "+bnk_n
    what_do = "List"

  CASE what_do = "Main"      && Осн. касса

    IF .NOT. EOF() .AND. .NOT. BOF()
      REPLACE MAIN_KASSA WITH .NOT. MAIN_KASSA
    ENDIF
    what_do = "List"

  CASE what_do = "Filter"    && Установка/очистка фильтра

    DO S_Filter
    IF EMPTY(flt)
      DEFINE PAD Filter OF (menu_name) PROMPT "Фильтр" KEY Ctrl-A, "F6"
    ELSE
      DEFINE PAD Filter OF (menu_name) PROMPT "Фильтр "+flt KEY Ctrl-A, "F6"
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
SELECT B_9115
USE
SELECT F_9115
USE
SELECT P_9115
USE
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура S_Filter     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Установка/очистка фильтра.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 15.01.1999 ──┘
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

*┌────────────────────────────────────────────────────────┐
*│ ┌─ Площадки ───────────────────┐ < Установить Ctrl-W > │
*│ │ ( ) ....:....!....:....!.... │ < Очистить          > │
*│ └──────────────────────────────┘ < Отказаться Esc    > │
*└────────────────────────────────────────────────────────┘

PRIVATE ex, point
ex = 1
point = IIF(EMPTY(flt), 0, AT(flt, p_list))
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH LEN(p_list)+5, 60, "Фильтр"
DO Sun_Bord WITH  2,  3, LEN(p_list)+3, 34, " Площадки "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO G_Place WITH 3, 5, point
@ 2, 36 GET ex PICTURE   ;
"@*T \! Установить Ctrl-W ;\ Очистить          ;\? Отказаться Esc    "

READ CYCLE

DO CASE
CASE ex = 2 .OR. point = 0
  flt = ""
  SET FILTER TO
CASE ex = 1
  flt = SUBSTR(p_list,point,1)
  SET FILTER TO ARM_S = flt
ENDCASE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_Def      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Установка умолчаний.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 15.01.1999 ──┘
PROCEDURE Set_Def
PARAMETER is_arm
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

*┌──────────────────────────────────────────────────────────┐
*│ Предприятие 999 9999999999999999 (F1) [ ] Выбор разрешен │
*│                                                          │
*│      [ ] Банк 9999999999999999  [ ] Выбор разрешен       │
*│                                                          │
*│             < OK Ctrl-W > < Отказаться Esc >             │
*└──────────────────────────────────────────────────────────┘

PRIVATE ex, f_c000, f_n000, b_c000, b_n000, b_sw000, sw_f, sw_b, f_c_old
ex = 1
f_c_old = ""
IF is_arm
  f_c000  = A_9115.ARM_FIRM
  f_n000  = PADL(F_9115.B_NAME,16)
  b_c000  = A_9115.ARM_BANK
  b_n000  = PADL(B_9115.B_NAME,16)
  b_sw000 = .F.
  sw_f    = A_9115.LET_FIRM
  sw_b    = A_9115.LET_BANK
ELSE
  f_c000  = P_9115.SYS_FIRM
  f_n000  = PADL(frm_n,16)
  b_c000  = P_9115.SYS_BANK
  b_n000  = PADL(bnk_n,16)
  b_sw000 = .F.
  sw_f    = .F.
  sw_b    = .F.
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 62, "Умолчания для "+IIF(is_arm, ;
       'рабочего места "'+ALLTRIM(ARM_N)+'"',  ;
       "предприятия")

*------------------------------------------------------------------------
*      Ввод полей бланка
*

DO Get_Firm WITH 2, 3, .T.
IF is_arm
  @ 2, 41 GET sw_f PICTURE "@*C Выбор разрешен"
ELSE
  @ 2, 41 GET sw_f PICTURE "@*C Выбор разрешен" DISABLE
ENDIF
DO Get_Bank WITH 4, 8, .T.
IF is_arm
  @ 4, 35 GET sw_b PICTURE "@*C Выбор разрешен"
ELSE
  @ 4, 35 GET sw_b PICTURE "@*C Выбор разрешен" DISABLE
ENDIF
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  IF is_arm
    REPLACE A_9115.ARM_FIRM WITH f_c000, ;
            A_9115.ARM_BANK WITH b_c000, ;
            A_9115.LET_FIRM WITH sw_f,   ;
            A_9115.LET_BANK WITH sw_b
    GO RECNO()
  ELSE
    REPLACE P_9115.SYS_FIRM WITH f_c000, ;
            P_9115.SYS_BANK WITH b_c000
    frm_n = f_n000
    bnk_n = b_n000
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
