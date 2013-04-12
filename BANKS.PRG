*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Banks        Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Коррекция списка банков.                         ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 01/12/1999 ══╝
PROCEDURE Banks

*
*  Переменные состояния для навигации
*
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"

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
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS FR_9113 AGAIN
SELECT 0
USE (base_path+"BANKS") ORDER TAG ALL_NAMES ALIAS B_9113 AGAIN
SET RELATION TO FIRM_CODE INTO FR_9113

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "Коррекция" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "Добавл." KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "Удал./Восст." KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
*┌Полное название────────────────────────────────────Удален────┐
*│99999999999999999999999999999999999999999999999999 99.99.9999│
*└─────────────────────────────────────────────────────────────┘
*┌─────────────────────────────────────────────────────────────┐
*│Короткое название 999999999999999  Фирма 999 9999999999999999│
*│Расч.счет 99999999999999999999 Корр.счет 99999999999999999999│
*│                        БИК 999999999                        │
*└─────────────────────────────────────────────────────────────┘
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY") = "ON", 61, 59)

DO D_Wins WITH ln, wd, "Список банков", 0, 3
@ 1, 2 SAY "Полное название────────────────────────────────────Удален"
@ WROWS()-4,  2 SAY "Короткое название                  Фирма"
@ WROWS()-3,  2 SAY "Расч.счет                      Корр.счет"
@ WROWS()-2, 26 SAY "БИК"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Del   DO Del_Bank
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"

*
*    BROWSE - меню
*
    BROWSE FIELDS L_NAME:50:H="",    ;
                  DATE_OFF:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && Коррекция банка

    IF BETWEEN(RECNO(), 1, RECCOUNT())
      DO Modi_Line
    ENDIF
    what_do = "List"

  CASE what_do = "Del"     && Удаление/восстановление

    DO Del_Bank
    what_do = "List"

  CASE what_do = "Add"     && Добавление фирмы

    DO Modi_Line WITH .T.
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
SELECT FR_9113
USE
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
PRIVATE cl
cl = SCHEME(13,6)
ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-4, 20 SAY B_NAME COLOR (cl)
@ WROWS()-4, 43 SAY FR_9113.FIRM_CODE COLOR (cl)
@ WROWS()-4, 47 SAY FR_9113.B_NAME COLOR (cl)
@ WROWS()-3, 12 SAY ACC_NO COLOR (cl)
@ WROWS()-3, 43 SAY CORR_NO COLOR (cl)
@ WROWS()-2, 30 SAY BIK COLOR (cl)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_Line    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Добавление/коррекция предприятия.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 01/12/1999 ──┘
PROCEDURE Modi_Line
PARAMETERS new

*
*  Переменные состояния для навигации
*
IF .NOT. EMPTY(DATE_OFF) .AND. .NOT. new
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Фирма удалена, какой смысл ее корректировать?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

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

*┌────────────────────────────────────────────────────────────────────────┐
*│┌─ Банк ───────────────────────────────────────────────────────────────┐│
*││9999999999999999999999999999999999999999999999999999999999999999999999││
*││                                                                      ││
*││  Короткое название 9999999999999999  Фирма 999 9999999999999999(F1)  ││
*│└──────────────────────────────────────────────────────────────────────┘│
*│ Расчетный счет 99999999999999999999    Корр. счет 99999999999999999999 │
*│                                                                        │
*│                             БИК 999999999                              │
*│                                                                        │
*│                    < OK Ctrl-W > < Отказаться Esc >                    │
*└────────────────────────────────────────────────────────────────────────┘

PRIVATE ex, cd, n0, n1, f_cd, f_n, c1, c2, b0
IF new
  cd  = "   "
  n0  = SPACE(16)
  n1  = SPACE(70)
  f_cd= "   "
  f_n = SPACE(16)
  c1  = SPACE(16)
  c2  = SPACE(16)
  b0  = SPACE(9)
ELSE
  cd  = BANK_CODE
  n0  = B_NAME
  n1  = L_NAME
  f_cd= FIRM_CODE
  f_n = FR_9113.B_NAME
  c1  = ACC_NO
  c2  = CORR_NO
  b0  = BIK
ENDIF

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 13, 76, IIF(new, "Добавление ", "Коррекция ")+"банка"
DO Sun_Bord WITH  2,  2,  6, 73, " Банк "
@ 5, 67 SAY "(F1)"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  3 GET n1
@  5,  5 SAY "Короткое название" GET n0
@  5, 41 SAY "Фирма" GET f_cd WHEN new .AND. Set_F1() VALID Tst_Firm()
@  5, 51 GET f_n WHEN .F.
@  7,  3 SAY "Расчетный счет" GET c1
@  7, 42 SAY "Корр. счет" GET c2
@  9, 31 SAY "БИК" GET b0
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  *
  * Отрабатываем бланк
  IF new
    SET ORDER TO TAG BANK DESCENDING
    IF .NOT. SEEK(f_cd)
      cd = "001"
    ELSE
      cd = RIGHT(STR(1000+VAL(BANK_CODE)+1),3)
    ENDIF
    APPEND BLANK
    SET ORDER TO TAG ALL_NAMES
  ENDIF
  REPLACE BANK_CODE WITH cd,  ;
          B_NAME    WITH n0,  ;
          L_NAME    WITH n1,  ;
          FIRM_CODE WITH f_cd,;
          ACC_NO    WITH c1,  ;
          CORR_NO   WITH c2,  ;
          BIK       WITH b0
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Blank    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Проверка корректности заполнения бланка.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 01/12/1999 ──┘
PROCEDURE Tst_Blank

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(n1)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Полное имя не может быть пустым!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD REPLICATE("{Tab}",2)
  RETURN .F.
ENDIF

IF EMPTY(n0)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Короткое имя не может быть пустым!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD REPLICATE("{Tab}",3)
  RETURN .F.
ENDIF

IF EMPTY(f_cd)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вам совершенно необходимо указать фирму!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD REPLICATE("{Tab}",4)
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Del_Bank     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Удаление/восстановление банка.                     │
*│                                                                        │
*└────────────────────────────────────────────────────────── 01/12/1999 ──┘
PROCEDURE Del_Bank

IF BETWEEN(RECNO(), 1, RECCOUNT())
  IF EMPTY(DATE_OFF)
    REPLACE DATE_OFF WITH DATE()
  ELSE
    REPLACE DATE_OFF WITH {}
  ENDIF
ENDIF
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_F1       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Установка обработчика F1.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.01.1999 ──┘
PROCEDURE Set_F1

ON KEY LABEL F1 DO What_F

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Firm     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Выход из поля кода фирмы.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.01.1999 ──┘
PROCEDURE Tst_Firm

IF ex = 2 .OR. EMPTY(f_cd) .OR. READKEY() % 256 = 12
  ON KEY LABEL F1
  RETURN .T.
ENDIF

f_cd = CHRTRAN(f_cd, shab1, shab2)
SHOW GET f_cd
IF .NOT. SEEK(f_cd, "FR_9113")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Код фирмы указан неверно!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
f_n = FR_9113.B_NAME
SHOW GET f_n
IF .NOT. EMPTY(FR_9113.DATE_OFF)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Эта фирма удалена!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

ON KEY LABEL F1

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура What_F       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Выбор фирмы из списка.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.01.1999 ──┘
PROCEDURE What_F

DO Ch_Firm WITH f_cd, f_n

SHOW GET f_cd
SHOW GET f_n

RETURN
