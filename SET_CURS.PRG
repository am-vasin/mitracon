*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Set_Curs     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Установка/просмотр курсов доллара.                   ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 18.02.1999 ══╝
PROCEDURE Set_Curs
PARAMETERS let_corr

PRIVATE curs_c, curs_n
curs_c = ""
curs_n = ""
IF .NOT. Ch_Curs(curs_c, curs_n)
  RETURN
ENDIF

DO View_C

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_c       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Хронология курса.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.02.1999 ──┘
PROCEDURE View_c

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

DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_9218 AGAIN

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS A_9218 AGAIN

SELECT 0
USE (base_path+"CURS_SET") ORDER TAG HRON ALIAS C_9218 AGAIN
SET RELATION TO WHO INTO P_9218
SET RELATION TO WHERE INTO A_9218 ADDITIVE
GO BOTTOM

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Type OF (menu_name) PROMPT "Тип курса" KEY Ctrl-A, "Tab"
ON SELECTION PAD Type OF (menu_name) DO Sw_Mode WITH "Type"

IF let_corr
  DEFINE PAD Corr OF (menu_name) PROMPT "Установка курса" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"
ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
*┌───Дата──────Время───Курс──Кто установил────────────Рабочее место───┐
*│ДД.ММ.ГГГГ ЧЧ:ММ:СС 99.999 NNNNNNNNNNNNNNNNNNNNNNNN AAAAAAAAAAAAAAAA│
*└────────────────────────────────────────────────────────────────────┘
PRIVATE ln, wd, w_c, w_n
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 68

DO D_Wins WITH ln, wd, "", 0, 0
@ 1, 2 SAY "───Дата──────Время───Курс──Кто установил────────────Рабочее место"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M")
    @ 0, 0 SAY PADC(ALLTRIM(curs_n), WCOLS()) COLOR SCHEME 5
    DO Prp_Nav_1
    IF let_corr
      ON KEY LABEL Enter KEYBOARD CHR(23)
    ENDIF
    ON KEY LABEL Tab DO Sw_Mode WITH "Type"

*
*    BROWSE - меню
*
    BROWSE KEY curs_c   ;
           FIELDS DATE_SET:H="",   ;
                  TIME_SET:H="",   ;
                  CURS:H="":P="99.999", ;
                  P_9218.FAMILY:H="",   ;
                  A_9218.ARM_N:H="",    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && Установка курса

    DO New_Val
    what_do = "List"

  CASE what_do = "Type"    && Смена типа

    w_c = curs_c
    w_n = curs_n
    IF Ch_Curs(w_c, w_n)
      curs_c = w_c
      curs_n = w_n
      GO BOTTOM
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
SELECT C_9218
USE
SELECT P_9218
USE
SELECT A_9218
USE
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура New_Val      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Установка курса.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.02.1999 ──┘
PROCEDURE New_Val
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

*┌─────────────────────────────────────────────────┐
*│                                                 │
*│ Текущее значение 99.999  Новое значение 99.999  │
*│                                                 │
*│        < OK Ctrl-W > < Отказаться Esc >         │
*└─────────────────────────────────────────────────┘

PRIVATE ex, c_flt, c_new
c_new = SELECT()
c_flt = Get_Curs(curs_c)
SET ORDER TO TAG HRON ASCENDING   && Эмпирика!
c_new = 0
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 53, ALLTRIM(curs_n)

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3, 3 SAY "Текущее значение "+TRANSFORM(c_flt, "99.999")+"  Новое значение"  ;
      GET c_new PICTURE "99.999"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Curs()

IF ex = 1
  *
  * Отрабатываем бланк
  SELECT C_9218
  APPEND BLANK
  REPLACE B_NAME   WITH curs_c, ;
          DATE_SET WITH DATE(), ;
          TIME_SET WITH TIME(), ;
          CURS     WITH c_new,  ;
          WHO      WITH user,   ;
          WHERE    WITH arm
  IF ALLTRIM(curs_c) == "CURS"
    PRIVATE ss, cnt0
    ss = SELECT()
    SELECT 0
    USE (base_path+"STOCK") ORDER TAG CODE
    DO Wt_Mess WITH "Пересчет цен..."
    cnt0 = 0
    SCAN
      cnt0 = cnt0+1
      IF cnt0 % 100 = 0
        @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt0)),WCOLS())
      ENDIF
      REPLACE PRICE_R    WITH Clc_Rub(PRICE, c_new),  ;
              PRICE_O_R  WITH Clc_Rub(PRICE_O, c_new),;
              PRICE_O1_R WITH Clc_Rub(PRICE_O1, c_new)
    ENDSCAN
    DO Wt_Mess
    USE
    SELECT (ss)
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_curs     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Проверка корректности курса.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.02.1999 ──┘
PROCEDURE Tst_curs
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF c_new <= 0
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Срочно обратитесь к разработчику!"
  mss(3) = CHR(0)+"Он купит все Ваши доллары по утроенному(!) курсу!"
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF c_flt # 0
  IF ABS(c_new-c_flt)/c_flt > .15
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+" Курс изменился более, чем на 15%! Это правильно? [ Нет! ] [ Да ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

RETURN .T.
