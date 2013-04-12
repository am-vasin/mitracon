*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Set_Pay      Разработчик Андрей Васин           29.10.97 16:53:28 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                           Начисление денег.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Set_Pay
PARAMETERS p_code, ye_p, mn_p

PRIVATE sel_sav, is_dummy, lst_sav, s, ye, mn, mss, cur_year

sel_sav = SELECT()

SELECT 0
USE (base_path+"PARMS") ALIAS P_1405 AGAIN
cur_year = SLR_YEAR

USE (base_path+"MOULD") ORDER TAG MONTH ALIAS MLD_PAY AGAIN
IF RECCOUNT() < 2
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одного утвержденного месяца!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT MLD_PAY
  USE
  SELECT (sel_sav)
  RETURN
ENDIF
  
IF TYPE("ye_p") = "N"
  ye = ye_p
ELSE
  ye = YEAR(DATE())
ENDIF

IF TYPE("mn_p") = "N"
  mn = mn_p
ELSE
  mn = MONTH(DATE())
ENDIF

SEEK STR(ye,4)+STR(mn,2)

IF .NOT. FOUND() .OR. RECNO() = RECCOUNT()
  GO BOTTOM
  SKIP -1
  mn = MONTH
  ye = YEAR
ENDIF

is_dummy = USED("DUMMY")
IF .NOT. is_dummy
  DO Use_Dummy
ENDIF

DIMENSION lst_sav(4)
IF USED("SLR_TYPE")
  lst_sav(1) = .T.
  SELECT SLR_TYPE
  lst_sav(2) = SET("FILTER")
  lst_sav(3) = SET("ORDER")
  lst_sav(4) = RECNO()
ELSE
  lst_sav(1) = .F.
  SELECT 0
  USE (base_path+"SLR_TYPE")
ENDIF
SET ORDER TO RANG
SET FILTER TO .NOT. EMPTY(TYPE)
IF EOF() .OR. BOF()
  GO TOP
ENDIF
IF EOF() .OR. BOF()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не создано ни одного типа начислений..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  IF lst_sav(1)
    s = lst_sav(2)
    SET FILTER TO &s
    s = lst_sav(3)
    SET ORDER TO &s
  ELSE
    USE
  ENDIF
  SELECT MLD_PAY
  USE
  RETURN
ENDIF

PUSH KEY CLEAR
IF .NOT. Chs_Type()
  IF lst_sav(1)
    s = lst_sav(2)
    SET FILTER TO &s
    s = lst_sav(3)
    SET ORDER TO &s
    IF BETWEEN(lst_sav(4), 1, RECCOUNT())
      GO lst_sav(4)
    ENDIF
  ELSE
    USE
  ENDIF
  IF .NOT. is_dummy
    SELECT DUMMY
    USE
  ENDIF
  SELECT MLD_PAY
  USE
  SELECT (sel_sav)
  POP KEY
  RETURN
ENDIF

POP KEY

DO CASE
CASE ALLTRIM(TYPE)=="1"
  DO Set_P_1 WITH p_code, ye, mn
CASE ALLTRIM(TYPE)=="2" .OR. ALLTRIM(TYPE)=="3"
  DO Set_P_23 WITH p_code, ye, mn
CASE ALLTRIM(TYPE)=="4" .OR. ALLTRIM(TYPE)=="5" .OR. ALLTRIM(TYPE)=="6"
  DO Set_P_456 WITH p_code, ye, mn
OTHERWISE
  DO (ALLTRIM(TYPE)) WITH p_code, ye, mn
ENDCASE

SELECT SLR_TYPE
IF lst_sav(1)
  s = lst_sav(2)
  SET FILTER TO &s
  s = lst_sav(3)
  SET ORDER TO &s
ELSE
  USE
ENDIF
IF .NOT. is_dummy
  SELECT DUMMY
  USE
ENDIF
SELECT MLD_PAY
USE
SELECT (sel_sav)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Выбор типа начисления.                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.10.97 17:13:35 ═╝
PROCEDURE Chs_Type
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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd, ex, rc
rc = RECNO()
ex = .F.
ln = 12
wd = 24

DO D_Wins WITH ln, wd, "Типы начислений", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:H="":24,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    ex = .T.
    EXIT

  OTHERWISE

    SELECT SLR_TYPE
    IF BETWEEN(rc, 1, RECCOUNT())
      GO rc
    ENDIF
    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN ex

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                        Начисление первого типа.                        ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.10.97 19:03:23 ═╝
PROCEDURE Set_P_1
PARAMETERS p_code, ye, mn

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
SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_PAY ORDER TAG CODE AGAIN
SEEK p_code
SELECT 0
USE (base_path+"PS_STAT") ALIAS PAY_STAT ORDER TAG CODE AGAIN
IF .NOT. SEEK(p_code)
  APPEND BLANK
  REPLACE CODE      WITH p_code, ;
          START     WITH Crpt_1(0,10,2), ;
          REST      WITH Crpt_1(0,10,2), ;
          SALARY    WITH PRS_PAY.SALARY, ;
          PREV_SUM  WITH Crpt_1(0,5),    ;
          PREV_SUMD WITH Crpt_1(0,5),    ;
          PAY_SUM   WITH Crpt_1(0,5),    ;
          PAY_VAL   WITH "Р",            ;
          S_DATE    WITH PRS_PAY.DATE_IN

ENDIF

*                          < Тип начисления >
*┌────────────────────────────────────────────────────────────────────┐
*│ ┌─ Сотрудник ────────────────────────────────────────────────────┐ │
*│ │ ....:....!....:....!.... ....:....!....:. ....:....!....:....! │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│                                 ┌──────────┐┌──────┐               │
*│              Сумма 999999.99 за │ ....:... ││ 1234 │               │
*│                                 └──────────┘└──────┘               │
*│                ┌─ Примечание ─────────────────────┐                │
*│                │     ....:....!....:....!....     │                │
*│                └──────────────────────────────────┘                │
*│                  < OK Ctrl-W > < Отказаться Esc >                  │
*└────────────────────────────────────────────────────────────────────┘

PRIVATE ex, mn_list, ye_list, i, ye_numb, ye_w, mn_w, total, nt
ye_numb = 5
DIMENSION ye_list(ye_numb)
FOR i = 1 TO ye_numb
  ye_list(i) = STR(cur_year+i-ye_numb,4)
ENDFOR
nt = SPACE(24)

DIMENSION mn_list(12)
FOR i = 1 TO 12
  mn_list(i) = Month_R(i)
ENDFOR
mn_w = mn
ye_w = ASCAN(ye_list, STR(ye,4))
total= 0
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 13, 72, ALLTRIM(SLR_TYPE.NAME)
DO Sun_Bord WITH 2, 3, 4, 68, " Сотрудник "
DO Sun_Bord WITH WROWS()-5, 18, WROWS()-3, 53, " Примечание "
@ 3, 5 SAY PRS_PAY.FAMILY-(" "+PRS_PAY.NAME)-(" "+PRS_PAY.S_NAME)
@ 6, 16 SAY "Сумма 999999.99 за"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 6, 22 GET total PICTURE "@Z 999999.99"
@ 5, 35 GET mn_w PICTURE "@^ " FROM mn_list SIZE 1, 12
@ 5, 47 GET ye_w PICTURE "@^ " FROM ye_list SIZE 1, 8
@ WROWS()-4, 24 GET nt
@ WROWS()-2, 20 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1 .AND. total # 0
  DO To_Inc WITH SLR_TYPE.CODE, p_code, 0, 0, total,   ;
                 SLR_TYPE.UNIT, VAL(ye_list(ye_w)), mn_w, nt
ENDIF
SELECT PRS_PAY
USE
SELECT PAY_STAT
USE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║              Начисление второго и третьего типов.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.10.97 19:03:23 ═╝
PROCEDURE Set_P_23
PARAMETERS p_code, ye, mn

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
SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_PAY ORDER TAG CODE AGAIN
SEEK p_code
SELECT 0
USE (base_path+"PS_STAT") ALIAS PAY_STAT ORDER TAG CODE AGAIN
IF .NOT. SEEK(p_code)
  APPEND BLANK
  REPLACE CODE      WITH p_code, ;
          START     WITH Crpt_1(0,10,2), ;
          REST      WITH Crpt_1(0,10,2), ;
          SALARY    WITH PRS_PAY.SALARY, ;
          PREV_SUM  WITH Crpt_1(0,5),    ;
          PREV_SUMD WITH Crpt_1(0,5),    ;
          PAY_SUM   WITH Crpt_1(0,5),    ;
          PAY_VAL   WITH "Р",            ;
          S_DATE    WITH PRS_PAY.DATE_IN

ENDIF

*                          < Тип начисления >
*┌────────────────────────────────────────────────────────────────────┐
*│ ┌─ Сотрудник ────────────────────────────────────────────────────┐ │
*│ │ ....:....!....:....!.... ....:....!....:. ....:....!....:....! │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│ Начислить за 99999.99 units_______  по 9999999.99 итого 9999999.99 │
*│                             ┌──────────┐┌──────┐                   │
*│                   За период │ ....:... ││ 1234 │                   │
*│                             └──────────┘└──────┘                   │
*│                ┌─ Примечание ─────────────────────┐                │
*│                │     ....:....!....:....!....     │                │
*│                └──────────────────────────────────┘                │
*│                  < OK Ctrl-W > < Отказаться Esc >                  │
*└────────────────────────────────────────────────────────────────────┘
PRIVATE ex, mn_list, ye_list, i, ye_numb, ye_w, mn_w, total, prc, q, nt
ye_numb = 5
DIMENSION ye_list(ye_numb)
FOR i = 1 TO ye_numb
  ye_list(i) = STR(cur_year+i-ye_numb,4)
ENDFOR
nt = SPACE(24)
total = 0
prc   = SLR_TYPE.PRICE
q     = 0
DIMENSION mn_list(12)
FOR i = 1 TO 12
  mn_list(i) = Month_R(i)
ENDFOR
mn_w = mn
ye_w = ASCAN(ye_list, STR(ye,4))
total= 0
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 14, 72, ALLTRIM(SLR_TYPE.NAME)
DO Sun_Bord WITH 2, 3, 4, 68, " Сотрудник "
DO Sun_Bord WITH WROWS()-5, 18, WROWS()-3, 53, " Примечание "
@ 3, 5 SAY PRS_PAY.FAMILY-(" "+PRS_PAY.NAME)-(" "+PRS_PAY.S_NAME)
@ 5, 3 SAY "Начислить за 99999.99 units_______  по 9999999.99 итого"
@ 5,25 SAY SLR_TYPE.UNIT
@ 7,21 SAY "За период"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 5, 16 GET q PICTURE "@Z 99999.99" VALID Set_Total()
@ 5, 42 GET prc PICTURE "@Z 9999999.99" VALID Set_Total() ;
              WHEN "3" $ SLR_TYPE.TYPE
@ 5, 59 GET total PICTURE "@Z 999999.99" WHEN .F.
@ 6, 31 GET mn_w PICTURE "@^ " FROM mn_list SIZE 1, 12
@ 6, 43 GET ye_w PICTURE "@^ " FROM ye_list SIZE 1, 8
@ WROWS()-4, 24 GET nt
@ WROWS()-2, 20 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1 .AND. total # 0
  DO To_Inc WITH SLR_TYPE.CODE, p_code, q, prc, total,   ;
                 SLR_TYPE.UNIT, VAL(ye_list(ye_w)), mn_w, nt
ENDIF
SELECT PRS_PAY
USE
SELECT PAY_STAT
USE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                             Расчет суммы.                              ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.10.97 21:02:46 ═╝
PROCEDURE Set_Total

total = q*prc
SHOW GET total

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║              Начисление четвертого - шестого типов.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.10.97 19:03:23 ═╝
PROCEDURE Set_P_456
PARAMETERS p_code, ye, mn

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
SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_PAY ORDER TAG CODE AGAIN
SEEK p_code
SELECT 0
USE (base_path+"PS_STAT") ALIAS PAY_STAT ORDER TAG CODE AGAIN
IF .NOT. SEEK(p_code)
  APPEND BLANK
  REPLACE CODE      WITH p_code, ;
          START     WITH Crpt_1(0,10,2), ;
          REST      WITH Crpt_1(0,10,2), ;
          SALARY    WITH PRS_PAY.SALARY, ;
          PREV_SUM  WITH Crpt_1(0,5),    ;
          PREV_SUMD WITH Crpt_1(0,5),    ;
          PAY_SUM   WITH Crpt_1(0,5),    ;
          PAY_VAL   WITH "Р",            ;
          S_DATE    WITH RPS_PAY.DATE_IN

ENDIF

*                          < Тип начисления >
*┌────────────────────────────────────────────────────────────────────┐
*│ ┌─ Сотрудник ────────────────────────────────────────────────────┐ │
*│ │ ....:....!....:....!.... ....:....!....:. ....:....!....:....! │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│        ┌──────────────────────────────────────────────────┐        │
*│        │  Оклад 99999    Кол-во рабочих дней в месяце 99  │        │
*│        └──────────────────────────────────────────────────┘        │
*│ Начислить за 99999.99 units_______  по 9999999.99 итого 9999999.99 │
*│                             ┌──────────┐┌──────┐                   │
*│                   За период │ ....:... ││ 1234 │                   │
*│                             └──────────┘└──────┘                   │
*│                  < OK Ctrl-W > < Отказаться Esc >                  │
*└────────────────────────────────────────────────────────────────────┘
PRIVATE ex, mn_list, ye_list, i, ye_numb, ye_w, mn_w, total, prc, q, nt
ye_numb = 5
DIMENSION ye_list(ye_numb)
FOR i = 1 TO ye_numb
  ye_list(i) = STR(cur_year+i-ye_numb,4)
ENDFOR
nt = SPACE(24)
total = 0
prc   = SLR_TYPE.PRICE
q     = 0
DIMENSION mn_list(12)
FOR i = 1 TO 12
  mn_list(i) = Month_R(i)
ENDFOR
mn_w = mn
ye_w = ASCAN(ye_list, STR(ye,4))
total= 0
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 17, 72, ALLTRIM(SLR_TYPE.NAME)
DO Sun_Bord WITH 2, 3, 4, 68, " Сотрудник "
DO Sun_Bord WITH 5, 10, 7, 61
DO Sun_Bord WITH WROWS()-5, 18, WROWS()-3, 53, " Примечание "
@  3, 5 SAY PRS_PAY.FAMILY-(" "+PRS_PAY.NAME)-(" "+PRS_PAY.S_NAME)
@  8, 3 SAY "Начислить за 99999.99 units_______  по 9999999.99 итого"
@  8,25 SAY SLR_TYPE.UNIT
@ 10,21 SAY "За период"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  8, 16 GET q PICTURE "@Z 99999.99" VALID Set_Total() WHEN Set_Pr()
@  8, 42 GET prc PICTURE "@Z 9999999.99" WHEN .F.
@  8, 59 GET total PICTURE "@Z 999999.99" WHEN .F.
@  9, 31 GET mn_w PICTURE "@^ " FROM mn_list SIZE 1, 12 VALID Set_Pr()
@  9, 43 GET ye_w PICTURE "@^ " FROM ye_list SIZE 1, 8 VALID Set_Pr()
@ WROWS()-4, 24 GET nt
@ WROWS()-2, 20 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1 .AND. total # 0
  DO To_Inc WITH SLR_TYPE.CODE, p_code, q, prc, total,   ;
                 SLR_TYPE.UNIT, VAL(ye_list(ye_w)), mn_w, nt
ENDIF
SELECT PRS_PAY
USE
SELECT PAY_STAT
USE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                  Расчет цены на основании календаря.                   ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 30.10.97 09:30:02 ═╝
PROCEDURE Set_Pr

PRIVATE sw, wg, d, s

d = 0
sw = .T.
IF .NOT. SEEK(ye_list(ye_w)+STR(mn_w,2), "MLD_PAY")
  sw = .F.
ELSE
  IF RECNO("MLD_PAY") = RECCOUNT("MLD_PAY")
    sw = .F.
  ELSE
    d = OCCURS("Р", MLD_PAY.DAYS)
  ENDIF
ENDIF

wg = Crpt_2(PAY_STAT.SALARY)
IF sw .OR. "4" $ SLR_TYPE.TYPE
  prc = wg
  IF "5" $ SLR_TYPE.TYPE
    prc = ROUND(prc/d,2)
  ENDIF
  IF "6" $ SLR_TYPE.TYPE
    prc = ROUND(wg/d/8,2)
  ENDIF
  prc = prc*SLR_TYPE.KOEFF
ELSE
  prc = 0
  WAIT WINDOW "ВНИМАНИЕ! Календарь за "+Month_R(mn_w)+" "+ye_list(ye_w)+ ;
              " не утвержден или не существует!"
ENDIF
SHOW GET prc
s = "Оклад "+ALLTRIM(STR(wg))+"    Кол-во рабочих дней в месяце "+ ;
    IIF(d = 0, "**", STR(d,2))
@ 6, 11 SAY PADC(s,50)
DO Set_Total

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Начисление отпускных.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.10.97 19:03:23 ═╝
PROCEDURE Rest_Year
PARAMETERS p_code, ye, mn

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
SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_PAY ORDER TAG CODE AGAIN
SEEK p_code
SELECT 0
USE (base_path+"PS_STAT") ALIAS PAY_STAT ORDER TAG CODE AGAIN
IF .NOT. SEEK(p_code)
  APPEND BLANK
  REPLACE CODE      WITH p_code, ;
          START     WITH Crpt_1(0,10,2), ;
          REST      WITH Crpt_1(0,10,2), ;
          SALARY    WITH PRS_PAY.SALARY, ;
          PREV_SUM  WITH Crpt_1(0,5),    ;
          PREV_SUMD WITH Crpt_1(0,5),    ;
          PAY_SUM   WITH Crpt_1(0,5),    ;
          PAY_VAL   WITH "Р",            ;
          S_DATE    WITH RPS_PAY.DATE_IN
ENDIF
IF EMPTY(S_DATE)
  REPLACE S_DATE WITH PRS_PAY.DATE_IN
ENDIF

*                          < Тип начисления >
*┌────────────────────────────────────────────────────────────────────┐
*│ ┌─ Сотрудник ────────────────────────────────────────────────────┐ │
*│ │ ....:....!....:....!.... ....:....!....:. ....:....!....:....! │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│   Оклад 99999 Сд. часть 999999.99 итого 999999.99 за день 9999.99  │
*│                                                                    │
*│   Начислить за 9999 год с ДД.ММ.ГГГГ по ДД.ММ.ГГГГ за 9999 дней    │
*│                                                                    │
*│   Отпуск 9999 дней  Адм. отпуск 9999 дней  Итого 9999 дней         │
*│                                                                    │
*│                  Итого за 9999 дней 9999999.99 $                   │
*│                                                                    │
*│                Примечание ....:....!....:....!....                 │
*│                                                                    │
*│                  < OK Ctrl-W > < Отказаться Esc >                  │
*└────────────────────────────────────────────────────────────────────┘
PRIVATE ex, ye_w, total, prc, q, nt, slr_w, d1, d2, n_days1, n_days2, days1, days2
PRIVATE slr_w1, slr_w2
days1 = 0
days2 = 0
n_days2 = 0
slr_w1 = Crpt_2(PAY_STAT.SALARY)  && Оклад
slr_w2 = Salary2(p_code, 3)       && Сдельная часть
slr_w  = slr_w1+slr_w2            && Итого

d1    = PAY_STAT.S_DATE
d2    = Last_Day(p_code)          && Последний закрытый день
nt    = SPACE(24)

n_days1 = d2-d1+1
DO Adm WITH days1, days2, n_days2
q       = n_days1-n_days2
prc     = ROUND(slr_w/330,2)
ye_w    = YEAR(d2)
total   = q*prc

ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 17, 72, ALLTRIM(SLR_TYPE.NAME)
DO Sun_Bord WITH 2, 3, 4, 68, " Сотрудник "
@  3, 5 SAY PRS_PAY.FAMILY-(" "+PRS_PAY.NAME)-(" "+PRS_PAY.S_NAME)
*------------------------------------------------------------------------
*      Ввод полей бланка
*

@  5,  5 SAY "Оклад" GET slr_w1 PICTURE "@Z 99999" VALID Calc_V()
@  5, 17 SAY "Сд. часть" GET slr_w2 PICTURE "@Z 99999.99" VALID Calc_V()
@  5, 37 SAY "итого" GET slr_w PICTURE "@Z 99999.99" WHEN .F.
@  5, 53 SAY "за день" GET prc PICTURE "@Z 9999.99" WHEN .F.
@  7,  5 SAY "Начислить за" GET ye_w PICTURE "@Z 9999" WHEN .F.
@  7, 23 SAY "год с" GET d1 PICTURE "@D" WHEN .F.
@  7, 40 SAY "по" GET d2 PICTURE "@D" VALID Calc_V(.T.)
@  7, 54 SAY "за" GET n_days1 PICTURE "@Z 9999" WHEN .F.
@  7, 62 SAY "дней"
@  9,  5 SAY "Отпуск" GET days1 PICTURE "@Z 9999" WHEN .F.
@  9, 17 SAY "дней  Адм. отпуск" GET days2 PICTURE "@Z 9999" WHEN .F.
@  9, 40 SAY "дней  Итого" GET n_days2 PICTURE "@Z 9999" WHEN .F.
@  9, 57 SAY "дней"
@ 11, 20 SAY "Итого за" GET q PICTURE "@Z 9999" WHEN .F.
@ 11, 34 SAY "дней" GET total PICTURE "@Z 9999999.99" WHEN .F.
@ 11, 50 SAY "$"
@ 13, 18 SAY "Примечание" GET nt
@ WROWS()-2, 20 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1 .AND. total # 0
  REPLACE PAY_STAT.S_DATE WITH d2+1
  DO To_Inc WITH SLR_TYPE.CODE, p_code, q, prc, total,   ;
                 SLR_TYPE.UNIT, ye_w, 0, nt
ENDIF
SELECT PRS_PAY
USE
SELECT PAY_STAT
USE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Расчет значений.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 05.11.97 13:07:00 ═╝
PROCEDURE Calc_V
PARAMETER is_adm
n_days1 = d2-d1+1
SHOW GET n_days1
IF is_adm
  DO Adm WITH days1, days2, n_days2
  SHOW GET days1
  SHOW GET days2
  SHOW GET n_days2
ENDIF
slr_w   = slr_w1+slr_w2
q       = n_days1-n_days2
prc     = ROUND(slr_w/330,2)
ye_w    = YEAR(d2)
total = q*prc

SHOW GET slr_w1
SHOW GET slr_w2
SHOW GET slr_w
SHOW GET q
SHOW GET prc
SHOW GET ye_w
SHOW GET total

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║           Расчет количества дней административного отпуска.            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 05.11.97 13:09:57 ═╝
PROCEDURE Adm
PARAMETERS d_1, d_2, d_h

PRIVATE s_sav, r_sav, n_w, str_w, m_st, m_en

s_sav = SELECT()
SELECT LS
r_sav = RECNO()
SEEK STR(p_code,5)
m_st = YEAR(d1)*12+MONTH(d1)
m_en = YEAR(d2)*12+MONTH(d2)
d_1 = 0
d_2 = 0
SCAN REST WHILE PS_CODE = p_code FOR BETWEEN(YEAR*12+MONTH, m_st, m_en)
  str_w = DAYS
  IF YEAR*12+MONTH = m_en
    str_w = LEFT(str_w, DAY(d2)-1)
  ENDIF
  IF YEAR*12+MONTH = m_st
    str_w = SUBSTR(str_w, DAY(d1))
  ENDIF
  d_1 = d_1 + OCCURS("О", str_w)
  d_2 = d_2 + OCCURS("А", str_w)
ENDSCAN
d_h = d_1+d_2
RETURN
