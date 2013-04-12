*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║          Список персонала с табелем за указанный месяц                ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE LS_TABLE
PRIVATE dummy_sw, dp, dp_list, dp_code, dp_n
dp = ""

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

DEFINE PAD Work OF (menu_name) PROMPT "Начисления"
ON PAD Work OF (menu_name) ACTIVATE POPUP Work

  DEFINE POPUP Work MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Work PROMPT "Табель" KEY Ctrl-A, "Enter"
  ON SELECTION BAR 1 OF Work DO Sw_Mode WITH "Tb_Use"

  DEFINE BAR  2 OF Work PROMPT "Закрыть м-ц" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 2 OF Work DO Sw_Mode WITH "Close"

  DEFINE BAR  3 OF Work PROMPT "Начислить" KEY Ctrl-A, "Ins"
  ON SELECTION BAR 3 OF Work DO Sw_Mode WITH "Inp_Pay"

  DEFINE BAR  4 OF Work PROMPT "Выдача аванса" KEY Ctrl-A, "Del"
  ON SELECTION BAR 4 OF Work DO Sw_Mode WITH "Pay_1"

  DEFINE BAR  5 OF Work PROMPT "Выдача зарплаты" KEY Ctrl-A, "Home"
  ON SELECTION BAR 5 OF Work DO Sw_Mode WITH "Pay_2"

  DEFINE BAR  6 OF Work PROMPT "Уволить!" KEY Ctrl-A, "F8"
  ON SELECTION BAR 6 OF Work DO Sw_Mode WITH "P_Out"

  DEFINE BAR  7 OF Work PROMPT "Новый год!"
  ON SELECTION BAR 7 OF Work DO Sw_Mode WITH "New_Year"

DEFINE PAD Pg_Up OF (menu_name) PROMPT "Месяц" KEY Ctrl-A, "F3"
ON SELECTION PAD Pg_Up OF (menu_name) Do Sw_Mode WITH "Pg_Mn"

DEFINE PAD Dep OF (menu_name) PROMPT "Отдел" KEY Ctrl-A, "F6"
ON SELECTION PAD Dep OF (menu_name) Do Sw_Mode WITH "Dep"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

***Определяем есть ли уже такой файл  

IF NOT FILE_O(base_path+"LS_TABLE.DBF")
  SELECT 0
  CREATE DBF (base_path+"LS_TABLE.DBF") ;
  ( PS_CODE N ( 5),;
    MONTH   N ( 2),;
    YEAR    N ( 4),;
    DAYS    C (31),;
    WHO     N ( 5),;
    WHERE   C ( 3))
  INDEX ON STR(PS_CODE,5)+STR(YEAR,4)+STR(MONTH,2) TAG PS_CODE
  USE	
ENDIF

SELECT 0
USE(base_path+"DEPS") ORDER TAG B_NAME
dp_n = 1
DIMENSION dp_list(dp_n), dp_code(dp_n)
dp_list(dp_n) = "Все отделы      "
dp_code(dp_n) = ""
SCAN
  dp_n = dp_n+1
  DIMENSION dp_list(dp_n), dp_code(dp_n)
  dp_list(dp_n) = B_NAME
  dp_code(dp_n) = STR(CODE,3)
ENDSCAN
dp_n = 1

USE(base_path+"SLR_TYPE")

IF NOT USED("LS")
  SELECT 0
  USE(base_path+"LS_TABLE") ALIAS LS ORDER TAG PS_CODE
ENDIF

SELECT LS

***Определяем есть ли уже такой файл  
 
IF .NOT. USED("PS")
  SELECT 0
  USE (base_path+"PERSONS") ALIAS PS ORDER TAG NAME
ENDIF  

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS WHERE AGAIN

SELECT 0
USE (base_path+"PS_STAT") ORDER TAG CODE ALIAS PS_ST000 AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WHO AGAIN

***Определяем есть ли уже такой файл  

IF .NOT. USED("MOULD")
  SELECT 0
  USE (base_path+"MOULD") ORDER TAG MONTH
  GO BOTTOM
  SKIP -1
ENDIF

dummy_sw = USED("DUMMY")
IF .NOT. dummy_sw
  DO Use_Dummy
ENDIF

SELECT PS

*
*   Расчет размеров окна
*
PRIVATE ln, wd, f_name, cl
ln = 18   && Количество видимых строк BROWSE
wd = 34

DO D_Wins WITH ln, wd, "", 1, 5

what_do = "list"
@ 1, 3 SAY "Отдел"
@ 2, 2 SAY "──Фамилия─────────────────────-?-Н"
DO Sun_Bord WITH WROWS()-7, 1, WROWS()-4, WCOLS()-2, " Примечание "
DO Sun_Bord WITH WROWS()-3, 1, WROWS()-1, WCOLS()-2, " Кто и где начислил деньги "
cl = SCHEME(13,1)+","+SCHEME(13,2)+",,,,,"+SCHEME(14,2)

f_name = ""
DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 9 SAY dp_list(dp_n)
    @ 0, 0 SAY PADC("Табель за "+MONTH_R(MOULD.MONTH)+STR(MOULD.YEAR,5), ;
               WCOLS()) COLOR SCHEME 5
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F3    Do Sw_Mode WITH "Pg_Mn"
    ON KEY LABEL Tab   Do Sw_Mode WITH "Close"
    ON KEY LABEL Ins   Do Sw_Mode WITH "Inp_Pay"
    ON KEY LABEL Del   DO Sw_Mode WITH "Pay_1"
    ON KEY LABEL Home  DO Sw_Mode WITH "Pay_2"
    ON KEY LABEL F6    Do Sw_Mode WITH "Dep"
    ON KEY LABEL F8    DO Sw_Mode WITH "P_Out"
    DO Set_Key
    SELECT PS
    SET RELATION TO STR(CODE,5)+STR(MOULD.YEAR,4)+STR(MOULD.MONTH,2) INTO LS
    SET RELATION TO CODE INTO PS_ST000 ADDITIVE
    IF EMPTY(dp)
      SET ORDER TO TAG NAME
    ELSE
      SET ORDER TO TAG DEP_NAME
    ENDIF

*
*    BROWSE - меню
*
    BROWSE KEY dp   ;
           FIELDS F000 = FAMILY-(" "+LEFT(NAME,1)+". "+LEFT(S_NAME,1)+"."):30:H="", ;
                  F001 = Is_Usual():1:H="",  ;
                  F002 = IIF(FOUND("LS") .AND. .NOT. EMPTY(LS.WHO), "√", " "):1:H="", ;
           DUMMY.F FREEZE DUMMY.F ;
           WHEN Show_Who()  ;
           NOAPPEND NODELETE NOMODIFY NORGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E") &&COLOR (cl)
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Tb_Use")
    ENDIF
    SET RELATION TO

  CASE what_do = "Dep"       && Выбор отдела

    DO Ch_Dep
    what_do = "list"

  CASE what_do = "New_Year"  && Новый год

    DO New_Year
    what_do = "list"

  CASE what_do = "Tb_Use"    && Табель по сотрудникам

    DO Bd_Table with MOULD.MONTH, MOULD.YEAR, PS.CODE

    what_do = "list"

  CASE what_do = "P_Out"     && Уволить

    DO P_Out WITH PS.CODE
    what_do = "list"

  CASE what_do = "Pg_Mn"     && Смена месяца

    DO Ch_Month
    what_do = "list"

  CASE what_do = "Close"     && Закрываем месяц

    DO Cl_Month
    what_do = "list"

  CASE what_do = "Inp_Pay"   && Начисления

    DO Set_Pay WITH PS.CODE, MOULD.YEAR, MOULD.MONTH
    what_do = "list"

  CASE what_do = "Pay_1"   && Аванс

    DO Pay_slr WITH PS.CODE, "1"
    what_do = "list"

  CASE what_do = "Pay_2"   && Зарплата

    DO Pay_slr WITH PS.CODE, "2"
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

SELECT SLR_TYPE
USE

SELECT LS
USE

SELECT PS
USE

SELECT WHO
USE

SELECT PS_ST000
USE

SELECT WHERE
USE

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
IF .NOT. dummy_sw
  SELECT DUMMY
  USE
ENDIF
SELECT MOULD

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║          Признак отклонения табеля от дефолтного.                     ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Is_Usual

IF .NOT. SEEK(STR(PS.CODE,5)+STR(MOULD.YEAR,4)+STR(MOULD.MONTH,2),"LS")
  RETURN " "
ENDIF

RETURN IIF(MOULD.DAYS = LS.DAYS, " ", "!")

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║             Кто и где начислил деньги по табелю.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Show_Who
PRIVATE s, s1, s2

IF EMPTY(LS.WHO) .OR. .NOT. FOUND("LS")
  s = "Деньги не начислены"
ELSE
  IF SEEK(LS.WHO,"WHO")
    s = ALLTRIM(WHO.FAMILY)+"  "
  ELSE
    s = "?  "
  ENDIF
  IF SEEK(LS.WHERE,"WHERE")
    s = s+ALLTRIM(WHERE.ARM_N)
  ELSE
    s = s+"?"
  ENDIF
ENDIF

STORE SPACE(29) TO s1, s2
IF FOUND("PS_ST000")
  s1 = PS_ST000.NOTE_1
  s2 = PS_ST000.NOTE_2
ENDIF
IF SET("BELL") = "ON" .AND. .NOT. EMPTY(s1) .AND. .NOT. EMPTY(s2)
  ?? CHR(7)
ENDIF

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-6, 2 SAY s1
@ WROWS()-5, 2 SAY s2
@ WROWS()-2, 2 SAY PADC(s, WCOLS()-4)
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                             Смена месяца.                             ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Ch_Month
PRIVATE _file,_d_file, mss

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

DEFINE PAD Choose OF (menu_name) PROMPT "Выбор" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"


*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

***Определяем есть ли уже такой файл  
PRIVATE s_sav, r_sav

s_sav = SELECT()

SELECT MOULD
r_sav = RECNO()
*
*   Расчет размеров окна
*
PRIVATE ln, wd, f_name, cl
*ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
ln = 16   && Количество видимых строк BROWSE
wd = 13

DO D_Wins WITH ln, wd, "Выберите:", 0, 0
what_do = "list"
@ 1, 2 SAY "─Месяц───Год"

f_name = ""

DO WHILE .T.
  DO CASE

  CASE what_do = "list"    && Просмотр списка

	statys_type = 3    
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

 SELECT MOULD
*
*    BROWSE - меню
*
    BROWSE FIELDS M000 = Month_R(MONTH) :8 :W=.F.:H="", ;
                  YEAR:H="",      ;
                  DUMMY.F FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NORGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")  &&COLOR (cl)
    ON KEY

    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Choose")
    ENDIF

  CASE what_do = "Choose"      && Выбор интересующего месяца

    EXIT

  OTHERWISE
	
    SELECT MOULD
	GO r_sav
    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
SELECT (s_sav)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Программа закрытия месяца.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 28.10.97 12:40:21 ═╝
PROCEDURE Cl_Month
PRIVATE mss, rc_sav, flg, cur_year, s_sav

IF MOULD.FLAG
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"А этот месяц уже закрыт..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"PARMS")
cur_year = SLR_YEAR
USE
IF MOULD.YEAR # cur_year
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Этот год еще (или уже) не открыт..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT(s_sav)
  RETURN
ENDIF

SELECT PS
rc_sav = RECNO()

flg = .T.
SCAN
  IF SEEK(STR(CODE,5)+STR(MOULD.YEAR,4)+STR(MOULD.MONTH,2), "LS")
    IF .NOT. EMPTY(LS.WHO)
      LOOP
    ENDIF
  ENDIF
  IF .NOT. Tbl_Pay(PS.CODE, MOULD.MONTH, MOULD.YEAR)
    flg = .F.
    EXIT
  ENDIF
  IF SEEK(STR(CODE,5)+STR(MOULD.YEAR,4)+STR(MOULD.MONTH,2), "LS")
    IF EMPTY(LS.WHO)
      flg = .F.
    ENDIF
  ENDIF
ENDSCAN

IF BETWEEN(rc_sav, 1, RECCOUNT())
  GO rc_sav
ENDIF
IF flg
  REPLACE MOULD.FLAG WITH .T.
ELSE
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Месяц не закрыт, т. к. Вы не произвели всех начислений!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_Key      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Установка обработчиков.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.09.1999 ──┘
PROCEDURE Set_Key

PRIVATE sh, i, c

sh = "АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЬЫЪЭЮЯ"
FOR i = 1 TO LEN(sh)
  c = "'"+SUBSTR(sh,i,1)+"'"
  ON KEY LABEL &c DO Seek_ WITH &c
ENDFOR

sh = SYS(15, lwr, sh)
FOR i = 1 TO LEN(sh)
  c = "'"+SUBSTR(sh,i,1)+"'"
  ON KEY LABEL &c DO Seek_ WITH &c
ENDFOR

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Поиск сотрудника по первой букве.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.09.1999 ──┘
PROCEDURE Seek_
PARAMETER c0

PRIVATE c1, r_sav

c1 = SYS(15, lwr, c0)
r_sav = RECNO()
IF .NOT. SEEK(dp+c1)
  ?? CHR(7)
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Dep       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Выбор отдела.                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.09.1999 ──┘
PROCEDURE Ch_Dep
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
DO D_Win_N WITH MIN(WROWS()-3,ALEN(dp_list)+3), 20, "Отделы предприятия"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
ex = dp_n

DO WHILE .T.
  ex = MAX(ex, 1)
  ex = MIN(ex, ALEN(dp_list))
  @ 1, 1 MENU dp_list, ALEN(dp_list), WROWS()-3
  READ MENU TO ex
  IF READKEY() % 256 = 15
    dp_n = ex
    dp = dp_code(dp_n)
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
*│   Процедура P_Out        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                                Уволить!                                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.04.2001 ──┘
PROCEDURE P_Out
PARAMETERS p_code

PRIVATE d_out, s_sav, tmpM, tmpY, minDay, MaxDay
s_sav = SELECT()
SELECT 0
USE (base_path+"LS_TABLE") ORDER TAG PS_CODE DESCENDING AGAIN ALIAS LSTB_1612
SET FILTER TO .NOT. EMPTY(WHO)
IF .NOT. SEEK(STR(p_code,5))
  USE (base_path+"PS_STAT") ORDER TAG CODE AGAIN ALIAS LSTB_1612
  SEEK p_code
  tmpM = MONTH(S_DATE)
  tmpY = YEAR(S_DATE)
  minDay = DAY(S_DATE)
ELSE
  tmpM = MONTH+1
  tmpY = YEAR
  IF tmpM > 12
    tmpM = 1
    tmpY = tmpY+1
  ENDIF
  minDay = 1
ENDIF
USE
maxDay = CTOD("01."+STR(tmpM,2)+"."+STR(tmpY,4))
maxDay = GOMONTH(maxDay,1)-1
maxDay = DAY(maxDay)
SELECT (s_sav)
d_out = {}
DO Get_Day
IF .NOT. EMPTY(d_out)
  DO Release WITH d_out, p_code
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Day      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Ввод даты увольнения                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.04.2001 ──┘
PROCEDURE Get_Day

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

*┌──────────────────────────────────────┐
*│         Уволить с ДД.ММ.ГГГГ         │
*│   < OK Ctrl-W > < Отказаться Esc >   │
*└──────────────────────────────────────┘

PRIVATE tmpD, ex, s_sav, tmpName
ex = 1
tmpD = 0
s_sav = SELECT()
SELECT 0
USE (base_path+"PERSONS") AGAIN ALIAS P_1412 ORDER TAG CODE
SEEK p_code
tmpName = ALLTRIM(FAMILY)+" "+LEFT(NAME, 1)+" "+LEFT(S_NAME, 1)
USE
SELECT (s_sav)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 5, 42, tmpName

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 11 SAY "Уволить с ДД.ММ.ГГГГ"
@ 2, 24 SAY RIGHT(STR(100+tmpM),2)
@ 2, 27 SAY STR(tmpY,4)
@ 2, 21 GET tmpD PICTURE "@Z 99"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Day()

IF ex = 1
  *
  * Отрабатываем бланк
  d_out = STR(tmpD,2)+"."+RIGHT(STR(100+tmpM),2)+"."+STR(tmpY,4)
  d_out = CTOD(d_out)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Day      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Проверка дня на корректность.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.04.2001 ──┘
PROCEDURE Tst_Day

PRIVATE mss

IF ex # 1
  RETURN .T.
ENDIF

IF .NOT. BETWEEN(tmpD, 1, MaxDay)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Такой даты в календаре не бывает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ELSE
  IF tmpD < MinDay
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Он тогда еще у нас не работал!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.
