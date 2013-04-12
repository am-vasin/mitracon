*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла For_MD       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║               Отчет по возвратам для Михаила Дубинского.               ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 10/18/1999 ══╝
PROCEDURE For_MD

PRIVATE d_0, d_1, dep_c, w

d_0 = {}
d_1 = DATE()
dep_c = 0
IF File_O(prmo_path+"FOR_MD.MEM")
  RESTORE FROM (prmo_path+"FOR_MD.MEM") ADDITIVE
  IF TYPE("___d") = "N"
    dep_c = ___d
  ENDIF
ENDIF

IF .NOT. G_Parms()
  RETURN
ENDIF

DELETE FILE (tmpo_path+"FOR_MD.DBF")
CREATE DBF (tmpo_path+"FOR_MD.DBF") ;
  ( FIRM      C( 3), ;
    REV_DOC   C(10), ;
    REV_DATE  D,     ;
    SALE_DOC  C(10), ;
    SALE_DATE D,     ;
    CUS_CODE  N( 8), ;
    CUS_NAME  C(50), ;
    WHO_REV_C N( 5), ;
    WHO_REV   C(24), ;
    WHO_SAL_C N( 5), ;
    WHO_SAL   C(24), ;
    STO_CODE  N( 7), ;
    PREFIX    C( 4), ;
    NAME      C(35), ;
    PRODUCER  C( 8), ;
    QNT       N( 6), ;
    PRICE_D   N(10,4) )

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_REV

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_SAL AGAIN

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS SALED AGAIN
SET RELATION TO WHO INTO P_SAL
SET RELATION TO CUS_CODE INTO ACCOUNT ADDITIVE

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON ALIAS REV AGAIN
SET RELATION TO WHO INTO P_REV

SEEK DTOS(d_0)
IF .NOT. FOUND()
  w = RECNO(0)
  IF BETWEEN(w, 1, RECCOUNT())
    GO w
  ENDIF
ENDIF

DO Wt_Mess WITH "Подборка данных!"

SCAN REST FOR TOTAL_R < 0 .AND. .NOT. EMPTY(REQ_DOC) WHILE DOC_DATE <= d_1
  @ WROWS()-1, 0 SAY PADC(DTOC(DOC_DATE), WCOLS())
  IF .NOT. SEEK(FIRM+LEFT(DTOS(REQ_DATE),4)+REQ_DOC, "SALED")
    LOOP
  ENDIF
  IF P_SAL.DEP_NO # dep_c
    LOOP
  ENDIF
  SELECT SALE
  SEEK REV.FIRM+LEFT(DTOS(REV.DOC_DATE),4)+REV.DOC_NUM
  SCAN REST WHILE REV.FIRM = FIRM .AND. ;
                  REV.DOC_DATE = DOC_DATE .AND. ;
                  REV.DOC_NUM = DOC_NUM
    SELECT FOR_MD
    APPEND BLANK
    REPLACE FIRM      WITH SALED.FIRM, ;
            REV_DOC   WITH REV.DOC_NUM,    ;
            REV_DATE  WITH REV.DOC_DATE,   ;
            SALE_DOC  WITH SALED.DOC_NUM,  ;
            SALE_DATE WITH SALED.DOC_DATE, ;
            CUS_CODE  WITH SALED.CUS_CODE, ;
            CUS_NAME  WITH ACCOUNT.CUS_NAME, ;
            WHO_REV_C WITH REV.WHO, ;
            WHO_REV   WITH P_REV.FAMILY, ;
            WHO_SAL_C WITH SALED.WHO, ;
            WHO_SAL   WITH P_SAL.FAMILY, ;
            STO_CODE  WITH SALE.CODE, ;
            PREFIX    WITH STOCK.PREFIX, ;
            NAME      WITH STOCK.NAME, ;
            PRODUCER  WITH STOCK.PRODUCER, ;
            QNT       WITH SALE.QNT,     ;
            PRICE_D   WITH SALE.PRICE_D
    SELECT SALE
  ENDSCAN
  SELECT REV
ENDSCAN
DO Wt_Mess

CLOSE DATABASES

PRIVATE f_name
IF TYPE("___f") = "C"
  f_name = ___f
ELSE
  f_name = ""
ENDIF

f_name = New_File(f_name, "dbf", "Имя файла с результатом")
IF .NOT. EMPTY(f_name)
  COPY FILE (tmpo_path+"FOR_MD.DBF") TO (f_name)
  w = RAT("\", f_name)
  ___f = LEFT(f_name,w-1)
ENDIF

SAVE ALL LIKE ___* TO (prmo_path+"FOR_MD.MEM")

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Parms      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                  Интерактивное заполнение параметров.                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10/18/1999 ──┘
PROCEDURE G_Parms
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

USE (base_path+"DEPS") ORDER TAG B_NAME

PRIVATE ex, d_names, d_codes, p
ex = 0
SCAN
  ex = ex+1
  DIMENSION d_names(ex), d_codes(ex)
  d_names(ex) = B_NAME
  d_codes(ex) = CODE
ENDSCAN
USE
IF ex < 1
  DIMENSION d_names(3)
  d_names(1) = ""
  d_names(2) = CHR(0)+"Список отделов пуст!"
  d_names(3) = ""
  DO Out_Mess WITH 7, "d_names"
  RETURN .F.
ENDIF

ex = 1
p = ASCAN(d_codes, dep_c)
p = MAX(p,1)

*------------------------------------------------------------------------
*┌─────────────────────────────────────────────┐
*│                ┌────────────────┐           │
*│          Отдел │....:....!....:.│           │
*│                └────────────────┘           │
*│ Возвраты с ДД.ММ.ГГГГ(F1) по ДД.ММ.ГГГГ(F1) │
*│      < OK Ctrl-W > < Отказаться Esc >       │
*└─────────────────────────────────────────────┘

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 49, "Заполните таблицу"
@ 3, 12 SAY "Отдел"
@ 5, 3 SAY "Возвраты с           (F1) по           (F1)"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 5, 14 GET d_0 PICTURE "@D" WHEN Set_F1(1) VALID Clr_F1()
@ 5, 32 GET d_1 PICTURE "@D" WHEN Set_F1(2) VALID Clr_F1()
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
@ 2, 18 GET p PICTURE "@^" FROM d_names SIZE 1, 18

READ CYCLE
dep_c = d_codes(p)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_F1       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Установка обработчика F1.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10/18/1999 ──┘
PROCEDURE Set_F1
PARAMETERS t

IF t = 1
  ON KEY LABEL F1 DO S_Date WITH 1
ELSE
  ON KEY LABEL F1 DO S_Date WITH 2
ENDIF

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура S_Date       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Выбор даты по календарю.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10/18/1999 ──┘
PROCEDURE S_Date
PARAMETERS t

IF t = 1
  d_0 = Get_Date(d_0)
  SHOW GET d_0
ELSE
  d_1 = Get_Date(d_1)
  SHOW GET d_1
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Clr_F1       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Очистка обработчика.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10/18/1999 ──┘
PROCEDURE Clr_F1

ON KEY LABEL F1

RETURN .T.
