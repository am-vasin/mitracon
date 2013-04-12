*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Rep_El1      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║               Проданные позиции по дням для предприятия.               ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 04/05/1999 ══╝
PROCEDURE Rep_El1

PRIVATE f_c000, f_n000, b_sw000, f_c_old, d0, d1
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
f_c000 = def_firm
f_c_old = def_firm
f_n000 = SPACE(16)
d1 = DATE()
d0 = d1-6

IF .NOT. F_Form()
  RETURN
ENDIF

DO Wt_Mess WITH "Подготовка данных..."
DO Prep_Dat  && Подготовка данных
DO Wt_Mess

d0 = New_File("", "DBF", "Укажите директорию и имя файла", 0)
IF .NOT. EMPTY(d0)
  COPY FILE (tmpo_path+"REP_EL1.DBF") TO (d0)
ENDIF

DELETE FILE (tmpo_path+"REP_EL1.DBF")

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура F_Form       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Заполнение экранной формы.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04/05/1999 ──┘
PROCEDURE F_Form
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

*┌──────────────────────────────────────────┐
*│  Предприятие 999 (F1) 9999999999999999   │
*│ ┌─ Период ─────────────────────────────┐ │
*│ │ С ДД.ММ.ГГГГ (F1) по ДД.ММ.ГГГГ (F1) │ │
*│ └──────────────────────────────────────┘ │
*│     < OK Ctrl-W > < Отказаться Esc >     │
*└──────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 46, "Заполните бланк"
DO Sun_Bord WITH  3,  3,  5, 42, " Период "
@ 4, 5 SAY "С            (F1) по            (F1)"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO Get_Firm WITH 2, 4, enab_firm
@ 4,  7 GET d0 WHEN Set_F1(1) VALID Clr_F1()
@ 4, 26 GET d1 WHEN Set_F1(2) VALID Clr_F1() 
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

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
*└────────────────────────────────────────────────────────── 04/05/1999 ──┘
PROCEDURE Set_F1
PARAMETERS mode

IF mode = 1
  ON KEY LABEL F1 DO Ch_Date WITH 1
ELSE
  ON KEY LABEL F1 DO Ch_Date WITH 2
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Clr_F1       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Очистка обработчика F1.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04/05/1999 ──┘
PROCEDURE Clr_F1

ON KEY LABEL F1

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Date      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│               Интерактивный выбор даты по календаю.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04/05/1999 ──┘
PROCEDURE Ch_Date
PARAMETERS mode

IF mode = 1
  d0 = Get_Date(d0)
  SHOW GET d0
ELSE
  d1 = Get_Date(d1)
  SHOW GET d1
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Blank    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Выход из бланка.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04/05/1999 ──┘
PROCEDURE Tst_Blank

PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF d0 > d1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Начальная дата интервала больше конечной? Здесь что-то не так!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали предприятие..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Dat     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Подготовка данных.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.04.1999 ──┘
PROCEDURE Prep_Dat

DELETE FILE (tmpo_path+"REP_EL0.DBF")
DELETE FILE (tmpo_path+"REP_EL0.CDX")
DELETE FILE (tmpo_path+"REP_EL1.DBF")

CREATE DBF (tmpo_path+"REP_EL0") ;
  ( SALE_DATE D,     ;
    CODE      N( 7), ;
    PREFIX    C( 4), ;
    NAME      C(35), ;
    PRODUCER  C( 8), ;
    GTD_NUM   C(26), ;
    COUNTRY   C(16), ;
    GTD_LINE  C( 4), ;
    QNT       N( 6), ;
    SALE_SUM  N(12,2) )
INDEX ON DTOS(SALE_DATE)+STR(CODE)+GTD_NUM+GTD_LINE+COUNTRY TAG CODE

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM

SEEK f_c000
SCAN REST WHILE FIRM = f_c000
  IF .NOT. BETWEEN(DOC_DATE, d0, d1)
    LOOP
  ENDIF
  SELECT REP_EL0
  IF .NOT. SEEK(DTOS(SALE.DOC_DATE)+STR(SALE.CODE)+SALE.GTD_NUM++SALE.GTD_LINE+SALE.COUNTRY)
    APPEND BLANK
    REPLACE CODE      WITH SALE.CODE,     ;
            SALE_DATE WITH SALE.DOC_DATE, ;
            GTD_NUM   WITH SALE.GTD_NUM,  ;
            GTD_LINE  WITH SALE.GTD_LINE, ;
            COUNTRY   WITH SALE.COUNTRY
  ENDIF
  REPLACE QNT      WITH QNT+SALE.QNT, ;
          SALE_SUM WITH SALE_SUM+SALE.PRICE_0*SALE.QNT
  SELECT SALE
ENDSCAN

USE (base_path+"STOCK") ORDER TAG CODE
SELECT REP_EL0
SET RELATION TO CODE INTO STOCK
REPLACE ALL PREFIX   WITH STOCK.PREFIX, ;
            NAME     WITH STOCK.NAME,   ;
            PRODUCER WITH STOCK.PRODUCER
INDEX ON DTOS(SALE_DATE)+NAME+PREFIX+PRODUCER TAG NAME
COPY TO (tmpo_path+"REP_EL1")
USE
DELETE FILE (tmpo_path+"REP_EL0.DBF")
DELETE FILE (tmpo_path+"REP_EL0.CDX")
SELECT STOCK
USE
RETURN
