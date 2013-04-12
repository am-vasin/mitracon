*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║         Подготовка данных для экспорта в бухгалтерию.                 ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Export_C

PRIVATE dat_0, dat_1, dat_v0, dat_v1
PRIVATE mss, new_sum, tmpNSP, tmpSum, tmpNds
PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2
m.tmpNds = 18
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
f_c000 = "   "
f_n000 = SPACE(16)
f_c_old = f_c000
USE (base_path+"PARMS")
dat_0  = DAT_START
dat_v0 = DAT_START
USE
dat_1  = DATE()
dat_v1 = DATE()

IF .NOT. Fill_Bl()
  RETURN
ENDIF

DO Wt_Mess WITH "Подборка данных..."

IF File_O(tmpo_path+"EXPORT_C.DBF")
  DELETE FILE (tmpo_path+"EXPORT_C.DBF")
ENDIF
SET EXCLUSIVE ON
CREATE DBF (tmpo_path+"EXPORT_C.DBF") ;
    ( DATE       D,       ;
      DEBIT      N(4,1),  ;
      CREDIT     N(4,1),  ;
      CUS_CODE   N( 5),   ;
      CUS_NAME   C(30),   ;
      REASON     C(25),   ;
      SUM        N(14,2), ;
      CONTAIN    C(30) )

USE (tmpo_path+"EXPORT_C.DBF")
SET EXCLUSIVE OFF

SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE
SEEK f_c000
tmpNSP = P_NALOG

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"PAY_BILL") ORDER TAG HRON

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON

SEEK DTOS(dat_0)
IF .NOT. FOUND()
  ex = RECNO(0)
  IF ex > 0 .AND. ex <= RECCOUNT()
    GO ex
  ELSE
    GO BOTTOM
    ex = 0
  ENDIF
ELSE
  ex = RECNO()
ENDIF
IF ex # 0
  SCAN REST WHILE DOC_DATE <= dat_1 FOR FIRM = f_c000
    IF NEW_TYPE # " " .OR. TOTAL_R = 0
      LOOP
    ENDIF
*    IF "***" $ DOC_NUM .OR. .NOT. EMPTY(SUBSTR(SAL_FLAGS,15,1))
*      LOOP
*    ENDIF
    c_name = IIF(SEEK(CUS_CODE,"ACCOUNT"),  ;
             ACCOUNT.CUS_NAME, CUS_NAME)
    new_sum = (ABS(TOTAL_R)+ROUND(MIN(ABS(TOTAL_R), ABS(KASSA))*NSP_/100,2))*SIGN(TOTAL_R)
    SELECT EXPORT_C
    APPEND BLANK
    REPLACE DEBIT      WITH 62.1,  ;
            CREDIT     WITH 46.1,  ;
            DATE       WITH SALE_TIT.DOC_DATE, ;
            CUS_CODE   WITH SALE_TIT.CUS_CODE,   ;
            CUS_NAME   WITH c_name,  ;
            REASON   WITH IIF(SALE_TIT.NEW_TYPE="0", "*"," ")+  ;
                LEFT(SALE_TIT.NEW_DOC,1)+ALLTRIM(SUBSTR(SALE_TIT.NEW_DOC,2)),  ;
            SUM        WITH new_sum,     ;
            CONTAIN    WITH "Отпущены комплектующие"
    SELECT SALE_TIT
  ENDSCAN
ENDIF

SELECT PAY_BILL
SEEK DTOS(dat_0)
IF .NOT. FOUND()
  ex = RECNO(0)
  IF ex > 0 .AND. ex <= RECCOUNT()
    GO ex
  ELSE
    GO BOTTOM
    ex = 0
  ENDIF
ELSE
  ex = RECNO()
ENDIF
IF ex # 0
  SCAN REST WHILE DOC_DATE <= dat_1 FOR FIRM = f_c000
    c_name = IIF(SEEK(CUS_CODE,"ACCOUNT"),  ;
             ACCOUNT.CUS_NAME, CUS_NAME)
    SELECT EXPORT_C
    APPEND BLANK
    IF EMPTY(PAY_BILL.SUB_DOC)
      m.tmpSum = PAY_BILL.BILL_SUM
      m.tmpSum  = ROUND(m.tmpSum*tmpNds/(100+tmpNds), 2)
      REPLACE DEBIT      WITH 76.7,  ;
              CREDIT     WITH 68.2,  ;
              DATE       WITH PAY_BILL.DOC_DATE, ;
              CUS_CODE   WITH PAY_BILL.CUS_CODE, ;
              CUS_NAME   WITH c_name,  ;
              REASON     WITH LEFT(PAY_BILL.DOC_NUM, 2)+"-"+ALLTRIM(SUBSTR(PAY_BILL.DOC_NUM,3)),  ;
              SUM        WITH m.tmpSum,			 ;
              CONTAIN    WITH "Счет-фактура на аванс"
    ELSE
      m.tmpSum = -PAY_BILL.BILL_SUM
      m.tmpSum  = ROUND(m.tmpSum*tmpNds/(100+tmpNds), 2)
      REPLACE DEBIT      WITH 68.2,  ;
              CREDIT     WITH 76.7,  ;
              DATE       WITH PAY_BILL.DOC_DATE, ;
              CUS_CODE   WITH PAY_BILL.CUS_CODE, ;
              CUS_NAME   WITH c_name,  ;
              REASON     WITH LEFT(PAY_BILL.DOC_NUM, 2)+"-"+ALLTRIM(SUBSTR(PAY_BILL.DOC_NUM,3))+"/"+ALLTRIM(PAY_BILL.SUB_DOC),  ;
              SUM        WITH m.tmpSum, ;
              CONTAIN    WITH "Счет-фактура на аванс"
    ENDIF
    SELECT PAY_BILL
  ENDSCAN
ENDIF

SELECT INCMONEY
SCAN FOR BETWEEN(ENTER_DAT, dat_0, dat_1) .AND. DOC_TYPE $ "1Б" .AND. FIRM = f_c000
  c_name = IIF(SEEK(CUS_CODE,"ACCOUNT"),  ;
           ACCOUNT.CUS_NAME, CUS_NAME)

  new_sum = INCMONEY.MONEY_R
  IF INCMONEY.DOC_TYPE = "Б"
    new_sum = (ABS(new_sum)+ROUND(ABS(new_sum)*tmpNSP/100,2))*SIGN(new_sum)
  ENDIF
  SELECT EXPORT_C
  APPEND BLANK
  REPLACE DEBIT      WITH 51.1,  ;
          CREDIT     WITH 62.1,  ;
          DATE       WITH IIF(EMPTY(INCMONEY.ENTER_DAT), INCMONEY.DOC_DATE, INCMONEY.ENTER_DAT), ;
          CUS_CODE   WITH INCMONEY.CUS_CODE,   ;
          CUS_NAME   WITH c_name,  ;
          REASON     WITH ALLTRIM(INCMONEY.ENTER_DOC), ;
          SUM        WITH new_sum, ;
          CONTAIN    WITH "Пост. на р/с за товар"
  SELECT INCMONEY
ENDSCAN

SET ORDER TO TAG HRON
SEEK DTOS(dat_0)
IF .NOT. FOUND()
  ex = RECNO(0)
  IF ex > 0 .AND. ex <= RECCOUNT()
    GO ex
  ELSE
    GO BOTTOM
    ex = 0
  ENDIF
ELSE
  ex = RECNO()
ENDIF
IF ex # 0
  SCAN REST WHILE DOC_DATE <= dat_1 FOR FIRM = f_c000 .AND. DOC_TYPE $ " Ч"
    c_name = IIF(SEEK(CUS_CODE,"ACCOUNT"),  ;
             ACCOUNT.CUS_NAME, CUS_NAME)
    SELECT EXPORT_C
    APPEND BLANK
    IF INCMONEY.MONEY_R > 0
      REPLACE DEBIT      WITH IIF(INCMONEY.DOC_TYPE = "Ч", 50.2, 50.1),  ;
              CREDIT     WITH 62.1,  ;
              DATE       WITH IIF(EMPTY(INCMONEY.ENTER_DAT), INCMONEY.DOC_DATE, INCMONEY.ENTER_DAT), ;
              CUS_CODE   WITH INCMONEY.CUS_CODE,   ;
              CUS_NAME   WITH c_name,  ;
              REASON     WITH LEFT(INCMONEY.DOC_NUM,1)+ALLTRIM(SUBSTR(INCMONEY.DOC_NUM,2)), ;
              SUM        WITH INCMONEY.MONEY_R+INCMONEY.N_OF_SALE, ;
              CONTAIN    WITH IIF(INCMONEY.DOC_TYPE = "Ч", "Чек (приход)", "Приход в кассу")
    ELSE
      REPLACE DEBIT      WITH 62.1,  ;
              CREDIT     WITH IIF(INCMONEY.DOC_TYPE = "Ч", 50.2, 50.1),  ;
              DATE       WITH IIF(EMPTY(INCMONEY.ENTER_DAT), INCMONEY.DOC_DATE, INCMONEY.ENTER_DAT), ;
              CUS_CODE   WITH INCMONEY.CUS_CODE,   ;
              CUS_NAME   WITH c_name,  ;
              REASON     WITH LEFT(INCMONEY.DOC_NUM,1)+ALLTRIM(SUBSTR(INCMONEY.DOC_NUM,2)), ;
              SUM        WITH -INCMONEY.MONEY_R-INCMONEY.N_OF_SALE, ;
              CONTAIN    WITH IIF(INCMONEY.DOC_TYPE = "Ч", "Чек (расход)", "Расход из кассы")
    ENDIF
    SELECT INCMONEY
  ENDSCAN
ENDIF

DO Wt_Mess

IF RECCOUNT("EXPORT_C") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"За указанный Вами период не найдено ни одной операции..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  CLOSE DATABASES
  DO To_Disk WITH tmpo_path+"EXPORT_C.DBF"
ENDIF

CLOSE DATABASES
DELETE FILE (tmpo_path+"EXPORT_C.DBF")

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Fill_Bl      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Заполнение бланка.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/15/1999 ──┘
PROCEDURE Fill_Bl
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
*│   ┌─ Интервал дат ───────────────┐   │
*│   │  С ДД.ММ.ГГГГ по ДД.ММ.ГГГГ  │   │
*│   └──────────────────────────────┘   │
*│Предприятие .... ....:....!....:. (F1)│
*│   < OK Ctrl-W > < Отказаться Esc >   │
*└──────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 42, "Заполните бланк"
DO Sun_Bord WITH  2,  5,  4, 36, " Интервал дат "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  8 SAY "С" GET dat_0
@  3, 21 SAY "по" GET dat_1
DO Get_Firm WITH 5, 2, .T.
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID T_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура T_Blank      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Проверка корректности заполнения бланка.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/15/1999 ──┘
PROCEDURE T_Blank
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы забыли указать предприятие!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_0 < dat_v0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Имеем данные только с "+DTOC(dat_v0)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_1 > dat_v1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Сегодня только "+DTOC(dat_v1)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_1 < dat_0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы указали начальную дату больше конечной!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
