*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл New_Pay      Разработчик Андрей Васин           24.09.97 18:23:10 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Просмотр оплаченых счетов.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE New_Pay
PARAMETERS allDocs
PRIVATE mss, tmpAll

m.tmpAll = m.allDocs

DO Prep_View
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не найдено ни одного оплаченного счета!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"NEW_PAY.DBF")
  DELETE FILE (tmpo_path+"NEW_PAY.CDX")
  RETURN
ENDIF

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
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DO Use_Dummy

SELECT NEW_PAY

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY") = "ON", 68, 66)

DO D_Wins WITH ln, wd, "Оплаченные счета", 0, 3
@ 1, 2 SAY "─N счета────────"+IIF(SET("CENTURY") = "ON", "───Дата────","──Дата───")
@ 1, COL() SAY "──По счету───Оплачено────Депозит─Список─Б"
@ WROWS()-4, 3 SAY "Предприятие: Счет                  Оплата "
@ WROWS()-3, 3 SAY "Клиент"
@ WROWS()-2, 3 SAY "Имя списка"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - меню
*
    BROWSE FIELDS NUM_BILL:H="",   ;
                  DAT_BILL:H="",   ;
                  BILL_SUM:H="":P="@Z 9999999.99", ;
                  PAY_SUM:H="":P="@Z 9999999.99",  ;
                  DEPOSIT:H="":P="@Z 9999999.99",  ;
                  LIST_NUM:H="",   ;
                  BRON:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_Cus()  ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED
DELETE FILE (tmpo_path+"NEW_PAY.DBF")
DELETE FILE (tmpo_path+"NEW_PAY.CDX")

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Подготовка временного файла.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 24.09.97 18:24:15 ═╝
PROCEDURE Prep_View
PRIVATE br, br_diff, br_emp, p_sum, cus_n, b_n, dp, f_nm

DELETE FILE (tmpo_path+"NEW_PAY.DBF")
DELETE FILE (tmpo_path+"NEW_PAY.CDX")
CREATE DBF (tmpo_path+"NEW_PAY.DBF") ;
   ( NUM_BILL C(15), ;
     DAT_BILL D,     ;
     BILL_SUM N(10,2), ;
     PAY_SUM  N(10,2), ;
     DEPOSIT  N(10,2), ;
     LIST_NUM C( 6), ;
     LIST_NAM C(32), ;
     BRON     C( 1), ;
     FIRM1    C(16), ;
     FIRM2    C(16), ;
     CUS_NAME C(50)  )

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG LIST

SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG BILL_NUM

SELECT 0
USE (base_path+"BILLS") ORDER TAG HRON

*SCAN FOR .NOT. EMPTY(INC_DOC) .AND. LEFT(DOC_NUM,1) = sys_char
SCAN FOR SEEK(FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM, "INCMONEY") .AND. LEFT(DOC_NUM,1) = sys_char
  IF .NOT. tmpAll .AND. WHERE # m.arm
    LOOP
  ENDIF
  br = " "
  IF .NOT. EMPTY(NUM_LIST)                 && Список был...
    IF .NOT. SEEK(NUM_LIST, "LIST_TIT")    &&  но уже удален...
      LOOP
    ENDIF
    IF .NOT. EMPTY(LIST_TIT.SAL_NUM)       &&    ... или продан
      LOOP
    ENDIF
    *
    *   А что с бронью?
    *
    SELECT LIST_DET
    SEEK STR(LIST_TIT.LST_NUM,6)
    br_diff = .F.
    br_emp  = .T.
    SCAN REST WHILE STR(LIST_TIT.LST_NUM,6) = LIST_NUM
      br_diff = br_diff .OR. QNT # QNT_REQ   && Не все еще забронировано
      br_emp  = br_emp .AND. QNT_REQ = 0     && Вообще ничего не забр.
    ENDSCAN
    IF .NOT. br_emp
      br = IIF(br_diff, "░", "▓")
    ENDIF
  ENDIF
  p_sum = 0
  IF SEEK(BILLS.FIRM+LEFT(DTOS(BILLS.DOC_DATE),4)+BILLS.DOC_NUM, "INCMONEY")
    p_sum = INCMONEY.MONEY_R
  ELSE
    SELECT BILLS
    LOOP
  ENDIF
  cus_n = BILLS.CUS_NAME
  dp = 0
  IF SEEK(INCMONEY.CUS_CODE,"ACCOUNT")
    cus_n = ACCOUNT.CUS_NAME
  ENDIF
  IF SEEK(STR(INCMONEY.CUS_CODE)+INCMONEY.FIRM,"CUS_REST")
    dp = IIF(INCMONEY.DOC_TYPE = "3", CUS_REST.ACC_SUMD, CUS_REST.ACC_SUM)
  ENDIF
  b_n = LEFT(BILLS.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(BILLS.DOC_NUM,5))
  IF BILLS.N_VER # 0
    b_n = b_n+"/"+ALLTRIM(STR(BILLS.N_VER))
  ENDIF
  = SEEK(BILLS.FIRM, "FIRMS")
  f_nm = FIRMS.B_NAME
  = SEEK(INCMONEY.FIRM, "FIRMS")
  SELECT NEW_PAY
  APPEND BLANK
  REPLACE NUM_BILL WITH b_n, ;
          DAT_BILL WITH BILLS.DOC_DATE, ;
          BILL_SUM WITH BILLS.BILL_SUM, ;
          PAY_SUM  WITH p_sum,  ;
          DEPOSIT  WITH dp,     ;
          LIST_NUM WITH TRANSFORM(BILLS.NUM_LIST,"@Z 999999"), ;
          LIST_NAM WITH IIF(EMPTY(BILLS.NUM_LIST), "", LIST_TIT.LST_NAME), ;
          BRON     WITH br, ;
          FIRM1    WITH f_nm, ;
          FIRM2    WITH FIRMS.B_NAME, ;
          CUS_NAME WITH cus_n
  SELECT BILLS
ENDSCAN

SELECT LIST_TIT
USE
SELECT LIST_DET
USE
SELECT ACCOUNT
USE
SELECT FIRMS
USE
SELECT CUS_REST
USE
SELECT INCMONEY
USE
SELECT BILLS
USE
SELECT NEW_PAY

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Вывод имени клиента.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 24.09.97 18:24:15 ═╝
PROCEDURE Show_Cus

ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-4, 21 SAY NEW_PAY.FIRM1
@ WROWS()-4, 45 SAY NEW_PAY.FIRM2
@ WROWS()-3, 10 SAY NEW_PAY.CUS_NAME
@ WROWS()-2, 15 SAY NEW_PAY.LIST_NAM

ACTIVATE WINDOW (ALIAS())

RETURN .T.
