*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Bill         Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                        Программа выписки счета.                        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 18.01.1999 ══╝
PROCEDURE Bill
PARAMETERS l_num

PRIVATE f_c000, f_n000, b_c000, b_n000, b_sw000, f_c_old, emp_l
PRIVATE shab1, shab2, cont_type, tmpType
tmpType = " "
emp_l = EMPTY(l_num)
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
PRIVATE vlt_tmp, c_code, c_name, w_nds
PRIVATE s_sav, acc_open
s_sav = SELECT()
acc_open = USED("ACCOUNT")
IF .NOT. acc_open
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME AGAIN
ENDIF
SELECT 0
IF .NOT. EMPTY(l_num)
  USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_9118 AGAIN
  IF .NOT. SEEK(l_num)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Списка N "+ALLTRIM(STR(l_num))+" не существует!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    IF .NOT. acc_open
      SELECT ACCOUNT
      USE
    ENDIF
    SELECT (s_sav)
    RETURN
  ENDIF
  tmpType = IIF(Is_Ur(LT_9118.CLI_CODE), " ", "0")
  IF EMPTY(R_TOTAL)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Список N "+ALLTRIM(STR(l_num))+" пуст! Это правильно? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 2
      USE
      IF .NOT. acc_open
        SELECT ACCOUNT
        USE
      ENDIF
      SELECT (s_sav)
      RETURN
    ENDIF
    emp_l = .T.
  ENDIF
  IF .NOT. EMPTY(SAL_NUM)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Список N "+ALLTRIM(STR(l_num))+" уже продан!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    IF .NOT. acc_open
      SELECT ACCOUNT
      USE
    ENDIF
    SELECT (s_sav)
    RETURN
  ENDIF
  IF EMPTY(FIRM)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"В списке N "+ALLTRIM(STR(l_num))+" не определена фирма!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    IF .NOT. acc_open
      SELECT ACCOUNT
      USE
    ENDIF
    SELECT (s_sav)
    RETURN
  ENDIF
  IF EMPTY(FIRM)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"В списке N "+ALLTRIM(STR(l_num))+" не определен банк!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    IF .NOT. acc_open
      SELECT ACCOUNT
      USE
    ENDIF
    SELECT (s_sav)
    RETURN
  ENDIF
  vlt_tmp = IIF(LT_9118.IS_USD, 2, 1)
  c_code = CLI_CODE
  c_name = PADR(LST_NAME,50)
  f_c000 = FIRM
  f_n000 = ""
  b_c000 = BANK
  b_n000 = ""
  b_sw000=.F.
  f_c_old=FIRM
  IF .NOT. EMPTY(c_code)
    SELECT ACCOUNT
    SET ORDER TO TAG CUS_CODE
    SEEK c_code
    c_name = CUS_NAME
  ELSE
    SELECT ACCOUNT
    SET ORDER TO TAG CUS_NAME
    GO TOP
  ENDIF
ELSE
  vlt_tmp = 1
  c_code = 0
  c_name = SPACE(50)
  IF EMPTY(def_firm) .AND. .NOT. enab_firm .OR. ;
     EMPTY(def_bank) .AND. .NOT. enab_bank
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"На этом рабочем месте запрещено выписывать счета!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    IF .NOT. acc_open
      SELECT ACCOUNT
      USE
    ENDIF
    SELECT (s_sav)
    RETURN
  ENDIF
  f_c000 = def_firm
  f_n000 = ""
  b_c000 = def_bank
  b_n000 = ""
  b_sw000=.F.
  f_c_old=def_firm
ENDIF
w_nds = IIF(vlt_tmp=1, nds_flt, 0)
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F0_9118 AGAIN
IF SEEK(f_c000)
  f_n000 = B_NAME
ELSE
  f_n000 = SPACE(FSIZE("B_NAME"))
ENDIF
USE (base_path+"BANKS") ORDER TAG BANK ALIAS F0_9118 AGAIN
IF SEEK(f_c000+b_c000)
  b_n000 = B_NAME
ELSE
  b_n000 = SPACE(FSIZE("B_NAME"))
ENDIF
USE

cont_type = .NOT. emp_l

IF .NOT. Bill_Tit()
  IF USED("LT_9118")
    SELECT LT_9118
    USE
  ENDIF
  IF .NOT. acc_open
    SELECT ACCOUNT
    USE
  ENDIF
  SELECT (s_sav)
  RETURN
ENDIF
IF .not. cont_type
  DO Cr_Bill0
ELSE
  DO Cr_Bill1
ENDIF

IF USED("LT_9118")
  SELECT LT_9118
  USE
ENDIF
IF .NOT. acc_open
  SELECT ACCOUNT
  USE
ENDIF
SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Bill_Tit     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Заголовок счета.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.01.1999 ──┘
PROCEDURE Bill_Tit
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

*┌──────────────────────────────────────────────────────────────────┐
*│                    ┌─ Валюта ──────────────┐                     │
*│                    │  ( ) Рубли   ( ) USD  │                     │
*│                    └───────────────────────┘                     │
*│                        Ставка НДС 999.9                          │
*│      ┌─ Клиент ───────────────────────────────────────────┐      │
*│      │ [ ] Новый                                          │      │
*│      │ ....:....!....:....!....:....!....:....!....:....! │      │
*│      └────────────────────────────────────────────────────┘      │
*│ Предприятие ... ....:....!....:. (F1)  [ ] Банк ....:....!....:. │
*│             [ ] Содержимое в соответствии со списком             │
*│                 < OK Ctrl-W > < Отказаться Esc >                 │
*└──────────────────────────────────────────────────────────────────┘

DO Use_Dummy
DO Use_Link
PRIVATE ex, new_c
ex = 1
new_c = EMPTY(c_code)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 14, 70, "Выписка счета"+IIF(EMPTY(l_num), "", "к списку "+ALLTRIM(STR(l_num)))
DO Sun_Bord WITH  2, 22,  4, 46, " Валюта "
DO Sun_Bord WITH  6,  8,  9, 61, " Клиент "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3, 25 GET vlt_tmp PICTURE "@*RH Рубли  ;USD" WHEN EMPTY(l_num) VALID Exit_Vlt()
@  5, 26 SAY "Ставка НДС" GET w_nds WHEN EMPTY(l_num) .AND. vlt_tmp = 1 PICTURE "999.9"
@  7, 10 GET new_c PICTURE "@*C Новый" WHEN EMPTY(l_num) VALID Get_Cus()
@  8, 10 GET c_name WHEN new_c
DO Get_Firm WITH 10, 3, enab_firm .AND. EMPTY(l_num)
DO Get_Bank WITH 10, 42, enab_bank .AND. EMPTY(l_num)
@ 11, 15 GET cont_type PICTURE "@*C Содержимое в соответствии со списком" WHEN .NOT. emp_l
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blnk()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Exit_Vlt     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Выход из поля валюты.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.01.1999 ──┘
PROCEDURE Exit_Vlt
IF vlt_tmp = 2
  w_nds = 0
  SHOW GET w_nds
ELSE
  w_nds = nds_flt
  SHOW GET w_nds
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Cus      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Выбор клиента из списка.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.01.1999 ──┘
PROCEDURE Get_Cus
IF new_c
  c_code = 0
  RETURN .T.
ENDIF

PRIVATE sel_rem, rec_rem

sel_rem = SELECT()
SELECT ACCOUNT
rec_rem = RECNO()
IF c_code # 0
  =SEEK(c_code)
ENDIF

IF .NOT. Custs("", .T., .F.)
  IF BETWEEN(sel_rem, 1, RECCOUNT())
    GO sel_rem
  ENDIF
  IF c_code = 0
    new_c = .T.
    SHOW GET new_c
  ENDIF
ELSE
  c_code = ACCOUNT.CUS_CODE
  c_name = ACCOUNT.CUS_NAME
  SHOW GET c_name
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Cr_Bill1     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Создание счета по списку.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.01.1999 ──┘
PROCEDURE Cr_Bill1

PRIVATE s_sav, s0, s1, d_n, d_d, p_w, k0, tmpS, s00, st
k0 = 1+w_nds/100
d_d = DATE()
d_n = Doc_Num("BILLS", f_c000, arm, d_d)
d_n = sys_char+CHRTRAN(arm," ","0")+STR(d_n,6)
s_sav = SELECT()
SELECT 0
USE (base_path+"BILLS_D") ORDER TAG DOC_NUM ALIAS BD_9118 AGAIN
SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_NAME ALIAS LD_9118 AGAIN
SEEK STR(l_num,6)
s0 = 0
s1 = 0
SCAN REST WHILE STR(l_num,6) = LIST_NUM
  IF vlt_tmp = 1
    p_w = ROUND(SAL_PRICE/k0, 2)
  ELSE
    p_w = ROUND(USD_PRICE/k0, 2)
  ENDIF  
  s00 = LD_9118.QNT*p_w
  st  = s00+ROUND(s00*w_nds/100, 2)
  SELECT BD_9118
  GO TOP
  DO WHILE .NOT. LOCK()
  ENDDO
  IF .NOT. EMPTY(DOC_NUM) .OR. BOF() .OR. EOF()
    UNLOCK
    DO WHILE .NOT. FLOCK()
    ENDDO
    APPEND BLANK
  ENDIF
  REPLACE FIRM     WITH f_c000,  ;
          DOC_NUM  WITH d_n,     ;
          DOC_DATE WITH d_d,     ;
          CODE     WITH LD_9118.CODE, ;
          NAME     WITH LD_9118.PREFIX+" "+LD_9118.NAME+" "+LD_9118.PRODUCER, ;
          QNT      WITH LD_9118.QNT, ;
          PRICE_0  WITH p_w,     ;
          SUM_0    WITH s00,     ;
          SUM_T    WITH st
  UNLOCK
  s0 = s0+SUM_0
  s1 = s1+SUM_T
  SELECT LD_9118
ENDSCAN
USE
SELECT BD_9118

USE (base_path+"BILLS") ORDER TAG DOC_NUM ALIAS BL_9118 AGAIN
IF .NOT. EMPTY(LT_9118.BILL_NUM)
  IF SEEK(LT_9118.BILL_FIRM+LT_9118.BILL_YEAR+LT_9118.BILL_NUM)
    REPLACE OLD WITH .T.
  ENDIF
ENDIF
SELECT LT_9118
REPLACE LT_9118.BILL_FIRM WITH f_c000, ;
        LT_9118.BILL_NUM  WITH d_n,    ;
        LT_9118.BILL_YEAR WITH STR(YEAR(d_d),4)
SELECT BL_9118
GO TOP
DO WHILE .NOT. LOCK()
ENDDO
IF .NOT. EMPTY(DOC_NUM) .OR. BOF() .OR. EOF()
  UNLOCK
*  DO WHILE .NOT. FLOCK()
*  ENDDO
  APPEND BLANK
ENDIF
REPLACE FIRM     WITH f_c000, ;
        DOC_NUM  WITH d_n,    ;
        DOC_DATE WITH d_d,    ;
        DOC_TIME WITH TIME(), ;
        DOC_TYPE WITH tmpType,;
        CUS_CODE WITH c_code, ;
        CUS_NAME WITH c_name, ;
        BANK     WITH b_c000, ;
        VALUTA   WITH IIF(vlt_tmp = 1, "Р", "$"),;
        NDS_     WITH w_nds,  ;
        SUM_0    WITH s0,     ;
        BILL_SUM WITH s1,     ;
        NUM_LIST WITH l_num,  ;
        WHERE    WITH arm,    ;
        WHO      WITH user
UNLOCK
USE
PRIVATE mss_0
DIMENSION mss_0(3)
mss_0(1) = ""
mss_0(2) = CHR(0)+"  Создан счет N "+LEFT(d_n,4)+"-"+ALLTRIM(RIGHT(d_n,6))+  ;
          "("+f_c000+"). Печатаем? [ Да ] [ Нет ]"
mss_0(3) = ""
IF Out_Mess(5,"mss_0") = 1
  DO P_Bill WITH f_c000, d_n, d_d
ENDIF

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Cr_Bill0     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Создание счета без списка.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.01.1999 ──┘
PROCEDURE Cr_Bill0

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
DELETE FILE (tmpo_path+"CR_BILL0.DBF")
CREATE DBF (tmpo_path+"CR_BILL0.DBF") ;
  ( NAME  C(30),  ;
    QNT   N( 6),  ;
    PRICE N(8,2), ;
    SUM_0 N(12,2),;
    SUM_T N(12,2) )
APPEND BLANK
REPLACE NAME WITH "Эл. компоненты в ассортименте"
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Create OF (menu_name) PROMPT "Создать счет" KEY Ctrl-A, "Ctrl-W"
ON SELECTION PAD Create OF (menu_name) DO Sw_Mode WITH "Create"

IF .NOT. EMPTY(c_code)
  DEFINE PAD Info OF (menu_name) PROMPT "Справка по клиенту"
  ON PAD Info OF (menu_name) ACTIVATE POPUP Info

    DEFINE POPUP Info MARGIN RELATIVE SHADOW COLOR SCHEME 4

    DEFINE BAR  1 OF Info PROMPT "Депозит" KEY Ctrl-A, "F2"
    ON SELECTION BAR 1 OF Info DO Sw_Mode WITH "ShowDep"

    DEFINE BAR  2 OF Info PROMPT "История" KEY Ctrl-A, "F3"
    ON SELECTION BAR 2 OF Info DO Sw_Mode WITH "ShowHist"

  IF .NOT. EMPTY(l_num)
    DEFINE PAD Calc OF (menu_name) PROMPT "Калькуляция" KEY Ctrl-A, "F1"
    ON SELECTION PAD Calc OF (menu_name) DO Sw_Mode WITH "Calc"
  ENDIF

ENDIF


DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, k0, term
k0 = 1+w_nds/100
*┌Наименование товара────────────Кол-во───Цена──Сумма без НДС──Сумма с НДС┐
*│999999999999999999999999999999 999999 99999.99 999999999.99 999999999.99│
*└────────────────────────────────────────────────────────────────────────┘
*┌Наименование товара────────────Кол-во───Цена─────────Сумма─┐
*│999999999999999999999999999999 999999 99999.99 999999999.99│
*└───────────────────────────────────────────────────────────┘
ln = WROWS("")-13   && Количество видимых строк BROWSE
wd = IIF(w_nds = 0, 59, 72)

DO D_Wins WITH ln, wd, "Заготовка счета", 0, 0
IF w_nds # 0
  @ 1, 2 SAY "Наименование товара────────────Кол-во───Цена──Сумма без НДС──Сумма с НДС"
ELSE
  @ 1, 2 SAY "Наименование товара────────────Кол-во───Цена─────────Сумма"
ENDIF
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - меню
*
    ON KEY LABEL F1 DO Sw_Mode WITH "Calc"
    ON KEY LABEL F2 DO Sw_Mode WITH "ShowDep"
    ON KEY LABEL F3 DO Sw_Mode WITH "ShowHist"
    IF w_nds = 0
      BROWSE FIELDS NAME:H="",    ;
                    QNT:H="":P="@Z 999999":V=S_Sum(1),  ;
                    PRICE:H="":P="@Z 99999.99":V=S_Sum(2):F, ;
                    SUM_0:H="":P="@Z 999999999.99":W=PRICE*QNT=0:V=S_Sum(3):F ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS NAME:H="",    ;
                    QNT:H="":P="@Z 999999":V=S_Sum(1),  ;
                    PRICE:H="":P="@Z 99999.99":V=S_Sum(2):F, ;
                    SUM_0:H="":P="@Z 999999999.99":W=PRICE*QNT=0:V=S_Sum(3):F,;
                    SUM_T:H="":P="@Z 999999999.99":W=.F. ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Create")
    ENDIF


  CASE what_do = "ShowDep"

    DO Cus_Info WITH c_code
    what_do = "List"
    
  CASE what_do = "ShowHist"

    DO Acc_Hist WITH c_code
    what_do = "List"
    
  CASE what_do = "Calc"

    DO Calc
    what_do = "List"
    
  CASE what_do = "Create"

    term = .T.
    EXIT

  OTHERWISE

    term = .F.
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
IF term
  SET FILTER TO .NOT. EMPTY(NAME) .OR. ;
                .NOT. EMPTY(QNT) .OR.  ;
                .NOT. EMPTY(PRICE) .OR.;
                .NOT. EMPTY(SUM_0) .OR.;
                .NOT. EMPTY(SUM_T)
  PRIVATE s0, s1, d_n, d_d
  SUM SUM_0, SUM_T TO s0, s1
  d_d = DATE()
  d_n = Doc_Num("BILLS", f_c000, arm, d_d)
  d_n = sys_char+CHRTRAN(arm," ","0")+STR(d_n,6)

  SELECT 0
  USE (base_path+"BILLS") ORDER TAG DOC_NUM ALIAS BL_9118 AGAIN
  IF .NOT. EMPTY(l_num)
    = SEEK(l_num, "LT_9118")
    IF .NOT. EMPTY(LT_9118.BILL_NUM)
      IF SEEK(LT_9118.BILL_FIRM+LT_9118.BILL_YEAR+LT_9118.BILL_NUM)
        REPLACE OLD WITH .T.
      ENDIF
    ENDIF
    SELECT LT_9118
    REPLACE LT_9118.BILL_FIRM WITH f_c000, ;
            LT_9118.BILL_NUM  WITH d_n,    ;
            LT_9118.BILL_YEAR WITH STR(YEAR(d_d),4)
  ENDIF
  SELECT BL_9118
  GO TOP
  DO WHILE .NOT. LOCK()
  ENDDO
  IF .NOT. EMPTY(DOC_NUM) .OR. BOF() .OR. EOF()
    UNLOCK
*    DO WHILE .NOT. FLOCK()
*    ENDDO
    APPEND BLANK
  ENDIF
  REPLACE FIRM     WITH f_c000, ;
          DOC_NUM  WITH d_n,    ;
          DOC_DATE WITH d_d,    ;
          DOC_TIME WITH TIME(), ;
          CUS_CODE WITH c_code, ;
          CUS_NAME WITH c_name, ;
          BANK     WITH b_c000, ;
          VALUTA   WITH IIF(vlt_tmp = 1, "Р", "$"),;
          NDS_     WITH w_nds,  ;
          SUM_0    WITH s0,     ;
          BILL_SUM WITH s1,     ;
          NUM_LIST WITH IIF(.NOT. EMPTY(l_num), l_num, 0),  ;
          WHERE    WITH arm,    ;
          WHO      WITH user
  UNLOCK
  USE (base_path+"BILLS_D") ORDER TAG DOC_NUM ALIAS BD_9118 AGAIN
  SELECT CR_BILL0
  SCAN
    SELECT BD_9118
    GO TOP
    DO WHILE .NOT. LOCK()
    ENDDO
    IF .NOT. EMPTY(DOC_NUM) .OR. BOF() .OR. EOF()
      UNLOCK
      DO WHILE .NOT. FLOCK()
      ENDDO
      APPEND BLANK
    ENDIF
    REPLACE FIRM     WITH f_c000,  ;
            DOC_NUM  WITH d_n,     ;
            DOC_DATE WITH d_d,     ;
            NAME     WITH CR_BILL0.NAME, ;
            QNT      WITH CR_BILL0.QNT,  ;
            PRICE_0  WITH CR_BILL0.PRICE,;
            SUM_0    WITH CR_BILL0.SUM_0, ;
            SUM_T    WITH CR_BILL0.SUM_T
    UNLOCK
    SELECT CR_BILL0
  ENDSCAN
  SELECT BD_9118
  USE
  SELECT CR_BILL0
  PRIVATE mss_0
  DIMENSION mss_0(3)
  mss_0(1) = ""
  mss_0(2) = CHR(0)+"  Создан счет N "+LEFT(d_n,4)+"-"+ALLTRIM(RIGHT(d_n,6))+  ;
            "("+f_c000+"). Печатаем? [ Да ] [ Нет ]"
  mss_0(3) = ""
  IF Out_Mess(5,"mss_0") = 1
    DO P_Bill WITH f_c000, d_n, d_d
  ENDIF
ENDIF
USE
DELETE FILE (tmpo_path+"CR_BILL0.DBF")

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура S_Sum        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│               Подпрограмма выхода из ВСЕХ полей BROWSE.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.01.1999 ──┘
PROCEDURE S_Sum
PARAMETERS f_numb

DO CASE
CASE f_numb = 1
  IF QNT*PRICE # 0
    REPLACE SUM_0 WITH QNT*PRICE, ;
            SUM_T WITH QNT*PRICE*k0
  ENDIF
CASE f_numb = 2
  IF QNT*PRICE # 0
    REPLACE SUM_0 WITH QNT*PRICE, ;
            SUM_T WITH QNT*PRICE*k0
  ENDIF
  IF QNT*PRICE # 0 .AND. READKEY() % 256 = 15
    IF RECNO() = RECCOUNT()
      APPEND BLANK
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH
    KEYBOARD "{DnArrow}{Tab}"
  ENDIF
CASE f_numb = 3
  REPLACE SUM_T WITH SUM_0*k0
  IF READKEY() % 256 = 15
    IF RECNO() = RECCOUNT()
      APPEND BLANK
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH
    KEYBOARD "{DnArrow}"
  ENDIF
ENDCASE

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Blnk     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Проверка бланка.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.01.1999 ──┘
PROCEDURE Tst_Blnk
PRIVATE mss
IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали предприятие!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(b_c000) .AND. EMPTY(tmpType)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали банк!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF new_c .AND. EMPTY(c_name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали имя клиента!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Calc         Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                              Калькуляция                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.04.2001 ──┘
PROCEDURE Calc

PRIVATE s_sav, sum01, sum1, sum02, sum2, tmp_arr

IF EOF() .OR. BOF()
  RETURN
ENDIF
DO Wt_Mess WITH "Рассчет..."
s_sav = SELECT()
SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS CR_1426
IF SEEK(STR(c_code)+f_c000)
  sum2 = IIF(vlt_tmp=1, ACC_SUM, ACC_SUMD)
ELSE
  sum2 = 0
ENDIF

sum1 = 0
USE (base_path+"LIST_DET") ORDER TAG LIST ALIAS LD_1426
SEEK STR(l_num,6)
SCAN REST WHILE LIST_NUM = STR(l_num,6)
  IF vlt_tmp=1
    sum1 = sum1+ROUND(QNT*SAL_PRICE, 2)
  ELSE
    sum1 = sum1+QNT*USD_PRICE
  ENDIF
ENDSCAN
USE
SELECT (s_sav)
sum2 = MAX(sum1-sum2, 0)
sum01 = sum1/(w_nds+100)*100
sum02 = sum2/(w_nds+100)*100

DIMENSION tmp_arr(3)
tmp_arr(1) = "Сумма по списку                 "+TRANSFORM(sum1, IIF(vlt_tmp=1, "999 999 999.99", "99 999 999.9999"))
tmp_arr(2) = "Сумма по списку учетом депозита "+TRANSFORM(sum2, IIF(vlt_tmp=1, "999 999 999.99", "99 999 999.9999"))
tmp_arr(3) = "Отказаться (Esc)"

DO Wt_Mess

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
*│ ┌─ Заголовок рамки ───────────────────────────┐ │
*│ │                                             │ │
*│ └─────────────────────────────────────────────┘ │
*│                                                 │
*│        < OK Ctrl-W > < Отказаться Esc >         │
*└─────────────────────────────────────────────────┘

PRIVATE ex, wd, p
wd = LEN(tmp_arr(1))+4
ex = 1
p = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, wd, "Выберите значение"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO WHILE .T.
  p = MAX(p,1)
  p = MIN(p,3)
  @ 1,1 MENU tmp_arr, 3
  READ MENU TO p
  IF READKEY() % 256 = 15
    EXIT
  ENDIF
  IF READKEY() % 256 = 12
    p = 3
    EXIT
  ENDIF
ENDDO

IF p < 3
  PRIVATE s000, s001
  *
  * Отрабатываем бланк
  SELECT CR_BILL0
  IF p = 1
    REPLACE SUM_0 WITH sum01, ;
            SUM_T WITH sum1
  ELSE
    REPLACE SUM_0 WITH sum02, ;
            SUM_T WITH sum2
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
