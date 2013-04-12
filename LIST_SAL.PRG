*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                     Программа продажи списка.                         ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE List_Sal
PARAMETERS ext_menu, buhg

PRIVATE sel_rem, check_w, ord_n, d_n, db_n, db_t, d_d, md_n, md_t, md_d
PRIVATE s00, s00d, s00_0, face_nam, dep_nam, mss, m_tot, m_tot_d, m_tot_0
PRIVATE numbs, dates, sums, ord_cnt, j001, max_ord
PRIVATE tmpNalog, sale_user, mss_sum

mss_sum = 0

m.sale_user = m.user
max_ord = 0

*IF LIST_TIT.IS_CHECK       && И все-таки НДС существует!
*  PRIVATE nds_flt
*  nds_flt = 0
*ENDIF

DIMENSION numbs(1), sums(1), dates(1)
ord_cnt = 0
STORE 0 TO s00, s00d, s00_0
IF acc_level > 1 .AND. LIST_TIT.WHO_LET = 0 .AND. PRESENT # 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Скидку надо утвердить..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF Saled(LIST_TIT.LST_NUM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Так уже продано..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF EMPTY(LIST_TIT.FIRM)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали предприятие..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

PRIVATE d_lst, d_inv, d_ord, d_bill, d_req, pay_sum, n_prcnt, pay_total
STORE 0 TO pay_sum, n_prcnt, pay_total
STORE .F. TO d_lst, d_inv, d_ord, d_bill, d_req

PUSH KEY CLEAR
sel_rem = SELECT()
IF hard_sale
  SELECT 0
  USE (base_path+"ORD_T") ORDER TAG DOC_NUM
  SEEK STR(LIST_TIT.LST_NUM,6)
  SCAN REST WHILE STR(LIST_TIT.LST_NUM,6) = LIST_NUM
    IF EMPTY(STO_MARK) .OR. EMPTY(WHO)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Не все заявки утверждены!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      USE
      SELECT (sel_rem)
      POP KEY
      RETURN
    ENDIF
  ENDSCAN
  USE
  SELECT (sel_rem)
ENDIF

DEFINE WINDOW Hide_M FROM 0, 0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW Hide_M

d_n = SPACE(10)   && Здесь будет номер накладной,
db_n= SPACE(10)   &&   номер счета-фактуры,
db_t= " "         &&    тип счета-фактуры,
d_d = DATE()      &&     и их дата

md_n = LIST_TIT.PAY_NUM  && Здесь будет номер денежного документа и
md_t = LIST_TIT.PAY_TYPE &&      его тип
md_d = LIST_TIT.PAY_DATE &&      и дата

face_nam = SPACE(22) && Через кого
dep_nam  = SPACE(50) && Название предприятия

check_w = 0
ord_n   = LIST_TIT.ORD_NUM+1
IF .NOT. Tst_List()
  RELEASE WINDOW Hide_M
  SELECT (sel_rem)
  POP KEY
  RETURN
ENDIF
PRIVATE sal_type, p_nav
*
*   Определяем тип расчета по умолчанию:
*                   - "3" - нет клиента;
*                   - "1" - Б/Н + доплата;
*                   - "2" - USD.
*

IF EMPTY(LIST_TIT.CLI_CODE)
  sal_type = "3"
ELSE
*  IF FOUND("BILLS") .AND. ALLTRIM(BILLS.STATUS) = "Оплачено"
    sal_type = "1"
*  ELSE
*    sal_type = "2"
*  ENDIF
ENDIF

IF .NOT. buhg
  DO Sal_Flg
ENDIF
p_nav = "form"
DO WHILE .T.
  DO CASE
  CASE p_nav = "form"       && Продажа
    DO CASE
    CASE sal_type = "1"     && Депозит тип 1
      IF buhg
        DO V_Buhg WITH IIF(LIST_TIT.IS_CHECK, 4, 1), LIST_TIT.LST_NUM
        p_nav = "abort"
      ELSE
        p_nav = Sale_(1)
      ENDIF
    CASE sal_type = "2"     && USD
      IF buhg
        DO V_Buhg WITH 3, LIST_TIT.LST_NUM
        p_nav = "abort"
      ELSE
        p_nav = Sale_(2)
      ENDIF
    CASE sal_type = "3"     && Нет клиента
      IF buhg
        DO V_Buhg WITH IIF(LIST_TIT.IS_CHECK, 4, 1), LIST_TIT.LST_NUM, IIF(LIST_TIT.IS_CHECK, 4, 1)
        p_nav = "abort"
      ELSE
        p_nav = Sale_3()
      ENDIF
    ENDCASE
  CASE p_nav = "switch"     && Смена типа депозита
*    sal_type = IIF(sal_type = "1", "2", "1")
*    sal_type = "1"
    p_nav = "form"
  OTHERWISE
    EXIT                    && Продали или отказались от продажи
  ENDCASE

ENDDO

IF m.p_nav # "abort"
  IF LIST_TIT.WHO_CR # m.sale_user
    m.sale_user = LIST_TIT.WHO_CR
    IF .NOT. Get_User()
      m.p_nav = "abort"
    ENDIF
  ENDIF
ENDIF

IF p_nav # "abort"     && Продаем...

  m_tot = 0
  m_tot_d = 0
  m_tot_0 = 0
  DO Last_Ord          && Дозаказ
  IF Money_Doc()       && Денежный документ
    DO Inv_Prp         && Накладная
    DO Clr_Kop
* Документы

    IF d_bill            && Печать счета-фактуры или...
      DO P_Bill_F WITH LIST_TIT.FIRM, db_n, d_d, db_t, (Sale_Mode = 'MEGA') && NEW 02.04.2002
*      DO P_Bill_F WITH LIST_TIT.FIRM, db_n, d_d, db_t && OLD
    ENDIF

    IF d_inv             && Печать накладной
      DO Prn_Inv WITH LIST_TIT.FIRM, d_n, d_d	&&, .F., (Sale_Mode = 'MEGA') && NEW 02.04.2002
*      DO Prn_Inv WITH LIST_TIT.FIRM, d_n, d_d   &&  OLD
    ENDIF


    IF md_t $ " " .AND. ord_cnt > 0 .AND. pay_total # 0
      FOR j00 = 1 TO ord_cnt
        DO BigOrd WITH LIST_TIT.FIRM, numbs(j00), dates(j00)
      ENDFOR
      DO ViewBig WITH max_ord
      SELECT BIGORD
      SCAN
        dates(RECNO()) = DOC_DATE
      ENDSCAN
      USE
      DELETE FILE (tmpo_path+"BIGORD.DBF")
      SELECT BIGDOC
      USE
      SELECT CUS_SUM
      USE
    ENDIF

    IF d_ord             && Печать приходного ордера или...
      FOR j001 = 1 TO ord_cnt
        DO P_Money WITH LIST_TIT.FIRM, md_t, numbs(j001), dates(j001)
      ENDFOR
    ENDIF

  ELSE
    PRIVATE mss000
    DIMENSION mss000(3)
    mss000(1) = ""
    mss000(2) = CHR(0)+"Продажа не состоялась!"
    mss000(3) = ""
  ENDIF
  IF d_req             && Печать требования на склад
    DO P_Req WITH LIST_TIT.FIRM, d_n, d_d
  ENDIF

ENDIF

RELEASE WINDOW Hide_M
SELECT (sel_rem)
POP KEY
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                     Программа проверки списка.                        ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Tst_List

PRIVATE k00, r00, mss, spec_w, w, c1, c2, empt_pr, pr_d, pr_r, p_tp, prv_r
PRIVATE st00r, st00d, tmpPrm, ex
DIMENSION mss(3)
IF LIST_TIT.IS_CHECK    && И все-таки чек существует!
*  PRIVATE nds_flt
*  nds_flt = 0
ENDIF

*
*     Заготовка заявки
*
DELETE FILE (tmpo_path+"REQ_TMP.DBF")
IF .NOT. buhg .AND. .NOT. hard_sale
  SELECT 0
  USE (base_path+"ORD_D")
  COPY STRUCTURE TO (tmpo_path+"REQ_TMP")
  USE (tmpo_path+"REQ_TMP") EXCLUSIVE
ENDIF

*
*     Заготовка накладной
*
IF .NOT. buhg
  DELETE FILE(tmpo_path+"SAL_TMP.DBF")
  SELECT 0
  USE (base_path+"SALE")
  COPY STRUCTURE TO (tmpo_path+"SAL_TMP")
  USE (tmpo_path+"SAL_TMP") EXCLUSIVE
ENDIF

s00 = 0
s00_0 = 0
s00d = 0
st00d = 0
st00r = 0
k00 = STR(LIST_TIT.LST_NUM,6)
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO000 AGAIN
SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_NAME ALIAS LIST_TMP AGAIN

IF .NOT. SEEK(k00)
  mss(1) = ""
  mss(2) = CHR(0)+"Список пуст! Нечего продавать!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT STO000
  USE
  IF USED("SAL_TMP")
    SELECT SAL_TMP
    USE
  ENDIF
  IF USED("REQ_TMP")
    SELECT REQ_TMP
    USE
  ENDIF
  RETURN .F.
ENDIF

r00 = .T.
spec_w = .F.
c1 = 0
c2 = 0
*
*   Пааашли по списку
*
empt_pr = .F.
prv_r = -1
SCAN REST WHILE LIST_NUM = k00
  IF CODE < 0 .OR. QNT = 0 .AND. QNT_REQ = 0 .AND. QNT_DUE = 0
    LOOP
  ENDIF
  IF CODE = 0 .OR. .NOT. SEEK(CODE,"STO000")  && Чужая!
    r00 = .F.
    mss(1) = ""
    mss(2) = CHR(0)+"Прежде чем продавать, избавьтесь от нескладских позиций!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    EXIT
  ENDIF
  IF QNT_REZ # 0
    r00 = .F.
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Прежде чем продавать, избавьтесь резерва!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    EXIT
  ENDIF
  IF prv_r = RECNO()
    r00 = .F.
    mss(1) = ""
    mss(2) = CHR(0)+"ВНИМАНИЕ! позиция "+ALLTRIM(STO000.PREFIX)+" "+ ;
                                         ALLTRIM(STO000.NAME)+" "+ ;
                                         ALLTRIM(STO000.PRODUCER)+" сдвоилась!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    EXIT
  ENDIF
  prv_r = RECNO()
  IF QNT_DUE # 0
    r00 = .F.
    mss(1) = ""
    mss(2) = CHR(0)+"Прежде чем продавать, избавьтесь от долга!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    EXIT
  ENDIF
  w = QNT-QNT_REQ      && Дозаказать!
  IF w # 0
    IF w > 0 .AND. STO000.QNT < w && .AND. EMPTY(IS_DEF) Никакой продажи в минус!
      r00 = .F.
      mss(1) = ""
      mss(2) = CHR(0)+"Прежде чем продавать, избавьтесь от дефицита!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      EXIT
    ENDIF
    IF hard_sale
      r00 = .F.
      mss(1) = ""
      mss(2) = CHR(0)+"Список не полностью заказан!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      EXIT
    ENDIF
    IF USED("REQ_TMP")
      SELECT REQ_TMP
      APPEND BLANK
      REPLACE LIST_NUM WITH k00,     ;
              ORD_NUM  WITH ord_n,   ;
              CODE     WITH LIST_TMP.CODE, ;
              QNT      WITH w
      c1 = c1+CODE
      c2 = c2+QNT
      SELECT LIST_TMP
    ENDIF
  ENDIF
  IF LIST_TIT.IS_USD
    IF USD_PRICE # USD_DEF
      spec_w = .T.
    ENDIF
  ELSE
    IF SAL_PRICE # Round_6(DEF_PRICE)
      spec_w = .T.
    ENDIF
  ENDIF
  IF IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE) <= 0
    empt_pr = .T.
  ENDIF
  IF LIST_TMP.QNT # 0
    IF LIST_TIT.IS_USD
      pr_r = Clc_Rub(LIST_TMP.USD_PRICE)
      pr_d = LIST_TMP.USD_PRICE
      p_tp = IIF(LIST_TMP.USD_PRICE = LIST_TMP.USD_DEF, " ", ;
                            IIF(.NOT. EMPTY(LIST_TMP.IS_SPEC), "S", "D"))
    ELSE
      pr_r = LIST_TMP.SAL_PRICE
      IF EMPTY(LIST_TMP.USD_PRICE)
        pr_d = ROUND(LIST_TMP.SAL_PRICE/kurs,4)
      ELSE
        pr_d = LIST_TMP.USD_PRICE
      ENDIF
      p_tp = IIF(LIST_TMP.SAL_PRICE = ROUND_6(LIST_TMP.DEF_PRICE), " ", ;
                            IIF(.NOT. EMPTY(LIST_TMP.IS_SPEC), "S", "D"))
    ENDIF
    p_tp = IIF(p_tp $ " D" .AND. .NOT. EMPTY(LIST_TMP.IS_OPT), "O", p_tp)
    IF USED("SAL_TMP")
      SELECT SAL_TMP
      APPEND BLANK
      REPLACE FIRM      WITH LIST_TIT.FIRM, ;
              CODE      WITH LIST_TMP.CODE, ;
              QNT       WITH LIST_TMP.QNT,  ;
              PRICE_D_L WITH IIF(own_val = "$", STO000.PRICE, ;
                                                STO000.PRICE_R/kurs), ;
              PRICE_R_L WITH IIF(own_val = "$", Clc_Rub(STO000.PRICE), ;
                                                STO000.PRICE_R), ;
              PRICE_TYPE WITH p_tp, ;
              PRICE_D   WITH pr_d,  ;
              PRICE_0   WITH ROUND(pr_r/(100+nds_flt)*100, 2), ;
              PRICE_R   WITH pr_r,  ;
              WHERE     WITH arm
      s00_0 = s00_0+QNT*PRICE_0
    ENDIF
    st00r = st00r+ROUND(QNT*STO000.PRICE_R, 2)
    st00d = st00d+QNT*STO000.PRICE
    s00   = s00+ROUND(QNT*pr_r, 2)
    s00d  = s00d+QNT*pr_d
  ENDIF
  SELECT LIST_TMP

ENDSCAN

IF empt_pr
  mss(1) = ""
*  mss(2) = CHR(0)+"  Список содержит пустые цены! Продаем? [ Да ] [ Нет ]"
  mss(2) = CHR(0)+"Список содержит пустые цены!"
  mss(3) = ""
*  r00 = Out_Mess(7, "mss") = 1
  DO Out_Mess WITH 7, "mss"
  r00 = .F.
ENDIF

tmpPrm = ALLTRIM(Get_Sys("SALE BOUND"))
IF EMPTY(tmpPrm)
  price_age = 0
  sumBound  = 0
ELSE
  ex = AT(" ", tmpPrm)
  sumBound = VAL(SUBSTR(tmpPrm, ex+1))
  price_age = VAL(LEFT(tmpPrm, ex-1))
  sumBound = (100-sumBound)/100
ENDIF

IF price_age # 0 .AND. sumBound # 0 .AND. s00 > 0
  IF (DATE() - LIST_TIT.CR_DAT > price_age) .AND. s00/st00r < sumBound
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Список создан "+DTOC(LIST_TIT.CR_DAT)+", не следует ли обновить цены?"
    mss(3) = CHR(0)+"  [ Следует! ]  [ Продаем! ]"
    mss(4) = ""
    IF Out_Mess(7, "mss") = 1
      r00 = .F.
    ENDIF
  ENDIF
ENDIF

check_w = STR(c1 % 9, 1)+STR(c2 % 9, 1)+ " "
IF spec_w .AND. r00
  IF acc_level > 1 .AND. LIST_TIT.WHO_LET = 0
    r00 = .F.
    mss(1) = ""
    mss(2) = CHR(0)+"Цены надо утвердить..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    IF LIST_TIT.WHO_LET = 0
      REPLACE LIST_TIT.WHO_LET WITH user
    ENDIF
  ENDIF
ENDIF
IF r00
  REPLACE LIST_TIT.R_TOTAL WITH IIF(LIST_TIT.IS_USD, s00d, s00)
ENDIF

USE
SELECT STO000
USE
IF USED("SAL_TMP")
  SELECT SAL_TMP
  USE
ENDIF
IF USED("REQ_TMP")
  SELECT REQ_TMP
  USE
ENDIF

RETURN r00

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║            Программа расчета с клиентом с депозита.                   ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Sale_
PARAMETERS d_type   && 1 - рубли, 2 - USD
PRIVATE l1, ln, wd, deposit, credit, c_nm, get_sum, c_name, inv_price
PRIVATE c_qnt, s_sav, f_name, f_code, pic_w, pic_alt, rest_w, change
DIMENSION c_qnt(9)
c_qnt = 0
pic_w = IIF(d_type = 1, "999 999 999.99", "9 999 999.9999")
pic_alt = IIF(d_type = 1, "9 999 999.9999", "999 999 999.99")

PUSH KEY CLEAR
wd = 50
ln = 19
= SEEK(LIST_TIT.CLI_CODE,"ACCOUNT")
c_nm = ALLTRIM(ACCOUNT.CUS_NAME)

n_prcnt = G_Nalog(LIST_TIT.FIRM)
IF d_type = 1
  IF .NOT. EMPTY(ACCOUNT.ACC_NOTE)
    WAIT ALLTRIM(ACCOUNT.ACC_NOTE) WINDOW NOWAIT
  ENDIF
ELSE
  IF .NOT. EMPTY(ACCOUNT.ACC_NOTED)
    WAIT ALLTRIM(ACCOUNT.ACC_NOTED) WINDOW NOWAIT
  ENDIF
ENDIF
s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS FR_9202 AGAIN
SEEK LIST_TIT.FIRM
f_name = ALLTRIM(B_NAME)
f_code = FIRM_CODE

*         Клиент:
*┌──────────────────────────────────────────────┐
*│ Предприятие ....:....!....:. Оплата в рублях │
*│        Депозит        9 999 999.9999         │
*│        По накладной   999 999 999.99         │
*│                      (999 999 999.99 USD)    │
*│        Остаток        999 999 999.99         │
*│        Кредит         999 999 999.99         │
*│        Оплата (нал.)  ░░░░▓░░░░▓░░░░         │
*│        Налог (nnn.n%) 999 999 999.99         │
*│        Итого          999 999 999.99         │
*│        Получено       ░░░░▓░░░░▓░░░░ (F1)    │
*│        Сдача          999 999 999.99         │
*│    ┌── Документы ───────────────────────┐    │
*│    │ [ ] Накладная     [ ] Счет-фактура │    │
*│    │ [ ] Прих. ордер   [ ] Требование   │    │
*│    └────────────────────────────────────┘    │
*│       < OK Ctrl-W > < Отказаться Esc >       │
*└──────────────────────────────────────────────┘

PRIVATE ex, l_rest, l_chang, nlg_sum
get_sum = 0

USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS CR_9202 AGAIN
inv_price = IIF(d_type = 1, s00, s00d)
IF SEEK(STR(ACCOUNT.CUS_CODE)+LIST_TIT.FIRM)
  IF d_type = 1
     deposit = ACC_SUM
     credit  = CR_SUM
  ELSE
     deposit = ACC_SUMD
     credit  = CRD_SUM
  ENDIF
ELSE
  deposit = 0
  credit  = 0
ENDIF
*IF LIST_TIT.IS_CHECK
*  pay_sum = inv_price
*ELSE
  pay_sum = MAX(0, inv_price-deposit)
*ENDIF
pay_total = ROUND(pay_sum*(100+n_prcnt)/100, IIF(d_type = 1, 2, 3))
nlg_sum = pay_total-pay_sum

rest_w  = deposit-inv_price+pay_sum
get_sum = 0
change = MAX(0, get_sum-pay_total)

d_lst = LIST_TIT.CLI_TYPE # 1
d_inv = .F.
*d_ord = d_type = 1 .AND. pay_sum > 0
*d_ord = .F.
*d_req = .F.
*d_bill = d_type = 1
*d_bill = .F.

d_inv = mode_d_i
d_ord = mode_d_o
d_req = mode_d_r
*IF LIST_TIT.IS_CHECK    && Всегда формитуем счет-фактуру!
*  d_bill = .F.
*ELSE
  d_bill = mode_d_b
*ENDIF

ex = 1

l1 = 2
DO D_W WITH "Money", ln, wd, c_nm, 1

@  2,  3 SAY "Предприятие "
@  2, COL() SAY f_name COLOR (SCHEME(13,6))
@  2, 32 SAY "Оплата в "+IIF(d_type = 1, "рублях", "USD") COLOR (SCHEME(13,6))
@  3, 10 SAY "Депозит"
@  4, 10 SAY "По накладной"
@  5, 24 SAY IIF(d_type = 1, "(               USD)", "(               руб)")
@  6, 10 SAY "Остаток"
@  7, 10 SAY "Кредит"
@  8, 10 SAY "Оплата (нал.)"
@  9, 10 SAY "Налог "+ALLTRIM(STR(n_prcnt,5,1))
@ 10, 10 SAY "Итого"
@ 11, 10 SAY "Получено                 (F1)"
@ 12, 10 SAY "Сдача"
@  3, 25 SAY deposit PICTURE pic_w COLOR (SCHEME(13,6))
@  4, 25 SAY inv_price PICTURE pic_w COLOR (SCHEME(13,6))
@  5, 25 SAY IIF(d_type = 1, s00d, s00) PICTURE pic_alt COLOR (SCHEME(13,6))
@  6, 25 GET rest_w PICTURE pic_w COLOR (","+SCHEME(13,6)) WHEN .F.
@  7, 25 SAY credit PICTURE pic_w COLOR (SCHEME(13,6))
@  8, 25 GET pay_sum PICTURE pic_w VALID Sh0_Rest() && WHEN .NOT. LIST_TIT.IS_CHECK
@  9, 25 GET nlg_sum PICTURE pic_w COLOR (","+SCHEME(13,6)) WHEN .F.
@ 10, 25 GET pay_total PICTURE pic_w COLOR (","+SCHEME(13,6)) WHEN .F.
@ 11, 25 GET get_sum PICTURE "@Z "+pic_w WHEN Enter_M() VALID Exit_M()
@ 12, 25 GET change PICTURE pic_w COLOR (","+SCHEME(13,6)) WHEN .F.
@ 13, 6 TO 16, 43
@ 13, 9 SAY " Документы "
@ 14,  8 GET d_inv  PICTURE "@*C Накладная"
@ 14, 26 GET d_bill PICTURE "@*C Счет-фактура" && WHEN .NOT. LIST_TIT.IS_CHECK
@ 15,  8 GET d_ord  PICTURE "@*C "+IIF(LIST_TIT.IS_CHECK, "Чек", "Прих. ордер") WHEN pay_sum # 0
@ 15, 26 GET d_req  PICTURE "@*C Требование"
@ 17, 9 GET ex     PICTURE "@*HT \ OK Ctrl-W ;\? Отказаться Esc "

IF sale_mode = "Mitracon"
  IF .NOT. EMPTY(deposit)
    PRIVATE mss_999
    DIMENSION mss_999(3)
    mss_999(1) = ""
    mss_999(2) = CHR(0)+"ВНИМАНИЕ! Непустой депозит!"
    mss_999(3) = ""
    DO Out_Mess WITH 7, "mss_999"
    RELEASE mss_999
  ENDIF
ENDIF

READ CYCLE VALID Sh_Rest()
IF READKEY() % 256 = 12
  ex = 2
ENDIF
DO CASE
CASE ex = 1
  ex = IIF(d_type = 1, "1", "2")+"sale"
CASE ex = 2
  ex = "abort"
ENDCASE

SELECT CR_9202
USE

SELECT (s_sav)

POP KEY
RELEASE WINDOW MONEY

RETURN ex

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║            Программа расчета с клиентом через кассу.                  ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Sale_3
PRIVATE l1, ln, wd, deposit, credit, c_nm, get_sum, c_name
PRIVATE c_qnt, s_sav, f_code, f_name, change
DIMENSION c_qnt(9)
c_qnt = 0

PUSH KEY CLEAR
wd = 58
ln = 14
c_nm = LIST_TIT.LST_NAME

s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS FR_9202 AGAIN
SEEK LIST_TIT.FIRM
f_name = ALLTRIM(B_NAME)
f_code = FIRM_CODE
USE
SELECT (s_sav)

*         Клиент:
*┌──────────────────────────────────────────────────────┐
*│     Покупатель ....:....!....:....!....:....!..      │
*│            По накладной   999 999 999.99             │
*│            Налог (nnn.n%) 999 999 999.99             │
*│            Итого          999 999 999.99             │
*│            Получено       ░░░░▓░░░░▓░░░░ (F1)        │
*│            Сдача          999 999 999.99             │
*│ ┌── Документы ─────────────────────────────────────┐ │
*│ │       [ ] Накладная       [ ] Счет-фактура       │ │
*│ │       [ ] Счет-фактура    [ ] Требование         │ │
*│ └──────────────────────────────────────────────────┘ │                                        │
*│           < OK Ctrl-W > < Отказаться Esc >           │
*└──────────────────────────────────────────────────────┘

PRIVATE ex, l_rest, l_chang, nlg_sum
get_sum = 0
pay_sum = s00
n_prcnt = G_Nalog(LIST_TIT.FIRM)
pay_total = ROUND(pay_sum*(100+n_prcnt)/100, 2)
nlg_sum = pay_total-pay_sum
change = MAX(0, get_sum-pay_total)

d_lst = LIST_TIT.CLI_TYPE # 1
d_inv = .F.
d_ord = .F.
d_bill = .F.
d_inv = mode_d_i
d_ord = mode_d_o
d_req = mode_d_r
*IF LIST_TIT.IS_CHECK
*  d_bill = .F.
*ELSE
  d_bill = mode_d_b
*ENDIF
ex = 1

l1 = 3
DO D_W WITH "Money", ln, wd, "Предприятие: "+f_name, 1

wd = 14
@ l1, wd SAY "По накладной  "+TRANSFORM(s00,"999 999 999.99")
l1 = l1+1
@ l1, wd SAY "Налог "+ALLTRIM(STR(n_prcnt,5,1))
@ l1, 28 SAY nlg_sum PICTURE "999 999 999.99"
l1 = l1+1
@ L1, WD SAY "Итого"
@ l1, 28 SAY pay_total PICTURE "999 999 999.99"
l1 = l1+1
@ l1, wd SAY "Получено     " GET get_sum PICTURE "@Z 999 999 999.99"  ;
             WHEN Enter_M() VALID Exit_M()
@ l1, wd+29 SAY "(F1)"
l1 = l1+1
@ l1, wd SAY "Сдача        " GET change PICTURE "@Z 999 999 999.99" ;
           COLOR (","+SCHEME(13,1)) WHEN .F.
l_chang = l1
l1 = l1+1
@ l1, 3 TO l1+3, WCOLS()-4
@ l1, 5 SAY " Документы "
@ l1+1,11 GET d_inv PICTURE "@*C Накладная"
@ l1+1,31 GET d_bill PICTURE "@*C Счет-фактура"
@ l1+2,11 GET d_ord PICTURE "@*C "+IIF(LIST_TIT.IS_CHECK, "Чек", "Прих. ордер") WHEN pay_sum # 0
@ l1+2,31 GET d_req PICTURE "@*C Требование"
@ l1+4,13 GET ex    PICTURE "@*HT \ OK Ctrl-W ;\? Отказаться Esc "
@ 2, 7 SAY "Покупатель" GET LIST_TIT.LST_NAME
READ CYCLE
IF READKEY() % 256 = 12
  ex = 2
ENDIF
DO CASE
CASE ex = 1
  ex = "10sale"
OTHERWISE
  ex = "abort"
ENDCASE

POP KEY
RELEASE WINDOW MONEY

RETURN ex

************************************************************************
PROCEDURE Sh0_Rest
PRIVATE mss
IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

rest_w  = deposit-inv_price+pay_sum
pay_total = ROUND(pay_sum*(100+n_prcnt)/100, IIF(d_type = 1, 2, 3))
nlg_sum = pay_total-pay_sum
SHOW GET rest_w
SHOW GET pay_total
SHOW GET nlg_sum

IF pay_sum = 0
  d_ord = .F.
  SHOW GET d_ord
ENDIF
IF d_type = 1 .AND. pay_sum >= 2000000
  WAIT WINDOW "ВНИМАНИЕ! Ну очень большая сумма!" NOWAIT
ENDIF

RETURN .T.

************************************************************************
PROCEDURE Sh_Rest
PRIVATE mss
IF READKEY() % 256 = 12 .OR. ex = 3 .OR. ex = 2
  RETURN .T.
ENDIF
DO CASE
CASE deposit-s00+credit+pay_sum < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет, так мы не расходимся! Маловато будет!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
CASE deposit-s00+pay_sum < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" Продаем в кредит? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN .F.
  ENDIF
ENDCASE

RETURN .T.

************************************************************************
PROCEDURE Enter_M
ON KEY LABEL F1 DO Clc_Cup
RETURN .T.

************************************************************************
PROCEDURE Exit_M
PRIVATE rt

IF READKEY() % 256 = 12
  ON KEY LABEL F1
  RETURN .T.
ENDIF

IF get_sum = 0
  get_sum = pay_total
ENDIF

IF get_sum < pay_total
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Маловато будет!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
IF TYPE("d_type") = "U"
  pay_total = ROUND(pay_sum*(100+n_prcnt)/100, 2)
ELSE
  pay_total = ROUND(pay_sum*(100+n_prcnt)/100, IIF(d_type = 1, 2, 3))
ENDIF
change = get_sum-pay_total
SHOW GET change
ON KEY LABEL F1
RETURN .T.

************************************************************************
PROCEDURE Clc_Cup

get_sum = Cupury(" ", c_qnt, 0, -23)
SHOW GET get_sum
KEYBOARD CHR(13)
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                            Программа дозаказа.                        ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Last_Ord
PRIVATE sel_rem, k00, l_ord, cnt_pos, cnt_neg

sel_rem = SELECT()
STORE 0 TO cnt_pos, cnt_neg
IF .NOT. File_O(tmpo_path+"REQ_TMP.DBF")
  RETURN
ENDIF
k00 = STR(LIST_TIT.LST_NUM,6)
SELECT 0
USE (tmpo_path+"REQ_TMP")
l_ord = RECCOUNT()
IF l_ord = 0
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
SCAN
  IF QNT < 0
    cnt_neg = cnt_neg+1
  ELSE
    cnt_pos = cnt_pos+1
  ENDIF
ENDSCAN

SELECT LIST_TIT
REPLACE LIST_TIT.ORD_NUM WITH LIST_TIT.ORD_NUM+1
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_ORD AGAIN
SELECT REQ_TMP
SCAN
  IF .NOT. SEEK(CODE,"STO_ORD")
    LOOP
  ENDIF
  SELECT STO_ORD
  REPLACE QNT_SAVE WITH QNT_SAVE+REQ_TMP.QNT,  ;
          QNT      WITH QNT-REQ_TMP.QNT
  SELECT REQ_TMP
ENDSCAN
USE
SELECT STO_ORD

USE (base_path+"ORD_T")
APPEND BLANK
REPLACE LIST_NUM  WITH k00,  ;
        LIST_NAME WITH LIST_TIT.LST_NAME, ;
        ORD_NUM   WITH LIST_TIT.ORD_NUM,  ;
        ORD_DATE  WITH d_d,   ;
        ORD_TIME  WITH TIME(),;
        CHECK_S   WITH "   ", ;
        SIZE_NEG  WITH cnt_neg, ;
        SIZE_POS  WITH cnt_pos, ;
        QUEUE     WITH LIST_TIT.ORD_QUEUE, ;
        WHO       WITH user,  ;
        WHERE     WITH arm

USE (base_path+"ORD_D")
APPEND FROM (tmpo_path+"REQ_TMP")

USE (base_path+"LIST_DET") ORDER TAG L_NAME ALIAS LIST_000 AGAIN

SEEK k00
REPLACE REST WHILE LIST_NUM = k00 QNT_REQ WITH QNT
PRIVATE mss
DIMENSION mss(4)
mss(1) = ""
mss(2) = CHR(0)+"ВНИМАНИЕ! Вы не полностью расчитались со складом по этому списку."
mss(3) = CHR(0)+"Сформирована заявка N "+ALLTRIM(STR(LIST_TIT.LST_NUM,10))+"/"+   ;
                                        +ALLTRIM(STR(LIST_TIT.ORD_NUM,10))
mss(4) = ""
DO Out_Mess WITH 7, "mss"
USE

SELECT (sel_rem)

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║              Программа формирования денежного документа.              ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Money_Doc
PRIVATE sel_sav, sum_w, rst, rst_0, rst_n, j, nds_w, prc_w, s_sav
PRIVATE chk_num, chk_date, chk_time, chk_err, mss, is_kkm, tmpNum

chk_num  = "         "
chk_date = DATE()
chk_time = SECONDS()
chk_err  = 0

IF pay_total = 0     && не было денег
  RETURN .T.
ENDIF

prc_w = G_Nalog(LIST_TIT.FIRM)  && % налога с продаж
IF LEFT(p_nav,1) = "1"   && Натуральный прих. ордер
  md_t = " "
  nds_w = nds_flt  && % НДС
ELSE                      && Приход USD
  md_t = "3"
  nds_w = 0        && % НДС
ENDIF
IF .NOT. LetMoney(LIST_TIT.FIRM, md_t)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Приход наличных недопустим для данного предприятия!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
rst   = pay_sum
rst_0 = IIF(pay_sum = s00, s00_0, ROUND(pay_sum/(100+nds_w)*100, 2) )
rst_n = pay_total-pay_sum

IF .NOT. LIST_TIT.IS_CHECK
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9B23 AGAIN
  SEEK LIST_TIT.FIRM
  max_ord = IIF(FSIZE("M_SUM")=0,0,M_SUM)
  USE
  SELECT (s_sav)
ELSE
  md_t = "Ч"
  max_ord = 0
  is_kkm = FirmKKM(LIST_TIT.FIRM)
  IF EMPTY(is_kkm)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"На этом предприятии нет кассового аппарата!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  is_kkm = VAL(ALLTRIM(is_kkm))
  IF File_O(base_path+"TO_KKM.DBF") .AND. is_kkm # 0
    DO Do_Check WITH rst_0, rst-rst_0, rst_n, chk_num, chk_date, chk_time, chk_err
    IF .NOT. EMPTY(chk_err)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Ошибка "+ALLTRIM(STR(chk_err))+" при попытке пробить чек"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

ord_cnt  = 0
IF rst < max_ord .OR. max_ord = 0
  DIMENSION numbs(1), sums(1,3), dates(1)
  ord_cnt  = 1
  numbs(1) = 0
  sums(1,1)  = rst
  sums(1,2)  = rst_0
  sums(1,3)  = rst_n
  dates(1)   = DATE()
ELSE
  DO WHILE rst > 0
    IF rst < max_ord
      sum_w = rst
    ELSE
      sum_w = max_ord-ROUND(0.1*RAND()*max_ord, 0)
    ENDIF
    ord_cnt = ord_cnt+1
    rst = rst-sum_w
    DIMENSION numbs(ord_cnt), sums(ord_cnt,3), dates(ord_cnt)
    numbs(ord_cnt) = 0
    dates(ord_cnt) = DATE()+ord_cnt-1
    sums(ord_cnt,1)  = sum_w
    IF rst # 0
      sums(ord_cnt,2)  = ROUND(sum_w/(100+nds_w)*100, 2)
      rst_0 = rst_0-sums(ord_cnt,2)
      sums(ord_cnt,3)  = ROUND(sum_w*prc_w/100, 2)
      rst_n = rst_n-sums(ord_cnt,3)
    ELSE
      sums(ord_cnt,2)  = rst_0
      sums(ord_cnt,3)  = rst_n
    ENDIF
  ENDDO
ENDIF

sel_sav = SELECT()
USE (base_path+"INCMONEY")
FOR j = 1 TO ord_cnt
  IF LIST_TIT.IS_CHECK
    IF EMPTY(chk_num)
      md_n = Doc_Num("CHECK", LIST_TIT.FIRM, arm, d_d)
    ELSE
      md_n = chk_num
    ENDIF
  ELSE
    IF sums(j,1) > 0
      md_n = Doc_Num(IIF(md_t = " ", "INC_ORDER", "MONEY_$"), LIST_TIT.FIRM, arm, d_d)
    ELSE
      md_n = Doc_Num(IIF(md_t = " ", "EXP_ORDER", "MONEY_$"), LIST_TIT.FIRM, arm, d_d)
      IF md_t = " "
        md_n = -md_n
      ENDIF
    ENDIF
  ENDIF
  IF TYPE("md_n") = "N"
    md_n = sys_char+STR(md_n,FSIZE("DOC_NUM")-1)
  ELSE
    md_n = sys_char+PADL(md_n,9)
  ENDIF
  APPEND BLANK
  REPLACE FIRM      WITH LIST_TIT.FIRM, ;
          DOC_TYPE  WITH IIF(LIST_TIT.IS_CHECK, "Ч", md_t), ;
          DOC_NUM   WITH md_n,    ;
          DOC_DATE  WITH chk_date-1+j,;
          DOC_TIME  WITH chk_time,;
          SALE_NUM  WITH d_n,     ;
          SALE_DATE WITH d_d,     ;
          ENTER_DAT WITH chk_date-1+j,;
          CUS_CODE  WITH LIST_TIT.CLI_CODE, ;
          CUS_NAME  WITH IIF(LIST_TIT.CLI_CODE = 0,  ;
                         LIST_TIT.LST_NAME, ACCOUNT.CUS_NAME), ;
          MONEY_0   WITH sums(j,2), ;
          NDS_      WITH nds_w,   ;
          MONEY_R   WITH IIF(md_t $ "Ч ",sums(j,1),sums(j,1)*kurs), ;
          MONEY_D   WITH IIF(md_t $ "Ч ", sums(j,1)/kurs, sums(j,1)), ;
          NL_PRC    WITH prc_w,   ;
          N_OF_SALE WITH sums(j,3), ;
          WHO       WITH user,  ;
          WHERE     WITH arm
  md_d = DOC_DATE
  IF DOC_TYPE = "Ч"
    mss_sum = MONEY_R
  ENDIF
  IF DOC_TYPE = "Ч" .AND. Is_Ur(CUS_CODE) .AND. YEAR(DOC_DATE) < 2006
    IF MONEY_R > 0 .OR. MONEY_D > 0
      tmpNum = Doc_Num("INC_ORDER", FIRM, arm, LEFT(DTOS(DATE()),4))
    ELSE
      tmpNum = Doc_Num("EXP_ORDER", FIRM, arm, LEFT(DTOS(DATE()),4))
      tmpNum = -tmpNum
    ENDIF
    REPLACE ENTER_DOC WITH CHR(0)+sys_char+STR(tmpNum,9)
  ENDIF
  numbs(j) = md_n
ENDFOR
USE (base_path+"ARM_LST")
GO arm_rec
REPLACE KASSA WITH KASSA+pay_total
USE
IF pay_total # pay_sum
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F00_9203 AGAIN
  IF SEEK(LIST_TIT.FIRM)
    REPLACE NALOG WITH NALOG+pay_total-pay_sum
  ENDIF
ENDIF
IF LIST_TIT.CLI_CODE # 0
  USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS C00_9203
  IF .NOT. SEEK(STR(LIST_TIT.CLI_CODE)+LIST_TIT.FIRM)
    APPEND BLANK
    REPLACE FIRM     WITH LIST_TIT.FIRM, ;
            CUS_CODE WITH LIST_TIT.CLI_CODE
  ENDIF
  IF md_t $ "Ч "
    REPLACE ACC_SUM WITH ACC_SUM+pay_sum
  ELSE
    REPLACE ACC_SUMD WITH ACC_SUMD+pay_sum
  ENDIF
ENDIF
USE
DO CheckRem WITH mss_sum

SELECT (sel_sav)

RETURN .T.

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║          Программа формирования накладной ( если требуется ).         ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Inv_Prp

PRIVATE sel_sav, arr_w, q_rep, mss, q_pos, tmpFlag, tmpNalog, q_my

sel_sav = SELECT()

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_000 AGAIN

SELECT 0
USE (base_path+"SALE") ALIAS SAL_000 AGAIN

db_t   = IIF(LEFT(p_nav,1) = "1", " ", "3")   && Тип продажи
SELECT 0

USE (base_path+"ACT_S") ORDER TAG SALE ALIAS INC_SALE AGAIN

SELECT 0
USE (tmpo_path+"SAL_TMP")
*
*   Коррекция брони ( именно брони! ) и формирование строк накладной
*
PRIVATE q_sale, q_rest, key_act
m_tot = 0
m_tot_d = 0
m_tot_0 = 0
SCAN FOR QNT # 0
  IF .NOT. SEEK(CODE, "STO_000")    && Странно, только что было...
    LOOP
  ENDIF
  SCATTER TO arr_w
  q_rest = QNT
  SELECT SAL_000
  q_pos = 0
  key_act = STR(SAL_TMP.CODE,7)+LIST_TIT.FIRM
  DO WHILE q_rest # 0
    IF .NOT. Ch_Part()
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"Внимание! "+ALLTRIM(STO_000.PREFIX)+" "+ALLTRIM(STO_000.NAME)
      mss(3) = CHR(0)+"Не хватает в партиях. Сумма по накладной будет другая!"
      mss(4) = ""
      DO Out_Mess WITH 7, "mss"
      EXIT
    ENDIF
    IF q_rest > 0
      q_my = MAX(INC_SALE.QNT_REST-INC_SALE.REST_REP, 0)
      q_sale = MIN(q_rest, INC_SALE.QNT_REST)
      q_rep = MAX(q_sale-q_my, 0)
    ELSE
      q_sale = q_rest
      IF sale_mode = "Mitracon"
        q_rep  = q_sale
      ELSE
        q_rep = 0
      ENDIF
    ENDIF
    q_rest = q_rest-q_sale
    SELECT STO_000
    REPLACE QNT_SAVE   WITH QNT_SAVE-q_sale, ;
            QNT_REP    WITH QNT_REP-q_rep
    SELECT INC_SALE
    REPLACE QNT_REST  WITH QNT_REST-q_sale, ;
            REST_REP  WITH REST_REP-q_rep,  ;
            LAST_SALE WITH DATE()
    SELECT SAL_000
    IF EMPTY(d_n)
*      IF LIST_TIT.IS_CHECK    && Всегда только накладная!
*        d_n = Doc_Num("G_CHECK", LIST_TIT.FIRM, arm, d_d)
*        d_n = sys_char+"***"+STR(d_n,6)
*      ELSE
        d_n = Doc_Num("SALE", LIST_TIT.FIRM, arm, d_d)
        d_n = sys_char+CHRTRAN(arm," ","0")+STR(d_n,6)
*      ENDIF
    ENDIF
    APPEND FROM ARRAY arr_w
    REPLACE DOC_NUM   WITH d_n,     ;
            DOC_DATE  WITH d_d,     ;
            FIRM      WITH LIST_TIT.FIRM, ;
            QNT_REP   WITH q_rep,   ;
            QNT       WITH q_sale,  ;
            ACT_FIRM  WITH INC_SALE.FIRM,    ;
            ACT_NUM   WITH INC_SALE.DOC_NUM, ;
            ACT_DATE  WITH INC_SALE.DOC_DATE,;
            PARTY     WITH INC_SALE.PARTY,   ;
            GTD_NUM   WITH INC_SALE.GTD_NUM, ;
            COUNTRY   WITH INC_SALE.COUNTRY, ;
            COUNTRYC  WITH INC_SALE.COUNTRYC,;
            GTD_LINE  WITH INC_SALE.GTD_LINE,;
            WHERE     WITH arm
    q_pos = q_pos+QNT
    m_tot = m_tot+ROUND(QNT*PRICE_R,2)
    m_tot_d = m_tot_d+QNT*PRICE_D
    m_tot_0 = m_tot_0+QNT*PRICE_0
  ENDDO
  SELECT SAL_TMP

ENDSCAN

USE
SELECT SAL_000
USE
SELECT INC_SALE
USE
SELECT STO_000

*IF .NOT. LIST_TIT.IS_CHECK
  IF db_t = " "   && Счет-фактура
    db_n = Doc_Num("BILL_F", LIST_TIT.FIRM, arm, d_d)
  ELSE                      && Якобы счет-фактура
    db_n = Doc_Num("BILL_F$", LIST_TIT.FIRM, arm, d_d)
  ENDIF
*ENDIF
USE (base_path+"SALE_TIT")
*IF LIST_TIT.IS_CHECK
*  db_n = ""
*ELSE
  db_n = sys_char+STR(db_n,FSIZE("NEW_DOC")-1)
*ENDIF
IF pay_total # 0
  md_t = IIF(LIST_TIT.IS_CHECK, "Ч", md_t)
ENDIF
tmpFlag = LIST_TIT.LST_FLAGS
tmpNalog = G_Nalog(LIST_TIT.FIRM)
IF LIST_TIT.IS_CHECK
  tmpFlag = STUFF(tmpFlag, 15, 1, "*")
ENDIF
APPEND BLANK
REPLACE FIRM      WITH LIST_TIT.FIRM, ;
        DOC_NUM   WITH d_n,     ;
        DOC_DATE  WITH d_d,     ;
        DOC_TIME  WITH SECONDS(),  ;
        REQ_DOC   WITH LIST_TIT.REQ_DOC,   ;
        REQ_DATE  WITH LIST_TIT.REQ_DATE,  ;
        NEW_DOC   WITH db_n,    ;
        NEW_TYPE  WITH db_t,    ;
        SAL_FLAGS WITH tmpFlag, ;
        CUS_CODE  WITH LIST_TIT.CLI_CODE,  ;
        CUS_NAME  WITH LIST_TIT.LST_NAME,  ;
        LIST_NUM  WITH STR(LIST_TIT.LST_NUM,6),  ;
        LIST_DATE WITH LIST_TIT.CR_DAT,  ;
        PAY_TYPE  WITH md_t,  ;
        PAY_DOC   WITH md_n,  ;
        PAY_DATE  WITH md_d,  ;
        TOTAL_0   WITH m_tot_0, ;
        NDS_      WITH IIF(db_t = " ", nds_flt, 0), ;
        NSP_      WITH tmpNalog, ;
        TOTAL_R   WITH m_tot, ;
        TOTAL_D   WITH m_tot_d, ;
        KASSA     WITH IIF(CUS_CODE = 0, m_tot, MIN(m_tot,pay_sum+LIST_TIT.KASSA)),  ;
        WHO_LET   WITH LIST_TIT.WHO_LET,  ;
        WHO       WITH sale_user,  ;
        WHERE     WITH arm
IF PAY_TYPE = "1" .OR. EMPTY(PAY_DOC)
  DO Rev_Bill WITH "S", RECNO("SALE_TIT")
ENDIF
IF .NOT. EMPTY(md_n)
  USE (base_path+"INCMONEY") ORDER TAG DOC_NUM ALIAS IM_9218 AGAIN
  SEEK LIST_TIT.FIRM+md_t+LEFT(DTOS(d_d),4)+md_n
  REPLACE SALE_NUM  WITH d_n, ;
          SALE_DATE WITH d_d
  USE
ENDIF

IF LIST_TIT.CLI_CODE # 0
  USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS CR_9203 AGAIN
  IF .NOT. SEEK(STR(LIST_TIT.CLI_CODE)+LIST_TIT.FIRM)
    APPEND BLANK
    REPLACE CUS_CODE WITH LIST_TIT.CLI_CODE, ;
            FIRM      WITH LIST_TIT.FIRM
  ENDIF
  IF db_t = " "
    REPLACE ACC_SUM WITH ACC_SUM-m_tot
  ELSE
    REPLACE ACC_SUMD WITH ACC_SUMD-m_tot_d
  ENDIF
  USE
ENDIF

SELECT LIST_TIT
REPLACE SAL_NUM  WITH d_n, ;
        SAL_DATE WITH d_d, ;
        WHO_SAL  WITH sale_user
SELECT (sel_sav)

IF .NOT. EMPTY(d_n)
  DO SendToVk WITH LIST_TIT.FIRM, d_n, d_d
ENDIF

IF USED("DEMAND")
  SELECT DEMAND
  IF USED("DEM_LINK")
    DO WHILE SEEK(LIST_TIT.LST_NUM, "DEM_LINK")
      GO DEM_LINK.LINK IN DEMAND
      REPLACE DEMAND.CLI_CODE WITH LIST_TIT.CLI_CODE, ;
              DEMAND.CLIENT   WITH LIST_TIT.LST_NAME
      REPLACE DEM_LINK.LST_NUM WITH 0
    ENDDO
  ENDIF
  SELECT (sel_sav)
ENDIF

RETURN

**************************************************************
PROCEDURE Ch_Part
PRIVATE s_sav
s_sav = SELECT()
SELECT INC_SALE
IF q_rest < 0
  SET FILTER TO
ELSE
  SET FILTER TO QNT_REST > 0
ENDIF
key_act = LEFT(key_act, 7)    && Ищем первую!
IF .NOT. SEEK(key_act, "INC_SALE")
  IF LEN(key_act) > 7
    key_act = LEFT(key_act, 7)
    IF .NOT. SEEK(key_act, "INC_SALE")
      RETURN .F.
    ENDIF
  ELSE
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Nalog      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Вычисление налога с продаж.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.01.2002 ──┘
PROCEDURE G_Nalog
PARAMETERS tmpFirm

PRIVATE s_sav, tmpUr, tmpPers

tmpUr = .T.
IF LIST_TIT.CLI_CODE # 0
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"ACCOUNT") SHARED AGAIN ALIAS CUS_2121 ORDER TAG CUS_CODE
  SEEK LIST_TIT.CLI_CODE
  tmpUr = EMPTY(MARK1)
  USE
  SELECT (s_sav)
ENDIF

*IF LIST_TIT.IS_CHECK .OR. .NOT. tmpUr
IF .NOT. tmpUr
  tmpPers = GetNalog(tmpFirm)
ELSE
  tmpPers = 0
ENDIF

RETURN tmpPers

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_User     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Выбираем того, кто продает.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.09.2006 ──┘
PROCEDURE Get_User
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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌─────────────────────────────────────────────────┐
*│ ( )                                             │
*│ ( )                                             │
*│ ( )                                             │
*│        < OK Ctrl-W >                            │
*└─────────────────────────────────────────────────┘

PRIVATE ex, tmpSw, code1, code2, code3, name1, name2, name3
m.code1 = m.user
m.code2 = LIST_TIT.WHO_CR
m.code3 = 0
m.tmpSw = 1
m.ex = 1
USE (m.base_path+"PERSONS") SHARED AGAIN ALIAS PRS_6912 ORDER TAG CODE IN 0
= SEEK(m.code1, "PRS_6912")
m.name1 = ALLTRIM(PRS_6912.FAMILY)+" "+LEFT(PRS_6912.NAME, 1)+" "+LEFT(PRS_6912.S_NAME, 1)
= SEEK(m.code2, "PRS_6912")
m.name2 = ALLTRIM(PRS_6912.FAMILY)+" "+LEFT(PRS_6912.NAME, 1)+" "+LEFT(PRS_6912.S_NAME, 1)
m.name3 = "Кто-то другой"
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 53, "ВНИМАНИЕ! Список создан не Вами. Кто продает"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 4, 8 SAY m.name3
@ 2, 3 GET m.tmpSw PICTURE "@*RV  "+m.name1+";"+m.name2+"; " VALID Tst_Other()

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------
DO CASE
CASE m.tmpSw = 1
  m.sale_user = m.code1
CASE m.tmpSw = 2
  m.sale_user = m.code2
CASE m.tmpSw = 3
  m.sale_user = m.code3
ENDCASE

USE IN PRS_6912
 
POP KEY
RELEASE WINDOW (win_name)
RETURN m.ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Other    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Выбираем сотрудника                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.09.2006 ──┘
PROCEDURE Tst_Other

PRIVATE tmpUser

IF m.tmpSw # 3
  m.code3 = 0
  m.name3 = "Кто-то другой"
ELSE
  m.tmpUser = m.code3

  DO Ch_Pers WITH m.tmpUser, m.name3
  m.code3 = m.tmpUser
  IF m.code3 = 0
    m.tmpSw = 1
    SHOW GET m.tmpSw
    m.name3 = "Кто-то другой"
  ENDIF
ENDIF
@ 4, 8 SAY PADR(m.name3, 40)

RETURN .T.
