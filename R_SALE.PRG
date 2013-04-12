***********************************************************************
*                                                                     *
*                        Собственно продажа.                          *
*                                                                     *
***********************************************************************
PROCEDURE R_Sale
PRIVATE sel_sav, arr_w, q_rep, mss, q_pos, d_n, d_d, db_n, p_d, p_r0, p_r, kurs, c_name

sel_sav = SELECT()
d_d = DATE()
d_n = ""
db_n= ""

*!*	SELECT 0
USE (base_store+"\CURS_SET") IN 0 ORDER TAG HRON DESCENDING ALIAS CRS_000 AGAIN
SELECT CRS_000
SEEK "CURS      "
kurs = CURS

*!*	SELECT 0
USE (base_store+"\SALE") IN 0 ALIAS SAL_000 AGAIN
SELECT SAL_000
*!*	SET NOCPTRANS TO WHERE
 
*!*	SELECT 0
USE (base_store+"\ACCOUNT") IN 0 ORDER TAG CUS_CODE ALIAS CUS_000 AGAIN
SELECT CUS_000

*!*	SELECT 0
USE (base_store+"\ACT_S") IN 0 ORDER TAG SALE ALIAS INC_SALE AGAIN
SELECT INC_SALE

SELECT DECL_LST
*
*   Коррекция брони ( именно брони! ) и формирование строк накладной
*
*!*	IF DECL_TIT.DOLLAR == "Ч"
*!*	  t_nds = 0
*!*	ENDIF
PRIVATE q_sale, q_rest, key_act
m_tot = 0
m_tot_d = 0
m_tot_0 = 0
SELECT DECL_LST
s = 0
SEEK STR(YEAR(DECL_TIT.DOC_DATE),4)+STR(DECL_TIT.DOC_NUM, FSIZE("DOC_NUM"))  && На начало заявки и...
SCAN REST WHILE DECL_TIT.DOC_DATE = DOC_DATE .AND. DECL_TIT.DOC_NUM = DOC_NUM && перебираем содержимое
  IF .NOT. SEEK(STO_CODE, "STO_AS")    && Странно, только что было...
    LOOP
  ENDIF
  p_d  = DECL_LST.PRICE
  IF DECL_TIT.DOLLAR = "3"
    p_r  = p_d*kurs
    p_r0 = ROUND(100*p_r/(100+t_nds),2)
    p_r  = ROUND(p_r0*(100+t_nds)/100,5)
  ELSE
*!*	    IF DECL_TIT.DOLLAR == "Ч"
*!*	      p_r0 = ROUND(DECL_LST.PRICE_R*(100+t_nds)/100,2)
*!*	      p_r  = ROUND(DECL_LST.PRICE_R*(100+t_nds)/100,2)
*!*	    ELSE
      p_r0 = DECL_LST.PRICE_R
      p_r  = ROUND(p_r0*(100+t_nds)/100,5)
*!*	    ENDIF
  ENDIF
  q_rest = QTY
  SELECT SAL_000
  q_pos = 0
  key_act = STR(DECL_LST.STO_CODE,7)+DECL_TIT.FIRM
  DO WHILE q_rest > 0
    IF .NOT. Ch_Part()
*!*	      DIMENSION mss(4)
*!*	      mss(1) = ""
*!*	      mss(2) = CHR(0)+"Внимание! "+ALLTRIM(STO_AS.PREFIX)+" "+ALLTRIM(STO_AS.NAME)
*!*	      mss(3) = CHR(0)+"Не хватает в партиях. Сумма по накладной будет другая!"
*!*	      mss(4) = ""
*!*	      DO Out_Mess WITH 7, "mss"
	=MESSAGEBOX("Внимание! "+ALLTRIM(STO_AS.PREFIX)+" "+ALLTRIM(STO_AS.NAME)+CHR(13)+;
	"Не хватает в партиях. Сумма по накладной будет другая!",0+64+0)
	err_level = MAX(err_level,1)	
    EXIT
    ENDIF
    q_sale = MIN(q_rest, INC_SALE.QNT_REST)
    q_rest = q_rest-q_sale
    q_rep = MIN(INC_SALE.REST_REP, q_sale)
    SELECT STO_AS
    REPLACE QNT_SAVE   WITH QNT_SAVE-q_sale, ;
            QNT_REP    WITH QNT_REP-q_rep
    SELECT INC_SALE
    REPLACE QNT_REST  WITH QNT_REST-q_sale, ;
            REST_REP  WITH REST_REP-q_rep,  ;
            LAST_SALE WITH DATE()
    SELECT SAL_000
    IF EMPTY(d_n)
*!*	      IF DECL_TIT.DOLLAR == "Ч"
*!*	        d_n = NEWCOUNT("G_CHECK", DECL_TIT.FIRM, DECL_TIT.WHERE, STR(YEAR(d_d),4)) 
*!*	        d_n = sys_char+"***"+STR(d_n,6)
*!*	      ELSE
***      d_n = Doc_Num("SALE", DECL_TIT.FIRM, DECL_TIT.WHERE, d_d) &&&&&&&&&&&&&&&&&&&&&
        d_n = NEWCOUNT("SALE", DECL_TIT.FIRM, DECL_TIT.WHERE, STR(YEAR(d_d),4)) 
		d_n = sys_char+RIGHT(STR(1000+VAL(DECL_TIT.WHERE)),3)+STR(d_n,6)
*!*	      ENDIF
    ENDIF

    SELECT SAL_000
    APPEND BLANK
    REPLACE FIRM       WITH DECL_TIT.FIRM, ;
            DOC_NUM    WITH d_n,           ;
            DOC_DATE   WITH d_d,           ;
            CODE       WITH DECL_LST.STO_CODE,;
            QNT        WITH q_sale,        ;
            QNT_REP    WITH q_rep,         ;
            PRICE_D_L  WITH STO_AS.PRICE,  ;
            PRICE_R_L  WITH STO_AS.PRICE_R,;
            PRICE_TYPE WITH "S",           ;
            PRICE_D    WITH p_d,           ;
            PRICE_0    WITH p_r0,          ;
            PRICE_R    WITH p_r,           ;
            ACT_FIRM   WITH INC_SALE.FIRM, ; 
            ACT_NUM    WITH INC_SALE.DOC_NUM, ;
            ACT_DATE   WITH INC_SALE.DOC_DATE,;
            GTD_NUM    WITH INC_SALE.GTD_NUM, ;
            GTD_LINE   WITH INC_SALE.GTD_LINE,;
            COUNTRY    WITH INC_SALE.COUNTRY, ;
            WHERE      WITH DECL_TIT.WHERE
            
    q_pos = q_pos+QNT
  ENDDO
  m_tot = m_tot+ROUND(q_pos*PRICE_R,2)
  m_tot_d = m_tot_d+q_pos*PRICE_D
  m_tot_0 = m_tot_0+q_pos*PRICE_0
  IF SEEK(DECL_TIT.LST_NUM + STR(DECL_LST.STO_CODE,7), "LD_AS")
    REPLACE LD_AS.QNT     WITH LD_AS.QNT-q_pos, ;
            LD_AS.QNT_REQ WITH LD_AS.QNT_REQ-q_pos
  ENDIF
  SELECT DECL_LST

ENDSCAN

SELECT SAL_000
USE
SELECT INC_SALE
USE
SELECT STO_AS

***db_n = Doc_Num("BILL_F",  DECL_TIT.FIRM, DECL_TIT.WHERE, d_d) &&&&&&&&&&&&&&&&&&&&&
*!*	IF .NOT. DECL_TIT.DOLLAR == "Ч"
  db_n = NEWCOUNT("BILL_F", DECL_TIT.FIRM, DECL_TIT.WHERE, STR(YEAR(d_d),4))
*!*	ELSE
*!*	  db_n = 0
*!*	ENDIF

SELECT CUS_000
SEEK DECL_TIT.CUS_CODE
c_name = CUS_NAME
*!*	SELECT 0
USE (base_store+"\SALE_TIT")  IN 0 ALIAS SAL_000 AGAIN
SELECT SAL_000
*!*	 SET NOCPTRANS TO WHERE
IF EMPTY(db_n)
  db_n = ""
ELSE
  db_n = sys_char+STR(db_n,FSIZE("NEW_DOC")-1)
ENDIF
APPEND BLANK
REPLACE FIRM      WITH DECL_TIT.FIRM, ;
        DOC_NUM   WITH d_n,     ;
        DOC_DATE  WITH d_d,     ;
        DOC_TIME  WITH SECONDS(),  ;
        REQ_DOC   WITH STR(DECL_TIT.SPEC_NUM,10),  ;
        REQ_DATE  WITH DECL_TIT.SPEC_DATE, ;
        NEW_DOC   WITH db_n,    ;
        NEW_TYPE  WITH " ",     ;
        SAL_FLAGS WITH IIF(DECL_TIT.DOLLAR == "Ч", "              **", "               *"), ;
        CUS_CODE  WITH DECL_TIT.CUS_CODE,  ;
        CUS_NAME  WITH CUS_000.CUS_NAME,   ;
        LIST_NUM  WITH DECL_TIT.LST_NUM ,  ;
        LIST_DATE WITH DECL_TIT.LST_DATE,  ;
        PAY_TYPE  WITH DECL_TIT.INC_TYPE,  ;
        PAY_DOC   WITH DECL_TIT.INC_NUM,   ;
        PAY_DATE  WITH DECL_TIT.INC_DATE,  ;
        TOTAL_0   WITH m_tot_0,   ;
        NDS_      WITH t_nds, ;
        NSP_      WITH DECL_TIT.P_NALOG, ;
        KASSA     WITH DECL_TIT.R_N,     ;
        TOTAL_R   WITH m_tot, ;
        TOTAL_D   WITH m_tot_d, ;
        WHO_LET   WITH DECL_TIT.WHO,  ;
        WHO       WITH DECL_TIT.WHO,  ;
        WHERE     WITH DECL_TIT.WHERE
        
IF NOT EMPTY(FSIZE("CLI_CODE"))
REPLACE CLI_CODE	WITH DECL_TIT.CLI_CODE
ENDIF
IF NOT EMPTY(FSIZE("CODE_STOCK"))
REPLACE CODE_STOCK	WITH DECL_TIT.CODE_OFF
ENDIF

USE

IF LT_AS.CLI_CODE # 0
  USE (base_store+"\CUS_REST") ORDER TAG CUS_CODE ALIAS CR_9203 AGAIN
  IF .NOT. SEEK(STR(DECL_TIT.CUS_CODE)+DECL_TIT.FIRM)
    APPEND BLANK
    REPLACE CUS_CODE  WITH DECL_TIT.CUS_CODE, ;
            FIRM      WITH DECL_TIT.FIRM
  ENDIF
  IF DECL_TIT.DOLLAR # "3"
    REPLACE ACC_S_H WITH ACC_S_H-m_tot
  ENDIF
  USE
ENDIF

IF DECL_TIT.DOLLAR = "3"
  DO Conv       && Конвертация!
ENDIF

SELECT DECL_TIT
REPLACE	SALE_NUM	WITH	d_n,;
		SALE_DATE	WITH	d_d,;
		FLAG		WITH	STR(err_level,1)
		
SELECT (sel_sav)

IF .NOT. EMPTY(d_n)
  DO SendToVk WITH DECL_TIT.FIRM, d_n, d_d
ENDIF

IF USED("CUS_000")
USE IN CUS_000
ENDIF
IF USED("CRS_000")
USE IN CRS_000
ENDIF

RETURN

**************************************************************
PROCEDURE Ch_Part
key_act = LEFT(key_act, 7)
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

*+------------------------------------------------------------------------+
*! Файл SendToVK     Разработчик Андрей Васин           06.03.98 11:14:35 !
*+------------------------------------------------------------------------+
*!                                                                        !
*!                        Посылка расхода в архив.                        !
*!                                                                        !
*+------------------------------------------------------------------------+
PROCEDURE SendToVK
PARAMETERS d_firm, d_num, d_date

PRIVATE sel_rem, d_year, c_c, c_sav, pr_v , vk_arch, tmpUser,d_who,c_c1,flgs
PRIVATE savePrice, p_sav, tmpPrice, sum0, qnt0, zeroPrice, zeroPriceC
PRIVATE who_saled,sale_stock
who_saled = 0
sale_stock = 0

vk_arch = Base_Compel + "\SALE"
IF TYPE("vk_arch") # "C"
  RETURN
ENDIF

savePrice = .T.

IF EMPTY(vk_arch)  &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  RETURN
ENDIF

IF TYPE("d_date") = "D"
  d_year = LEFT(DTOS(d_date),4)
ENDIF
IF TYPE("d_date") = "N"
  d_year = STR(d_date,4)
ENDIF
IF TYPE("d_date") = "C"
  d_year = d_date
ENDIF

sel_rem = SELECT()
*!*	SELECT 0
USE (base_store+"\SALE_TIT") IN 0 ORDER TAG DOC_NUM ALIAS SEND_TMP AGAIN
SELECT SEND_TMP

IF .NOT. SEEK(d_firm+d_year+d_num)
  USE
  SELECT (sel_rem)
  RETURN
ELSE
  IF NOT EMPTY(FSIZE("CODE_STOCK"))
  sale_stock = SEND_TMP.code_stock
  ENDIF
ENDIF

IF TOTAL_R < 0 .AND. CUS_CODE >= 0 .AND. .NOT. EMPTY(REQ_DOC)
  PRIVATE r_sav, k000
  k000 = FIRM+LEFT(DTOS(REQ_DATE),4)+REQ_DOC
  r_sav = RECNO()
  IF SEEK(k000)
    who_saled = WHO
  ENDIF
  GO r_sav
ENDIF

IF CUS_CODE < 0
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
flgs = SAL_FLAGS
c_c = CUS_CODE
c_c1 = IIF(FSIZE("CLI_CODE")#0,CLI_CODE,0)
d_who = WHO


USE (Base_Compel+"\SALE") ALIAS SEND_RES AGAIN

IF FSIZE("PRICE_DC") = 0
  savePrice = .F.
ELSE
  SET NOCPTRANS TO PRICE_DC
ENDIF

USE (base_store+"\SALE") IN 0 ORDER TAG DOC_NUM ALIAS SEND_TMP AGAIN

IF savePrice
  SELECT 0
  USE (base_store+"\ACT_S") AGAIN ALIAS PARTY_D ORDER TAG INC_LIST
  SET NOCPTRANS TO PRICE_DC
  SELECT SEND_TMP
  SET RELATION TO STR(CODE,7)+ACT_FIRM+DTOS(ACT_DATE)+ACT_NUM INTO PARTY_D
ENDIF

SELECT SEND_TMP

SEEK d_firm+d_year+d_num
c_sav = 0
p_sav = SPACE(10)
tmpPrice = SPACE(10)
zeroPrice = .F.
zeroPriceC = Crpt_1(0, 10, 4)
*SCAN REST WHILE d_firm = FIRM .AND. d_num = DOC_NUM .AND. d_year = LEFT(DTOS(DOC_DATE),4)
SCAN REST WHILE FIRM = d_firm .AND. DOC_NUM = d_num .AND. LEFT(DTOS(DOC_DATE),4) = d_year
  IF savePrice
    tmpPrice = Crpt_2(PARTY_D.PRICE_DC)
  ENDIF
  SELECT SEND_RES
  IF c_sav # SEND_TMP.CODE  && .OR. p_sav # tmpPrice 
    sum0 = 0
    qnt0 = 0
    zeroPrice = .F.
    APPEND BLANK
*    IF SEND_TMP.PRICE_D # 0 .AND. LEFT(SEND_TMP.DOC_NUM,1) = sys_char
      pr_v = SEND_TMP.PRICE_D
*    ELSE
*      pr_v = ROUND(SEND_TMP.PRICE_R/kurs_3,5)
*    ENDIF
    REPLACE FIRM     	WITH SEND_TMP.FIRM,     ;
            DOC_NUM  	WITH SEND_TMP.DOC_NUM,  ;
            DATE     	WITH SEND_TMP.DOC_DATE, ;
            SUB_SYS  	WITH LEFT(SEND_TMP.DOC_NUM,1), ;
            CUS_CODE 	WITH c_c, ;
            STO_CODE 	WITH SEND_TMP.CODE,;
            SAL_FLAGS   WITH flgs,         ;
            QTY      	WITH SEND_TMP.QNT, ;
            PRICE    	WITH pr_v, ;
            RPRICE   	WITH SEND_TMP.PRICE_R, ;
            PRICE_DC    WITH zeroPriceC,    ;
            PRICE_TYPE 	WITH SEND_TMP.PRICE_TYPE
            
	 IF NOT EMPTY(FSIZE("MAN_CODE"))            
	   REPLACE    MAN_CODE    WITH d_who 
	 ENDIF
     IF FSIZE("SAL_CODE") # 0
       REPLACE SAL_CODE WITH who_saled
     ENDIF
	
	 IF NOT EMPTY(FSIZE("DEPARTMENT"))
	 REPLACE    DEPARTMENT	WITH IIF(NOT EMPTY(glDeps),glDeps,"S")
	 ENDIF

   	 IF NOT EMPTY(FSIZE("DIRECT"))
	 REPLACE    DIRECT	WITH IIF(NOT EMPTY(glDirect),glDirect,"")
	 ENDIF
 
     IF NOT EMPTY(FSIZE("CLI_CODE"))
     REPLACE	CLI_CODE	WITH c_c1
     ENDIF

   	 IF NOT EMPTY(FSIZE("CODE_STOCK"))
	 REPLACE  CODE_STOCK WITH sale_stock
	 ENDIF
            
    c_sav = SEND_TMP.CODE
    p_sav = tmpPrice
  ELSE
    REPLACE QTY WITH QTY+SEND_TMP.QNT
  ENDIF
  IF savePrice
    IF tmpPrice # 0
      sum0 = sum0+SEND_TMP.QNT*tmpPrice
      qnt0 = qnt0+SEND_TMP.QNT
      REPLACE PRICE_DC WITH Crpt_1(ROUND(sum0/qnt0,4), 10, 4)
    ENDIF
  ENDIF
  SELECT SEND_TMP
ENDSCAN

USE
SELECT SEND_RES
USE
IF savePrice
  SELECT PARTY_D
  USE
ENDIF

SELECT (sel_rem)

RETURN

*+------------------------------------------------------------------------+
*! Файл Conv         Разработчик Андрей Васин           18.05.99 11:14:35 !
*+------------------------------------------------------------------------+
*!                                                                        !
*!                        Конвертация.                                    !
*!                                                                        !
*+------------------------------------------------------------------------+
PROCEDURE Conv
PRIVATE s_sav, doc_n, doc_d, sums, numbs, rst, rst_0, rest_d, k_w00, ord_cnt
PRIVATE sum_w, j00, doc1, doc2

s_sav = SELECT()

*
*   Коррекция депозита
*
*!*	SELECT 0
USE (base_store+"\CUS_REST") IN 0 ORDER TAG CUS_CODE ALIAS CR_9203 AGAIN
SELECT CR_9203
IF .NOT. SEEK(STR(DECL_TIT.CUS_CODE)+DECL_TIT.FIRM)
  APPEND BLANK
  REPLACE CUS_CDODE WITH DECL_TIT.CUS_CODE, ;
          FIRM      WITH DECL_TIT.FIRM
ENDIF
**** 25.05.2001 ******* REPLACE ACC_SD_H WITH ACC_SD_H-m_tot_d
REPLACE ACC_SD_H WITH ACC_SD_H-DECL_TIT.SUM_D
*
*  Рублевый депозит не корректируем, т. к. он не корректируется и при продаже!

rst_n = m_tot*pay_prc/100
USE (base_store+"\FIRMS") ORDER TAG FIRM_CODE ALIAS FRM_9203 AGAIN
REPLACE NALOG WITH NALOG + rst_n

*----------------------------------------------------------
rst = SELECT()
SELECT 0
USE (base_store+"\FIRMS") ORDER TAG FIRM_CODE ALIAS F_9B23 AGAIN
SEEK DECL_TIT.FIRM
max_ord = IIF(FSIZE("M_SUM")=0, max_ord,M_SUM)
USE 
SELECT (rst)
*----------------------------------------------------------
USE (base_store+"\INCMONEY") ALIAS MNY_9212 AGAIN
SELECT MNY_9212
*!*	SET NOCPTRANS TO WHERE
 
doc_d = DATE()
rst_0 = ROUND(m_tot/(100+t_nds)*100,2)
rst   = m_tot
rst_n = m_tot*pay_prc/100
**** 25.05.2001 ******* rst_d = m_tot_d
rst_d = DECL_TIT.SUM_D
k_w00 = rst/rst_d
ord_cnt  = 0
IF rst < max_ord .OR. max_ord = 0
  DIMENSION numbs(1), sums(1,4)
  ord_cnt  = 1
  numbs(1) = 0
  sums(1,1) = rst
  sums(1,2) = rst_0
  sums(1,3) = rst_n
  sums(1,4) = rst_d
ELSE
  DO WHILE rst > 0
    IF rst < max_ord
      sum_w = rst
    ELSE
      sum_w = max_ord-ROUND(0.1*RAND()*max_ord, 0)
    ENDIF
    ord_cnt = ord_cnt+1
    rst = rst-sum_w
    DIMENSION numbs(ord_cnt), sums(ord_cnt,4)
    numbs(ord_cnt) = 0
    sums(ord_cnt,1)  = sum_w
    IF rst # 0
      sums(ord_cnt,2)  = ROUND(sum_w/(100+t_nds)*100, 2)
      rst_0 = rst_0-sums(ord_cnt,2)
      sums(ord_cnt,3)  = ROUND(sum_w*pay_prc/100, 2)
      rst_n = rst_n-sums(ord_cnt,3)
      sums(ord_cnt,4)  = ROUND(sum_w/k_w00,3)
      rst_d = rst_d-sums(ord_cnt,4)
    ELSE
      sums(ord_cnt,2)  = rst_0
      sums(ord_cnt,3)  = rst_n
      sums(ord_cnt,4)  = rst_d
    ENDIF
  ENDDO
ENDIF

FOR j00 = 1 TO ord_cnt
*!*	  doc_n = NewCount("INC_ORDER", DECL_TIT.FIRM, DECL_TIT.WHERE,STR(YEAR(doc_d),4)) &&&&&&&&&&&&&&&&&&&&
*!*	  doc_n = sys_char+STR(doc_n,9)
  doc_n = NewCount("INC_ORDER", DECL_TIT.FIRM, DECL_TIT.WHERE,STR(YEAR(doc_d),4)) &&&&&&&&&&&&&&&&&&&&
  doc_n = sys_char+STR(doc_n,9)
  
  numbs(j00) = doc_n
  SELECT MNY_9212
  APPEND BLANK
  REPLACE FIRM       WITH DECL_TIT.FIRM,;
          DOC_TYPE   WITH " ",          ;
          DOC_NUM    WITH doc_n,        ;
          DOC_DATE   WITH doc_d+j00-1,  ;
          DOC_TIME   WITH SECONDS(),    ;
          ENTER_DAT  WITH doc_d+j00-1,  ;
          CUS_CODE   WITH DECL_TIT.CUS_CODE,  ;
          CUS_NAME   WITH c_name,       ;
          MONEY_0    WITH sums(j00,2),  ;
          NDS_       WITH t_nds,        ;
          MONEY_R    WITH sums(j00,1),  ;
          MONEY_D    WITH sums(j00,4),  ;
          NL_PRC     WITH pay_prc,      ;
          N_OF_SALE  WITH sums(j00,3),  ;
          WHO        WITH DECL_TIT.WHO, ;
          WHERE      WITH DECL_TIT.WHERE
ENDFOR

*!*	doc_n = NewCount("MONEY_$", DECL_TIT.FIRM, DECL_TIT.WHERE, STR(YEAR(doc_d),4)) &&&&&&&&&&&&&&&&&&&&&&&&&&&&
doc_n = NewCount("MONEY_$", DECL_TIT.FIRM, DECL_TIT.WHERE, STR(YEAR(doc_d),4)) &&&&&&&&&&&&&&&&&&&&&&&&&&&&

SELECT MNY_9212
APPEND BLANK
REPLACE FIRM       WITH DECL_TIT.FIRM,  ;
        DOC_TYPE   WITH "3",                ;
        DOC_NUM    WITH sys_char+STR(doc_n,9), ;
        DOC_DATE   WITH doc_d,   ;
        DOC_TIME   WITH SECONDS(), ;
        ENTER_DAT  WITH doc_d,   ;
        CUS_CODE   WITH DECL_TIT.CUS_CODE,  ;
        CUS_NAME   WITH c_name,  ;
        MONEY_0    WITH -DECL_TIT.SUM_D,   ;
        NDS_       WITH 0,        ;
        MONEY_R    WITH -ROUND(DECL_TIT.SUM_D*kurs,2),  ;
        MONEY_D    WITH -DECL_TIT.SUM_D,   ;
        NL_PRC     WITH 0,        ;
        N_OF_SALE  WITH 0,        ;
        WHO        WITH DECL_TIT.WHO,     ;
        WHERE      WITH DECL_TIT.WHERE

******* 25.05.2001 *******        MONEY_0    WITH -m_tot_d,   ;
******* 25.05.2001 *******        MONEY_R    WITH -ROUND(DECL_TIT.SUM_D*kurs,2),  ;
******* 25.05.2001 *******        MONEY_D    WITH -DECL_TIT.SUM_D,   

USE (base_store+"\KASSA") ALIAS KSS_9212 AGAIN
SELECT KSS_9212
*!*	SET NOCPTRANS TO WHERE
doc1 = NewCount("KASSA", DECL_TIT.FIRM, DECL_TIT.WHERE, STR(YEAR(doc_d),4)) &&&&&&&&&&&&&&&&&&&&
doc1 = sys_char+RIGHT(STR(1000+VAL(DECL_TIT.WHERE)),3)+STR(doc1,6)

SELECT KSS_9212
APPEND BLANK
REPLACE DOC_NUM    WITH doc1,      ;
        DOC_DATE   WITH doc_d,     ;
        DOC_TIME   WITH SECONDS(), ;
        VALUTA     WITH "$",       ;
        TOTAL      WITH -DECL_TIT.SUM_D,  ;
        WHO_REP    WITH DECL_TIT.WHO, ;
        WHO_GET    WITH DECL_TIT.WHO, ;
        WHERE      WITH DECL_TIT.WHERE

******* 25.05.2001 *******         TOTAL      WITH -m_tot_d,  

SELECT KSS_9212

doc2 = NewCount("KASSA", DECL_TIT.FIRM, DECL_TIT.WHERE, STR(YEAR(doc_d),4)) &&&&&&&&&&&&&&&&&&&&&&&
doc2 = sys_char+RIGHT(STR(1000+VAL(DECL_TIT.WHERE)),3)+STR(doc2,6)

SELECT KSS_9212
APPEND BLANK
REPLACE DOC_NUM    WITH doc2,      ;
        DOC_DATE   WITH doc_d,     ;
        DOC_TIME   WITH SECONDS(), ;
        VALUTA     WITH "Р",       ;
        TOTAL      WITH m_tot+m_tot*pay_prc/100, ;
        WHO_REP    WITH DECL_TIT.WHO, ;
        WHO_GET    WITH DECL_TIT.WHO, ;
        WHERE      WITH DECL_TIT.WHERE
*
*  Основная касса
*
USE (base_store+"\KASSA_M") ALIAS KSS_9212 AGAIN
SELECT KSS_9212
*!*	SET NOCPTRANS TO WHERE

doc_n = NewCount("KASSA_M", DECL_TIT.FIRM, DECL_TIT.WHERE, STR(YEAR(doc_d),4))  &&&&&&&&&&&&&&&&&&&&&&&&&&
doc_n = sys_char+STR(doc_n,6)

SELECT KSS_9212
APPEND BLANK
REPLACE DOC_NUM    WITH doc_n,  ;
        DOC_DATE   WITH doc_d,  ;
        DOC_TIME   WITH SECONDS(),  ;
        CODE       WITH 1,      ;
        REF_NUM    WITH doc2,   ;
        REF_DATE   WITH doc_d,  ;
        SUM_R      WITH m_tot+m_tot*pay_prc/100, ;
        WHO        WITH DECL_TIT.WHO,   ;
        WHERE      WITH DECL_TIT.WHERE

doc_n = NewCount("KASSA_M", DECL_TIT.FIRM, DECL_TIT.WHERE, STR(YEAR(doc_d),4)) &&&&&&&&&&&&&&&&&&&&&&&&
doc_n = sys_char+STR(doc_n,6)

SELECT KSS_9212
APPEND BLANK
REPLACE DOC_NUM    WITH doc_n,  ;
        DOC_DATE   WITH doc_d,  ;
        DOC_TIME   WITH SECONDS(), ;
        CODE       WITH 1,      ;
        REF_NUM    WITH doc1,   ;
        REF_DATE   WITH doc_d,  ;
        SUM_D      WITH -DECL_TIT.SUM_D, ;
        WHO        WITH DECL_TIT.WHO,   ;
        WHERE      WITH DECL_TIT.WHERE
******* 25.05.2001 *******        SUM_D      WITH -m_tot_d, ;

doc_n = NewCount("KASSA_M", DECL_TIT.FIRM, DECL_TIT.WHERE, STR(YEAR(doc_d),4))   &&&&&&&&&&&&&&&&&&&&&&&&&&&&&
doc_n = sys_char+STR(doc_n,6)

SELECT KSS_9212
APPEND BLANK
REPLACE DOC_NUM    WITH doc_n,  ;
        DOC_DATE   WITH doc_d,  ;
        DOC_TIME   WITH SECONDS(),  ;
        CODE       WITH 2,      ;
        SUM_R      WITH -m_tot-m_tot*pay_prc/100,;
        SUM_D      WITH DECL_TIT.SUM_D,;
        NOTE       WITH c_name, ;
        WHO        WITH DECL_TIT.WHO,   ;
        WHERE      WITH DECL_TIT.WHERE


******* 25.05.2001 *******        SUM_D      WITH m_tot_d,

USE
SELECT (s_sav)

RETURN
