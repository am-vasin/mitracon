*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º           à®£à ¬¬  ¯¥à¥¤ ç¨ ¤ ­­ëå ­  ®á­®¢­®© áª« ¤.                º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Send0

PRIVATE s_date, t_num, pthw, arr_w, n, i, m, own_c, korr_c
PRIVATE n_over      && ç¨á«® ¯¥à¥ªàëâ¨©
n_over = 4

IF .NOT. G_Exclu()      && ëâ ¥¬áï § å¢ â¨âì ¬®­®¯®«ì­ë© ¤®áâã¯ ª ¡ §¥
  RETURN
ENDIF

DO Prep_TD    && ‘®§¤ ¥¬ ¨«¨ ®ç¨é ¥¬ âà ­§¨â­ãî ¤¨à¥ªâ®à¨î
pthw = tmpo_path+"transmit\"

s_date = {}
t_num  = 0

USE (base_path+"CUSTOMER")
LOCATE FOR .NOT. EMPTY(SELF)
own_c = STR(CODE, 8)
LOCATE FOR .NOT. EMPTY(SUP)
korr_c = STR(CODE, 8)
USE

DO Parm_TD WITH "SM"+korr_c

DO Wt_Mess WITH "¥à¥¤ ç  ¯à¨¥¬­ëå  ªâ®¢"
DO S_Acts WITH pthw, s_date    && ¥à¥¤ ç  ¯à¨¥¬­ëå  ªâ®¢
DO Wt_Mess

DO Wt_Mess WITH "¥à¥¤ ç  ­ ª« ¤­ëå"
DO S_Inv WITH pthw, s_date     && ¥à¥¤ ç  ­ ª« ¤­ëå
DO Wt_Mess

DO Wt_Mess WITH "¥à¥¤ ç  ®âç¥â  ¯® ¤¥­ì£ ¬"
DO S_Money WITH pthw, s_date   && ¥à¥¤ ç  ¤¥­¥£
DO Wt_Mess

DO Wt_Mess WITH "¥à¥¤ ç  áç¥â®¢"
DO S_Bills WITH pthw, s_date   && ¥à¥¤ ç  áç¥â®¢
DO Wt_Mess

*
*    ®¬¥­ª« âãà 
DO Wt_Mess WITH "¥à¥¤ ç  ­®¬¥­ª« âãàë"
USE (base_path+"STOCK")
COPY FOR QNT  # 0 .OR. ;
     QNT_SAVE # 0 .OR. ;
     QNT_REP  # 0 .OR. ;
     QNT_REZ  # 0 .OR. ;
     QNT_REAL # 0      ;
     TO (pthw+"STOCK")  ;
     FIELDS ;
         CODE,     PREFIX,   NAME,   PRODUCER,  ;
         QNT,      QNT_REP,  QNT_REAL, ;
         QNT_SAVE, QNT_REZ,  PRICE_B,  PRICE_B_R,;
         PRICE,    PRICE_R
DO Wt_Mess

DO Wt_Mess WITH "¥à¥¤ ç  á¯¨áª  ¯®áâ®ï­­ëå ª«¨¥­â®¢"
USE (base_path+"ACCOUNT")
COPY TO (pthw+"ACCOUNT")
USE (base_path+"CUS_BIL")
COPY TO (pthw+"CUS_BIL")
USE (base_path+"CLI_INFO")
COPY TO (pthw+"CLI_INFO")
DO Wt_Mess

DO Wt_Mess WITH "¥à¥¤ ç  á¯¨áª  à ¡®ç¨å ¬¥áâ"
USE (base_path+"ARM_LST")
COPY TO (pthw+"ARM_LST")
DO Wt_Mess

DO Wt_Mess WITH "¥à¥¤ ç  á¯à®á "
DO S_Demand WITH  pthw, s_date   && ¥à¥¤ ç  á¯à®á 
DO Wt_Mess

*
*  ‡ ¯¨áì ¢ ¦ãà­ « ¯¥à¥¤ ç
USE (base_path+"TR_LOG")
APPEND BLANK
REPLACE TR_CODE  WITH "SM",   ;
        TR_NUM   WITH t_num,  ;
        TR_DATE  WITH DATE(), ;
        TR_TIME  WITH TIME(), ;
        TR_START WITH s_date, ;
        ID_CORR  WITH korr_c, ;
        ID_SELF  WITH own_c

COPY STRUCTURE TO (pthw+"ID_SEND")
SCATTER TO arr_w
USE (pthw+"ID_SEND")

*arr_w(6) = sys_char
APPEND FROM ARRAY arr_w
USE

DO C_Exclu      && Žá¢®¡®¦¤ ¥¬ ¡ §ã

DO Arch WITH pthw, "F_REPORT"

PRIVATE out_path, nm_f, nm_f1
nm_f = "SA"+RIGHT(STR(100000+t_num,6),5)
n = ADIR(arr_w,pthw+"*.*")
FOR i = 1 TO n
  IF LEFT(arr_w(i,1),10) # "F_REPORT.A")
    DELETE FILE (pthw+arr_w(i,1))
  ELSE
    nm_f1 = log_path+nm_f+RIGHT(arr_w(i,1),4)
    DELETE FILE (nm_f1)
    COPY FILE (pthw+arr_w(i,1)) TO (nm_f1)
  ENDIF
ENDFOR

out_path = "\"
out_path = Int_Dir(out_path, "Šã¤  ¯¨è¥¬ ¤ ­­ë¥?", 0)

IF .NOT. EMPTY(out_path)
  DO Copy_A WITH pthw+"F_REPORT", out_path+"F_REPORT"  && ‘®åà ­¥­¨¥  àå¨¢ 
ENDIF

RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º         à®£à ¬¬  ¯¥à¥¤ ç¨ ¯à¨¥¬­ëå  ªâ®¢ ­  ®á­®¢­®© áª« ¤.          º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE S_Acts
PARAMETERS pth, sdt   && ¯ãâì ¤«ï à¥§ã«ìâ â  ¨ ­ ç. ¤ â 
PRIVATE arr_t

USE (base_path+"ACT_S") ORDER TAG DOC_NUM

COPY STRUCTURE TO (pth+"ACT_S")
SELECT 0
USE (pth+"ACT_S") ALIAS DETAIL

SELECT 0
USE (base_path+"ACT_S_T")

COPY STRUCTURE TO (pth+"ACT_S_T")
SELECT 0
USE (pth+"ACT_S_T") ALIAS TITLE

SELECT ACT_S_T
SCAN FOR DOC_DATE >= sdt .AND. FLG_BUY = "R"

  SCATTER TO arr_t
  SELECT TITLE
  APPEND FROM ARRAY arr_t

  SELECT ACT_S
  SEEK ACT_S_T.FIRM+STR(YEAR(ACT_S_T.DOC_DATE),4)+ACT_S_T.DOC_NUM
  SCAN REST WHILE ACT_S_T.FIRM = FIRM .AND. ACT_S_T.DOC_DATE = DOC_DATE .AND. ACT_S_T.DOC_NUM = DOC_NUM

    SCATTER TO arr_t
    SELECT DETAIL
    APPEND FROM ARRAY arr_t

    SELECT ACT_S

  ENDSCAN
  SELECT ACT_S_T

ENDSCAN

USE
SELECT ACT_S
USE
SELECT DETAIL
USE
SELECT TITLE
USE

RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º            à®£à ¬¬  ¯¥à¥¤ ç¨ ­ ª« ¤­ëå ­  ®á­®¢­®© áª« ¤.            º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE S_Inv
PARAMETERS pth, sdt   && ¯ãâì ¤«ï à¥§ã«ìâ â  ¨ ­ ç. ¤ â 
PRIVATE arr_t, rc_w, s_dt

USE (base_path+"PARMS")
s_dt = DAT_START

USE (base_path+"SALE") ORDER TAG DOC_NUM

COPY STRUCTURE TO (pth+"SALE")
SELECT 0
USE (pth+"SALE") ALIAS DETAIL

SELECT 0
*USE (base_path+"ACT_S") ORDER TAG INC_LIST
USE (base_path+"ACT_S") ORDER TAG PARTY

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON
SEEK DTOS(sdt)
IF .NOT. FOUND()
  rc_w = RECNO(0)
ELSE
  rc_w = RECNO()
ENDIF

COPY STRUCTURE TO (pth+"SALE_TIT")
SELECT 0
USE (pth+"SALE_TIT") ALIAS TITLE

SELECT SALE_TIT
IF rc_w > 0 .AND. rc_w <= RECCOUNT()
  GO rc_w
  SCAN REST

    SCATTER TO arr_t
    SELECT TITLE
    APPEND FROM ARRAY arr_t

    SELECT SALE
    SEEK SALE_TIT.FIRM+LEFT(DTOS(SALE_TIT.DOC_DATE),4)+SALE_TIT.DOC_NUM
    SCAN REST WHILE SALE_TIT.FIRM = FIRM .AND. SALE_TIT.DOC_DATE = DOC_DATE .AND. SALE_TIT.DOC_NUM = DOC_NUM

      SCATTER TO arr_t
      SELECT DETAIL
      APPEND FROM ARRAY arr_t
      IF SEEK(PARTY, "ACT_S")
        SELECT ACT_S
        REPLACE QNT_REP WITH REST_REP
      ENDIF
      IF ACT_S.DOC_DATE < s_dt
        SELECT STOCK
        SEEK SALE.CODE
        REPLACE QNT_S_REP WITH QNT_S_REP-SALE.QNT_REP
      ENDIF
      SELECT SALE
      REPLACE QNT_REP WITH 0

    ENDSCAN
    SELECT SALE_TIT

  ENDSCAN

ENDIF

USE
SELECT SALE
USE
SELECT ACT_S
USE
SELECT DETAIL
USE
SELECT STOCK
USE
SELECT TITLE
USE

RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º              à®£à ¬¬  ¯¥à¥¤ ç¨ ¤¥­¥£ ­  ®á­®¢­®© áª« ¤.              º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE S_Money
PARAMETERS pth, sdt   && ¯ãâì ¤«ï à¥§ã«ìâ â  ¨ ­ ç. ¤ â 
PRIVATE arr_t, rc_w

USE (base_path+"KASSA") ORDER TAG HRON

COPY STRUCTURE TO (pth+"KASSA")
SELECT 0
USE (pth+"KASSA") ALIAS DETAIL

SELECT KASSA
SEEK DTOS(sdt)
IF .NOT. FOUND()
  rc_w = RECNO(0)
ELSE
  rc_w = RECNO()
ENDIF

IF rc_w > 0 .AND. rc_w <= RECCOUNT()
  GO rc_w
  SCAN REST

    SCATTER TO arr_t
    SELECT DETAIL
    APPEND FROM ARRAY arr_t
    SELECT KASSA

  ENDSCAN

ENDIF

USE (base_path+"INCMONEY") ORDER TAG HRON

COPY STRUCTURE TO (pth+"INCMONEY")
SELECT DETAIL
USE (pth+"INCMONEY") ALIAS DETAIL

SELECT INCMONEY
SEEK DTOS(sdt)
IF .NOT. FOUND()
  rc_w = RECNO(0)
ELSE
  rc_w = RECNO()
ENDIF

IF rc_w > 0 .AND. rc_w <= RECCOUNT()
  GO rc_w
  SCAN REST

    SCATTER TO arr_t
    SELECT DETAIL
    APPEND FROM ARRAY arr_t
    SELECT INCMONEY

  ENDSCAN

ENDIF

USE
SELECT DETAIL
USE

RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º              à®£à ¬¬  ¯¥à¥¤ ç¨ áç¥â®¢ ­  ®á­®¢­®© áª« ¤.             º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE S_Bills
PARAMETERS pth, sdt   && ¯ãâì ¤«ï à¥§ã«ìâ â  ¨ ­ ç. ¤ â 
PRIVATE arr_t, rc_w

USE (base_path+"BILLS_D") ORDER TAG DOC_NUM
COPY STRUCTURE TO (pth+"BILLS_D")
SELECT 0
USE (pth+"BILLS_D") ALIAS DETAIL

SELECT 0
USE (base_path+"BILLS") ORDER TAG HRON
COPY STRUCTURE TO (pth+"BILLS")
SELECT 0
USE (pth+"BILLS") ALIAS TITLE

SELECT BILLS
SEEK DTOS(sdt)
IF .NOT. FOUND()
  rc_w = RECNO(0)
ELSE
  rc_w = RECNO()
ENDIF

IF rc_w > 0 .AND. rc_w <= RECCOUNT()
  GO rc_w
  SCAN REST

    SCATTER TO arr_t
    SELECT TITLE
    APPEND FROM ARRAY arr_t
    
    SELECT BILLS_D
    SEEK BILLS.FIRM+LEFT(DTOS(BILLS.DOC_DATE),4)+BILLS.DOC_NUM
    SCAN REST WHILE BILLS.FIRM = FIRM .AND. BILLS.DOC_DATE = DOC_DATE .AND. BILLS.DOC_NUM = DOC_NUM

      SCATTER TO arr_t
      SELECT DETAIL
      APPEND FROM ARRAY arr_t
      SELECT BILLS_D

    ENDSCAN

    SELECT BILLS

  ENDSCAN

ENDIF

USE
SELECT DETAIL
USE
SELECT TITLE
USE
SELECT BILLS_D
USE

RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º              à®£à ¬¬  ¯¥à¥¤ ç¨ á¯à®á  ­  ®á­®¢­®© áª« ¤.             º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE S_Demand
PARAMETERS pth, sdt   && ¯ãâì ¤«ï à¥§ã«ìâ â  ¨ ­ ç. ¤ â 
PRIVATE arr_t, rc_w, over_lst

USE (base_path+"DEMAND")

COPY STRUCTURE TO (pth+"DEMAND")
SELECT 0
USE (pth+"DEMAND") ALIAS DETAIL

over_lst = STR(YEAR(DATE()),4)+STR(t_num-n_over,5)
rc_w = STR(YEAR(DATE()),4)+STR(t_num,5)
SELECT DEMAND
SCAN

  IF .NOT. EMPTY(PASSED)
    IF PASSED < over_lst
      LOOP
    ENDIF
  ELSE
    REPLACE PASSED WITH rc_w
  ENDIF
  SCATTER TO arr_t
  SELECT DETAIL
  APPEND FROM ARRAY arr_t
  SELECT DEMAND

ENDSCAN

USE
SELECT DETAIL
USE

RETURN
