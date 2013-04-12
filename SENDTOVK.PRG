*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ SendToVK      งเ กฎโ็จช €ญคเฅฉ  แจญ           06.03.98 11:14:35 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                        ฎแ๋ซช  เ แๅฎค  ข  เๅจข.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE SendToVK
PARAMETERS d_firm, d_num, d_date

PRIVATE sel_rem, d_year, c_c, c_c1, c_sav, pr_v, d_no, tmpUser, flgs
PRIVATE savePrice, p_sav, tmpPrice, sum0, qnt0, zeroPrice, zeroPriceC
PRIVATE who_saled, tmpDir
who_saled = 0

savePrice = sale_mode = "Compel"

IF EMPTY(vk_arch)
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
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS SEND_TMP AGAIN

IF .NOT. SEEK(d_firm+d_year+d_num)
  USE
  SELECT (sel_rem)
  RETURN
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

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PERS_TMP AGAIN
SEEK SEND_TMP.WHO
tmpDir  = STR(DIRECT,3)
tmpUser = CODE
d_no = IIF(EMPTY(SEND_TMP.LIST_NUM), "999", STR(DEP_NO,3))
SELECT SEND_TMP

c_c = CUS_CODE
c_c1 = IIF(FSIZE("CLI_CODE") = 0, 0, CLI_CODE)

USE (vk_arch) ALIAS SEND_RES AGAIN

IF FSIZE("PRICE_DC") = 0
  savePrice = .F.
ENDIF

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS SEND_TMP AGAIN
IF savePrice
  IF LEFT(d_num, 1) = "B"
    SELECT 0
    USE (base_path+"ACT_S") AGAIN ALIAS PARTY_D ORDER TAG SALE
    SELECT SEND_TMP
    SET RELATION TO STR(CODE,7) INTO PARTY_D
  ELSE
    SELECT 0
    USE (base_path+"ACT_S") AGAIN ALIAS PARTY_D ORDER TAG PARTY
    SELECT SEND_TMP
    SET RELATION TO PARY INTO PARTY_D
  ENDIF
ENDIF

SEEK d_firm+d_year+d_num
c_sav = 0
STORE "          " TO p_sav, tmpPrice
zeroPrice = .F.
zeroPriceC = Crpt_1(0, 10, 4)
SCAN REST WHILE d_firm = FIRM .AND. d_num = DOC_NUM .AND. d_year = LEFT(DTOS(DOC_DATE),4)
  IF savePrice
    tmpPrice = Crpt_2(PARTY_D.PRICE_DC)
  ENDIF
  SELECT SEND_RES
  IF c_sav # SEND_TMP.CODE    && .OR. p_sav # tmpPrice
    sum0 = 0
    qnt0 = 0
    zeroPrice = .F.
    APPEND BLANK
*    IF SEND_TMP.PRICE_D # 0 .AND. LEFT(SEND_TMP.DOC_NUM,1) = sys_char
      pr_v = SEND_TMP.PRICE_D
*    ELSE
*      pr_v = ROUND(SEND_TMP.PRICE_R/kurs_3,5)
*    ENDIF
    REPLACE FIRM       WITH SEND_TMP.FIRM,     ;
            DOC_NUM    WITH SEND_TMP.DOC_NUM,  ;
            DATE       WITH SEND_TMP.DOC_DATE, ;
            SUB_SYS    WITH LEFT(SEND_TMP.DOC_NUM,1), ;
            CUS_CODE   WITH c_c,               ;
            STO_CODE   WITH SEND_TMP.CODE,     ;
            QTY        WITH SEND_TMP.QNT,      ;
            PRICE      WITH pr_v,              ;
            RPRICE     WITH SEND_TMP.PRICE_R,  ;
            DEPARTMENT WITH d_no,              ;
            MAN_CODE   WITH tmpUser,           ;
            SAL_FLAGS  WITH flgs,              ;
            PRICE_DC   WITH zeroPriceC,        ;
            PRICE_TYPE WITH SEND_TMP.PRICE_TYPE
    IF FSIZE("CLI_CODE") # 0
      REPLACE CLI_CODE WITH c_c1
    ENDIF
    IF FSIZE("SAL_CODE") # 0
      REPLACE SAL_CODE WITH who_saled
    ENDIF
    IF .NOT. EMPTY(who_saled)
      = SEEK(who_saled, "PERS_TMP")
      tmpDir = STR(PERS_TMP.DIRECT, 3)
    ENDIF
    IF FSIZE("DIRECT") # 0
      REPLACE DIRECT WITH tmpDir
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
SELECT PERS_TMP
USE
IF savePrice
  SELECT PARTY_D
  USE
ENDIF

SELECT (sel_rem)

RETURN
