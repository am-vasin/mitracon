*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                       ฎเเฅชๆจ๏ คฅฏฎงจโฎข.                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Rem_Cus

PRIVATE cnt, d_s, err, ab, sch, is_data, log_rec, err_cnt

*log_rec = Use_Log("เฎขฅเช  คฅฏฎงจโฎข")

USE (base_path+"PARMS")
sch = SCHEME(13,1)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,4)+","+ ;
      SCHEME(13,5)+","+ ;
      SCHEME(13,6)+","+ ;
      SCHEME(13,7)+","+ ;
      SCHEME(13,8)+","+ ;
      SCHEME(13,9)
d_s = DAT_START

DELETE FILE (log_path+"N_T_CF.DBF")
DELETE FILE (log_path+"N_T_CF.CDX")
CREATE DBF (log_path+"N_T_CF")  ;
   (  FIRM      C( 3),    ;
      COUNT_R   N(15,2),  ;
      COUNT_D   N(15,4),  ;
      REST_R    N(15,2),  ;
      REST_D    N(15,4)  )
INDEX ON FIRM TAG FIRM
SELECT 0
USE (base_path+"FIRMS")
SCAN
  SELECT N_T_CF
  APPEND BLANK
  REPLACE FIRM WITH FIRMS.FIRM_CODE
  SELECT FIRMS
ENDSCAN

*
*   โชเ๋ข ฅฌ ไ ฉซ๋, ชฎโฎเ๋ฅ ญใฆญ๋ ฏเจ ฏเฎขฅเชฅ
*
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE
SET FILTER TO DOC_DATE >= d_s

SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG SUP_CODE
SET FILTER TO DOC_DATE >= d_s

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG CUS_CODE
SET FILTER TO DOC_DATE >= d_s

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ACCOUNT")

PRIVATE ln, wd, tt, ft, w1, w2, w3, res
ab = .F.

cnt = 0

SCAN
  cnt = cnt+1
  @ 10, 10 SAY STR(cnt)+" "+CUS_NAME
  err = .F.
  SELECT N_T_CF
  SCAN
    IF SEEK(STR(ACCOUNT.CUS_CODE)+N_T_CF.FIRM, "CUS_REST")
      REPLACE COUNT_R WITH CUS_REST.ACC_S_S,  ;
              COUNT_D WITH CUS_REST.ACC_SD_S, ;
              REST_R  WITH CUS_REST.ACC_SUM+CUS_REST.ACC_S_H+CUS_REST.REST_SUM,  ;
              REST_D  WITH CUS_REST.ACC_SUMD+CUS_REST.ACC_SD_H
    ELSE
      REPLACE COUNT_R WITH 0, ;
              COUNT_D WITH 0, ;
              REST_R  WITH 0, ;
              REST_D  WITH 0
    ENDIF
  ENDSCAN
  IF ab
    EXIT
  ENDIF
  DO Cnt_Rest     &&  แ็ฅโ ฎแโ โ ช 
  SELECT N_T_CF
  SCAN FOR COUNT_D # REST_D .OR. COUNT_R # REST_R
    SELECT CUS_REST
    IF .NOT. SEEK(STR(ACCOUNT.CUS_CODE)+N_T_CF.FIRM)
      APPEND BLANK
      REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE, ;
              FIRM     WITH N_T_CF.FIRM
    ENDIF
    REPLACE ACC_SUM  WITH N_T_CF.COUNT_R-ACC_S_H-CUS_REST.REST_SUM,  ;
            ACC_SUMD WITH N_T_CF.COUNT_D-ACC_SD_H
    SELECT N_T_CF
  ENDSCAN
  SELECT ACCOUNT
ENDSCAN
ON KEY

is_data = .F.
CLOSE DATABASES

RETURN is_data

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    ฎคแ็ฅโ ฎแโ โช  ญ  คฅฏฎงจโฅ.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_Rest

SELECT INCMONEY
SEEK ACCOUNT.CUS_CODE
SCAN REST WHILE CUS_CODE = ACCOUNT.CUS_CODE
  IF .NOT. SEEK(FIRM, "N_T_CF")
    LOOP
  ENDIF
  DO CASE
  CASE INLIST(DOC_TYPE, " ", "1")    && / จซจ ฏเจๅ. ฎเคฅเ
    REPLACE N_T_CF.COUNT_R WITH N_T_CF.COUNT_R+MONEY_R
  CASE DOC_TYPE = "3"                && USD
    REPLACE N_T_CF.COUNT_D WITH N_T_CF.COUNT_D+MONEY_D
  ENDCASE
ENDSCAN

SELECT SALE_TIT
SEEK ACCOUNT.CUS_CODE
SCAN REST WHILE CUS_CODE = ACCOUNT.CUS_CODE
  IF .NOT. SEEK(FIRM, "N_T_CF")
    LOOP
  ENDIF
  REPLACE N_T_CF.COUNT_R WITH N_T_CF.COUNT_R-TOTAL_R
ENDSCAN

SELECT ACT_S_T
SEEK ACCOUNT.CUS_CODE
SCAN REST WHILE SUP_CODE = ACCOUNT.CUS_CODE
  IF .NOT. SEEK(FIRM, "N_T_CF")
    LOOP
  ENDIF
  IF VALUTA = "$"                && ใฏจซจ ง  คฎซซ เ๋
    REPLACE N_T_CF.COUNT_D WITH N_T_CF.COUNT_D+TOTAL_H
  ELSE
    REPLACE N_T_CF.COUNT_R WITH N_T_CF.COUNT_R+TOTAL_H
  ENDIF
ENDSCAN

RETURN
