*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Rem_acc       งเ กฎโ็จช €ญคเฅฉ  แจญ           14.01.98 09:29:43 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                       ฎแแโ ญฎขซฅญจฅ คฅฏฎงจโฎข.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Rem_Acc

SET DELE ON
SET EXCLU OFF
DO Set_Lwr

USE (base_path+"PARMS")
d_s = DAT_START

DELETE FILE TST_ACC.DBF
CREATE DBF TST_ACC  ;
     (  CUS_CODE  N( 8),  ;
        CUS_NAME  C(50),  ;
        SUM_B     N(15,2),;
        SUM_C     N(15,2),;
        SUM0_B    N(15,2),;
        SUM0_C    N(15,2),;
        SUMD_B    N(15,4),;
        SUMD_C    N(15,4)  )
        
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
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
CLEAR
cnt = 0
SCAN
  cnt = cnt+1
  @ 10, 10 SAY STR(cnt)
  SELECT TST_ACC
  APPEND BLANK
  REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE, ;
          CUS_NAME WITH ACCOUNT.CUS_NAME, ;
          SUM_B    WITH ACCOUNT.ACC_SUM+ACCOUNT.ACC_S_H, ;
          SUM0_B   WITH ACCOUNT.ACC_SUM0+ACCOUNT.ACC_S0_H, ;
          SUMD_B   WITH ACCOUNT.ACC_SUMD+ACCOUNT.ACC_SD_H, ;
          SUM_C    WITH ACCOUNT.ACC_S_S, ;
          SUM0_C   WITH ACCOUNT.ACC_S0_S, ;
          SUMD_C   WITH ACCOUNT.ACC_SD_S
  SELECT ACCOUNT
ENDSCAN
USE
SELECT TST_ACC

cnt = 0
SCAN
  cnt = cnt+1
  @ 10, 10 SAY STR(cnt)
  DO Cnt_Rest     &&  แ็ฅโ ฎแโ โ ช 
ENDSCAN
CLOSE DATABASES

IF .T. &&&&&&&&&&&&&&&&&&&&&&
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
SELECT 0
USE TST_ACC
SCAN
  IF .NOT. SEEK(CUS_CODE, "ACCOUNT")
    LOOP
  ENDIF
  SELECT ACCOUNT
  REPLACE ACC_S_S  WITH ACC_S_S +TST_ACC.SUM_B-TST_ACC.SUM_C, ;
          ACC_S0_S WITH ACC_S0_S+TST_ACC.SUM0_B-TST_ACC.SUM0_C, ;
          ACC_SD_S WITH ACC_SD_S+TST_ACC.SUMD_B-TST_ACC.SUMD_C
  SELECT TST_ACC
ENDSCAN
CLOSE DATA
ENDIF &&&&&&&&&&&&&&&&&&&&&&&&&&

CLEAR

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    ฎคแ็ฅโ ฎแโ โช  ญ  คฅฏฎงจโฅ.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_Rest

STORE 0 TO w1, w2, w3
SELECT INCMONEY
SEEK TST_ACC.CUS_CODE
SCAN REST WHILE CUS_CODE = TST_ACC.CUS_CODE
  DO CASE
  CASE INLIST(DOC_TYPE, " ", "1")    && / จซจ ฏเจๅ. ฎเคฅเ
    w1 = w1+MONEY_R
  CASE DOC_TYPE =  "2"               && ชฎก๋ ฏเจๅ. ฎเคฅเ
    w2 = w2+MONEY_R
  CASE DOC_TYPE = "3"                && USD
    w3 = w3+MONEY_D
  ENDCASE
ENDSCAN

SELECT SALE_TIT
SEEK TST_ACC.CUS_CODE
SCAN REST WHILE CUS_CODE = TST_ACC.CUS_CODE
  DO CASE
  CASE NEW_TYPE = " "                && / จซจ ฏเจๅ. ฎเคฅเ
    w1 = w1-TOTAL_R
  CASE NEW_TYPE =  "0"               && ชฎก๋ ฏเจๅ. ฎเคฅเ
    w2 = w2-TOTAL_R
  ENDCASE
ENDSCAN

SELECT ACT_S_T
SEEK TST_ACC.CUS_CODE
SCAN REST WHILE SUP_CODE = TST_ACC.CUS_CODE
  DO CASE
  CASE INC_TYPE = " "                && / จซจ ฏเจๅ. ฎเคฅเ
    w1 = w1+TOTAL_H
  CASE INC_TYPE =  "0"               && ชฎก๋ ฏเจๅ. ฎเคฅเ
    w2 = w2+TOTAL_H
  ENDCASE
ENDSCAN
SELECT TST_ACC
REPLACE SUM_C  WITH SUM_C+w1, ;
        SUM0_C WITH SUM0_C+w2, ;
        SUMD_C WITH SUMD_C+w3

RETURN
