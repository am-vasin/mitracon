*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Rem_Sal      ��о�｀砒┴ ��むォ ��瓱�           12.01.98 16:07:54 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               蹍������ �ムメ腑�� �����き諷 � ���皀Ε�.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Rem_Sal
PRIVATE sw_1, sw_2, sw_3, s1, s2, s3

SET DELE ON
SET EXCLU OFF

USE (base_path+"SALE") ORDER TAG DOC_NUM
SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON

SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO SALE
SET RELATION TO PAY_TYPE+LEFT(DTOS(PAY_DATE),4)+PAY_DOC INTO INCMONEY ADDITIVE

CLEAR

SCAN
  @ 10, 10 SAY DOC_DATE &&&&&&&&&&&&&&&
  SELECT SALE
  s1 = 0
  SCAN REST WHILE DOC_NUM = SALE_TIT.DOC_NUM .AND. DOC_DATE = SALE_TIT.DOC_DATE
    s1 = s1+QNT*PRICE_R
  ENDSCAN
  SELECT SALE_TIT
  IF s1 = TOTAL_R   && �瓮 � ��瑙お�, ��腑�� �� �キ錺�!
    LOOP
  ENDIF
  ? DTOC(DOC_DATE)+" "+DOC_NUM+STR(TOTAL_R,15,2)+STR(s1,15,2)+  ;
      IIF(FOUND("INCMONEY"), STR(INCMONEY.MONEY_R,15,2), "")
  sw_1 = KASSA = TOTAL_R
  sw_2 = KASSA = INCMONEY.MONEY_R
  sw_3 = TOTAL_R = INCMONEY.MONEY_R
  REPLACE TOTAL_R WITH s1, KASSA WITH IIF(sw_1, s1, KASSA)
  IF FOUND("INCMONEY")
    SELECT INCMONEY
    IF sw_2
      REPLACE MONEY_R WITH SALE_TIT.KASSA
    ELSE
      IF sw_2
        REPLACE MONEY_R WITH SALE_TIT.TOTAL_R
      ENDIF
    ENDIF
    SELECT SALE_TIT
  ENDIF
ENDSCAN

CLOSE DATA
