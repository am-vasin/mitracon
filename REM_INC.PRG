*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Rem_Inc      ��о�｀砒┴ ��むォ ��瓱�           12.01.98 17:02:54 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       ��玻オ罔� �爬ガ�諷 ��皰�.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Rem_Inc
PRIVATE s1, s2

SET DELE ON
SET EXCLU OFF

USE (base_path+"ACT_S") ORDER TAG DOC_NUM
SELECT 0
USE (base_path+"ACT_S_T")

SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO ACT_S

CLEAR

SCAN
  @ 10, 10 SAY DOC_DATE &&&&&&&&&&&&&&&&&&&&&&&
  SELECT ACT_S
  s1 = 0
  s2 = 0
  SCAN REST WHILE DOC_NUM = ACT_S_T.DOC_NUM .AND. DOC_DATE = ACT_S_T.DOC_DATE
    s1 = s1+QNT_INC*PRICE_R
    s2 = s2+QNT_INC*NDS
  ENDSCAN
  SELECT ACT_S_T
  IF s1 = TOTAL_H .AND. s2 = TOTAL_NDS  && �瓮 � ��瑙お�, ��腑�� �� �キ錺�!
    LOOP
  ENDIF
  ? DTOC(DOC_DATE)+" "+DOC_NUM+STR(TOTAL_H,13,2)+STR(s1,13,2)+"     "+  ;
      STR(TOTAL_NDS,13,2)+STR(s2,13,2)
  REPLACE TOTAL_H WITH s1, TOTAL_NDS WITH s2, TOTAL_0 WITH s1-s2
ENDSCAN

CLOSE DATA

CLEAR
