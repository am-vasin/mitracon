*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Rem_Kas      ��о�｀砒┴ ��むォ ��瓱�           14.01.98 09:29:43 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ��瘁����←キ┘ ��瘁� �爛く爬閧��                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Rem_Kas

USE (base_path+"PARMS")
d_s = DAT_START
USE (base_path+"KASSA_M")
SET FILTER TO DOC_DATE >= d_s
STORE 0 TO s00_r1, s00_d1
SUM SUM_R, SUM_D TO s00_r1, s00_d1
USE (base_path+"KASSA_S")
CLEAR

@ 10, 10 SAY "�����琺: ��瘍."+STR(SUM_D_S+s00_d1,15,4)+" �� ��Д"+STR(SUM_D, 15,4)
@ 12, 10 SAY "�磧��:   ��瘍."+STR(SUM_R_S+s00_r1,15,2)+" �� ��Д"+STR(SUM_R, 15,2)

REPLACE SUM_D_S WITH SUM_D-s00_d1
REPLACE SUM_R_S WITH SUM_R-s00_r1
CLEAR

RETURN
