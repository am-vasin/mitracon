*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      蹍������ �瓣�｀Δキ�� ��щ.                     �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE C_Exclu
PRIVATE mss

USE PARMS

IF EXCLU # -1
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"踸 ���謔�� �瓣�｀え碎 ��с ����諷 △珮� �謠甅┼�瘡:"
  mss(3) = CHR(0)+"��� �� °�� ��゛���牀����!"
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

REPLACE EXCLU WITH 1

CLOSE DATABASES
RETURN
