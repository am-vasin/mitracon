*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Get_Sys      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  ���竍�碎 痰牀�� ├ ����� �����モ牀�.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 12.01.2000 様�
PROCEDURE Get_Sys
PARAMETERS id

PRIVATE s_sav, r_value
s_sav = SELECT()
SELECT 0
USE (base_path+"SYS_PARM") ORDER TAG ID_CODE
IF SEEK(PADR(id,FSIZE("ID_CODE")))
  r_value = VALUE
ELSE
  r_value = ""
ENDIF
USE
SELECT (s_sav)

RETURN r_value
