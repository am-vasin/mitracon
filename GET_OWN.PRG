*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Get_Own      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�            ���竍�碎 痰牀�� ├ ����� 甌＝癶キ�諷 �����モ牀�.            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 03.05.2001 様�
PROCEDURE Get_Own
PARAMETERS id

PRIVATE s_sav, r_value
IF .NOT. File_O(prmo_path+"ARM_MODE.DBF")
  RETURN ""
ENDIF
s_sav = SELECT()
SELECT 0
USE (prmo_path+"ARM_MODE") ORDER TAG ID_CODE
IF SEEK(PADR(id,FSIZE("ID_CODE")))
  r_value = VALUE
ELSE
  r_value = ""
ENDIF
USE
SELECT (s_sav)

RETURN r_value
