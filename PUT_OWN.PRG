*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Put_Own      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�             ������碎 痰牀�� � ���� 甌＝癶キ�諷 �����モ牀�.             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 03.05.2001 様�
PROCEDURE Put_Own
PARAMETERS id, p_value

PRIVATE s_sav
s_sav = SELECT()
SELECT 0
IF .NOT. File_O(base_path+"ARM_MODE.DBF")
  USE (base_path+"SYS_PARM")
  COPY STRUCTURE WITH CDX TO (prmo_path+"ARM_MODE")
ENDIF
USE (prmo_path+"ARM_MODE") ORDER TAG ID_CODE
IF .NOT. SEEK(PADR(id,FSIZE("ID_CODE")))
  APPEND BLANK
  REPLACE ID_CODE WITH id, VALUE WITH p_value
ELSE
  REPLACE VALUE WITH p_value
ENDIF
USE
SELECT (s_sav)

RETURN
