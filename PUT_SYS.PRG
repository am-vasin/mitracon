*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Put_Sys      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   ������碎 痰牀�� � ���� �����モ牀�.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 12.01.2000 様�
PROCEDURE Put_Sys
PARAMETERS id, p_value

PRIVATE s_sav
s_sav = SELECT()
SELECT 0
USE (base_path+"SYS_PARM") ORDER TAG ID_CODE
IF .NOT. SEEK(PADR(id,FSIZE("ID_CODE")))
  APPEND BLANK
  REPLACE ID_CODE WITH id, VALUE WITH p_value
ELSE
  REPLACE VALUE WITH p_value
ENDIF
USE
SELECT (s_sav)

RETURN
