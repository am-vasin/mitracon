*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� FirmKKM      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   軺く爬閧┘ � ��瘁��覃 �������...                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 09/14/2000 様�
PROCEDURE FirmKKM
PARAMETERS f_code

PRIVATE s_sav, r_val

s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") AGAIN ALIAS FRM_0914 ORDER TAG FIRM_CODE
= SEEK(f_code)
r_val = FRM_0914.KKM
USE
SELECT (s_sav)

RETURN r_val
