*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� K_Refr       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       ｡��←キ┘ �窶�� ぎ�����.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 30.03.1999 様�
PROCEDURE K_Refr

PRIVATE s_sav
s_sav = SELECT()
SELECT 0
USE (base_path+"CURS_SET") ORDER TAG HRON DESCENDING ALIAS CRS_9330 AGAIN
m.kurs   = Get_Curs("CURS","CRS_9330")
m.kurs_b = Get_Curs("CURS_B","CRS_9330")
m.kurs_3 = Get_Curs("CURS_N","CRS_9330")
m.kurs_mmvb = Get_Curs("CURS_MMVB","CRS_9330")
USE
SELECT (s_sav)
RETURN
