*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Doc_Year     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                ��� か� ぎ�祠キ�� � 瓱�〓�讚�� 筮爼�皀.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 16.12.2004 様�
PROCEDURE Doc_Year
PARAMETERS pDate

DO CASE
CASE TYPE("m.pDate") = "N"
  RETURN STR(m.pDate,4)
CASE TYPE("m.pDate") = "C"
  RETURN m.pDate
CASE TYPE("m.pDate") = "D"
  RETURN STR(YEAR(m.pDate),4)
ENDCASE
RETURN ""
