*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Date_Dig     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                      ��え牀→� ��硅 � 矗� 瓱�〓��                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 13.02.2009 様�
PROCEDURE Date_Dig
PARAMETERS prmDate

PRIVATE digs, res, tmpV

m.digs = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
m.tmpV = YEAR(m.prmDate)
m.tmpV = m.tmpV%100
m.res = SUBSTR(m.digs, m.tmpV, 1)
m.tmpV = MONTH(m.prmDate)
m.res = m.res+SUBSTR(m.digs, m.tmpV, 1)
m.tmpV = DAY(m.prmDate)
m.res = m.res+SUBSTR(m.digs, m.tmpV, 1)

RETURN m.res
