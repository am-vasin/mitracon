*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Get_Sto      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*� ���竍�碎 �����モ琺 瓷����: ��瘠 ��｀硅 � �┃. ∇ガ� �÷�｀皖� ��錢��.  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 12.01.2000 様�
PROCEDURE Get_Sto
PARAMETER queue_t

PRIVATE buf, tmp, s_sav
buf = PADR(Get_Sys("STOCK"), 40)

tmp = SUBSTR(buf,1,5)
b_stock = IIF(EMPTY(tmp), "00:00", tmp)

tmp = SUBSTR(buf,7,5)
e_stock = IIF(EMPTY(tmp), "23:59", tmp)

*tmp = SUBSTR(buf,13,5)
*t_stock = VAL(ALLTRIM(tmp))
s_sav = SELECT()
SELECT 0
USE base_path+"QUEUE"
LOCATE FOR Q_CODE = queue_t
IF .NOT. FOUND()
  t_stock = 0
ELSE
  t_stock = Q_TIME
ENDIF
USE
SELECT (s_sav)

RETURN
