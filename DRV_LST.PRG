*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Drv_Lst      ��о�｀砒┴ ��むォ ��瓱�           12.02.97 18:16:17 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       ���甌� ぎ痰祚�諷 む��〓�.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Drv_Lst
PRIVATE s, i, sv, err_lst, err_cod, d, res

s = "CDEFGHIJKLMNOPQRSTUVWXYZ"
i = 1
sv = SET("DEFAULT")
DIMENSION err_lst(1)
err_lst(1) = 1907
err_cod = 0
i = 1
res = "A:B:"
DO WHILE i <= LEN(s)
  err_cod = 0
  d = SUBSTR(s,i,1)+":"
  SET DEFAULT TO (d)
  IF err_cod = 0
    res = res+d
  ENDIF
  i = i+1
ENDDO
SET DEFAULT TO (sv)

RETURN res
