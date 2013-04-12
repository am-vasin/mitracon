*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Drv_Lst       งเ กฎโ็จช €ญคเฅฉ  แจญ           12.02.97 18:16:17 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                       ‘ฏจแฎช คฎแโใฏญ๋ๅ คเ ฉขฎข.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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
