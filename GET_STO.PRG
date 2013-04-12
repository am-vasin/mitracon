*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Get_Sto       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ ฎซใ็จโ์ ฏ เ ฌฅโเ๋ แชซ ค : ็ แ๋ เ กฎโ๋ จ ฌจญ. ขเฅฌ๏ ฎกเ กฎโชจ ง ๏ขชจ.  บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 12.01.2000 ออผ
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
