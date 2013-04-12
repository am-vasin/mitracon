*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Delta_T       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                       งญฎแโ์ แชซ คแชฎฃฎ ขเฅฌฅญจ.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 12.01.2000 ออผ
PROCEDURE Delta_T
PARAMETERS date_1, time_1, date_0, time_0  && date_1-time_1-date_0-time_0

PRIVATE time_1w, time_0w, b_stock_w, e_stock_w
b_stock_w = VAL(ALLTRIM(LEFT(b_stock,2)))*60+VAL(ALLTRIM(SUBSTR(b_stock,4,2)))
e_stock_w = VAL(ALLTRIM(LEFT(e_stock,2)))*60+VAL(ALLTRIM(SUBSTR(e_stock,4,2)))
time_1w = VAL(ALLTRIM(LEFT(time_1,2)))*60+VAL(ALLTRIM(SUBSTR(time_1,4,2)))
time_0w = VAL(ALLTRIM(LEFT(time_0,2)))*60+VAL(ALLTRIM(SUBSTR(time_0,4,2)))

time_1w = MAX(time_1w, b_stock_w)
time_1w = MIN(time_1w, e_stock_w)
time_0w = MAX(time_0w, b_stock_w)
time_0w = MIN(time_0w, e_stock_w)
RETURN time_1w-time_0w+(date_1-date_0)*(e_stock_w-b_stock_w)
