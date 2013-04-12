*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Add_T         งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                 ฎก ขซฅญจฅ ฌจญใโ ช แชซ คแชฎฌใ ขเฅฌฅญจ.                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 12.01.2000 ออผ
PROCEDURE Add_T
PARAMETERS date_0, time_0, mnts, date_res, time_res

IF mnts <= 0
  date_res = {}
  time_res = ""
  RETURN
ENDIF
PRIVATE time_0w, b_stock_w, e_stock_w, d_len, m, h
b_stock_w = VAL(ALLTRIM(LEFT(b_stock,2)))*60+VAL(ALLTRIM(SUBSTR(b_stock,4,2)))
e_stock_w = VAL(ALLTRIM(LEFT(e_stock,2)))*60+VAL(ALLTRIM(SUBSTR(e_stock,4,2)))
d_len     = e_stock_w - b_stock_w
time_0w = VAL(ALLTRIM(LEFT(time_0,2)))*60+VAL(ALLTRIM(SUBSTR(time_0,4,2)))
IF mnts >= 0
  date_res = date_0+FLOOR(mnts/d_len)
  time_res = mnts%d_len
  time_res = time_0w+time_res
  IF time_res > e_stock_w
    date_res = date_res+1
    time_res = b_stock_w+time_res-e_stock_w
  ENDIF
ELSE
  date_res = date_0-FLOOR(ABS(mnts)/d_len)
  time_res = ABS(mnts)%d_len
  time_res = time_0w-time_res
  IF time_res < b_stock_w
    date_res = date_res-1
    time_res = e_stock_w+time_res-b_stock_w
  ENDIF
  
ENDIF

h = FLOOR(time_res/60)
m = time_res%60
time_res = RIGHT(STR(100+h),2)+":"+RIGHT(STR(100+m),2)+":00"
RETURN
