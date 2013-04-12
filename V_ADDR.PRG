*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  V_Addr        งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                    ”ฎเฌจเฎข ญจฅ  คเฅแ  คซ๏ ฏฅ็ โจ.                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 03.07.2000 ออผ
PROCEDURE V_Addr
PARAMETERS parmAddr

IF sale_mode = "Compel"
  IF EMPTY(SUBSTR(parmAddr, 2, 1))
    RETURN parmAddr+"   "
  ELSE
    RETURN LEFT(parmAddr,2)+"-"+SUBSTR(parmAddr, 3, 2)+"-"+  ;
           SUBSTR(parmAddr, 5, 2)+"-"+SUBSTR(parmAddr, 7, 2)
  ENDIF
ENDIF

RETURN parmAddr
