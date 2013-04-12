*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  cNameOnl      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                  งข ญจฅ ชซ แแ  กฅง ใเฎขญ๏                             บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 04.04.2008 ออผ
PROCEDURE cNameOnl
PARAMETERS sName

PRIVATE i, ln

IF EMPTY(m.sName)
  RETURN ""
ENDIF
m.i = 1
m.ln = LEN(m.sName)
FOR m.i = 1 TO m.ln
  IF .NOT. SUBSTR(m.sName, m.i, 1) $ " 0123456789"
    EXIT
  ENDIF
ENDFOR

RETURN SUBSTR(m.sName, m.i)
