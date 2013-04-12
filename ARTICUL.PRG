*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Articul       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                     เฅฎกเ งฎข ญจฅ ชฎค  ข  เโจชใซ                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 25.12.2007 ออผ
PROCEDURE Articul
PARAMETERS prmCode

PRIVATE tmpNum, tmpRes

m.tmpNum = ALLTRIM(STR(prmCode))
m.tmpRes = ""
DO WHILE .NOT. EMPTY(m.tmpNum)
  IF LEN(m.tmpNum) <= 2
    m.tmpRes = m.tmpNum+m.tmpRes
    EXIT
  ENDIF
  m.tmpRes = "-"+RIGHT(m.tmpNum, 2)+m.tmpRes
  m.tmpNum = LEFT(m.tmpNum, LEN(m.tmpNum)-2)
ENDDO

RETURN PADL(m.tmpRes, 10)
