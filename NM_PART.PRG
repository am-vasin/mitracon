*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Nm_Part       งเ กฎโ็จช CMP_LOG.PRG            บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                         ๋คฅซฅญจฅ ็ แโจ จฌฅญจ                          บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 05/18/2006 ออผ
PROCEDURE Nm_Part
PARAMETERS prmName, prmLen
*
*  prmName - จแๅฎคญฎฅ ญ งข ญจฅ,
*  prmLen  - โเฅกใฅฌ ๏ คซจญ 
*

PRIVATE tmpLen
m.tmpLen = 25
IF TYPE("m.prmLen") ="N"
  m.tmpLen = m.prmLen
ENDIF
IF LEN(m.prmName) <= m.tmpLen
  RETURN PADR(m.prmName, m.tmpLen)
ENDIF
IF EMPTY(SUBSTR(m.prmName, m.tmpLen+1))
  RETURN LEFT(m.prmName, m.tmpLen)
ENDIF
RETURN LEFT(m.prmName, m.tmpLen-1)+""
