*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Ord_Sum       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                      ‘ฎงค ญจฅ จญโฅฃเ ซ์ญฎฉ ง ๏ขชจ                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 24.11.2006 ออผ
PROCEDURE Ord_Sum
PARAMETERS prmList, prmOrd

*
*	prmList - ญฎฌฅเ แฏจแช  (C);
*	prmOrd  - ญฎฌฅเ ง ๏ขชจ - ฅแซจ ใช ง ญ, โฎ แใฌฌ เญ ๏ ง ๏ช  ไฎเฌจเใฅโแ๏ โฎซ์ชฎ คซ๏ ญฅฅ
*
PRIVATE s_sav, tmpNum, Cr_OK, tmpKey

m.tmpKey = m.prmList+IIF(TYPE("m.prmOrd")="N", STR(m.prmOrd, 3), "")
m.s_sav  = SELECT()
m.Cr_OK = .F.
SELECT 0
USE m.base_path+"ORD_T" SHARED AGAIN ALIAS TIT_5B15 ORDER TAG DOC_NUM
IF .NOT. SEEK(m.tmpKey)
  USE
  SELECT(m.s_sav)
  RETURN .F.
ENDIF

m.tmpNum = SOrd_Num(m.prmList)
m.tmpNum = STR(m.tmpNum, FSIZE("CHECK_S"))

IF TYPE("m.prmOrd")="N"
  IF EMPTY(CHECK_S) .AND. EMPTY(STO_MARK)
    REPLACE CHECK_S WITH m.tmpNum
    m.Cr_OK = .T.
  ENDIF
ELSE
  SCAN REST WHILE TIT_5B15.LIST_NUM = m.prmList FOR EMPTY(CHECK_S) .AND. EMPTY(STO_MARK)
    REPLACE CHECK_S WITH m.tmpNum
    m.Cr_OK = .T.
  ENDSCAN
ENDIF

USE

SELECT (m.s_sav)

RETURN m.Cr_OK
