*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  SOrd_Num      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                   ฎซใ็จโ์ ญฎฌฅเ จญโฅฃเ ซ์ญฎฉ ง ๏ขชจ                   บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 24.11.2006 ออผ
PROCEDURE SOrd_Num
PARAMETERS prmList

*
*	prmList - ญฎฌฅเ แฏจแช  (C)
*
PRIVATE s_sav, tmpNum

m.tmpNum = 0
m.s_sav  = SELECT()
SELECT 0
USE m.base_path+"ORD_T" SHARED AGAIN ALIAS O_TIT_5B15 ORDER TAG DOC_NUM
SEEK m.prmList
SCAN REST WHILE O_TIT_5B15.LIST_NUM = m.prmList
  m.tmpNum = MAX(m.tmpNum, VAL(O_TIT_5B15.CHECK_S))
ENDSCAN

USE

SELECT (m.s_sav)

RETURN m.tmpNum+1
