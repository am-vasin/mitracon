*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Date_Dig      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                      ฎคจเฎขช  ค โ๋ ข โเจ แจฌขฎซ                       บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.02.2009 ออผ
PROCEDURE Date_Dig
PARAMETERS prmDate

PRIVATE digs, res, tmpV

m.digs = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
m.tmpV = YEAR(m.prmDate)
m.tmpV = m.tmpV%100
m.res = SUBSTR(m.digs, m.tmpV, 1)
m.tmpV = MONTH(m.prmDate)
m.res = m.res+SUBSTR(m.digs, m.tmpV, 1)
m.tmpV = DAY(m.prmDate)
m.res = m.res+SUBSTR(m.digs, m.tmpV, 1)

RETURN m.res
