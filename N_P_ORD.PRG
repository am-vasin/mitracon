*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  N_P_Ord       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ    ฌฅแโฎ ฏฅ็ โจ ง ๏ขชจ: ฏฅ็ โ์ แใฌฌ เญฎฉ ง ๏ขชจ + ฏเฅคข. แฎงค ญจฅ     บ
*บ                            ฅแซจ โเฅกใฅโแ๏.                             บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 25.11.2006 ออผ
PROCEDURE N_P_Ord
PARAMETERS prmList, prmOrd
*
* prmList - ‘ฏจแฎช (C)
* prmOrd  -  ๏ขช  (N)

PRIVATE s_sav
m.s_sav = SELECT()
SELECT 0
USE m.base_path+"ORD_T" SHARED AGAIN ORDER TAG DOC_NUM ALIAS ORD_6B25
IF .NOT. SEEK(m.prmList+STR(m.prmOrd, 3))
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF

IF EMPTY(REQ_DATE)
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF

IF EMPTY(CHECK_S)
  DO Ord_Sum WITH LIST_NUM
ENDIF

= SEEK(m.prmList+STR(m.prmOrd, 3))
DO P_Ord_S WITH LIST_NUM, CHECK_S

USE
SELECT (m.s_sav)
RETURN
