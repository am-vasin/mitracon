*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Is_Acc        งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                        เฎขฅเช  ญ ซจ็จ๏ คฎแโใฏ                         บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 17.10.2006 ออผ
PROCEDURE Is_Acc
PARAMETERS accType, us_code
*
*  accType - โจฏ คฎแโใฏ ,
*  us_code - ชฎค ฏฎซ์งฎข โฅซ๏ (ฅแซจ ญฅ ใช ง ญ, โฎ user).

PRIVATE s_sav, tmpUser

m.tmpUser = IIF(TYPE("m.us_code") = "N", m.us_code, m.user)

IF m.tmpUser = 0
 RETURN .T.
ENDIF

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"ACC_PERS") SHARED AGAIN ORDER TAG PERS_CODE ALIAS A_61017
IF .NOT. SEEK(m.tmpUser)
  USE
  SELECT (m.s_sav)
  RETURN .F.
ENDIF

m.tmpUser = .NOT. EMPTY(SUBSTR(ACCESS, m.accType, 1))
USE
SELECT (m.s_sav)
RETURN m.tmpUser
