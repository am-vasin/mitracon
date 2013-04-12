*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Leg_Firm      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                 ฎฏใแโจฌฎแโ์ ฏเฅคฏเจ๏โจ๏ คซ๏ ชซจฅญโ .                  บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 20.12.2000 ออผ
PROCEDURE Leg_Firm
PARAMETERS c_code, f_code

PRIVATE s_sav, tmpType, tmpRet

s_sav = SELECT()
SELECT 0
USE base_path+"CLI_TYPE" AGAIN ALIAS CT_0C20 ORDER TAG CUS_DEP
IF .NOT. SEEK(STR(c_code)+STR(user_dep,3))
  tmpType = "  0"
ELSE
  tmpType = CT_0C20.TYPE
ENDIF
USE base_path+"CUS_LINK" AGAIN ALIAS CT_0C20 ORDER TAG TYPE
tmpRet = .NOT. SEEK(STR(user_dep,3)+tmpType+f_code)
USE
SELECT (s_sav)

RETURN tmpRet
