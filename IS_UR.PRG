*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Is_Ur         งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                  ขซ๏ฅโแ๏ ซจ ชซจฅญโ ๎เจคจ็ฅแชจฌ ซจๆฎฌ                  บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 05.08.2003 ออผ
PROCEDURE Is_Ur
PARAMETERS c_code
PRIVATE s_sav, tmpUr

s_sav = SELECT()
SELECT 0
USE (base_path+"ACCOUNT") SHARED AGAIN ALIAS CUS_3805 ORDER TAG CUS_CODE
SEEK c_code
tmpUr = .NOT. MARK1 $ "12"
USE
SELECT (s_sav)

RETURN tmpUr
