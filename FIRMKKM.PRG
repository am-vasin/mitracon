*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  FirmKKM       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                   เใคฏเจ๏โจฅ จ ช แแฎข๋ฉ  ฏฏ เ โ...                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 09/14/2000 ออผ
PROCEDURE FirmKKM
PARAMETERS f_code

PRIVATE s_sav, r_val

s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") AGAIN ALIAS FRM_0914 ORDER TAG FIRM_CODE
= SEEK(f_code)
r_val = FRM_0914.KKM
USE
SELECT (s_sav)

RETURN r_val
