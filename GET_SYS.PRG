*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Get_Sys       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                  ฎซใ็จโ์ แโเฎชใ จง ไ ฉซ  ฏ เ ฌฅโเฎข.                  บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 12.01.2000 ออผ
PROCEDURE Get_Sys
PARAMETERS id

PRIVATE s_sav, r_value
s_sav = SELECT()
SELECT 0
USE (base_path+"SYS_PARM") ORDER TAG ID_CODE
IF SEEK(PADR(id,FSIZE("ID_CODE")))
  r_value = VALUE
ELSE
  r_value = ""
ENDIF
USE
SELECT (s_sav)

RETURN r_value
