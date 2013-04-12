*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Get_Own       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ            ฎซใ็จโ์ แโเฎชใ จง ไ ฉซ  แฎกแโขฅญญ๋ๅ ฏ เ ฌฅโเฎข.            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 03.05.2001 ออผ
PROCEDURE Get_Own
PARAMETERS id

PRIVATE s_sav, r_value
IF .NOT. File_O(prmo_path+"ARM_MODE.DBF")
  RETURN ""
ENDIF
s_sav = SELECT()
SELECT 0
USE (prmo_path+"ARM_MODE") ORDER TAG ID_CODE
IF SEEK(PADR(id,FSIZE("ID_CODE")))
  r_value = VALUE
ELSE
  r_value = ""
ENDIF
USE
SELECT (s_sav)

RETURN r_value
