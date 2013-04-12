*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Put_Own       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ              ฏจแ โ์ แโเฎชใ ข ไ ฉซ แฎกแโขฅญญ๋ๅ ฏ เ ฌฅโเฎข.             บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 03.05.2001 ออผ
PROCEDURE Put_Own
PARAMETERS id, p_value

PRIVATE s_sav
s_sav = SELECT()
SELECT 0
IF .NOT. File_O(base_path+"ARM_MODE.DBF")
  USE (base_path+"SYS_PARM")
  COPY STRUCTURE WITH CDX TO (prmo_path+"ARM_MODE")
ENDIF
USE (prmo_path+"ARM_MODE") ORDER TAG ID_CODE
IF .NOT. SEEK(PADR(id,FSIZE("ID_CODE")))
  APPEND BLANK
  REPLACE ID_CODE WITH id, VALUE WITH p_value
ELSE
  REPLACE VALUE WITH p_value
ENDIF
USE
SELECT (s_sav)

RETURN
