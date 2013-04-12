*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Put_Sys       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                    ฏจแ โ์ แโเฎชใ ข ไ ฉซ ฏ เ ฌฅโเฎข.                   บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 12.01.2000 ออผ
PROCEDURE Put_Sys
PARAMETERS id, p_value

PRIVATE s_sav
s_sav = SELECT()
SELECT 0
USE (base_path+"SYS_PARM") ORDER TAG ID_CODE
IF .NOT. SEEK(PADR(id,FSIZE("ID_CODE")))
  APPEND BLANK
  REPLACE ID_CODE WITH id, VALUE WITH p_value
ELSE
  REPLACE VALUE WITH p_value
ENDIF
USE
SELECT (s_sav)

RETURN
