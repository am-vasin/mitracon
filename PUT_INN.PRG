*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Put_INN       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                            ฏจแ์  จ .                            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 02.09.2005 ออผ
PROCEDURE Put_INN
PARAMETERS c_code, prmINN, prmKPP

*
* c_code - ชฎค ชซจฅญโ 
* prmINN - 
* prmKPP - 
*

PRIVATE s_sav

IF m.c_code <= 0
  RETURN
ENDIF
m.s_sav = SELECT()
SELECT 0
USE m.base_path+"CUS_BIL" ORDER TAG CUS_CODE SHARED AGAIN ALIAS CB_5902
IF .NOT. SEEK(m.c_code)
  APPEND BLANK
  REPLACE CUS_CODE WITH m.c_code
ENDIF
REPLACE INN WITH m.prmINN, ;
        KPP WITH m.prmKPP
IF EMPTY(CUS_NAME)
  USE m.base_path+"ACCOUNT" ORDER TAG CUS_CODE SHARED AGAIN ALIAS CUS_5902 IN 0
  IF SEEK(CUS_CODE, "CUS_5902")
    REPLACE CUS_NAME WITH CUS_5902.CUS_NAME
  ENDIF
  USE IN CUS_5902
ENDIF

USE

SELECT (m.s_sav)

RETURN
