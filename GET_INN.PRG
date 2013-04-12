*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Get_INN       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                          ฎซใ็ฅญจฅ  จ .                          บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 02.09.2005 ออผ
PROCEDURE Get_INN
PARAMETERS c_code, prmINN, prmKPP

*
* c_code - ชฎค ชซจฅญโ 
* prmINN - 
* prmKPP - 
*

PRIVATE s_sav

m.s_sav = SELECT()
SELECT 0
USE m.base_path+"CUS_BIL" ORDER TAG CUS_CODE SHARED AGAIN ALIAS CB_5902
m.prmINN = SPACE(FSIZE("INN"))
m.prmKPP = SPACE(FSIZE("KPP"))
IF m.c_code # 0
  IF SEEK(m.c_code)
    m.prmINN = CB_5902.INN
    m.prmKPP = CB_5902.KPP
  ENDIF
ENDIF
USE
SELECT (m.s_sav)

RETURN
