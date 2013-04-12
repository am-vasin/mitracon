*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Cus_Sum       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                แ็ฅโ ญ ซจ็ญฎฉ แใฌฌ๋ ฏฎ ชซจฅญโใ ง  คฅญ์.                บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 09/20/2000 ออผ
PROCEDURE Cus_Sum
PARAMETERS pCus, pDate, pFirm

PRIVATE s_sav, opend, tmpSum

s_sav = SELECT()
opend = USED("CUS_SUM")
IF opend
  SELECT CUS_SUM
ELSE
  SELECT 0
  USE (base_path+"INCMONEY") AGAIN ALIAS CUS_SUM ORDER TAG CUS_CODE
ENDIF
= SEEK(pCus)
SUM REST WHILE CUS_CODE = pCus ;
    FOR DOC_TYPE $ " " .AND. DOC_DATE = pDate .AND. pFirm = FIRM ;
    MONEY_R TO tmpSum
IF .NOT. opend
  USE
ENDIF
SELECT (s_sav)
RETURN tmpSum
