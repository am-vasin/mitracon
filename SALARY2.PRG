*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Salary2       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                         ‘คฅซ์ญ ๏ ็ แโ์ ฎฏซ โ๋                          บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 04.04.2001 ออผ
PROCEDURE Salary2
PARAMETERS p_code, n_mn

PRIVATE s_sav, tmpM, tmpSum

s_sav = SELECT()
SELECT 0

USE (base_path+"SLR_MOVE") ORDER TAG PS_CODE1 DESCENDING ALIAS SM_1404 AGAIN
SET FILTER TO CODE = 2

tmpSum = 0
IF SEEK(STR(p_code,5))
  tmpM = Last_Day(p_code)
  tmpM = YEAR(tmpM)*12+MONTH(tmpM)
  SCAN REST WHILE PS_CODE = p_code .AND. tmpM-YEAR*12-MONTH < n_mn
    IF tmpM-YEAR*12-MONTH >= 0
      tmpSum = tmpSum+Crpt_2(TOTAL)
    ENDIF
  ENDSCAN
ENDIF

USE
SELECT (s_sav)

RETURN ROUND(tmpSum/n_mn,2)
