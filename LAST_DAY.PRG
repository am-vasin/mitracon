*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Last_Day      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                    ฎแซฅคญจฉ ง ชเ๋โ๋ฉ คฅญ์ โ กฅซ๏.                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 04.04.2001 ออผ
PROCEDURE Last_Day
PARAMETERS p_code

PRIVATE s_sav, retDate, sv_date

retDate = {}
sv_date = SET("DATE")
SET DATE GERMAN
s_sav = SELECT()
SELECT 0
USE (base_path+"LS_TABLE") ALIAS LD_1404 AGAIN ORDER TAG PS_CODE DESCENDING
SET FILTER TO .NOT. EMPTY(WHO)
IF SEEK(STR(p_code,5))
  retDate = GOMONTH(CTOD("01."+STR(MONTH,2)+"."+STR(YEAR,4)),1)-1
ENDIF

USE
SELECT (s_sav)
SET DATE (sv_date)

RETURN retDate
