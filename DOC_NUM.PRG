*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  DOC_NUM       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ            เฎฃเ ฌฌ  ฏฎซใ็ฅญจ๏ ฎ็ฅเฅคญฎฃฎ ญฎฌฅเ  คฎชใฌฅญโ .            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 18.01.1999 ออผ
PROCEDURE DOC_NUM
PARAMETERS d_type, firm_p, arm_p, year_p
*
* d_type - โจฏ คฎชใฌฅญโ ;
* firm_p - ชฎค ไจเฌ๋;
* arm_p  - ชฎค เ กฎ็ฅฃฎ ฌฅแโ ;
* year_p - ฃฎค จซจ ค โ  คฎชใฌฅญโ .
*

PRIVATE s_sav, k, year_w, res, sw1, sw2, sw3
year_w = "    "
DO CASE
CASE TYPE("year_p") = "C"
  year_w = year_p
CASE TYPE("year_p") = "N"
  year_w = STR(year_p,4)
CASE TYPE("year_p") = "D"
  year_w = STR(YEAR(year_p),4)
ENDCASE
s_sav = SELECT()
SELECT 0
USE (base_path+"CNTDESCR") ORDER TAG CODE ALIAS DC_9118 AGAIN
IF .NOT. SEEK(PADR(d_type,10))
  USE
  SELECT (s_sav)
  RETURN 0
ENDIF
sw1 = FIRM_FILL
sw2 = ARM_FILL
sw3 = YEAR_FILL
USE (base_path+"DOC_CNT") ORDER TAG CNT_KEY ALIAS DC_9118 AGAIN
k = PADR(d_type,10)+IIF(sw1, firm_p, "   ")+ ;
                    IIF(sw2, arm_p, "   ")+ ;
                    IIF(sw3, year_w, "    ")

DO WHILE .NOT. FLOCK()
ENDDO
IF .NOT. SEEK(k)
  APPEND BLANK
  REPLACE DOC_CODE WITH PADR(d_type,10), ;
          FIRM_C   WITH IIF(sw1, firm_p, ""), ;
          ARM_C    WITH IIF(sw2, arm_p, ""),  ;
          YEAR     WITH IIF(sw3, year_w, ""), ;
          LAST_DOC WITH 1
  res = LAST_DOC
  UNLOCK
ELSE
  REPLACE LAST_DOC WITH LAST_DOC+1
  res = LAST_DOC
  UNLOCK  
ENDIF
USE
SELECT (s_sav)
RETURN res
