*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  Pay_Bill      §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                ‘®§¤ ­¨¥ áç¥â -ä ªâãàë ¯à¨ ¡/­ ¯« â¥¦¥.                 º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 10.01.2009 ÍÍ¼
PROCEDURE Pay_Bill
PARAMETERS prmRec

PRIVATE s_sav, tmp_sum, tmpDoc

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"INCMONEY") SHARED AGAIN ALIAS PB_MONEY
GO m.prmRec
IF DOC_TYPE # "1"
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF
IF MONEY_R > 0
  SELECT 0
  USE (m.base_path+"CUS_REST") SHARED AGAIN ORDER TAG CUS_CODE ALIAS PB_REST
  SEEK STR(PB_MONEY.CUS_CODE)+PB_MONEY.FIRM
  m.tmp_sum = MIN(PB_MONEY.MONEY_R, PB_REST.ACC_SUM+PB_REST.ACC_S_H)
  IF m.tmp_sum > 0
    USE (m.base_path+"PAY_BILL") SHARED AGAIN ALIAS PB_DOC
    m.tmpDoc = Doc_Num("PAY_BILL", PB_MONEY.FIRM, m.arm, DATE())
    m.tmpDoc = RIGHT(DTOC(DATE()), 2)+STR(m.tmpDoc,8)
    APPEND BLANK
    REPLACE	FIRM		WITH PB_MONEY.FIRM,		;
			DOC_NUM		WITH m.tmpDoc,			;
			DOC_DATE	WITH IIF(EMPTY(PB_MONEY.ENTER_DAT), DATE(), PB_MONEY.ENTER_DAT),;
			DOC_TIME	WITH TIME(),			;
			CUS_CODE	WITH PB_MONEY.CUS_CODE,	;
			PAY_DOC		WITH PB_MONEY.DOC_NUM,	;
			PAY_YEAR	WITH LEFT(DTOS(PB_MONEY.DOC_DATE), 4),;
			BILL_SUM	WITH m.tmp_sum,			;
			REST_SUM	WITH m.tmp_sum,			;
			WHO			WITH m.user
    USE
  ELSE
    USE
  ENDIF
ELSE
  DO Rev_Bill WITH "M", RECNO("PB_MONEY")
ENDIF

USE IN PB_MONEY
SELECT (m.s_sav)
RETURN
