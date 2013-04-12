*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  Rev_Bill      §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                 ‚®§¢à â­ë¥ áç¥â  ä ªâãàë ¯® ¯« â¥¦ ¬.                  º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 10.01.2009 ÍÍ¼
PROCEDURE Rev_Bill
PARAMETERS prmType, prmRec
*
*  prmType - â¨¯ ¢®§¢à â  "M" - ¯« â¥¦, "S" - ¯à®¤ ¦ , 
*  prmRec  - ­®¬¥à § ¯¨á¨ ¢ â ¡«¨æ¥.

PRIVATE s_sav, tmpSum, docSum, tmpDoc, tmpSub, tmpDate

m.s_sav = SELECT()
SELECT 0
IF m.prmType = "M"
  USE (m.base_path+"INCMONEY") SHARED AGAIN ALIAS RB_SOURCE
  GO prmRec
  m.tmpSum = ABS(MONEY_R)
  m.tmpDate = IIF(EMPTY(ENTER_DAT), DOC_DATE, ENTER_DAT)
ELSE
  USE (m.base_path+"SALE_TIT") SHARED AGAIN ALIAS RB_SOURCE
  GO prmRec
  m.tmpSum = MAX(TOTAL_R, 0)
  m.tmpDate = DOC_DATE
ENDIF
IF m.tmpSum = 0
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF
SELECT 0
USE (m.base_path+"PAY_BILL") SHARED AGAIN ALIAS RB_DOC ORDER TAG DOC_NUM DESCENDING
SELECT 0
USE (m.base_path+"PAY_BILL") SHARED AGAIN ORDER TAG REST ALIAS RB_REST

DO WHILE SEEK(STR(RB_SOURCE.CUS_CODE)+RB_SOURCE.FIRM) .AND. m.tmpSum > 0
  m.docSum = MIN(m.tmpSum, REST_SUM)
  m.tmpSum = m.tmpSum-m.docSum
  m.tmpDoc = DOC_NUM
  REPLACE REST_SUM WITH REST_SUM-m.docSum
*  m.tmpDoc = Doc_Num("PAY_BILL", RB_SOURCE.FIRM, m.arm, DATE())
*  m.tmpDoc = m.sys_char+STR(m.tmpDoc,9)
  SELECT RB_DOC
  SEEK RB_SOURCE.FIRM+m.tmpDoc
  m.tmpSub = VAL(SUB_DOC)+1
  m.tmpSub = STR(m.tmpSub, FSIZE("SUB_DOC"))
  APPEND BLANK
  REPLACE	FIRM		WITH RB_SOURCE.FIRM,	;
			DOC_NUM		WITH m.tmpDoc,			;
			SUB_DOC		WITH m.tmpSub,			;
			DOC_DATE	WITH m.tmpDate,			;
			DOC_TIME	WITH TIME(),			;
			CUS_CODE	WITH RB_SOURCE.CUS_CODE,;
			PAY_DOC		WITH RB_REST.PAY_DOC,	;
			PAY_YEAR	WITH RB_REST.PAY_YEAR,	;
			REV_TYPE	WITH m.prmType,			;
			REV_DOC		WITH RB_SOURCE.DOC_NUM,	;
			REV_YEAR	WITH LEFT(DTOS(RB_SOURCE.DOC_DATE), 4),;
			BILL_SUM	WITH -m.docSum,			;
			WHO			WITH m.user
  SELECT RB_REST
ENDDO

USE IN RB_DOC
USE IN RB_REST
USE IN RB_SOURCE

SELECT (m.s_sav)
RETURN
