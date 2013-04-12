*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Cr_List       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ   ‘ฎงค ญจฅ แฏจแช  ญ  ฎแญฎข ญจจ ญ ชซ คญฎฉ (ญฅฎกๅฎคจฌฎ ฏเจ ขฎงขเ โฅ).    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 13.11.2005 ออผ
PROCEDURE Cr_List
PARAMETERS prmFirm, prmNum, prmDate

PRIVATE s_sav, nOrd

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"SALE_TIT") SHARED AGAIN ALIAS ST_5B13 ORDER TAG DOC_NUM
IF .NOT. SEEK(m.prmFirm+LEFT(DTOS(m.prmDate), 4)+prmNum)
  USE
  SELECT (m.s_sav)
  RETURN
ENDIF

SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS LT_5B13 ORDER TAG LST_NUM
IF SEEK(VAL(ST_5B13.LIST_NUM))
  USE IN ST_5B13
  USE IN LT_5B13
  SELECT (m.s_sav)
  RETURN
ENDIF

SELECT 0
USE (m.base_path+"ORD_T") SHARED AGAIN ALIAS OT_5B13 ORDER TAG DOC_NUM DESCENDING
IF .NOT. SEEK(ST_5B13.LIST_NUM)
  m.nOrd = 0
ELSE
  m.nOrd = ORD_NUM
ENDIF
USE

SELECT LT_5B13
APPEND BLANK
REPLACE LST_ARM		WITH ST_5B13.WHERE,		;
        LST_NUM		WITH VAL(ST_5B13.LIST_NUM),;
        CR_DAT		WITH DATE(),			;
        LST_NAME	WITH ST_5B13.CUS_NAME,	;
        LST_FLAGS	WITH ST_5B13.SAL_FLAGS,	;
        IS_CHECK	WITH ST_5B13.PAY_TYPE= "—",;
        CLI_CODE	WITH ST_5B13.CUS_CODE,	;
        FIRM		WITH ST_5B13.FIRM,		;
        SAL_NUM		WITH ST_5B13.DOC_NUM,	;
        SAL_DATE	WITH ST_5B13.DOC_DATE,	;
        WHO_SAL		WITH ST_5B13.WHO,		;
        PAY_FIRM	WITH ST_5B13.FIRM,		;
        PAY_TYPE	WITH ST_5B13.PAY_TYPE,	;
        PAY_NUM		WITH ST_5B13.PAY_DOC,	;
        PAY_DATE	WITH ST_5B13.PAY_DATE,	;
        R_TOTAL		WITH ST_5B13.TOTAL_R,	;
        KASSA		WITH ST_5B13.KASSA,		;
        ORD_NUM		WITH m.nOrd,			;
        WHO_CR		WITH ST_5B13.WHO,		;
        WHERE_CR	WITH ST_5B13.WHERE,		;
        DATE_REQ	WITH DATE(),			;
        ORD_COUNT	WITH m.nOrd

SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ALIAS LD_5B13

SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS STO_5B13 ORDER TAG CODE

SELECT 0
USE (m.base_path+"SALE") SHARED AGAIN ALIAS SD_5B13 ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STO_5B13

SEEK ST_5B13.FIRM+LEFT(DTOS(ST_5B13.DOC_DATE), 4)+ST_5B13.DOC_NUM
SCAN REST WHILE ST_5B13.FIRM = FIRM .AND. ;
                YEAR(ST_5B13.DOC_DATE) = YEAR(DOC_DATE) .AND. ;
                ST_5B13.DOC_NUM = DOC_NUM
  SELECT LD_5B13
  APPEND BLANK
  REPLACE LIST_NUM	WITH ST_5B13.LIST_NUM,	;
          CODE		WITH SD_5B13.CODE,		;
          PREFIX	WITH STO_5B13.PREFIX,	;
          NAME		WITH STO_5B13.NAME,		;
          PRO_CODE	WITH STO_5B13.PRO_CODE,	;
          PRODUCER	WITH STO_5B13.PRODUCER,	;
          ADDRESS	WITH STO_5B13.ADDRESS,	;
          QNT		WITH SD_5B13.QNT,		;
          QNT_REQ	WITH SD_5B13.QNT,		;
          USD_DEF	WITH SD_5B13.PRICE_D_L,	;
          DEF_PRICE	WITH SD_5B13.PRICE_R_L,	;
          USD_PRICE	WITH SD_5B13.PRICE_D,	;
          SAL_PRICE	WITH SD_5B13.PRICE_R
  SELECT SD_5B13
ENDSCAN

USE IN SD_5B13
USE IN ST_5B13
USE IN LD_5B13
USE IN LT_5B13
USE IN STO_5B13

SELECT (m.s_sav)

RETURN
