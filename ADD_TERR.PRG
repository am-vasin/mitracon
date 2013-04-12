*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Add_Terr      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ             ชซ๎็ฅญจฅ ญฎฌฅญชซ โใเ๋ ’ฅเเ ซฅชโเฎญจชจ ข DMS              บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 14.04.2009 ออผ
PROCEDURE Add_Terr
PARAMETERS srcFile

PRIVATE s_sav

IF UPPER(RIGHT(srcFile, 3)) = "CSV"
  DO Terr_CSV WITH srcFile
  RETURN
ENDIF

m.s_sav = SELECT()
SELECT 0
USE m.base_path+"CLASSES" SHARED AGAIN ORDER TAG CODE ALIAS CLS_TERR
SELECT 0
USE m.base_path+"STOCK" SHARED AGAIN ORDER TAG CODE ALIAS STO_TERR
SET RELATION TO CLASS INTO CLS_TERR
SELECT 0
USE (m.srcFile) SHARED AGAIN ALIAS TERRA
SET RELATION TO CODE INTO STO_TERR
SCAN
  SELECT TMP_DMS
  APPEND BLANK
  REPLACE	CLASS		WITH STO_TERR.CLASS,	;
  			CLASS_DKO	WITH CLS_TERR.NAME,		;
  			CODE		WITH TERRA.CODE,		;
  			NAME		WITH STO_TERR.NAME,		;
  			PRODUCER	WITH STO_TERR.PRODUCER,	;
  			QNT			WITH TERRA.QNT_TER,		;
  			PRICE1		WITH TERRA.PRICE_1,		;
  			SUP_DATE	WITH "1 คฅญ์!"
  SELECT TERRA
ENDSCAN

USE
USE IN CLS_TERR
USE IN STO_TERR
SELECT (m.s_sav)

RETURN
