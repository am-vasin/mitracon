*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Add_Mega      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ             ชซ๎็ฅญจฅ ญฎฌฅญชซ โใเ๋ ฅฃ ซฅชโเฎญจชจ ข DMS               บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 25.04.2009 ออผ
PROCEDURE Add_Mega
PARAMETERS srcFile

PRIVATE s_sav

IF .NOT. File_O(m.srcFile)
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
  			NAME		WITH TERRA.NAME,		;
  			PRODUCER	WITH TERRA.PRODUCER,	;
  			CORPUS		WITH TERRA.CORPUS,		;
  			QNT			WITH TERRA.QNT,			;
  			PRICE1		WITH TERRA.PRICE,		;
  			SUP_DATE	WITH "‘,–…€ €"
*  			SUP_DATE	WITH ""+IIF("VITR"$TERRA.STOCK, "(ข)", "")+" 4 คญ๏"7
  SELECT TERRA
ENDSCAN

USE
USE IN CLS_TERR
USE IN STO_TERR
SELECT (m.s_sav)

RETURN
