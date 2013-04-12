*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  Add_Mit       §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                   ‚ª«îç¥­¨¥ ­®¬¥­ª« âãàë Œ¨â¨­  ¢ DMS                  º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 14.04.2009 ÍÍ¼
PROCEDURE Add_Mit
PARAMETERS srcFile

PRIVATE s_sav, tmpCode, is_Dig, cSize

IF .NOT. File_O(m.srcFile+".dbf")
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
m.is_Dig = TYPE("TMP_DMS.CODE") = "N"
m.cSize = FSIZE("CODE", "TMP_DMS")
SCAN FOR QNT > 0
  IF m.is_Dig
    m.tmpCode = CODE
  ELSE
    m.tmpCode = ALLTRIM(STR(CODE))
    m.tmpCode = PADR(m.tmpCode, m.cSize)
  ENDIF
  SELECT TMP_DMS
  APPEND BLANK
  REPLACE	CLASS		WITH STO_TERR.CLASS,	;
  			CLASS_DKO	WITH CLS_TERR.NAME,		;
  			CODE		WITH m.tmpCode,			;
  			NAME		WITH STO_TERR.NAME,		;
  			PRODUCER	WITH STO_TERR.PRODUCER,	;
  			QNT			WITH TERRA.QNT,			;
  			PRICE1		WITH TERRA.PRICE,		;
  			SUP_DATE	WITH "‘ª« ¤ ü 2"
  SELECT TERRA
ENDSCAN

USE
USE IN CLS_TERR
USE IN STO_TERR
SELECT (m.s_sav)

RETURN
