*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Add_Mega     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�             ���鈑キ┘ ���キ���矣琺 �ィ���オ矗��┴� � DMS               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 25.04.2009 様�
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
  			SUP_DATE	WITH "����,���� ����������"
*  			SUP_DATE	WITH "��"+IIF("VITR"$TERRA.STOCK, "(�)", "")+" 4 き�"7
  SELECT TERRA
ENDSCAN

USE
USE IN CLS_TERR
USE IN STO_TERR
SELECT (m.s_sav)

RETURN
