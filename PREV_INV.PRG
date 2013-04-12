*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  Prev_Inv      §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                à¥¤¢ à¨â¥«ì­ë© ¯®¤áç¥â áã¬¬ë ­ ª« ¤­®©.                º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 11.01.2006 ÍÍ¼
PROCEDURE Prev_Inv
PARAMETERS prmList	&& ®¬¥à á¯¨áª  (C)

PRIVATE s_sav, tmpName, tmpPrdList, i, tmpNDS, tmpQnt, tmpRest, tmpGTD

m.tmpName = SYS(3)
m.tmpName = "XXX"
m.s_sav = SELECT()

DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
SELECT 0
CREATE DBF(m.tmpo_path+m.tmpName) ;
	(	CODE	N( 7),   ;
		GTD		C(50),   ;
		QNT		N(10),   ;
		PRICE_0	N(15, 2),;
		SUM_0	N(15, 2),;
		SUM_H	N(15, 2) )

USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_6111
INDEX ON STR(CODE)+GTD TAG CODE

SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS LT_6111 ORDER TAG LST_NUM
SEEK VAL(m.prmList)
IF IS_USD
  m.tmpNDS = 0
ELSE
  m.tmpNDS = m.nds_flt
ENDIF

m.tmpPrdList = ""
IF m.nds_flt # 0
  USE (m.base_path+"B_GROUPS") SHARED AGAIN ALIAS BG_6111
  SCAN
    m.tmpPrdList = m.tmpPrdList+" "+DEV_LIST
  ENDSCAN
ENDIF
m.tmpPrdList = m.tmpPrdList+" "

USE (m.base_path+"ACT_S") SHARED AGAIN ALIAS PT_6111 ORDER TAG SALE
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS ST_6111 ORDER TAG CODE
SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ALIAS LD_6111 ORDER TAG LIST
SET RELATION TO CODE INTO ST_6111
SEEK m.prmList
SCAN REST WHILE LIST_NUM = m.prmList FOR QNT > 0
  DO CASE
  CASE m.nds_flt = 0
    SELECT TMP_6111
    APPEND BLANK
    REPLACE	CODE	WITH LD_6111.CODE,		;
			QNT		WITH LD_6111.QNT,		;
			PRICE_0	WITH LD_6111.USD_PRICE
  CASE " "+ALLTRIM(STR(ST_6111.PRO_CODE))+" " $ m.tmpPrdList
    SELECT TMP_6111
    APPEND BLANK
    REPLACE	CODE	WITH LD_6111.CODE,		;
			QNT		WITH LD_6111.QNT,		;
			PRICE_0	WITH ROUND(LD_6111.SAL_PRICE/(100+m.tmpNDS)*100, 2)
  OTHERWISE
    m.tmpQnt  = 0
    m.tmpRest = QNT
    SELECT PT_6111
    SEEK STR(LD_6111.CODE, 7)
    DO WHILE m.tmpRest > 0
      m.tmpQnt = MIN(m.tmpRest, QNT_REST)
      m.tmpGTD = ALLTRIM(GTD_NUM)+"-"+ALLTRIM(GTD_LINE)+" "+COUNTRY
      m.tmpGTD = PADR(m.tmpGTD, 50)
      m.tmpRest = m.tmpRest-m.tmpQnt
      SKIP
      SELECT TMP_6111
      IF .NOT. SEEK(STR(LD_6111.CODE)+m.tmpGTD)
        APPEND BLANK
        REPLACE	CODE	WITH LD_6111.CODE,	;
        		GTD		WITH m.tmpGTD,		;
				QNT		WITH m.tmpQnt,		;
				PRICE_0	WITH ROUND(LD_6111.SAL_PRICE/(100+m.tmpNDS)*100, 2)
	  ELSE
	    REPLACE QNT WITH QNT+m.tmpQnt
      ENDIF
      SELECT PT_6111
    ENDDO
  ENDCASE
  SELECT LD_6111
ENDSCAN

USE IN PT_6111
USE IN LD_6111
USE IN ST_6111
USE IN TMP_6111

RETURN
