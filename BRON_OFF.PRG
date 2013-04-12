*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Bron_Off      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                             งกเฎญจเฎข ญจฅ                             บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 12.11.2005 ออผ
PROCEDURE Bron_Off
PARAMETERS prmList
*
*  prmList - ญฎฌฅเ แฏจแช  (C)
*

PRIVATE s_sav, tmpName, mss, tmpZone, tmpNeg, tmpPos
PRIVATE tmpDoc, tmpCnt, ordList, ordLen

DO Scan_Ord WITH m.prmList
m.ordLen = 0
m.s_sav = SELECT()
SELECT 0
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	CODE	N( 7), ;
		ADDRESS	C( 8), ;
		LINK    N(10), ;
		ZONE	C( 2), ;
		QNT		N( 8), ;
		QNT_REZ	N( 8)  )

USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_5B12

SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS ST_5B12

SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS LT_5B12
SEEK VAL(m.prmList)

SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ORDER TAG LIST ALIAS LD_5B12
SET RELATION TO CODE INTO ST_5B12

SEEK m.prmList
SCAN REST WHILE LIST_NUM = m.prmList && FOR QNT_REQ # 0
  m.tmpZone = WhatZone(ST_5B12.ADDRESS)
  SELECT TMP_5B12
  APPEND BLANK
  REPLACE CODE		WITH LD_5B12.CODE,    ;
		  ADDRESS	WITH ST_5B12.ADDRESS, ;
		  LINK		WITH RECNO("LD_5B12"),;
		  ZONE		WITH m.tmpZone,       ;
		  QNT		WITH LD_5B12.QNT_REQ
  SELECT LD_5B12
ENDSCAN
SET RELATION TO

SELECT 0
USE (m.base_path+"ORD_D") SHARED AGAIN ORDER TAG DOC_NUM ALIAS OD_5B12
SET RELATION TO CODE INTO ST_5B12

SELECT 0
USE (m.base_path+"ORD_T") SHARED AGAIN ORDER TAG DOC_NUM ALIAS OT_5B12

SELECT TMP_5B12
INDEX ON CODE TAG CODE

SELECT OT_5B12
SEEK m.prmList
SCAN REST WHILE LIST_NUM = m.prmList FOR EMPTY(REQ_DATE)
  SELECT OD_5B12
  SEEK OT_5B12.LIST_NUM+STR(OT_5B12.ORD_NUM, 3)
  SCAN REST WHILE LIST_NUM = OT_5B12.LIST_NUM .AND. ORD_NUM = OT_5B12.ORD_NUM
    IF .NOT. SEEK(CODE, "TMP_5B12")
      SELECT TMP_5B12
      m.tmpZone = WhatZone(ST_5B12.ADDRESS)
      SELECT TMP_5B12
      APPEND BLANK
      REPLACE CODE		WITH OD_5B12.CODE,    ;
		      ADDRESS	WITH ST_5B12.ADDRESS, ;
		      ZONE		WITH m.tmpZone
      SELECT OD_5B12
    ENDIF
    REPLACE TMP_5B12.QNT_REZ WITH TMP_5B12.QNT_REZ+OD_5B12.QNT
  ENDSCAN
  SELECT OT_5B12
*
*	… “… “’…€’ € € ……€…  €……€…!
*
*  REPLACE STO_MARK  WITH m.user, ;
*          TERM_DATE WITH DATE(), ;
*          TERM_TIME WITH TIME()
ENDSCAN

SELECT TMP_5B12
DELETE ALL FOR QNT = 0 .AND. QNT_REZ = 0
PACK

IF RECCOUNT("TMP_5B12") = 0
  DO Scan_Ord WITH m.prmList
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"ฎ ํโฎฌใ แฏจแชใ ญจ็ฅฃฎ ญฅ ง กเฎญจเฎข ญฎ!"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN LD_5B12
  USE IN ST_5B12
  USE IN OD_5B12
  USE IN OT_5B12
  USE IN LT_5B12
  USE IN TMP_5B12
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
  SELECT (m.s_sav)
  RETURN
ENDIF

SELECT OD_5B12
SET RELATION TO

SELECT LD_5B12
SET ORDER TO

SELECT TMP_5B12
SET RELATION TO LINK INTO LD_5B12, CODE INTO ST_5B12
STORE 0 TO m.tmpNeg, m.tmpPos

SCAN FOR .NOT. EMPTY(QNT_REZ)
  IF m.tmpNeg+m.tmpPos = 0
    REPLACE LT_5B12.ORD_NUM WITH LT_5B12.ORD_NUM+1
    SELECT OT_5B12
    APPEND BLANK
    REPLACE LIST_NUM	WITH LD_5B12.LIST_NUM, ;
            LIST_NAME	WITH LT_5B12.LST_NAME, ;
            ORD_NUM		WITH LT_5B12.ORD_NUM,  ;
            ORD_DATE	WITH DATE(),           ;
            ORD_TIME	WITH TIME(),           ;
            WHO			WITH m.user,           ;
            WHERE		WITH m.arm

*
*            STO_MARK	WITH m.user,           ;
*            TERM_DATE   WITH DATE(),           ;
*            TERM_TIME   WITH TIME(),           ;
*            REQ_DATE    WITH DATE(),           ;
*            REQ_TIME    WITH TIME(),           ;
*

  ENDIF
  SELECT OD_5B12
  APPEND BLANK
  REPLACE LIST_NUM WITH OT_5B12.LIST_NUM, ;
          ORD_NUM  WITH OT_5B12.ORD_NUM,  ;
          CODE     WITH TMP_5B12.CODE,    ;
          QNT      WITH -TMP_5B12.QNT_REZ
  IF QNT < 0
    m.tmpNeg = m.tmpNeg+1
  ELSE
    m.tmpPos = m.tmpPos+1
  ENDIF
  IF .NOT. EMPTY(TMP_5B12.LINK)
    SELECT LD_5B12
    REPLACE QNT_REQ WITH QNT_REQ+OD_5B12.QNT
    SELECT ST_5B12
    REPLACE QNT      WITH QNT-OD_5B12.QNT, ;
            QNT_SAVE WITH QNT_SAVE+OD_5B12.QNT
  ENDIF
  SELECT TMP_5B12
ENDSCAN
IF m.tmpNeg+m.tmpPos # 0
  SELECT OT_5B12
  REPLACE OT_5B12.SIZE_NEG WITH m.tmpNeg, ;
          OT_5B12.SIZE_POS WITH m.tmpPos
ENDIF

SELECT TMP_5B12

INDEX ON ADDRESS TAG ADDRESS
GO TOP
m.tmpZone = ""
m.tmpDoc  = 0
m.tmpCnt  = 0
STORE 0 TO m.tmpNeg, m.tmpPos

SCAN FOR QNT-QNT_REZ # 0
  IF m.tmpCnt > m.ordLen .AND. m.ordLen > 0 .OR. .NOT. m.tmpZone == ZONE
    m.tmpZone = ZONE
    IF m.tmpNeg+m.tmpPos # 0
      REPLACE OT_5B12.SIZE_NEG WITH m.tmpNeg, ;
              OT_5B12.SIZE_POS WITH m.tmpPos
    ENDIF
    m.tmpDoc = m.tmpDoc+1
    DIMENSION ordList[m.tmpDoc]
    SELECT OT_5B12
    APPEND BLANK
    REPLACE LT_5B12.ORD_NUM   WITH LT_5B12.ORD_NUM+1, ;
            LT_5B12.ORD_COUNT WITH LT_5B12.ORD_COUNT+1
    ordList[m.tmpDoc] = LT_5B12.ORD_NUM

    REPLACE LIST_NUM	WITH LD_5B12.LIST_NUM,  ;
            LIST_NAME	WITH LT_5B12.LST_NAME,  ;
            ORD_NUM		WITH LT_5B12.ORD_NUM,   ;
            ORD_DATE	WITH DATE(),            ;
            ORD_TIME	WITH TIME(),            ;
            ZONE		WITH TMP_5B12.ZONE,     ;
            WHO			WITH m.user,            ;
            WHERE		WITH m.arm,             ;
            REQ_DATE	WITH DATE(),			;
            REQ_TIME	WITH TIME()
    m.tmpCnt = 0
    STORE 0 TO m.tmpNeg, m.tmpPos
  ENDIF
  m.tmpCnt = m.tmpCnt+1
  SELECT OD_5B12
  APPEND BLANK
  REPLACE LIST_NUM WITH OT_5B12.LIST_NUM, ;
          ORD_NUM  WITH OT_5B12.ORD_NUM,  ;
          CODE     WITH TMP_5B12.CODE,    ;
          QNT      WITH TMP_5B12.QNT_REZ-TMP_5B12.QNT
  IF QNT < 0
    m.tmpNeg = m.tmpNeg+1
  ELSE
    m.tmpPos = m.tmpPos+1
  ENDIF
  IF .NOT. EMPTY(TMP_5B12.LINK)
    REPLACE ST_5B12.QNT      WITH ST_5B12.QNT-QNT, ;
            ST_5B12.QNT_SAVE WITH ST_5B12.QNT_SAVE+QNT
    REPLACE LD_5B12.QNT_REQ  WITH LD_5B12.QNT_REQ+QNT
  ENDIF
  SELECT TMP_5B12
ENDSCAN
IF m.tmpNeg+m.tmpPos # 0
  SELECT OT_5B12
  REPLACE OT_5B12.SIZE_NEG WITH m.tmpNeg, ;
          OT_5B12.SIZE_POS WITH m.tmpPos
ENDIF

USE IN LD_5B12
USE IN ST_5B12
USE IN OD_5B12
USE IN OT_5B12
USE IN LT_5B12
USE IN TMP_5B12
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
SELECT (m.s_sav)
DO Scan_Ord WITH m.prmList
RETURN
