*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  To_Sale       §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º               ‚ë­¥á¥­¨¥ ¯®¬¥ç¥­­ëå ¯®§¨æ¨© ¤«ï ¯à®¤ ¦¨.                º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 02.05.2006 ÍÍ¼
PROCEDURE To_Sale
PARAMETERS n_list	&& ­®¬¥à á¯¨áª  (C)

PRIVATE s_sav, new_list, tmpName, tmpArr, mss, tmpZone, o_type
PRIVATE old_ord, new_ord, rc_count
PRIVATE tmpSum, tmpSumOrd, retRec

DO Scan_Ord WITH m.n_list		&& ‚ë¯à ¢«ï¥¬ ¯®¬¥âª¨ "­¥ á®¡¨à âì"
m.retRec = 0
m.tmpName = SYS(3)
m.new_list = ""
m.s_sav = SELECT()

SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS STO_6502
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	LINK	N(10), ;  
		CODE	N( 7), ;
		TMP_ORD	N( 5), ;
		ZONE	C( 2), ;
		QNT		N(10), ;
		QNT_REQ N(10), ;
		QNT_ORD	N(10)  )
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_BRON

SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ORDER TAG LIST ALIAS DET_6502

*
*   ƒ®â®¢¨¬ ¢à¥¬¥­­ë© á¯¨á®ª ¨ ¯à®¢¥àï¥¬ ­  ¤¥ä¨æ¨â
SEEK m.n_list
SCAN REST WHILE LIST_NUM = m.n_list FOR CODE # 0 .AND. .NOT. EMPTY(MARK_TMP) .AND. QNT > 0
  = SEEK(CODE, "STO_6502")
  IF STO_6502.QNT < MAX(QNT-QNT_REQ, 0)		&&  „¥ä¨æ¨â
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"‘à¥¤¨ ¯®¬¥ç¥­­ëå ¯®§¨æ¨© ¨¬¥¥â ¬¥áâ® ¤¥ä¨æ¨â..."
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    USE IN TMP_BRON
    USE IN DET_6502
    USE IN STO_6502
    DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
    SELECT (m.s_sav)
    RETURN m.retRec
  ENDIF
  m.tmpZone = WhatZone(STO_6502.ADDRESS)
  SELECT TMP_BRON
  APPEND BLANK
  REPLACE	LINK	WITH RECNO("DET_6502"),	;
			CODE	WITH DET_6502.CODE,		;
			ZONE	WITH m.tmpZone,			;
			QNT		WITH DET_6502.QNT,		;
			QNT_REQ	WITH DET_6502.QNT_REQ
  SELECT DET_6502
ENDSCAN

IF RECCOUNT("TMP_BRON") = 0
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"‚ë ­¥ ®â¬¥â¨«¨ ­¨ ®¤­®© áª« ¤áª®© ¯®§¨æ¨¨..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN TMP_BRON
  USE IN DET_6502
  USE IN STO_6502
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  SELECT (m.s_sav)
  RETURN m.retRec
ENDIF

SELECT TMP_BRON
INDEX ON CODE TAG CODE

SELECT 0
USE (m.base_path+"ORD_T") SHARED AGAIN ORDER TAG DOC_NUM ALIAS OT_6502

SELECT 0
USE (m.base_path+"ORD_D") SHARED AGAIN ORDER TAG DOC_NUM ALIAS OD_6502

*
*  à®¢¥àª  ª®àà¥ªâ­®áâ¨ ¡à®­¨ ¨ ­ «¨ç¨¥ ­¥ á®¡à ­­ëå § ï¢®ª
m.o_type = 0		&& ­¥â § ï¢®ª ¢®¢á¥
SEEK m.n_list
m.tmpSumOrd = 0
SCAN REST WHILE LIST_NUM = m.n_list
  IF LIST_NUM # OT_6502.LIST_NUM .OR. ORD_NUM # OT_6502.ORD_NUM
    =SEEK(LIST_NUM+STR(ORD_NUM, 3), "OT_6502")
    m.o_type = IIF(EMPTY(OT_6502.REQ_DATE), 1, 2)	&& 1-à¥§¥à¢, 2 - ¡à®­ì
    m.tmpSumOrd = MAX(m.tmpSumOrd, VAL(ALLTRIM(OT_6502.CHECK_S)))
  ENDIF
  IF .NOT. SEEK(CODE, "TMP_BRON")
    LOOP
  ENDIF
  IF .NOT. EMPTY(OT_6502.REQ_DATE) .AND. EMPTY(OT_6502.STO_MARK)
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"¥ ¢á¥ ¯®§¨æ¨¨ á®¡à ­ë..."
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    USE IN TMP_BRON
    USE IN DET_6502
    USE IN STO_6502
    USE IN OT_6502
    USE IN OD_6502
    DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
    DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
    SELECT (m.s_sav)
    RETURN m.retRec
  ENDIF
  SELECT TMP_BRON
  REPLACE QNT_ORD WITH QNT_ORD+OD_6502.QNT
  SELECT OD_6502
ENDSCAN

*
*  à®¢¥àï¥¬ á®®â¢¥âáâ¢¨¥ ¡à®­¨ § ï¢ª ¬
SELECT TMP_BRON
LOCATE FOR QNT_REQ # QNT_ORD
IF FOUND()
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Žè¨¡ª  ¯à¨ ¡à®­¨à®¢ ­¨¨..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN TMP_BRON
  USE IN DET_6502
  USE IN STO_6502
  USE IN OT_6502
  USE IN OD_6502
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
  SELECT (m.s_sav)
  RETURN m.retRec
ENDIF

*
* ‘®§¤ ¥¬ ­®¢ë© á¯¨á®ª
SELECT DET_6502
SET ORDER TO
USE (m.base_path+"LINK_ORD") SHARED AGAIN ORDER TAG LIST_CODE ALIAS LNK_6502 IN 0
SELECT TMP_BRON
m.tmpSum = 0
SCAN
  IF EMPTY(m.new_list)	&& á¯¨á®ª ¥é¥ ­¥ á®§¤ ­
    SELECT 0
    USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS TIT_6502
    SEEK VAL(m.n_list)
    SCATTER TO tmpArr
    m.new_list = Doc_Num("LISTS", FIRM, m.arm, DATE())
    APPEND FROM ARRAY tmpArr
    m.tmpName = LEFT(LST_NAME, LEN(LST_NAME)-8)+"à"+STR(LST_NUM, 6)
    REPLACE	LST_NUM		WITH m.new_list, ;
    		CR_DAT		WITH DATE(),     ;
    		LST_NAME	WITH m.tmpName,  ;
    		WHO_CR		WITH m.user,     ;
    		WHERE_CR	WITH m.arm,      ;
    		ORD_NUM		WITH 0,          ;
    		ORD_COUNT	WITH 0,          ;
    		DATE_REQ	WITH DATE(),	 ;
    		TIME_REQ	WITH TIME()
    m.new_list = STR(m.new_list, 6)
    m.retRec = RECNO()
  ENDIF
  SELECT DET_6502
  GO TMP_BRON.LINK
  IF EMPTY(QNT_DUE)
    REPLACE LIST_NUM WITH m.new_list, MARK_TMP WITH ""
    SELECT LNK_6502
    DO WHILE SEEK(m.n_list+STR(DET_6502.CODE))
      REPLACE LIST_NUM WITH m.new_list
    ENDDO
    SELECT DET_6502
  ELSE
    SCATTER TO tmpArr
    REPLACE QNT WITH 0, QNT_REQ WITH 0, MARK_TMP WITH ""
    APPEND FROM ARRAY tmpArr
    REPLACE	LIST_NUM WITH m.new_list,	;
			MARK_TMP WITH "",			;
			QNT_DUE WITH 0
  ENDIF
  REPLACE QNT_REQ WITH QNT
  m.tmpSum = m.tmpSum+IIF(TIT_6502.IS_USD, QNT*USD_PRICE, ROUND(QNT*SAL_PRICE, 2))
  SELECT TMP_BRON
ENDSCAN
USE IN LNK_6502
SELECT TIT_6502
REPLACE R_TOTAL WITH m.tmpSum
= SEEK(VAL(m.n_list), "TIT_6502")
REPLACE R_TOTAL WITH R_TOTAL-m.tmpSum
SELECT TMP_BRON
*
*    §¡à®­¨àã¥¬ áâ àë© á¯¨á®ª
IF m.o_type = 1		&& à¥§¥à¢ - ­ ¤® á®¡«îáâ¨ §®­ë...
  INDEX ON ZONE TAG ZONE FOR QNT_REQ # 0
  m.tmpZone = CHR(0)
  SCAN
    IF m.tmpZone # ZONE		&& ®¢ ï § ï¢ª 
      m.tmpZone = ZONE
      SELECT TIT_6502
      REPLACE ORD_NUM WITH ORD_NUM+1
      SELECT OT_6502
      APPEND BLANK
      REPLACE LIST_NUM	WITH m.n_list,			;
              LIST_NAME	WITH TIT_6502.LST_NAME,	;
              ORD_NUM	WITH TIT_6502.ORD_NUM,	;
              ORD_DATE	WITH DATE(),			;
              ORD_TIME	WITH TIME(),			;
              ZONE		WITH m.tmpZone,			;
              WHO		WITH m.user,			;
              WHERE		WITH m.arm
    ENDIF
    SELECT OD_6502
    APPEND BLANK
    REPLACE	LIST_NUM	WITH OT_6502.LIST_NUM,	;
			ORD_NUM		WITH OT_6502.ORD_NUM,	;
			CODE		WITH TMP_BRON.CODE,		;
			QNT			WITH -TMP_BRON.QNT_REQ
    SELECT OT_6502
    REPLACE SIZE_NEG WITH SIZE_NEG+1
    SELECT TMP_BRON
  ENDSCAN
ENDIF
IF m.o_type = 2		&& ¡à®­ì ¢á¥ ¢ ®¤­ã § ï¢ªã ¨ á ¬¨ ãâ¢¥à¦¤ ¥¬!
  m.tmpZone = ""
  m.rc_count = 0
  SCAN FOR QNT_REQ # 0
    IF EMPTY(m.tmpZone)			&& ‘®§¤ ¥¬ § £®«®¢®ª
      m.tmpZone = "*"
      SELECT TIT_6502
      REPLACE ORD_NUM WITH ORD_NUM+1
      SELECT OT_6502
      APPEND BLANK
      REPLACE LIST_NUM	WITH m.n_list,			;
              LIST_NAME	WITH TIT_6502.LST_NAME,	;
              ORD_NUM	WITH TIT_6502.ORD_NUM,	;
              ORD_DATE	WITH DATE(),			;
              ORD_TIME	WITH TIME(),			;
              TERM_DATE	WITH DATE(),			;
              TERM_TIME	WITH TIME(),			;
              REQ_DATE	WITH DATE(),			;
              REQ_TIME	WITH TIME(),			;
              CHECK_S	WITH STR(m.tmpSumOrd+1,3),;
              STO_MARK	WITH m.user,			;
              WHO		WITH m.user,			;
              WHERE		WITH m.arm
      m.old_ord = ORD_NUM
*
*  ... ¨ âãâ ¦¥ ¡à®­¨àã¥¬ ¤«ï ­®¢®£® á¯¨áª 
      SELECT TIT_6502
      = SEEK(VAL(m.new_list), "TIT_6502")
      REPLACE ORD_NUM WITH ORD_NUM+1
      SELECT OT_6502
      APPEND BLANK
      REPLACE LIST_NUM	WITH m.new_list,		;
              LIST_NAME	WITH TIT_6502.LST_NAME,	;
              ORD_NUM	WITH TIT_6502.ORD_NUM,	;
              ORD_DATE	WITH DATE(),			;
              ORD_TIME	WITH TIME(),			;
              TERM_DATE	WITH DATE(),			;
              TERM_TIME	WITH TIME(),			;
              REQ_DATE	WITH DATE(),			;
              REQ_TIME	WITH TIME(),			;
              CHECK_S	WITH "  1",				;
              STO_MARK	WITH m.user,			;
              WHO		WITH m.user,			;
              WHERE		WITH m.arm
      m.new_ord = ORD_NUM
    ENDIF
    SELECT OD_6502
    APPEND BLANK
    REPLACE	LIST_NUM	WITH m.n_list,			;
			ORD_NUM		WITH m.old_ord,			;
			CODE		WITH TMP_BRON.CODE,		;
			QNT			WITH -TMP_BRON.QNT_REQ
    APPEND BLANK
    REPLACE	LIST_NUM	WITH m.new_list,		;
			ORD_NUM		WITH m.new_ord,			;
			CODE		WITH TMP_BRON.CODE,		;
			QNT			WITH TMP_BRON.QNT_REQ
    m.rc_count = m.rc_count+1
    SELECT TMP_BRON
  ENDSCAN
  SELECT OT_6502
  SEEK m.n_list+STR(m.old_ord, 3)
  REPLACE SIZE_NEG WITH m.rc_count
  SEEK m.new_list+STR(m.new_ord, 3)
  REPLACE SIZE_POS WITH m.rc_count
ENDIF

SELECT TIT_6502
= SEEK(VAL(m.new_list), "TIT_6502")
SELECT TMP_BRON
INDEX ON ZONE TAG ZONE1
m.tmpZone = CHR(0)
SCAN
  IF QNT = QNT_REQ .AND. m.o_type = 2	&& ¡à®­ì, § ï¢ª  ã¦¥ á®§¤ ­ 
    LOOP
  ENDIF
  IF m.tmpZone # ZONE		&& ®¢ ï § ï¢ª 
    m.tmpZone = ZONE
    SELECT TIT_6502
    REPLACE ORD_NUM WITH ORD_NUM+1, ORD_COUNT WITH ORD_COUNT+1
    SELECT OT_6502
    APPEND BLANK
    REPLACE LIST_NUM	WITH m.new_list,		;
            LIST_NAME	WITH TIT_6502.LST_NAME,	;
            ORD_NUM		WITH TIT_6502.ORD_NUM,	;
            ORD_DATE	WITH DATE(),			;
            ORD_TIME	WITH TIME(),			;
            ZONE		WITH m.tmpZone,			;
            REQ_DATE	WITH DATE(),			;
            REQ_TIME	WITH TIME(),			;
            WHO			WITH m.user,			;
            WHERE		WITH m.arm
  ENDIF
  SELECT OD_6502
  APPEND BLANK
  REPLACE	LIST_NUM	WITH OT_6502.LIST_NUM,	;
			ORD_NUM		WITH OT_6502.ORD_NUM,	;
			CODE		WITH TMP_BRON.CODE,		;
			QNT			WITH IIF(m.o_type = 2, TMP_BRON.QNT-TMP_BRON.QNT_REQ, TMP_BRON.QNT)
  SELECT OT_6502
  REPLACE SIZE_POS WITH SIZE_POS+1
  IF TMP_BRON.QNT#TMP_BRON.QNT_REQ
    SELECT STO_6502
    SEEK TMP_BRON.CODE
    REPLACE QNT			WITH QNT-TMP_BRON.QNT+TMP_BRON.QNT_REQ, ;
            QNT_SAVE	WITH QNT_SAVE+TMP_BRON.QNT-TMP_BRON.QNT_REQ
  ENDIF
  SELECT TMP_BRON
ENDSCAN
m.tmpName = DBF()
USE
DELETE FILE (m.tmpName)
DELETE FILE (LEFT(m.tmpName, LEN(m.tmpName)-3)+"cdx")
USE IN DET_6502
USE IN TIT_6502
USE IN STO_6502
USE IN OT_6502
USE IN OD_6502

SELECT (m.s_sav)

RETURN m.retRec
