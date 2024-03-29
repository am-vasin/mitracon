*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Arm_P        ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                        �����モ琺 ��｀腑�� �メ��                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 06.11.2005 様�
PROCEDURE Arm_P
PARAMETERS parmID, parmValue

*
*  parmID		- ┐キ皋筥��皰� �����モ��,
*  parmValue	- Л�腑�┘ �����モ�� - メ�� �矚竄痰≪モ 皰 艪キ┘, ┃�腑 - ����瘡
*

PRIVATE s_sav, getMode, tmpRet, tmpValue, lenID, lenVal, tmpID, tmpCount, tmpBuf

m.getMode = TYPE("m.parmValue") # "C"
IF .NOT. m.getMode
  m.tmpValue = ALLTRIM(m.parmValue)
ENDIF
m.tmpRet = ""
m.s_sav = SELECT()
SELECT 0
IF .NOT. File_O(prmo_path+"ARM_P.DBF")
  CREATE DBF (prmo_path+"ARM_P.DBF") ;
	(	PARM_ID		C(20), ;
		PARM_LINE	N( 2), ;
		PARM_VALUE	C(80)  )
  INDEX ON PARM_ID+STR(PARM_LINE, 2) TAG PARM_ID
ELSE
  USE (prmo_path+"ARM_P.DBF") SHARED AGAIN ORDER TAG PARM_ID
ENDIF
m.lenID		= FSIZE("PARM_ID")
m.lenVal	= FSIZE("PARM_VALUE")
m.tmpID		= PADR(UPPER(m.parmID), m.lenID)

IF m.getMode
  SEEK m.tmpID
  SCAN REST WHILE m.tmpID = PARM_ID
    m.tmpRet = m.tmpRet+PARM_VALUE
  ENDSCAN
  USE
  SELECT (m.s_sav)
  RETURN ALLTRIM(m.tmpRet)
ELSE
  DO WHILE SEEK(m.tmpID)
    BLANK
  ENDDO
  m.tmpCount = 0
  DO WHILE .NOT. EMPTY(m.tmpValue)
    m.tmpCount = m.tmpCount+1
    IF LEN(m.tmpValue) <= m.lenVal
      m.tmpBuf = m.tmpValue
      m.tmpValue = ""
    ELSE
      m.tmpBuf = LEFT(m.tmpValue, m.lenVal)
      m.tmpValue = SUBSTR(m.tmpValue, m.lenVal+1)
    ENDIF
    GO TOP
    IF .NOT. EMPTY(PARM_ID) .OR. BOF()
      APPEND BLANK
    ENDIF
    REPLACE PARM_ID    WITH m.tmpID,    ;
            PARM_LINE  WITH m.tmpCount, ;
            PARM_VALUE WITH m.tmpBuf
  ENDDO
ENDIF

USE
SELECT (m.s_sav)
RETURN
