*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� PersParm     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 ��え※ゃ��讚襯 ��痰牀��� ���讌���皀��                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 04.02.2006 様�
PROCEDURE PersParm
PARAMETERS prmID, prmValue
*
* prmID		- ��� �����モ��;
* prmValue	- Л�腑�┘ �����モ�� (C) - メ�� 礫���� - ����瘡, ┃�腑 艪キ┘
*

PRIVATE getMode, s_sav, tmpCode, tmpLine, tmpLen, tmpKey, tmpCount

m.getMode = TYPE("m.prmValue") # "C"
s_sav = SELECT()
SELECT 0
IF .NOT. File_O(m.send_log+"PERSPARM.DBF")
  CREATE DBF (m.send_log+"PERSPARM") ;
	(	WHO			N( 5),	;
		PARM_CODE	C(10),	;
		LINE_NUM	N( 2),	;
		PARM_VAL	C(32)	)
  INDEX ON PARM_CODE+STR(WHO, 5)+STR(LINE_NUM, 2) TAG PARM_CODE
ENDIF

USE (m.send_log+"PERSPARM") SHARED AGAIN ALIAS MODIPARM ORDER TAG PARM_CODE
m.tmpCode = PADR(UPPER(m.prmID), FSIZE("PARM_CODE"))
m.tmpLen = FSIZE("PARM_VAL")
m.tmpKey = m.tmpCode+STR(m.user, FSIZE("WHO"))

IF m.getMode	&& ���竍�碎
  m.tmpLine = ""
  SEEK m.tmpKey
  SCAN REST WHILE PARM_CODE == m.tmpCode .AND. m.user = WHO
    m.tmpLine = m.tmpLine+PARM_VAL
  ENDSCAN
  USE
  SELECT (m.s_sav)
  RETURN ALLTRIM(m.tmpLine)
ELSE			&& ������碎
  DO WHILE SEEK(m.tmpKey)
    REPLACE	WHO			WITH 0,		;
			PARM_CODE	WITH "",	;
			LINE_NUM	WITH 0
  ENDDO
  m.tmpLen  = FSIZE("PARM_VAL")
  m.tmpLine = ALLTRIM(m.prmValue)
  m.tmpCount = 0
  DO WHILE .NOT. EMPTY(m.tmpLine)
    m.tmpCount = m.tmpCount+1
    GO TOP
    IF .NOT. EMPTY("PARM_CODE") .OR. BOF()
      APPEND BLANK
    ENDIF
    REPLACE	WHO			WITH m.user,		;
			PARM_CODE	WITH m.tmpCode,		;
			LINE_NUM	WITH m.tmpCount,	;
			PARM_VAL	WITH LEFT(m.tmpLine, m.tmpLen)
    m.tmpLine = SUBSTR(m.tmpLine, m.tmpLen+1)
  ENDDO
ENDIF

USE
SELECT (m.s_sav)
RETURN
