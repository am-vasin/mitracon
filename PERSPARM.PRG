*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  PersParm      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                 ญคจขจคใ ซ์ญ๋ฅ ญ แโเฎฉช  ฏฎซ์งฎข โฅซ๏                  บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 04.02.2006 ออผ
PROCEDURE PersParm
PARAMETERS prmID, prmValue
*
* prmID		- ชฎค ฏ เ ฌฅโเ ;
* prmValue	- งญ ็ฅญจฅ ฏ เ ฌฅโเ  (C) - ฅแซจ ใช ง ญ - ง ฏจแ์, จญ ็ฅ ็โฅญจฅ
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

IF m.getMode	&& ฏฎซใ็จโ์
  m.tmpLine = ""
  SEEK m.tmpKey
  SCAN REST WHILE PARM_CODE == m.tmpCode .AND. m.user = WHO
    m.tmpLine = m.tmpLine+PARM_VAL
  ENDSCAN
  USE
  SELECT (m.s_sav)
  RETURN ALLTRIM(m.tmpLine)
ELSE			&& ง ฏจแ โ์
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
