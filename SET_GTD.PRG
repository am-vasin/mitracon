*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º   ˆ¬ï ä ©«  Set_GTD       §à ¡®âç¨ª €­¤à¥© ‚ á¨­                      º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                       “áâ ­®¢ª  ƒ’„ ¤«ï ¯®§¨æ¨¨                        º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 10.04.2002 ÍÍ¼
PROCEDURE Set_GTD
PARAMETERS pCode, pPref, pName, pProd, pGTD_num, pCountry, pCntC, pGTD_line, badG, pGTDName

PRIVATE s_sav, tmpName, p, i, nameWd, g_name, tmpDate, tmpQnt
PRIVATE nFound, GTDFound, lastDate, d1, tmpPoint
PRIVATE period
period = 120       &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
g_name = ""
tmpDate= DATE()
tmpQnt = 0
s_sav = SELECT()

IF USED("SET_GTD")
  SELECT SET_GTD
ELSE
  SELECT 0
  USE (path_comm+"GTD") ORDER TAG NAME DESCENDING ALIAS SET_GTD AGAIN
ENDIF

cnt_cur = 0
nameWd = FSIZE("NAME", "SET_GTD")

tmpName = ALLTRIM(pPref)+ALLTRIM(pName)
p = LEN(tmpName)
tmpName = PADR(tmpName, nameWd)
nFound   = 0
lastDate = {}
DO WHILE p > 2
  IF SEEK(PADR(tmpName, nameWd), "SET_GTD")
    nFound = nFound+1
    DIMENSION GTDFound(nFound,6)
    GTDFound(nFound,1) = SET_GTD.GTD_NUM
    GTDFound(nFound,2) = SET_GTD.GTD_DATE
    GTDFound(nFound,3) = SET_GTD.COUNTRY
    GTDFound(nFound,4) = SET_GTD.GTD_LINE
    GTDFound(nFound,5) = SET_GTD.NAME
    GTDFound(nFound,6) = SET_GTD.COUNTRYC
    IF EMPTY(lastDate) .OR. lastDate < SET_GTD.GTD_DATE
      lastDate = SET_GTD.GTD_DATE
    ENDIF
  ENDIF
  p = p-1
  tmpName = LEFT(tmpName,p)
ENDDO

IF nFound = 0
  tmpName = ALLTRIM(pName)
  p = LEN(tmpName)
  tmpName = PADR(tmpName, nameWd)
  DO WHILE p > 2
    IF SEEK(PADR(tmpName, nameWd), "SET_GTD")
      nFound = nFound+1
      DIMENSION GTDFound(nFound,6)
      GTDFound(nFound,1) = SET_GTD.GTD_NUM
      GTDFound(nFound,2) = SET_GTD.GTD_DATE
      GTDFound(nFound,3) = SET_GTD.COUNTRY
      GTDFound(nFound,4) = SET_GTD.GTD_LINE
      GTDFound(nFound,5) = SET_GTD.NAME
      GTDFound(nFound,6) = SET_GTD.COUNTRYC
      IF EMPTY(lastDate) .OR. lastDate < SET_GTD.GTD_DATE
        lastDate = SET_GTD.GTD_DATE
      ENDIF
    ENDIF
    p = p-1
    tmpName = LEFT(tmpName,p)
  ENDDO
ENDIF

SELECT (s_sav)

IF nFound # 0
  PRIVATE tmpLen, tmpPoint
  p = FLOOR((DATE()-lastDate)/period)
  d1 = DATE()-period*(p+1)
  tmpLen = 0
  tmpPoint = 0
  FOR i = 1 TO nFound
    IF GTDFound(i,2) < d1
      LOOP
    ENDIF
    IF tmpLen < LEN(ALLTRIM(GTDFound(i,5)))
      tmpLen = LEN(ALLTRIM(GTDFound(i,5)))
      tmpPoint = i
    ENDIF
  ENDFOR
  IF tmpPoint # 0
    g_name    = GTDFound(tmpPoint,5)
    pGTD_num  = GTDFound(tmpPoint,1)
    pCountry  = GTDFound(tmpPoint,3)
    pCntC     = GTDFound(tmpPoint,6)
    pGTD_line = GTDFound(tmpPoint,4)
    badG = .F.
    IF TYPE("pGTDName") = "C"
      pGTDName = g_name
    ENDIF
  ENDIF
ELSE
  DO GTDClass WITH pCode, pGTD_num, tmpDate, pCountry, pCntC, pGTD_line, tmpQnt
  badG = .T.
ENDIF

RETURN
