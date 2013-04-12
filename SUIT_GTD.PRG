***********************************************************
*                                                         *
*          Подбор ГТД из существующих партий              *
*                                                         *
***********************************************************
PROCEDURE Suit_GTD
PARAMETERS prmCode, prmNum, prmLine, prmCntry, prmCC

PRIVATE s_sav

STORE "" TO m.prmNum, m.prmLine, m.prmCntry

m.s_sav = SELECT()
SELECT 0
USE m.base_path+"ACT_S" SHARED AGAIN ORDER TAG CODE DESCENDING ALIAS SUIT_GTD
IF SEEK(m.prmCode)
  LOCATE REST WHILE CODE = m.prmCode FOR .NOT. EMPTY(GTD_NUM)
  IF FOUND()
    m.prmNum = GTD_NUM
    m.prmLine = GTD_LINE
    m.prmCntry = COUNTRY
    m.prmCC    = COUNTRYC
  ENDIF
ENDIF
USE
SELECT (m.s_sav)
RETURN
