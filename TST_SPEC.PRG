USE PARMS
dt = DAT_START

USE ACT_S_T ORDER TAG DOC_NUM
SELECT 0

USE ACT_S ORDER TAG CODE
SELECT 0

USE SALE_TIT ORDER TAG DOC_NUM
SELECT 0

USE SALE ORDER TAG CODE
SELECT 0

USE SPEC_SUM ORDER TAG POS_CUS
SELECT 0

USE STOCK ORDER TAG CODE
l = 1
CLEAR
SCAN
  @ l, 0 SAY NAME
  q = QNT_S_SPEC
  q_ = 0
  SELECT SALE
  SEEK STOCK.CODE
  SCAN REST WHILE CODE = STOCK.CODE FOR DOC_DATE >= dt
    IF .NOT. SEEK(LEFT(DTOS(DOC_DATE),4)+DOC_NUM, "SALE_TIT")
      LOOP
    ENDIF
    IF SALE_TIT.CUS_CODE >= 0
      LOOP
    ENDIF
    q = q+QNT
  ENDSCAN

  SELECT ACT_S
  SEEK STOCK.CODE
  SCAN REST WHILE CODE = STOCK.CODE FOR DOC_DATE >= dt
    IF .NOT. SEEK(LEFT(DTOS(DOC_DATE),4)+DOC_NUM, "ACT_S_T")
      LOOP
    ENDIF
    IF ACT_S_T.SUP_CODE >= 0
      LOOP
    ENDIF
    q = q-QNT_INC
  ENDSCAN

  SELECT SPEC_SUM
  SEEK STR(STOCK.CODE,7)
  SCAN REST WHILE CODE = STOCK.CODE
    q_ = q_+QNT
  ENDSCAN
  SELECT STOCK
  @ l, COL()+1 SAY IIF(q = QNT_SPEC, " OK", " ! ")+ ;
                   IIF(q_ = QNT_SPEC, " OK", " ! ")
  IF q = QNT_SPEC .AND. q_ = QNT_SPEC
    @ l, COL()+1 SAY " OK "
  ELSE
    @ l, COL()+1 SAY STR(STOCK.QNT_S_SPEC)+STR(STOCK.QNT_SPEC)+STR(q)+STR(q_)
    l = MOD(l+1,24)
  ENDIF

ENDSCAN

CLOSE DATA
