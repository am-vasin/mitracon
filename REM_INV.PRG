*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Rem_Inv       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                      ฎเเฅชๆจ๏ แใฌฌ ฏฎ ญ ชซ คญ๋ฌ.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 21.04.1999 ออผ
PROCEDURE Rem_Inv
PRIVATE s0, s0d, c0, p0, q0

CREATE DBF (log_path+"REM_INV")  ;
  (  FIRM     C( 4),   ;
     DOC_NUM  C(10),   ;
     DOC_DATE D,       ;
     CUS_CODE N( 8),   ;
     START_S  N(15,2), ;
     RES_S    N(15,2) )
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM
SELECT 0
USE (base_path+"SALE_TIT")
SCAN
  @ 10, 10 SAY FIRM+" "+DTOC(DOC_DATE)+" "+DOC_NUM
  =SEEK(FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM, "SALE")
  SELECT SALE
  s0d= 0
  s0 = 0
  c0 = 0
  p0 = 0
  q0 = 0
  SCAN REST WHILE FIRM = SALE_TIT.FIRM .AND. DOC_DATE = SALE_TIT.DOC_DATE .AND. DOC_NUM = SALE_TIT.DOC_NUM
    IF c0 = CODE .AND. p0 = PRICE_R
      q0 = q0+QNT
    ELSE
      s0 = s0+ROUND(p0*q0,2)
      q0 = QNT
      c0 = CODE
      p0 = PRICE_R
    ENDIF
    s0d = s0d+PRICE_D*QNT
  ENDSCAN
  s0 = s0+ROUND(p0*q0,2)
  SELECT SALE_TIT
  IF s0 # TOTAL_R
    SELECT REM_INV
    APPEND BLANK
    REPLACE FIRM     WITH SALE_TIT.FIRM,     ;
            DOC_NUM  WITH SALE_TIT.DOC_NUM,  ;
            DOC_DATE WITH SALE_TIT.DOC_DATE, ;
            CUS_CODE WITH SALE_TIT.CUS_CODE, ;
            START_S  WITH SALE_TIT.TOTAL_R,  ;
            RES_S    WITH s0
    SELECT SALE_TIT
******************    REPLACE TOTAL_R WITH s0
  ENDIF
  IF s0d # TOTAL_D
    REPLACE TOTAL_D WITH s0d
  ENDIF
ENDSCAN

CLOSE DATA
RETURN
