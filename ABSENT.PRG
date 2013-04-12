*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Absent        งเ กฎโ็จช €ญคเฅฉ  แจญ           02.02.98 09:34:56 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ               ฎก ขซฅญจฅ ฏฎงจๆจจ ข แฏจแฎช ฎโแใโแโขใ๎้จๅ.               บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Absent
PARAMETER sto_c
PRIVATE s_sav

s_sav = SELECT()

SELECT 0
USE (base_path+"ABSENT") ORDER TAG CODE
IF .NOT. SEEK(sto_c)
  GO TOP
  IF CODE # 0
    APPEND BLANK
  ENDIF
ENDIF
REPLACE CODE WITH sto_c, D_DATE WITH DATE()

USE

SELECT (s_sav)

RETURN
