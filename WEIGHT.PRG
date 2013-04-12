*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Weight        งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                  แ็ฅโ ขฅแ  คซ๏ แฏจแช  จซจ ญ ชค คญฎฉ.                  บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 14.08.2002 ออผ
PROCEDURE Weight
PARAMETERS d_num, d_date, d_firm
PRIVATE s_sav, is_inv, tmpKey, s000

s_sav = SELECT()

SELECT 0
USE (path_comm+"CORPUS") AGAIN ALIAS COR_2814 ORDER TAG CODE
SELECT 0
USE (path_comm+"SUBJECT") AGAIN ALIAS SUB_2814 ORDER TAG STO_CODE
SET RELATION TO COR_CODE INTO COR_2814
SELECT 0
USE (base_path+"SALE_TIT") AGAIN ALIAS TIT_2814

is_inv = .T.
IF TYPE("d_num") = "N"
  SET ORDER TO TAG LIST_NUM
  tmpKey = STR(d_num, 6)
  SEEK tmpKey
ELSE
  SET ORDER TO TAG DOC_NUM
  SEEK d_firm+LEFT(DTOS(d_date), 4)+d_num
ENDIF

IF .NOT. FOUND()
  IF TYPE("d_num") # "N"
    USE
    SELECT COR_2814
    USE
    SELECT SUB_2814
    USE
    SELECT (s_sav)
    RETURN 0
  ENDIF
  USE (base_path+"LIST_DET") AGAIN ALIAS DET_2814 ORDER TAG LIST
  SEEK tmpKey
  is_inv = .F.
ELSE
  tmpKey = TIT_2814.FIRM+LEFT(DTOS(TIT_2814.DOC_DATE), 4)+TIT_2814.DOC_NUM
  USE (base_path+"SALE") AGAIN ALIAS DET_2814 ORDER TAG DOC_NUM
  SEEK tmpKey
ENDIF

SET RELATION TO CODE INTO SUB_2814
s000 = 0
SCAN REST
  s000 = s000+QNT*COR_2814.WEIGHT
  IF EOF()
    EXIT
  ENDIF
  IF is_inv
    IF FIRM+LEFT(DTOS(DOC_DATE), 4)+DOC_NUM # tmpKey
      EXIT
    ENDIF
  ELSE
    IF LIST_NUM # tmpKey
      EXIT
    ENDIF
  ENDIF
ENDSCAN

USE
SELECT COR_2814
USE
SELECT SUB_2814
USE

SELECT (s_sav)

RETURN s000
