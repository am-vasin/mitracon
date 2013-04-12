*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ s_analog      งเ กฎโ็จช €ญคเฅฉ  แจญ           05.05.97 13:10:48 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                 เฎฃเ ฌฌ  ฎกญฎขซฅญจ๏ แฏจแช   ญ ซฎฃฎข.                  บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE S_Analog
PRIVATE f_s, mss, pth

pth = ""
DO Get_Path WITH pth, "S_ANALOG"
f_s = Int_File(pth, "DBF", "“ช ฆจโฅ ไ ฉซ  ญ ซฎฃฎข")
IF EMPTY(f_s)
  RETURN
ENDIF
USE (f_s) ALIAS SOURCE
IF FSIZE("STO_CODE") = 0 .OR. ;
  FSIZE("NAME")     = 0 .OR. ;
  FSIZE("ANA_CODE") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"โฎ ญฅ ไ ฉซ  ญ ซฎฃฎข!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"ANALOG")

DO Wt_Mess WITH " แแโ ญฎขช   ญ ซฎฃฎข..."
SELECT SOURCE
SCAN FOR ANA_CODE # 0 .AND. SEEK(ANA_CODE, "STOCK")
  SELECT ANALOG
  IF EOF() .OR. BOF()
    APPEND BLANK
  ENDIF
  REPLACE CODE_1 WITH SOURCE.STO_CODE, ;
          NAME_1 WITH SOURCE.NAME,     ;
          CODE_2 WITH SOURCE.ANA_CODE
  SKIP
  SELECT SOURCE
ENDSCAN

SELECT ANALOG
DELETE REST
SET ORDER TO TAG CODE

SELECT STOCK
REPLACE ALL IS_CHANGE WITH IIF(SEEK(CODE,"ANALOG"), "๛", " ")

DO Wt_Mess

CLOSE DATABASES

RETURN
