*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Gtd_Send      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                        ฅเฅค ็  ’ ข จโ ฎญ.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 08/24/2000 ออผ
PROCEDURE Gtd_Send

PRIVATE cnt
DELETE FILE (tmpo_path+"GTD_SND0.DBF")

CREATE DBF (tmpo_path+"GTD_SND0.DBF") ;
  ( CODE     N( 7),  ;
    GTD_NUM  C(26),  ;
    GTD_LINE C( 4),  ;
    COUNTRY  C(16)   )

SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE DESCENDING

SELECT 0
USE (base_path+"STOCK")

cnt = 0

DO Wt_Mess WITH "ฎคฃฎโฎขช  ค ญญ๋ๅ"
SCAN
  cnt = cnt+1
  IF cnt % 100 = 0
    @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)), WCOLS())
  ENDIF
  SELECT ACT_S
  SEEK STOCK.CODE
  SCAN REST WHILE CODE = STOCK.CODE
    IF EMPTY(GTD_NUM)
      LOOP
    ELSE
      SELECT GTD_SND0
      APPEND BLANK
      REPLACE CODE     WITH STOCK.CODE,     ;
              GTD_NUM  WITH ACT_S.GTD_NUM,  ;
              GTD_LINE WITH ACT_S.GTD_LINE, ;
              COUNTRY  WITH ACT_S.COUNTRY
      EXIT
    ENDIF
  ENDSCAN
  SELECT STOCK
ENDSCAN

DO Wt_Mess

USE
SELECT ACT_S
USE
SELECT GTD_SND0

PRIVATE tmpPth

tmpPth = Int_Dir("C:\", "ใค  ฏจ่ฅฌ ค ญญ๋ฅ?")
IF .NOT. EMPTY(tmpPth)
  DELETE FILE (tmpPth+"GTD_SEND.DBF")
  COPY TO (tmpPth+"GTD_SEND")
ENDIF

USE

DELETE FILE (tmpo_path+"GTD_SND0.DBF")

RETURN
