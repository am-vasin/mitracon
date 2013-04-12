*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  Gtd_Recv      งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                         เจฅฌ ’ ข จโ ฎญฅ.                         บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 08/24/2000 ออผ
PROCEDURE Gtd_Recv

PRIVATE tmpPth
PRIVATE cnt

tmpPth = Int_Dir("C:\", "คฅ ญ ๅฎค๏โแ๏ ค ญญ๋ฅ?")
IF EMPTY(tmpPth)
  RETURN
ENDIF

IF .NOT. File_O(tmpPth+"GTD_SEND.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"’ ฌ ญฅโ ค ญญ๋ๅ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

USE (base_path+"ACT_S") ORDER TAG CODE
SELECT 0
USE (tmpPth+"GTD_SEND.DBF")

cnt = 0

DO Wt_Mess WITH " แแโ ญฎขช  ค ญญ๋ๅ"
SCAN
  cnt = cnt+1
  IF cnt % 100 = 0
    @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)), WCOLS())
  ENDIF
  SELECT ACT_S
  SEEK GTD_SEND.CODE
  SCAN REST WHILE CODE = GTD_SEND.CODE
    IF EMPTY(GTD_NUM)
      REPLACE GTD_NUM  WITH GTD_SEND.GTD_NUM, ;
              COUNTRY  WITH GTD_SEND.COUNTRY, ;
              GTD_LINE WITH GTD_SEND.GTD_LINE
    ENDIF
  ENDSCAN
  SELECT GTD_SEND
ENDSCAN

DO Wt_Mess

SELECT ACT_S
USE

RETURN
