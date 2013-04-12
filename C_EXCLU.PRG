*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      เฎฃเ ฌฌ  แขฎกฎฆคฅญจ๏ ก ง๋.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE C_Exclu
PRIVATE mss

USE PARMS

IF EXCLU # -1
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"เจ ฏฎฏ๋โชฅ ฎแขฎกฎคจโ์ ก งใ ค ญญ๋ๅ ขคเใฃ ข๋๏แญจซฎแ์:"
  mss(3) = CHR(0)+"ฎญ  ญฅ ก๋ซ  ง กซฎชจเฎข ญ !"
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

REPLACE EXCLU WITH 1

CLOSE DATABASES
RETURN
