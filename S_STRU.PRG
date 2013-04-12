*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  S_Stru        งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ             เฎฃเ ฌฌ  ไฎเฌจเฎข ญจ๏ แโเใชโใเ๋ ก ง๋ ค ญญ๋ๅ.              บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 12/10/1998 ออผ
PROCEDURE S_Stru

PRIVATE pth1, pth2, arr, err

STORE SPACE(50) TO pth1, pth2
arr = 1
CLEAR
@ 10, 2 SAY "       ใโ์ ช ก งฅ ค ญญ๋ๅ" GET pth1
@ 12, 2 SAY "ใค  ฏเฎฌฅแโจโ์ เฅงใซ์โ โ" GET pth2
@ 14, 23 GET arr PICTURE "@*HT \! OK (Ctrl-W) ;\? โช ง โ์แ๏ (Esc) "
READ CYCLE
IF arr = 2
  RETURN
ENDIF
RELEASE arr
pth1 = ALLTRIM(pth1)
pth2 = ALLTRIM(pth2)

SET EXCLU OFF
CLEAR
DO Set_Lwr
n = ADIR(arr, pth1+"*.DBF")
err = 0
ON ERROR err = ERROR()
FOR i = 1 TO n
  WAIT WIND arr(i,1) NOWAIT
  err = 0
  USE (pth1+arr(i,1))
  IF err # 0
    WAIT WIND arr(i,1)+" ่จกช  ü "+ALLTIM(STR(err))
  ELSE
    COPY STRUCTURE TO (pth2+arr(i,1)) WITH CDX
  ENDIF
ENDFOR

ON ERROR
USE

RETURN

PROCEDURE set_lwr
PRIVATE j
ERASE lwr
PUBLIC lwr
lwr = ""
For j = 1 to 255
  lwr = lwr+CHR(j)
ENDFOR
lwr = stuff(lwr,65,26,"abcdefghijklmnopqrstuvwxyz")
lwr = stuff(lwr,128,32," กขฃคฅฆงจฉชซฌญฎฏเแโใไๅๆ็่้๊๋์ํ๎๏")
RETURN
