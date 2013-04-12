*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ           เฎฃเ ฌฌ  ฎฏเฅคฅซฅญจ๏ ฏฅเฅ็ญ๏ คฎแโใฏญ๋ๅ คจแชฎข.             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Disk_Lst

PRIVATE res, shab, i, n, err_sav, def_sav, err_sw, w

def_sav = SET("DEFAULT")+CURDIR()     && โฅชใ้ ๏ คจเฅชโฎเจ๏
err_sav = ON("ERROR")                 && โฅชใ้จฉ ฎกเ กฎโ็จช ฎ่จกฎช
shab    = "cdefghijklmnopqrstuvwxyz"  && ่ กซฎญ - แฏจแฎช ขฎงฌ. คเ ฉขฎข
res     = ""                          && เฅงใซ์โ โ - แฏจแฎช คฎแโ. คเ ฉขฎข
n       = LEN(shab)

err_sw   = .T.
ON ERROR err_sw = .F.

FOR i = 1 TO n
  w = SUBSTR(shab,i,1)
  err_sw = .T.
  SET DEFAULT TO (w+":\")
  IF err_sw
    res = res+w
  ENDIF
ENDFOR

ON ERROR &err_sav
SET DEFAULT TO (def_sav)
  
RETURN res
