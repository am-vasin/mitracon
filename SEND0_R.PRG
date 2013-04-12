*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ           เฎฃเ ฌฌ  ฏฎขโฎเญฎฉ ฏฅเฅค ็จ ฏฎแซฅคญฅฃฎ ฎโ็ฅโ .             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Send0_R

PRIVATE pthw, n, mss, wk_arr

pthw = tmpo_path+"transmit\"

n = ADIR(arr_w,pthw+"F_REPORT.A*")
IF n = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"จช ชฎฃฎ ฏฎแซฅคญฅฃฎ ฎโ็ฅโ  ญฅ แใ้ฅแโขใฅโ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

FOR i = 1 TO n
  DO To_Disk WITH pthw+arr_w(i,1)
ENDFOR

RETURN
