*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Repe_Rep      งเ กฎโ็จช €ญคเฅฉ  แจญ           18.03.97 23:36:14 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ            เฎฃเ ฌฌ  ฏฎขโฎเ  ฎโ็ฅโ  ฏฅเฅค ฎแญฎขญ๋ฌ แชซ คฎฌ.            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Repe_Rep
PARAMETERS f_nam, t_nam

PRIVATE arr_l, arr_m, n, i, s, win_name, pth

win_name = "Reps"
n = ADIR(arr_l,log_path+f_nam+"?????.ARJ")

IF n = 0
  DIMENSION arr_m(3)
  arr_m(1) = ""
  arr_m(2) = CHR(0)+" ฆใเญ ซฅ ญฅโ ญจ ฎคญฎฃฎ ฎโ็ฅโ !"
  arr_m(3) = ""
  DO Out_Mess WITH 7, "arr_m"
  RETURN
ENDIF

DIMENSION arr_m(n)

FOR i = 1 TO n
  arr_m(i) = "N "+SUBSTR(arr_l(i,1), 3, 5)+" ฎโ "+TRANSFORM(arr_l(i,3),"D")
ENDFOR

ln = MIN(WROWS("")-6, n)
wd = LEN(arr_m(1))+4

DO D_Win_N WITH ln+3, wd, "‘ฏจแฎช ฎโ็ฅโฎข"

i = n

DO WHILE .T.
  i = MAX(i,1)
  i = MIN(i,n)
  @ 1,1 MENU arr_m, n, ln
  READ MENU TO i
  s = READKEY() % 256
  IF s = 12
    i = 0
    EXIT
  ENDIF
  IF s = 15
    EXIT
  ENDIF
ENDDO

RELEASE WINDOW (win_name)
IF s = 15

  pth = "\"
  pth = Int_Dir(pth, "๋กจเ ฅฌ คจเฅชโฎเจ๎", 0)
  IF .NOT. EMPTY(pth)
    DO Copy_A WITH log_path+arr_l(i,1), pth+t_nam
  ENDIF

ENDIF

RETURN
