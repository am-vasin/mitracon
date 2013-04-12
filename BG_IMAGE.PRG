PROCEDURE Bg_Image
PRIVATE bg_arr, cl, n, i, ln, wd, l, c, a_auto

DIMENSION bg_arr(1)
DO Prep_Bg
cl = SCHEME(13,8)
cl = LEFT(cl,AT("/",cl))
n  = SCHEME(13,1)
n  = SUBSTR(n,AT("/",n)+1)
IF RIGHT(cl,1) = "+"
  n = LEFT(cl,LEN(cl)-1)+"*"
ENDIF
cl = cl+n
n = ALEN(bg_arr)
FOR i = 1 TO n
  @ i-1, 0 SAY bg_arr(i) COLOR (cl)
ENDFOR
DO Prep_Tt

DIMENSION a_auto(6)

a_auto(1) = "                                          "
a_auto(2) = "                                          "
a_auto(3) = "                                          "
a_auto(4) = PADC("Версия "+vrs_p,42)
a_auto(5) = "                                          "
a_auto(6) = "                                          "

ln = ALEN(a_auto)
wd = LEN(a_auto(1))

c = CEILING((WCOLS()-wd)*0.8)
l = WROWS()-ln-3

IF .F.    &&&&&&&&&&&&&&&&&&&&&
cl = SCHEME(13,8)
@ l+1, c+2 FILL TO l+ln, c+wd+1 COLOR (cl)
FOR i = 1 TO ln
  @ l+i-1, c SAY a_auto(i) COLOR SCHEME 13
ENDFOR
ENDIF     &&&&&&&&&&&&&&&&&&&&&&&

RETURN
