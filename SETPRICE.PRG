PROCEDURE SetPrice
PARAMETERS s1, s2

PRIVATE i, sw, n, cp
n = LEN(s2)
sw = ""
DO WHILE LEN(sw) < n
  sw = sw+s1
ENDDO
cp = 0
FOR i = 1 TO n
  cp = MOD(ASC(SUBSTR(sw,i,1))+ASC(SUBSTR(s2,i,1))+cp,256)
  s2 = STUFF(s2,i,1,CHR(cp))
ENDFOR
s2 = STUFF(s2,1,1,CHR(MOD(ASC(SUBSTR(s2,1,1))+cp,256)))

RETURN
