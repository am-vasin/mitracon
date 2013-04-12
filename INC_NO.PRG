*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ   ฅเฅชฎคจเฎขช  ญฎฌฅเ  ขๅฎค๏้ฅฉ ญ ชซ คญฎฉ ข ญฎฌฅเ ชฎเฎกชจ จ ฎกเ โญฎ.  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Inc_No
PARAMETERS nmb
PRIVATE shab, res, w

shab = " ABCDEFGHIJKLMNOPQRSTUVWXYZ"

IF TYPE("nmb") = "N"
  w = FLOOR(nmb/100)
  w = w%728
  res = SUBSTR(shab, FLOOR(w/27)+1, 1)+SUBSTR(shab, w%27+1, 1)
  w = nmb%100
  res = res+RIGHT(STR(100+w,3),2)
  RETURN res
ENDIF

PRIVATE c1, c2
w = RIGHT(ALLTRIM(UPPER(nmb)),4)
res = VAL(ALLTRIM(RIGHT(w,2)))
c1 = LEN(w)
IF c1 < 3
  RETURN res
ENDIF
w   = PADL(UPPER(w), 4)
c1  = LEFT(w,1)
c2  = SUBSTR(w,2,1)
c1  = MAX(AT(c1, shab)-1,0)
c2  = MAX(AT(c2, shab)-1,0)
res = (c1*27+c2)*100+res

RETURN res
