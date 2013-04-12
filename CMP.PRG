set exclu off
set dele on

use z:\store\base\sale alias main
sele 0
use d:\div_100\sale alias copy
delta = 0
delta1 = 0
CLEAR
SCAN
  @ 10, 10 SAY STR(recno())
  s = QNT*(PRICE_R*1000-MAIN.PRICE_R)
  delta = delta+s
  delta1 = delta1+ABS(s)
  skip in main
endscan

CLEAR
? delta
? delta1
close data