set dele on
set exclu off
set talk off
do n_digit
use base\stock
nn = str(reccount())
n = 1
scan
  n = n+1
  if n%1000 = 0
    wait window str(n)+nn nowait
  endif
  replace num_part with chrtran(name, n_digit, "")
endscan
use
quit


************************************************************************
PROCEDURE N_Digit

PUBLIC n_digit
PRIVATE i, c

m.n_digit = ""
FOR m.i = 1 TO 256
  m.c = CHR(m.i-1)
  IF m.c $ "0123456789"
    LOOP
  ENDIF
  m.n_digit = m.n_digit+m.c
ENDFOR

