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

