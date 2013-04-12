set dele on
set exclu off
set talk off
do n_digit
do set_lwr
use base\stock order tag name alias old
select 0
use base\stock again order tag code
select 0
use compel\subject order tag sto_code
select 0
use rigol
scan
  nw = .t.
  tmpNm = padr(name, fsize("name", "stock"))
  select stock
  if seek(tmpNm, "old")
    if code > 9000000
      go recno("old")
      nw = .f.
    else
      select old
      replace date_off with date()
      select stock
    endif
  endif
  if nw
    go bottom
    cd = code+1
    append blank
    replace code     with cd, ;
            name     with rigol.name, ;
            pro_code with 1562, ;
            producer with "RIGOL", ;
            class    with rigol.class, ;
            num_part with chrtran(rigol.name, n_digit, ""), ;
            date_on  with date()
  endif
  select subject
  if .not. seek(stock.code)
    append blank
    replace sto_code with stock.code
  endif
  replace function with rigol.descr
  select rigol
endscan

select stock
set order to tag pro_code
seek " 1562"
scan rest while pro_code = 1562
  if code > 9000000
    loop
  endif
  if alltrim(producer) == "RIGOL"
    replace date_off with date()
  endif
endscan


close data


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

**********************************************************
PROCEDURE set_lwr
PRIVATE j
ERASE lwr
PUBLIC lwr
lwr = ""
For j = 1 to 255
  lwr = lwr+CHR(j)
ENDFOR
lwr = stuff(lwr,65,26,"abcdefghijklmnopqrstuvwxyz")
lwr = stuff(lwr,128,32," ΅Ά£¤¥¦§¨©ª«¬­®―ΰαβγδεζηθικλμνξο")
RETURN
