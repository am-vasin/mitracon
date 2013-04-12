*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Calendar      งเ กฎโ็จช €ญคเฅฉ + €ซซ           08.04.97 20:26:38 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                     เฎฃเ ฌฌ  เจแฎข ญจ๏ โ กฅซ๏.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Tabel
PRIVATE arr_w, title, i, s_date, date_sav, j, cl_sav, win_name, ln, wd, n
PRIVATE col0, col1

SCATTER TO arr_w

*
*   ”ฎเฌจเใฅฌ ง ฃฎซฎขฎช ฎชญ !
*
DO CASE
CASE MONTH = 1
  title = "ญข เ์"
CASE MONTH = 2
  title = "”ฅขเ ซ์"
CASE MONTH = 3
  title = " เโ"
CASE MONTH = 4
  title = "€ฏเฅซ์"
CASE MONTH = 5
  title = " ฉ"
CASE MONTH = 6
  title = "๎ญ์"
CASE MONTH = 7
  title = "๎ซ์"
CASE MONTH = 8
  title = "€ขฃใแโ"
CASE MONTH = 9
  title = "‘ฅญโ๏กเ์"
CASE MONTH = 10
  title = "ชโ๏กเ์"
CASE MONTH = 11
  title = "ฎ๏กเ์"
CASE MONTH = 12
  title = "ฅช กเ์"
ENDCASE

title = title + STR(YEAR,5)+"  "+PS.FAMILY

date_sav = SET("DATE")
SET DATE GERMAN

s_date = CTOD("01."+STR(MONTH,2)+"."+STR(YEAR,4))
cl_sav = 5
FOR i = 3 TO 33
  IF arr_w(i) = "!"
    EXIT
  ENDIF
  DIMENSION cl(i-2), rw(i-2), h_sw(i-2)
  j = DOW(s_date)
  rw(i-2) = 1+IIF(j = 1, 7, j-1)
  cl(i-2) = cl_sav
  IF j = 1
    cl_sav = cl_sav+3
  ENDIF
  h_sw(i-2) = arr_w(i) # " "
  s_date = s_date+1

ENDFOR
n = ALEN(h_sw)

win_name = "TABEL"
ln = 10
wd = cl(n)+4
DO D_Win_N WITH ln, wd, title

col0   = SCHEME(13,1)
col1   = SCHEME(14,2)

@ 2, 2 SAY "ญ" COLOR (col0)
@ 3, 2 SAY "โ" COLOR (col0)
@ 4, 2 SAY "‘เ" COLOR (col0)
@ 5, 2 SAY "—โ" COLOR (col0)
@ 6, 2 SAY "โ" COLOR (col0)
@ 7, 2 SAY "‘ก" COLOR (col1)
@ 8, 2 SAY "แ" COLOR (col1)

FOR i = 1 TO n
  @ rw(i), cl(i) SAY STR(i,2) COLOR (IIF(h_sw(i), col1, col0))
ENDFOR

RETURN

