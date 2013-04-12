*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Cupupy        งเ กฎโ็จช €ญคเฅฉ  แจญ           15.04.97 11:20:13 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                       เฎฃเ ฌฌ  ฏฎคแ็ฅโ  ชใฏ๎เ.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cupury
PARAMETERS c_arr, q_arr, mov_l, mov_c
PRIVATE ln_arr, s, ln, wd, i, w_sav, q_w, mov1, mov2, stat_type

stat_type = 1
mov1 = 0
mov2 = 0
IF TYPE("mov_l") = "N"
  mov1 = mov_l
ENDIF
IF TYPE("mov_c") = "N"
  mov2 = mov_c
ENDIF

PUSH KEY CLEAR
ON KEY LABEL Esc KEYBOARD CHR(23)
ON KEY LABEL LeftMouse DO Mouse
IF TYPE("q_arr") # "C"
  q_arr = "q_w"
ENDIF
s = 0
ln_arr = ALEN(&c_arr)
ln     = ln_arr+6
wd     = 23
w_sav  = WOUTPUT()
DO D_W WITH "Cupury", ln, wd, " ซ์ชใซ๏ๆจ๏", 0
MOVE WINDOW Cupury BY mov1, mov2
SHOW WINDOW Cupury TOP

@ 1, 1 TO WROWS()-3, WCOLS()-2
@ WROWS()-2, 0 SAY PADC("เฎกฅซ - ฎ็จแโจโ์", WCOLS())
@ WROWS()-1, 0 SAY PADC("Esc - ข๋ๅฎค", WCOLS())
FOR i = 1 TO ln_arr
  @ i+1, 2 SAY STR(&c_arr(i),8)
  IF TYPE(q_arr+"(i)") = "U"
    DIMENSION &q_arr(i)
  ENDIF
  IF TYPE(q_arr+"(i)") # "N"
    &q_arr(i) = 0
  ENDIF
  s = s+&q_arr(i)*&c_arr(i)
  @ i+1, 13 GET &q_arr(i) PICTURE "@Z 999999" VALID V_Cup() WHEN W_Cup()
ENDFOR
ON KEY LABEL SpaceBar DO Clr_V
@ 1, 1 FILL TO WROWS()-1, WCOLS()-1 COLOR SCHEME 13
@ WROWS()-4, 4 SAY "‘ใฌฌ  "+STR(s,10)
READ CYCLE
POP KEY
RELEASE WINDOW Cupury
IF EMPTY(w_sav)
  ACTIVATE SCREEN
ELSE
  ACTIVATE WINDOW (w_sav)
ENDIF
RETURN s

*************************************************************
PROCEDURE V_Cup
s = 0
FOR i = 1 TO ln_arr
  s = s+&q_arr(i)*&c_arr(i)
ENDFOR
@ WROWS()-4, 4 SAY "‘ใฌฌ  "+STR(s,10)

RETURN .T.

*************************************************************
PROCEDURE W_Cup
@ 1, 1 FILL TO WROWS()-1, WCOLS()-1 COLOR SCHEME 13
RETURN .T.

*************************************************************
PROCEDURE Clr_V
&q_arr = 0
s = 0
SHOW GETS
@ 1, 1 FILL TO WROWS()-1, WCOLS()-1 COLOR SCHEME 13
@ WROWS()-4, 4 SAY "‘ใฌฌ  "+STR(s,10)
RETURN
