PROCEDURE S_Saver
PARAMETERS s_mess, period

PRIVATE s_len, sled, delta_x, delta_y, x, y, w_sav, c_sav, i, j, k

s_len = 10

DIMENSION sled(s_len, 3), pic(s_len, 10)


x = FLOOR(RAND()*WCOLS()-0.01)
x = MIN(x, WCOLS()-LEN(s_mess))
y = FLOOR(RAND()*WROWS()-0.01)

sled(1, 1) = s_mess
sled(1, 2) = x
sled(1, 3) = y

FOR i = 2 TO s_len
  FOR k = 1 TO 10
    pic(i, k) = ""
    FOR j = 1 TO LEN(s_mess)
      c_sav = 2*(s_len+1-i)/(s_len-1)*RAND()
      DO CASE
      CASE c_sav < 0.2
        pic(i, k) = pic(i, k)+" "
      CASE c_sav < 0.4
        pic(i, k) = pic(i, k)+"°"
      CASE c_sav < 0.6
        pic(i, k) = pic(i, k)+"±"
      CASE c_sav < 0.8
        pic(i, k) = pic(i, k)+"²"
      OTHERWISE
        pic(i, k) = pic(i, k)+"Û"
      ENDCASE
    ENDFOR
  ENDFOR
  sled(i, 2) = x
  sled(i, 3) = y
ENDFOR

delta_x = FLOOR(RAND()*3.99)
delta_y = FLOOR(RAND()*3.99)

DO CASE
CASE delta_x = 0
  delta_x = -1
CASE delta_x = 1
  delta_x = -0.5
CASE delta_x = 2
  delta_x = 0.5
CASE delta_x = 3
  delta_x = 1
ENDCASE

DO CASE
CASE delta_y = 0
  delta_y = -1
CASE delta_y = 1
  delta_y = -0.5
CASE delta_y = 2
  delta_y = 0.5
CASE delta_y = 3
  delta_y = 1
ENDCASE

w_sav = WOUTPUT()
IF .NOT. EMPTY(w_sav)
  HIDE WINDOW (w_sav)
ENDIF
ACTIVATE SCREEN

SAVE SCREEN TO s_sav
c_sav = SET("CURSOR") = "ON"
SET CURSOR OFF

@ 0, 0 FILL TO WROWS()-1, WCOLS()-1 COLOR n/n

k = 0
DO WHILE .T.
  FOR i = 2 TO s_len
    sled(s_len+2-i,2) = sled(s_len+1-i,2)
    sled(s_len+2-i,3) = sled(s_len+1-i,3)
  ENDFOR
  sled(1,2) = x
  sled(1,3) = y
  k = MOD(k+1,10)
  FOR i = 1 TO s_len-1
    @ FLOOR(sled(s_len+1-i,3)), FLOOR(sled(s_len+1-i,2)) SAY pic(s_len+1-i,k+1) COLOR w/n
  ENDFOR
  @ FLOOR(sled(1,3)), FLOOR(sled(1,2)) SAY sled(1,1) COLOR n/w
    
  IF x = 0 .AND. delta_x < 0 .OR. x+LEN(s_mess) >= WCOLS() .AND. delta_x > 0
    delta_x = -delta_x
  ENDIF
  
  IF y <= 0 .AND. delta_y < 0 .OR. y >= WROWS()-1
    delta_y = -delta_y
  ENDIF
  
  x = x+delta_x
  y = y+delta_y
  
  IF period > 0
    IF INKEY(period,"M") # 0
      EXIT
    ENDIF
  ELSE
    IF INKEY("M") # 0
      EXIT
    ENDIF
  ENDIF
ENDDO

RESTORE SCREEN FROM s_sav

IF c_sav
  SET CURSOR ON
ENDIF
IF .NOT. EMPTY(w_sav)
  ACTIVATE WINDOW (w_sav)
ENDIF

RETURN
