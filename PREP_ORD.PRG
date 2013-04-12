*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          เฎฃเ ฌฌ  แฎงค ญจ๏ จ/จซจ ฏฅ็ โจ ง ๏ขชจ ญ  แชซ ค.             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prep_Ord

PRIVATE mss, w_rem
PRIVATE o_l, o_n, i, k, ln, wd, s, p_d, ord_len, n_ords

WAIT WIND "ฏฅเ ๆจ๏ ฎโฌฅญฅญ " NOWAIT
RETURN

ord_len = 20      &&  ชแจฌ ซ์ญ ๏ คซจญ  ง ๏ขชจ.
n_ords  = 1

IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ช ๏ ฌฎฆฅโ ก๋โ์ ง ๏ขช ? ‘ฏจแฎช ฏเฎค ญ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

w_rem   = WOUTPUT()
PUSH KEY CLEAR

IF LIST_TIT.ORD_NUM # 0
  o_n = LIST_TIT.ORD_NUM+1
  DIMENSION o_l(o_n)
  FOR i = 1 TO LIST_TIT.ORD_NUM
    o_l(i) = STR(LIST_TIT.LST_NUM, 6)+"/"+ALLTRIM(STR(i,3))
    o_l(i) = PADR(o_l(i),10)
  ENDFOR
  o_l(o_n) = "ฎข ๏     "
  ln = MIN(o_n+4, WROWS("")-9)
  wd = LEN(o_l(o_n))+4
  DO D_W WITH "W_Tmp", ln, wd, " ๏ขชจ", 0
  @ WROWS()-1, 0 SAY PADC("Esc-ฎโช ง โ์แ๏",WCOLS())
  i = o_n
  DO WHILE .T.
    i = MIN(i,o_n)
    i = MAX(i,0)
    @ 1,1 MENU o_l, o_n, WROWS()-4
    READ MENU TO i
    s = READKEY()%256
    IF s = 12
      RELEASE WINDOW W_Tmp
      IF EMPTY(w_rem)
        ACTIVATE SCREEN
      ELSE
        ACTIVATE WINDOW (w_rem)
      ENDIF
      RETURN
    ENDIF
    IF s = 15
      RELEASE WINDOW W_Tmp
      IF EMPTY(w_rem)
        ACTIVATE SCREEN
      ELSE
        ACTIVATE WINDOW (w_rem)
      ENDIF
      EXIT
    ENDIF
  ENDDO
ELSE
  i = 1
ENDIF

p_d = i <= LIST_TIT.ORD_NUM

IF i > LIST_TIT.ORD_NUM
  p_d = Md_Ord()
ENDIF

IF p_d
  FOR k = i TO i+n_ords-1
    IF new_ord
      DO Prn_OrdN WITH LIST_TIT.LST_NUM, k
    ELSE
      DO Prn_Ord WITH LIST_TIT.LST_NUM, k
    ENDIF
  ENDFOR
ENDIF
POP KEY

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  แฎงค ญจ๏ ง ๏ขชจ ญ  แชซ ค.                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Md_Ord

PRIVATE sel, cnt, l_w, delta, mss, i, check1, check2, cnt_neg, cnt_pos
PRIVATE note_w, dt_ready, tm_ready, curZone, tmpZone
PRIVATE s_sav00

DIMENSION mss(5)
mss(1) = ""
mss(2) = ""
mss(3) = ""
mss(4) = ""
mss(5) = ""

sel = SELECT()

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S000 AGAIN
IF File_O(base_path+"ZONES.DBF")
  SELECT 0
  USE (base_path+"ZONES") ORDER TAG ZONE
ENDIF
SELECT 0
USE (base_path+"ORD_T") ALIAS OT000 AGAIN
SELECT 0
USE (base_path+"ORD_D") ALIAS O000 AGAIN
SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_ADDRESS ALIAS L000 AGAIN
SET RELATION TO CODE INTO S000
l_w = STR(LIST_TIT.LST_NUM,6)
SEEK l_w
cnt = 0
cnt_neg = 0
cnt_pos = 0
n_ords = 0
check1 = 0
check2 = 0
sum_list = LIST_TIT.R_TOTAL
DO Get_Sto WITH LIST_TIT.ORD_QUEUE
IF LIST_TIT.ORD_NUM = 0
  IF EMPTY(LIST_TIT.DATE_REQ)
    IF EMPTY(t_stock)
      dt_ready = {}
      tm_ready = ""
    ELSE
      dt_ready = DATE()
      tm_ready = TIME()
      DO Add_T WITH dt_ready, tm_ready, t_stock+2, dt_ready, tm_ready
    ENDIF
  ELSE
    dt_ready = LIST_TIT.DATE_REQ
    tm_ready = LIST_TIT.TIME_REQ
  ENDIF
ELSE
  dt_ready = LIST_TIT.DATE_REQ
  tm_ready = LIST_TIT.TIME_REQ
ENDIF
note_w = SPACE(FSIZE("NOTE","OT000"))
p_d = .NOT. hard_sale
IF Ord_Note("N "+ALLTRIM(STR(LIST_TIT.LST_NUM))+", "+ ;
                  ALLTRIM(LIST_TIT.LST_NAME), dt_ready, tm_ready, note_w, p_d, LIST_TIT.ORD_QUEUE)
  curZone = CHR(0)+CHR(0)
  s_sav00 = SELECT()
  SELECT 0
  USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS OT_0105 AGAIN
  SEEK STR(LIST_TIT.LST_NUM,6)
  REPLACE REST WHILE LIST_NUM = STR(LIST_TIT.LST_NUM,6) ;
          REQ_DATE WITH dt_ready, REQ_TIME WITH tm_ready
  USE
  SELECT (s_sav00)
  REPLACE LIST_TIT.DATE_REQ WITH dt_ready, LIST_TIT.TIME_REQ WITH tm_ready

  sum_list = 0
  SCAN REST WHILE LIST_NUM = l_w
    sum_list = sum_list+QNT*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)
    IF CODE <= 0 .OR. QNT = QNT_REQ
      LOOP
    ENDIF
    delta = QNT - QNT_REQ
    IF delta > 0 .AND. S000.QNT-delta < 0
      mss(2) = CHR(0)+"ฎงจๆจ๏ "+ALLTRIM(S000.PREFIX)+" "+ALLTRIM(S000.NAME)+":"
      mss(3) = CHR(0)+"ง ฏเฎ่ฅญฎ "+ALLTRIM(STR(delta))+  ;
                      " ข ญ ซจ็จจ "+ALLTRIM(STR(S000.QNT))+","
      mss(4) = CHR(0)+"เฎญจเใฅฌ "+ALLTRIM(STR(MAX(S000.QNT,0)))+"."
      DO Out_Mess WITH 7, mss
      delta = MAX(S000.QNT,0)
    ENDIF
    tmpZone = WhatZone(ADDRESS)
    IF cnt >= ord_len .OR. tmpZone # curZone
      n_ords = n_ords+1
      SELECT LIST_TIT
      REPLACE NOTE_STO WITH "*",  ;
              ORD_NUM  WITH ORD_NUM+1
      SELECT OT000
      REPLACE SIZE_NEG WITH cnt_neg, SIZE_POS WITH cnt_pos
      APPEND BLANK
      REPLACE LIST_NUM  WITH l_w, ;
              LIST_NAME WITH LIST_TIT.LST_NAME, ;
              ORD_NUM   WITH LIST_TIT.ORD_NUM,  ;
              ORD_DATE  WITH DATE(),   ;
              ORD_TIME  WITH TIME(),   ;
              WHO       WITH user,     ;
              WHERE     WITH arm,      ;
              NOTE      WITH note_w,   ;
              ZONE      WITH tmpZone,  ;
              QUEUE     WITH LIST_TIT.ORD_QUEUE, ;
              REQ_DATE  WITH dt_ready, ;
              REQ_TIME  WITH tm_ready
      cnt = 0
      cnt_neg = 0
      cnt_pos = 0
      curZone = tmpZone
    ENDIF
    cnt = cnt+1
    SELECT S000
    REPLACE QNT       WITH QNT-delta,  ;
            QNT_SAVE  WITH QNT_SAVE+delta
    SELECT O000
    APPEND BLANK
    REPLACE LIST_NUM WITH l_w, ;
            ORD_NUM  WITH LIST_TIT.ORD_NUM, ;
            CODE     WITH L000.CODE,  ;
            QNT      WITH delta
    IF delta < 0
      cnt_neg = cnt_neg+1
    ELSE
      cnt_pos = cnt_pos+1
    ENDIF
    SELECT L000
    REPLACE QNT     WITH QNT_REQ+delta,  ;
            QNT_REQ WITH QNT_REQ+delta
  ENDSCAN
  IF cnt # 0
    SELECT OT000
    REPLACE SIZE_NEG WITH cnt_neg, SIZE_POS WITH cnt_pos
    SELECT L000
  ENDIF
  IF n_ords = 0
    p_d = .F.
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"‘ฏจแฎช ฏฎซญฎแโ์๎ ง กเฎญจเฎข ญ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF

ENDIF

USE
SELECT OT000
USE
SELECT O000
USE
SELECT S000
USE
IF USED("ZONES")
  SELECT ZONES
  USE
ENDIF
SELECT LIST_TIT
REPLACE R_TOTAL  WITH sum_list, ;
        DATE_REQ WITH dt_ready, ;
        TIME_REQ WITH tm_ready

SELECT (sel)

RETURN p_d
