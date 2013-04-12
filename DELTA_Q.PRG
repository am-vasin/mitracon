*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ           เฎฃเ ฌฌ  ฏฎคฃฎโฎขชจ ไ ฉซ  จงฌฅญฅญญจ๏ ญ ซจ็จ๏.              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Delta_Q
PARAMETERS date_s, date_e, is_win
*
*  date_s - ญ ็ ซ์ญ ๏ ค โ ;
*  date_e - ชฎญฅ็ญ ๏ ค โ ;
*  is_win - ญ ซจ็จฅ จญคจช ๆจจ.
*

PRIVATE wd, sc0, clc

USE STOCK ALIAS ST_DELTA AGAIN

SELECT 0
DELETE FILE (tmpo_path+"DELTA_Q.DBF")
DELETE FILE (tmpo_path+"DELTA_Q.CDX")
CREATE DBF (tmpo_path+"DELTA_Q.DBF")  ;
     (   CODE   N(FSIZE("CODE", "ST_DELTA")), ;
         INC_Q  N(8),    ;
         INC_QR N(8),    ;
         EXP_Q  N(8),    ;
         EXP_QR N(8),    ;
         EXP_QF N(8),    ;
         MOV_F  N(8)    )
USE (tmpo_path+"DELTA_Q") EXCLUSIVE
INDEX ON CODE TAG CODE

sc0      = 0
clc      = 0
wd = FSIZE("NAME","ST_DELTA")+FSIZE("PREFIX","ST_DELTA")+21
IF is_win
  DO D_W WITH "Delta_Q", 6, wd, "", 0
  wd = wd-20
  @ 1, 1 SAY "ฺฤฤฤฤฤฤฤฤยฤฤฤฤฤฤย"+REPLICATE("ฤ",wd)+"ฟ"
  @ 2, 1 SAY "ณ เฅฌ๏  ณ ฎเ.Nณ"+PADC("ฎงจๆจ๏     ",wd)+"ณ"
  @ 3, 1 SAY "รฤฤฤฤฤฤฤฤลฤฤฤฤฤฤล"+REPLICATE("ฤ",wd)+"ด"
  @ 4, 1 SAY "ณ        ณ      ณ"+SPACE(wd)+"ณ"
  @ 5, 1 SAY "ภฤฤฤฤฤฤฤฤมฤฤฤฤฤฤม"+REPLICATE("ฤ",wd)+"ู"
ENDIF

DO Clc_Sale

DO Clc_Inc




IF is_win
  RELEASE WINDOW Delta_Q
ENDIF

SELECT ST_DELTA
USE
SELECT DELTA_Q
USE
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     เฎฃเ ฌฌ  เ แ็ฅโ  เ แๅฎค .                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Clc_Sale
PRIVATE w, w1, w2, w3

SELECT 0
USE SALE_TIT ORDER TAG DOC_NUM ALIAS TIT_DELTA AGAIN

SELECT 0
USE SALE     ORDER TAG CODE    ALIAS DET_DELTA AGAIN
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO TIT_DELTA

SELECT ST_DELTA

clc = 1
sc0 = SECONDS()
IF is_win
  @ 0, 0 SAY PADC("……: เฎค ฆจ",WCOLS()) COLOR SCHEME 5
ENDIF

SCAN
  IF is_win
    clc = clc+1
    w = SECOND()-sc0
    w1 = FLOOR(w/3600)
    w2 = FLOOR(w/60)
    w3 = w%60
    w  = RIGHT(STR(100+w1,3),2)+"."+RIGHT(STR(100+w2,3),2)+"."+RIGHT(STR(100+w3,3),2)
    @ 4, 2 SAY w
    @ 4,11 SAY STR(clc,6)
    @ 4,18 SAY PREFIX+" "+NAME
  ENDIF
  IF .NOT. SEEK(CODE, "DET_DELTA")
    LOOP
  ENDIF
  STORE 0 TO w1, w2, w3
  SELECT DET_DELTA
  SCAN REST WHILE CODE = ST_DELTA.CODE FOR BETWEEN(DOC_DATE,date_s,date_e)
    IF TIT_DELTA.CUS_TYPE = 4 .AND. EMPTY(TIT_DELTA.LIST_NUM)
      w2 = w2+QNT
    ELSE
      w1 = w1+QNT
    ENDIF
    w3 = w3+QNT_REP
  ENDSCAN
  SELECT DELTA_Q
  IF .NOT. SEEK(ST_DELTA.CODE)
    APPEND BLANK
  ENDIF
  REPLACE CODE   WITH ST_DELTA.CODE, ;
          EXP_Q  WITH w1,    ;
          EXP_QR WITH w3,    ;
          EXP_QF WITH w2
  SELECT ST_DELTA

ENDSCAN

SELECT TIT_DELTA
USE

SELECT DET_DELTA
USE

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     เฎฃเ ฌฌ  เ แ็ฅโ  ฏเจๅฎค .                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Clc_Inc
PRIVATE w, w1, w2, w3

SELECT 0
USE ACT_S     ORDER TAG CODE    ALIAS INC_DELTA AGAIN

SELECT ST_DELTA

clc = 1
sc0 = SECONDS()
IF is_win
  @ 0, 0 SAY PADC("……: เจๅฎค",WCOLS()) COLOR SCHEME 5
ENDIF

SCAN
  IF is_win
    clc = clc+1
    w = SECOND()-sc0
    w1 = FLOOR(w/3600)
    w2 = FLOOR(w/60)
    w3 = w%60
    w  = RIGHT(STR(100+w1,3),2)+"."+RIGHT(STR(100+w2,3),2)+"."+RIGHT(STR(100+w3,3),2)
    @ 4, 2 SAY w
    @ 4,11 SAY STR(clc,6)
    @ 4,18 SAY PREFIX+" "+NAME
  ENDIF
  IF .NOT. SEEK(CODE, "INC_DELTA")
    LOOP
  ENDIF
  STORE 0 TO w1, w2
  SELECT INC_DELTA
  SCAN REST WHILE CODE = ST_DELTA.CODE FOR BETWEEN(DOC_DATE,date_s,date_e)
    w1 = w1+QNT
    w2 = w2+QNT_REP
  ENDSCAN
  SELECT DELTA_Q
  IF .NOT. SEEK(ST_DELTA.CODE)
    APPEND BLANK
  ENDIF
  REPLACE CODE   WITH ST_DELTA.CODE, ;
          INC_Q  WITH w1,    ;
          INC_QR WITH w2
  SELECT ST_DELTA

ENDSCAN

SELECT TIT_DELTA
USE

SELECT DET_DELTA
USE

RETURN
