*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                        º
*º                     ®áë«ª  ä ©«  ­ «¨ç¨ï ¢ ä¨«¨ «¥.                   º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ 15.04.98 11:31:54 Í¼
PROCEDURE S_List
PARAMETERS cc

PRIVATE s_sav, f_name, mss, q_type, w_q, cnt

s_sav = SELECT()

SELECT 0
USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE ALIAS S_STO_T0 AGAIN
IF .NOT. SEEK(cc)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"‡  ä¨«¨ «®¬ ­¨ç¥£® ­¥ ç¨á«¨âáï."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN .T.
ENDIF

DIMENSION mss(4)
mss(1) = ""
mss(2) = CHR(0)+"‚ ª ª®¬ ¢¨¤¥ ¯¥à¥¤ ¥¬ á¢®¥ ­ «¨ç¨¥?"
mss(3) = CHR(0)+"   [ ¨ ¢ ª ª®¬! ] [ ’®«ìª® ¯®¬¥âª¨ ] [ Š®«¨ç¥áâ¢  ]"
mss(4) = ""
q_type = Out_Mess(5, "mss")

DELETE FILE (tmpo_path+"RYNL00_T.DBF")
COPY NEXT 1 TO (tmpo_path+"RYNL00_T.DBF")
USE

DELETE FILE (tmpo_path+"RYNL00.DBF")
CREATE DBF (tmpo_path+"RYNL00") ;
  ( CUS_CODE  N( 8), ;
    CODE      N( 5), ;
    PRO_CODE  N( 7), ;
    OBJ_CODE  N(10), ;
    PREFIX    C( 4), ;
    NAME      C(35), ;
    PRODUCER  C( 8), ;
    NUM_PART  C(35), ;
    NAME_ADD  C(35), ;
    DIV_CODE  N( 5), ;
    QNT_STORE N( 6), ;
    QNT_ROUND N( 5), ;
    DATE_ON   D,     ;
    DATE_OFF  D,     ;
    WHO_ADD   N( 3), ;
    WHERE_ADD C( 3), ;
    WHO_LEG   N( 3), ;
    NOTE_S    M,     ;
    QNT       N( 6), ;
    PRICE_D   N(10,4),;
    PRICE_R   N(10,2)  )

SELECT 0
USE (base_path+"RYN_L") ORDER TAG C_CODE ALIAS S_RYN_L AGAIN
SELECT 0
USE (base_path+"STOCK") ALIAS STO_TTT AGAIN
SET RELATION TO STR(cc,5)+STR(CODE,7) INTO S_RYN_L

DO Wt_Mess WITH "®¤£®â®¢ª  ­®¬¥­ª« âãàë..."
cnt = 0

SCAN
  cnt = cnt+1
  IF cnt % 100 = 0
    @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)),WCOLS())
  ENDIF
  IF .NOT. EMPTY(DATE_OFF) .AND. .NOT. FOUND("S_RYN_L")
    LOOP
  ENDIF
  SELECT RYNL00
  APPEND BLANK
  DO CASE
  CASE q_type = 1
    q_w = 0
  CASE q_type = 2
    q_w = IIF(STO_TTT.QNT > 0, -99999, 0)
  CASE q_type = 3
    q_w = STO_TTT.QNT
  ENDCASE
  REPLACE CUS_CODE  WITH cc,                ;
          CODE      WITH STO_TTT.CODE,      ;
          PRO_CODE  WITH STO_TTT.PRO_CODE,  ;
          OBJ_CODE  WITH STO_TTT.OBJ_CODE,  ;
          PREFIX    WITH STO_TTT.PREFIX,    ;
          NAME      WITH STO_TTT.NAME,      ;
          PRODUCER  WITH STO_TTT.PRODUCER,  ;
          NUM_PART  WITH STO_TTT.NUM_PART,  ;
          NAME_ADD  WITH STO_TTT.NAME_ADD,  ;
          DIV_CODE  WITH STO_TTT.DIV_CODE,  ;
          QNT_STORE WITH q_w,               ;
          QNT_ROUND WITH STO_TTT.QNT_ROUND, ;
          DATE_ON   WITH STO_TTT.DATE_ON,   ;
          DATE_OFF  WITH STO_TTT.DATE_ON,   ;
          WHO_ADD   WITH STO_TTT.WHO_ADD,   ;
          WHERE_ADD WITH STO_TTT.WHERE_ADD, ;
          WHO_LEG   WITH STO_TTT.WHO_LEG,   ;
          NOTE_S    WITH STO_TTT.NOTE,      ;
          QNT       WITH IIF(FOUND("S_RYN_L"), S_RYN_L.QNT, 0), ;
          PRICE_D   WITH IIF(FOUND("S_RYN_L"), S_RYN_L.PRICE_D, 0),   ;
          PRICE_R   WITH IIF(FOUND("S_RYN_L"), S_RYN_L.PRICE_R, 0)
  SELECT STO_TTT
ENDSCAN
DO Wt_Mess

USE
SELECT RYNL00
USE
SELECT S_RYN_L
USE

SELECT (s_sav)

RETURN "RYNL00"
