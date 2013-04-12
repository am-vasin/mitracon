*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º                  à®£à ¬¬  ãáâ ­®¢ª¨ áª¨¤ª¨.                          º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Discount
PARAMETERS lst_o, ext_menu   && ¯¥à¥ç¥­ì á¯¨áª®¢ ¨ ¨¬ï ¢­¥è­¥£® ¬¥­î

PRIVATE rc_sav, als_sav, lst_o, sum001, sum002, flg, wk, m_nam, w_sav

rc_sav = RECNO("LIST_TIT")
als_sav = SELECT()
m_nam = IIF(TYPE("ext_menu") = "C", ext_menu, "")
PUSH KEY CLEAR
IF .NOT. EMPTY(m_nam)
  HIDE MENU (m_nam)
ENDIF
w_sav = WOUTPUT()
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13

SELECT 0
USE DIS_SCH

SELECT 0
USE path_comm+"SUBJECT" ORDER TAG STO_CODE ALIAS SUB_DIS AGAIN

SELECT 0
USE LIST_DET ORDER TAG L_NAME ALIAS LIST_DIS AGAIN
SET RELATION TO CODE INTO SUB_DIS

flg = " "
sum001 = 0
sum002 = 0

DO Dis_Sum WITH lst_o, sum001, sum002, flg
PRIVATE mss
wk = .T.
DO CASE
CASE sum001 = 0 .AND. (flg # "1" .OR. .NOT. EMPTY(lst_o))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Š ª ï áª¨¤ª ? ‘ ç¥£®?"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  wk = .F.
CASE flg = "0"
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  € â ¬ ¥áâì ¤¥ä¨æ¨â. ‘ç¨â ¥¬ áª¨¤ªã? [ „  ] [ ¥â ]"
  mss(3) = ""
  wk = Out_Mess(7,"mss") = 1
CASE flg = "1"
  wk = .F.
  IF EMPTY(lst_o)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ‘ª¨¤ª  ã¦¥ ¯®áç¨â ­ . Žâ¬¥­¨¬? [ „  ] [ ¥â ]"
    mss(3) = ""
    IF Out_Mess(7,"mss") = 1
      DO Clr_Dis
    ENDIF
  ELSE
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"‘ª¨¤ª  ã¦¥ ¯®áç¨â ­ ."
    mss(3) = ""
    DO Out_Mess WITH 7,"mss"
  ENDIF
CASE flg = "2"
  wk = .F.
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Š ª ï áª¨¤ª ? à®¤ ­®!"
  mss(3) = ""
  DO Out_Mess WITH 7,"mss"
CASE flg = "3"
  wk = .F.
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Œë á ¯®¤ àª®¢ áª¨¤ªã ­¥ ¤ ¥¬!"
  mss(3) = ""
  DO Out_Mess WITH 7,"mss"
ENDCASE

IF wk
  DO Dis_Val WITH lst_o, sum001, sum002
ENDIF

SELECT LIST_DIS
USE
SELECT DIS_SCH
USE
SELECT SUB_DIS
USE

SELECT (als_sav)
GO rc_sav IN LIST_TIT

ACTIVATE SCREEN
IF .NOT. EMPTY(m_nam)
  @ 0, 0 SAY PADL("Œ¥­î F10 ",WCOLS()) COLOR SCHEME 13
  SHOW MENU (m_nam)
ENDIF
IF .NOT. EMPTY(w_sav)
  ACTIVATE WINDOW (w_sav)
ENDIF

POP KEY

RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º                  à®£à ¬¬  ®â¬¥­ë áª¨¤ª¨.                             º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Clr_Dis

PRIVATE sum_1
SELECT LIST_DIS

n_wk = STR(LIST_TIT.LST_NUM,6)
SEEK n_wk
sum_1 = 0
SCAN REST WHILE n_wk = LIST_DIS.LIST_NUM FOR CODE >= 0
  IF EMPTY(IS_SPEC) .AND. EMPTY(IS_OPT)
    IF LIST_TIT.IS_USD
      REPLACE SAL_PRICE WITH 0, ;
              USD_PRICE WITH USD_DEF
    ELSE
      REPLACE SAL_PRICE WITH DEF_PRICE, ;
              USD_PRICE WITH 0
    ENDIF
  ENDIF
  sum_1 = sum_1+QNT*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)
ENDSCAN
SELECT LIST_TIT
REPLACE R_TOTAL WITH sum_1,;
        IS_DIS  WITH " ",  ;
        WHO_LET WITH 0,    ;
        PRESENT WITH 0
RETURN
