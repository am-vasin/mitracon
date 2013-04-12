*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎแฌฎโเ คฅไฅชโญ๋ๅ คฅฏฎงจโฎข.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE New_V_C

PRIVATE sch, mss

sch = SCHEME(13,1)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,2)+","+ ;
      SCHEME(13,4)+","+ ;
      SCHEME(13,5)+","+ ;
      SCHEME(13,6)+","+ ;
      SCHEME(13,7)+","+ ;
      SCHEME(13,8)+","+ ;
      SCHEME(13,9)

IF .NOT. FILE_O(log_path+"N_T_C.DBF")   && ฅโ ไ ฉซ  แ เฅงใซ์โ โฎฌ
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"เฎขฅเชจ คฅฏฎงจโฎข ญฅ ฏเฎจงขฎคจซฎแ์."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

USE (log_path+"N_T_C")
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ คฅไฅชโญ๋ๅ คฅฏฎงจโฎข!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  USE
  RETURN
ENDIF

DO Use_Dummy
SELECT N_T_C

PRIVATE ln, wd, tt, ft, w1
DIMENSION tt(3), ft(1)
tt(1) = ;
"ฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤ"
tt(2) = ;
" N/ฏฎเ ณ         ฎชใฏ โฅซ์                               ณฅไ."
tt(3) = ;
"ฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤ"
ft(1) = ;
"ฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤ"

ln = WROWS("")-10
wd = LEN(ft(1))
DO Def_Win WITH "Tst_C_M", "Tst_C_E", "Tst_C_I", ln, wd, ;
                "ฅไฅชโญ๋ฅ คฅฏฎงจโ๋", tt, ft

@ 1, 1 SAY "ฺ"
@ 2, 1 SAY "ณ"
@ 3, 1 SAY "ร"
@ WROWS()-1, 1 SAY "ภ"

@ 1, WCOLS()-2 SAY "ฟ"
@ 2, WCOLS()-2 SAY "ณ"
@ 3, WCOLS()-2 SAY "ด"
@ WROWS()-1, WCOLS()-2 SAY "ู"

ACTIVATE WINDOW Tst_C_E
GO TOP

BROWSE FIELDS NUMB:H="",     ;
              CUS_NAME:H="", ;
              IS_DEF:H="", ;
              DUMMY.F:H="" FREEZE DUMMY.F  ;
              WINDOW Tst_C_I IN WINDOW Tst_C_E COLOR (sch)

RELEASE WINDOW Tst_C_I
RELEASE WINDOW Tst_C_M
RELEASE WINDOW Tst_C_E
CLOSE DATABASES
RETURN
