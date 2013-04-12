*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            ฎข ๏ ฏเฎฃเ ฌฌ  ฏเฎแฌฎโเ  คฅไฅชโญ๋ๅ ญ ชซ คญ๋ๅ.             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE New_V_I

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

IF .NOT. FILE_O(log_path+"N_T_I.DBF")   && ฅโ ไ ฉซ  แ เฅงใซ์โ โฎฌ
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"เฎขฅเชจ ญ ชซ คญ๋ๅ ญฅ ฏเฎจงขฎคจซฎแ์."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

USE (log_path+"N_T_I")
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ คฅไฅชโญ๋ๅ ญ ชซ คญ๋ๅ!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  USE
  RETURN
ENDIF

DO Use_Dummy
SELECT N_T_I

PRIVATE ln, wd, tt, ft
DIMENSION tt(3), ft(1)

IF SET("CENTURY") = "ON"
  tt(1) = ;
  "ฤฤฤฤฤฤฤยฤฤฤยฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤ"
  tt(2) = ;
  " N/ฏฎเ ณ”เฌณ N ฎช-โ   ณ    โ    ณ   ซจฅญโ       ณ่."
  tt(3) = ;
  "ฤฤฤฤฤฤฤลฤฤฤลฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤ"
  ft(1) = ;
  "ฤฤฤฤฤฤฤมฤฤฤมฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤ"
ELSE
  tt(1) = ;
  "ฤฤฤฤฤฤฤยฤฤฤยฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤ"
  tt(2) = ;
  " N/ฏฎเ ณ”เฌณ N ฎช-โ   ณ   โ   ณ   ซจฅญโ       ณ่."
  tt(3) = ;
  "ฤฤฤฤฤฤฤลฤฤฤลฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤ"
  ft(1) = ;
  "ฤฤฤฤฤฤฤมฤฤฤมฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤ"
ENDIF

ln = WROWS("")-10
wd = LEN(ft(1))
DO Def_Win WITH "Tst_I_M", "Tst_I_E", "Tst_I_I", ln, wd, ;
                "‘ฏจแฎช คฅไฅชโญ๋ๅ ญ ชซ คญ๋ๅ", tt, ft

@ 1, 1 SAY "ฺ"
@ 2, 1 SAY "ณ"
@ 3, 1 SAY "ร"
@ WROWS()-1, 1 SAY "ภ"

@ 1, WCOLS()-2 SAY "ฟ"
@ 2, WCOLS()-2 SAY "ณ"
@ 3, WCOLS()-2 SAY "ด"
@ WROWS()-1, WCOLS()-2 SAY "ู"

ACTIVATE WINDOW Tst_I_E
GO TOP
BROWSE FIELDS NUMB:H="",     ;
              FIRM:H="",     ;
              DOC_NUM:H="",  ;
              DOC_DATE:H="", ;
              DOC_NAME:H="", ;
              IS_DEF:H="",   ;
              DUMMY.F:H="" FREEZE DUMMY.F  ;
              NOMENU WINDOW Tst_I_I IN WINDOW Tst_I_E COLOR (sch)

RELEASE WINDOW Tst_I_I
RELEASE WINDOW Tst_I_M
RELEASE WINDOW Tst_I_E
CLOSE DATABASES
RETURN
