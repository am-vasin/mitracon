*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                ฎข ๏ ฏเฎฃเ ฌฌ  ฏเฎแฌฎโเ  ก ซ ญแ  ฏฎ ญ ซจ็จ๎.          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE New_V_N

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

IF .NOT. FILE_O(log_path+"N_T_N.DBF")   && ฅโ ไ ฉซ  แ เฅงใซ์โ โฎฌ
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"เฎขฅเชจ ก ซ ญแ  ญฅ ฏเฎจงขฎคจซฎแ์."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

USE (log_path+"N_T_N")
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ เ แๅฎฆคฅญจฉ ฏฎ ชฎซจ็ฅแโขใ!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  USE
  RETURN
ENDIF

DO Use_Dummy
SELECT N_T_N

PRIVATE ln, wd, tt, ft
DIMENSION tt(3), ft(1)
tt(1) = ;
"ฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤยฤฤฤฤยฤฤฤฤยฤฤฤฤยฤฤฤฤยฤฤฤฤยฤฤฤฤ"
tt(2) = ;
" N/ฏฎเ ณ   ฎงจๆจ๏                    ณ ซ.ณฅ ซณ”ซ.ณ”ซ.ณ เ.ณ‘ฏ.ณ‘ฏ.   "
tt(3) = ;
"ฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤลฤฤฤฤ"
ft(1) = ;
"ฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤมฤฤฤฤมฤฤฤฤมฤฤฤฤมฤฤฤฤมฤฤฤฤมฤฤฤฤ"

ln = WROWS("")-10
wd = LEN(ft(1))
DO Def_Win WITH "Tst_N_M", "Tst_N_E", "Tst_N_I", ln, wd, ;
                "ฎงจๆจจ แ ญ เใ่ฅญญ๋ฌ ก ซ ญแฎฌ ฏฎ ญ ซจ็จ๎", tt, ft

@ 1, 1 SAY "ฺ"
@ 2, 1 SAY "ณ"
@ 3, 1 SAY "ร"
@ WROWS()-1, 1 SAY "ภ"

@ 1, WCOLS()-2 SAY "ฟ"
@ 2, WCOLS()-2 SAY "ณ"
@ 3, WCOLS()-2 SAY "ด"
@ WROWS()-1, WCOLS()-2 SAY "ู"

ACTIVATE WINDOW Tst_N_E
GO TOP
BROWSE FIELDS NUMB:H="",   ;
              f001 = PREFIX+" "+Nm_Part(NAME):30:H="",   ;
              NAL_S:H="",  ;
              NAL_R:H="",  ;
              MOV_F:H="",  ;
              NAL_F:H="",  ;
              NAL_B:H="",  ;
              NAL_B:H="",  ;
              NAL_B:H="",  ;
              NAL_SP:H="", ;
              MOV_SP:H="", ;
              DUMMY.F:H="" FREEZE DUMMY.F  ;
              NOMENU WINDOW Tst_N_I IN WINDOW Tst_N_E COLOR (sch)
RELEASE WINDOW Tst_N_I
RELEASE WINDOW Tst_N_M
RELEASE WINDOW Tst_N_E
CLOSE DATABASES
RETURN
