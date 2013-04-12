*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                   บ
*บ                 เฎฃเ ฌฌ  ฏเฎขฅเชจ ก ซ ญแ .                       บ
*บ                                                                   บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Tst_Bal
PARAMETERS switch
PRIVATE sw_nc, sw_nv, sw_cc, sw_cv, sw_ic, sw_iv, sw_kc, sw_kv, sw_sc, sw_sv
PRIVATE ln, wd, ex, sw_wk

sw_wk = TYPE("switch") = "L"

*                            เฎขฅเช  ก ซ ญแ 
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤ ซจ็จฅฤฤฤฤฤฤฟ ฺฤฅฏฎงจโ๋ฤฤฤฤฤฟ ฺฤ ชซ คญ๋ฅฤฤฤฤฟ ฺฤ แแ ฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ [ ]  แ็ฅโ   ณ ณ [ ]  แ็ฅโ   ณ ณ [ ]  แ็ฅโ   ณ ณ [ ]  แ็ฅโ & ณ ณ
*ณ ณ [ ] เฎแฌฎโเ ณ ณ [ ] เฎแฌฎโเ ณ ณ [ ] เฎแฌฎโเ ณ ณ     เฎแฌฎโเ ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ                                < OK >                               ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
*    5                22          34   39               56               72
*

*         เฎขฅเช  ก ซ ญแ 
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ  ฺฤ ซจ็จฅฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ  ณ
*ณ  ณ [ ]  แ็ฅโ  [ ] เฎแฌฎโเ ณ  ณ
*ณ  ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู  ณ
*ณ  ฺฤฅฏฎงจโ๋ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ  ณ
*ณ  ณ [ ]  แ็ฅโ  [ ] เฎแฌฎโเ ณ  ณ
*ณ  ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู  ณ
*ณ  ฺฤ ชซ คญ๋ฅฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ  ณ
*ณ  ณ [ ]  แ็ฅโ  [ ] เฎแฌฎโเ ณ  ณ
*ณ  ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู  ณ
*ณ  ฺฤ ซฎฃฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ  ณ
*ณ  ณ [ ]  แ็ฅโ  [ ] เฎแฌฎโเ ณ  ณ
*ณ  ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู  ณ
*ณ  ฺฤ แแ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ  ณ
*ณ  ณ [ ]  แ็ฅโ  [ ] เฎแฌฎโเ ณ  ณ
*ณ  ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู  ณ
*ณ             < OK >             ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู


IF sw_wk
  wd = 36
  ln =  19
  sw_nc = .F.
  sw_nv = .T.
  sw_cc = .F.
  sw_cv = .T.
  sw_ic = .F.
  sw_iv = .T.
  sw_sc = .F.
  sw_sv = .T.
  sw_kc = .F.
  sw_kv = .T.
  ex    = 1
ELSE
  sw_nc = "N" $ UPPER(switch)
  sw_nv = .F.
  sw_cc = "C" $ UPPER(switch)
  sw_cv = .F.
  sw_ic = "S" $ UPPER(switch)
  sw_iv = .F.
  sw_sc = "A" $ UPPER(switch)
  sw_sv = .F.
  sw_kc = "K" $ UPPER(switch)
  sw_kv = .F.
ENDIF

IF sw_wk
  DO D_W WITH "T_Bal", ln, wd, "เฎขฅเช  ก ซ ญแ ", 1

  @ 2,  4 TO 4, 31
  @ 2,  6 SAY " ซจ็จฅ"

  @ 5,  4 TO 7, 31
  @ 5,  6 SAY "ฅฏฎงจโ๋"

  @ 8,  4 TO 10, 31
  @ 8,  6 SAY " ชซ คญ๋ฅ"

  @ 11,  4 TO 13, 31
  @ 11,  6 SAY " ซฎฃ"

  @ 14,  4 TO 16, 31
  @ 14,  6 SAY " แแ "

  @ 3,  6 GET sw_nc PICTURE "@*C  แ็ฅโ"
  @ 3, 18 GET sw_nv PICTURE "@*C เฎแฌฎโเ"

  @ 6,  6 GET sw_cc PICTURE "@*C  แ็ฅโ"
  @ 6, 18 GET sw_cv PICTURE "@*C เฎแฌฎโเ"

  @ 9,  6 GET sw_ic PICTURE "@*C  แ็ฅโ"
  @ 9, 18 GET sw_iv PICTURE "@*C เฎแฌฎโเ"

  @ 12,  6 GET sw_sc PICTURE "@*C  แ็ฅโ"
  @ 12, 18 GET sw_sv PICTURE "@*C เฎแฌฎโเ"

  @ 15,  6 GET sw_kc PICTURE "@*C  แ็ฅโ"
  @ 15, 18 GET sw_kv PICTURE "@*C เฎแฌฎโเ"

  @ 17, 15 GET ex PICTURE "@*HT \ OK "
  READ CYCLE

  RELEASE WINDOW T_Bal

ENDIF

IF sw_nc
  DO New_T_N WITH sw_wk
ENDIF
IF sw_nv
  DO New_V_N
ENDIF

IF sw_cc
  DO New_T_C WITH sw_wk
ENDIF
IF sw_cv
  DO New_V_C
ENDIF

IF sw_ic
  DO New_T_I WITH sw_wk
ENDIF
IF sw_iv
  DO New_V_I
ENDIF

IF sw_sc
  DO New_T_S WITH "c"
ENDIF
IF sw_sv
  DO New_T_S WITH "v"
ENDIF

IF sw_kc
  DO Main_K WITH "c"
  DO New_T_K WITH "c"
ENDIF

IF sw_kv
  DO Main_K WITH "v"
  DO New_T_K WITH "v"
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                   บ
*บ       เฎฃเ ฌฌ  ฏเฎขฅเชจ ก ซ ญแ  ฎแญฎขญฎฉ ช แแ๋.                  บ
*บ                                                                   บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Main_K
PARAMETER mode

PRIVATE s00_d0, s00_r0, s00_d1, s00_r1, s00_d2, s00_r2, s00_dat, s00_tim,  ;
        ln, wd
IF mode = "c"
  PRIVATE msslog_rec, err_cnt

  log_rec = Use_Log("เฎขฅเช  ฎแญ. ช แแ๋")

  USE (base_path+"KASSA_M")
  STORE 0 TO s00_r1, s00_d1
  SUM SUM_R, SUM_D TO s00_r1, s00_d1
  USE (base_path+"KASSA_S")
  s00_d0 = SUM_D_S
  s00_r0 = SUM_R_S
  s00_d2 = SUM_D
  s00_r2 = SUM_R
  s00_dat = DATE()
  s00_tim = TIME()
  s00_dat = DATE()
  s00_tim = TIME()
  err_cnt = 0
  err_cnt = err_cnt+IIF(s00_r0+s00_r1 = s00_r2, 0, 1)
  err_cnt = err_cnt+IIF(s00_d0+s00_d1 = s00_d2, 0, 1)

  DELETE FILE (log_path+"T_KASSA.MEM")
  SAVE TO (log_path+"T_KASSA.MEM") ALL LIKE s00_*
  DO Term_Log WITH log_rec, err_cnt

ELSE

  IF .NOT. File_O(log_path+"T_KASSA.MEM")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"ฅโ ไ ฉซ  แ เฅงใซ์โ โฎฌ ฏเฎขฅเชจ ช แแ๋!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN
  ENDIF
  RESTORE FROM (log_path+"T_KASSA.MEM") ADDITIVE
  USE (base_path+"KASSA_S")
  ln = 9
  wd = 76
  DO D_W WITH "Main_K", ln, wd,  ;
              "แญฎขญ ๏ ช แแ  ฎโ "+DTOC(s00_dat)+" "+s00_tim, 0
  @ 0, 0 SAY ""
? " ฺฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
? " ณ  ซ๎โ  ณ  ็. งญ ็ฅญจ๏ ณ   ขจฆฅญจฅ    ณแโ โฎช เ แ็ฅโ.ณแโ โฎช ฏฎ ..ณ"
? " รฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด"
? " ณ ใกซจ  ณ               ณ               ณ               ณ               ณ"
? " รฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด"
? " ณ USD    ณ               ณ               ณ               ณ               ณ"
? " ภฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
? "                            ฆฌจโฅ ซ๎กใ๎ ชซ ขจ่ใ."
  PRIVATE c1, c2
  c1 = IIF(s00_r0+s00_r1 = s00_r2, 13, 7)
  c2 = IIF(s00_d0+s00_d1 = s00_d2, 13, 7)
  @ 4, 12 SAY STR(s00_r0,14,2)
  @ 4, 28 SAY STR(s00_r1,14,2)
  @ 4, 44 SAY STR(s00_r0+s00_r1,14,2) COLOR SCHEME c1
  @ 4, 60 SAY STR(s00_r2,14,2) COLOR SCHEME c1
  @ 6, 12 SAY STR(s00_d0,14,4)
  @ 6, 28 SAY STR(s00_d1,14,4)
  @ 6, 44 SAY STR(s00_d0+s00_d1,14,4) COLOR SCHEME c2
  @ 6, 60 SAY STR(s00_d2,14,4) COLOR SCHEME c2
  
  = INKEY(0,"HM")
  RELEASE WINDOW Main_K

ENDIF
USE
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ       เฎฃเ ฌฌ  ฎโชเ๋โจ๏ ( แฎงค ญจ๏ ) ฆใเญ ซ  ฏเฎขฅเชจ ก ซ ญแ .        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 09.07.97 10:07:21 อผ
PROCEDURE Use_Log
PARAMETER t_nm    && ฌ๏ โฅแโ 
PRIVATE i

IF .NOT. File_O(base_path+"BAL_LOG.DBF") .OR. ;
   .NOT. File_O(base_path+"BAL_LOG.CDX")
  DELETE FILE (base_path+"BAL_LOG.DBF")
  DELETE FILE (base_path+"BAL_LOG.CDX")
  CREATE DBF (base_path+"BAL_LOG")  ;
       ( S_DATE  D,     ;
         S_TIME  C( 8), ;
         T_DATE  D,     ;
         T_TIME  C( 8), ;
         T_NAME  C(24), ;
         ERRORS  N( 6)  )
  INDEX ON DTOS(S_DATE)+S_TIME TAG HRON
  FOR i = 1 TO 100
    APPEND BLANK
  ENDFOR
  GO TOP
ELSE
  USE (base_path+"BAL_LOG") ORDER TAG HRON
  GO TOP
ENDIF

REPLACE S_DATE WITH DATE(), ;
        S_TIME WITH TIME(), ;
        T_DATE WITH {},     ;
        T_TIME WITH " ",    ;
        T_NAME WITH t_nm,   ;
        ERRORS WITH 0

i = RECNO()
USE

RETURN i

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                 โฌฅโช  ข ฆใเญ ซฅ ฎ ง ขฅเ่ฅญจจ โฅแโ .                  บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 09.07.97 10:25:11 อผ
PROCEDURE Term_Log
PARAMETERS n_rc, n_err

USE (base_path+"BAL_LOG")
GO n_rc

REPLACE T_DATE WITH DATE(), ;
        T_TIME WITH TIME(), ;
        ERRORS WITH n_err

USE
RETURN
