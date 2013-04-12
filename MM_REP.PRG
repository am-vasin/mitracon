*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ MM_Rep        งเ กฎโ็จช €ญคเฅฉ  แจญ           13.04.98 17:17:11 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                      โ็ฅโ ฏฎ ช แแฎข๋ฌ ฎฏฅเ ๆจ๏ฌ.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE MM_Rep
PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_p, f_p, t_d, f_d, ffeed, sw
PRIVATE sel_rem, f_name, sb, n_p_det, n_pg, f_pg
PRIVATE s_pg, str_w, nds, doc_tit, p_drctry
PRIVATE d_0, d_1, d000

USE (base_path+"PARMS")
d000 = DAT_START
d_0  = d000
d_1  = DATE()
USE
IF .NOT. Interval()
  RETURN
ENDIF

DO Wt_Mess WITH "ฎคฃฎโฎขช  คฎชใฌฅญโ "
DO Prep_DBF
DO Wt_Mess

DIMENSION sb(7,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Date_0}"
sb( 2,2) = TRANSFORM(d_0, "@D ")

sb( 3,1) = "{Date_1}"
sb( 3,2) = TRANSFORM(d_1, "@D ")

sb( 4,1) = "{Sum_Page+ }"
sb( 4,2) = ""

sb( 5,1) = "{Sum_Doc+  }"
sb( 5,2) = ""

sb( 6,1) = "{Sum_Page- }"
sb( 6,2) = ""

sb( 7,1) = "{Sum_Doc-  }"
sb( 7,2) = ""
n = ALEN(sb,1)

SELECT 0
USE DOC_FORM

LOCATE FOR "MM_REP" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
IF n > 0
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ELSE
  DIMENSION t_p(1)
  t_p(1) = ""
ENDIF

n = MEMLINES(PAGE_F)
IF n > 0
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ELSE
  DIMENSION f_p(1)
  f_p(1) = ""
ENDIF

n = MEMLINES(DOC_H)
IF n > 0
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ELSE
  DIMENSION t_d(1)
  t_d(1) = ""
ENDIF

n = MEMLINES(DOC_F)
IF n > 0
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ELSE
  DIMENSION f_d(1)
  f_d(1) = ""
ENDIF
  
USE

SELECT MM_REP
INDEX ON SYS(15,lwr,NAME) TAG NAME

DO Ini_Prn WITH "โ็ฅโ ฏฎ ช แแฅ",p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

n_p_det = p_ln-MAX(ALEN(t_p),ALEN(t_d))-MAX(ALEN(f_p),ALEN(f_d))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = CEILING(RECCOUNT()/n_p_det)
ENDIF
PRIVATE s_pn, s_dn, s_pp, s_dp
STORE 0 TO s_pn, s_dn, s_pp, s_dp

FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  sb( 4,2) = TRANSFORM(s_pp, "@Z 999999999.99")
  sb( 5,2) = TRANSFORM(s_dp, "@Z 999999999.99")
  IF f_pg # 1
    EJECT
    FOR i = 1 TO ALEN(t_p)
      str_w = t_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(t_d)
      str_w = t_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ENDIF

  s_pp = 0
  s_pn = 0
  FOR i = 1 TO n_p_det
    str_w = NAME+" "+TRANSFORM(SUM_P,"@Z 999999999.99")+" "+TRANSFORM(SUM_N,"@Z 999999999.99")
    @ PROW()+1,0 SAY str_w
    SKIP
    IF EOF()
      EXIT
    ENDIF
    s_pp = s_pp+SUM_P
    s_pn = s_pn+SUM_N
  ENDFOR
  sb( 4,2) = TRANSFORM(s_pp, "@Z 999999999.99")
  sb( 6,2) = TRANSFORM(s_pn, "@Z 999999999.99")
  s_dp = s_dp+s_pp
  s_dn = s_dn+s_pn
  sb( 5,2) = TRANSFORM(s_dp, "@Z 999999999.99")
  sb( 7,2) = TRANSFORM(s_dn, "@Z 999999999.99")

  IF f_pg = n_pg
    FOR i = 1 TO ALEN(f_d)
      str_w = f_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR
USE

DO Term_Prn WITH "", tmpo_path+"MM_REP.DBF"

DELETE FILE (tmpo_path+"MM_REP.DBF")
DELETE FILE (tmpo_path+"MM_REP.CDX")
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎฃเ ฌฌ  ฏฎคฃฎโฎขชจ แฎคฅเฆจฌฎฃฎ แฏจแช .                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prep_DBF
PARAMETERS f_nam
PRIVATE w_q, w_s, n_sav, sn, sp, curs_w

DELETE FILE (tmpo_path+"MM_REP.DBF")
DELETE FILE (tmpo_path+"MM_REP.CDX")

CREATE DBF (tmpo_path+"MM_REP") (CODE     N( 3), ;
                                 NAME     C(16), ;
                                 SUM_P    N(12,2), ;
                                 SUM_N    N(12,2) )
INDEX ON CODE TAG CODE

APPEND FROM (base_path+"MM_TYPES") FIELDS CODE, NAME

SELECT 0
USE (base_path+"CURS_SET") ORDER TAG HRON DESCENDING ALIAS CURS

SELECT 0
USE (base_path+"KASSA_M")
n_sav = SET("NEAR") = "OFF"
SET NEAR ON

SCAN FOR BETWEEN(DOC_DATE, d_0, d_1)
  curs_w = Get_Curs("CURS", "CURS", DOC_DATE)
  SELECT MM_REP
  SEEK KASSA_M.CODE
  sp = KASSA_M.SUM_D+KASSA_M.SUM_R/curs_w
  IF sp > 0
    sn = 0
  ELSE
    sn = -sp
    sp = 0
  ENDIF
  REPLACE SUM_N WITH SUM_N+sn, SUM_P WITH SUM_P+sp
  SELECT KASSA_M
ENDSCAN

USE
SELECT CURS
USE
SELECT MM_REP

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ขขฎค  จญโฅเข ซ  ค โ.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Interval

*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃ ๆจจ
*

PRIVATE stat_type     && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
                                        1 - กซ ญช;
                                        2 - BROWSE - โ กซจๆ ;
                                        3 - BROWSE - แฏจแฎช.
PRIVATE what_do       && ฌ๏ เฅฆจฌ .
PRIVATE menu_name     && ฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎.
PRIVATE last_mouse    && เฅฌ๏ ฏฎแซฅคญฅฃฎ ญ ฆ โจ๏ งขฅเจญฎฉ ชญฎฏชจ.
PRIVATE win_name      && ฌ๏ ฎชญ  ( ฎชฎญ คซ๏ BROWSE ).

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ‘ฎคฅเฆ โฅซ์ญ ๏ ็ แโ์ ฏเฎฃเ ฌฌ๋:
*

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ  ็ ซ์ญ ๏ ค โ  ..   ฎญฅ็ญ ๏ ค โ  .. ณ
*ณ                                                      ณ
*ณ           < OK Ctrl-W > < โช ง โ์แ๏ Esc >           ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 6, 58, "“ช ฆจโฅ จญโฅเข ซ"

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@ 2,  3 SAY " ็ ซ์ญ ๏ ค โ " GET d_0 PICTURE "@D "
@ 2, 31 SAY "ฎญฅ็ญ ๏ ค โ " GET d_1 PICTURE "@D "
@ 4, 13 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE VALID Tst_Dat()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎขฅเช  ชฎเเฅชโญฎแโจ จญโฅเข ซ .                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Tst_Dat

PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF d_1 > DATE()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"€ แฅฃฎคญ๏ ฅ้ฅ โฎซ์ชฎ"+TRANSFORM(DATE(),"@D")+"..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF d_1 < d_0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ็ ซ์ญ ๏ ค โ  ญฅ ฌฎฆฅโ ก๋โ์ กฎซ์่ฅ ชฎญฅ็ญฎฉ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
