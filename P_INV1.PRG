*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ฏฅ็ โจ ญ ชซ คญฎฉ.                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE P_Inv1
PARAMETERS l_num
*
*  l_num - ญฎฌฅเ แฏจแช 
*

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_p
PRIVATE f_p, f_d, d_year, k00, sw, ffeed, p_drctry
PRIVATE sel_rem, f_name, sb, n_p_det, n_pg, f_pg, s_pg, str_w
PRIVATE nsp_prc, nds, nds_prc, nn, c_w, sum_p_n, sum_tot
sel_rem = SELECT()

PRIVATE sum_w, sum_w0, sum_p, sum_p0, d_n, d_d, ord_txt, l_face, l_dep
d_n = "/"
nsp_prc = 0

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS INV_TTT AGAIN
k00 = l_num
IF .NOT. SEEK(k00)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ แฏจแช  แ ญฎฌฅเฎฌ "+ALLTRIM(STR(l_num))+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
d_d = TRANSFORM(DATE(),"@D")

ord_txt = ""

l_dep  = CLI_CODE = 0  && ฎฆญฎ ซจ ชฎเเฅชโจเฎข โ์ ฏเฅคฏเจ๏โจฅ
IF EMPTY(CLI_CODE)
  dep_nam = LST_NAME
ELSE
  dep_nam = CLI_CODE
ENDIF
face_nam = SPACE(24)
l_face = .T.       && ฎฆญฎ ซจ ชฎเเฅชโจเฎข โ์ ไ ฌจซจ๎

IF .NOT. EMPTY(CLI_CODE)
  SELECT 0
  USE ACCOUNT ORDER TAG CUS_CODE ALIAS ACC000 AGAIN
  SEEK dep_nam
  dep_nam = ALLTRIM(CUS_NAME)
  USE
ENDIF

SELECT 0
USE (base_path+"PARMS")
nds_prc = NDS_
USE

SELECT INV_TTT
IF CLI_CODE >= 0
  DO C_Names
ENDIF

c_w = CLI_CODE
sum_w = 0
sum_w0 = 0
sum_p_n = 0
f_name = SYS(3)
DO Wt_Mess WITH "ฎคฃฎโฎขช  แฏจแช "
DO Prep_Inv WITH f_name
sum_p = Capital(Num2RStr(FLOOR(ABS(sum_w))))+" เใก. "+ ;
        STR((ABS(sum_w)-FLOOR(ABS(sum_w)))*100,2)+" ชฎฏ."
sum_p0 = Capital(Num2RStr(FLOOR(ABS(sum_w0))))+" เใก. "+ ;
         STR((ABS(sum_w0)-FLOOR(ABS(sum_w0)))*100,2)+" ชฎฏ."
sum_tot = STR(ABS(sum_w+sum_w*nsp_prc/100),10,2) 
sum_tot = VAL(sum_tot)
sum_p_n = Capital(Num2RStr(FLOOR(ABS(sum_tot))))+ " เใก. "+ ;
          STR((ABS(sum_tot)-FLOOR(ABS(sum_tot)))*100,2)+" ชฎฏ."
nds = STR(ABS(sum_w)*nds_prc/100,10,2)
nsp = STR(ABS(sum_w)*nsp_prc/100,10,2)

DO Wt_Mess

DIMENSION sb(21,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Pg_Sum  }"
sb( 2,2) = ""

sb( 3,1) = "{Inv_No   }"
sb( 3,2) = d_n

sb( 4,1) = "{Inv_Dat }"
sb( 4,2) = d_d

sb( 5,1) = "{Order                            }"
sb( 5,2) = ord_txt

sb( 6,1) = "{Department                                      }"
sb( 6,2) = dep_nam

sb( 7,1) = "{Face                                            }"
sb( 7,2) = face_nam

sb( 8,1) = "{Tot_Sum }"
sb( 8,2) = STR(sum_w,10,2)

sb( 9,1) = "{Sum_Str1                                                   }"
str_w = ""
i = ""
DO Break_Str WITH sum_p, i, str_w, LEN(sb(9,1))
sb( 9,2)   = i
sum_p = str_w

sb(10,1) = "{Sum_Str2                                                   }"
i = ""
DO Break_Str WITH sum_p, i, str_w, LEN(sb(10,1))
sb(10,2)   = i
sum_p = str_w

sb(11,1) = "{NDS     }"
sb(11,2) = nds

sb(12,1) = "{List}"
sb(12,2) = STR(l_num)

sb(14,1) = "{Tot_Sum0}"   && ‘ใฌฌ  กฅง ‘
sb(14,2) = STR(sum_w0,10,2) 

sb(15,1) = "{Pg_Sum0 }"   && ‘ใฌฌ  กฅง ‘ ญ  แโเ ญจๆฅ
sb(15,2) = ""

sb(16,1) = "{SumN    }"   && ‘
sb(16,2) = nsp

sb(17,1) = "{%NDS}"
sb(17,2) = STR(nds_prc)

sb(18,1) = "{%NSP}"
sb(18,2) = STR(nsp_prc,6,2)

sb(19,1) = "{Tot_SumN}"   && ‘ใฌฌ  แ ‘
sb(19,2) = STR(sum_tot,12,2)

sb(20,1) = "{Sum_Str1_N                                                 }"
str_w = ""
i = ""
DO Break_Str WITH sum_p_n, i, str_w, LEN(sb(20,1))
sb(20,2)   = i
sum_p_n = str_w

sb(21,1) = "{Sum_Str2_N                                                 }"
i = ""
DO Break_Str WITH sum_p_n, i, str_w, LEN(sb(21,1))
sb(21,2)   = i

sb(13,1) = "{Autor                       }"
SELECT 0
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF user = 0
  sb(13,2) = "‘€!"
ELSE
  IF SEEK (user)
    sb(13,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)    
  ELSE
    sb(13,2) = "?!!"
  ENDIF
ENDIF         
sb(13,2) = PADR(sb(13,2),LEN(sb(13,1)))
USE

n = ALEN(sb,1)
*FOR i = 1 TO n
*  sb(i,2) = PADR(sb(i,2), LEN(sb(i,1)))
*ENDFOR
SELECT 0
USE DOC_FORM
LOCATE FOR IIF(c_w < 0, "CORR_EXP", "INVOICE9") == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR
n = MEMLINES(PAGE_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR

n = MEMLINES(PAGE_F)
DIMENSION f_p(n)
FOR i = 1 TO n
  f_p(i) = MLINE(PAGE_F,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR
USE

SELECT TMP_LST

DO Ini_Prn WITH " ชซ คญ ๏ N "+d_n, p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
nn = 0

n_p_det = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = MAX(CEILING(RECCOUNT()/n_p_det),1)
ENDIF

FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1
    EJECT
  ENDIF
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
  sum_p = 0
  sum_p0 = 0
  IF RECCOUNT() # 0
    FOR i = 1 TO n_p_det
      nn = nn+1
      @ PROW()+1,0 SAY STR(nn,3)+" "+PREFIX+" "+NAME+" "+PRODUCER+" "+STR(QNT,6)+" "+STR(PRC_0,11,2)+" "+STR(SUM_0,12,2)+" "+STR(SUM_T,12,2)
      sum_p = sum_p+TMP_LST.SUM_T
      sum_p0 = sum_p0+TMP_LST.SUM_0
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
*  USE
  str_w = STR(sum_p,12,2)
  sb( 2,2) = str_w
  str_w = STR(sum_p0,12,2)
  sb(15,2) = str_w
*  sb( 2,2) = PADR(sb(2,2), LEN(sb(2,1)))
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
SELECT INV_TTT
USE

DO Term_Prn WITH "", tmpo_path+f_name+".DBF"

DELETE FILE (tmpo_path+f_name+".DBF")
DELETE FILE (tmpo_path+f_name+".CDX")
SELECT (sel_rem)
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎฃเ ฌฌ  ฏฎคฃฎโฎขชจ แฎคฅเฆจฌฎฃฎ แฏจแช .                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prep_Inv
PARAMETERS f_nam
PRIVATE c_sav, p_sav
CREATE DBF (tmpo_path+f_nam) (PREFIX   C( 4), ;
                              NAME     C(35), ;
                              PRODUCER C( 8), ;
                              QNT      N( 6), ;
                              PRC_0    N( 9,4), ;
                              PRC      N( 9,4), ;
                              SUM_0    N(12,4), ;
                              SUM_T    N(12,4))
USE (tmpo_path+f_nam) ALIAS TMP_LST
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT 0
USE STOCK ORDER TAG CODE ALIAS ST000 AGAIN
SELECT 0
USE LIST_DET ORDER TAG LIST ALIAS S000 AGAIN
SET RELATION TO CODE INTO ST000
k00 = STR(k00,6)
= SEEK(k00)
c_sav = 0
p_sav = 0
SCAN REST WHILE LIST_NUM = k00
  sum_w = sum_w+IIF(INV_TTT.IS_USD, S000.USD_PRICE*kurs,;
                                    S000.SAL_PRICE)*S000.QNT
  sum_w0 = sum_w0+IIF(INV_TTT.IS_USD,;
                  S000.USD_PRICE*kurs - S000.USD_PRICE*kurs*nds_prc/100,;
                  S000.SAL_PRICE-S000.SAL_PRICE*nds_prc/100)*S000.QNT
  SELECT TMP_LST
  IF c_sav = S000.CODE .AND. p_sav = IIF(INV_TTT.IS_USD,S000.USD_PRICE,S000.SAL_PRICE)
    REPLACE QNT   WITH QNT+S000.QNT,  ;
            SUM_0 WITH SUM_0+IIF(INV_TTT.IS_USD,;
                  S000.USD_PRICE*kurs-S000.USD_PRICE*kurs*nds_prc/100,;
                  S000.SAL_PRICE-S000.SAL_PRICE*nds_prc/100)*S000.QNT,;
            SUM_T WITH SUM_T+IIF(INV_TTT.IS_USD,;
                       S000.USD_PRICE,S000.SAL_PRICE)*S000.QNT
  ELSE
    APPEND BLANK
    REPLACE PREFIX   WITH ST000.PREFIX,  ;
            NAME     WITH ST000.NAME,    ;
            PRODUCER WITH ST000.PRODUCER,;
            QNT      WITH S000.QNT,      ;
            PRC      WITH IIF(INV_TTT.IS_USD,S000.USD_PRICE*kurs,S000.SAL_PRICE), ;
            PRC_0    WITH IIF(INV_TTT.IS_USD,;
                          S000.USD_PRICE*kurs-S000.USD_PRICE*kurs*nds_prc/100,;
                          S000.SAL_PRICE-S000.SAL_PRICE*nds_prc/100), ;
            SUM_0    WITH IIF(INV_TTT.IS_USD,;
                          S000.USD_PRICE*kurs-S000.USD_PRICE*kurs*nds_prc/100,;
                          S000.SAL_PRICE-S000.SAL_PRICE*nds_prc/100)*S000.QNT,;
            SUM_T    WITH IIF(INV_TTT.IS_USD, S000.USD_PRICE*kurs,S000.SAL_PRICE)*S000.QNT
  ENDIF
  c_sav = S000.CODE
  p_sav = IIF(INV_TTT.IS_USD,S000.USD_PRICE,S000.SAL_PRICE)
  SELECT S000
ENDSCAN
USE
SELECT ST000
USE
SELECT TMP_LST

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ           เฎฃเ ฌฌ  ขขฎค  จฌฅญจ ชซจฅญโ  จ ฏเฅคฏเจ๏โจ๏.                บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE C_Names

PRIVATE ln, wd, ex, mss, c1

IF .NOT. l_face
  RETURN
ENDIF

ln = 10
wd = 68
ex = 1

*
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ  ซฎฃ แ ฏเฎค ฆ 999.99 %                                        ณ
*ณ                                                                ณ
*ณ เฃ ญจง ๆจ๏ ....:....!....:....!....:....!....:....!....:....! ณ
*ณ                                                                ณ
*ณ               —ฅเฅง ชฎฃฎ ....:....!....:....!..                ณ
*ณ                                                                ณ
*ณ               < OK Ctrl-W > < โช ง โ์แ๏ Esc >                 ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

DO D_W WITH "W_000", ln, wd, " ฏฎซญจโฅ!", 1
@ 2,  3 SAY " ซฎฃ แ ฏเฎค ฆ" GET nsp_prc PICTURE "999.99"
@ 4,  3 SAY "เฃ ญจง ๆจ๏" GET dep_nam WHEN l_dep
@ 6, 17 SAY "—ฅเฅง ชฎฃฎ" GET face_nam
@ 8, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
READ CYCLE VALID Empt_N()

RELEASE WINDOW W_000
RETURN

**********************************************************************
PROCEDURE Empt_N

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF EMPTY(face_nam)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ใแโฎฅ จฌ๏, ํโฎ ฏเ ขจซ์ญฎ? [   ] [ ฅโ ]"
  mss(3) = ""
  IF Out_Mess(7,"mss") = 2
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.