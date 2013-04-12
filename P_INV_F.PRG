*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                  ฅ็ โ์ ญ ชซ คญฎฉ ฏฅเฅค ็จ ข ไจซจ ซ.                   บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 17.06.97 16:33:43 อผ
PROCEDURE P_Inv_F
PARAMETERS inv_n, inv_d
*
*  inv_n - ญฎฌฅเ ญ ชซ คญฎฉ ( ขญใโเฅญญจฉ ไฎเฌ โ );
*  inv_d - ซจกฎ ค โ  ญ ชซ คญฎฉ ซจกฎ ฃฎค ข แจฌขฎซ์ญฎฌ จซจ ็จแซฎขฎฌ ไฎเฌ โฅ;
*

PRIVATE p_ln, fnt, n_cp, lft, n, i, j
PRIVATE t_p, f_p, f_d, d_year, k00, sw, ffeed, p_drctry, a_code
PRIVATE sel_rem, f_name, sb, n_p_det, n_pg, f_pg, s_pg, str_w, nds, nn
PRIVATE c_w
sel_rem = SELECT()

IF TYPE("inv_d") = "D"
  d_year = LEFT(DTOS(inv_d),4)
ENDIF
IF TYPE("inv_d") = "N"
  d_year = STR(inv_d,4)
ENDIF
IF TYPE("inv_d") = "C"
  d_year = inv_d
ENDIF

PRIVATE d_n, d_d, ord_txt, l_face, l_dep
d_n = ALLTRIM(STR(inv_n))

*
*  ‘ฏฅๆจ ซ์ญ๋ฅ ญ คฏจแจ คซ๏ ’
*
PRIVATE l_spec, gtd_spec, cnt_spec, lst_spec
SELECT 0
USE (base_path+"B_GROUPS") ALIAS BG_0317 AGAIN
l_spec = 0
SCAN
  l_spec = l_spec+1
  DIMENSION gtd_spec(l_spec), cnt_spec(l_spec), lst_spec(l_spec)
  gtd_spec(l_spec) = GTD_NUM
  cnt_spec(l_spec) = COUNTRY
  lst_spec(l_spec) = DEV_LIST
ENDSCAN

USE (base_path+"RYN_M_T") ORDER TAG DOC_NUM ALIAS INV_TTT AGAIN
k00 = d_year+STR(inv_n,5)
IF .NOT. SEEK(k00)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ ญ ชซ คญฎฉ แ ญฎฌฅเฎฌ "+d_n
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
d_d = TRANSFORM(DOC_DATE,"@D")
a_code = WHO
IF .NOT. EMPTY(ORD_NUM)
  ord_txt = " ๏ขช  N "+ALLTRIM(ORD_NUM)+" ฎโ "+TRANSFORM(ORD_DATE,"@D")
ELSE
  ord_txt = ""
ENDIF

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC000 AGAIN
SEEK INV_TTT.CUS_CODE
dep_nam = CUS_NAME
c_w = CUS_CODE
USE

SELECT INV_TTT

USE

f_name = SYS(3)
DO Wt_Mess WITH "ฎคฃฎโฎขช  ญ ชซ คญฎฉ"
DO Prep_Inv WITH f_name
DO Wt_Mess

DIMENSION sb(6,2)
sb(1,1) = "{Pg/Pgs}"
sb(1,2) = ""

sb(2,1) = "{Inv_No   }"
sb(2,2) = d_n

sb(3,1) = "{Inv_Dat }"
sb(3,2) = d_d

sb(4,1) = "{Order                            }"
sb(4,2) = ord_txt

sb(5,1) = "{Department                                      }"
sb(5,2) = dep_nam

sb(6,1) = "{Autor                       }"
SELECT 0
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF a_code = 0
  sb(6,2) = "‘€!"
ELSE
  IF SEEK (a_code)
    sb(6,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)    
  ELSE
    sb(6,2) = "?!!"
  ENDIF
ENDIF         
sb(6,2) = PADR(sb(6,2),LEN(sb(6,1)))
USE

n = ALEN(sb,1)
FOR i = 1 TO n
  sb(i,2) =PADR(sb(i,2), LEN(sb(i,1)))
ENDFOR

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "INV_REAL" = ALLTRIM(UPPER(DOC_NAME))
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

USE

SELECT TMP_LST

DO Ini_Prn WITH " ชซ คญ ๏ N "+d_n, p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
nn = 0

n_p_det = FLOOR((p_ln-ALEN(t_p)-ALEN(f_p))/2)
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = CEILING(RECCOUNT()/n_p_det)
ENDIF

FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) =PADR(sb(1,2), LEN(sb(1,1)))
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
  FOR i = 1 TO n_p_det
    nn = nn+1
    @ PROW()+1,0 SAY STR(nn,3)+" "+NAME+" "+STR(QNT,6)

*    IF EMPTY(GTD_NUM)
*      @ PROW()+1,0 SAY "    ชใฏช  คฎ 25.02.2000, ’ ญฅ ใช ง ญ."
*    ELSE
      @ PROW()+1,0 SAY "   ’ N "+GTD_NUM+"/"+GTD_LINE+"    ‘โเ ญ  "+COUNTRY
*    ENDIF
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
*  USE
  FOR i = 1 TO ALEN(f_p)
    str_w = f_p(i)
    FOR j = 1 TO ALEN(sb,1)
      str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
    ENDFOR
    @ PROW()+1, 0 SAY str_w
  ENDFOR
ENDFOR
DELETE FILE (tmpo_path+"TMP_INVF.DBF")
COPY TO (tmpo_path+"TMP_INVF")
USE

DO Term_Prn WITH "",tmpo_path+"TMP_INVF.DBF",c_w

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

PRIVATE tmpPoint, tmpVal

CREATE DBF (tmpo_path+f_nam) (NAME     C(39), ;
                              GTD_NUM  C(26), ;
                              COUNTRY  C(16), ;
                              GTD_LINE C( 4), ;
                              QNT      N( 6)    )
USE (tmpo_path+f_nam) ALIAS TMP_LST
INDEX ON SUBSTR(NAME,5)+LEFT(NAME,4) TAG NAME

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST000 AGAIN
SELECT 0
USE (base_path+"RYN_M") ORDER TAG DOC_NUM ALIAS S000 AGAIN
SET RELATION TO CODE INTO ST000
= SEEK(k00)
SCAN REST WHILE DOC_NUM = inv_n
  SELECT TMP_LST
  APPEND BLANK
  REPLACE NAME     WITH ST000.PREFIX+" "+ST000.NAME+" "+ST000.PRODUCER, ;
          QNT      WITH S000.QNT

  tmpPoint = 1
  tmpVal = " "+ALLTRIM(STR(ST000.PRO_CODE))+" "
  IF l_spec > 1
    FOR j = 2 TO l_spec
      IF tmpVal $ lst_spec(j)
        tmpPoint = j
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  IF EMPTY(S000.GTD_NUM) .OR. tmpPoint # 1
    REPLACE GTD_NUM WITH gtd_spec(tmpPoint), ;
            COUNTRY WITH cnt_spec(tmpPoint)
  ELSE
    REPLACE GTD_NUM  WITH S000.GTD_NUM,   ;
            COUNTRY  WITH S000.COUNTRY,   ;
            GTD_LINE WITH S000.GTD_LINE
  ENDIF
  SELECT S000
ENDSCAN
USE
SELECT ST000
USE
SELECT TMP_LST

RETURN
