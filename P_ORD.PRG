*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ P_Ord         งเ กฎโ็จช €ญคเฅฉ  แจญ           19.05.98 11:03:43 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                       ฅ็ โ์ ง ช ง  ฏฎแโ ข้จชใ.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE P_Ord
PARAMETERS frm00, ord_n, ord_d
*
*  frm00 - ฏเฅคฏเจ๏โจฅ;
*  ord_n - ญฎฌฅเ ง ช ง  ( ขญใโเฅญญจฉ ไฎเฌ โ );
*  ord_d - ซจกฎ ค โ  ง ช ง  ซจกฎ ฃฎค ข แจฌขฎซ์ญฎฌ จซจ ็จแซฎขฎฌ ไฎเฌ โฅ.
*

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, l_num
PRIVATE t_p, f_p, d_year, k00, sw, ffeed, p_drctry
PRIVATE sel_rem, f_name, sb, n_p_det, n_pg, f_pg, s_pg, str_w, nds, nn, c_w
PRIVATE a_code

sel_rem = SELECT()

IF TYPE("ord_d") = "D"
  d_year = LEFT(DTOS(ord_d),4)
ENDIF
IF TYPE("ord_d") = "N"
  d_year = STR(ord_d,4)
ENDIF
IF TYPE("ord_d") = "C"
  d_year = ord_d
ENDIF

PRIVATE d_n, d_d, sup_n, cus_n
d_n = IIF(TYPE("ord_n") = "C", PADL(ALLTRIM(ord_n), 10), STR(ord_n,10))

SELECT 0
USE (base_path+"ORD_TIT") ORDER TAG ORD_NUM ALIAS INV_TTT AGAIN
k00 = frm00+d_year+d_n
IF .NOT. SEEK(k00)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ ง ๏ขชจ แ ญฎฌฅเฎฌ "+ALLTRIM(d_n)
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
d_d = TRANSFORM(ORD_DATE,"@D")
a_code = WHO_ORD

SELECT 0
USE (base_path+"CUSTOMER") ORDER TAG CODE ALIAS CUS000 AGAIN
SEEK INV_TTT.SUP_CODE
sup_n = L_NAME
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS CUS000 AGAIN
SEEK INV_TTT.FIRM
cus_n = L_NAME
USE

f_name = SYS(3)
DO Wt_Mess WITH "ฎคฃฎโฎขช  แฏจแช "
DO Prep_Ord WITH f_name
DO Wt_Mess

DIMENSION sb(7,2)
sb(1,1) = "{Pg/Pgs}"
sb(1,2) = ""

sb(2,1) = "{Ord_No   }"
sb(2,2) = ALLTRIM(d_n)

sb(3,1) = "{Ord_Dat }"
sb(3,2) = d_d

sb(4,1) = "{Sup}"
sb(4,2) = sup_n

sb(5,1) = "{Cus}"
sb(5,2) = cus_n

sb(6,1) = "{Note}"
sb(6,2) = INV_TTT.NOTE

sb(7,1) = "{Autor                       }"
SELECT 0
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF a_code = 0
  sb(7,2) = "‘€!"
ELSE
  IF SEEK (a_code)
    sb(7,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)    
  ELSE
    sb(7,2) = "?!!"
  ENDIF
ENDIF         
sb(7,2) = PADR(sb(7,2),LEN(sb(7,1)))
USE

n = ALEN(sb,1)

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "ORD_SUP" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MAX(MEMLINES(PAGE_H),1)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR

n = MAX(MEMLINES(PAGE_F),1)
DIMENSION f_p(n)
FOR i = 1 TO n
  f_p(i) = MLINE(PAGE_F,i)
ENDFOR

USE

SELECT TMP_LST

DO Ini_Prn WITH " ช ง N "+ALLTRIM(d_n), p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
nn = 0

n_p_det = p_ln-ALEN(t_p)-ALEN(f_p)
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = MAX(CEILING(RECCOUNT()/n_p_det),1)
ENDIF

FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADL(sb(1,2), LEN(sb(1,1)))
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
  IF RECCOUNT() # 0
    FOR i = 1 TO n_p_det
      nn = nn+1
      @ PROW()+1,0 SAY STR(nn,5)+" "+PREFIX+" "+NAME+" "+PRODUCER+" "+STR(QNT,6)
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  FOR i = 1 TO ALEN(f_p)
    str_w = f_p(i)
    FOR j = 1 TO ALEN(sb,1)
      str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
    ENDFOR
    @ PROW()+1, 0 SAY str_w
  ENDFOR
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
PROCEDURE Prep_Ord
PARAMETERS f_nam
PRIVATE c_sav, p_sav

CREATE DBF (tmpo_path+f_nam) (PREFIX   C( 4), ;
                              NAME     C(35), ;
                              PRODUCER C( 8), ;
                              QNT      N( 6) )
USE (tmpo_path+f_nam) ALIAS TMP_LST
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST000 AGAIN
SELECT 0
USE (base_path+"ORDER") ORDER TAG ORD_NUM ALIAS S000 AGAIN
SET RELATION TO CODE INTO ST000
= SEEK(k00)
SCAN REST WHILE ORD_NUM = d_n .AND. FIRM = frm00
  SELECT TMP_LST
  APPEND BLANK
  IF FOUND("ST000")
    REPLACE PREFIX   WITH ST000.PREFIX,  ;
            NAME     WITH ST000.NAME,    ;
            PRODUCER WITH ST000.PRODUCER,;
            QNT      WITH S000.QNT
  ELSE
    REPLACE PREFIX   WITH S000.PREFIX,  ;
            NAME     WITH S000.NAME,    ;
            PRODUCER WITH S000.PRODUCER,;
            QNT      WITH S000.QNT
  ENDIF
  SELECT S000
ENDSCAN
USE
SELECT ST000
USE
SELECT TMP_LST

RETURN
