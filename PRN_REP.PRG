*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ         เฎฃเ ฌฌ  ฏฅ็ โจ ฎโ็ฅโฎข ฏฅเฅค ฏฎแโ ข้จชฎฌ ฏฎ ฏฎงจๆจจ.        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prn_Rep
PARAMETER c_code

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_p, f_p, t_d, f_d, d_year, p_drctry
PRIVATE sel_rem, sb, det_str, str_w, ffeed, w01, v_tmp, dt

dt = CTOD("01.01.2005")
dt = Get_Date(dt)

sel_rem = SELECT()

SELECT 0
DELETE FILE (tmpo_path+"prn_rep.dbf")
DELETE FILE (tmpo_path+"prn_rep1.cdx")
DELETE FILE (tmpo_path+"prn_rep1.dbf")
CREATE DBF (tmpo_path+"prn_rep1.dbf") ;
	(	N_REP		N( 5), ;
		DT_REP		D,     ;
		CODE		N( 7), ;
		PREFIX		C( 4), ;
		NAME		C(35), ;
		PRODUCER	C( 8), ;
		QNT			N( 8), ;
		PRICE_R		N( 8, 2),;
		PRICE_D		N(10, 4) )

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_6402 AGAIN
SEEK c_code
SELECT 0
USE (base_path+"REPORT") ORDER TAG CODE ALIAS RP_6402 AGAIN
SEEK STR(c_code)
SCAN REST WHILE CODE = c_code FOR D_REP >= dt
  SELECT PRN_REP1
  APPEND BLANK
  REPLACE	N_REP		WITH RP_6402.N_REP,		;
			DT_REP		WITH RP_6402.D_REP,		;
			CODE		WITH RP_6402.CODE,		;
			PREFIX		WITH ST_6402.PREFIX,	;
			NAME		WITH ST_6402.NAME,		;
			PRODUCER	WITH ST_6402.PRODUCER,	;
			QNT			WITH RP_6402.QNT,		;
			PRICE_R		WITH RP_6402.PRICE_R,	;
			PRICE_D		WITH RP_6402.PRICE_D
  SELECT RP_6402
ENDSCAN
USE IN RP_6402
USE IN ST_6402
IF RECCOUNT("PRN_REP1") = 0
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"  ใช ง ญญ๋ฉ ฏฅเจฎค ฎโ็ฅโฎข ฏฎ ฏฎงจๆจจ ญฅ ก๋ซฎ."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN PRN_REP1
  DELETE FILE (tmpo_path+"prn_rep1.dbf")
  SELECT (sel_rem)
  RETURN
ENDIF
SELECT PRN_REP1
INDEX ON DTOS(DT_REP)+STR(N_REP) TAG TMP DESCENDING
COPY TO (tmpo_path+"prn_rep.dbf")
USE
DELETE FILE (tmpo_path+"prn_rep1.cdx")
DELETE FILE (tmpo_path+"prn_rep1.dbf")
USE (tmpo_path+"prn_rep")

*{S_Date  } ฅเจฎค
*{Pr}       เฅไจชแ
*{Name                   }  จฌฅญฎข ญจฅ
*{Prod  } ”จเฌ 

DIMENSION sb(4,2)
sb( 1,1) = "{S_Date  }"
sb( 1,2) = DTOS(dt)
sb( 2,1) = "{Pr}"
sb( 2,2) = PREFIX
sb( 3,1) = "{Name                   }"
sb( 3,2) = NAME
sb( 4,1) = "{Prod  }"
sb( 4,2) = PRODUCER

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "SUP_REP" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
IF n = 0
  DIMENSION t_p(1)
  t_p(1) = ""
ELSE
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)
IF n = 0
  DIMENSION f_p(1)
  f_p(1) = ""
ELSE
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_H)
IF n = 0
  DIMENSION t_d(1)
  t_d(1) = ""
ELSE
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_F)
IF n = 0
  DIMENSION f_d(1)
  f_d(1) = ""
ELSE
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ENDIF
USE

SELECT PRN_REP
GO TOP
PRIVATE n_p_det, cur_p, hol_p, j
n_p_det = p_ln-MAX(ALEN(t_p), ALEN(t_d))-MAX(ALEN(f_d), ALEN(f_p))-1
hol_p = CEILING(RECCOUNT()/n_p_det)

DO Ini_Prn WITH "โ็ฅโ ฏฎ ฏฎงจๆจจ", p_ln, lft, n_cp, fnt, ffeed, p_drctry

FOR cur_p = 1 TO hol_p
  IF cur_p = 1
    @ PROW(), PCOL() SAY Subst_H(t_d(1))
    FOR i = 2 TO ALEN(t_d)
      @ PROW()+1, 0 SAY Subst_H(t_d(i))
    ENDFOR
  ELSE
    @ PROW(), PCOL() SAY Subst_H(t_p(1))
    FOR i = 2 TO ALEN(t_p)
      @ PROW()+1, 0 SAY Subst_H(t_p(i))
    ENDFOR
  ENDIF
  FOR i = 1 TO n_p_det
    @ PROW()+1,0 SAY DTOC(DT_REP)+STR(N_REP)+STR(QNT, 9)+STR(PRICE_D, 15, 4)
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
  FOR i = 1 TO ALEN(f_p)
    @ PROW()+1, 0 SAY Subst_H(f_p(i))
  ENDFOR
  IF cur_p # hol_p
    EJECT
  ENDIF
ENDFOR

FOR i = 1 TO ALEN(f_d)
  @ PROW()+1, 0 SAY Subst_H(f_d(i))
ENDFOR

USE IN PRN_REP
DO Term_Prn WITH "", tmpo_path+"PRN_REP.DBF"
DELETE FILE (tmpo_path+"prn_rep.dbf")

SELECT (sel_rem)
RETURN

******************************************************************
PROCEDURE Subst_H
PARAMETER s_line

PRIVATE r_line, i

r_line = s_line
FOR i = 1 TO ALEN(sb, 1)
  r_line = STRTRAN(r_line, sb(i, 1), sb(i, 2))
ENDFOR

RETURN r_line
