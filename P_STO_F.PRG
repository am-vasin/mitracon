*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ P_Sto_F       งเ กฎโ็จช €ญคเฅฉ  แจญ           04.02.98 09:29:28 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                  ฅ็ โ์ ขฅคฎฌฎแโจ ญ ซจ็จ๏ ข ไจซจ ซฅ.                   บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE P_Sto_F
PARAMETER c_code

PRIVATE s_sav, vlt, last_d, c_name, mntss

mntss = 2
vlt  = ""
last_d = {}
s_sav = SELECT()

DO Wt_Mess WITH "ฎคฃฎโฎขช  ขฅคฎฌฎแโจ ญ ซจ็จ๏..."
IF .NOT. Prep_Tmp1()
  DO Wt_Mess
  SELECT (s_sav)
  RETURN
ENDIF
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS QWERTY AGAIN
SEEK c_code
c_name = ALLTRIM(CUS_NAME)
USE
SELECT P_TMP_F

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_p, f_p, f_d, d_year, p_drctry
PRIVATE sb, n_p_det, n_pg, f_pg, s_pg, str_w, c_w

PRIVATE sum_d, sum_p

DIMENSION sb( 6,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Doc_D}"
sb( 2,2) = DTOC(DATE())

sb( 3,1) = "{Price_D}"
sb( 3,2) = DTOC(last_d)

sb( 4,1) = "{Customer}"
sb( 4,2) = c_name

sb( 5,1) = "{Pg_Sum       }"
sb( 5,2) = ""

sb( 6,1) = "{Doc_Sum      }"
sb( 6,2) = ""

SELECT 0
USE DOC_FORM
LOCATE FOR "P_STO_F" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
IF n = 0
  n = 1
  DIMENSION t_p(n)
  t_p(n) = ""
ELSE
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)
IF n = 0
  n = 1
  DIMENSION f_p(n)
  f_p(n) = ""
ELSE
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_F)
IF n = 0
  n = 1
  DIMENSION f_d(n)
  f_d(n) = ""
ELSE
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ENDIF

USE

DO Wt_Mess

SELECT P_TMP_F

DO Ini_Prn WITH "ฅคฎฌฎแโ์ ญ ซจ็จ๏ ข ไจซจ ซฅ", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

n_p_det = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = MAX(CEILING(RECCOUNT()/n_p_det),1)
ENDIF

STORE 0 TO sum_d, sum_p

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
  IF RECCOUNT() # 0
    FOR i = 1 TO n_p_det
      @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+" "+STR(QNT,6)+" "+STR(PRICE,10,mntss)+" "+STR(QNT*PRICE,15,mntss)
      sum_p = sum_p+QNT*PRICE
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  sb( 5,2) = STR(sum_p,15,mntss)
  sum_d = sum_d+sum_p
  sb( 6,2) = STR(sum_d,15,mntss)
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

DO Term_Prn WITH "", tmpo_path+"P_TMP_F.DBF"

DELETE FILE (tmpo_path+"P_TMP_F.DBF")

SELECT (s_sav)

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ฎคฃฎโฎขช  ขเฅฌฅญญฎฃฎ ไ ฉซ .                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 04.02.98 09:32:21 อผ
PROCEDURE Prep_Tmp1
PRIVATE mss
SELECT 0
USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE ALIAS RLT000 AGAIN
IF .NOT. SEEK(c_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ชซจฅญโฎฌ ญจ็ฅฃฎ ญฅ ็จแซจโแ๏!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN .F.
ENDIF

vlt = VALUTA
last_d = DATE_P

USE

DELETE FILE (tmpo_path+"P_TMP_F0.DBF")
DELETE FILE (tmpo_path+"P_TMP_F0.CDX")

DELETE FILE (tmpo_path+"P_TMP_F.DBF")
DELETE FILE (tmpo_path+"P_TMP_F.CDX")

CREATE DBF (tmpo_path+"P_TMP_F0") ;
  ( PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT      N( 6), ;
    PRICE    N(10,IIF(vlt = "$", 4, 2)) )
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

IF vlt = "$"
  mntss = 3
ENDIF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_PSF AGAIN
SELECT 0
USE (base_path+"RYN_L") ORDER TAG CUS_CODE ALIAS RL000 AGAIN
SET RELATION TO CODE INTO STO_PSF
SEEK c_code
SCAN REST WHILE CUS_CODE = c_code
  IF QNT = 0
    DELETE
    LOOP
  ENDIF
  SELECT P_TMP_F0
  APPEND BLANK
  REPLACE PREFIX	WITH	STO_PSF.PREFIX,   ;
          NAME		WITH	STO_PSF.NAME,     ;
          PRODUCER	WITH	STO_PSF.PRODUCER, ;
          QNT		WITH	RL000.QNT,        ;
          PRICE		WITH	IIF(vlt = "$", RL000.PRICE_D,RL000.PRICE_R)
  SELECT RL000
ENDSCAN

USE
SELECT STO_PSF
USE
SELECT P_TMP_F0
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ชซจฅญโฎฌ ญจ็ฅฃฎ ญฅ ็จแซจโแ๏!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"P_TMP_F0.DBF")
  DELETE FILE (tmpo_path+"P_TMP_F0.CDX")
  RETURN .F.
ENDIF

COPY TO (tmpo_path+"P_TMP_F")
USE (tmpo_path+"P_TMP_F")
DELETE FILE (tmpo_path+"P_TMP_F0.DBF")
DELETE FILE (tmpo_path+"P_TMP_F0.CDX")

RETURN .T.
