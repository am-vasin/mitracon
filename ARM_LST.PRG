*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ฏฅ็ โจ ญฎฌฅญชซ โใเ๋ € .                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Arm_Lst

PARAMETERS arm_p
*
*  arm_p - ชฎค เ กฎ็ฅฃฎ ฌฅแโ , ฅแซจ ฎโแใโแโขใฅโ, โฎ โฅชใ้จฉ
*

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, j0, stw, t_p, e_p, tmp_f, p_drctry
PRIVATE arm_w, arm_nm, sb, mss, nm_w, pr_w, ffeed

DIMENSION sb(3,2)

IF TYPE("arm_p") = "C"
  arm_w = arm_p
ELSE
  arm_w = arm
ENDIF

sel_rem = SELECT()

SELECT 0

USE ARM_LST ORDER TAG ARM_C AGAIN ALIAS ARM000
IF .NOT. SEEK(arm_w)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ เ กฎ็ฅฃฎ ฌฅแโ  แ ชฎคฎฌ "+ALLTRIM(arm_w)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
arm_nm = ARM_N

sb(1,1) = "{Pg/Pgs}"
sb(1,2) = "????"

sb(2,1) = "{Date    }"
sb(2,2) = TRANSFORM(DATE(),"@D")
sb(2,2) = PADR(sb(2,2),LEN(sb(2,1)))

sb(3,1) = "{Arm Name      }"
sb(3,2) = arm_nm

USE DOC_FORM
LOCATE FOR "ARM_LST" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR


n = MEMLINES(PAGE_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR

n = MEMLINES(PAGE_F)
DIMENSION e_p(n)
FOR i = 1 TO n
  e_p(i) = MLINE(PAGE_F,i)
ENDFOR
USE

tmp_f = tmpo_path+SYS(3)
CREATE DBF (tmp_f)   ;
      (IS_STO C( 3), ;
       PREFIX C( 4), ;
       NAME   C(35), ;
       QNT    N( 6), ;
       QNT_B  N( 6), ;
       QNT_D  N( 6), ;
       LISTS  C(24)  )
USE (tmp_f) ALIAS P_TMP
INDEX ON NAME+PREFIX TAG NAME

SELECT 0
USE STOCK ORDER TAG CODE AGAIN ALIAS S000

SELECT 0
USE LIST_DET ORDER TAG L_CODE AGAIN ALIAS LD000
SET RELATION TO CODE INTO S000

SELECT 0
USE LIST_TIT ORDER TAG LST_ARM AGAIN ALIAS LT000
SET RELATION TO STR(LST_NUM,6) INTO LD000

SEEK arm_w
SCAN REST WHILE LST_ARM = arm_w
  IF .NOT. FOUND("LD000") .OR. .NOT. EMPTY(SAL_NUM)
    LOOP
  ENDIF
  SELECT LD000
  SCAN REST WHILE STR(LT000.LST_NUM,6) = LIST_NUM
    IF CODE < 0
      LOOP
    ENDIF
    IF LD000.QNT = 0 .AND. LD000.QNT_DUE = 0 .AND. LD000.QNT_REQ = 0
      LOOP
    ENDIF
    IF FOUND("S000")
      i = " ๛ "
      nm_w = S000.NAME
      pr_w = S000.PREFIX
    ELSE
      i = "   "
      nm_w = LD000.NAME
      pr_w = LD000.PREFIX
    ENDIF
    SELECT P_TMP
    IF .NOT. SEEK(nm_w+pr_w)
      APPEND BLANK
      j = ""
      j0 = j+ALLTRIM(LD000.LIST_NUM)
    ELSE
      j = ALLTRIM(LISTS)
      j0 = j+", "+ALLTRIM(LD000.LIST_NUM)
    ENDIF
    IF LEN(j0) > FSIZE("LISTS")
      j0 = j+"..."
    ENDIF
    REPLACE IS_STO WITH i, ;
            PREFIX WITH pr_w, ;
            NAME   WITH nm_w, ;
            QNT    WITH QNT+LD000.QNT, ;
            QNT_B  WITH QNT_B+LD000.QNT_REQ, ;
            QNT_D  WITH QNT_D+LD000.QNT_DUE, ;
            LISTS  WITH j0
    SELECT LD000
  ENDSCAN
  SELECT LT000
ENDSCAN
USE
SELECT LD000
USE
SELECT S000
USE
SELECT P_TMP

PRIVATE pg_h
pg_h = RECCOUNT()
IF pg_h = 0
  USE
  DELETE FILE (tmp_f+".dbf")
  DELETE FILE (tmp_f+".cdx")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  เ กฎ็จฌ ฌฅแโฎฌ ญจ็ฅฃฎ ญฅ ็จแซจโแ๏!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (sel_rem)
  RETURN
ENDIF

DO Ini_Prn WITH "ฎฌฅญชซ โใเ  € ", p_ln, lft, n_cp, fnt, ffeed, p_drctry

p_ln = IIF(p_ln = 0, pg_h+10, p_ln-ALEN(t_p)-ALEN(e_p)-1)
pg_h = CEILING(pg_h/p_ln)
PRIVATE i0, i1
GO TOP
FOR i0 = 1 TO pg_h
  sb(1,2) = ALLTRIM(STR(i0))+"/"+ALLTRIM(STR(pg_h))
  FOR i = 1 TO ALEN(t_p)
    stw = t_p(i)
    FOR j = 1 TO 3
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    IF i = 1
      @ PROW(), PCOL() SAY stw
    ELSE
      @ PROW()+1, 0 SAY stw
    ENDIF
  ENDFOR
  FOR i1 = 1 TO p_ln
    stw = IS_STO + " "+ ;
          PREFIX + " "+ ;
          NAME   + " "+ ;
          STR(QNT,6)  + " "+ ;
          STR(QNT_B,6)+ " "+ ;
          STR(QNT_D,6)+ " "+ ;
          LISTS
    @ PROW()+1, 0 SAY stw
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
  FOR i = 1 TO ALEN(e_p)
    stw = e_p(i)
    @ PROW()+1, 0 SAY stw
  ENDFOR
  IF i0 # pg_h
    EJECT
  ENDIF
ENDFOR
USE
DELETE FILE (tmp_f+".dbf")
DELETE FILE (tmp_f+".cdx")

DO Term_Prn WITH ""

SELECT (sel_rem)
RETURN
