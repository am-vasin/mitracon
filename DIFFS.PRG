*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º ” ©« Diffs         §à ¡®âç¨ª €­¤à¥© ‚ á¨­           13.06.98 09:29:28 º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º                      ¥ç âì ¢¥¤®¬®áâ¨ à áå®¦¤¥­¨©.                     º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Diffs

PRIVATE s_sav, vlt, last_d, c_name, mntss

mntss = 2
vlt  = ""
last_d = {}
s_sav = SELECT()

SELECT 0
USE (log_path+"DIFFS") ORDER TAG NAME ALIAS QWERTY AGAIN

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_p, f_p, f_d, d_year, p_drctry
PRIVATE sb, n_p_det, n_pg, f_pg, s_pg, str_w, c_w

DIMENSION sb( 1,2)
sb( 1,1) = "{Page}"
sb( 1,2) = ""

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "DIFFS" == ALLTRIM(UPPER(DOC_NAME))
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

USE

SELECT QWERTY

DO Ini_Prn WITH "‚¥¤®¬®áâì à áå®¦¤¥­¨©", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

n_p_det = p_ln-ALEN(t_p)-ALEN(f_p)-1

f_pg = 1
c_w  = 0

SCAN FOR QNT_O # QNT_S
  IF c_w >= n_p_det
    sb( 1,2) = ALLTRIM(STR(f_pg))
    sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
    c_w = 0
    f_pg = f_pg+1
    EJECT
  ENDIF
  IF c_w = 0
    sb( 1,2) = ALLTRIM(STR(f_pg))
    sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
    FOR i = 1 TO ALEN(t_p)
      str_w = t_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF f_pg = 1 .AND. i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ENDIF
  @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+" "+STR(QNT_O,6)+" "+STR(QNT_S,6)
  c_w = c_w+1
ENDSCAN
USE
sb( 1,2) = ALLTRIM(STR(f_pg))
sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
FOR i = 1 TO ALEN(f_p)
  str_w = f_p(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR
EJECT

DO Term_Prn WITH "", log_path+"DIFFS.DBF"

SELECT (s_sav)

RETURN
