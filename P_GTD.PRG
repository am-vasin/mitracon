*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  P_GTD         งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                              ฅ็ โ์ ’.                               บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 04.04.2000 ออผ
PROCEDURE P_GTD
PARAMETERS GTD_n
*
*  GTD_n - ญฎฌฅเ ’
*

PRIVATE p_ln, fnt, n_cp, lft, n, i, j
PRIVATE t_p, f_p, f_d, f_d, d_year, k00, sw, ffeed, p_drctry, a_code
PRIVATE sel_rem, f_name, sb, n_p_det, n_pg, f_pg, s_pg, str_w, nds, nn

sel_rem = SELECT()

SELECT 0
USE (path_comm+"GTD_TIT") ORDER TAG GTD_NUM ALIAS GTD_TTT AGAIN
IF .NOT. SEEK(GTD_n)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ ’ แ ญฎฌฅเฎฌ "+GTD_n
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
d_d = TRANSFORM(GTD_DATE,"@D")
k00 = GTD_NUM
sw  = IIF(EMPTY(GTD_TYPE), "[ ]", "[X]")
USE 

f_name = SYS(3)
DO Wt_Mess WITH "ฎคฃฎโฎขช  ’"
DO Prep_Tmp WITH f_name
DO Wt_Mess

DIMENSION sb(4,2)
sb(1,1) = "{Pg/Pgs}"
sb(1,2) = ""

sb(2,1) = "{GTD_No                  }"
sb(2,2) = k00

sb(3,1) = "{GTD_Dat }"
sb(3,2) = d_d

sb(4,1) = "{ }"
sb(4,2) = sw

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "GTD" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
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
    f_d(i) = MLINE(PAGE_F,i)
  ENDFOR
ENDIF

USE

SELECT TMP_0404

DO Ini_Prn WITH "’", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
nn = 0

n_p_det = FLOOR(p_ln-MAX(ALEN(t_p),ALEN(t_d))-MAX(ALEN(f_p),ALEN(f_d)))
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
  FOR i = 1 TO n_p_det
    nn = nn+1
    @ PROW()+1,0 SAY STR(nn,3)+" "+NAME+" "+COUNTRY+" "+GTD_LINE+STR(QNT,7)
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
  IF f_pg # n_pg
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_d)
      str_w = f_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR
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
PROCEDURE Prep_Tmp
PARAMETERS f_nam

SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_NUM ALIAS GTD_0404 AGAIN
SEEK k00
COPY REST WHILE GTD_NUM = k00 TO (tmpo_path+f_nam)
USE (tmpo_path+f_nam) ALIAS TMP_0404

RETURN
