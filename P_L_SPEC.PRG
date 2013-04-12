*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º ” ©« P_L_Spec      §à ¡®âç¨ª €­¤à¥© ‚ á¨­           13.04.98 17:17:11 º
*ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*º                                                                        º
*º              ¥ç âì ¤®ªã¬¥­â  ãâ¢¥à¦¤¥­¨ï á¯¥æ.¤¢¨¦¥­¨ï.               º
*º                                                                        º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE P_L_Spec
PARAMETERS doc_n, doc_d
*
*  doc_n  - ­®¬¥à ¤®ªã¬¥­â ;
*  doc_d  - «¨¡® ¤ â  ­ ª« ¤­®© «¨¡® £®¤ ¢ á¨¬¢®«ì­®¬ ¨«¨ ç¨á«®¢®¬ ä®à¬ â¥.
*
PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_p, f_p, t_d, f_d, d_year, ffeed, sw
PRIVATE sel_rem, f_name, sb, n_p_det, n_pg, f_pg
PRIVATE s_pg, str_w, nds, doc_tit, p_drctry
PRIVATE k00

sel_rem = SELECT()

IF TYPE("doc_d") = "D"
  d_year = LEFT(DTOS(doc_d),4)
ENDIF
IF TYPE("doc_d") = "N"
  d_year = STR(doc_d,4)
ENDIF
IF TYPE("doc_d") = "C"
  d_year = doc_d
ENDIF

PRIVATE d_d, dep_nam, who_doc

SELECT 0
USE (base_path+"LET_SP_T") ORDER TAG DOC_NUM ALIAS DOC_TTT AGAIN
k00 = d_year+doc_n
IF .NOT. SEEK(k00)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"¥â ¤®ªã¬¥­â  á ­®¬¥à®¬ "+ALLTRIM(doc_n)
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF

d_d = TRANSFORM(DOC_DATE,"@D")

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC000 AGAIN
SEEK DOC_TTT.CUS_CODE
dep_nam = ALLTRIM(CUS_NAME)
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PRS000 AGAIN
SEEK DOC_TTT.WHO
who_doc = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+". "+LEFT(S_NAME,1)+"."
USE

SELECT DOC_TTT

f_name = SYS(3)
DO Wt_Mess WITH "®¤£®â®¢ª  ¤®ªã¬¥­â "
DO Prep_DBF WITH f_name
DO Wt_Mess

DIMENSION sb(5,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Doc_Num}"
sb( 2,2) = ALLTRIM(doc_n)

sb( 3,1) = "{Doc_Date}"
sb( 3,2) = d_d

sb( 4,1) = "{Cus_Name}"
sb( 4,2) = dep_nam

sb( 5,1) = "{Manager}"
sb( 5,2) = who_doc

n = ALEN(sb,1)

SELECT 0
USE DOC_FORM

LOCATE FOR "P_L_SPEC" == ALLTRIM(UPPER(DOC_NAME))
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

SELECT DOC_TTT
USE

SELECT TMP_LST

DO Ini_Prn WITH "“â¢¥à¦¤¥­¨¥ á¯¥æ¤¢¨¦¥­¨ï N "+ALLTRIM(doc_n),  ;
                 p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

n_p_det = p_ln-MAX(ALEN(t_p),ALEN(t_d))-MAX(ALEN(f_p),ALEN(f_d))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = CEILING(RECCOUNT()/n_p_det)
ENDIF

FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
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

  STORE 0 TO sp_d, sp_i, sp_p, sp_n
  FOR i = 1 TO n_p_det
    str_w = PREFIX+" "+NAME+" "+PRODUCER+TRANSFORM(QNT,"@Z 9999999")
    @ PROW()+1,0 SAY str_w
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR

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

DO Term_Prn WITH "", tmpo_path+f_name

DELETE FILE (tmpo_path+f_name+".DBF")
DELETE FILE (tmpo_path+f_name+".CDX")
SELECT (sel_rem)
RETURN

*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º              à®£à ¬¬  ¯®¤£®â®¢ª¨ á®¤¥à¦¨¬®£® á¯¨áª .                 º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Prep_DBF
PARAMETERS f_nam
PRIVATE w_q, w_s

CREATE DBF (tmpo_path+f_nam) (CODE     N( 7), ;
                              PREFIX   C( 4), ;
                              NAME     C(35), ;
                              PRODUCER C( 8), ;
                              QNT      N( 6) )
USE (tmpo_path+f_nam) ALIAS TMP_LST
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST000 AGAIN
SELECT 0
USE (base_path+"LET_SPEC") ORDER TAG DOC_NUM ALIAS S000 AGAIN
SET RELATION TO CODE INTO ST000
= SEEK(k00)
SCAN REST WHILE DOC_NUM = doc_n .AND. d_year = LEFT(DTOS(DOC_DATE),4)
  SELECT TMP_LST
  APPEND BLANK
  REPLACE CODE     WITH ST000.CODE,     ;
          PREFIX   WITH ST000.PREFIX,   ;
          NAME     WITH ST000.NAME,     ;
          PRODUCER WITH ST000.PRODUCER, ;
          QNT      WITH S000.QNT
  SELECT S000
ENDSCAN
USE
SELECT ST000
USE
SELECT TMP_LST

RETURN
