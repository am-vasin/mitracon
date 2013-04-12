*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Per1_Sal      งเ กฎโ็จช €ญคเฅฉ  แจญ           15.01.98 15:53:12 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                  ‘ฏเ ขช  ฏฎ ง เฏซ โฅ คซ๏ แฎโเใคญจช .                   บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Per1_Sal
PARAMETER p_code, d0, d1

PRIVATE s_sav
s_sav = SELECT()

SET DATE GERMAN

DO Prep_Tmp

SELECT SLR_TMP
GO TOP
IF .NOT. BOF() .AND. .NOT. EOF()
  DO Prn_Doc
ELSE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ใช ง ญญ๋ฉ ฏฅเจฎค ญฅ ก๋ซฎ ญจ ข๋ฏซ โ ญจ ญ ็จแซฅญจฉ."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF  

SELECT SLR_TMP
USE

DELETE FILE (tmpo_path+"SLR_TMP.DBF")
DELETE FILE (tmpo_path+"SLR_TMP.DBF")
SELECT (s_sav)

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ฎคฃฎโฎขช  ขเฅฌฅญญฎฃฎ ไ ฉซ .                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 15.01.98 16:23:20 อผ
PROCEDURE Prep_Tmp
PRIVATE rst

SELECT 0
USE (base_path+"SLR_TYPE") ORDER TAG CODE ALIAS ST_9928 AGAIN
SELECT 0
USE (base_path+"SLR_MOVE") ORDER TAG PS_CODE ALIAS SM_9928 AGAIN
SET RELATION TO CODE INTO ST_9928
SELECT 0

DELETE FILE (tmpo_path+"SLR_TMP.DBF")
DELETE FILE (tmpo_path+"SLR_TMP.DBF")
CREATE DBF (tmpo_path+"SLR_TMP.DBF") ;
  ( PS_CODE N( 5), ;
    D_DATE  D,     ;
    D_TIME  C( 8), ;
    D_NUM   C( 7), ;
    OPER_N  C(24), ;
    QNT     N(8,2),;
    UNIT    C(12), ;
    PRICE   N(8,2),;
    TOTAL   N(8,2),;
    R_PART  N(8,2),;
    COURCE  N(8,3),;
    REST    N(8,2)  )

INDEX ON STR(PS_CODE,5)+DTOS(D_DATE)+D_TIME TAG ORD

SELECT SM_9928
SEEK p_code
SCAN REST WHILE p_code = PS_CODE
  SELECT SLR_TMP
  APPEND BLANK
  REPLACE PS_CODE WITH SM_9928.PS_CODE,  ;
          D_DATE  WITH SM_9928.DOC_DATE, ;
          D_TIME  WITH SM_9928.DOC_TIME, ;
          D_NUM   WITH SM_9928.DOC_NUM,  ;
          OPER_N  WITH ST_9928.NAME,     ;
          QNT     WITH SM_9928.QNT,      ;
          UNIT    WITH SM_9928.UNIT,     ;
          PRICE   WITH Crpt_2(SM_9928.PRICE), ;
          R_PART  WITH Crpt_2(SM_9928.R_PART),;
          COURCE  WITH Crpt_2(SM_9928.COURCE),;
          TOTAL   WITH Crpt_2(SM_9928.TOTAL)
  SELECT SM_9928
ENDSCAN

USE
SELECT ST_9928

USE (base_path+"PS_STAT") ALIAS PS_9928 AGAIN

SCAN FOR SEEK(STR(CODE,5), "SLR_TMP")
  rst = Crpt_2(PS_9928.START)
  SELECT SLR_TMP
  SCAN REST WHILE PS_CODE = PS_9928.CODE
    rst = rst+TOTAL
    IF BETWEEN(D_DATE, d0, d1)
      REPLACE REST WITH rst
    ELSE
      DELETE
    ENDIF
  ENDSCAN
  SELECT PS_9928
ENDSCAN

USE

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ฅ็ โ์ คฎชใฌฅญโ .                            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 15.01.98 17:39:40 อผ
PROCEDURE Prn_Doc
PRIVATE p_ln, sb, fnt, n_cp, lft, ffeed, n, i, l_flt, str_w, t_p, p_drctry
PRIVATE distr, cnt

DIMENSION sb(3,2)

sb( 1,1) = "{Client  }"
sb( 1,2) = ""

sb( 2,1) = "{S_Date}"
sb( 2,2) = DTOC(d0)

sb( 3,1) = "{E_Date}"
sb( 3,2) = DTOC(d1)

SELECT 0
USE DOC_FORM
LOCATE FOR "SLR_REP" = ALLTRIM(UPPER(DOC_NAME))
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
USE

USE (base_path+"PERSONS") ORDER TAG DEP_NAME ALIAS PS_9928 AGAIN
SET RELATION TO STR(CODE,5) INTO SLR_TMP

DO Ini_Prn WITH " เฏซ โ ", p_ln, lft, n_cp, fnt, ffeed, p_drctry
l_flt = 0
cnt = 0
SCAN FOR FOUND("SLR_TMP")
  sb( 1,2) = ALLTRIM(FAMILY)+" "+ALLTRIM(NAME)+" "+ALLTRIM(S_NAME)
  FOR i = 1 TO ALEN(t_p)
    str_w = t_p(i)
    FOR j = 1 TO ALEN(sb,1)
      str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
    ENDFOR
    IF l_flt = 0
      @ PROW(), PCOL() SAY str_w
    ELSE
      @ PROW()+1, 0 SAY str_w
    ENDIF
*    IF p_ln # 0
*      IF l_flt % p_ln = 0 .AND. l_flt # 0
*        EJECT
*      ENDIF
*    ENDIF
    l_flt = l_flt+1
  ENDFOR
  SELECT SLR_TMP
  SCAN REST WHILE PS_CODE = PS_9928.CODE
    IF EMPTY(R_PART)
      distr = ""
    ELSE
      distr = " ("+ALLTRIM(STR(ABS(TOTAL)-R_PART,10,2))+","+ ;
                   ALLTRIM(STR(R_PART,10,2))+"*"+ ;
                   ALLTRIM(STR(COURCE,8,3))+"="+  ;
                   ALLTRIM(STR(ROUND(R_PART*COURCE,2),8,3))+")"
    ENDIF
    str_w = DTOC(D_DATE)+" "+D_NUM+" "+OPER_N+" "+  ;
            TRANSFORM(QNT,"@Z 999.99")+" "+UNIT+" "+   ;
            TRANSFORM(PRICE,"@Z 99999.99")+" "+     ;
            TRANSFORM(TOTAL,"@Z 99999.99")+" "+     ;
            TRANSFORM(REST,"99999.99")+distr
*    IF p_ln # 0
*      IF l_flt % p_ln = 0 .AND. l_flt # 0
*        EJECT
*      ENDIF
*    ENDIF
    l_flt = l_flt+1
    @ PROW()+1, 0 SAY str_w
  ENDSCAN
  SELECT PS_9928
ENDSCAN
USE

DO Term_Prn WITH ""

RETURN
