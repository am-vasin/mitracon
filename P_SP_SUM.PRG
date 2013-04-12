*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ P_Sp_Sum      งเ กฎโ็จช €ญคเฅฉ  แจญ           06.04.98 16:28:40 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ         ฅ็ โ์ คฎชใฌฅญโ  "‘ใฌฌ เญฎฅ คขจฆฅญจฅ ฏฎ แฏฅๆชซจฅญโใ".          บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE P_Sp_Sum
PARAMETERS c_code, vlt

PRIVATE s_sav

s_sav = SELECT()

DO Prep_DBF WITH c_code, vlt

DO P_Doc

USE
DELETE FILE (tmpo_path+"SPEC_TMP")
SELECT (s_sav)
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                         ฎคฃฎโฎขช  DBF-ไ ฉซ .                          บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 06.04.98 16:29:48 อผ
PROCEDURE Prep_DBF
PARAMETERS c_code, vlt

SELECT 0
DELETE FILE (tmpo_path+"SPEC0.DBF")
DELETE FILE (tmpo_path+"SPEC0.CDX")

CREATE DBF (tmpo_path+"SPEC0")  ;
  ( CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    PRICE    N(10, IIF(vlt="$", 4, 2)), ;
    QNT_POS  N( 6), ;
    QNT_NEG  N( 6) )

INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT 0
USE STOCK ORDER TAG CODE

SELECT 0
USE (base_path+"SPEC_SUM") ORDER TAG CUS_POS
SET RELATION TO CODE INTO STOCK
SEEK STR(c_code,8)
SCAN REST WHILE CUS_CODE = c_code
  SELECT SPEC0
  APPEND BLANK
  REPLACE CODE     WITH STOCK.CODE,     ;
          PREFIX   WITH STOCK.PREFIX,   ;
          NAME     WITH STOCK.NAME,     ;
          PRODUCER WITH STOCK.PRODUCER, ;
          PRICE    WITH IIF(vlt="$", STOCK.PRICE, STOCK.PRICE_R), ;
          QNT_POS  WITH IIF(SPEC_SUM.QNT < 0, -SPEC_SUM.QNT, 0),  ;
          QNT_NEG  WITH IIF(SPEC_SUM.QNT > 0,  SPEC_SUM.QNT, 0)
  SELECT SPEC_SUM
ENDSCAN

USE

SELECT STOCK
USE

SELECT SPEC0
COPY TO (tmpo_path+"SPEC_TMP")
USE (tmpo_path+"SPEC_TMP")

DELETE FILE (tmpo_path+"SPEC0.DBF")
DELETE FILE (tmpo_path+"SPEC0.CDX")

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                   ฅฏฎแเฅคแโขฅญญฎ ฏฅ็ โ์ คฎชใฌฅญโ .                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 07.04.98 14:52:26 อผ
PROCEDURE P_Doc

PRIVATE sum_n_p, sum_p_p, sum_n_d, sum_p_d, sn, sp, pic1, pic2

pic1 = IIF(vlt="$", "@Z 99 999.9999", "@Z 9999 999.99")
pic2 = IIF(vlt="$", "@Z 9 999 999.9999", "@Z 999 999 999.99")
DIMENSION sb(9,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Spec Name}"
sb( 2,2) = ALLTRIM(ACCOUNT.CUS_NAME)

sb( 3,1) = "{Date}"
sb( 3,2) = TRANSFORM(DATE(), "@D")

sb( 4,1) = "{Pg_Sum_Neg  }"
sb( 4,2) = ""

sb( 5,1) = "{Pg_Sum_Pos  }"
sb( 5,2) = ""

sb( 6,1) = "{Pg_Sum_Tot  }"
sb( 6,2) = ""

sb( 7,1) = "{Dc_Sum_Neg  }"
sb( 7,2) = ""

sb( 8,1) = "{Dc_Sum_Pos  }"
sb( 8,2) = ""

sb( 9,1) = "{Dc_Sum_Tot  }"
sb( 9,2) = ""

PRIVATE p_ln, fnt, n_cp, lft, ffeed, p_drctry, s_sv
PRIVATE n, i, t_p, f_p, f_d
DIMENSION t_p(1), f_p(1), f_d(1)

STORE "" TO t_p(1), f_p(1), f_d(1)
s_sv = SELECT()

SELECT 0
USE DOC_FORM
LOCATE FOR "CORR_SUM" == ALLTRIM(UPPER(DOC_NAME))
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

n = MAX(MEMLINES(DOC_F),1)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR
USE

SELECT (s_sv)

DO Ini_Prn WITH "‘ใฌฌ เญฎฅ คขจฆฅญจฅ ฏฎ แฏฅๆชซจฅญโใ", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

PRIVATE n_p_det, n_pg, f_pg, str_w
n_p_det = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = MAX(CEILING(RECCOUNT()/n_p_det),1)
ENDIF

STORE 0 TO sum_n_d, sum_p_d, sum_n_p, sum_p_p
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
  STORE 0 TO sum_n_p, sum_p_p
  IF RECCOUNT() # 0
    FOR i = 1 TO n_p_det
      sn = QNT_NEG*PRICE
      sp = QNT_POS*PRICE
      @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+" "+  ;
             TRANSFORM(PRICE, pic1)+" "+  ;
             TRANSFORM(QNT_NEG, "@Z 999999")+" "+  ;
             TRANSFORM(sn, pic2)+" "+ ;
             TRANSFORM(QNT_POS, "@Z 999999")+" "+  ;
             TRANSFORM(sp, pic2)+" "+ ;
             TRANSFORM(sp-sn, pic2)
      sum_n_p = sum_n_p+sn
      sum_p_p = sum_p_p+sp
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  sum_n_d = sum_n_d+sum_n_p
  sum_p_d = sum_p_d+sum_p_p
  sb( 4,2) = TRANSFORM(sum_n_p, pic2)
  sb( 5,2) = TRANSFORM(sum_p_p, pic2)
  sb( 6,2) = TRANSFORM(sum_p_p-sum_n_p, pic2)
  sb( 7,2) = TRANSFORM(sum_n_d, pic2)
  sb( 8,2) = TRANSFORM(sum_p_d, pic2)
  sb( 9,2) = TRANSFORM(sum_p_d-sum_n_d, pic2)
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

DO Term_Prn WITH "", tmpo_path+"SPEC_TMP.DBF"

RETURN
