*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Who_Inc      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  ���-〓 �爬�閧諷 ぅ���ォ �� �ム┏�.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 30.04.1999 様�
PROCEDURE Who_Inc

PRIVATE dt0, dt1
dt0 = {}
dt1 = DATE()
IF .NOT. G_Period(dt0, dt1)
  RETURN
ENDIF

DO Wt_Mess WITH " ��ぃ�皰→� ����諷... "
DO Prep_Tmp
DO Wt_Mess
DO P_Doc
USE
DELETE FILE (tmpo_path+"WHO000.DBF")
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��ぃ�皰→� �����.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 30.04.1999 陳�
PROCEDURE Prep_Tmp
PRIVATE rc, w

DELETE FILE (tmpo_path+"WHO_INC.DBF")
DELETE FILE (tmpo_path+"WHO_INC.CDX")

CREATE DBF (tmpo_path+"WHO_INC.DBF") ;
  ( CODE     N( 3), ;
    NAME     C(30), ;
    QNT_POS  N(10), ;
    QNT      N(10), ;
    QNT_PACK N(10) )
INDEX ON CODE TAG CODE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"ACT_S") ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK

SELECT 0
USE (base_path+"ACT_S_T")

SCAN FOR BETWEEN(DOC_DATE, dt0, dt1) .AND. SUP_CODE > 0
  w = DTOC(DOC_DATE)+" "+DOC_NUM
  @ WROWS()-1, 0 SAY PADC(w, WCOLS())
  IF .NOT. SEEK(WHO_ACT, "WHO_INC")
    SELECT WHO_INC
    APPEND BLANK
    REPLACE CODE WITH ACT_S_T.WHO_ACT
  ENDIF
  SELECT ACT_S
  SEEK ACT_S_T.FIRM+LEFT(DTOS(ACT_S_T.DOC_DATE),4)+ACT_S_T.DOC_NUM
  SCAN REST WHILE FIRM = ACT_S_T.FIRM .AND. DOC_DATE = ACT_S_T.DOC_DATE .AND. DOC_NUM = ACT_S_T.DOC_NUM
    REPLACE WHO_INC.QNT_POS  WITH WHO_INC.QNT_POS+1, ;
            WHO_INC.QNT      WITH WHO_INC.QNT+ABS(QNT_INC),   ;
            WHO_INC.QNT_PACK WITH WHO_INC.QNT_PACK+ABS(FLOOR(QNT_INC/MAX(STOCK.QNT_ROUND,1)))
  ENDSCAN
  SELECT ACT_S_T
ENDSCAN

USE

SELECT ACT_S
USE

SELECT STOCK
USE (base_path+"PERSONS") ORDER TAG CODE
SELECT WHO_INC
SET RELATION TO CODE INTO PERSONS
REPLACE ALL NAME WITH ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)
SET RELATION TO
SELECT PERSONS
USE
SELECT WHO_INC
INDEX ON NAME TAG NAME
COPY TO (tmpo_path+"WHO000")
USE (tmpo_path+"WHO000") ALIAS WHO_INC
DELETE FILE (tmpo_path+"WHO_INC.DBF")
DELETE FILE (tmpo_path+"WHO_INC.CDX")

RETURN
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� P_Doc        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �ョ�碎 ぎ�祠キ��.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 30.04.1999 陳�
PROCEDURE P_Doc
PRIVATE p_ln, fnt, n_cp, lft, n, i, t_p, f_p, t_d, f_d, p_drctry
PRIVATE sel_rem, f_name, nn, ffeed, sb

DIMENSION sb(9,2)
sb( 1,1) = "{��./��-�}"
sb( 1,2) = ""

sb( 2,1) = "{���� �  }"
sb( 2,2) = DTOC(dt0)

sb( 3,1) = "{���� �  }"
sb( 3,2) = DTOC(dt1)

sb( 4,1) = "{���. ��.}"
sb( 4,2) = ""

sb( 5,1) = "{��. ��. }"
sb( 5,2) = ""

sb( 6,1) = "{��. ��. }"
sb( 6,2) = ""

sb( 7,1) = "{���. ��.}"
sb( 7,2) = ""

sb( 8,1) = "{��. ��. }"
sb( 8,2) = ""

sb( 9,1) = "{��. ��. }"
sb( 9,2) = ""

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "WHO_INC" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
IF n = 0
  DIMENSION t_p(1)
ELSE
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)
IF n = 0
  DIMENSION f_p(1)
ELSE
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_H)
IF n = 0
  DIMENSION t_d(1)
ELSE
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_F)
IF n = 0
  DIMENSION f_d(1)
ELSE
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ENDIF
USE

SELECT WHO_INC
PRIVATE n_p_det, sum_00, sum_01, sum_02, cur_p, hol_p, str_w
PRIVATE sub_1, sub_2, sub_3, sub_4, sub_5, sum_10, sum_11, sum_12, j

STORE 0 TO sum_00, sum_01, sum_02
n_p_det = p_ln-MAX(ALEN(t_p),ALEN(t_d))-MAX(ALEN(f_p),ALEN(f_d))-1
hol_p = CEILING(RECCOUNT()/n_p_det)

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

FOR cur_p = 1 TO hol_p
  sb( 1,2) = ALLTRIM(STR(cur_p))+"/"+ALLTRIM(STR(hol_p))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF cur_p # 1
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
  STORE 0 TO sum_10, sum_11, sum_12
  IF RECCOUNT() # 0
    FOR i = 1 TO n_p_det
      str_w = NAME+STR(QNT_POS, 11)+STR(QNT, 11)+STR(QNT_PACK,11)
      @ PROW()+1, 0 SAY str_w
      sum_10 = sum_10+QNT_POS
      sum_11 = sum_11+QNT
      sum_12 = sum_12+QNT_PACK
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  sum_00 = sum_00+sum_10
  sum_01 = sum_01+sum_11
  sum_02 = sum_02+sum_12
  sb(4, 2) = STR(sum_10)
  sb(5, 2) = STR(sum_11)
  sb(6, 2) = STR(sum_12)
  sb(7, 2) = STR(sum_00)
  sb(8, 2) = STR(sum_01)
  sb(9, 2) = STR(sum_02)
  IF cur_p = hol_p
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
DO Term_Prn WITH "", tmpo_path+"WHO000.DBF"

RETURN
