*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                      �ムョキ� �����き諷 ( �/� ).                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Inv_Bn

PRIVATE dat_0, dat_1, dat_v0, dat_v1, ex, sw, p_numb, p_line
PRIVATE mss

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※�∽┬
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

PRIVATE ln, ln1, l_ex, wk_list
USE PLACES
p_numb = RECCOUNT()
p_line = ""
FOR dat_1 = 1 TO p_numb
  p_line = p_line+CHR(ASC("A")+dat_1-1)
ENDFOR

USE PARMS
dat_0  = DAT_START
dat_v0 = DAT_START
USE
dat_1  = DATE()
dat_v1 = DATE()

DIMENSION wk_list(p_numb)
FOR ex = 1 TO p_numb
  wk_list(ex) = CHR(ASC("A")+ex-1) $ p_line
ENDFOR

ex   = 1

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��皀燿�� 陳陳陳陳陳陳陳陳� 敖 ç��お� 陳陳陳陳陳陳陳陳陳朕 �
*� �                           � �                              � �
*� � �����讚�� ���� ��.��.���� � � ( ) ....:....!....:....!.... � �
*� �                           � �                              � �
*� � ���ョ��� ����  ��.��.���� � �                              � �
*� �                           � �                              � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�               < OK Ctrl-W > < �皖���碎瘴 Esc >                 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

ln1  = p_numb+4
ln   = ln1+4
l_ex = 6+p_numb

DO D_Win_N WITH ln, 68, "�┼赳�"

DO Sun_Bord WITH 2, 3, 8, 31
@  2, 5 SAY " ��皀燿�� "

DO Sun_Bord WITH 2, 32, 2+ln1-1, 64
@  2, 34 SAY " ç��お� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 4,  5 SAY "�����讚�� ����" GET dat_0 PICTURE "@D"
@ 6,  5 SAY "���ョ��� ���� " GET dat_1 PICTURE "@D"
DO G_Place WITH 4, 35, wk_list

@ l_ex, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID V_Dat()

POP KEY

RELEASE WINDOW (win_name)
IF ex = 2
  RETURN
ENDIF

USE ACCOUNT ORDER TAG CUS_CODE

SELECT 0
USE INCMONEY ORDER TAG DOC_NUM

SELECT 0
USE SALE_TIT ORDER TAG HRON

IF .NOT. SEEK(DTOS(dat_0))
  ex = RECNO(0)
  IF BETWEEN(ex,1,RECCOUNT())
    GO ex
  ENDIF
ENDIF

DO Wt_Mess WITH "��ぁ�爲� ����諷..."

DELETE FILE (tmpo_path+"INV_BN.DBF")
*DELETE FILE (tmpo_path+"INV_BN.CDX")
SELECT 0
SET EXCLUSIVE ON
CREATE DBF (tmpo_path+"INV_BN.DBF")  ;
   ( CUS_NAME C(FSIZE("CUS_NAME","ACCOUNT")),  ;
     EXP_DATE D,     ;
     EXP_NUM  C(11), ;
     INV_SUM  N(12), ;
     ORD_SUM  N(12)  )

SET EXCLUSIVE OFF
PRIVATE s_inv_0, s_inv_1, s_ord_0, s_ord_1, c_name
SELECT SALE_TIT

SCAN REST WHILE DOC_DATE <= dat_1 FOR NEW_TYPE = " " .AND. LEFT(DOC_NUM,1) $ p_line

  s_inv_0 = TOTAL_R
  s_ord_0 = KASSA

  IF .NOT. SEEK(CUS_CODE, "ACCOUNT")
    c_name = SALE_TIT.CUS_NAME
  ELSE
    c_name = ACCOUNT.CUS_NAME
  ENDIF
  
  SELECT INV_BN
  APPEND BLANK
  REPLACE CUS_NAME WITH c_name, ;
          EXP_DATE WITH SALE_TIT.DOC_DATE,  ;
          EXP_NUM  WITH LEFT(SALE_TIT.DOC_NUM,4)+"-"+ALLTRIM(RIGHT(SALE_TIT.DOC_NUM,6)), ;
          INV_SUM  WITH s_inv_0, ;
          ORD_SUM  WITH s_ord_0
  SELECT SALE_TIT

ENDSCAN

DO Wt_Mess
IF RECCOUNT("INV_BN") = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ���ぅ�� �� �き�� �����き��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"INV_BN.DBF")
  *DELETE FILE (tmpo_path+"INV_BN.CDX")
  RETURN
ENDIF  

PRIVATE sb, p_ln, fnt, n_cp, lft, t_p, f_p, f_d, n, i, p_drctry
DIMENSION sb(7,2)

*{Pgs}        - �矗�����/≡ィ� 痰�����
*{Dat1  }     - �����讚�� ����
*{Dat2  }     - ���ョ��� ����
*{T001      } - �祠�� �� �����き覓 �� 痰����罐
*{T002      } - �祠�� �� �爬�. �爐ム�� �� 痰����罐
*{T101      } - �祠�� �� �����き覓 �� ぎ�祠キ矣
*{T102      } - �祠�� �� �爬�. �爐ム�� �� ぎ�祠キ矣

sb(1,1) = "{Pgs}"
sb(1,2) = ""

sb(2,1) = "{Dat1  }"
sb(2,2) = TRANSFORM(dat_0,"@D ")

sb(3,1) = "{Dat2  }"
sb(3,2) = TRANSFORM(dat_1,"@D ")

sb(4,1) = "{T001      }"
sb(4,2) = "          "

sb(5,1) = "{T002      }"
sb(5,2) = "          "

sb(6,1) = "{T101      }"
sb(6,2) = "          "

sb(7,1) = "{T102      }"
sb(7,2) = "          "

SELECT ACCOUNT
USE

SELECT INCMONEY
USE

SELECT SALE_TIT

USE DOC_FORM
LOCATE FOR "INV_BN" == ALLTRIM(UPPER(DOC_NAME))

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
DIMENSION f_p(n)
FOR i = 1 TO n
  f_p(i) = MLINE(PAGE_F,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR

USE
SELECT INV_BN
GO TOP

PRIVATE n_pg, f_pg, n_det, j, stw

n_det = p_ln-ALEN(t_p)-ALEN(f_p)-ALEN(f_d)
n_pg  = CEILING(RECCOUNT()/n_det)

STORE 0 TO s_inv_1, s_ord_1
DO Ini_Prn WITH "�����き襯 �/�", p_ln, lft, n_cp, fnt, ffeed, p_drctry
FOR f_pg = 1 TO n_pg
  IF f_pg # 1
    EJECT
  ENDIF
  STORE 0 TO s_inv_0, s_ord_0
  sb(1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  FOR i = 1 TO ALEN(t_p)
    stw = t_p(i)
    FOR j = 1 TO 7
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR
  FOR i = 1 TO n_det

    stw = PADR(TRANSFORM(EXP_DATE,"@D "),10)+" "+EXP_NUM+" "+  ;
          TRANSFORM(INV_SUM,"@Z 999999999999")+" "+     ;
          TRANSFORM(ORD_SUM,"@Z 999999999999")+" "+     ;
          CUS_NAME
    s_inv_0 = s_inv_0+INV_SUM
    s_ord_0 = s_ord_0+ORD_SUM
    @ PROW()+1, 0 SAY stw
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR

  sb(4,2) = TRANSFORM(s_inv_0,"999999999999")
  sb(5,2) = TRANSFORM(s_ord_0,"999999999999")

  s_inv_1 = s_inv_1+s_inv_0
  s_ord_1 = s_ord_1+s_ord_0

  FOR i = 1 TO ALEN(f_p)
    stw = f_p(i)
    FOR j = 1 TO 7
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR
  IF EOF()
    EXIT
  ENDIF
ENDFOR

sb(6,2) = TRANSFORM(s_inv_1,"999999999999")
sb(7,2) = TRANSFORM(s_ord_1,"999999999999")

FOR i = 1 TO ALEN(f_d)
  stw = f_d(i)
  FOR j = 1 TO 7
    stw = STRTRAN(stw, sb(j,1), sb(j,2))
  ENDFOR
  @ PROW()+1, 0 SAY stw
ENDFOR

DO Term_Prn WITH ""

CLOSE DATABASES
DELETE FILE (tmpo_path+"INV_BN.DBF")
*DELETE FILE (tmpo_path+"INV_BN.CDX")

RETURN

*********************************************************************
PROCEDURE V_Dat

IF ex = 2
  RETURN .T.
ENDIF

IF dat_0 < dat_v0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��ゥ� ����襯 皰�讓� � "+TRANSFORM(dat_v0,"@D")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  dat_0 = dat_v0
  SHOW GETS
  RETURN .F.
ENDIF

IF dat_1 > dat_v1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�ィ�き� 皰�讓� "+TRANSFORM(dat_v1,"@D")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  dat_1 = dat_v1
  SHOW GETS
  RETURN .F.
ENDIF
PRIVATE l_tmp, i

l_tmp = ""
FOR i = 1 TO p_numb
  IF wk_list(i)
    l_tmp = l_tmp+CHR(ASC("A")+i-1)
  ENDIF
ENDFOR

IF EMPTY(l_tmp)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�甄� ��� �� ┃皀爛痺モ �� �き� �����お�?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

p_line = l_tmp

RETURN .T.
