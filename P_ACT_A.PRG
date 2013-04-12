*������������������������������������������������������������������������Ŀ
*�   ��楤�� P_Act_A      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    ����� �ਥ����� ��� � ���ᠬ�                    �
*�                                                                        �
*����������������������������������������������������������� 20.02.2002 ���
PROCEDURE P_Act_A
PARAMETERS tmpFirm, tmpDoc, tmpDate

PRIVATE d_year

IF TYPE("tmpDate") = "D"
  d_year = LEFT(DTOS(tmpDate),4)
ENDIF
IF TYPE("tmpDate") = "N"
  d_year = STR(tmpDate,4)
ENDIF
IF TYPE("tmpDate") = "C"
  d_year = tmpDate
ENDIF

PRIVATE s_sav				&& ����� ��࠭塞 ����� ࠡ�祩 ������
PRIVATE d_year				&& ��� ���㬥��
PRIVATE d_n					&& ����� ���-� ��� ����
PRIVATE k00					&& ���� ��� ���᪠ ���㬥��
PRIVATE sb					&& �ᥢ������ ���㬥��
PRIVATE sb_det				&& �ᥢ������ ��⠫쭮� ��ப�
PRIVATE p_ln				&& ����� ��࠭��� � ��ப��
PRIVATE fnt					&& ����+�ਥ���� ��࠭���
PRIVATE n_cp				&& ������⢮ �����
PRIVATE lft					&& ����� ����
PRIVATE ffeed				&& �����襭�� ���-�
PRIVATE p_drctry			&& ���ࠢ����� ����
PRIVATE t_d					&& ��������� ���-�
PRIVATE f_d					&& ������ ���-�
PRIVATE t_p					&& ��������� ��࠭���
PRIVATE f_p					&& ������ ��࠭���
PRIVATE det					&& ��ࠧ ��⠫쭮� ��ப�
PRIVATE n_p_det1			&& ���-�� ��⠫��� ��ப �� ��࠭��
PRIVATE n_p_det				&& ���-�� ��⠫��� ��ப �� ��࠭��
PRIVATE n_pg				&& ���-�� ��࠭�� � ���㬥��
PRIVATE f_pg				&& ������ ��࠭��
PRIVATE dep_cod				&& ��� ������

PRIVATE n, i, j, k, nn, str_w, tmpPic

s_sav = SELECT()

*
*   ���뢠�� ⠡����
*
k00 = tmpFirm+d_year+tmpDoc
SELECT 0
USE (base_path+"ACT_S_T") ALIAS TIT_20220 AGAIN ORDER TAG DOC_NUM
IF .NOT. SEEK(k00)
  USE
  SELECT (s_sav)
  RETURN
ENDIF
IF TIT_20220.VALUTA = "$"
  tmpPic = "9999999999.9999"
ELSE
  tmpPic = "999999999999.99"
ENDIF

SELECT 0
USE (base_path+"ACCOUNT") ALIAS ACC_20220 AGAIN ORDER TAG CUS_CODE
SEEK TIT_20220.SUP_CODE

SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_20220 AGAIN ORDER TAG CODE
SEEK TIT_20220.WHO_ACT

SELECT 0
USE (base_path+"FIRMS") ALIAS FRM_20220 AGAIN ORDER TAG FIRM_CODE
SEEK TIT_20220.FIRM

f_name = SYS(3)
DO Wt_Mess WITH "�����⮢�� ���㬥��"
DO Print_Tmp WITH f_name
DO Wt_Mess

***************************************************
*
*   ���� ���㬥��
*
DIMENSION sb(14,2)
*
sb(01,1) = "{���.   }"		&& ��࠭��

*
*		�����䨪��� ���㬥��
*
sb(02,1) = "{���. �   }"	&& ����� ���㬥��
sb(02,2) = PADR(ALLTRIM(TIT_20220.DOC_NUM), LEN(sb(02,1)))

sb(03,1) = "{���    }"		&& ��� ���-�
sb(03,2) = DTOC(TIT_20220.DOC_DATE)

sb(04,1) = "{��������                    }"		&& ���� ���-�
sb(04,2) = ALLTRIM(PRS_20220.FAMILY)+" "+LEFT(PRS_20220.NAME, 1)+" "+LEFT(PRS_20220.S_NAME, 1)
sb(04,2) = PADR(sb(04,2), LEN(sb(04,1)))
*
*		������
*
sb(05,1) = "{���⠢騪                                       }"
							&& ��� ���⠢騪�
sb(05,2) = ACC_20220.CUS_NAME
sb(05,2) = PADR(sb(05,2), LEN(sb(05,1)))

sb(06,1) = "{�����⥫�                                      }"
							&& ��� ������
sb(06,2) = FRM_20220.L_NAME
sb(06,2) = PADR(sb(05,2), LEN(sb(05,1)))

sb(07,1) = "{���./����}"		&& ���㯪� / ���������
sb(07,2) = IIF(TIT_20220.FLG_BUY = "B", "���㯪�   ", "���������")

sb(08,1) = "{����. � }"		&& ����� ���������
sb(08,2) = TIT_20220.EXP_NUM

sb(09,1) = "{��� ���}"		&& ��� ���������
sb(09,2) = DTOC(TIT_20220.EXP_DATE)

sb(10,1) = "{%���}"			&& % ���
IF TIT_20220.VALUTA = "�"
  sb(10,2) = STR(TIT_20220.NDS_, 6, 1)
ELSE
  sb(10,2) = "   0.0"
ENDIF

sb(11,1) = "{�⮣� ���    }"	&& ���� �㬬� �� ���㬥���
sb(11,2) = "               "

sb(12,1) = "{�⮣� ���    }"	&& ���� �㬬� �� ��࠭��
sb(12,2) = "               "

sb(13,1) = "{�⮣� 0 ���  }"	&& �㬬� ��� ��� �� ���㬥���
sb(13,2) = "               "

sb(14,1) = "{�⮣� 0 ���  }"	&& �㬬� ��� ��� �� ��࠭��
sb(14,2) = "            "
***************************************************
*
*   ���� ��⠫쭮� ��ப�
*
DIMENSION sb_det(15,2)
*
sb_det(01,1) = "{��}"						&& ��䨪�
sb_det(02,1) = "{������������           }"	&& ������������
sb_det(03,1) = "{��ଠ }"					&& ��ଠ �ந�����⥫�
sb_det(04,1) = "{����      }"				&& ���� ��� ���
sb_det(05,1) = "{�-��}"						&& ���-��
sb_det(06,1) = "{�㬬� 0      }"			&& �㬬� ��� ���
sb_det(07,1) = "{�㬬�        }"			&& �㬬� � ���
sb_det(08,1) = "{��� �                   }"	&& � ���
sb_det(09,1) = "{��࠭�        }"			&& ��࠭� �� ���
sb_det(10,1) = "{��}"						&& ��ப� ���
sb_det(11,1) = "{��� }"						&& ���-�� �� ���㬥���
sb_det(12,1) = "{%���}"						&& % ���
sb_det(12,2) = STR(TIT_20220.NDS_, 6, 1)
sb_det(13,1) = "{���� }"					&& ����
sb_det(14,1) = "{���. ����    }"			&& ���. ����
sb_det(15,1) = "{�}"						&& ��� ��࠭�

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "ACT_ADDR" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

*
*		������ ��������� ���-�
*
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

*
*		������ ��������� ��࠭���
*
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

*
*		������ ������� ��࠭���
*
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

*
*		������ ������� ���-�
*
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

*
*		������ ��⠫쭮� ��ப�
*
n = MEMLINES(DETAIL)
IF n = 0
  DIMENSION det(1)
  det(1) = ""
ELSE
  DIMENSION det(n)
  FOR i = 1 TO n
    det(i) = MLINE(DETAIL,i)
  ENDFOR
ENDIF

USE

DO Ini_Prn WITH "������", p_ln, lft, n_cp, fnt, ffeed, p_drctry

SELECT PRINT_TMP
GO TOP

nn = RECCOUNT()		&& �ᥣ� ��⠫��� �����
n_p_det  = p_ln-ALEN(t_d)-MAX(ALEN(f_p),ALEN(f_d))
							&& ���-�� ��ப ��� ��⠫��� ���� �� ��ࢮ� ��࠭��
n_p_det1 = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
							&& ���-�� ��ப ��� ��⠫��� ���� �� ����ࢮ� ��࠭��
n_p_det  = FLOOR(n_p_det/ALEN(det))		&& ���-�� ��⠫��� ����� �� ��ࢮ� ��࠭��
n_p_det1 = FLOOR(n_p_det1/ALEN(det))	&& ���-�� ��⠫��� ����� �� ����ࢮ� ��࠭��
n_pg = 1
nn = nn-n_p_det		&& ����쪮 ��⠫��� ��㯯 �� ��� ��࠭��� ��� ��ࢮ�

IF n_p_det <= 0		&& ������ ��� ��࠭��� ��� ���������... �� ����� ��࠭��!
  n_p_det = RECCOUNT()
ELSE
  n_pg = 1+CEILING(MAX(nn, 0)/n_p_det1)
ENDIF
PRIVATE sum_d, sum_0_d, sum_p, sum_0_p

STORE 0 TO sum_d, sum_0_d
FOR f_pg = 1 TO n_pg		&& �� ��࠭�栬...
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))	&& ��࠭��/��࠭��
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1   && ��������� �� ��ࢮ� ��࠭���
    EJECT
    n_p_det = n_p_det1		&& ����⠢�塞 ����� ��࠭���...
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
  ELSE        && ��������� ��ࢮ� ��࠭���
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
  *
  *   ��⠫쭠� ���� ��࠭���
  * 
  i = 0		&& ���稪 ��⠫��� ��㯯
  STORE 0 TO sum_p, sum_0_p
  DO WHILE .T.

    ****************************************************
    *
    *     ������塞 ���� ��⠫쭮� ��㯯�
    *

    * sb_det(01,1) = "{Pr}"			&& ��䨪�
    sb_det(01,2) = PRINT_TMP.PREFIX

    * sb_det(02,1) = "{Name                   }"	&& ������������
    sb_det(02,2) = PRINT_TMP.NAME

    * sb_det(03,1) = "{Prod  }"		&& ��ଠ �ந�����⥫�
    sb_det(03,2) = PRINT_TMP.PRODUCER

    * sb_det(04,1) = "{Price   }"	&& ����
    IF TIT_20220.VALUTA = "$"
      sb_det(04,2) = STR(PRINT_TMP.PRICE, 10, 4)
    ELSE
      sb_det(04,2) = STR(PRINT_TMP.PRICE, 10, 2)
    ENDIF

    * sb_det(05,1) = "{Qnt }"		&& ���-��
    sb_det(05,2) = STR(PRINT_TMP.QNT, 6)

    *sb_det(06,1) = "{�㬬� 0      }"			&& �㬬� ��� ���
    sb_det(06,2) = TRANSFORM(PRINT_TMP.SUM_0, tmpPic)

    *sb_det(07,1) = "{�㬬�        }"			&& �㬬� � ���
    sb_det(07,2) = TRANSFORM(PRINT_TMP.SUM_TOT, tmpPic)

    *sb_det(08,1) = "{��� �                   }"	&& � ���
    sb_det(08,2) = PRINT_TMP.GTD_NUM

    *sb_det(09,1) = "{��࠭�        }"			&& ��࠭� �� ���
    sb_det(09,2) = PRINT_TMP.COUNTRY

    *sb_det(15,1) = "{�}"						&& ��� ��࠭�
    sb_det(15,2) = PRINT_TMP.COUNTRYC

    *sb_det(10,1) = "{��}"						&& ��ப� ���
    sb_det(10,2) = PRINT_TMP.GTD_LINE

    *sb_det(11,1) = "{��� }"					&& ���-�� �� ���㬥���
    sb_det(11,2) = PRINT_TMP.GTD_NUM

    *sb_det(12,1) = "{%���}"					&& % ���

    *sb_det(13,1) = "{���� }"					&& ����
    sb_det(13,2) = PRINT_TMP.ADDR

    *sb_det(14,1) = "{���. ����    }"			&& ���. ����
    sb_det(14,2) = PRINT_TMP.MEMO_ST


    sum_0_p = sum_0_p + PRINT_TMP.SUM_0
    sum_0_d = sum_0_d + PRINT_TMP.SUM_0
    sum_p   = sum_p   + PRINT_TMP.SUM_TOT
    sum_d   = sum_d   + PRINT_TMP.SUM_TOT

    FOR k = 1 TO ALEN(det)		&& ����⠭���� ����� � ����� ��⠫쭮� ��㯯�
      str_w = det(k)
      FOR j = 1 TO ALEN(sb_det,1)
        str_w = STRTRAN(str_w, sb_det(j,1), sb_det(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR

    i = i+1
    SKIP
    IF EOF("PRINT_TMP")
      EXIT
    ENDIF
    IF  i >= n_p_det
      EXIT
    ENDIF
  ENDDO

  *
  *    ����砭�� ��࠭���
  *
  sb(12,2) = TRANSFORM(sum_p, tmpPic)
  sb(14,2) = TRANSFORM(sum_0_p, tmpPic)

  IF f_pg = n_pg		&& ��᫥���� ��࠭��?
    sb(11,2) = TRANSFORM(sum_d, tmpPic)
    sb(13,2) = TRANSFORM(sum_0_d, tmpPic)
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

DO Term_Prn WITH "", (tmpo_path+f_name+".DBF")

DELETE FILE (tmpo_path+f_name+".DBF")
DELETE FILE (tmpo_path+f_name+".CDX")

SELECT TIT_20220
USE
SELECT ACC_20220
USE
SELECT PRS_20220
USE
SELECT FRM_20220
USE

SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Print_Tmp    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      �����⮢�� �६������ 䠩��                       �
*�                                                                        �
*����������������������������������������������������������� 21.02.2002 ���
PROCEDURE Print_Tmp
PARAMETER tmpName

PRIVATE s_sav, tmpPrice, s0, s1

s_sav = SELECT()
SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ALIAS STO_20220 ORDER TAG CODE
SELECT 0
USE (base_path+"ACT_S") SHARED AGAIN ALIAS DET_20220 ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STO_20220
SELECT 0
CREATE DBF (tmpo_path+tmpName) ;
  ( CODE		N( 7), ;
    PREFIX		C( 4), ;
    NAME		C(35), ;
    PRODUCER	C( 8), ;
    ADDR		C( 8), ;
    MEMO_ST		C(16), ;
    GTD_NUM		C(26), ;
    COUNTRY		C(16), ;
    COUNTRYC	C( 3), ;
    GTD_LINE	C( 4), ;
    QNT_DOC		N( 6), ;
    QNT			N( 6), ;
    PRICE		N(12, IIF(TIT_20220.VALUTA = "�", 2, 4)),;
    SUM_0		N(15, IIF(TIT_20220.VALUTA = "�", 2, 4)),;
    SUM_TOT		N(15, IIF(TIT_20220.VALUTA = "�", 2, 4)) )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS PRINT_TMP

SELECT DET_20220
SEEK k00
SCAN REST WHILE DOC_NUM = TIT_20220.DOC_NUM .AND. ;
                FIRM    = TIT_20220.FIRM    .AND. ;
                DOC_DATE= TIT_20220.DOC_DATE
  SELECT PRINT_TMP
  IF TIT_20220.VALUTA = "$"
    tmpPrice = DET_20220.PRICE_D
    s0 = tmpPrice*DET_20220.QNT_INC
    s1 = s0
  ELSE
    tmpPrice = DET_20220.PRICE_0
    s0 = tmpPrice*DET_20220.QNT_INC
    s1 = ROUND(s0*(100+TIT_20220.NDS_)/100, 2)
  ENDIF
  APPEND BLANK
  REPLACE	CODE		WITH DET_20220.CODE,     ;
			PREFIX		WITH STO_20220.PREFIX,   ;
			NAME		WITH STO_20220.NAME,     ;
			PRODUCER	WITH STO_20220.PRODUCER, ;
			ADDR		WITH STO_20220.ADDRESS,  ;
			MEMO_ST		WITH STO_20220.MEMO_ST,  ;
			GTD_NUM		WITH DET_20220.GTD_NUM,  ;
			COUNTRY		WITH DET_20220.COUNTRY,  ;
			COUNTRYC	WITH DET_20220.COUNTRYC, ;
			GTD_LINE	WITH DET_20220.GTD_LINE, ;
			QNT_DOC		WITH DET_20220.QNT_EXP,  ;
			QNT			WITH DET_20220.QNT_INC,  ;
			PRICE		WITH tmpPrice,           ;
			SUM_0		WITH s0,                 ;
			SUM_TOT		WITH s1
  SELECT DET_20220
ENDSCAN

USE
SELECT STO_20220
USE
SELECT PRINT_TMP
INDEX ON ADDR TAG ADDR 
SELECT (s_sav)

RETURN