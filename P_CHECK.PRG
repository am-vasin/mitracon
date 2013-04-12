*������������������������������������������������������������������������Ŀ
*�   ��楤�� P_Check      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         ����� ⮢�୮�� 祪�                          �
*�                                                                        �
*����������������������������������������������������������� 04.04.2006 ���
PROCEDURE P_Check
PARAMETERS tmpFirm, tmpDoc, tmpDate

PRIVATE s_sav				&& ����� ��࠭塞 ����� ࠡ�祩 ������
PRIVATE d_year				&& ��� ���㬥��
PRIVATE d_nc				&& ����� ���-� ��� ���� (⮢. 祪)
PRIVATE d_nb				&& ����� ���-� ��� ���� (���-䠪���)
PRIVATE d_ni				&& ����� ���-� ��� ���� (���������)
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
PRIVATE c_code				&& ��� ������

PRIVATE n, i, j, k, nn, str_w, line_1, line_2
PRIVATE wDoc, wFirm, wDate

IF TYPE("tmpDate") = "D"
  d_year = LEFT(DTOS(tmpDate),4)
ENDIF
IF TYPE("tmpDate") = "N"
  d_year = STR(tmpDate,4)
ENDIF
IF TYPE("tmpDate") = "C"
  d_year = tmpDate
ENDIF

s_sav = SELECT()

*
*   ���뢠�� ⠡����
*
k00 = tmpFirm+d_year+tmpDoc
SELECT 0
USE (base_path+"SALE_TIT") ALIAS TIT_20220 AGAIN ORDER TAG DOC_NUM
IF .NOT. SEEK(k00)
  USE
  SELECT (s_sav)
  RETURN
ENDIF
wFirm = FIRM
wDoc  = DOC_NUM
wDate = DOC_DATE

IF EMPTY(T_CHECK)
  PRIVATE tmpNum
  tmpNum = Doc_Num("G_CHECK", FIRM, WHERE, DOC_DATE)
  REPLACE T_CHECK WITH STR(tmpNum)
ENDIF
c_code = CUS_CODE

SELECT 0
USE (base_path+"ACCOUNT") ALIAS ACC_20220 AGAIN ORDER TAG CUS_CODE
SEEK TIT_20220.CUS_CODE

SELECT 0
USE (base_path+"PERSONS") ALIAS PRS_20220 AGAIN ORDER TAG CODE
SEEK TIT_20220.WHO

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
DIMENSION sb(16,2)
*
sb(01,1) = "{���.   }"		&& ��࠭��

*
*		�����䨪��� ���㬥��
*
sb(02,1) = "{����. �  }"	&& ����� ���������
sb(02,2) = LEFT(TIT_20220.DOC_NUM, 4)+"-"+ALLTRIM(SUBSTR(TIT_20220.DOC_NUM, 5))
sb(02,2) = PADR(sb(02,2), LEN(sb(02,1)))

sb(03,1) = "{��-�. �  }"	&& ����� ���-䠪����
sb(03,2) = LEFT(TIT_20220.NEW_DOC, 1)+"-"+ALLTRIM(SUBSTR(TIT_20220.NEW_DOC, 2))
sb(03,2) = PADR(sb(03,2), LEN(sb(03,1)))

sb(04,1) = "{���.�. � }"	&& ����� ⮢�୮�� 祪�
sb(04,2) = ALLTRIM(TIT_20220.T_CHECK)
sb(04,2) = PADR(sb(04,2), LEN(sb(04,1)))

sb(05,1) = "{���    }"		&& ��� ���-�
sb(05,2) = DTOC(TIT_20220.DOC_DATE)

sb(06,1) = "{��������                    }"		&& ���� ���-�
sb(06,2) = ALLTRIM(PRS_20220.FAMILY)+" "+LEFT(PRS_20220.NAME, 1)+" "+LEFT(PRS_20220.S_NAME, 1)
sb(06,2) = PADR(sb(06,2), LEN(sb(06,1)))
*
*		������
*
sb(07,1) = "{������                                          }"
sb(07,2) = ACC_20220.CUS_NAME
sb(07,2) = PADR(sb(05,2), LEN(sb(05,1)))

sb(08,1) = "{�।���⨥                                     }"
sb(08,2) = FRM_20220.L_NAME
sb(08,2) = PADR(sb(05,2), LEN(sb(05,1)))

sb(09,1) = "{�㬬� ��� ��}"	&& ���� �㬬� �� ���㬥��� (�筮)
sb(09,2) = "               "

sb(10,1) = "{�㬬� ���    }"	&& ���� �㬬� �� ���㬥���
sb(10,2) = "               "

sb(11,1) = "{�㬬� ��� ��}"	&& ���� �㬬� �� ��࠭�� (�筮)
sb(11,2) = "               "

sb(12,1) = "{�⮣� ���    }"	&& ���� �㬬� �� ��࠭��
sb(12,2) = "               "

sb(13,1) = "{��� �ய����     }"
sb(13,2) = Date_Str(TIT_20220.DOC_DATE)

sb(14,1) = "{�㬬� �ய����                                  }"
sb(14,2) = "                                                  "

sb(15,1) = "{�㬬� �ய���� 1                                }"
sb(15,2) = "                                                  "

sb(16,1) = "{�㬬� �ய���� 2                                }"
sb(16,2) = "                                                  "

***************************************************
*
*   ���� ��⠫쭮� ��ப�
*
DIMENSION sb_det( 8,2)
*
sb_det(01,1) = "{��}"						&& ��䨪�
sb_det(02,1) = "{������������           }"	&& ������������
sb_det(03,1) = "{��ଠ }"					&& ��ଠ �ந�����⥫�
sb_det(04,1) = "{���� �筮}"				&& ���� �筮
sb_det(05,1) = "{����    }"					&& ���� ���㣫.
sb_det(06,1) = "{�-��}"						&& ���-��
sb_det(07,1) = "{�㬬� �筮  }"			&& �㬬� �筮
sb_det(08,1) = "{�㬬�        }"			&& �㬬� ���㣫

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "T_CHECK" == ALLTRIM(UPPER(DOC_NAME))

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

DO Ini_Prn WITH "������ 祪", p_ln, lft, n_cp, fnt, ffeed, p_drctry

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
    sb_det(01,2) = LEFT(PRINT_TMP.PREFIX, LEN(sb_det(01,1)))

    * sb_det(02,1) = "{Name                   }"	&& ������������
    sb_det(02,2) = LEFT(PRINT_TMP.NAME, LEN(sb_det(02,1)))


    * sb_det(03,1) = "{Prod  }"		&& ��ଠ �ந�����⥫�
    sb_det(03,2) = LEFT(PRINT_TMP.PRODUCER, LEN(sb_det(03,1)))



    *sb_det(04,1) = "{���� �筮}"				&& ���� �筮
    sb_det(04,2) = STR(PRINT_TMP.PRICE, 12, 5)
    
    *sb_det(05,1) = "{����    }"					&& ���� ���㣫.
    sb_det(05,2) = STR(ROUND(PRINT_TMP.PRICE, 2), 10, 2)

    *sb_det(06,1) = "{�-��}"						&& ���-��
    sb_det(06,2) = STR(QNT, 6)

    sb_det(07,1) = "{�㬬� �筮  }"			&& �㬬� �筮
    sb_det(07,2) = STR(ROUND(QNT*PRICE, 2), 15, 2)

    sb_det(08,1) = "{�㬬�        }"			&& �㬬� ���㣫
    sb_det(08,2) = STR(QNT*ROUND(PRICE, 2), 15, 2)


    sum_0_p = sum_0_p + ROUND(QNT*PRICE, 2)
    sum_0_d = sum_0_d + ROUND(QNT*PRICE, 2)
    sum_p   = sum_p   + QNT*ROUND(PRICE, 2)
    sum_d   = sum_d   + QNT*ROUND(PRICE, 2)

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
  sb(11,2) = STR(sum_0_p, 15, 2)
  sb(12,2) = STR(sum_p, 15, 2)

  IF f_pg = n_pg		&& ��᫥���� ��࠭��?

    sb( 9,2) = STR(sum_0_d, 15, 2)
    sb(10,2) = STR(sum_d, 15, 2)
    str_w = Capital(Num2RStr(FLOOR(ABS(sum_0_d))))+" ��. "+ ;
            STR((ABS(sum_0_d)-FLOOR(ABS(sum_0_d)))*100,2)+" ���."
    sb(14,2) = PADR(str_w, LEN(sb(14,1)))

    line_1 = ""
    line_2 = ""
    DO Break_Str WITH str_w, line_1, line_2, LEN(sb(15,1))
    sb(15,2) = line_1
    str_w    = line_2

    line_1 = ""
    DO Break_Str WITH str_w, line_1, line_2, LEN(sb(16,1))
    sb(16,2) = line_1

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

DO Term_Prn WITH "", (tmpo_path+f_name+".DBF"), c_code, "CHECK", wFirm, wDoc, wDate

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
USE (base_path+"SALE") SHARED AGAIN ALIAS DET_20220 ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STO_20220
SELECT 0
CREATE DBF (tmpo_path+tmpName) ;
  ( CODE		N( 7), ;
    PREFIX		C( 4), ;
    NAME		C(35), ;
    PRODUCER	C( 8), ;
    QNT			N( 6), ;
    PRICE		N(12, 5) )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS PRINT_TMP
INDEX ON CODE TAG CODE

SELECT DET_20220
SEEK k00
SCAN REST WHILE DOC_NUM = TIT_20220.DOC_NUM .AND. ;
                FIRM    = TIT_20220.FIRM    .AND. ;
                DOC_DATE= TIT_20220.DOC_DATE
  SELECT PRINT_TMP
*  IF .NOT. SEEK(DET_20220.CODE)
    APPEND BLANK
*  ENDIF
  REPLACE	CODE		WITH DET_20220.CODE,     ;
			PREFIX		WITH STO_20220.PREFIX,   ;
			NAME		WITH STO_20220.NAME,     ;
			PRODUCER	WITH STO_20220.PRODUCER, ;
			QNT			WITH QNT+DET_20220.QNT,  ;
			PRICE		WITH DET_20220.PRICE_R
  SELECT DET_20220
ENDSCAN

USE
SELECT STO_20220
USE
SELECT PRINT_TMP
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
SELECT (s_sav)

RETURN