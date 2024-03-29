PARAMETERS firm_c, doc_n, doc_d  &&, flgs
*
*  firm_c - ��� �।�����;
*  doc_n  - ����� ���㬥��;
*  doc_d  - ���� ��� ���㬥��, ���� ��� � ᨬ���쭮� ��� �᫮��� �ଠ�;
*  flg    - 䫠���.
*

PRIVATE s_sav   && ����� ࠡ�祩 ������ ��� ������
PRIVATE d_year  && ��� ���㬥��
PRIVATE d_n     && ����� ���㬥�� � ⥪�⮢�� ����
PRIVATE d_d     && ��� ���㬥�� � ⥪�⮢�� ����
PRIVATE c_name  && ��� ������ (�᫨ ⠪���� ����� ����)
PRIVATE c_code  && ��� ������ (�᫨ ⠪���� ����� ����)
PRIVATE sb      && ���ᨢ ���ᠭ�� �ᥢ�������
PRIVATE t_d     && ���ᨢ � ��ப��� 蠡���� "��������� ���㬥�"
PRIVATE t_p     && ���ᨢ � ��ப��� 蠡���� "��������� ��࠭���"
PRIVATE f_d     && ���ᨢ � ��ப��� 蠡���� "������ ���㬥�"
PRIVATE f_p     && ���ᨢ � ��ப��� 蠡���� "������ ��࠭���"
PRIVATE n, j    && ���稪�, �������, ��ࠬ���� 横��
PRIVATE p_ln    && ����� ��࠭��� � ��ப��
PRIVATE fnt     && ����: " " - �����, "1" - ᦠ�� (condenced)
PRIVATE n_cp    && ��᫮ �����
PRIVATE lft     && ���� ᫥�� � ᨬ�����
PRIVATE ffeed   && �����襭�� ���㬥��: �� ����⥫쭮� - �ண�� ��ப,
                &&      ����⥫쭮� - �ண�� �ଠ�
PRIVATE p_drcty && ���ࠢ����� ���� (�ਭ��)
PRIVATE k00     && ���� ��� ���᪠ ���㬥��

s_sav = SELECT()

*
* ��ନ�㥬 ��� ���㬥��
IF TYPE("doc_n") = "D"
  d_year = LEFT(DTOS(doc_n),4)
ENDIF
IF TYPE("doc_n") = "N"
  d_year = STR(doc_n,4)
ENDIF
IF TYPE("doc_n") = "C"
  d_year = doc_n
ENDIF

*
* �������� ����� ���㬥��
d_n = LEFT(doc_n,4)+"-"+ALLTRIM(RIGHT(doc_n,6)) !!!!!!!!!!!!!!!
d_n = ALLTRIM(doc_n)                            !!!!!!!!!!!!!!!

*
*  ��������� ���㬥�⮢...
SELECT 0
USE (base_path+"!!!!!_TIT") ORDER TAG DOC_NUM ALIAS TIT_PRINT AGAIN
k00 = firm_c+d_year+doc_n        !!!!!!!!!!!!!!!!!!!
*
*  �饬...
IF .NOT. SEEK(k00)  && �� ��諨...
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ���㬥�� � ����஬ "+d_n
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*
*  �饬 ������...
IF .NOT. EMPTY(CUS_CODE)
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_NNNN AGAIN
  SEEK TIT_PRINT.CUS_CODE
  c_name = ALLTRIM(CUS_NAME)
  c_code = CUS_CODE
  USE
ENDIF
SELECT TIT_PRINT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

*
*   �����⮢�� �६������ 䠩��.
*
*  ��������! �� ��室� �� 䠩�� �� ������ � �ணࠬ�� ���� 䠩��
*            �஬� �६������ ������ ���� �������!
*            �६���� 䠩� �� ����� ����� �����ᮢ!
*
DO Wt_Mess WITH "�����⮢�� ᯨ᪠"
DO Prep_Tmp
DO Wt_Mess

*
*  ����뢠�� �ᥢ������
DIMENSION sb(??,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Doc_Num  }"
sb( 2,2) = d_n

sb( 3,1) = "{Doc_Date}"
sb( 3,2) = d_d

sb( 4,1) = "{Customer         }"
sb( 4,2) = c_name
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

*
* ���砥� ���ᠭ�� ���㬥��
SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "!!!!!!!!!!" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN   && ����� ��࠭��� � ��ப��
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT  && ����: " " - �����, "1" - ᦠ�� (condenced)
n_cp = DOC_FORM.N_COPIES   && ��᫮ �����
lft  = DOC_FORM.LEFT_FIELD && ���� ᫥�� � ᨬ�����
ffeed =DOC_FORM.F_FEED     && �����襭�� ���㬥��: 
                           &&      �� ����⥫쭮� - �ண�� ��ப,
                           &&      ����⥫쭮� - �ண�� �ଠ�
p_drctry = DOC_FORM.P_DIR  && ���ࠢ����� ���� (�ਭ��)

*
*  ��ନ�㥬 蠡���� ��� ���������� � ��������
*
n = MEMLINES(PAGE_H)  &&     ��������� ��࠭��� (�� ��ࢮ�)
IF n > 0
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ELSE
  DIMENSION t_p(1)
  t_p(1) = ""
ENDIF

n = MEMLINES(DOC_H)  &&     ��������� ��ࢮ� ��࠭���
IF n > 0
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ELSE
  DIMENSION t_d(ALEN(t_p))
  FOR j = 1 TO ALEN(t_p)
    t_d(j) = t_p(j)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)  &&     ������ ��࠭��� (�� ��᫥����!)
IF n > 0
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ELSE
  DIMENSION f_p(1)
  f_p(1) = ""
ENDIF

n = MEMLINES(DOC_F)  &&     ������ ��᫥���� ��࠭���
IF n > 0
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ELSE
  DIMENSION f_d(ALEN(f_p))
  FOR j = 1 TO ALEN(f_p)
    f_d(j) = f_p(j)
  ENDFOR
ENDIF

USE

SELECT TMP_LST

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

PRIVATE n_p_det  && ��᫮ ��⠫��� ��ப �� ��࠭��
PRIVATE n_pg     && ��᫮ ��࠭�� � ���㬥��
PRIVATE f_pg     && ������ ��࠭�� ���㬥�� �� ����
PRIVATE str_w    && ��ࠧ ���⠥��� ��ப�
PRIVATE sum_p0, sum_p1 && ...�㬬� �� ��࠭�栬
PRIVATE sum_d0, sum_d1 && ...�㬬� �� ���㬥���
PRIVATE nn       && ����� ��ப� �� ���浪�...

n_p_det = p_ln-MAX(ALEN(f_p),ALEN(f_d))-MAX(ALEN(t_p),ALEN(t_d))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = MAX(CEILING(RECCOUNT()/n_p_det),1)
ENDIF

*
*  ��������� ���㬥��
sb( 1,2) = "1/"+ALLTRIM(STR(n_pg))
sb( 1,2) = PADL(sb(1,2), LEN(sb(1,1)))
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

STORE 0 TO sum_d0, sum_d1, nn
FOR f_pg = 1 TO n_pg    && �� ��࠭�栬...

  IF f_pg # 1  && ��������� ��࠭��� (�� ��ࢮ�!)
    *  �᫨ ����� ���� �㬥��� ��࠭��...
    sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
    sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
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
  ENDIF
  STORE 0 TO sum_p0, sum_p1
  IF RECCOUNT() # 0
    *
    *  ��⠫�� ��ப� ��।��� ��࠭���
    FOR i = 1 TO n_p_det
      nn = nn+1
      @ PROW()+1,0 SAY !!!!!!!!!!!!!!!!!
      sum_p0 = sum_p0+!!!!!!!!!!!
      sum_p1 = sum_p1+!!!!!!!!!!!
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  sb( !!!!!,2) = TRANSFORM(sum_p0, "9999999.99")
  sb( !!!!!,2) = PADL(sb(2,2), LEN(sb(2,1)))
  sb( !!!!!,2) = TRANSFORM(sum_p1, "9999999.99")
  sb( !!!!!,2) = PADL(sb(2,2), LEN(sb(2,1)))
  sum_d0 = sum_d0+sum_p0
  sum_d1 = sum_d1+sum_p1
  *
  *  ����砭�� ��।��� ��࠭��� (�� ��᫥����!)
  *
  IF f_pg # n_pg
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
  ENDIF
ENDFOR
sb( !!!!!,2) = TRANSFORM(sum_d0, "9999999.99")
sb( !!!!!,2) = PADL(sb(2,2), LEN(sb(2,1)))
sb( !!!!!,2) = TRANSFORM(sum_d1, "9999999.99")
sb( !!!!!,2) = PADL(sb(2,2), LEN(sb(2,1)))
*
*   ����砭�� ��᫥���� ��࠭���
*
FOR i = 1 TO ALEN(f_d)
  str_w = f_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR
str_w = DBF("TMP_LST")
USE

DO Term_Prn WITH "", str_w, IIF(TYPE("c_code") = "N", c_code, .F.)

DELETE FILE (str_w)
SELECT (sel_rem)
RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�              �ணࠬ�� �����⮢�� ᮤ�ন���� ᯨ᪠.                 �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Prep_Tmp
PRIVATE tag_n, tag_exp
SELECT 0
CREATE DBF (tmpo_path+"PRINT.TMP")
   (PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT      N( 6), ;
    PRC      N( 8,2), ;
    SUM_T    N(10,2))
USE (tmpo_path+"PRINT.TMP") ALIAS TMP_LST EXCLUSIVE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

*
*   �����������
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_PRINT AGAIN
*
*   ��⠫�� 䠩�
SELECT 0
USE (base_path+"!!!!!_D") ORDER TAG DOC_NUM ALIAS DET_PRINT AGAIN
SET RELATION TO CODE INTO ST_PRINT

*
*   ��।��塞 �����᭮� ��ࠦ����
*
tag_n = 1
DO WHILE .NOT. TAG() == TAG(tag_n)
  tag_n = tag_n+1
ENDDO
tag_exp = SYS(14,tag_n)


= SEEK(k00)

SCAN REST WHILE EVALUATE(tag_exp) = k000   && �� ��ப�� ���㬥��
  SELECT TMP_LST
  APPEND BLANK
  REPLACE PREFIX   WITH ST_PRINT.PREFIX,  ;
          NAME     WITH ST_PRINT.NAME,    ;
          PRODUCER WITH ST_PRINT.PRODUCER,;
          QNT      WITH DET_PRINT.QNT,      ;
          PRC      WITH DET_PRINT.PRICE_R,  ;
          SUM_T    WITH DET_PRINT.PRICE_R*DET_PRINT.QNT
  ENDIF
  SELECT DET_PRINT
ENDSCAN
USE

SELECT ST_PRINT
USE

SELECT TIT_PRINT
USE

SELECT TMP_LST

RETURN
