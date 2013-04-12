*�����������������������������������������������������������������������ͻ
*�   ��楤�� P_Def        ��ࠡ��稪 ������ ����ਥ���                 �
*�����������������������������������������������������������������������Ķ
*�                   �ணࠬ�� ���� ᯨ᪠ "�����".                  �
*�                                                                       �
*����������������������������������������������������������� 09.07.2002 ͼ
PROCEDURE P_Def
PARAMETER doc_n

PRIVATE s_sav   && ����� ࠡ�祩 ������ ��� ������
PRIVATE d_n     && ����� ���㬥�� � ⥪�⮢�� ����
PRIVATE d_d     && ��� ���㬥�� � ⥪�⮢�� ����
PRIVATE l_name  && ������������ ᯨ᪠
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

s_sav = SELECT()

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_2708 AGAIN
IF .NOT. SEEK(doc_n)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ᯨ᪠ � ����஬ "+ALLTRIM(STR(doc_n))
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ELSE
  IF "�����" $ LST_NAME
    d_n   = ALLTRIM(STR(doc_n))
    l_name  = LST_NAME
    d_d     = CR_DAT
    DO Prep_Def
  ELSE
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"���᮪ � "+ALLTRIM(STR(doc_n))+' �� ᮤ�ন� "�����!"'
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
ENDIF

*
*   �����⮢�� �६������ 䠩��.
*
DO Wt_Mess WITH "�����⮢�� ᯨ᪠"
DO Wt_Mess

*
*  ����뢠�� �ᥢ������
DIMENSION sb(7,2)
sb(1,1) = "{���.   }"
sb(1,2) = ""

sb(2,1) = "{���᮪}"
sb(2,2) = d_n

sb(3,1) = "{���      }"
sb(3,2) = DTOC(d_d)

sb(4,1) = "{��� ᯨ᪠                      }"
sb(4,2) = l_name

sb(5,1) = "{�⮣� �� ���}"
sb(5,2) = ""

sb(6,1) = "{�⮣� �� ���}"
sb(6,2) = ""

sb(7,1) = "{�����   }"
sb(7,2) = IIF(LT_2708.IS_USD,"  �.�.   ","  ��.   ")

*   ���� ��⠫쭮� ��ப�
*
DIMENSION sb_det(12,2)
*
sb_det(01,1) = "{���}"						&& ��䨪�
sb_det(02,1) = "{������������           }"	&& ������������
sb_det(03,1) = "{��ଠ }"					&& ��ଠ �ந�����⥫�
sb_det(04,1) = "{�-��  }"		    		&& ���-�� (�����)
sb_det(05,1) = "{����      }"
sb_det(06,1) = "{�㬬�     }"
sb_det(07,1) = "{ProductMan   }"
sb_det(08,1) = "{X}"                        && �� ���㯠��: � - �������, � - ᥩ��, _ - ��� �����
sb_det(09,1) = "{Obs}"
sb_det(10,1) = "{��� ����.}"
sb_det(11,1) = "{������}"
sb_det(12,1) = "{�����}"

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "LST_DEF" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN   && ����� ��࠭��� � ��ப��
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT  && ����: " " - �����, "1" - ᦠ�� (condenced)
n_cp = DOC_FORM.N_COPIES   && ��᫮ �����
lft  = DOC_FORM.LEFT_FIELD && ���� ᫥�� � ᨬ�����
ffeed =DOC_FORM.F_FEED     && �����襭�� ���㬥��: 
                           &&      �� ����⥫쭮� - �ண�� ��ப,
                           &&      ����⥫쭮� - �ண�� �ଠ�
p_drctry = DOC_FORM.P_DIR  && ���ࠢ����� ���� (�ਭ��)

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

DO Ini_Prn WITH "�⪠��", p_ln, lft, n_cp, fnt, ffeed, p_drctry

PRIVATE sum_p, sum_d, nn
sum_d = 0

SELECT TMP_DEF
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

FOR f_pg = 1 TO n_pg		&& �� ��࠭�栬...
  sb(1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))	&& ��࠭��/��࠭��
  sb(1,2) = PADR(sb(1,2), LEN(sb(1,1)))
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
  sum_p = 0

  i = 0		&& ���稪 ��⠫��� ��㯯
  DO WHILE .T.
    sb_det(01,2) = TMP_DEF.PREFIX
    sb_det(02,2) = TMP_DEF.NAME
    sb_det(03,2) = TMP_DEF.PRODUCER
    sb_det(04,2) = STR(TMP_DEF.QNT,6)
    sb_det(05,2) = STR(TMP_DEF.PRICE*TMP_DEF.QNT,10,3)
    sb_det(06,2) = STR(TMP_DEF.PRICE*TMP_DEF.QNT,10,3)
    sb_det(07,2) = TMP_DEF.P_MEN
    sb_det(08,2) = TMP_DEF.X
    sb_det(09,2) = IIF(TMP_DEF.OBSOLETE, "O"," ")
    sb_det(10,2) = DTOC(TMP_DEF.W_DATE)
    sb_det(11,2) = STR(TMP_DEF.QNT_FIL,6)
    sb_det(12,2) = STR(TMP_DEF.QNT_REZ,6)

    FOR k = 1 TO ALEN(det)		&& ����⠭���� ����� � ����� ��⠫쭮� ��㯯�
      str_w = det(k)
      FOR j = 1 TO ALEN(sb_det,1)
        str_w = STRTRAN(str_w, sb_det(j,1), sb_det(j,2) )
      ENDFOR
      @ PROW(), 0 SAY str_w
    ENDFOR
    sum_p = sum_p + VAL(sb_det(6,2))
    sum_d = sum_d + VAL(sb_det(6,2))
    i = i+1
    SKIP
    IF EOF("TMP_DEF")
      EXIT
    ENDIF
    IF  i >= n_p_det
      EXIT
    ENDIF
  ENDDO

  *
  *    ����砭�� ��࠭���
  *
  sb(5,2) = STR(sum_p,10,3)
  sb(6,2) = STR(sum_d,10,3)
  IF f_pg = n_pg		&& ��᫥���� ��࠭��?
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
SELECT LT_2708
USE

DO Term_Prn WITH "", (tmpo_path+"TMP_DEF")

DELETE FILE(tmpo_path+"TMP_DEF.DBF")

SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_Def                                                   �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    �����⮢�� �६������ 䠩��.                        �
*�                                                                        �
*����������������������������������������������������������� 17.06.2002 ���
PROCEDURE Prep_Def
PRIVATE dat_wait

SELECT 0
DELETE FILE(tmpo_path+"TMP_DEF.DBF")
CREATE DBF(tmpo_path+"TMP_DEF");
         ( PREFIX   C( 4),  ;
           NAME     C(35),  ;
           PRODUCER C( 8),  ;
           QNT      N( 6),  ;
           PRICE    N(10,3),;
           SUMM     N(10,3),;
           P_MEN    C(13),  ;
           X        C( 1),  ;
           OBSOLETE L,      ;     
           W_DATE   D,      ;
           QNT_FIL  N( 6),  ;
           QNT_REZ  N( 6) )
INDEX ON NAME TAG NAME

SELECT 0
IF File_O(path_comm+"PURCHASE.DBF")
  USE (path_comm+"PURCHASE") ALIAS P_2708 ORDER TAG NAME_DATE AGAIN
  SET FILTER TO PURFLAG = "K"
ENDIF

SELECT 0
USE (path_comm+"SUBJECT") ORDER TAG STO_CODE ALIAS SJ_2708 AGAIN

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S_2708 AGAIN
SET RELATION TO CODE INTO SJ_2708

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG LIST ALIAS LD_2708 AGAIN
SET RELATION TO CODE INTO S_2708
IF SEEK(STR(doc_n,6))
  SCAN REST WHILE LIST_NUM = STR(doc_n,6) FOR .NOT.EMPTY(QNT)
    IF SEEK(CODE, "S_2708")
      dat_wait = {}
      IF USED("P_2708")
        SELECT P_2708
        IF SEEK(S_2708.NAME+S_2708.PREFIX+S_2708.PRODUCER, "P_2708")
          dat_wait = P_2708.DATE
        ENDIF
      ENDIF
      SELECT TMP_DEF
      APPEND BLANK
      REPLACE PREFIX   WITH S_2708.PREFIX,   ;
              NAME     WITH S_2708.NAME,     ;
              PRODUCER WITH S_2708.PRODUCER, ;
              QNT      WITH LD_2708.QNT,     ;
              PRICE    WITH IIF(LT_2708.IS_USD,LD_2708.USD_PRICE,LD_2708.SAL_PRICE),;
              SUMM     WITH IIF(LT_2708.IS_USD,LD_2708.USD_PRICE*LD_2708.QNT,LD_2708.SAL_PRICE*LD_2708.QNT),;
              X        WITH SJ_2708.NOPUR,   ;
              OBSOLETE WITH SJ_2708.OBSOLETE,;     
              P_MEN    WITH SJ_2708.AUTHOR,  ;
              W_DATE   WITH dat_wait,        ;
              QNT_FIL  WITH S_2708.QNT_REAL, ;
              QNT_REZ  WITH S_2708.QNT_REZ
      SELECT LD_2708
    ENDIF
  ENDSCAN
ENDIF

IF USED("P_2708")
  SELECT P_2708
  USE
ENDIF

SELECT SJ_2708
USE
SELECT S_2708
USE
SELECT LD_2708
USE

SELECT TMP_DEF
RETURN