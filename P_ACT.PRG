*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� P_Act        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                         ����� �ਥ����� ���.                         �
*�                                                                        �
*����������������������������������������������������������� 27.01.1999 �ͼ
PROCEDURE P_Act
PARAMETERS firm_c, doc_n, doc_d, flg
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
IF TYPE("doc_d") = "D"
  d_year = LEFT(DTOS(doc_d),4)
ENDIF
IF TYPE("doc_d") = "N"
  d_year = STR(doc_d,4)
ENDIF
IF TYPE("doc_d") = "C"
  d_year = doc_d
ENDIF

*
*  ��ࠬ���� ���ᠭ�� ०��� ���� ���㬥�� � ࠡ�稥 ��६����.
*
PRIVATE is_u        && ���㬥�� � USD
PRIVATE is_full     && ����� (᮪�饭��) ���㬥��
PRIVATE is_diff     && ���쪮 ��宦�����
PRIVATE is_corr     && ���������� � �ਥ����� ����
PRIVATE is_narr     && ����� - ��� �⮫�殢 � �㬬���  NEW 30.06.1999
PRIVATE w_arr       && ���ᨢ ��� �ନ஢���� �㬬� �ய����

PRIVATE sum_0       && �㬬� ��� ���
PRIVATE sum_1       && �㬬� � ���
PRIVATE sum_11      && �㬬� � ��� �� ���������
PRIVATE sum_12      && ����⮪/������� �㬬� � ���

PRIVATE sum_0_p     && �㬬� ��� ��� �� ��࠭��
PRIVATE sum_1_p     && �㬬� � ��� �� ��࠭��
PRIVATE sum_11_p    && �㬬� � ��� �� ��������� �� ��࠭��
PRIVATE sum_12_p    && ����⮪/������� �㬬� � ��� �� ��࠭��

PRIVATE sum_0_d     && �㬬� ��� ��� �� ���㬥���
PRIVATE sum_1_d     && �㬬� � ��� �� ���㬥���
PRIVATE sum_11_d    && �㬬� � ��� �� ��������� �� ���㬥���
PRIVATE sum_12_d    && ����⮪/������� �㬬� � ��� �� ���㬥���

PRIVATE flg_w, shab_name, i, flg_valid, ch, a_code

flg_w = IIF(TYPE("flg") = "C", UPPER(flg), "D")

* NEW 30.06.1999
* �஢��塞 �����⨬���� 䫠��� � flg_w - ��祢 �.

IF EMPTY(flg_w)
  flg_w = "?"
ENDIF

flg_valid="DFS?#"  && ���᮪ �����⨬�� ���祭�� 䫠���

l_flg=LEN(flg_w)
FOR i = 1 TO l_flg              && scan all char flags in string flg_w
  ch = SUBSTR(flg_w,i,1)
  IF NOT (ch $ flg_valid)       && replace an invalid flag by "?"
    flg_w = CHRTRAN(flg_w, ch, "?")
  ENDIF
ENDFOR

* ����� �஢�ન 䫠���
* END OF NEW 30.06.1999

shab_name = ""   && ��� 蠡���� ���㬥�� � DOC_LST

is_u    = .F.    && ���㬥�� � USD
is_full = .T.    && ����� (᮪�饭��) ���㬥��
is_corr = .F.    && ���������� � �ਥ����� ����
is_diff = .F.
is_narr = .F.    && ����� - ��� �⮫�殢 � �㬬��� NEW 30.06.1999

sum_0   = 0    && �㬬� ��� ���
sum_1   = 0    && �㬬� � ���
sum_11  = 0    && �㬬� � ��� �� ���������
sum_12  = 0    && ����⮪/������� �㬬� � ���

sum_0_p  = 0   && �㬬� ��� ��� �� ��࠭��
sum_1_p  = 0   && �㬬� � ��� �� ��࠭��
sum_11_p = 0   && �㬬� � ��� �� ��������� �� ��࠭��
sum_12_p = 0   && ����⮪/������� �㬬� � ��� �� ��࠭��

sum_0_d  = 0   && �㬬� ��� ��� �� ���㬥���
sum_1_d  = 0   && �㬬� � ��� �� ���㬥���
sum_11_d = 0   && �㬬� � ��� �� ��������� �� ���㬥���
sum_12_d = 0   && ����⮪/������� �㬬� � ��� �� ���㬥���


*
* �������� ����� ���㬥��
d_n = ALLTRIM(doc_n)

*
*  ��������� ���㬥�⮢...
SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG DOC_NUM ALIAS TIT_PRINT AGAIN
k00 = firm_c+d_year+doc_n
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
  SELECT (s_sav)
  RETURN
ENDIF
d_d = DTOC(DOC_DATE)
a_code = WHO_ACT

IF .NOT. Set_Flg()  && ��⠭���� ⨯� ���㬥��
  USE
  SELECT (s_sav)
  RETURN
ENDIF

*
*  �饬 ������...
IF .NOT. EMPTY(SUP_CODE)
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_NNNN AGAIN
  SEEK TIT_PRINT.SUP_CODE
  c_name = ALLTRIM(CUS_NAME)
  c_code = CUS_CODE
  USE
ENDIF
SELECT TIT_PRINT

*
*   �����⮢�� �६������ 䠩��.
*
*  ��������! �� ��室� �� 䠩�� �� ������ � �ணࠬ�� ���� 䠩��
*            �஬� �६������ � ���������� ���㬥�⮢ ������ ���� �������!
*            �६���� 䠩� �� ����� ����� �����ᮢ!
*
DO Wt_Mess WITH "�����⮢�� ᯨ᪠"
DO Prep_Tmp
DO Wt_Mess

*
*  ����뢠�� �ᥢ������
DIMENSION sb(26,2)
sb(26,1) = "{�ਬ�砭��                                                            }"
sb(26,2) = ALLTRIM(TIT_PRINT.NOTE)

sb( 1,1) = "{���/���}"
sb( 1,2) = ""

sb( 2,1) = "{N ���_�}"
sb( 2,2) = d_n

sb( 3,1) = "{��� ���}"
sb( 3,2) = d_d

sb( 4,1) = "{��� ��室�}"
sb( 4,2) = IIF(TIT_PRINT.FLG_BUY = "B", "���㯪�", "���������")

sb( 5,1) = "{�����}"
sb( 5,2) = IIF(TIT_PRINT.VALUTA = "$", "� �", "��")

j = SELECT()
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS PRN_FIRM AGAIN
SEEK firm_c
sb(23, 1) = "{�㪮����⥫�          }"
sb(23, 2) = BOSS
sb(24, 1) = "{��. ��壠���         }"
sb(24, 2) = ABAK
sb(25, 1) = "{�����                }"
sb(25, 2) = KASSIR

sb( 6,1) = "{�।���⨥                                     }"
sb( 6,2) = L_NAME
USE
SELECT (j)

sb( 7,1) = "{���⠢騪                                       }"
sb( 7,2) = c_name

sb( 8,1) = "{N ����. }"
sb( 8,2) = ALLTRIM(TIT_PRINT.EXP_NUM)

sb( 9,1) = "{��� �. }"
sb( 9,2) = DTOC(TIT_PRINT.EXP_DATE)

sb(10,1) = "{�㬬� �/� 0�}"
sb(10,2) = ""

sb(11,1) = "{�㬬� � � 0�}"
sb(11,2) = ""

sb(12,1) = "{�㬬� � � 1�}"
sb(12,2) = ""

sb(13,1) = "{�㬬� � � 2�}"
sb(13,2) = ""

sb(14,1) = "{�㬬� �/� 0�}"
sb(14,2) = TRANSFORM(TIT_PRINT.TOTAL_0, "999 999 999.99")

sb(15,1) = "{�㬬� � � 0�}"
sb(15,2) = TRANSFORM(TIT_PRINT.TOTAL_H, "999 999 999.99")

sb(16,1) = "{�㬬� � � 1�}"
sb(16,2) = ""

sb(17,1) = "{�㬬� � � 2�}"
sb(17,2) = ""

sb(18,1) = "{�㬬� ���   }"
sb(18,2) = TRANSFORM(TIT_PRINT.TOTAL_H-TIT_PRINT.TOTAL_0, "999 999 999.99")

sb(19,1) = "{%��}"
sb(19,2) = TRANSFORM(TIT_PRINT.NDS_, "999.9")

DIMENSION w_arr(2)
sb(20,1) = "{�㬬� �ய���� 1                                                    }"
sb(21,1) = "{�㬬� �ய���� 2                                                    }"
w_arr(1) = LEN(sb(20,1))
w_arr(2) = LEN(sb(21,1))

DO Nt_Lines WITH ABS(TIT_PRINT.TOTAL_H), w_arr, TIT_PRINT.VALUTA
sb(20,2) = w_arr(1)
sb(21,2) = w_arr(2)

sb(22,1) = "{Autor                       }"
SELECT 0
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF a_code = 0
  sb(22,2) = "���!"
ELSE
  IF SEEK (a_code)
    sb(22,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)    
  ELSE
    sb(22,2) = "?!!"
  ENDIF
ENDIF         
sb(22,2) = PADR(sb(22,2),LEN(sb(22,1)))
USE

* ���砥� ���ᠭ�� ���㬥��
SELECT TIT_PRINT
USE (base_path+"DOC_FORM")
LOCATE FOR shab_name == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN   && ����� ��࠭��� � ��ப��
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT   && ����: " " - �����, "1" - ᦠ�� (condenced)
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
nn = 0
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
  sum_0_p  = 0   && �㬬� ��� ��� �� ��࠭��
  sum_1_p  = 0   && �㬬� � ��� �� ��࠭��
  sum_11_p = 0   && �㬬� � ��� �� ��������� �� ��࠭��
  sum_12_p = 0   && ����⮪/������� �㬬� � ��� �� ��࠭��
  IF RECCOUNT() # 0
    *
    *  ��⠫�� ��ப� ��।��� ��࠭���
    FOR i = 1 TO n_p_det
      nn = nn+1
      sum_0  = SUM_0
      sum_1  = SUM_P_NDS
      sum_11 = SUM_INV
      sum_12 = SUM_DELTA
      * NEW 30.06.1999
      IF is_narr   && ����� �⮫�殢 � ���-���
*        @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+  ;
*            IIF(is_u,STR(PRICE,10,4),STR(PRICE,10,2))+STR(QNT, 7)+ ;
*              STR(QNT_INV,7)+STR(QNT_DELTA,7)+ ;
*              IIF(is_u, "", TRANSFORM(SUM_P_NDS,"9999999.99"))

        @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+  ;
            +STR(QNT, 7)+STR(QNT_INV,7)+STR(QNT_DELTA,7)

      ELSE
        @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+  ;
            STR(QNT, 7)+  ;
            IIF(.NOT. is_full, "",  ;
              STR(QNT_INV,7)+ ;
              STR(QNT_DELTA,7))
      ENDIF  && END OF NEW 30.06.1999
      sum_0_p  = sum_0_p+sum_0     && �㬬� ��� ��� �� ��࠭��
      sum_1_p  = sum_1_p+sum_1     && �㬬� � ��� �� ��࠭��
      sum_11_p = sum_11_p+sum_11   && �㬬� � ��� �� ��������� �� ��࠭��
      sum_12_p = sum_12_p+sum_12   && ����⮪/������� �㬬� � ��� �� ��࠭��
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  sb(10,2) = IIF(is_u,TRANSFORM(sum_0_p, "9 999 999.9999"),TRANSFORM(sum_0_p, "999 999 999.99"))
  sb(10,2) = PADL(sb(10,2), LEN(sb(10,1)))
  sb(11,2) = IIF(is_u,TRANSFORM(sum_1_p, "9 999 999.9999"),TRANSFORM(sum_1_p, "999 999 999.99"))
  sb(11,2) = PADL(sb(11,2), LEN(sb(11,1)))
  sb(12,2) = IIF(is_u,TRANSFORM(sum_11_p,"9 999 999.9999"),TRANSFORM(sum_11_p, "999 999 999.99"))
  sb(12,2) = PADL(sb(12,2), LEN(sb(12,1)))
  sb(13,2) = IIF(is_u,TRANSFORM(sum_12_p,"9 999 999.9999"),TRANSFORM(sum_12_p, "999 999 999.99"))
  sb(13,2) = PADL(sb(13,2), LEN(sb(13,1)))
  sum_0_d  = sum_0_d+sum_0_p
  sum_1_d  = sum_1_d+sum_1_p
  sum_11_d = sum_11_d+sum_11_p
  sum_12_d = sum_12_d+sum_12_p
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
ENDFOR
sb(14,2) = IIF(is_u,TRANSFORM(sum_0_p, "9 999 999.9999"),TRANSFORM(sum_0_p, "999 999 999.99"))
sb(14,2) = PADL(sb(14,2), LEN(sb(14,1)))
sb(15,2) = IIF(is_u,TRANSFORM(sum_1_p, "9 999 999.9999"),TRANSFORM(sum_1_p, "999 999 999.99"))
sb(15,2) = PADL(sb(15,2), LEN(sb(15,1)))
sb(16,2) = IIF(is_u,TRANSFORM(sum_11_p, "9 999 999.9999"),TRANSFORM(sum_11_p, "999 999 999.99"))
sb(16,2) = PADL(sb(16,2), LEN(sb(16,1)))
sb(17,2) = IIF(is_u,TRANSFORM(sum_12_p, "9 999 999.9999"),TRANSFORM(sum_12_p, "999 999 999.99"))
sb(17,2) = PADL(sb(17,2), LEN(sb(17,1)))
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
SELECT (s_sav)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_Tmp     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    ����⮢�� ᮤ�ন���� ���㬥��.                    �
*�                                                                        �
*����������������������������������������������������������� 01/19/1999 ���
PROCEDURE Prep_Tmp

PRIVATE tag_n, tag_exp, pr_w
SELECT 0
DELETE FILE (tmpo_path+"PRINT.TMP")
CREATE DBF (tmpo_path+"PRINT.TMP") ;
   (CODE      N( 7), ;
    PREFIX    C( 4), ;
    NAME      C(35), ;
    PRODUCER  C( 8), ;
    QNT       N( 6), ;
    PRICE     N( 9,4), ;
    SUM_0     N(12,2), ;
    SUM_P_NDS N(12,2), ;
    QNT_INV   N( 6),   ;
    SUM_INV   N(12,2), ;
    QNT_DELTA N( 6),   ;
    SUM_DELTA N(12,2) )

USE (tmpo_path+"PRINT.TMP") ALIAS TMP_LST EXCLUSIVE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

*
*   �����������
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_PRINT AGAIN
*
*   ��⠫�� 䠩�
SELECT 0
USE (base_path+"ACT_S") ORDER TAG DOC_NUM ALIAS DET_PRINT AGAIN
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

SCAN REST WHILE EVALUATE(tag_exp) = k00   && �� ��ப�� ���㬥��
  IF is_diff .AND. QNT_INC = QNT_EXP
    LOOP
  ENDIF
  IF .NOT. is_full .AND. QNT_INC = 0
    LOOP
  ENDIF

  pr_w = IIF(is_u, PRICE_D, PRICE_R)
  SELECT TMP_LST
  APPEND BLANK
  REPLACE CODE      WITH DET_PRINT.CODE,    ;
          PREFIX    WITH ST_PRINT.PREFIX,   ;
          NAME      WITH ST_PRINT.NAME,     ;
          PRODUCER  WITH ST_PRINT.PRODUCER, ;
          QNT       WITH DET_PRINT.QNT_INC, ;
          PRICE     WITH IIF(is_u,DET_PRINT.PRICE_D,DET_PRINT.PRICE_0), ;
          SUM_0     WITH IIF(is_u,DET_PRINT.PRICE_D*DET_PRINT.QNT_INC,DET_PRINT.PRICE_0*DET_PRINT.QNT_INC), ;
          SUM_P_NDS WITH ROUND(pr_w*DET_PRINT.QNT_INC,2), ;
          QNT_INV   WITH DET_PRINT.QNT_EXP, ;
          SUM_INV   WITH ROUND(pr_w*DET_PRINT.QNT_EXP,2), ;
          QNT_DELTA WITH DET_PRINT.QNT_INC-DET_PRINT.QNT_EXP
  REPLACE SUM_DELTA WITH SUM_P_NDS-SUM_INV
  SELECT DET_PRINT
ENDSCAN
USE

SELECT ST_PRINT
USE

SELECT TMP_LST
COPY TO (tmpo_path+"PRINT_T")
USE
DELETE FILE (tmpo_path+"PRINT.TMP")
USE (tmpo_path+"PRINT_T") ALIAS TMP_LST EXCLUSIVE

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Set_Flg      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      ���塞 ���祭�� 䫠����...                      �
*�                                                                        �
*����������������������������������������������������������� 27.01.1999 ���
PROCEDURE Set_Flg

is_u = TIT_PRINT.VALUTA = "$"
is_corr = "!" $ TIT_PRINT.DOC_NUM
DO CASE
CASE is_corr
  is_full = .F.
  shab_name = IIF(is_u, "ACT_SH!$", "ACT_SH!")
  RETURN .T.
CASE "D" $ flg_w          && CORR 30.06.1999
  is_full = .NOT. EMPTY(TIT_PRINT.EXP_NUM)
  DO Set_Type
  shab_name = "ACT_"+IIF(is_full, "LN", "SH")+IIF(is_u, "$", "")
  RETURN .T.
CASE "F" $ flg_w          && CORR 30.06.1999
  is_full = .T.
  DO Set_Type
  shab_name = "ACT_"+IIF(is_full, "LN", "SH")+IIF(is_u, "$", "")
  RETURN .T.
CASE "S" $ flg_w .OR. "?" $ flg_w       && CORR 30.06.1999
  is_full = .F.
  DO Set_Type
  shab_name = "ACT_"+IIF(is_full, "LN", "SH")+IIF(is_u, "$", "")
  RETURN .T.
CASE "#" $ flg_w           && NEW 30.06.1999
  is_narr = .T.
  is_full = .F.
  DO Set_Type
  shab_name = "ACT_N"
  RETURN .T.               && END OF NEW 30.06.1999
ENDCASE

*is_full = .F.
*shab_name = "ACT_SH"+IIF(is_u, "$", "")

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Set_Type     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ��⠭���� ⨯� ���㬥��.                        �
*�                                                                        �
*����������������������������������������������������������� 26/02/1999 ���
PROCEDURE Set_Type

IF EMPTY(TIT_PRINT.EXP_NUM)
  RETURN
ENDIF

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*��������������������������Ŀ
*�                          �
*�  [ ] ���쪮 ��宦�����  �
*�                          �
*�          < �� >          �
*����������������������������
PRIVATE ex
ex = 1

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 30, "��� ���㬥��"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3,  4 GET is_diff PICTURE "@*C ���쪮 ��宦�����"
@ 5, 12 GET ex PICTURE "@*T \ OK "
READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN