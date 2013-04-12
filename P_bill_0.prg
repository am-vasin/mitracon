*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� P_Bill_0                                                   �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                        ����� ��� ��� ᯨ᪠.                        �
*�                                                                        �
*������������������������������������������������������������� 02.04.2001ͼ
PROCEDURE P_Bill_0
PARAMETERS firm_c, doc_n, doc_d
*
*  firm_c - ��� �।�����;
*  doc_n  - ����� ���㬥��;
*  doc_d  - ���� ��� ���㬥��, ���� ��� � ᨬ���쭮� ��� �᫮��� �ଠ�;
*
PRIVATE s_sav   && ����� ࠡ�祩 ������ ��� ������
PRIVATE d_year  && ��� ���㬥��
PRIVATE d_n     && ����� ���㬥�� � ⥪�⮢�� ����
PRIVATE d_d     && ��� ���㬥�� � ⥪�⮢�� ����
PRIVATE a_code  && Ko� ࠡ�⭨��, ���室����襣� ���죨 
PRIVATE c_name  && ��� ������ (�᫨ ⠪���� ����� ����)
PRIVATE c_code  && ��� ������ (�᫨ ⠪���� ����� ����)
PRIVATE sb      && ���ᨢ ���ᠭ�� �ᥢ�������
PRIVATE t_d     && ���ᨢ � ��ப��� 蠡���� "��������� ���㬥�"
PRIVATE n, j    && ���稪�, �������, ��ࠬ���� 横��
PRIVATE p_ln    && ����� ��࠭��� � ��ப��
PRIVATE fnt     && ����: " " - �����, "1" - ᦠ�� (condenced)
PRIVATE n_cp    && ��᫮ �����
PRIVATE lft     && ���� ᫥�� � ᨬ�����
PRIVATE ffeed   && �����襭�� ���㬥��: �� ����⥫쭮� - �ண�� ��ப,
                &&      ����⥫쭮� - �ண�� �ଠ�
PRIVATE p_drcty && ���ࠢ����� ���� (�ਭ��)
PRIVATE k00     && ���� ��� ���᪠ ���㬥��
PRIVATE is_r    && ��� ��� - �㡫���

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
* �������� ����� ���㬥��
d_n = LEFT(doc_n,4)+"-"+ALLTRIM(RIGHT(doc_n,6))

*
*  ��������� ���㬥�⮢...
SELECT 0
USE (base_path+"BILLS") ORDER TAG DOC_NUM ALIAS TIT_PRINT AGAIN
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
is_r = VALUTA = "�"
a_code = WHO

*
*  �饬 ������...
IF .NOT. EMPTY(CUS_CODE)
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_PRINT AGAIN
  SEEK TIT_PRINT.CUS_CODE
  c_name = ALLTRIM(CUS_NAME)
  c_code = CUS_CODE
  USE
ELSE
  c_name = ALLTRIM(CUS_NAME)
  c_code = CUS_NAME
ENDIF

SELECT TIT_PRINT
d_d = DTOC(DOC_DATE)

*
*   �����⮢�� �६������ 䠩��.
*
*  ��������! �� ��室� �� 䠩�� �� ������ � �ணࠬ�� ���� 䠩��
*            �஬� �६������ � ��������� ������ ���� �������!
*            �६���� 䠩� �� ����� ����� �����ᮢ!
*

DO Wt_Mess WITH "�����⮢�� ᯨ᪠"
DO Prep_Tmp
DO Wt_Mess
SELECT TIT_PRINT

*
*  ����뢠�� �ᥢ������
DIMENSION sb(29,2)

sb( 1,1) = "{Doc_Num  }"
sb( 1,2) = d_n

sb( 2,1) = "{Doc_Date}"
sb( 2,2) = d_d

sb( 3,1) = "{Firm             }"
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS TMP_PRINT AGAIN
SEEK TIT_PRINT.FIRM
sb(25, 1) = "{�㪮����⥫�          }"
sb(25, 2) = ALLTRIM(BOSS)
sb(26, 1) = "{��. ��壠���         }"
sb(26, 2) = ALLTRIM(ABAK)
sb(27, 1) = "{�����                }"
sb(27, 2) = ALLTRIM(KASSIR)

sb(24,1) = "{%NSP}"
sb(24,2) = P_NALOG

sb( 3,2) = ALLTRIM(L_NAME)

sb( 4,1) = "{INN     }"
sb( 4,2) = INN

sb( 5,1) = "{Bank             }"
USE (base_path+"BANKS") ORDER TAG BANK ALIAS TMP_PRINT AGAIN
SEEK TIT_PRINT.FIRM+TIT_PRINT.BANK
sb( 5,2) = ALLTRIM(L_NAME)

sb( 6,1) = "{Acc_Bill}"
sb( 6,2) = ALLTRIM(ACC_NO)

sb( 7,1) = "{Korr_Bill}"
sb( 7,2) = ALLTRIM(CORR_NO)

sb( 8,1) = "{BIK    }"
sb( 8,2) = ALLTRIM(BIK)

sb( 9,1) = "{OKONH}"
sb( 9,2) = ""

sb(10,1) = "{OKPO }"
sb(10,2) = ""
USE

sb(11,1) = "{Customer         }"
sb(11,2) = c_name

sb(12,1) = "{Sum_0    }"
sb(12,2) = ""

sb(13,1) = "{Sum_H    }"
sb(13,2) = ""

sb(14,1) = "{Sum_NDS  }"
sb(14,2) = ALLTRIM(TRANSFORM(TIT_PRINT.BILL_SUM-TIT_PRINT.SUM_0,"999 999 999 999.99"))

sb(15,1) = "{Prc_NDS}"
sb(15,2) = ALLTRIM(STR(TIT_PRINT.NDS_,5,1))

sb(16,1) = "{Sum_Note1                                                         }"

sb(17,1) = "{Sum_Note2                                                         }"

sb(18,1) = "{Sum_Note3                                                         }"

PRIVATE s_note
DIMENSION s_note(3)
s_note(1) = LEN(sb(16,1))
s_note(2) = LEN(sb(18,1))
s_note(3) = LEN(sb(18,1))
DO Nt_Lines WITH TIT_PRINT.BILL_SUM, s_note, TIT_PRINT.VALUTA
sb(16,2) = s_note(1)
sb(17,2) = s_note(2)
sb(18,2) = s_note(3)

sb(20,1) = "{Sum_H_NSP}"
sb(20,2) = ROUND(TIT_PRINT.BILL_SUM*(100+sb(24,2))/100,2)

sb(21,1) = "{Sum_Note1+NSP                                                     }"

sb(22,1) = "{Sum_Note2+NSP                                                     }"

sb(23,1) = "{Sum_Note3+NSP                                                     }"

DIMENSION s_note(3)
s_note(1) = LEN(sb(16,1))
s_note(2) = LEN(sb(18,1))
s_note(3) = LEN(sb(18,1))
DO Nt_Lines WITH sb(20,2), s_note, TIT_PRINT.VALUTA
sb(21,2) = s_note(1)
sb(22,2) = s_note(2)
sb(23,2) = s_note(3)
sb(24,2) = STR(sb(24,2), 6, 1)
sb(20,2) = STR(sb(20,2), 11, 2)

sb(19,1) = "{Autor                       }"
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF a_code = 0
  sb(19,2) = "���!"
ELSE
  IF SEEK (a_code)
    sb(19,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)    
  ELSE
    sb(19,2) = "?!!"
  ENDIF
ENDIF         
sb(19,2) = PADR(sb(19,2),LEN(sb(19,1)))

sb(28,1) = "{��� ������ }"
sb(29,1) = "{���� ������                                               }"
USE (base_path+"CUS_BIL") ORDER TAG CUS_CODE ALIAS CB9903 SHARED AGAIN
IF SEEK(c_code, "CB9903")
  sb(28,2) = INN
  sb(29,2) = ADDRESS
ELSE
  sb(28,2) = ""
  sb(29,2) = ""
ENDIF
USE

* ���砥� ���ᠭ�� ���㬥��
SELECT TIT_PRINT
USE (base_path+"DOC_FORM")
  LOCATE FOR IIF(is_r, "BILL_0", "BILL_0$") == ALLTRIM(UPPER(DOC_NAME))

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
n = MEMLINES(DOC_H)  &&     ��������� ��ࢮ� ��࠭���
IF n > 0
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ENDIF

USE

SELECT TMP_LST
STORE 0 TO sum_d0, sum_d1
IF RECCOUNT() # 0
  SCAN
    sum_d0 = sum_d0 + SUM_0
    sum_d1 = sum_d1 + SUM_T
  ENDSCAN
ENDIF
sb(12,2) = TRANSFORM(sum_d0, "99999999.99")
sb(12,2) = PADL(sb(12,2), LEN(sb(13,1)))
sb(13,2) = TRANSFORM(sum_d1, "99999999.99")
sb(13,2) = PADL(sb(13,2), LEN(sb(14,1)))
str_w = DBF("TMP_LST")

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

PRIVATE n_p_det  && ��᫮ ��⠫��� ��ப �� ��࠭��
PRIVATE str_w    && ��ࠧ ���⠥��� ��ப�
PRIVATE sum_d0, sum_d1 && ...�㬬� �� ���㬥���

*
*  ��������� ���㬥��
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

PRIVATE tag_n, tag_exp
SELECT 0
CREATE DBF (tmpo_path+"PRINT.TMP") ;
   (NAME     C(39), ;
    QNT      N(10), ;
    PRC      N( 9,2), ;
    SUM_0    N(15,2), ;
    SUM_T    N(15,2))
USE (tmpo_path+"PRINT.TMP") ALIAS TMP_LST EXCLUSIVE

*
*   ��⠫�� 䠩�
SELECT 0
USE (base_path+"BILLS_D") ORDER TAG DOC_NUM ALIAS DET_PRINT AGAIN

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
  SELECT TMP_LST
  APPEND BLANK
  REPLACE NAME     WITH DET_PRINT.NAME,   ;
          QNT      WITH DET_PRINT.QNT,    ;
          PRC      WITH DET_PRINT.PRICE_0,;
          SUM_0    WITH DET_PRINT.SUM_0,  ;
          SUM_T    WITH DET_PRINT.SUM_T
  SELECT DET_PRINT
ENDSCAN
USE

SELECT TMP_LST

RETURN