*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� P_Money      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                    ����� ���� �������� ���㬥�⮢.                    �
*�                                                                        �
*����������������������������������������������������������� 25.01.1999 �ͼ
PROCEDURE P_Money
PARAMETERS firm_c, doc_t, doc_n, doc_d  &&, flgs
*
*  firm_c - ��� �।�����;
*  doc_t  - ⨯ ���㬥��;
*  doc_n  - ����� ���㬥��;
*  doc_d  - ���� ��� ���㬥��, ���� ��� � ᨬ���쭮� ��� �᫮��� �ଠ�;
*  flg    - 䫠���.
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
PRIVATE t_p     && ���ᨢ � ��ப��� 蠡���� "��������� ��࠭���"
PRIVATE f_d     && ���ᨢ � ��ப��� 蠡���� "������ ���㬥�"
PRIVATE f_p     && ���ᨢ � ��ப��� 蠡���� "������ ��࠭���"
PRIVATE n, j, i && ���稪�, �������, ��ࠬ���� 横��
PRIVATE p_ln    && ����� ��࠭��� � ��ப��
PRIVATE fnt     && ����: " " - �����, "1" - ᦠ�� (condenced)
PRIVATE n_cp    && ��᫮ �����
PRIVATE lft     && ���� ᫥�� � ᨬ�����
PRIVATE ffeed   && �����襭�� ���㬥��: �� ����⥫쭮� - �ண�� ��ப,
                &&      ����⥫쭮� - �ண�� �ଠ�
PRIVATE p_drcty && ���ࠢ����� ���� (�ਭ��)
PRIVATE k00     && ���� ��� ���᪠ ���㬥��
PRIVATE div_arr && ��� ࠧ������ �� ��ப�
PRIVATE tmp_val
PRIVATE psp_no, psp_sr, psp_l1, psp_l2, psp_l3, face_nam
PRIVATE tmpDocType, tmpDocNum

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
*  ��������� ���㬥�⮢...
SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM ALIAS TIT_PRINT AGAIN
k00 = firm_c+doc_t+d_year+doc_n
*
*  �饬...
IF .NOT. SEEK(k00)  && �� ��諨...
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ���㬥�� � ����஬ "+LEFT(doc_n,1)+ALLTRIM(RIGHT(doc_n,9))
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
face_nam = CONTACT
psp_no  = PSP_SER
psp_sr  = PSP_NUM
psp_l1  = PSP_WH1
psp_l2  = PSP_WH2
psp_l3  = PSP_WH3
tmpDocType = DOC_TYPE
tmpDocNum  = DOC_NUM
IF LEFT(ENTER_DOC, 1) = CHR(0)
  tmpDocType = " "
  tmpDocNum  = SUBSTR(ENTER_DOC, 2, 10)
ENDIF
dep_nam = CUS_NAME
IF (EMPTY(face_nam) .OR. EMPTY(psp_no)) .AND. INLIST(tmpDocType, " ", "2");
                    .AND. MONEY_R < 0
  DO G_Contact
ENDIF
*
* �������� ����� ���㬥��
*IF tmpDocType = " " .AND. "0" $ firm_c
*  d_n = VAL(ALLTRIM(SUBSTR(doc_n,2)))
*  d_n = LEFT(doc_n,1)+ALLTRIM(STR(IIF(d_n > 500, 200+(d_n-200)%300, d_n)))
*ELSE
  d_n = LEFT(tmpDocNum,1)+ALLTRIM(RIGHT(tmpDocNum,6))
*ENDIF
d_d = DTOC(DOC_DATE)
a_code = WHO

*
*  �饬 ������...
c_name = CUS_NAME 
IF .NOT. EMPTY(CUS_CODE)
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_NNNN AGAIN
  SEEK TIT_PRINT.CUS_CODE
  c_name = ALLTRIM(CUS_NAME)
  c_code = CUS_CODE
  USE
ENDIF
SELECT TIT_PRINT

*
*  ����뢠�� �ᥢ������
DIMENSION sb(50,2)

sb( 1,1) = "{N ���-� }"
sb( 1,2) = PADR(d_n, LEN(sb( 1,1)))

sb( 2,1) = "{���    }"
sb( 2,2) = d_d

tmp_val = ""
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS TMP_9125 AGAIN
sb(48, 1) = "{�㪮����⥫�          }"
sb(48, 2) = ""
sb(49, 1) = "{��. ��壠���         }"
sb(49, 2) = ""
sb(50, 1) = "{�����                }"
sb(50, 2) = ""
IF SEEK(firm_c)
  tmp_val = L_NAME
  sb(48, 2) = BOSS
  sb(49, 2) = ABAK
  sb(50, 2) = KASSIR
ENDIF

sb( 3,1) = "{�।���⨥                                   }"
sb( 3,2) = PADR(ALLTRIM(tmp_val), LEN(sb(3,1)))

sb( 4,1) = "{�।���⨥ ��ப� 1     }"
sb( 5,1) = "{�।���⨥ ��ப� 2     }"

DIMENSION div_arr(2)
div_arr(1) = LEN(sb(4,1))
div_arr(2) = LEN(sb(5,1))
DO Div_Line
sb( 4,2) = div_arr(1)
sb( 5,2) = div_arr(2)

sb( 6,1) = "{���� �।�����                               }"
sb( 6,2) = ADDRESS

sb( 7,1) = "{����䮭   }"
sb( 7,2) = PHONE

sb( 8,1) = "{���     }"
sb( 8,2) = INN

tmp_val = ""
USE (base_path+"BANKS") ORDER TAG BANK ALIAS TMP_9125 AGAIN
IF SEEK(firm_c+TIT_PRINT.BANK)
  tmp_val = L_NAME
ENDIF
sb( 9,1) = "{����                                            }"
sb( 9,2) = PADR(ALLTRIM(tmp_val), LEN(sb(3,1)))

sb(10,1) = "{���� ��ப� 1            }"
sb(11,1) = "{���� ��ப� 2            }"

DIMENSION div_arr(2)
div_arr(1) = LEN(sb(10,1))
div_arr(2) = LEN(sb(11,1))
DO Div_Line
sb(10,2) = div_arr(1)
sb(11,2) = div_arr(2)

sb(12,1) = "{������ ���    }"
sb(12,2) = PADR(ACC_NO, LEN(sb(12,1)))

sb(13,1) = "{����. ���        }"
sb(13,2) = PADR(CORR_NO, LEN(sb(13,1)))

sb(14,1) = "{���    }"
sb(14,2) = PADR(BIK, LEN(sb(14,1)))

USE

sb(15,1) = "{������                                          }"
sb(15,2) = PADR(c_name,50)

sb(16,1) = "{������ 1/1    }"
sb(17,1) = "{������ 1/2               }"
sb(18,1) = "{������ 1/3               }"

tmp_val = sb(15,2)
DIMENSION div_arr(3)
div_arr(1) = LEN(sb(16,1))
div_arr(2) = LEN(sb(17,1))
div_arr(3) = LEN(sb(18,1))
DO Div_Line
sb(16,2) = div_arr(1)
sb(17,2) = div_arr(2)
sb(18,2) = div_arr(3)

sb(19,1) = "{������ 2/1                          }"
sb(20,1) = "{������ 2/2                          }"

tmp_val = sb(15,2)
DIMENSION div_arr(2)
div_arr(1) = LEN(sb(19,1))
div_arr(2) = LEN(sb(20,1))
DO Div_Line
sb(19,2) = div_arr(1)
sb(20,2) = div_arr(2)

SELECT TIT_PRINT
sb(21,1) = "{��१ ����          }"
sb(21,2) = PADR(TIT_PRINT.CONTACT, LEN(sb(21,1)))

sb(22,1) = "{N ����.  }"
sb(22,2) = IIF(EMPTY(SALE_NUM), "", LEFT(SALE_NUM,4)+"-"+ALLTRIM(RIGHT(SALE_NUM,6)))
sb(22,2) = PADR(sb(22,2), LEN(sb(22,1)))

sb(23,1) = "{��� �. }"
sb(23,2) = IIF(EMPTY(SALE_DATE), "          ", DTOC(SALE_DATE))

sb(24,1) = "{N ���. }"
sb(24,2) = IIF(EMPTY(BILL_NUM), "", LEFT(BILL_NUM,4)+"-"+ALLTRIM(RIGHT(BILL_NUM,6)))
sb(24,2) = PADR(sb(24,2), LEN(sb(24,1)))

sb(25,1) = "{��� ��.}"
sb(25,2) = IIF(EMPTY(BILL_DAT), "          ", DTOC(BILL_DAT))

sb(26,1) = "{N ���⥦�     }"
sb(26,2) = ENTER_DOC
sb(26,2) = PADR(sb(26,2), LEN(sb(26,1)))

sb(27,1) = "{��� ��.}"
sb(27,2) = IIF(EMPTY(ENTER_DAT), "          ", DTOC(ENTER_DAT))

sb(28,1) = "{�㬬� ��.  }"
sb(28,2) = TRANSFORM(IIF(tmpDocType = " ", ABS(MONEY_R+N_OF_SALE), MONEY_R+N_OF_SALE), "999 999 999.99")

sb(29,1) = "{�㬬� USD   }"
sb(29,2) = TRANSFORM(IIF(tmpDocType = " ", ABS(MONEY_D), MONEY_D), "999 999 999.99")

sb(30,1) = "{%��}"
sb(30,2) = TRANSFORM(NL_PRC, "999.9")

sb(31,1) = "{�����       }"
sb(31,2) = TRANSFORM(IIF(tmpDocType = " ", ABS(N_OF_SALE), N_OF_SALE), "999 999 999.99")

sb(32,1) = "{%��}"
sb(32,2) = TRANSFORM(NDS_, "999.9")

sb(33,1) = "{���         }"
DO CASE
CASE tmpDocType $ " �"
  sb(33,2) = TRANSFORM(ABS(MONEY_R-MONEY_0), "999 999 999.99")
CASE tmpDocType = "1"
  sb(33,2) = TRANSFORM(MONEY_R-MONEY_0, "999 999 999.99")
OTHERWISE
  sb(33,2) = TRANSFORM(MONEY_D-MONEY_0, "999 999 999.99")
ENDCASE

sb(34,1) = "{�㬬� �ய���� ��ப� 1/1                     }"
sb(35,1) = "{�㬬� �ய���� ��ப� 1/2                     }"
sb(36,1) = "{�㬬� �ய���� ��ப� 1/3                     }"

tmp_val = ABS(IIF(tmpDocType $ "34", MONEY_D+N_OF_SALE, MONEY_R+N_OF_SALE))
tmp_val = Num2RStr(FLOOR(tmp_val))+IIF(tmpDocType $ "34", " � � ", " �� ")+ ;
          RIGHT(STR(100+(tmp_val-FLOOR(tmp_val))*100),2)+IIF(tmpDocType $ "34", "", " ���.")
tmp_val = Capital(tmp_val)
DIMENSION div_arr(3)
div_arr(1) = LEN(sb(34,1))
div_arr(2) = LEN(sb(35,1))
div_arr(3) = LEN(sb(36,1))
DO Div_Line
sb(34,2) = div_arr(1)
sb(35,2) = div_arr(2)
sb(36,2) = div_arr(3)

sb(37,1) = "{�㬬� �ய���� ��ப� 2/1}"
sb(38,1) = "{�㬬� �ய���� ��ப� 2/2}"
sb(39,1) = "{�㬬� �ய���� ��ப� 2/3}"
sb(40,1) = "{�㬬� �ய���� ��ப� 2/4}"
sb(41,1) = "{�㬬� �ய���� ��ப� 2/5}"

tmp_val = ABS(IIF(tmpDocType $ "34", MONEY_D+N_OF_SALE, MONEY_R+N_OF_SALE))
tmp_val = Num2RStr(FLOOR(tmp_val))+IIF(tmpDocType $ "34", " � � ", " �� ")+ ;
          RIGHT(STR(100+(tmp_val-FLOOR(tmp_val))*100),2)+IIF(tmpDocType $ "34", "", " ���.")
tmp_val = Capital(tmp_val)
DIMENSION div_arr(5)
div_arr(1) = LEN(sb(37,1))
div_arr(2) = LEN(sb(38,1))
div_arr(3) = LEN(sb(39,1))
div_arr(4) = LEN(sb(40,1))
div_arr(5) = LEN(sb(41,1))
DO Div_Line
sb(37,2) = div_arr(1)
sb(38,2) = div_arr(2)
sb(39,2) = div_arr(3)
sb(40,2) = div_arr(4)
sb(41,2) = div_arr(5)

*
* ���砥� ���ᠭ�� ���㬥��
DO CASE
CASE tmpDocType = " "
  tmp_val = IIF("-" $ tmpDocNum, "EXP_ORDER", "INC_ORDER")
CASE tmpDocType = "�"
  tmp_val = "CHECK"
CASE tmpDocType = "�"
  tmp_val = "SBERBANK"
CASE tmpDocType = "1"
  tmp_val = "BN_MONEY"
CASE tmpDocType = "3"
  tmp_val = "USD_MONEY"
CASE tmpDocType = "4"
  tmp_val = "USD_BN"
ENDCASE
*------------------------------------
sb(42,1) = "{Autor                       }"

sb(43, 1) = "{��� N}"
sb(43, 2) = psp_no

sb(44, 1) = "{���}"
sb(44, 2) = psp_sr

sb(45, 1) = "{��ᯮ�� �뤠� 1                       }"
sb(45, 2) = psp_l1

sb(46, 1) = "{��ᯮ�� �뤠� 2                       }"
sb(46, 2) = psp_l2

sb(47, 1) = "{��ᯮ�� �뤠� 3                       }"
sb(47, 2) = psp_l3

USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF a_code = 0
  sb(42,2) = "���!"
ELSE
  IF SEEK (a_code)
    sb(42,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)    
  ELSE
    sb(42,2) = "?!!"
  ENDIF
ENDIF         
sb(42,2) = PADR(sb(42,2),LEN(sb(42,1)))
*--------------------------------------
USE (base_path+"DOC_FORM")
LOCATE FOR tmp_val == ALLTRIM(UPPER(DOC_NAME))

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

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

PRIVATE str_w    && ��ࠧ ���⠥��� ��ப�

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

SELECT (s_sav)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Div_Line     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          ��������� ��ப�...                           �
*�                                                                        �
*����������������������������������������������������������� 25.01.1999 ���
PROCEDURE Div_Line

PRIVATE s_line, f_line, r_line, n_line, j

n_line = ALEN(div_arr)
s_line = ALLTRIM(tmp_val)
FOR j = 1 TO n_line
  f_line = ""
  r_line = ""
  DO Break_Str WITH s_line, f_line, r_line, div_arr(j)
  div_arr(j) = PADR(ALLTRIM(f_line), div_arr(j))
  s_line = r_line
ENDFOR

RETURN

*************************************************************************
PROCEDURE G_Contact

PRIVATE ln, wd, ex, mss, c1

IF .NOT. EMPTY(face_nam) .AND. .NOT. EMPTY(psp_no)
  RETURN
ENDIF

DO Pasport WITH CUS_CODE, dep_nam, face_nam, psp_sr, psp_no, psp_l1, psp_l2, psp_l3

PRIVATE s_sav

REPLACE CONTACT  WITH face_nam, ; 
        CUS_NAME WITH dep_nam,  ;
        PSP_NUM  WITH psp_sr,   ;
        PSP_SER  WITH psp_no,   ;
        PSP_WH1  WITH psp_l1,   ;
        PSP_WH2  WITH psp_l2,   ;
        PSP_WH3  WITH psp_l3

IF .NOT. EMPTY(SALE_NUM)
  s_sav = SELECT()
  SELECT 0
  USE SALE_TIT ORDER TAG DOC_NUM ALIAS S12345 AGAIN
  IF SEEK(LEFT(DTOS(TIT_PRINT.SALE_DATE),4)+TIT_PRINT.SALE_NUM)
    REPLACE CONTACT WITH face_nam, CUS_NAME WITH dep_nam
  ENDIF
  USE
  SELECT (s_sav)
ENDIF

RETURN .T.