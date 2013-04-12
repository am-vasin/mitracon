*������������������������������������������������������������������������ͻ
*� ���� Del_Inv      ���ࠡ��稪 ���३ ��ᨭ           28.09.98 16:03:50 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                  �������� ��������� ���᪠ � ������.                  �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Del_Inv

PRIVATE n_doc, d_doc, i, n_doc_w, f_code
SET DATE GERMAN
n_doc = SPACE(11)
d_doc = {}
f_code = "   "

DO Fill_Form
IF EMPTY(n_doc)
  RETURN
ENDIF

USE (base_path+"STOCK") ORDER TAG CODE
SELECT 0
USE (base_path+"ACT_S") ORDER TAG PARTY
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE
SELECT 0
USE (base_path+"RYN_L") ORDER TAG C_CODE
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM
n_doc = ALLTRIM(n_doc)
i = AT("-", n_doc)
n_doc_w = f_code+LEFT(DTOS(d_doc),4)+LEFT(n_doc, i-1)+PADL(SUBSTR(n_doc, i+1), 6)
IF .NOT. SEEK(n_doc_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ��������� � ����஬ "+n_doc+" �� "+DTOC(d_doc)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

= SEEK(CUS_CODE,"ACCOUNT")
= SEEK(STR(CUS_CODE)+FIRM,"CUS_REST")
IF .NOT. Show_N()
  CLOSE DATABASES
  RETURN
ENDIF

SELECT CUS_REST
REPLACE ACC_SUM WITH ACCOUNT.ACC_SUM+SALE_TIT.TOTAL_R

SELECT SALE
SEEK n_doc_w
SCAN REST WHILE FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM = n_doc_w

  *  �����頥� �� ᪫��
  = SEEK(CODE,"STOCK")
  = SEEK(PARTY, "ACT_S")
  REPLACE STOCK.QNT_REAL WITH STOCK.QNT_REAL+QNT, ;
          STOCK.QNT_REP  WITH STOCK.QNT_REP+QNT_REP
  REPLACE ACT_S.QNT_REST WITH ACT_S.QNT_REST+QNT, ;
          ACT_S.REST_REP WITH ACT_S.REST_REP+QNT_REP
  * �����頥� �� ॠ������
  SELECT RYN_L
  IF .NOT. SEEK(STR(SALE_TIT.CUS_CODE,5)+STR(SALE.CODE,7))
    APPEND BLANK
    REPLACE CUS_CODE WITH SALE_TIT.CUS_CODE, ;
            CODE     WITH SALE.CODE
  ENDIF
  REPLACE QNT WITH SALE.QNT+QNT
  SELECT SALE
  DELETE

ENDSCAN

SELECT SALE_TIT
DELETE
CLOSE DATABASES

RETURN


*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                          ����뢠�� ���������.                          �
*�                                                                        �
*����������������������������������������������������� 28.09.98 16:27:21 ͼ
PROCEDURE Fill_Form

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

*���������������������������������������Ŀ
*� ��������� N 99999999999 �� ��.��.���� �
*�                                       �
*�   < OK Ctrl-W > < �⪠������ Esc >    �
*�����������������������������������������

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 6, 43, "������, �������!"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 2,  3 SAY "��������� N" GET n_doc
@ 2, 27 SAY "��" GET d_doc PICTURE "@D"
@ 4,  5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 2
  n_doc = SPACE(11)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                        ���⢥ত��� ������...                         �
*�                                                                        �
*����������������������������������������������������� 28.09.98 16:33:22 ͼ
PROCEDURE Show_N

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

*���������������������������������������������������������������Ŀ
*� ��������� N 99999999999 �� ��.��.���� �� �㬬� 999 999 999.99 �
*�    �� ������ ��������������������������������������������Ŀ   �
*�    �  ....:....!....:....!....:....!....:....!....:....!  �   �
*�    ��������������������������������������������������������   �
*�               < OK Ctrl-W > < �⪠������ Esc >                �
*�����������������������������������������������������������������

PRIVATE ex, nm_w     &&
                     &&   ���塞 � ������塞 ���� ������
ex   = 1             &&
nm_w = CUS_NAME      &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 67, "������, ���⢥न..."
DO Sun_Bord WITH 3, 6, 5, 61, " ������ "
@ 2, 3 SAY "��������� N "+PADR(n_doc,11)+" �� "+DTOC(d_doc)+ ;
   " �� �㬬� "+TRANSFORM(SALE_TIT.TOTAL_R,"999 999 999.99")
@ 4, 9 SAY ACCOUNT.CUS_NAME
*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 6, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1