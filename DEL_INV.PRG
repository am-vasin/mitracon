*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Del_Inv      ��о�｀砒┴ ��むォ ��瓱�           28.09.98 16:03:50 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                  ����キ┘ �����き�� �皴竅�� � ������.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
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
  mss(2) = CHR(0)+"�モ �����き�� � ���ム�� "+n_doc+" �� "+DTOC(d_doc)+"!"
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

  *  ��Б����ガ �� 瓷���
  = SEEK(CODE,"STOCK")
  = SEEK(PARTY, "ACT_S")
  REPLACE STOCK.QNT_REAL WITH STOCK.QNT_REAL+QNT, ;
          STOCK.QNT_REP  WITH STOCK.QNT_REP+QNT_REP
  REPLACE ACT_S.QNT_REST WITH ACT_S.QNT_REST+QNT, ;
          ACT_S.REST_REP WITH ACT_S.REST_REP+QNT_REP
  * ��Б����ガ �� 爛��├�罔�
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


*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                          ���щ��ガ �����き竡.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 28.09.98 16:27:21 夕
PROCEDURE Fill_Form

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� �����き�� N 99999999999 �� ��.��.���� �
*�                                       �
*�   < OK Ctrl-W > < �皖���碎瘴 Esc >    �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 6, 43, "��む鈬�, �������!"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2,  3 SAY "�����き�� N" GET n_doc
@ 2, 27 SAY "��" GET d_doc PICTURE "@D"
@ 4,  5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 2
  n_doc = SPACE(11)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                        ��も▲爨��ガ 〓Б���...                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 28.09.98 16:33:22 夕
PROCEDURE Show_N

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� �����き�� N 99999999999 �� ��.��.���� �� 痺��� 999 999 999.99 �
*�    敖 ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕   �
*�    �  ....:....!....:....!....:....!....:....!....:....!  �   �
*�    青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰   �
*�               < OK Ctrl-W > < �皖���碎瘴 Esc >                �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, nm_w     &&
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&
nm_w = CUS_NAME      &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 67, "��む鈬�, ��も▲爐�..."
DO Sun_Bord WITH 3, 6, 5, 61, " ��┘�� "
@ 2, 3 SAY "�����き�� N "+PADR(n_doc,11)+" �� "+DTOC(d_doc)+ ;
   " �� 痺��� "+TRANSFORM(SALE_TIT.TOTAL_R,"999 999 999.99")
@ 4, 9 SAY ACCOUNT.CUS_NAME
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 6, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1
