*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�               �襦�� ���皀Ν��� ぎ�祠キ�� か� 磴��キ��.                �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE D_Money

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

*   �皖琺��ガ ����� ��
PRIVATE ex, tmpDoc, tmpFirm, tmpType, tmpCode, tmpYear, sel_sav
PRIVATE tmpName, tmpReason, nFirms, t_list, tmp1Doc, t_name
tmpYear = YEAR(DATE())
tmpReason = SPACE(48)
DIMENSION t_list(7), t_name(7)
t_list(1) = " "
t_name(1) = "踸絎き覃 �爐ム "

t_list(2) = " "
t_name(2) = "��痳�き覃 �爐ム "

t_list(3) = "1"
t_name(3) = "�/� ���皀� "

t_list(4) = "3"
t_name(4) = "��ぅ�瓱珮ガ覃 ��."

t_list(5) = "�"
t_name(5) = "�オ      "

t_list(6) = "�"
t_name(6) = "�．燹��� "

t_list(7) = "4"
t_name(7) = "��お�. �/� "

sel_sav = SELECT()

SELECT 0
USE (base_path+"FIRMS") ORDER TAG B_NAME ALIAS D_MONEY AGAIN
nFirms = 0
tmpFirm = 1
tmpType = 1
tmpDoc  = 0
SCAN
  nFirms = nFirms+1
  DIMENSION tmpCode(nFirms), tmpName(nFirms)
  tmpCode(nFirms) = FIRM_CODE
  tmpName(nFirms) = FIRM_CODE+" "+B_NAME
ENDSCAN
USE
SELECT (sel_sav)
IF nFirms = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き��� �爛く爬閧��"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

*    ����キ┘ ���皀Ν��� ぎ�祠キ��
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� ��� 9999  ���ム ぎ�祠キ�� (皰�讓� 罔籥���� ��痰�) 999999999 �
*�       敖陳陳陳陳陳陳陳陳陳�             敖陳陳陳陳陳陳陳陳� �
*� ��爼� �                   � �┓ ���皀�� �                 � �
*�       青陳陳陳陳陳陳陳陳陳�             青陳陳陳陳陳陳陳陳� �
*� 踸�ョ��┘ ....:....!....:....!....:....!....:....!....:... �
*�              < OK Ctrl-W > < �皖���碎瘴 Esc >               �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 65, "����キ┘ ���皀Ν��� ぎ�祠キ��"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@  2,  3 SAY "���" GET tmpYear PICTURE "9999"
@  2, 13 SAY "���ム ぎ�祠キ�� (皰�讓� 罔籥���� ��痰�)" GET tmpDoc PICTURE "@Z 999999999"
@  4,  3 SAY "��爼�"
@  4, 31 SAY "�┓ ���皀��"
@  3,  9 GET tmpFirm  PICTURE "@^" FROM tmpName SIZE 1, 19
@  3, 43 GET tmpType  PICTURE "@^" FROM t_name  SIZE 1, 17
@  6,  3 SAY "踸�ョ��┘" GET tmpReason
@  7, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  DO S_Money
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                         蹍甃�矗 ぎ�祠キ��.                           �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE S_Money

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

*   �皖琺��ガ ����� ��
PRIVATE ex, sel_sav, mss

sel_sav = SELECT()

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM ALIAS M_MONEY AGAIN
IF tmpType = 2
  tmp1Doc = STR(-tmpDoc, 9)
ELSE
  tmp1Doc = STR(tmpDoc, 9)
ENDIF
tmp1Doc = sys_char+tmp1Doc
IF .NOT. SEEK(tmpCode(tmpFirm)+t_list(tmpType)+STR(tmpYear,4)+tmp1Doc)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���祠キ� �� ���ぅ�"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_sav)
  RETURN
ENDIF

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS C_MONEY AGAIN
SEEK M_MONEY.CUS_CODE

SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_MONEY AGAIN
SEEK M_MONEY.FIRM


*    ����キ┘ ���皀Ν��� ぎ�祠キ��
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� ���祠キ� � A999999999 �� 99.99.9999                       �
*�                                                           �
*� �┓ ぎ�祠キ��:                                            �
*�                                                           �
*� ��┘�� ....:....!....:....!....:....!....:....!....:....! �
*�                                                           �
*� �祠�� 999 999 999.99�    999 999 999.99$                  �
*�                                                           �
*�           < �����碎 Ctrl-W > < �皖���碎瘴 Esc >           �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 12, 63, "����キ┘ ���皀Ν��� ぎ�祠キ��"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@  2,  3 SAY "���祠キ� � "+LEFT(M_MONEY.DOC_NUM, 1)+ALLTRIM(SUBSTR(M_MONEY.DOC_NUM, 2))
@  2, 25 SAY "�� "+DTOC(M_MONEY.DOC_DATE)
@  4,  3 SAY "�┓ ぎ�祠キ��: "+t_name(tmpType)
@  6,  3 SAY "��┘�� "+ALLTRIM(C_MONEY.CUS_NAME)
@  8,  3 SAY "�祠�� "+TRANSFORM(M_MONEY.MONEY_R, "999 999 999.99")+"�    "+ ;
             TRANSFORM(M_MONEY.MONEY_D, "999 999 999.99")+"$"
@ 10, 13 GET ex PICTURE "@*HT \! �����碎 Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  DO Del_Mon WITH tmpCode(tmpFirm), t_list(tmpType), tmp1Doc, tmpYear, tmpReason
ENDIF
*--------------------------------------------------------------------------

USE IN C_MONEY
USE IN F_MONEY
USE IN M_MONEY
SELECT (sel_sav)
POP KEY
RELEASE WINDOW (win_name)

RETURN
