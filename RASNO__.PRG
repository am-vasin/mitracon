*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� RASNO__      ��о�｀砒┴ ���矗┘���                        �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                    ����� か� �����き�� ��Л����瓱�                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 21.10.1999 様�
PROCEDURE RASNO__

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
PRIVATE cod_0, nam_0, qnt_0, s_sav, p
p = 1
qnt_0 = 0
s_sav = SELECT()

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (base_path+"S_RIGHTS") ORDER TAG RIGHT

SELECT 0
USE (base_path+"USERS") ORDER TAG USER_NAME
SET RELATION TO S_RIGHTS INTO S_RIGHTS 
SET RELATION TO USER_CODE INTO PERSONS ADDITIVE

SCAN FOR S_RIGHTS.R_FLAGS < "2" 
  qnt_0 = qnt_0 + 1
  DIMENSION cod_0(qnt_0), nam_0(qnt_0)
  cod_0(qnt_0) = USER_CODE
  nam_0(qnt_0) = ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)
ENDSCAN

USE 

SELECT S_RIGHTS
USE 

SELECT PERSONS
USE 

SELECT (s_sav)
IF qnt_0 = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = " "
  mss(2) = chr(0)+"�モ 皰��, �皰 ��оヨ�� �ムイ�艫 ぅ���ォ!"
  mss(3) = " "
  DO OUT_MESS WITH 7, "mss"
  RETURN .F.
ENDIF
*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�    踳く爬閧┘ AAA AAAAAAAAAAAAAAAA (F1)    �
*�              敖陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �皰 ��оヨ┼ �AAAAAAAAAAAAAAAAAAAAAAAAAAAA� �
*�              青陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�         踸�ョ��┘ AAAAAAAAAAAAAAAA         � 
*�                                             �
*�            [ ] �ョ�碎 �����き��             �
*�                                             �
*�    < OK Ctrl-W > < �皖���碎瘴 Esc >         �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
PRIVATE ex
ex = 1
*------------------------------------------------------------------------
PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 12, 49, "�������皀 ゛���"
@ 4,3 SAY "�皰 ��оヨ┼"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO Get_Firm WITH 2, 6, enab_firm
@ 3, 16 GET p PICTURE "@^" SIZE 1,30 FROM nam_0
@ 6, 11 SAY "踸�ョ��┘" GET nt
@ 8, 14 GET to_print PICTURE "@*C �ョ�碎 �����き��"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_F()
w_l = cod_0(p)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1
