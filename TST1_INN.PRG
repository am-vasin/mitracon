*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Tst1_INN     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                    蹍▲爲� �� ����腮� ゃ゛ォ ���.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 14.05.2006 様�
PROCEDURE Tst1_INN
PARAMETERS prmCus

PRIVATE tmpRet, s_sav, tmpName, tmpCount, tmpCName, tmpINN

m.tmpRet = .T.
m.tmpCount = 0

IF EMPTY(m.prmCus)
  RETURN m.tmpRet
ENDIF

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS ACC_6514
SEEK m.prmCus
m.tmpCName = ALLTRIM(CUS_NAME)
SELECT 0
USE (m.base_path+"CUS_BIL") SHARED AGAIN ORDER TAG CUS_CODE ALIAS BIL_6514
SEEK m.prmCus
IF EMPTY(INN)
  USE IN ACC_6514
  USE IN BIL_6514
  SELECT (m.s_sav)
  RETURN m.tmpRet
ENDIF
SELECT 0
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	CUS_NAME	C(50) )
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_LIST
SELECT BIL_6514
m.tmpINN = INN
SET ORDER TO TAG INN
SEEK m.tmpINN
SCAN REST WHILE INN = m.tmpINN FOR CUS_CODE # m.prmCus
  = SEEK(CUS_CODE, "ACC_6514")
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE CUS_NAME WITH ACC_6514.CUS_NAME
  SELECT BIL_6514
ENDSCAN

USE IN ACC_6514
USE IN BIL_6514
IF RECCOUNT("TMP_LIST") = 0
  USE IN TMP_LIST
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  SELECT (m.s_sav)
  RETURN m.tmpRet
ENDIF

m.tmpRet = View_Dbl()

USE IN TMP_LIST
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
SELECT (m.s_sav)

RETURN m.tmpRet

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_Dbl     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                 ����щ��ガ ゃ゛� � 甎��荐��ガ ��� °碎                 �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 14.05.2006 陳�
PROCEDURE View_Dbl
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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖 ������〓� ����� 陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �                                             � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�                                                 �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >         �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH MIN(RECCOUNT("TMP_LIST")+6, 20), 68, "蹍▲爲� ���"
SELECT TMP_LIST
GO TOP

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  2,  3 SAY "��� ��┘��� "+ALLTRIM(m.tmpCName)
@  3,  3 SAY "痺薀痰≪鈞 ��┘�硅 � ���━ Ε ���:"
SCAN NEXT 14
  @ ROW()+1, 3 SAY CUS_NAME
ENDSCAN
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*IF m.ex = 1
  *
  * �矗���硅��ガ ゛���
  
*ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.ex = 1
