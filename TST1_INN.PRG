*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Tst1_INN     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                    �஢�ઠ �� ����稥 �㡫�� ���.                     �
*�                                                                        �
*����������������������������������������������������������� 14.05.2006 �ͼ
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� View_Dbl     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                 �����뢠�� �㡫� � ��訢��� ��� ����                 �
*�                                                                        �
*����������������������������������������������������������� 14.05.2006 ���
PROCEDURE View_Dbl
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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*�������������������������������������������������Ŀ
*� �� ��������� ࠬ�� ��������������������������Ŀ �
*� �                                             � �
*� ����������������������������������������������� �
*�                                                 �
*�        < OK Ctrl-W > < �⪠������ Esc >         �
*���������������������������������������������������

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH MIN(RECCOUNT("TMP_LIST")+6, 20), 68, "�஢�ઠ ���"
SELECT TMP_LIST
GO TOP

*------------------------------------------------------------------------
*      ���� ����� ������
*
@  2,  3 SAY "��� ������ "+ALLTRIM(m.tmpCName)
@  3,  3 SAY "�������� ������� � ⠪�� �� ���:"
SCAN NEXT 14
  @ ROW()+1, 3 SAY CUS_NAME
ENDSCAN
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

*IF m.ex = 1
  *
  * ��ࠡ��뢠�� �����
  
*ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.ex = 1