*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Ch_Arm       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                         �롮� ࠡ�祣� ����.                          �
*�                                                                        �
*����������������������������������������������������������� 03.01.2004 �ͼ
PROCEDURE Ch_Arm
PARAMETERS tmpCode, tmpArm

PRIVATE c_arm, n_arm, cnt, tmpPoint, s_sav
cnt = 0
s_sav = SELECT()
SELECT 0
tmpPoint = 1
USE (base_path+"ARM_LST") SHARED AGAIN ORDER TAG NAME
SCAN
  cnt = cnt+1
  DIMENSION c_arm(cnt), n_arm(cnt)
  c_arm(cnt) = ARM_C
  n_arm(cnt) = ARM_N
  IF ARM_C == tmpCode
    tmpPoint = cnt
  ENDIF
ENDSCAN

USE
SELECT (s_sav)

IF EMPTY(cnt)
  tmpCode = ""
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

*����������������������������������Ŀ
*���������������������������������Ŀ�
*��                                ��
*��                                ��
*��                                ��
*��                                ��
*��                                ��
*��                                ��
*��                                ��
*��                                ��
*��                                ��
*��                                ��
*��                                ��
*��                                ��
*��                                ��
*��                                ��
*��                                ��
*����������������������������������ٳ
*� < OK Ctrl-W > < �⪠������ Esc > �
*������������������������������������

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH WROWS()-8, 38, "�롮� ࠡ�祣� ����"
*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 2,  2 GET tmpPoint FROM n_arm SIZE WROWS()-4, WCOLS()-4 FUNCTION "&N"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
IF ex=1
  tmpCode = c_arm(tmpPoint)
  IF TYPE("tmpArm") = "C"
    tmpArm = n_arm(tmpPoint)
  ENDIF
ELSE
  tmpCode = "   "
ENDIF

RETURN