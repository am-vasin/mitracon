*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Ch_Arm       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                         �襦�� ��｀腑�� �メ��.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 03.01.2004 様�
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*楽陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖                                崖
*崖                                崖
*崖                                崖
*崖                                崖
*崖                                崖
*崖                                崖
*崖                                崖
*崖                                崖
*崖                                崖
*崖                                崖
*崖                                崖
*崖                                崖
*崖                                崖
*崖                                崖
*崖                                崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH WROWS()-8, 38, "�襦�� ��｀腑�� �メ��"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2,  2 GET tmpPoint FROM n_arm SIZE WROWS()-4, WCOLS()-4 FUNCTION "&N"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

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
