*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Get_List     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                �〓� ��゛��� か� ���瓷� �� ���ム� 甎�瓷�.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 27.02.2006 様�
PROCEDURE Get_List
PARAMETERS sValue

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
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                 �
*�       ���ム 甎�瓷� ....:.       �
*�                                 �
*�< OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, tmpPatt
ex = 1
tmpPatt = IIF(TYPE("sValue") = "N", sValue, 0)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 40, "���瓷 �� ���ム� 甎�瓷�"
*DO Sun_Bord WITH  2,  2,  4, 49, " ������〓� ����� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 3,  9 SAY "���ム 甎�瓷�" GET tmpPatt PICTURE "@Z 999999"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex # 1
  *
  * �矗���硅��ガ ゛���
  tmpPatt = 0
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN tmpPatt
