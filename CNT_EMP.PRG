*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Cnt_Emp      ��о�｀砒┴ ��むォ ��瓱�           29.10.97 09:39:56 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�             ��め腑� �竅硅� ゛����� ��┘�皰� か� �����ぎ�.              �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Cnt_Emp
PRIVATE cnt, n, c_s, nm, i

IF (sys_char # "A" .AND. sale_mode # "Mitin")
  USE (base_path+"PLACES")
  LOCATE FOR P_CHAR = sys_char
  nm = P_NAME
  USE (base_path+"ACCOUNT") ORDER TAG EMP_NAME
  SEEK sys_char
  COUNT REST WHILE SUBSYS = sys_char TO cnt
  USE
  DO View_1
ELSE
  n = 0
  USE (base_path+"PLACES")
  SCAN
    n = n+1
    DIMENSION cnt(n), c_s(n), nm(n)
    cnt(n)  = 0
    c_s(n) = P_CHAR
    nm(n)   = P_NAME
  ENDSCAN
  USE (base_path+"ACCOUNT") ORDER TAG EMP_NAME
  FOR i = 1 TO n
    SEEK c_s(i)
    COUNT REST WHILE SUBSYS = c_s(i) TO cnt(i)
  ENDFOR
  USE
  DO View_2
ENDIF

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ ��玻オ罔� ━キ� ��┘���.                   �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE View_1

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                      �
*�   �痰���瘡 999999 ゛����� ��┘�皰�   �
*�                                      �
*�         ��Μ�皀 艪�-�─磴�...        �
*�                                      �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 42, "ç��お�: "+ALLTRIM(nm)

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 2 SAY PADC("�痰���瘡 "+ALLTRIM(STR(cnt))+" ゛����� ��┘�皰�",WCOLS()-4)
@ 5, 2 SAY PADC("��Μ�皀 艪�-�─磴�...",WCOLS()-4)
= INKEY(0, "HM")
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                  蹍������ ��玻オ罔� ━キ� ��┘���.                   �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE View_2

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
PRIVATE i

*   鉑硅� ゛���� ��┘�皰�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                  �
*�  ....:....!....:....!.... 99999  �
*�                                  �
*�     ��Μ�皀 艪�-�─磴�...        �
*�                                  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ALEN(cnt)+7, 38, "鉑硅� ゛���� ��┘�皰�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
FOR i = 1 TO ALEN(nm)
  @ 2+i, 4 SAY PADR(ALLTRIM(nm(i)),25)+STR(cnt(i),5)
ENDFOR
@ WROWS()-3, 2 SAY PADC("��Μ�皀 艪�-�─磴�...", WCOLS()-4)
*--------------------------------------------------------------------------
= INKEY(0,"HM")
POP KEY
RELEASE WINDOW (win_name)
RETURN
