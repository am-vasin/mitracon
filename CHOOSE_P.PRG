*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Choose_P     ��о�｀砒┴ ��むォ ��瓱�           02.09.97 15:06:16 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       蹍������ �襦��� �����お�.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Choose_P

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �                                             � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�                                                 �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >         �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE p_n, p_c, p_num, point
*------------------------------------------------------------------------
point = SELECT()

SELECT 0
USE (base_path+"PLACES") ALIAS P_WWWWW AGAIN
p_num = 1
DIMENSION p_n(p_num), p_c(p_num)
p_n(1) = PADR("�瓮 �����お�", FSIZE("P_NAME"))
p_c(1) = ""
SCAN
  p_num = p_num+1
  DIMENSION p_n(p_num), p_c(p_num)
  p_n(p_num) = P_NAME
  p_c(p_num) = P_CHAR
ENDSCAN

USE
SELECT (point)

IF p_num = 2
  RETURN ""
ENDIF

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH ALEN(p_n)+3, LEN(p_n(1))+4, "ç��お�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
point = 1
DO WHILE .T.
  point = MIN(point,ALEN(p_n))
  point = MAX(point,1)
  @ 1, 1 MENU p_n, ALEN(p_n)
  READ MENU TO point
  IF READKEY() % 256 = 15
    EXIT
  ENDIF
ENDDO

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN p_c(point)
