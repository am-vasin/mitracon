*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              ���竅皋�襯 皋�� ���皀Ε� か� �爛く爬閧��.                �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Let_Pay
PARAMETER f_code      && ��� �爛く爬閧��

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
PRIVATE ex, tmp_list, p_n, p_bn, p_ue, p_c, p_sb, p_bue
sel_sav = SELECT()

SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS L_PAY AGAIN
SEEK f_code
p_n   = .NOT. "�" $ H_TYPES
p_bn  = .NOT. "1" $ H_TYPES
p_ue  = .NOT. "3" $ H_TYPES
p_c   = .NOT. "�" $ H_TYPES
p_sb  = .NOT. "�" $ H_TYPES
p_bue = .NOT. "4" $ H_TYPES

*    ���竅皋�襯 皋�� ���皀Ε� か� �爛く爬閧��
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖 ��Б��┘ �爛く爬閧�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �  ....:....!....:....!....:....!....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� [ ] ����膈襯 ���皀Θ	       [ ] �ェ����膈襯 ���皀Θ     �
*� [ ] ��ぅ�瓱珮ガ襯 ���皀Θ   [ ] �オ�                    �
*� [ ] �．燹���	               [ ] ��ぅ�瓱珮ガ覃 �/�       �
*�            < OK Ctrl-W > < �皖���碎瘴 Esc >             �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 61, "���竅皋�襯 皋�� ���皀Ε� か� �爛く爬閧��"
DO Sun_Bord WITH 2,  3,  4, 57, " ��Б��┘ �爛く爬閧�� "
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@  3,  6 SAY L_NAME
@  5,  3 GET p_n   PICTURE "@*C ����膈襯 ���皀Θ"
@  5, 31 GET p_bn  PICTURE "@*C �ェ����膈襯 ���皀Θ"
@  6,  3 GET p_ue  PICTURE "@*C ��ぅ�瓱珮ガ襯 ���皀Θ"
@  6, 31 GET p_c   PICTURE "@*C �オ�"
@  7,  3 GET p_sb  PICTURE "@*C �．燹���"
@  7, 31 GET p_bue PICTURE "@*C ��ぅ�瓱珮ガ覃 �/�"
@  8, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  tmp_list = IIF(p_n, "", "�")+ ;
             IIF(p_bn, "", "1")+ ;
             IIF(p_ue, "", "3")+ ;
             IIF(p_c, "", "�")+ ;
             IIF(p_sb, "", "�")+ ;
             IIF(p_bue, "", "4")
  REPLACE H_TYPES WITH tmp_list
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
USE

SELECT (sel_sav)

RETURN
