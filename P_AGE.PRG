*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                �痰���→� �爛ぅ�讚��� "〓о�痰�" か� 罐�.              �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE P_Age
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

PRIVATE price_age, ex
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&
IF File_O(base_path+"P_AGE.MEM")
  RESTORE FROM (base_path+"P_AGE.MEM") ADDITIVE
ELSE
  price_age = 7
ENDIF

*------------------------------------------------------------------------
*       ����讓� "ぅ爨━" 罐��
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                  �
*�       ����腑痰〓 きォ 999        �
*�                                  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*�                                  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 38, '����讓� "ぅ爨━" 罐��'

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 9 SAY "����腑痰〓 きォ" GET price_age PICTURE "999" VALID Wrong()
@ 5, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

IF ex = 1
  SAVE ALL LIKE price_age TO (base_path+"P_AGE.MEM")
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                         蹍▲爲� ���※�讚�痰�.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Wrong

IF READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF price_age > 0
  RETURN .T.
ENDIF

PRIVATE mss
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"����腑痰〓 きォ ぎ�Ν� °碎 ｀�跏� �祀�!"
mss(3) = ""
DO Out_Mess WITH 7, "mss"

RETURN .F.
