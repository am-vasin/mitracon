*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Spec_Rep     ��о�｀砒┴ ��むォ ��瓱�           17.09.97 12:56:30 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                       ����→� �� 甎ユ.��┘����.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Spec_Rep
PRIVATE info_t, c_code, c_name

c_code = 0
c_name = SPACE(29)
info_t  = 1

DO WHILE .T.
  c_name = LEFT(c_name,29)
  DO Chs_Spec WITH c_code, c_name

  DO CASE
  CASE info_t = 1
    DO Cus_Info WITH c_code
  CASE info_t = 2
    DO Acc_Hist WITH c_code
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

CLOSE DATABASES

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                   �襦�� 甎ユ��┘��� � 皋�� 甎��→�.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 17.09.97 13:10:34 夕
PROCEDURE Chs_Spec
PARAMETERS c_f, c_n
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
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�    敖 [ ] �┓ あ└キ�� 陳陳陳陳陳朕    �
*�    � ....:....!....:....!....:....�    �
*�    青陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰    �
*� < �ク�Ж� > < �※Ε�┘ > < �諷�� Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE s_w
s_w = .F.
*------------------------------------------------------------------------
PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 44, "����→� �� 甎ユ. ��┘����"
DO Sun_Bord WITH 2, 6, 4, 37, ""

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2,  8 GET s_w PICTURE "@*C �┓ あ└キ��" VALID S_Spec()
@ 3,  8 GET c_n WHEN .F.
@ WROWS()-2, 3 GET info_t PICTURE "@*HT \ �ク�Ж� ;\ �※Ε�┘ ;\? �諷�� Esc "

READ CYCLE VALID Tst_B()
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                            �襦�� 皋�� あ└キ��.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 16.11.2000 夕
PROCEDURE S_Spec
PARAMETERS c, n
 
DO Spec_Fil WITH c_f, c_n
s_w = .F.
SHOW GET s_w
SHOW GET c_n

RETURN .T.
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   蹍罐ゃ�� Tst_B                                                      �
*�                            �諷�� ├ ゛����.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 17.11.2000 夕
PROCEDURE Tst_B

PRIVATE mss

IF info_t = 3
  RETURN .T.
ENDIF
  
IF EMPTY(c_f)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 礫����� 皋� あ└キ��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD "{Tab}{Tab}"
  RETURN .F.
ENDIF

RETURN .T.