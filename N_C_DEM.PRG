*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� N_C_Dem      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                           ��牀� �� ��┘�矣.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 27.12.2000 様�
PROCEDURE N_C_Dem
PARAMETER super
PRIVATE pl, tmpCusCode, tmpCusName

tmpCusCode = 0
tmpCusName = SPACE(50)
DO Use_Dummy
DO Use_Link
SELECT 0
IF super
  USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
ELSE
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
ENDIF
IF super
  pl = Choose_P()
ELSE
  pl = sys_char
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� 敖 [ ] ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!....:....!....:....!....:....!  � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�           < ����→� > < ��牀� > < �諷�� Esc >           �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE sw_cus, ex
sw_cus = .F.
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 7, 61, "��牀� �� ��┘�矣"
DO Sun_Bord WITH  2,  3,  4, 57, "            "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@ 2,  6 GET sw_cus PICTURE "@*C ��┘��" VALID Ch_Cus()
@ 3,  5 GET tmpCusName WHEN .F.
@ 5, 13 GET ex PICTURE "@*HT \ ����→� ;\ ��牀� ;\? �諷�� Esc "

READ CYCLE VALID To_Work()

*--------------------------------------------------------------------------
SELECT ACCOUNT
USE
POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Cus       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                             �襦�� ��┘���.                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 27.12.2000 陳�
PROCEDURE Ch_Cus
PRIVATE tmpC, tmpN

IF sw_cus = .F.
  RETURN .T.
ENDIF
sw_cus = .F.

SHOW GET sw_cus

IF super
  IF Custs("", .F., .F., .F., pl)
    tmpCusName = ACCOUNT.CUS_NAME
    SHOW GET tmpCusName
    tmpCusCode = ACCOUNT.CUS_CODE
  ENDIF
ELSE
  tmpC = tmpCusCode
  tmpN = tmpCusName
  DO Own_Cus WITH tmpC, tmpN
  IF tmpC # tmpCusCode
    tmpCusCode = tmpC
    tmpCusName = tmpN
    = SEEK(tmpC, "ACCOUNT")
    SHOW GET tmpCusName
  ENDIF
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� To_Work      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ｡��｀皖� ���牀��.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 27.12.2000 陳�
PROCEDURE To_Work
PRIVATE mss

IF ex = 3
  RETURN .T.
ENDIF

IF tmpCusCode = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 礫����� ��┘���"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 1
ENDIF

IF ex = 1
  DO Acc_Hist WITH tmpCusCode
ELSE
  DO Cus_Dem1 WITH tmpCusCode, tmpCusName
ENDIF
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR (SCHEME(1,2))

RETURN .F.
