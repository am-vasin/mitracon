*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Rep_Rep      ��о�｀砒┴ ��むォ ��瓱�           10.10.97 15:49:10 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                             ��≒�� �砒モ�.                             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Rep_Rep
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
DO Use_Dummy

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (base_path+"REP_TIT")
SET RELATION TO SUP_CODE INTO ACCOUNT
GO BOTTOM

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = IIF(SET("CENTURY") = "ON", 57, 55)

DO D_Wins WITH ln, wd, "�砒モ� �ムイ ��痰�∫┴���", 0, 1
@ 1, 2 SAY "���ム�"+IIF(SET("CENTURY") = "ON", "陳�����陳陳", "陳����陳�")+ ;
           "��痰�∫┴"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS N_REP:H="":5,     ;
                  D_REP:H="",       ;
                  ACCOUNT.CUS_NAME:H="":50, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_F()  ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �襦�� �砒モ�

    DO Out_R_S WITH REP_TIT.N_REP, REP_TIT.D_REP
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                        �ョ�碎 ぎ�. ┃筮爼粭┬.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 10.10.97 16:10:13 夕
PROCEDURE Show_F
PRIVATE nm1, nm2

ACTIVATE WINDOW (win_name+"_M") SAME

IF .NOT. SEEK(REP_TIT.WHO, "PERSONS")
  nm1 = SPACE(FSIZE("FAMILY", "PERSONS")+6)
ELSE
  nm1 = ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+". "+LEFT(PERSONS.S_NAME,1)+"."
  nm1 = PADR(nm1, FSIZE("FAMILY", "PERSONS")+6)
ENDIF

IF .NOT. SEEK(REP_TIT.WHERE, "ARM_LST")
  nm2 = SPACE(FSIZE("ARM_N", "ARM_LST")+6)
ELSE
  nm2 = ARM_LST.ARM_N
ENDIF

@ WROWS()-2, 2 SAY "�皰 "
@ ROW(), COL() SAY nm1 COLOR SCHEME 1
@ ROW(), COL() SAY " �ぅ "
@ ROW(), COL() SAY nm2 COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS())

RETURN .T.
