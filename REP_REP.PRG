*������������������������������������������������������������������������ͻ
*� ���� Rep_Rep      ���ࠡ��稪 ���३ ��ᨭ           10.10.97 15:49:10 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                             ����� ����.                             �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Rep_Rep
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
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
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ������⢮ ������� ��ப BROWSE
wd = IIF(SET("CENTURY") = "ON", 57, 55)

DO D_Wins WITH ln, wd, "����� ��। ���⠢騪���", 0, 1
@ 1, 2 SAY "������"+IIF(SET("CENTURY") = "ON", "��Ą������", "�Ą�����")+ ;
           "���⠢騪"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
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

  CASE what_do = "Choose"    && �롮� ����

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

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                        ����� ���. �����樨.                         �
*�                                                                        �
*����������������������������������������������������� 10.10.97 16:10:13 ͼ
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

@ WROWS()-2, 2 SAY "�� "
@ ROW(), COL() SAY nm1 COLOR SCHEME 1
@ ROW(), COL() SAY " ��� "
@ ROW(), COL() SAY nm2 COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS())

RETURN .T.