*������������������������������������������������������������������������ͻ
*� ���� Ord_List     ���ࠡ��稪 ���३ ��ᨭ           02.02.98 09:34:56 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�            ����� � ������ � 䠩� ��� �� ���⠢�� ⮢��.           �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Ord_List
PARAMETER manCode
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
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

DO Use_Dummy
SELECT 0
USE (base_path+"CUSTOMER") ORDER TAG CODE
SELECT 0
USE (base_path+"ORD_TIT")
SET RELATION TO SUP_CODE INTO CUSTOMER
IF TYPE("m.manCode") ="N"
  SET FILTER TO WHO_ORD = m.manCode
ENDIF
GO BOTT
IF EOF() .OR. BOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� �� ����� ���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT CUSTOMER
  USE
  SELECT DUMMY
  USE
  RETURN
ENDIF

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Send OF (menu_name) PROMPT "��।��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Send OF (menu_name) DO Sw_Mode WITH "Send"

DEFINE PAD URM OF (menu_name) PROMPT "���" KEY Ctrl-A, "Home"
ON SELECTION PAD URM OF (menu_name) DO Sw_Mode WITH "URM"

DEFINE PAD Print OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD View OF (menu_name) PROMPT "��ᬮ��" KEY Ctrl-A, "Tab"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ������⢮ ������� ��ப BROWSE
wd = IIF(SET("DATE") = "ON", 75, 73)

DO D_Wins WITH ln, wd, "���", 0, 0

IF SET("DATE") = "ON"
  @ 1, 2 SAY "��č���� ���.����Ą������ď��⠢騪�������ďਬ�砭��"
ELSE
  @ 1, 2 SAY "��č���� ���.��Ą�����ď��⠢騪�������ďਬ�砭��"
ENDIF

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+P DO Sw_Mode WITH "Print"
    ON KEY LABEL Tab    DO Sw_Mode WITH "View"
    ON KEY LABEL Home   DO Sw_Mode WITH "URM"

*
*    BROWSE - ����
*
    BROWSE FIELDS FIRM:H="", ORD_NUM:H="", ORD_DATE:H="",     ;
                  CUSTOMER.B_NAME:H="", NOTE:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Send")
    ENDIF

  CASE what_do = "Send"    && ���� ��� ��।��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO S_Ord WITH FIRM, ORD_NUM, ORD_DATE, ""
    ENDIF
    what_do = "List"

  CASE what_do = "Print"   && �����

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_Ord WITH FIRM, ORD_NUM, ORD_DATE
    ENDIF
    what_do = "List"

  CASE what_do = "View"    && ��ᬮ��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO V_Order WITH FIRM, ORD_DATE, ORD_NUM
    ENDIF
    what_do = "List"

  CASE what_do = "URM"    && ���

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO To_URM_O WITH FIRM, ORD_DATE, ORD_NUM
    ENDIF
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