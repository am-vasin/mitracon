*������������������������������������������������������������������������ͻ
*� ���� Corr_Sum     ���ࠡ��稪 ���३ ��ᨭ           06.04.98 15:59:07 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                        �⮣� �� ᯥ�. ��������.                        �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Corr_Sum

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
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
PRIVATE cnt
COUNT REST WHILE CUS_CODE < 0 FOR EMPTY(DATE_OFF) TO cnt

IF cnt = 0
  CLOSE DATABASES
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� �� ������ ᯥ檫����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
GO TOP

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "����� ���㬥��"
ON PAD Choose OF (menu_name) ACTIVATE POPUP Choose

  DEFINE POPUP Choose MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Choose PROMPT "� �㡫��" KEY Ctrl-A, "Enter"
  ON SELECTION BAR 1 OF Choose DO Sw_Mode WITH "Choose_R"

  DEFINE BAR  2 OF Choose PROMPT "� �������" KEY Ctrl-A, "Ctrl+Enter"
  ON SELECTION BAR 2 OF Choose DO Sw_Mode WITH "Choose$"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = MIN(100, cnt)   && ������⢮ ������� ��ப BROWSE
wd = 24

DO D_Wins WITH ln, wd, "����.�������", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter      KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "Choose$"

*
*    BROWSE - ����
*
    BROWSE KEY -9999999, -1 ;
           FIELDS F000 = LEFT(CUS_NAME,24):24:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose_R")
    ENDIF

  CASE what_do = "Choose_R"    && ����� ᯨ᪠ � �㡫��

    DO P_Sp_Sum WITH CUS_CODE, "�"
    what_do = "List"

  CASE what_do = "Choose$"     && ����� ᯨ᪠ � �������

    DO P_Sp_Sum WITH CUS_CODE, "$"
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