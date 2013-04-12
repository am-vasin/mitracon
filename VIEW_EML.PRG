*������������������������������������������������������������������������ͻ
*� ���� View_Fax     ���ࠡ��稪 ���३ ��ᨭ           30.09.98 16:00:53 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                           ��ᬮ�� ��. �����.                          �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE View_Eml

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

DEFINE PAD View OF (menu_name) PROMPT "��ᬮ��" KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Delete OF (menu_name) PROMPT "��������"
ON PAD Delete OF (menu_name) ACTIVATE POPUP Delete

  DEFINE POPUP Delete MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Delete PROMPT "����饩 ��ப�" KEY Ctrl-A, "Del"
  ON SELECTION BAR 1 OF Delete DO Sw_Mode WITH "Del_1"

  DEFINE BAR  2 OF Delete PROMPT "��� ��ࠢ������" KEY Ctrl-A, "F8"
  ON SELECTION BAR 2 OF Delete DO Sw_Mode WITH "Del_All"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

DO Use_Dummy
SELECT 0
USE (send_log+"SENDMESS")
SET FILTER TO WHERE = arm .AND. MAIL # " "
GO TOP

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS()-12    && ������⢮ ������� ��ப BROWSE
wd = IIF(SET("CENTURY") = "ON", 55, 51)

DO D_Wins WITH ln, wd, "��ࠢ�� ��. �����", 0, 2
@ WROWS()-3, 3 SAY "����"
@ WROWS()-2, 3 SAY "����"
IF SET("CENTURY") = "ON"
  @ 1, 2 SAY ;
  "�������Ą��������Ċ���ᯮ�����������������Ď�ࠢ���"
ELSE
  @ 1, 2 SAY ;
  "�������Ą������Ċ���ᯮ�����������������Ď�ࠢ�."
ENDIF
what_do = "List"
GO BOTT

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Del   DO Sw_Mode WITH "Del_1"
    ON KEY LABEL F8    DO Sw_Mode WITH "Del_All"

*
*    BROWSE - ����
*
    BROWSE FIELDS CODE:H="",    ;
                  DATE_ON:H="", ;
                  TO:H="",      ;
                  SENDMAIL:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Footer()  ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "View")
    ENDIF

  CASE what_do = "View"    && ��ᬮ�� ���㬥��

    DELETE FILE (tmpo_path+"VIEW_FAX.TXT")
    IF .NOT. File_O(ALLTRIM(NAME))
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"���� ��᫠��� �� �����-� ��稭�� ���祭�..."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      IF ".DBF" $ NAME
        DO View_DBF WITH ALLTRIM(NAME), ALLTRIM(SUBJECT)
      ELSE
        DO View_Txt WITH ALLTRIM(NAME), ALLTRIM(SUBJECT), 66, 0, "1"
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Del_1"   && �������� ��ப�

    IF .NOT. BOF() .AND. .NOT. EOF()
      DELETE FILE (ALLTRIM(NAME))
      DELETE
    ENDIF
    what_do = "List"

  CASE what_do = "Del_All"    && �������� ��ࠢ������ 䠪ᮢ

    SCAN FOR .NOT. EMPTY(SENDFAX)
      DELETE
      DELETE FILE (ALLTRIM(NAME))
      DELETE
    ENDSCAN
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
*�                                Footer!                                 �
*�                                                                        �
*����������������������������������������������������� 30.09.98 16:45:32 ͼ
PROCEDURE Footer

ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-3, 8 SAY LEFT(SUBJECT,WCOLS()-10)
@ WROWS()-2, 9 SAY MAILADDR

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.