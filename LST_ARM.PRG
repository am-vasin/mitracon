*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Lst_Arm      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                 �������ୠ⨢�� ᯨ᮪ ࠡ��� ����                  �
*�                                                                        �
*����������������������������������������������������������� 21.10.2005 �ͼ
PROCEDURE Lst_Arm
PARAMETERS tmpAlias


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
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE base_path+"ARM_LST" SHARED AGAIN ORDER TAG NAME ALIAS ARM_5A21
SET RELATION TO ARM_C INTO (m.tmpAlias)

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����/�⪠������" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD All OF (menu_name) PROMPT "����� ��" KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION PAD All OF (menu_name) DO Sw_Mode WITH "All"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, rc_sav
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = FSIZE("ARM_N")+2

DO D_Wins WITH m.ln, m.wd, "���᮪ ࠡ��� ����", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "All"

*
*    BROWSE - ����
*
    BROWSE FIELDS &tmpAlias..MARK:H="", ;
                  ARM_N:H="",           ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && ��ᬮ�� ᯨ᪠

   SELECT (m.tmpAlias)
   IF .NOT. SEEK(ARM_5A21.ARM_C)
     APPEND BLANK
     REPLACE CODE WITH ARM_5A21.ARM_C
     IF FSIZE("NAME") # 0
       REPLACE NAME WITH ALLTRIM(ARM_5A21.ARM_N)
     ENDIF
   ENDIF
   REPLACE MARK WITH IIF(EMPTY(MARK), "�", " ")
   SELECT ARM_5A21
   what_do = "List"

  CASE what_do = "All"    && �⬥��� ��

    m.rc_sav = RECNO()
    SCAN
      SELECT (m.tmpAlias)
      IF .NOT. SEEK(ARM_5A21.ARM_C)
        APPEND BLANK
        REPLACE CODE WITH ARM_5A21.ARM_C
        IF FSIZE("NAME") # 0
          REPLACE NAME WITH ARM_5A21.ARM_N
        ENDIF
      ENDIF
      REPLACE MARK WITH "�"
      SELECT ARM_5A21
    ENDSCAN
    IF BETWEEN(m.rc_sav, 1, RECCOUNT())
      GO m.rc_sav
    ENDIF

    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
USE IN ARM_5A21
SELECT (m.s_sav)

RETURN
