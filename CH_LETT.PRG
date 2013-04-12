*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Ch_Lett      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                         �롮� ����⮢�� ���쬠                         �
*�                                                                        �
*����������������������������������������������������������� 10.04.2006 �ͼ
PROCEDURE Ch_Lett
PARAMETERS prmSubj, prmText

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
USE (base_path+"MAILTXT") SHARED AGAIN ALIAS ML_6410 ORDER TAG WHO

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Modify OF (menu_name) PROMPT "����." KEY Ctrl-A, "Tab"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Delete OF (menu_name) PROMPT "�������" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, tmpRec, blnk0
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = FSIZE("SUBJECT")
SCATTER TO blnk0 BLANK

DO D_Wins WITH m.ln, m.wd, "����⮢�� ��ᥬ", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Modify"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Delete"

*
*    BROWSE - ����
*
    BROWSE KEY m.user REST ;
           FIELDS SUBJECT,  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && �롮�

    IF TYPE("m.prmSubj") = "C"
      m.prmSubj = ALLTRIM(SUBJECT)
    ENDIF
    IF TYPE("m.prmText") = "C"
      SET ORDER TO TAG LETTID
      m.ln = LETTID
      SCAN REST WHILE m.ln = LETTID
        m.prmText = m.prmText+TEXT
      ENDSCAN
      m.prmText = ALLTRIM(m.prmText)
    ENDIF
    EXIT

  CASE m.what_do = "Modify"    && ���४��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO ModiLett WITH LETTID
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Add"       && ����������

    m.tmpRec = ModiLett()
    IF .NOT. EMPTY(m.tmpRec)
      GO m.tmpRec
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Delete"    && ��������

    IF .NOT. BOF() .AND. .NOT. EOF()
      m.tmpRec = LETTID
      SET ORDER TO TAG LETTID
      DO WHILE SEEK(STR(m.tmpRec))
        GATHER FROM blnk0
      ENDDO
      SET ORDER TO TAG WHO
    ENDIF
    m.what_do = "List"

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

USE

SELECT (m.s_sav)

RETURN