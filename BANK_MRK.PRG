********************************************************************************
*    ���� Bank_Mrk       ���ࠡ��稪 ���३ ��ᨭ                10.01.2012    *
********************************************************************************
*                                                                              *
*                ��ନ஢���� ᯨ᪠ ������ ��� ��२���������                 *
*                                                                              *
********************************************************************************
PROCEDURE Bank_Mrk

PRIVATE mrkChar, r_sav, new_name

m.mrkChar = CHR(251)

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
SELECT TMP_BANKS
SET ORDER TO TAG NAME
GO TOP


*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mark OF (menu_name) PROMPT "�⬥⪨"
ON PAD Mark OF (menu_name) ACTIVATE POPUP Mark

  DEFINE POPUP Mark MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Mark PROMPT "�⬥���/������" KEY Ctrl-A, "Enter"
  ON SELECTION BAR 1 OF Mark DO Sw_Mode WITH "Makr"

  DEFINE BAR  2 OF Mark PROMPT "�⬥��� ��" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 2 OF Mark DO Sw_Mode WITH "M_All"

  DEFINE BAR  3 OF Mark PROMPT "������ ��" KEY Ctrl-A, "Shift-Tab"
  ON SELECTION BAR 3 OF Mark DO Sw_Mode WITH "C_All"

DEFINE PAD Filter OF (menu_name) PROMPT "������" KEY Ctrl-A, "F3"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Exec OF (menu_name) PROMPT "��२�������� ��." KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION PAD Exec OF (menu_name) DO Sw_Mode WITH "Exec"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = FSIZE("NAME")+2

DO D_Wins WITH m.ln, m.wd, "���᮪ ������", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F3			DO Sw_Mode WITH "Filter"
    ON KEY LABEL Ctrl-Enter	DO Sw_Mode WITH "Exec"
    ON KEY LABEL Tab		DO Sw_Mode WITH "M_All"
    ON KEY LABEL BackTab	DO Sw_Mode WITH "C_All"

*
*    BROWSE - ����
*
    BROWSE FIELDS NAME:H="", ;
                  MRK:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Mark")
    ENDIF

  CASE m.what_do = "Mark"    && ��⠭�����/������

    IF .NOT. EOF() .AND. .NOT. BOF()
      REPLACE MRK WITH IIF(EMPTY(MRK), m.mrkChar, "")
      SKIP
      IF EOF()
        GO BOTTOM
      ENDIF
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "M_All"    && �⬥��� ��

    m.r_sav = RECNO()
    REPLACE ALL MRK WITH m.mrkChar
    IF BETWEEN(m.r_sav, 1, RECCOUNT())
      GO m.r_sav
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "C_All"    && ������ ��

    m.r_sav = RECNO()
    REPLACE ALL MRK WITH ""
    IF BETWEEN(m.r_sav, 1, RECCOUNT())
      GO m.r_sav
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Filter"    && ��⠭�����/����� 䨫���

    IF TAG() = "NAME"
      SET ORDER TO TAG MARKED
      SET MARK OF PAD Filter OF (menu_name) TO .T.
    ELSE
      SET ORDER TO TAG NAME
      SET MARK OF PAD Filter OF (menu_name) TO .F.
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Exec"    && ��२���������

    m.new_name = BankName("����� �������� �����", FSIZE("NAME"))
    IF .NOT. EMPTY(m.new_name)
      DO Bank_Ren WITH m.new_name
    ENDIF
    GO TOP
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
SELECT (m.s_sav)

RETURN

********************************************************************************
*                                                                              *
*                           ��२��������� ������                              *
*                                                                              *
********************************************************************************
PROCEDURE Bank_Ren
PARAMETERS prmName

PRIVATE f_Name, m.s_sav

m.f_Name = DBF()
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"CUS_BANK") SHARED AGAIN ALIAS BANK_ADD
SELECT 0
USE (m.base_path+"CUS_BIL") SHARED AGAIN ALIAS BANK_MAIN
SELECT 0
USE (m.f_Name) AGAIN ORDER TAG MARKED ALIAS BN_U
SELECT 0
USE (m.f_Name) AGAIN ALIAS BN
SET RELATION TO SYS(15, m.lwr, NAME) INTO BN_U
SCAN
  IF .NOT. FOUND("BN_U")
    LOOP
  ENDIF
  IF IS_MAIN
    SELECT BANK_MAIN
    GO BN.LINK
    REPLACE BANK WITH m.prmName
  ELSE
    SELECT BANK_ADD
    GO BN.LINK
    REPLACE BANK WITH m.prmName
  ENDIF
  SELECT BN
  IF EMPTY(MRK)
    DELETE
  ENDIF
ENDSCAN

USE
SELECT BN_U
DELETE ALL
USE IN BN_U
USE IN BANK_MAIN
USE IN BANK_ADD

SELECT (m.s_sav)

RETURN
