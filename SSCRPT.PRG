*******************************************************************
*                                                                 *
*               �����᪠ �� ����७�� �����                       *
*                                                                 *
*******************************************************************
PROCEDURE Sscrpt
PARAMETERS prmUser

PRIVATE tmpUser, tmpCUser, tmpNUser
IF TYPE("m.prmUser") = "N"
  m.tmpUser = m.prmUser
ELSE
  m.tmpUser = m.User
ENDIF
m.tmpCUser = STR(m.tmpUser)

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
USE (m.base_path+"SSCRPT") SHARED AGAIN ORDER TAG SS_CODE ALIAS TMP_LIST
SELECT 0
USE (m.base_path+"SSCRPT") SHARED AGAIN ORDER TAG SS_CODE ALIAS TMP_MODIFY
SELECT 0
USE (m.base_path+"SSCRPT_T") SHARED AGAIN ORDER TAG SS_NAME ALIAS TMP_TYPE
*SET RELATION TO STR(SS_CODE)+m.tmpCUser INTO TMP_LIST
USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS TMP_PRS IN 0
= SEEK(m.tmpCUser, "TMP_PRS")
m.tmpNUser = ALLTRIM(TMP_PRS.FAMILY)+" "+LEFT(TMP_PRS.NAME, 1)+" "+LEFT(TMP_PRS.S_NAME, 1)
USE IN TMP_PRS


*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD modify OF (menu_name) PROMPT "��⠢���/㤠����" KEY Ctrl-A, "Enter"
ON SELECTION PAD modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = FSIZE("SS_NAME", "TMP_TYPE")+2

DO D_Wins WITH m.ln, m.wd, "���� SALE: �����᪠", 1, 0
@ 1, 2 SAY "����㤭�� "+m.tmpNUser
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    SELECT TMP_TYPE
    GO RECNO()
*    WAIT WINDOW IIF(FOUND("TMP_LIST"), "�", "N")

*
*    BROWSE - ����
*
    BROWSE FIELDS SS_NAME:H="",     ;
           MRK = IIF(SEEK(STR(SS_CODE)+m.tmpCUser, "TMP_LIST"), "�", " "):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY

    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE m.what_do = "Modify"    &&

    SELECT TMP_MODIFY
    IF SEEK(STR(TMP_TYPE.SS_CODE)+m.tmpCUser)
      REPLACE SS_CODE WITH 0, SS_USER WITH 0
    ELSE
      GO TOP
      IF .NOT. EMPTY(SS_USER) .OR. BOF() .OR. EOF()
        APPEND BLANK
      ENDIF
      REPLACE SS_CODE WITH TMP_TYPE.SS_CODE, SS_USER WITH m.tmpUser
    ENDIF
    SELECT TMP_TYPE
    =SEEK(STR(TMP_TYPE.SS_CODE)+m.tmpCUser, "TMP_LIST")
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
USE IN TMP_LIST
USE IN TMP_TYPE
USE IN TMP_MODIFY
SELECT (m.s_sav)

RETURN
