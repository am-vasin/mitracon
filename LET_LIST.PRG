*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Let_List     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�         ���४�� ᯨ᪠ ����饭��� � ������� ࠡ�祣� ����.          �
*�                                                                        �
*����������������������������������������������������������� 15.09.2006 �ͼ
PROCEDURE Let_List
PARAMETERS prmArm	&& ��� ࠡ�祣� ����, �᫨ ����, � ⥪�饥.

PRIVATE tmpArm
m.tmpArm = IIF(EMPTY(m.prmArm), m.arm, m.prmArm)

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
USE (m.base_path+"ARM_LST") SHARED AGAIN ALIAS ARM_6914 ORDER TAG ARM_C
SEEK m.tmpArm
SELECT 0
USE (m.base_path+"LET_LIST") SHARED AGAIN ALIAS LL_6914 ORDER TAG USER_CODE
SELECT 0
USE (m.base_path+"PERSONS") SHARED AGAIN ALIAS PR_6914 ORDER TAG NAME
SET RELATION TO STR(CODE)+m.tmpArm INTO LL_6914


*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�������/�������" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
m.ln = WROWS("")-5   && ������⢮ ������� ��ப BROWSE
m.wd = 46

DO D_Wins WITH m.ln, m.wd, "���᮪ ����饭��� � ������� ࠡ�祣� ����", 1, 0
m.what_do = "List"
@ 1, 2 SAY "����祥 ���� "+ARM_6914.ARM_N
@ 2, 2 SAY ;
"đ���㤭��������������������������������Ą���."
*....:....!....:....!....:....!....:....! 

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS ;
    	   tmpFIO = ALLTRIM(PR_6914.FAMILY)+" "+LEFT(PR_6914.NAME, 1)+" "+LEFT(PR_6914.S_NAME, 1):40:H="", ;
    	   tmpMrk = IIF(FOUND("LL_6914"), "  �  ", "     "):5:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && ��ᬮ�� ᯨ᪠

    IF .NOT. EOF() .AND. .NOT. BOF()
      SELECT LL_6914
      IF FOUND("LL_6914")
        REPLACE USER_CODE WITH 0, ARM_CODE WITH ""
      ELSE
        GO TOP
        IF .NOT. EMPTY(USER_CODE) .OR. EOF()
          APPEND BLANK
        ENDIF
        REPLACE USER_CODE WITH PR_6914.CODE, ARM_CODE WITH ARM_6914.ARM_C
      ENDIF
      SELECT PR_6914
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
USE IN LL_6914
USE IN PR_6914
USE IN ARM_6914
SELECT (m.s_sav)

RETURN