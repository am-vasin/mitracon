*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� V_Menu       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                             ��ᬮ�� ����.                             �
*�                                                                        �
*����������������������������������������������������������� 19.02.2002 �ͼ
PROCEDURE V_Menu

PRIVATE tmpLevel, tmpName, tmpKey, tmpTitle

USE (base_path+"S_RIGHTS")
SET FILTER TO .NOT. EMPTY(NAME)

SELECT 0
USE (base_path+"MENU_LST")

tmpTitle = "�᭮���� ����"
tmpLevel = ""
tmpName = SYS(3)
SELECT 0
CREATE DBF (tmpo_path+tmpName) (LINK N(10))
USE (tmpo_path+tmpName) ALIAS TMP_MENU EXCLUSIVE

DO Fil_Tmp

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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT TMP_MENU

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("P_PROMPT", "MENU_LST")+2

DO D_Wins WITH ln, wd, "���� ��⥬�", 1, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 0 SAY PADC(tmpTitle, WCOLS())
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    

*
*    BROWSE - ����
*
    BROWSE FIELDS MENU_LST.P_PROMPT:H="",  ;
                  P000 = IIF(EMPTY(MENU_LST.COMM_STR), "", " "), ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && ��ᬮ�� ᯨ᪠

    IF EMPTY(MENU_LST.COMM_STR)
      tmpLevel = STR(MENU_LST.LEVEL1, 2)
      IF .NOT. EMPTY(MENU_LST.LEVEL2)
        tmpLevel = tmpLevel+STR(MENU_LST.LEVEL2, 2)
      ENDIF
      DO Fil_Tmp
    ELSE
      DO V_Rights
    ENDIF
    what_do = "List"

  OTHERWISE

    IF LEN(tmpLevel) = 0
      EXIT
    ELSE
      tmpLevel = LEFT(tmpLevel, LEN(tmpLevel)-2)
      DO Fil_Tmp
    ENDIF
    what_do = "List"

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
SELECT TMP_MENU
tmpName = DBF()
USE
DELETE FILE (tmpName)
SELECT MENU_LST
USE
SELECT S_RIGHTS
USE

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Fil_Tmp      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      ���������� �६������ 䠩��.                      �
*�                                                                        �
*����������������������������������������������������������� 19.02.2002 ���
PROCEDURE Fil_Tmp

SELECT TMP_MENU
SET RELATION TO
ZAP

SELECT MENU_LST
SET ORDER TO TAG LEVELS
SEEK tmpLevel
IF LEN(tmpLevel) # 0
  tmpTitle = ALLTRIM(P_PROMPT)
  SKIP
ELSE
  tmpTitle = "�᭮���� ����"
ENDIF

SCAN REST WHILE LEFT(STR(MENU_LST.LEVEL1,2)+STR(MENU_LST.LEVEL2,2)+  ;
                     STR(MENU_LST.LEVEL3,2), LEN(tmpLevel)) = tmpLevel
  IF LEN(tmpLevel) = 0 .AND. MENU_LST.LEVEL2 # 0
    LOOP
  ENDIF
  IF LEN(tmpLevel)  = 2 .AND. MENU_LST.LEVEL3 # 0
    LOOP
  ENDIF
  SELECT TMP_MENU
  APPEND BLANK
  REPLACE LINK WITH RECNO("MENU_LST")
  SELECT MENU_LST
ENDSCAN
SET ORDER TO

SELECT TMP_MENU
SET RELATION TO LINK INTO MENU_LST
GO TOP

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� V_Rights     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            ����樨 ����㯠.                            �
*�                                                                        �
*����������������������������������������������������������� 20.02.2002 ���
PROCEDURE V_Rights

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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT S_RIGHTS
GO TOP

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Users OF (menu_name) PROMPT "���짮��⥫�" KEY Ctrl-A, "Tab"
ON SELECTION PAD Users OF (menu_name) DO Sw_Mode WITH "Users"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("NAME")+2

DO D_Wins WITH ln, wd, ALLTRIM(MENU_LST.P_PROMPT), 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Users"

*
*    BROWSE - ����
*
    BROWSE FIELDS S000 = IIF(EMPTY(SUBSTR(MENU_LST.ACC_LABELS, RIGHT, 1)), "�", " "):1:H="", ;
                  NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && ����� ����㯠
    
    SELECT MENU_LST
    IF EMPTY(SUBSTR(MENU_LST.ACC_LABELS, S_RIGHTS.RIGHT, 1))
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS, S_RIGHTS.RIGHT, 1, "*")
    ELSE
      REPLACE ACC_LABELS WITH STUFF(ACC_LABELS, S_RIGHTS.RIGHT, 1, " ")
    ENDIF
    SELECT S_RIGHTS

    what_do = "List"

  CASE what_do = "Users"    && ���짮��⥫�
    
    DO R_Users WITH S_RIGHTS.RIGHT
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
SELECT (s_sav)

RETURN