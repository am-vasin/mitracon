*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� GetArm       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                         �롮� ࠡ�祣� ����.                          �
*�                                                                        �
*����������������������������������������������������������� 18.04.2000 �ͼ
PROCEDURE GetArm
PARAMETERS startV

PRIVATE tmpPoint, armPoint, s_sav, retVal

s_sav = SELECT()
SELECT 0

tmpPoint = 1
DELETE FILE (tmpo_path+"SQUARES.DBF")
DELETE FILE (tmpo_path+"SQUARES.CDX")

CREATE DBF (tmpo_path+"SQUARES") ;
  ( SQ_CHAR C( 1), ;
    SQ_NAME C(12)  )
INDEX ON SQ_CHAR TAG SQ_CHAR
APPEND BLANK
REPLACE SQ_NAME WITH "�� ���頤��"

SELECT 0
DELETE FILE (tmpo_path+"GETARM.DBF")
DELETE FILE (tmpo_path+"GETARM.CDX")

CREATE DBF (tmpo_path+"GETARM") ;
  ( SQ_CHAR  C( 1), ;
    ARM_CODE N( 4), ;
    ARM_NAME C(16)  )
INDEX ON SQ_CHAR+SYS(15, lwr, ARM_NAME)  TAG SQ_CHAR
INDEX ON SYS(15, lwr, ARM_NAME) TAG ARM_NAME ADDITIVE

SELECT 0
USE (base_path+"ARM_LST") ALIAS ARMS_0418 AGAIN
SCAN
  IF .NOT. EMPTY(ARM_S)
    IF .NOT. SEEK(ARM_S, "SQUARES")
      SELECT SQUARES
      APPEND BLANK
      REPLACE SQ_CHAR WITH ARMS_0418.ARM_S, ;
              SQ_NAME WITH "���頤�� "+ARMS_0418.ARM_S
      IF sys_char = SQ_CHAR
        tmpPoint = RECNO()
      ENDIF
    ENDIF
  ENDIF
  SELECT GETARM
  APPEND BLANK
  REPLACE SQ_CHAR  WITH ARMS_0418.ARM_S,    ;
          ARM_CODE WITH ARMS_0418.ARM_CODE, ;
          ARM_NAME WITH ARMS_0418.ARM_N
  IF TYPE("startV") = "N"
    IF startV = ARM_CODE
      armPoint = RECNO()
    ENDIF
  ENDIF
  SELECT ARMS_0418
ENDSCAN

USE
SELECT SQUARES
GO tmpPoint
SELECT GETARM
IF EMPTY(SQUARES.SQ_CHAR)
  SET ORDER TO TAG ARM_NAME
ELSE
  SET ORDER TO TAG SQ_CHAR
ENDIF

IF TYPE("armPoint") = "N"
  GO armPoint
  IF SQ_CHAR # SQUARES.SQ_CHAR
    GO 1 IN SQUARES
  ENDIF
ELSE
  GO TOP
ENDIF


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

SELECT 0
DO Use_Dummy
SELECT GETARM

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Square OF (menu_name) PROMPT "���頤��" KEY Ctrl-A, "F1"
ON SELECTION PAD Square OF (menu_name) DO Sw_Mode WITH "Square"

DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("ARM_NAME")

DO D_Wins WITH ln, wd, "����稥 ����", 1, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 2 SAY SQUARES.SQ_NAME
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1    DO Sw_Mode WITH "Square"

*
*    BROWSE - ����
*
    BROWSE KEY ALLTRIM(SQUARES.SQ_CHAR);
           FIELDS ARM_NAME:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �����

    retVal = ARM_CODE
    EXIT

  CASE what_do = "Square"    && ���頤��

    DO Get_Sq
    what_do = "List"

  OTHERWISE

    retVal = IIF(TYPE("startV") = "N", startV, -1)
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
SELECT SQUARES
USE

DELETE FILE (tmpo_path+"SQUARES.DBF")
DELETE FILE (tmpo_path+"SQUARES.CDX")
DELETE FILE (tmpo_path+"GETARM.DBF")
DELETE FILE (tmpo_path+"GETARM.CDX")

SELECT (s_sav)

RETURN retVal

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Get_Sq       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            �롮� ���頤��.                             �
*�                                                                        �
*����������������������������������������������������������� 18.04.2000 ���
PROCEDURE Get_Sq

PRIVATE r_sav
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

s_sav = SELECT()

SELECT SQUARES

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "������" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-18   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("SQ_NAME")

DO D_Wins WITH ln, wd, "���頤��", 0, 0
what_do = "List"

r_sav = RECNO()
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS SQ_NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && ��ᬮ�� ᯨ᪠

    IF r_sav # RECNO()
      SELECT GETARM
      IF EMPTY(SQUARES.SQ_CHAR)
        SET ORDER TO TAG ARM_NAME
      ELSE
        SET ORDER TO TAG SQ_CHAR
      ENDIF
      GO TOP IN GETARM
    ENDIF
    EXIT

  OTHERWISE

    GO r_sav
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