*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� R_Users      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                ���᮪ ���짮��⥫�� � ������� ����㯮�                �
*�                                                                        �
*����������������������������������������������������������� 20.02.2002 �ͼ
PROCEDURE R_Users
PARAMETERS r_code

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
PRIVATE tmpName, tmpRight

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"S_RIGHTS") SHARED AGAIN ALIAS TMP_20220 ORDER TAG RIGHT
SEEK r_code
tmpRight = ALLTRIM(NAME)
USE (base_path+"PERSONS") SHARED AGAIN ALIAS PS_20220 ORDER TAG CODE
SELECT 0
USE (base_path+"USERS") SHARED AGAIN ALIAS USER_20220
SET RELATION TO USER_CODE INTO PS_20220

SELECT 0
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ;
  ( CODE   N( 5), ;
    FAMILY C(FSIZE("FAMILY", "PS_20220")), ;
    NAME   C(FSIZE("NAME", "PS_20220")),   ;
    S_NAME C(FSIZE("S_NAME", "PS_20220"))  )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_20220

SELECT USER_20220
SCAN FOR S_RIGHTS = r_code
  SELECT TMP_20220
  APPEND BLANK
  REPLACE CODE   WITH PS_20220.CODE,  ;
          FAMILY WITH PS_20220.FAMILY,;
          NAME   WITH PS_20220.NAME,  ;
          S_NAME WITH PS_20220.S_NAME
  SELECT USER_20220
ENDSCAN
USE
SELECT PS_20220
USE
SELECT TMP_20220
INDEX ON SYS(15, lwr, FAMILY+NAME+S_NAME) TAG NAME
GO TOP

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("FAMILY")+FSIZE("NAME")+FSIZE("S_NAME")+2

DO D_Wins WITH ln, wd, tmpRight, 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS FAMILY:H="",  ;
                  NAME:H="",    ;
                  S_NAME:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && ��ᬮ�� ᯨ᪠

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
tmpRight = DBF()
USE
DELETE FILE (tmpRight)
DELETE FILE (LEFT(tmpRight, LEN(tmpRight)-3)+"cdx")
SELECT (s_sav)

RETURN