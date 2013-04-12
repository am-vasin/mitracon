*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Lett_Get      ���ࠡ��稪 ���३ ��ᨭ                     �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                      ����祭�� �室��� ᮮ�饭��                      �
*�                                                                        �
*����������������������������������������������������������� 28.10.2005 �ͼ
PROCEDURE Lett_Get
PARAMETERS prmUser, prmArm

PRIVATE tmpUser, tmpArm, tmpBlank, tmpName
m.tmpUser = IIF(.NOT. EMPTY(m.prmUser), m.prmUser, m.user)
m.tmpArm  = IIF(.NOT. EMPTY(m.prmArm ), m.prmArm , m.arm )
m.tmpName = SYS(3)

m.s_sav = SELECT()

IF .NOT. Is_Lett()
  RETURN
ENDIF
GO TOP IN TMP_LIST

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

SELECT 0
DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS TMP_PRS

SELECT 0
USE (base_path+"LETT_OUT") SHARED AGAIN ORDER TAG MESS_ID ALIAS TMP_OUT
SET RELATION TO WHO INTO TMP_PRS

SELECT 0
USE (base_path+"LETT_IN") SHARED AGAIN ALIAS TMP_IN
SET RELATION TO MESS_ID INTO TMP_OUT

SELECT TMP_LIST
SET RELATION TO LINK INTO TMP_IN

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Read OF (menu_name) PROMPT "������" KEY Ctrl-A, "Enter"
ON SELECTION PAD Read OF (menu_name) DO Sw_Mode WITH "Read"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
m.wd = 52

DO D_Wins WITH m.ln, m.wd, "��� ��� ����㯨�� ᫥���騥 ᮮ�饭��", 0, 1
m.what_do = "List"

@ 1, 2 SAY ;
 "��������������������������������Ď�ࠢ����"
*X....:....!....:....!....:....!..X��.��.����X��:��:��X
@ WROWS()-3, 2 SAY "��ࠢ�⥫�"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 2
    SHOW MENU (m.menu_name)
    ACTIVATE WINDOW (win_name+"_M") SAME
    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS TMP_OUT.TITLE:H="",    ;
                  TMP_OUT.CR_DATE:H="",  ;
                  TMP_OUT.CR_TIME:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Read")
    ENDIF

  CASE m.what_do = "Read"    && ��ᬮ�� ᮮ�饭��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO LettView WITH TMP_IN.MESS_ID
      REPLACE TMP_IN.READ_DATE WITH DATE(), ;
              TMP_IN.READ_TIME WITH TIME(), ;
              TMP_IN.WHO_READ  WITH m.user
      DELETE
      SKIP
      IF EOF()
        GO BOTT
      ENDIF
    ENDIF
    IF EOF() .OR. BOF()
      EXIT
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
USE IN TMP_IN
USE IN TMP_OUT
USE IN TMP_PRS
USE IN TMP_LIST
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
SELECT (m.s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Draw_Foot    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���㥬 �������⥫��� ���ଠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 20.10.1998 ���
PROCEDURE Draw_Foot
PRIVATE tmpV

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*
m.tmpV = ALLTRIM(TMP_PRS.FAMILY)+" "+LEFT(TMP_PRS.NAME, 1)+" "+LEFT(TMP_PRS.S_NAME, 1)
m.tmpV = PADR(m.tmpV, WCOLS()-6)
@ 	WROWS()-2, 3 SAY m.tmpV

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Is_Lett      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�        �஢�ઠ ������ �����⠭��� ��ᥬ � �����⮢�� ᯨ᪠        �
*�                                                                        �
*����������������������������������������������������������� 28.10.2005 ���
PROCEDURE Is_Lett

PRIVATE s_sav, now

m.s_sav = SELECT()
m.now   = DTOS(DATE())+TIME()
SELECT 0
USE (m.base_path+"LETT_IN") SHARED AGAIN ALIAS TMP_L_5A28 ORDER TAG ADDRESS
IF SEEK("* "+STR(m.tmpUser, 5))
  IF DTOS(SHOW_DATE)+SHOW_TIME <= m.now
    DO Cr_Tmp
    SCAN REST WHILE EMPTY(READ_DATE) .AND. ADDRESS_T = " " .AND. ADDRESS = STR(m.tmpUser, 5) ;
                    .AND. DTOS(SHOW_DATE)+SHOW_TIME <= m.now
      SELECT TMP_LIST
      APPEND BLANK
      REPLACE LINK      WITH RECNO("TMP_L_5A28"), ;
              WHEN_SHOW WITH DTOS(TMP_L_5A28.SHOW_DATE)+TMP_L_5A28.SHOW_TIME
      SELECT TMP_L_5A28
    ENDSCAN
  ENDIF
ENDIF

m.tmpArm = PADR(m.tmpArm, 5)
IF SEEK("**"+m.tmpArm)
  IF DTOS(SHOW_DATE)+SHOW_TIME <= m.now
    DO Cr_Tmp
    SCAN REST WHILE EMPTY(READ_DATE) .AND. ADDRESS_T = "*" .AND. ADDRESS = m.tmpArm ;
                    .AND. DTOS(SHOW_DATE)+SHOW_TIME <= m.now
      SELECT TMP_LIST
      APPEND BLANK
      REPLACE LINK      WITH RECNO("TMP_L_5A28"), ;
              WHEN_SHOW WITH DTOS(TMP_L_5A28.SHOW_DATE)+TMP_L_5A28.SHOW_TIME
      SELECT TMP_L_5A28
    ENDSCAN
  ENDIF
ENDIF

USE
SELECT (m.s_sav)

RETURN USED("TMP_LIST")

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Cr_Tmp       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       �������� �६������ 䠩��                        �
*�                                                                        �
*����������������������������������������������������������� 28.10.2005 ���
PROCEDURE Cr_Tmp

PRIVATE s_sav

IF USED("TMP_LIST")
  RETURN
ENDIF

m.s_sav = SELECT()

SELECT 0

CREATE DBF(m.tmpo_path+m.tmpName) ;
	(	LINK		N(10), ;
		WHEN_sHOW	C(16)  )
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_LIST
INDEX ON WHEN_SHOW TAG HRON

SELECT (m.s_sav)

RETURN