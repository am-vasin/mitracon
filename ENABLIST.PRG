*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� EnabList     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�   ���᮪ ࠡ��� ����, ����㯭�� ���짮��⥫� ��� ������/��ᯮ��.    �
*�                                                                        �
*����������������������������������������������������������� 15.09.2006 �ͼ
PROCEDURE EnabList
PARAMETERS prmUser, prmArmN

PRIVATE tmpUser, mss

m.tmpUser = IIF(EMPTY(m.prmUser), m.user, m.prmUser)
USE (m.base_path+"LET_LIST") SHARED AGAIN ALIAS LL_6915 ORDER TAG USER_CODE IN 0
IF .NOT. SEEK(STR(m.tmpUser), "LL_6915")
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��� �� ����㯭� ᯨ᪨ �� ������ ࠡ�祣� ����"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN LL_6915
  RETURN "   "
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
USE (m.base_path+"ARM_LST") SHARED AGAIN ALIAS ARM_6915 ORDER TAG NAME
SET RELATION TO STR(m.tmpUser)+ARM_C INTO LL_6915
SET FILTER TO FOUND("LL_6915")
GO TOP
IF BOF() .OR. EOF()
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"��� �� ����㯭� ᯨ᪨ �� ������ ࠡ�祣� ����"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN LL_6915
  USE IN ARM_6915
  SELECT (m.s_sav)
  RETURN "   "
ENDIF

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, retV
m.retV = "   "
m.ln = WROWS("")-7   && ������⢮ ������� ��ப BROWSE
m.wd = 16

DO D_Wins WITH m.ln, m.wd, "����. ࠡ�稥 ����", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS ARM_N:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && ��ᬮ�� ᯨ᪠

    m.retV = ARM_C
    IF TYPE("m.prmArmN") = "C"
      m.prmArmN = ARM_N
    ENDIF
    EXIT
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
USE IN LL_6915
USE IN ARM_6915
SELECT (m.s_sav)

RETURN m.retV