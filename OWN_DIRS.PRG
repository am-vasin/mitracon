*������������������������������������������������������������������������ͻ
*� ���� Own_Dirs     ���ࠡ��稪 ���३ ��ᨭ           10.03.98 12:36:46 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�            �����⠢����� ���⮢ ���ࠢ����� ����.                   �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Own_Dirs
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
PRIVATE pt_ok         && ����ন� �� OWN_DIRS ���� PRNTYPE
*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

DO Use_Dummy

SELECT 0
IF .NOT. File_O(base_path+"PRN_DIRS.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ᮧ��� ��騩 ᯨ᮪ ���ࠢ����� ����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
USE (base_path+"PRN_DIRS.DBF")
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��騩 ᯨ᮪ ���ࠢ����� ���� ����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF
SELECT 0
IF .NOT. File_O(base_path+"PRNSETUP.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ᮧ��� ��騩 ᯨ᮪ ��⠭���� �ਭ�஢!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
USE (base_path+"PRNSETUP.DBF")
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��騩 ᯨ᮪ ��⠭���� �ਭ�஢ ����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  RETURN
ENDIF

pt_ok=.F.
SELECT 0
IF .NOT. File_O(prmo_path+"OWN_DIRS.DBF")
  CREATE DBF (prmo_path+"OWN_DIRS.DBF") (PORT_NAME C(8), NOTE C(24), PRNTYPE N(3))
  APPEND BLANK
  REPLACE PRNTYPE WITH 1, PORT_NAME WITH "PRN:"
  * �� 㬮�砭�� �� �ਭ��� ⨯� 1
ELSE
  USE (prmo_path+"OWN_DIRS.DBF")
  FOR i=1 TO FCOUNT()
    IF TRIM(FIELD(i))="PRNTYPE"
      pt_ok=.T.
    ENDIF
  ENDFOR
  IF !pt_ok
  	ERASE tmpo_path+'own_str.DBF'
	ERASE tmpo_path+'own_NEW.DBF'
    *  ������� OWN_DIRS � OWN_STR
	COPY STRUCTURE EXTENDED TO tmpo_path+'own_str'
	SELECT 0
	USE (tmpo_path+'own_str.DBF')
	* ��ࠢ�塞 �������� - ������塞 ��墠��饥 ����
	APPEND BLANK
	REPLACE Field_Name with 'PRNTYPE', Field_type with 'N', Field_Len with 3, Field_dec with 0
	USE
	* ������� ����� OWN_DIRS � ���������� �����
	CREATE tmpo_path+'own_new' FROM tmpo_path+'own_str.DBF'
	* ������塞 ����묨 �� ��ன OWN_DIRS
	APPEND FROM prmo_path+'own_dirs'
	SELECT OWN_DIRS
	USE
	* "��२�����뢠��" 
	ERASE prmo_path+'OWN_DIRS.DBF'
	SELECT OWN_NEW
	COPY TO prmo_path+'OWN_DIRS'
	USE prmo_path+'OWN_DIRS'
	REPLACE ALL PRNTYPE WITH 1 && �� 㬮�砭�� �� �ਭ��� ⨯� 1
  ENDIF	
ENDIF

SELECT PRN_DIRS
SCAN
  SELECT OWN_DIRS
  IF .NOT. BETWEEN(RECNO("PRN_DIRS"), 1, RECCOUNT())
    APPEND BLANK
  ENDIF
  IF EMPTY(PORT_NAME)
    REPLACE PORT_NAME WITH "PRN:", PRNTYPE WITH 1
  ENDIF
  SELECT PRN_DIRS
ENDSCAN

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "���४�஢���" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = RECCOUNT("PRN_DIRS")   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("NAME","PRN_DIRS")+  ;
     FSIZE("PORT_NAME","OWN_DIRS")+  ;
     FSIZE("PR_NAME","PRNSETUP")+2

SELECT OWN_DIRS
SET RELATION TO RECNO() INTO PRN_DIRS
SET RELATION TO PRNTYPE INTO PRNSETUP ADDITIVE 
* �冷� � ����ࠬ� ⨯�� ��������

DO D_Wins WITH ln, wd, "���ࠢ����� ����", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
	GO TOP
    BROWSE FIELDS PRN_DIRS.NAME:H="",         ;
           OWN_DIRS.PORT_NAME:H="",           ;
           PRNSETUP.PR_NAME:H="",             ;
           DUMMY.F:H="" FREEZE DUMMY.F        ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && ���४��

    DO Corr_Dir
    GO RECNO()
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                   ���४��/���������� ���ࠢ�����.                    �
*�                                                                        �
*����������������������������������������������������� 10.03.98 13:00:42 ͼ
PROCEDURE Corr_Dir
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

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

*�����������������������������������������Ŀ
*�            ���� ....:....!              �
*�              ������������������������Ŀ �
*� ��� �ਭ�� �....:....!....:....!....� �
*�              �������������������������� �
*�    < OK Ctrl-W > < �⪠������ Esc >     �
*�������������������������������������������

PRIVATE ex, pt, nt, ar_pty, pty
ex = 1             &&
pt = PORT_NAME
nt = NOTE
pty = PRNTYPE

SELECT PRNSETUP
COPY TO ARRAY ar_pty FIELDS PR_NAME
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 45, ALLTRIM(PRN_DIRS.NAME)
*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 4, 3 SAY "��� �ਭ��"
@ 2,13 SAY "����" GET pt VALID T_Port()
@ 3, 16 GET pty PICTURE "@^" FROM ar_pty SIZE 1, 24
@ 6, 4 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE && VALID T_Port()

SELECT OWN_DIRS
IF ex = 1
  REPLACE PORT_NAME WITH pt, PRNTYPE WITH pty
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                       � �� ���⮥ �� ��� ����?!                       �
*�                                                                        �
*����������������������������������������������������� 10.03.98 13:08:10 ͼ
PROCEDURE T_Port

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(pt)
  pt = "PRN:"
  SHOW GET pt
ENDIF

RETURN .T.