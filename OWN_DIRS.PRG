*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Own_Dirs     ��о�｀砒┴ ��むォ ��瓱�           10.03.98 12:36:46 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�            ����痰�←キ┘ ��珥�� �����←キ�鍖 �ョ�皋.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Own_Dirs
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).
PRIVATE pt_ok         && ��ぅ爨�� �� OWN_DIRS ���� PRNTYPE
*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

DO Use_Dummy

SELECT 0
IF .NOT. File_O(base_path+"PRN_DIRS.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 甌Г�� �♂┤ 甎�甌� �����←キ┤ �ョ�皋!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
USE (base_path+"PRN_DIRS.DBF")
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"｡薑� 甎�甌� �����←キ┤ �ョ�皋 �竅�!"
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
  mss(2) = CHR(0)+"�� 甌Г�� �♂┤ 甎�甌� 竅����〓� �爬�皀牀�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
USE (base_path+"PRNSETUP.DBF")
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"｡薑� 甎�甌� 竅����〓� �爬�皀牀� �竅�!"
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
  * �� 祠������� ≡� �爬�皀琺 皋�� 1
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
    *  �矗礫矣�� OWN_DIRS � OWN_STR
	COPY STRUCTURE EXTENDED TO tmpo_path+'own_str'
	SELECT 0
	USE (tmpo_path+'own_str.DBF')
	* �甎��←錺� 痰珮�矣珮 - ぎ��←錺� �ュ����鉗ゥ ����
	APPEND BLANK
	REPLACE Field_Name with 'PRNTYPE', Field_type with 'N', Field_Len with 3, Field_dec with 0
	USE
	* ��Г�ガ ��≪� OWN_DIRS � ぎ��←キ�覓 ���ガ
	CREATE tmpo_path+'own_new' FROM tmpo_path+'own_str.DBF'
	* ������錺� ����覓� ├ 痰�牀� OWN_DIRS
	APPEND FROM prmo_path+'own_dirs'
	SELECT OWN_DIRS
	USE
	* "�ムエ�キ��襤�ガ" 
	ERASE prmo_path+'OWN_DIRS.DBF'
	SELECT OWN_NEW
	COPY TO prmo_path+'OWN_DIRS'
	USE prmo_path+'OWN_DIRS'
	REPLACE ALL PRNTYPE WITH 1 && �� 祠������� ≡� �爬�皀琺 皋�� 1
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
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "��玻オ皋牀��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = RECCOUNT("PRN_DIRS")   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("NAME","PRN_DIRS")+  ;
     FSIZE("PORT_NAME","OWN_DIRS")+  ;
     FSIZE("PR_NAME","PRNSETUP")+2

SELECT OWN_DIRS
SET RELATION TO RECNO() INTO PRN_DIRS
SET RELATION TO PRNTYPE INTO PRNSETUP ADDITIVE 
* �錣�� � ���ム��� 皋��� ��Б����

DO D_Wins WITH ln, wd, "�����←キ�� �ョ�皋", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
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

  CASE what_do = "Corr"    && ��玻オ罔�

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

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                   ��玻オ罔�/ぎ��←キ┘ �����←キ��.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 10.03.98 13:00:42 夕
PROCEDURE Corr_Dir
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�            ��珥 ....:....!              �
*�              敖陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �┓ �爬�皀�� �....:....!....:....!....� �
*�              青陳陳陳陳陳陳陳陳陳陳陳潰 �
*�    < OK Ctrl-W > < �皖���碎瘴 Esc >     �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PRIVATE ex, pt, nt, ar_pty, pty
ex = 1             &&
pt = PORT_NAME
nt = NOTE
pty = PRNTYPE

SELECT PRNSETUP
COPY TO ARRAY ar_pty FIELDS PR_NAME
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 45, ALLTRIM(PRN_DIRS.NAME)
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 4, 3 SAY "�┓ �爬�皀��"
@ 2,13 SAY "��珥" GET pt VALID T_Port()
@ 3, 16 GET pty PICTURE "@^" FROM ar_pty SIZE 1, 24
@ 6, 4 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE && VALID T_Port()

SELECT OWN_DIRS
IF ex = 1
  REPLACE PORT_NAME WITH pt, PRNTYPE WITH pty
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                       � �� �竅皰� �� ━� ��珥�?!                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 10.03.98 13:08:10 夕
PROCEDURE T_Port

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(pt)
  pt = "PRN:"
  SHOW GET pt
ENDIF

RETURN .T.
