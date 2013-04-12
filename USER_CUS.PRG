*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� User_Cus     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                       ���᮪ �����⮢ ��������                        �
*�                                                                        �
*����������������������������������������������������������� 28.12.2000 �ͼ
PROCEDURE User_Cus
PARAMETERS super

PRIVATE curUserC, tmpName, curUserN, s_sav, mss, patt
patt = ""

tmpName = SYS(3)
curUserC = user
s_sav = SELECT()
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS P_0C28
SEEK user
curUserN = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
USE
CREATE DBF (tmpo_path+tmpName) ;
   ( CUS_CODE  N( 8), CUS_NAME C(50) )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_LIST
INDEX ON CUS_CODE TAG CUS_CODE
INDEX ON SYS(15, lwr, CUS_NAME) TAG CUS_NAME ADDITIVE

DO Prp_List

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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
SELECT 0
DO Use_Dummy
SELECT TMP_LIST

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF super
  DEFINE PAD Manager OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Tab"
  ON SELECTION PAD Manager OF (menu_name) DO Sw_Mode WITH "Manager"
ENDIF

DEFINE PAD Seek OF (menu_name) PROMPT "����" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Add OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "�������" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 50

DO D_Wins WITH ln, wd, "���᮪ �����⮢ ��������", 1, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M")
    @ 1, 2 SAY "�������� "+PADR(curUserN, 30)
    DO Prp_Nav_1
    ON KEY LABEL Del DO Sw_Mode WITH "Del"
    ON KEY LABEL Ins DO Sw_Mode WITH "Add"
    ON KEY LABEL F7  DO Sw_Mode WITH "Seek"
    IF super
      ON KEY LABEL Tab DO Sw_Mode WITH "Manager"
    ENDIF

*
*    BROWSE - ����
*
    BROWSE FIELDS CUS_NAME,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  CASE what_do = "Del"    && ����塞

    IF .NOT. BOF() .AND. .NOT. EOF()
      DELETE
    ENDIF
    what_do = "List"

  CASE what_do = "Add"    && ������塞

    DO Add_Cus
    what_do = "List"

  CASE what_do = "Manager"    && �롮� ��������

    DO C_Manager
    what_do = "List"

  CASE what_do = "Seek"      && ����

    patt = Get_Shab(patt, "������ 蠡��� ��� ���᪠")
    DO Seek_Cus WITH patt
    what_do = "List"

  OTHERWISE

    DO Save_List
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
SELECT TMP_LIST
tmpName = DBF()
USE
DELETE FILE (tmpName)
DELETE FILE (LEFT(tmpName, LEN(tmpName)-3)+"cdx")
RETURN
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prp_List     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                �����⮢�� ᯨ᪠ �����⮢ ��� ��������                �
*�                                                                        �
*����������������������������������������������������������� 28.12.2000 ���
PROCEDURE Prp_List

SELECT TMP_LIST
ZAP
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG WHO_NEW AGAIN ALIAS ACC_0C28
SEEK curUserC
SCAN REST WHILE WHO_NEW = curUserC
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE CUS_CODE WITH ACC_0C28.CUS_CODE, ;
          CUS_NAME WITH ACC_0C28.CUS_NAME
  SELECT ACC_0C28
ENDSCAN
USE

SELECT TMP_LIST
GO TOP
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Add_Cus      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       �������� ������ � ᯨ᮪                        �
*�                                                                        �
*����������������������������������������������������������� 28.12.2000 ���
PROCEDURE Add_Cus

PRIVATE s_sav, skp

s_sav = SELECT()
skp = .F.
SELECT 0
DO Use_Dummy
SELECT 0
DO Use_Link
SELECT 0
USE (base_path+"ACCOUNT") AGAIN ORDER TAG CUS_NAME
IF Custs("", .F., .F., .F., sys_char)
  SELECT TMP_LIST
  SET ORDER TO TAG CUS_CODE
  IF .NOT. SEEK(ACCOUNT.CUS_CODE)
    IF .NOT. EMPTY(ACCOUNT.WHO_NEW) .AND. ACCOUNT.WHO_NEW # curUserC
      SELECT 0
      USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS PER_0C29
      SEEK ACCOUNT.WHO_NEW
      PRIVATE mss
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"�⨬ �����⮬ 㦥 ���������� "+ALLTRIM(FAMILY)+" "+ ;
               LEFT(NAME,1)+" "+LEFT(S_NAME,1)
      mss(3) = CHR(0)+"  ���塞 ��������? [ �� ] [ ��� ]"
      mss(4) = ""
      skp = Out_Mess(7, "mss") = 2
      USE
      SELECT TMP_LIST
    ENDIF
    IF .NOT. skp
      APPEND BLANK
      REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE, ;
              CUS_NAME WITH ACCOUNT.CUS_NAME
    ENDIF
  ENDIF
  SET ORDER TO TAG CUS_NAME
ENDIF
SELECT ACCOUNT
USE
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Seek_Cus     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ���⥪��� ���� ������.                       �
*�                                                                        �
*����������������������������������������������������������� 26.12.2000 ���
PROCEDURE Seek_Cus
PARAMETERS patt

IF EMPTY(patt)
  RETURN
ENDIF

DO Prp_Seek WITH patt
DO Choose_C

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prp_Seek     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      �⡮� ����権 �� ���⥪���.                       �
*�                                                                        �
*����������������������������������������������������������� 26.12.2000 ���
PROCEDURE Prp_Seek
PARAMETERS patt

PRIVATE s_sav, tmpName, r_sav, patt_w
patt_w = SYS(15, lwr, patt)
s_sav = SELECT()
SELECT 0
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ;
  ( LINK     N(10), ;
    CUS_NAME C(50)  )
USE (tmpo_path+tmpName) ALIAS FOUND

SELECT TMP_LIST
r_sav = RECNO()
SCAN
  IF patt $ SYS(15, lwr, CUS_NAME)
    SELECT FOUND
    APPEND BLANK
    REPLACE LINK     WITH RECNO("TMP_LIST"), ;
            CUS_NAME WITH TMP_LIST.CUS_NAME
    SELECT TMP_LIST
  ENDIF
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Choose_C     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�               �롮� ������ �� ��࠭���� �� ���⥪���.                �
*�                                                                        �
*����������������������������������������������������������� 26.12.2000 ���
PROCEDURE Choose_C

PRIVATE tmpName, mss

SELECT FOUND
tmpName = DBF()
*
*    � �� �� ����稪?
IF RECCOUNT("FOUND") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��祣� �� �������..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE(tmpName)        && �ਡ�ઠ...
  SELECT TMP_DEM
  RETURN
ENDIF
GO TOP
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
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = MIN(WROWS("")-11, RECCOUNT())   && ������⢮ ������� ��ப BROWSE
wd = 50
DO D_Wins WITH ln, wd, "������� ���᪠", 0, 0
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
    BROWSE FIELDS CUS_NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"	&& ���� ��ࠫ...

    GO FOUND.LINK IN TMP_LIST
    EXIT

  OTHERWISE					&& ���� �⪠�����

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
DELETE FILE(tmpName)        && �ਡ�ઠ...
SELECT TMP_LIST

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Save_List    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ���࠭���� ᯨ᪠.                           �
*�                                                                        �
*����������������������������������������������������������� 29.12.2000 ���
PROCEDURE Save_List

PRIVATE mss, s_sav

DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"���࠭�� ���������? [ �� ] [ ��� ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 2
  RETURN
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG WHO_NEW AGAIN ALIAS CUS_0C29
DO WHILE SEEK(CurUserC)
  REPLACE WHO_NEW WITH 0
ENDDO
SET ORDER TO TAG CUS_CODE
SELECT TMP_LIST
SCAN
  SELECT CUS_0C29
  SEEK TMP_LIST.CUS_CODE
  REPLACE WHO_NEW WITH CurUserC
  SELECT TMP_LIST
ENDSCAN

ZAP
SELECT CUS_0C29
USE

SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� C_Manager    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            ����� ��������                             �
*�                                                                        �
*����������������������������������������������������������� 29.12.2000 ���
PROCEDURE C_Manager

PRIVATE tmpC, tmpN, mss

tmpC = curUserC
tmpN = curUserN

DO Ch_Pers WITH tmpC, tmpN

IF tmpC # curUserC
  DO Save_List
  curUserC = tmpC
  curUserN = tmpN
  DO Prp_List
ENDIF