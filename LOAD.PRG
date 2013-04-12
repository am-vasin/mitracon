*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Load     ���ࠡ��稪 ���३ ��ᨭ                          �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                            ���ᠭ�� ��㧮�.                            �
*�                                                                        �
*����������������������������������������������������������� 20.01.2000 �ͼ
PROCEDURE Load

PRIVATE path_as, mss

path_as = ALLTRIM(Get_Sys("PATH_AS"))
IF EMPTY(path_as)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��।����� ���� ������ ��㧮�."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
mss = .F.
IF TYPE("path_comm") = "C"
  mss = .NOT. EMPTY(path_comm)
ENDIF
IF .NOT. mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��।����� ���� ���� ������."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
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

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (path_as+"LOAD") ORDER TAG LD_NUM ALIAS LT_0120 AGAIN
SELECT 0
USE (path_comm+"INVOICET") ORDER TAG LD_BOX ALIAS IT_0120 AGAIN
SELECT 0
USE (path_as+"BOXNO") ORDER TAG BOXNO ALIAS BX_0120 AGAIN
SELECT 0
USE (path_as+"LOAD_D") ORDER TAG LOAD_NUM ALIAS LD_0120 AGAIN
SET RELATION TO STR(YEAR(LD_DATE),4)+STR(LD_NUM,FSIZE("LD_NUM")) ;
    INTO LT_0120
SET RELATION TO ;
    STR(YEAR(LD_DATE),4)+STR(LD_NUM)+OFFICE+STR(BOX_YEAR,4)+STR(BOXNO) ;
    INTO IT_0120 ADDITIVE
SET RELATION TO ;
    OFFICE+STR(BOX_YEAR,4)+STR(BOXNO) ;
    INTO BX_0120 ADDITIVE
*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Order OF (menu_name) PROMPT "���"
ON PAD Order OF (menu_name) ACTIVATE POPUP Order

  DEFINE POPUP Order MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Order PROMPT "�����廊��� �� ��㧠�" KEY Ctrl-A, "Ctrl-F1"
  ON SELECTION BAR 1 OF Order DO Sw_Mode WITH "OrdLoad"

  DEFINE BAR  2 OF Order PROMPT " ����廊��� �� ��஡���" KEY Ctrl-A, "F1"
  ON SELECTION BAR 2 OF Order DO Sw_Mode WITH "OrdBox"

  DEFINE BAR  3 OF Order PROMPT "\-"

  DEFINE BAR  4 OF Order PROMPT "��� ��஡��" KEY Ctrl-A, "F6"
  ON SELECTION BAR 4 OF Order DO Sw_Mode WITH "AllBox"

  DEFINE BAR  5 OF Order PROMPT " ���쪮 ��஡�� � �������묨" KEY Ctrl-A, "Shift-F6"
  ON SELECTION BAR 5 OF Order DO Sw_Mode WITH "InvBox"

  DEFINE BAR  6 OF Order PROMPT " ���쪮 �ਭ���" KEY Ctrl-A, "Ctrl-F6"
  ON SELECTION BAR 6 OF Order DO Sw_Mode WITH "GotBox"

  DEFINE BAR  7 OF Order PROMPT " ���쪮 �� �ਭ���" KEY Ctrl-A, "Alt-F6"
  ON SELECTION BAR 7 OF Order DO Sw_Mode WITH "NotGotBox"

DEFINE PAD View OF (menu_name) PROMPT "��ᬮ��"
ON PAD View OF (menu_name) ACTIVATE POPUP View

  DEFINE POPUP View MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF View PROMPT "����ন���� ��஡��" KEY Ctrl-A, "F2"
  ON SELECTION BAR 1 OF View DO Sw_Mode WITH "ViewBox"

  DEFINE BAR  2 OF View PROMPT "�ਥ���� ��⮢" KEY Ctrl-A, "F3"
  ON SELECTION BAR 2 OF View DO Sw_Mode WITH "ViewActs"

DEFINE PAD Seek OF (menu_name) PROMPT "����"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "�� ��ન஢��" KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "SeekMask"

  DEFINE BAR  2 OF Seek PROMPT "�� ����樨" KEY Ctrl-A, "Ctrl-F7"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "SeekPos"

DEFINE PAD Weight OF (menu_name) PROMPT "���" KEY Ctrl-A, "Tab"
ON SELECTION PAD Weight OF (menu_name) DO Sw_Mode WITH "Weight"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-5   && ������⢮ ������� ��ப BROWSE
wd = 75
*
*���N ��㧠�Ą�� ��㧠�Ď������N �.ă���đ�Ď��������č�����������ďਥ���Ŀ
*�XXXXXXXXXX ��.��.���� XXXXXXXX XXXX XXXX C ��.��.���� XXXXXXXXXX ��.��.�����
*���������Č�ન஢�� ��஡�� XXXXXXXXXX��������������������������������������
DO D_Wins WITH ln, wd, "���祭� ��஡��", 0, 1
@ 1, 2 SAY ;
"��N ��㧠�Ą�� ��㧠�Ď������N �.ă���đ�Ď��������č�����������ďਥ���"
what_do = "List"

GO BOTTOM
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Ctrl+F1  DO Sw_Mode WITH "OrdLoad"
    ON KEY LABEL F1       DO Sw_Mode WITH "OrdBox"
    ON KEY LABEL F6       DO Sw_Mode WITH "AllBox"
    ON KEY LABEL Shift+F6 DO Sw_Mode WITH "InvBox"
    ON KEY LABEL Ctrl+F6  DO Sw_Mode WITH "GotBox"
    ON KEY LABEL Alt+F6   DO Sw_Mode WITH "NotGotBox"
    ON KEY LABEL F2       DO Sw_Mode WITH "ViewBox"
    ON KEY LABEL F3       DO Sw_Mode WITH "ViewActs"
    ON KEY LABEL F7       DO Sw_Mode WITH "SeekMask"
    ON KEY LABEL Ctrl+F7  DO Sw_Mode WITH "SeekPos"
    ON KEY LABEL Tab      DO Sw_Mode WITH "Weight"

*
*    BROWSE - ����
*
    BROWSE FIELDS LD_0120.LD_NUM:H="",  ;
                  LD_0120.LD_DATE:H="", ;
                  LD_0120.OFFICE:H="",  ;
                  LD_0120.BOXNO:H="",   ;
                  LD_0120.BOX_YEAR:H="",;
                  URGENT = IIF(EMPTY(LT_0120.QUICKLY), " ", "�"):H="", ;
                  LT_0120.LEADTIME:H="",;
                  IT_0120.EXP_NUM:H="", ;
                  IT_0120.DATE_INC:H="",;
           DUMMY.F:H="" FREEZE DUMMY.F  ;
           WHEN Draw_Foot()    ;
           NOMODIFY ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  CASE what_do = "OrdLoad"    && ����冷��� �� ��㧠�

    SET ORDER TO LOAD_NUM
    DEFINE BAR  1 OF Order PROMPT "�����廊��� �� ��㧠�" KEY Ctrl-A, "Ctrl-F1"
    DEFINE BAR  2 OF Order PROMPT " ����廊��� �� ��஡���" KEY Ctrl-A, "F1"
    what_do = "List"

  CASE what_do = "OrdBox"     && ����冷��� �� ��஡���

    SET ORDER TO BOXNO
    DEFINE BAR  1 OF Order PROMPT " ����廊��� �� ��㧠�" KEY Ctrl-A, "Ctrl-F1"
    DEFINE BAR  2 OF Order PROMPT "�����廊��� �� ��஡���" KEY Ctrl-A, "F1"
    what_do = "List"

  CASE what_do = "AllBox"     && �� ��஡��

    DEFINE BAR  4 OF Order PROMPT "��� ��஡��" KEY Ctrl-A, "F6"
    DEFINE BAR  5 OF Order PROMPT " ���쪮 ��஡�� � �������묨" KEY Ctrl-A, "Shift-F6"
    DEFINE BAR  6 OF Order PROMPT " ���쪮 �ਭ���" KEY Ctrl-A, "Ctrl-F6"
    DEFINE BAR  7 OF Order PROMPT " ���쪮 �� �ਭ���" KEY Ctrl-A, "Alt-F6"
    SET FILTER TO
    what_do = "List"

  CASE what_do = "InvBox"     && ���쪮 ��஡�� � �������묨

    DEFINE BAR  4 OF Order PROMPT " �� ��஡��" KEY Ctrl-A, "F6"
    DEFINE BAR  5 OF Order PROMPT "����쪮 ��஡�� � �������묨" KEY Ctrl-A, "Shift-F6"
    DEFINE BAR  6 OF Order PROMPT " ���쪮 �ਭ���" KEY Ctrl-A, "Ctrl-F6"
    DEFINE BAR  7 OF Order PROMPT " ���쪮 �� �ਭ���" KEY Ctrl-A, "Alt-F6"
    SET FILTER TO FOUND("IT_0120")
    what_do = "List"

  CASE what_do = "GotBox"     && ���쪮 �ਭ���

    DEFINE BAR  4 OF Order PROMPT " �� ��஡��" KEY Ctrl-A, "F6"
    DEFINE BAR  5 OF Order PROMPT " ���쪮 ��஡�� � �������묨" KEY Ctrl-A, "Shift-F6"
    DEFINE BAR  6 OF Order PROMPT "����쪮 �ਭ���" KEY Ctrl-A, "Ctrl-F6"
    DEFINE BAR  7 OF Order PROMPT " ���쪮 �� �ਭ���" KEY Ctrl-A, "Alt-F6"
    SET FILTER TO FOUND("IT_0120") .AND. .NOT. EMPTY(IT_0120.DATE_INC)
    what_do = "List"

  CASE what_do = "NotGotBox"     && ���쪮 �� �ਭ���

    DEFINE BAR  4 OF Order PROMPT " �� ��஡��" KEY Ctrl-A, "F6"
    DEFINE BAR  5 OF Order PROMPT " ���쪮 ��஡�� � �������묨" KEY Ctrl-A, "Shift-F6"
    DEFINE BAR  6 OF Order PROMPT " ���쪮 �ਭ���" KEY Ctrl-A, "Ctrl-F6"
    DEFINE BAR  7 OF Order PROMPT "����쪮 �� �ਭ���" KEY Ctrl-A, "Alt-F6"
    SET FILTER TO FOUND("IT_0120") .AND. EMPTY(IT_0120.DATE_INC)
    what_do = "List"

  CASE what_do = "ViewBox"     && ����ন��� ��஡��

    DO ViewBox
    what_do = "List"

  CASE what_do = "ViewActs"    && �ਥ��� ����

    DO ViewAct
    what_do = "List"

  CASE what_do = "SeekMask"    && ���� �� ��થ஢��

    DO SeekMask
    what_do = "List"

  CASE what_do = "SeekPos"     && ���� �� ����樨

    DO SeekPos
    what_do = "List"

  CASE what_do = "Weight"       && ���

    IF FOUND("BX_0120")
      DO Get_Wght
    ENDIF
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
USE
SELECT LT_0120
USE
SELECT BX_0120
USE
SELECT IT_0120
USE
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Draw_Foot    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���㥬 �������⥫��� ���ଠ��.                    �
*�                                                                        �
*����������������������������������������������������������� 20.10.1998 ���
PROCEDURE Draw_Foot
PRIVATE s, p
ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*
s = " ��ન஢�� ��஡��: "+IIF(EMPTY(LD_0120.MASK), ;
        ALLTRIM(LD_0120.OFFICE)+ALLTRIM(STR(LD_0120.BOXNO)),ALLTRIM(LD_0120.MASK))
@ WROWS()-2, 3 SAY PADR(s, 35)
@ WROWS()-2, COL() SAY "���: �।�. "+TRANSFORM(BX_0120.WEIGHT,"@Z 999.9")+  ;
                       "   �筮 "+TRANSFORM(BX_0120.WEIGHT_ST,"@Z 999.9")

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� ViewBox      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          ����ন��� ��஡��.                           �
*�                                                                        �
*����������������������������������������������������������� 21.01.2000 ���
PROCEDURE ViewBox

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
DO Prep_Pos
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��஡�� ����..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"TMP_POS.DBF")
  DELETE FILE (tmpo_path+"TMP_POS.CDX")
  SELECT (s_sav)
  RETURN
ENDIF
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
wd = 48

*�� ������ ������������������������������ ���-��Ŀ
*�XXXX XXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXX 99999999�
*��������������������������������������������������

DO D_Wins WITH ln, wd, "����ন��� ��஡�� "+  ;
       LD_0120.OFFICE+" "+ALLTRIM(STR(LD_0120.BOXNO)), 0, 0
@ 1, 2 SAY "� ������ ������������������������������ ���-���"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - ����
*
    BROWSE FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="", QNT:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

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
USE
DELETE FILE (tmpo_path+"TMP_POS.DBF")
DELETE FILE (tmpo_path+"TMP_POS.CDX")

SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_Pos     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�            �����⮢�� �६������ 䠩�� ᮤ�ন���� ��஡��.            �
*�                                                                        �
*����������������������������������������������������������� 21.01.2000 ���
PROCEDURE Prep_Pos

SELECT 0
DELETE FILE (tmpo_path+"TMP_POS.DBF")
DELETE FILE (tmpo_path+"TMP_POS.CDX")
CREATE DBF (tmpo_path+"TMP_POS") ;
   (  ID     C(11),   ;
      PREFIX C( 4),   ;
      NAME   C(35),   ;
      PRODUCER C( 8), ;
      QNT    N( 8)    )
INDEX ON ID TAG ID

SELECT 0
USE (path_comm+"PURBASE") ALIAS ST_0121 ORDER TAG IDP AGAIN

SELECT 0
USE (path_comm+"ONHAND") ALIAS CNT_0121 ORDER TAG BOXNO AGAIN
SET RELATION TO PURSYS+STR(IDP) INTO ST_0121

SEEK LD_0120.OFFICE+STR(LD_0120.BOX_YEAR,4)+STR(LD_0120.BOXNO)
SCAN REST WHILE LD_0120.OFFICE = BOXOFFIC .AND. ;
                LD_0120.BOX_YEAR = BOXYEAR .AND. ;
                LD_0120.BOXNO = BOXNO
  SELECT TMP_POS
  IF .NOT. SEEK(CNT_0121.PURSYS+STR(CNT_0121.IDP))
    APPEND BLANK
    REPLACE ID       WITH CNT_0121.PURSYS+STR(CNT_0121.IDP), ;
            PREFIX   WITH ST_0121.EQVPREF, ;
            NAME     WITH ST_0121.EQVNAME, ;
            PRODUCER WITH ST_0121.EQVPROD
  ENDIF
  REPLACE QNT WITH QNT+CNT_0121.QTY
  SELECT CNT_0121
ENDSCAN
USE
SELECT ST_0121
USE
SELECT TMP_POS
INDEX ON NAME+PREFIX+PRODUCER TAG NAME FOR QNT # 0
GO TOP

RETURN


*������������������������������������������������������������������������Ŀ
*�   ��楤�� ViewAct      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                     ��ᬮ�� �ਥ���� ��⮢.                           �
*�                                                                        �
*����������������������������������������������������������� 21.01.2000 ���
PROCEDURE ViewAct

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
IF EMPTY(IT_0120.EXP_NUM) .OR. EMPTY(IT_0120.EXP_DATE)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� ��஡�� ��� ���������..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

s_sav = SELECT()

SELECT 0
DO Use_Dummy
DO Prp_Act
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� �� ������ �ਥ����� ���..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"TMP_ACT.DBF")
  DELETE FILE (tmpo_path+"TMP_ACT.CDX")
  SELECT (s_sav)
  RETURN
ENDIF
*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "����� �ਥ����� ���" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"
*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 25

*ڔ���N ���-�����Ą����Ŀ
*�XXX NNNNNNNNNN ��.��.�����
*���������������������������
GO TOP
DO D_Wins WITH ln, wd, "�ਥ��� ����", 0, 0
@ 1, 2 SAY "����N ���-�����Ą��"
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
    BROWSE FIELDS FIRM:H="", D_NUM:H="", D_DATE:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && ����� �ਥ����� ���

    DO P_Act WITH FIRM, D_NUM, D_DATE
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
USE
DELETE FILE (tmpo_path+"TMP_ACT.DBF")
DELETE FILE (tmpo_path+"TMP_ACT.CDX")

SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prp_Act      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�            �����⮢�� �६������ 䠩�� � �ਥ��묨 ��⠬�.             �
*�                                                                        �
*����������������������������������������������������������� 21.01.2000 ���
PROCEDURE Prp_Act
PRIVATE p, tmpStr, tmpKey
SELECT 0
DELETE FILE (tmpo_path+"TMP_ACT.DBF")
DELETE FILE (tmpo_path+"TMP_ACT.CDX")
CREATE DBF (tmpo_path+"TMP_ACT") ;
   (  FIRM   C( 3),   ;
      D_NUM  C(10),   ;
      D_DATE D   )

INDEX ON FIRM+STR(YEAR(D_DATE),4)+D_NUM TAG D_NUM

SELECT 0
USE (base_path+"ACT_S_T") ALIAS A_0121 ORDER TAG DOC_NUM AGAIN

SELECT 0
USE (path_comm+"INVOICED") ALIAS ID_0121 ORDER TAG EXP_NUM AGAIN

SEEK STR(YEAR(IT_0120.EXP_DATE),4)+IT_0120.EXP_NUM
SCAN REST WHILE IT_0120.EXP_DATE = EXP_DATE .AND. EXP_NUM = IT_0120.EXP_NUM ;
       FOR .NOT. EMPTY(AKT_NUM)
  tmpStr = ALLTRIM(AKT_NUM)
  DO WHILE .NOT. EMPTY(tmpStr)
    p = AT(",", tmpStr)
    IF p = 0
      EXIT
    ENDIF
    tmpKey = LEFT(tmpStr,p-1)
    tmpStr = SUBSTR(tmpStr,p+1)
    tmpKey = LEFT(tmpKey,7)+PADL(SUBSTR(tmpKey,8), 10)
    IF SEEK(tmpKey, "TMP_ACT")
      LOOP
    ENDIF
    IF .NOT. SEEK(tmpKey, "A_0121")
      LOOP
    ENDIF
    SELECT TMP_ACT
    APPEND BLANK
    REPLACE FIRM   WITH A_0121.FIRM,    ;
            D_NUM  WITH A_0121.DOC_NUM, ;
            D_DATE WITH A_0121.DOC_DATE
  ENDDO
  SELECT ID_0121
ENDSCAN
SELECT TMP_ACT
SET ORDER TO
IF RECCOUNT() # 0
  SCAN WHILE .NOT. "!" $ D_NUM
    p = RECNO()
    = SEEK(FIRM+STR(YEAR(D_DATE),4)+D_NUM, "A_0121")
    SELECT A_0121
    tmpKey = FIRM+DTOS(DOC_DATE)+DOC_NUM
    SCAN REST FOR "!" $ DOC_NUM .AND. tmpKey = FIRM+DTOS(EXP_DATE)+EXP_NUM
      SELECT TMP_ACT
      APPEND BLANK
      REPLACE FIRM   WITH A_0121.FIRM,    ;
              D_NUM  WITH A_0121.DOC_NUM, ;
              D_DATE WITH A_0121.DOC_DATE
      SELECT A_0121
    ENDSCAN
    SELECT TMP_ACT
    GO p
  ENDSCAN
ENDIF
SELECT A_0121
USE
SELECT ID_0121
USE
SELECT TMP_ACT
INDEX ON DTOS(D_DATE) TAG HRON
GO TOP

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� SeekMask     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      ���� ��஡�� �� ��ન஢��.                      �
*�                                                                        �
*����������������������������������������������������������� 22.01.2000 ���
PROCEDURE SeekMask

PRIVATE shab, o_sav, r_sav, s_sav, n_rec
shab = GetMask()
IF EMPTY(shab)
  RETURN
ENDIF

s_sav = SELECT()
SELECT LD_0120
o_sav = TAG()
r_sav = RECNO()

DO Prp_Mask

n_rec = ViewMask()
SELECT LD_0120
SET ORDER TO (o_sav)
IF BETWEEN(n_rec, 1, RECCOUNT())
  GO n_rec
ELSE
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� GetMask      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                   ���� �������� ��஡�� ��� ���᪠.                    �
*�                                                                        �
*����������������������������������������������������������� 22.01.2000 ���
PROCEDURE GetMask
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
*���� ��஡�� �� ��ન஢��
*��������������������������������������������Ŀ
*� ������ 蠡��� ��� ���᪠ 9999999999999999 �
*�                                            �
*�      < OK Ctrl-W > < �⪠������ Esc >      �
*����������������������������������������������

PRIVATE ex, sh
ex = 1
sh = SPACE(16)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 6, 48, "���� ��஡�� �� ��ન஢��"

*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 2, 3 SAY "������ 蠡��� ��� ���᪠" GET sh
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN IIF(ex = 1, ALLTRIM(sh), "")

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prp_Mask     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                             ���� ��஡��.                             �
*�                                                                        �
*����������������������������������������������������������� 22.01.2000 ���
PROCEDURE Prp_Mask

PRIVATE s

SELECT 0
DELETE FILE (tmpo_path+"LNK_0122.DBF")
CREATE DBF (tmpo_path+"LNK_0122") ( N_REF N(10))

shab = SYS(15, lwr, shab)

SELECT LD_0120
SCAN
  s = IIF(EMPTY(LD_0120.MASK),  ;
        ALLTRIM(LD_0120.OFFICE)+ALLTRIM(STR(LD_0120.BOXNO)),ALLTRIM(LD_0120.MASK))
  s = SYS(15, lwr, s)
  IF shab $ s
    SELECT LNK_0122
    APPEND BLANK
    REPLACE N_REF WITH RECNO("LD_0120")
    SELECT LD_0120
  ENDIF
ENDSCAN

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� ViewMask     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ��ᬮ�� � �롮� ��஡��.                        �
*�                                                                        �
*����������������������������������������������������������� 22.01.2000 ���
PROCEDURE ViewMask

IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ������� �� ����� ��஡��..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"LNK_0122.DBF")
  RETURN
ENDIF

PRIVATE tmpRet
tmpRet = 0
  
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

SELECT LD_0120
SET ORDER TO
SELECT LNK_0122
SET RELATION TO N_REF INTO LD_0120
*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�롮�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Cont OF (menu_name) PROMPT "����ন��� ��஡��" KEY Ctrl-A, "F2"
ON SELECTION PAD Cont OF (menu_name) DO Sw_Mode WITH "ViewBox"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-5   && ������⢮ ������� ��ப BROWSE
wd = 75
*
*���N ��㧠�Ą�� ��㧠�Ď������N �.ă���đ�Ď��������č�����������ďਥ���Ŀ
*�XXXXXXXXXX ��.��.���� XXXXXXXX XXXX XXXX C ��.��.���� XXXXXXXXXX ��.��.�����
*���������Č�ન஢�� ��஡�� XXXXXXXXXX��������������������������������������
DO D_Wins WITH ln, wd, "���祭� ��஡��", 0, 0
@ 1, 2 SAY ;
"��N ��㧠�Ą�� ��㧠�Ď������N �.ă���đ�Ď��������č�����������ďਥ���"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F2       DO Sw_Mode WITH "ViewBox"

*
*    BROWSE - ����
*
    BROWSE FIELDS LD_0120.LD_NUM:H="",  ;
                  LD_0120.LD_DATE:H="", ;
                  LD_0120.OFFICE:H="",  ;
                  LD_0120.BOXNO:H="",   ;
                  LD_0120.BOX_YEAR:H="",;
                  LD_0120.BOXNO:H="",   ;
                  URGENT = IIF(EMPTY(LT_0120.QUICKLY), " ", "�"):H="", ;
                  LT_0120.LEADTIME:H="",;
                  IT_0120.EXP_NUM:H="", ;
                  IT_0120.DATE_INC:H="",;
           DUMMY.F:H="" FREEZE DUMMY.F  ;
           WHEN Draw_Foot()    ;
           NOMODIFY ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �롮�

    tmpRet = LNK_0122.N_REF
    EXIT

  CASE what_do = "ViewBox"     && ����ন��� ��஡��

    SELECT LD_0120
    DO ViewBox
    SELECT LNK_0122
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
USE
DELETE FILE (tmpo_path+"LNK_0122.DBF")

RETURN tmpRet

*������������������������������������������������������������������������Ŀ
*�   ��楤�� SeekPos      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                           ���� �� ����樨.                            �
*�                                                                        �
*����������������������������������������������������������� 22.01.2000 ���
PROCEDURE SeekPos
PRIVATE shab, n_rec, s_sav, r_sav, o_sav

r_sav = RECNO()
o_sav = ORDER()
s_sav = SELECT()
SET ORDER TO TAG BOXNO
shab = GetPos()
IF EMPTY(shab)
  RETURN
ENDIF

DO Prp_Pos

n_rec = ViewPos()
SELECT LD_0120
IF BETWEEN(n_rec, 1, RECCOUNT())
  GO n_rec
ENDIF
SELECT (s_sav)
SET ORDER TO (o_sav)
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� GetPos      ���ࠡ��稪 ���३ ��ᨭ                       �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    ���� ⥪�� ��� ���᪠ ����樨.                     �
*�                                                                        �
*����������������������������������������������������������� 22.01.2000 ���
PROCEDURE GetPos
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

*���� ����樨
*�����������������������������������������������������Ŀ
*� ������ 蠡��� ��� ���᪠ 9999999999999999999999999 �
*�                                                     �
*�          < OK Ctrl-W > < �⪠������ Esc >           �
*�������������������������������������������������������

PRIVATE ex, sh
ex = 1
sh = SPACE(25)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 6, 57, "���� ����樨"

*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 2, 3 SAY "������ 蠡��� ��� ���᪠" GET sh
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN IIF(ex = 1, ALLTRIM(sh), "")

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prp_pos      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       �����⮢�� ᯨ᪠ ����権.                       �
*�                                                                        �
*����������������������������������������������������������� 22.01.2000 ���
PROCEDURE Prp_pos

DO Wt_Mess WITH "����..."
DELETE FILE (tmpo_path+"TMP_LIST.DBF")
DELETE FILE (tmpo_path+"TMP_LIST.CDX")
CREATE DBF (tmpo_path+"TMP_LIST") ;
  (  PREFIX   C( 4), ;
     NAME     C(35), ;
     PRODUCER C( 8), ;
     BOXNO    N( 7), ;
     OFFICE   C( 8), ;
     BOXYEAR  N( 4), ;
     REC_NO   N(10)  )
INDEX ON NAME+PREFIX+PRODUCER+STR(BOXNO)+OFFICE+STR(BOXYEAR) TAG TMP
cnt = 0
SELECT 0
USE (path_comm+"PURBASE") ALIAS ST_0122 AGAIN ORDER TAG IDP
SELECT 0
USE (path_comm+"ONHAND") ALIAS ONH_0122 AGAIN
SET RELATION TO PURSYS+STR(IDP) INTO ST_0122

SCAN
  cnt = cnt+1
*  @ WROW()-1, 0 SAY STR(cnt)
  IF .NOT. shab $ ST_0122.EQVPREF+ST_0122.EQVNAME+ST_0122.EQVPROD
    LOOP
  ENDIF
  IF SEEK(ST_0122.EQVNAME+ST_0122.EQVPREF+ST_0122.EQVPROD+ ;
          +STR(BOXNO)+BOXOFFIC+STR(BOXYEAR), "TMP_LIST")
    LOOP
  ENDIF
  IF .NOT. SEEK(ONH_0122.BOXOFFIC+STR(ONH_0122.BOXYEAR,4)+  ;
                STR(ONH_0122.BOXNO), "LD_0120")
    LOOP
  ENDIF
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE PREFIX   WITH ST_0122.EQVPREF,  ;
          NAME     WITH ST_0122.EQVNAME,  ;
          PRODUCER WITH ST_0122.EQVPROD,  ;
          BOXNO    WITH ONH_0122.BOXNO,   ;
          OFFICE   WITH ONH_0122.BOXOFFIC,;
          BOXYEAR  WITH ONH_0122.BOXYEAR, ;
          REC_NO   WITH RECNO("LD_0120")
  SELECT ONH_0122
ENDSCAN
DO Wt_Mess
USE
SELECT ST_0122
USE
SELECT TMP_LIST

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� ViewPos      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      ��ᬮ�� �⮡࠭��� ����権.                      �
*�                                                                        �
*����������������������������������������������������������� 24.01.2000 ���
PROCEDURE ViewPos

PRIVATE retVal, k00
retVal = 0
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��祣� �� 㤠���� ����..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"TMP_LIST.DBF")
  DELETE FILE (tmpo_path+"TMP_LIST.CDX")
  RETURN retVal
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

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT TMP_LIST
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
ln = MIN(WROWS("")-8,RECCOUNT())   && ������⢮ ������� ��ப BROWSE
wd = 61
*�ď��������������������������������������N ���.Ď������ă��Ŀ
*�XXXX xxxxxxxxxXxxxxxxxxxXxxxxx xxxxxxxx NNNNNNN XXXXXXXX �����
*���������������������������������������������������������������

DO D_Wins WITH ln, wd, "���᮪ �⮡࠭��� ����権", 0, 0
what_do = "List"
GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS PREFIX:H="",   ;
                  NAME:H="",     ;
                  PRODUCER:H="", ;
                  BOXNO:H="",    ;
                  OFFICE:H="",   ;
                  BOXYEAR:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �롮�

    retVal = REC_NO
    EXIT

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
USE
DELETE FILE (tmpo_path+"TMP_LIST.DBF")
DELETE FILE (tmpo_path+"TMP_LIST.CDX")

SELECT (s_sav)

RETURN retVal


*������������������������������������������������������������������������Ŀ
*�   ��楤�� Get_Wght     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         ��⠭���� ��� ��㧠.                          �
*�                                                                        �
*����������������������������������������������������������� 21.04.2000 ���
PROCEDURE Get_Wght

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

*��������������������������������Ŀ
*� �� ��� ���������������������Ŀ �
*� � �।�. 99999  ���� 999.9 � �
*� ������������������������������ �
*�< OK Ctrl-W > < �⪠������ Esc >�
*����������������������������������

PRIVATE ex, tmpWeight
ex = 1
tmpWeight = BX_0120.WEIGHT_ST
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 36, ALLTRIM(LD_0120.OFFICE)+ALLTRIM(STR(LD_0120.BOXNO))
DO Sun_Bord WITH  2,  3,  4, 32, " ��� "

*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 3,  5 SAY "�।�." GET BX_0120.WEIGHT PICTURE "@Z 999.9" WHEN .F.
@ 3, 19 SAY "����" GET tmpWeight PICTURE "@Z 999.9"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

IF ex = 1
  *
  * ��ࠡ��뢠�� �����
  REPLACE BX_0120.WEIGHT_ST WITH tmpWeight
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN