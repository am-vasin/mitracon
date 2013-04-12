*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Stations     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                            ���᮪ ��������.                            �
*�                                                                        �
*����������������������������������������������������������� 26.03.2000 �ͼ
PROCEDURE Stations
PARAMETERS cd, nm

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
IF EMPTY(cd)
  USE (base_path+"STATIONS") ORDER TAG NAME ALIAS ST_0326 AGAIN
ELSE
  USE (base_path+"STATIONS") ORDER TAG CODE ALIAS ST_0326 AGAIN
  IF SEEK(cd)
    SET ORDER TO TAG NAME
  ELSE
    SET ORDER TO TAG NAME
    GO TOP
  ENDIF
ENDIF

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Modify OF (menu_name) PROMPT "���४�஢���" KEY Ctrl-A, "Tab"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "�������", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Modify"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"

*
*    BROWSE - ����
*
    BROWSE FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && �롮�

    IF .NOT. BOF() .AND. .NOT. EOF()
      cd = CODE
      IF TYPE("nm") = "C"
        nm = NAME
      ENDIF
      EXIT
    ELSE
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"��� �� �� 祣� �롨���!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      what_do = "List"
    ENDIF
    what_do = "List"

  CASE what_do = "Modify"    && ���४��

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Modi_Item
    ENDIF
    what_do = "List"

  CASE what_do = "Add"       && ����������

    DO Modi_Item WITH .T.
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
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Modi_Item    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      ���४��/���������� �����.                      �
*�                                                                        �
*����������������������������������������������������������� 26.03.2000 ���
PROCEDURE Modi_Item
PARAMETERS new

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

*�������������������������������������Ŀ
*�                                     �
*�  �������� ....:....!....:....!....  �
*�                                     �
*�  < OK Ctrl-W > < �⪠������ Esc >   �
*���������������������������������������

PRIVATE ex, nm_w
ex = 1
nm_w = IIF(new, SPACE(24), NAME)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 7, 41, IIF(new, "����������", "���४��")+" �������"

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 4 SAY "��������" GET nm_w
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1
  IF new
    SET ORDER TO TAG CODE
    GO BOTT
    IF EOF() .OR. BOF()
      ex = "    1"
    ELSE
      ex = VAL(ALLTRIM(CODE))
      ex = STR(ex+1,5)
    ENDIF
    APPEND BLANK
    REPLACE CODE WITH ex
    SET ORDER TO TAG NAME
  ENDIF
  REPLACE NAME WITH nm_w
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_Blank    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    �ࠢ��쭮��� ���������� ������.                     �
*�                                                                        �
*����������������������������������������������������������� 26.03.2000 ���
PROCEDURE Tst_Blank

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ���-� �� ���뢠����?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.