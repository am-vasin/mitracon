*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Ch_Pers      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                     �롮� ���㤭��� �।�����.                      �
*�                                                                        �
*����������������������������������������������������������� 01.09.1999 �ͼ
PROCEDURE Ch_Pers
PARAMETERS cd, fio, fm, nm, s_nm

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
USE (base_path+"DEPS") ORDER TAG CODE AGAIN ALIAS DP_9901

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS PR_9901
SET RELATION TO DEP_NO INTO DP_9901
IF cd # 0 .AND. SEEK(cd)
  SET ORDER TO TAG NAME
ELSE
  SET ORDER TO TAG NAME
  GO TOP
ENDIF

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Filter OF (menu_name) PROMPT "������"
ON PAD Filter OF (menu_name) ACTIVATE POPUP Filter

  DEFINE POPUP Filter MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Filter PROMPT "��⠭�����" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1 OF Filter DO Sw_Mode WITH "Dep"

  DEFINE BAR  2 OF Filter PROMPT "������" KEY Ctrl-A, "Shift-Tab"
  ON SELECTION BAR 2 OF Filter DO Sw_Mode WITH "Clr"

DEFINE PAD Exit OF (menu_name) PROMPT "�⪠������" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, d_name
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("FAMILY")+FSIZE("NAME")+FSIZE("S_NAME")
d_name = PADR("�� �⤥��", FSIZE("B_NAME","DP_9901"))

DO D_Wins WITH ln, wd, "����㤭��� �।�����", 1, 1
@ 1, 3 SAY "������"
@ WROWS()-3, 3 SAY " �⤥� "
what_do = "List"
ln = 0

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 10 SAY d_name
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab DO Sw_Mode WITH "Dep"
    ON KEY LABEL BackTab DO Sw_Mode WITH "Clr"
    DO Set_Key

*
*    BROWSE - ����
*
    IF "DEP" $ TAG()
      BROWSE KEY STR(ln,3) ;
             FIELDS F000 = FAMILY-(" "+NAME)-(" "+S_NAME):H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Foot()    ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS F000 = FAMILY-(" "+NAME)-(" "+S_NAME):H="", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Foot()    ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && ��ᬮ�� ᯨ᪠

    cd = CODE
    IF TYPE("fio") = "C"
      fio = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
    ENDIF
    IF TYPE("fio") = "C"
      fio = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
    ENDIF
    IF TYPE("fm") = "C"
      fm = FAMILY
    ENDIF
    IF TYPE("nm") = "C"
      nm = NAME
    ENDIF
    IF TYPE("s_nm") = "C"
      s_nm = S_NAME
    ENDIF
    EXIT

  CASE what_do = "Clr"        && ������ 䨫���

    ln = 0
    SET ORDER TO TAG NAME
    d_name = PADR("�� �⤥��", FSIZE("B_NAME","DP_9901"))
    what_do = "List"

  CASE what_do = "Dep"        && ��⠭����� 䨫���

    DO Ch_Dep WITH ln, wd, d_name
    IF ln # 0
      SET ORDER TO TAG DEP_NAME
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
SELECT DP_9901
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

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ��㥬...
*

@ WROWS()-2, 3 SAY DP_9901.B_NAME

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Set_Key      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                    ��⠭���� ��ࠡ��稪�� ��� �㪢.                    �
*�                                                                        �
*�                                                                        �
*����������������������������������������������������������� 01.09.1999 ���
PROCEDURE Set_Key

PRIVATE shb_l, shb_u, s, s1, i
shb_u = "ABCDEFGHIJKLMNOPQASTUVWXYZ��������������������������������"
shb_l = SYS(15, lwr, shb_u)

FOR i = 1 TO LEN(shb_l)
  s =  SUBSTR(shb_l,i,1)
  s1 = '"'+s+'"'
  ON KEY LABEL (s) DO Seek_ WITH &s1
ENDFOR
FOR i = 1 TO LEN(shb_u)
  s =  SUBSTR(shb_u,i,1)
  s1 = '"'+SUBSTR(shb_l,i,1)+'"'
  ON KEY LABEL (s) DO Seek_ WITH &s1
ENDFOR
*������������������������������������������������������������������������Ŀ
*�   ��楤�� Seek_        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                         ���� �� ��ࢮ� �㪢�.                         �
*�                                                                        �
*����������������������������������������������������������� 01.09.1999 ���
PROCEDURE Seek_
PARAMETERS lett

PRIVATE r_sv

r_sv = RECNO()

IF "DEP" $ TAG()
  SEEK STR(ln,3)+lett
ELSE
  SEEK lett
ENDIF
IF .NOT. FOUND()
  ?? CHR(7)
  IF BETWEEN(r_sv,1,RECCOUNT())
    GO r_sv
  ENDIF
ENDIF

RETURN