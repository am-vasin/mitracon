
*
*  ���������� ��������� ��� ���������
*

PRIVATE stat_type     && ��� ���������: 0 - �������������;
                                        1 - �����;
                                        2 - BROWSE - �������;
                                        3 - BROWSE - ������.
PRIVATE what_do       && ��� ������.
PRIVATE menu_name     && ��� ������������ ����.
PRIVATE last_mouse    && ����� ���������� ������� �������� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).
PRIVATE s_sav         && ����� ������� ������� ��� ���������� � ��������������!

*
*   ��������� ���������� ���������� ���������...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   ��������� ����� �.�., ������������� ����� � �. �.
*
* <����1 > (<�����>)
*  +- <����1 > (<�����>)
*  !  +- <����1 > (<�����>)
*  !  !  +- <����1 > (<�����>)
*  !  !  !  +- <����1 > (<�����>)
*  !  !  !  +- <����1 > (<�����>)
*  !  !  + <����1 > (<�����>)
*  !  +- <����1 > (<�����>)
*  !  +- <����1 > (<�����>)
*  +- <����1 > (<�����>)

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
-
-
-
-
-
-
-
-
-

*
*   ���������� ����������� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD <Pad name> OF (menu_name) PROMPT "<Prompt>" KEY Ctrl-A, "<Hot key>"
ON SELECTION PAD <Pad name> OF (menu_name) DO Sw_Mode WITH "<Mode name>"

DEFINE PAD <Pad name> OF (menu_name) PROMPT "<Prompt>"
ON PAD <Pad name> OF (menu_name) ACTIVATE POPUP <Popup name>

  DEFINE POPUP <Popup name> MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF <Popup name> PROMPT <Prompt> KEY Ctrl-A, "<Hot key>"
  ON SELECTION BAR 1 OF <Popup name> DO Sw_Mode WITH "<Mode name>"

DEFINE PAD Exit OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ������ �������� ����
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && ���������� ������� ����� BROWSE
m.wd = IIF(SET("CENTURY") = "ON", !!!!!!!!!,  !!!!!!!!!!)

DO D_Wins WITH m.ln, m.wd, "<Browse title>", 0, 1
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && �������� ������

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS < ���� ���� >,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && �������� ������

    DO !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
*   ������������� (�������������!) ��������� DBF-�����.
*
SELECT (m.s_sav)

RETURN

*+------------------------------------------------------------------------+
*!   ��������� Draw_Foot    ����������� ������ �����                      !
*+------------------------------------------------------------------------+
*!                                                                        !
*!                   ������ �������������� ����������.                    !
*!                                                                        !
*+---------------------------------------------------------- 20.10.1998 --+
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� �����, ������...
*

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
