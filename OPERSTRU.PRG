*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� OperStru     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�           ��ନ஢���� �������� ࠧ����� ���ᮢ�� ����権.           �
*�                                                                        �
*����������������������������������������������������������� 10/22/1998 �ͼ
PROCEDURE OperStru

PRIVATE fl_level, s_sav, what_do, titles
fl_level = 0
DIMENSION titles(1)   && ��� ��� ���ᨢ - �� �� ����, ��� �⥪ ����������
titles = "�᭮��� ࠧ����"

s_sav = SELECT()
DO Use_Dummy
SELECT 0
USE (base_path+"MM_TYPES") ORDER TAG GROUP ALIAS O8A22 AGAIN  && ����樨
SELECT 0

USE (base_path+"OPERSTRU") ORDER TAG CODE ALIAS C8A22 AGAIN   && ���筨� �����
SELECT 0
USE (base_path+"OPERSTRU") ORDER TAG PREV_LINE ALIAS V8A22 AGAIN && ����⢥��� ���४��

what_do = "Level"
DO WHILE .T.
  DO CASE
  CASE what_do = "Level"
    what_do = Level()
  CASE what_do = "TermList"
    DO TermList
    what_do = "Level"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

USE
SELECT C8A22
USE
SELECT O8A22
USE

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Level        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�               ���४�� ᯨ᪠ �㭪⮢ ⥪�饣� �஢��.                �
*�                                                                        �
*����������������������������������������������������������� 10/22/1998 ���
PROCEDURE Level

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
PRIVATE ret_v
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

DEFINE PAD Corr OF (menu_name) PROMPT "����." KEY Ctrl-A, "Tab"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Corr"

DEFINE PAD Add OF (menu_name) PROMPT "���." KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "����." KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Next OF (menu_name) PROMPT "����. �஢���" KEY Ctrl-A, "Enter"
ON SELECTION PAD Next OF (menu_name) DO Sw_Mode WITH "Next"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, mss, rc_sav, w
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, titles(ALEN(titles)), 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Corr"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Del"

*
*    BROWSE - ����
*
    BROWSE KEY fl_level FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Next")
    ENDIF

  CASE what_do = "Corr"    && ���४�஢���

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Modi_Line
    ENDIF
    what_do = "List"

  CASE what_do = "Add"     && ��������

    DO Modi_Line WITH .T.
    what_do = "List"

  CASE what_do = "Del"     && �������

    IF EOF() .OR. BOF()
      what_do = "List"
      LOOP
    ENDIF

    IF IS_TERM
      IF SEEK(STR(CODE), "O8A22")
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"��� ࠧ��� ᮤ�ন� ����樨!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        LOOP
      ENDIF
    ELSE
      w = CODE
      rc_sav = RECNO()
      IF SEEK(w)
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"��� ࠧ��� ᮤ�ন� ���ࠧ����!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        GO rc_sav
        LOOP
      ENDIF
      GO rc_sav
    ENDIF
    REPLACE CODE WITH 0
    SEEK fl_level
    what_do = "List"

  CASE what_do = "Next"    && �� ᫥���騩 �஢���

    IF .NOT. BOF() .AND. .NOT. EOF()
      fl_level = CODE
      DIMENSION titles(ALEN(titles)+1)
      titles(ALEN(titles)) = ALLTRIM(NAME)
      ret_v =IIF(IS_TERM, "TermList", "Level")
      EXIT
    ELSE
      what_do = "List"
    ENDIF

  OTHERWISE

    IF fl_level = 0
      ret_v = "Exit"
    ELSE
      = SEEK(fl_level,"C8A22")
      fl_level = C8A22.PREV_LINE
      DIMENSION titles(ALEN(titles)-1)
      ret_v = "Level"
    ENDIF
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

RETURN ret_v

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Modi_Line    ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                     ���४��/���������� ࠧ����.                      �
*�                                                                        �
*����������������������������������������������������������� 10/22/1998 ���
PROCEDURE Modi_Line
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

*����������������������������������Ŀ
*� �� ��������� ࠧ���� ���������Ŀ �
*� �   ....:....!....:....!....   � �
*� �������������������������������� �
*�[ ] ����� �� ᮤ�ন� ���ࠧ������
*� < OK Ctrl-W > < �⪠������ Esc > �
*������������������������������������

PRIVATE ex, nm, sw
IF new
  nm = SPACE(24)
  sw = .F.
ELSE
  nm = NAME
  sw = IS_TERM
ENDIF
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 8, 38, IIF(new, "��������", "���४��")+" ࠧ����"
DO Sun_Bord WITH  2,  3,  4, 34, " ��������� ࠧ���� "

*------------------------------------------------------------------------
*      ���� ����� ������
*
@ 3, 7 GET nm
@ 5, 2 GET sw PICTURE "@*C ����� �� ᮤ�ন� ���ࠧ�����"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE VALID Tst_B()

IF ex = 1
  *
  * ��ࠡ��뢠�� �����
  IF new
    GO TOP IN C8A22
    IF C8A22.CODE = 0 .AND. .NOT. BOF("C8A22") .AND. .NOT. EOF("C8A22")
      GO RECNO("C8A22")
    ELSE
      APPEND BLANK
    ENDIF
    GO BOTTOM IN C8A22
    REPLACE CODE WITH MAX(C8A22.CODE+1, 1)
  ENDIF
  REPLACE NAME      WITH nm,  ;
          PREV_LINE WITH fl_level, ;
          IS_TERM   WITH sw

ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_B        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                �஢�ઠ �ࠢ��쭮�� ���������� ������.                �
*�                                                                        �
*����������������������������������������������������������� 10/22/1998 ���
PROCEDURE Tst_B

PRIVATE r_sav, mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(nm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�������� ࠧ���� �� ����� ���� �����!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF .NOT. new .AND. IS_TERM # sw
  IF .NOT. sw   && �⠫ �� ��!
    IF SEEK(STR(CODE), "O8A22")
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"������ ᮤ�ন� ����樨 � ��� ����� �� ����� ���� �������!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      RETURN .F.
    ENDIF
  ELSE
    rc_sav = RECNO()
    IF SEEK(CODE)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"������ ᮤ�ন� ���ࠧ���� � ��� ����� �� ����� ���� �������!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      GO rc_sav
      RETURN .F.
    ENDIF
    GO rc_sav
  ENDIF
ENDIF

RETURN .T.


*������������������������������������������������������������������������Ŀ
*�   ��楤�� TermList     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ���᮪ ����権 � ࠧ����.                       �
*�                                                                        �
*����������������������������������������������������������� 22.10.1998 ���
PROCEDURE TermList
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

SELECT O8A22
SET RELATION TO GROUP INTO C8A22
SET ORDER TO NAME

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Inc OF (menu_name) PROMPT "����� � ࠧ���" KEY Ctrl-A, "Enter"
ON SELECTION PAD Inc OF (menu_name) DO Sw_Mode WITH "Inc"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 54

DO D_Wins WITH ln, wd, ALLTRIM(V8A22.NAME), 0, 0
@ 1, 4 SAY " ������ ������������������ ������ "
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter DO Inc_Oper

*
*    BROWSE - ����
*
    BROWSE FIELDS F000 = IIF(GROUP = V8A22.CODE, "�", " "):1:H="",   ;
           NAME:H="", ;
           F001 = IIF(M_RUB, "��", "� �"):H="":3,  ;
           F002 = IIF(M_DIR=0, "��室" ,"���室"):6:H="", ;
           F003 = IIF(GROUP = 0 .OR. .NOT. FOUND("C8A22"), "", C8A22.NAME):24:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"

  CASE what_do = "Inc"       && ��⠢���!

    DO Inc_Oper
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
SET RELATION TO
SET ORDER TO GROUP
SELECT V8A22
fl_level = PREV_LINE
DIMENSION titles(ALEN(titles)-1)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Inc_Oper     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ��⠢�� ����樨 � ࠧ���.                       �
*�                                                                        �
*����������������������������������������������������������� 22.10.1998 ���
PROCEDURE Inc_Oper

REPLACE GROUP WITH V8A22.CODE
RETURN