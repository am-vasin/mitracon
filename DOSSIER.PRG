*������������������������������������������������������������������������ͻ
*� ���� Dossier      ���ࠡ��稪 ���३ ��ᨭ           25.05.97 20:14:01 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�             �ணࠬ�� ��ᬮ�� � ���४樨 ���� ������.             �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Dossier
PARAMETERS c_code, flags
PRIVATE let_add, mss, sel_sav, c_nam, emp_l

let_add = .F.
IF TYPE("flags") = "C"
  let_add = "C" $ flags
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

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = "DOSS"

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
sel_sav = SELECT()
SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC999 AGAIN
IF .NOT. SEEK(c_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�����४�� ��� ������."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_sav)
  RETURN
ENDIF
c_nam = ALLTRIM(CUS_NAME)
USE (base_path+"DOSSIER") ORDER TAG CUS_HIST DESCENDING ALIAS DOSS999 AGAIN
emp_l = .F.
IF .NOT. SEEK(STR(c_code,8))
  IF .NOT. let_add
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"� ������ ���⮥ ����."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (sel_sav)
    RETURN
  ENDIF
  emp_l = .T.
ENDIF
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PERS999 AGAIN
SELECT DOSS999
*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "��ᬮ��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

IF let_add
  DEFINE PAD Add OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Ins"
  ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "Add"

  DEFINE PAD Importance OF (menu_name) PROMPT "��������" KEY Ctrl-A, "Tab"
  ON SELECTION PAD Importance OF (menu_name) Do Sw_Mode WITH "Importance"

ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, tit
ln = 15   && ������⢮ ������� ��ப BROWSE
wd = IIF(SET("CENTURY") = "ON", 52, 50)

DO D_Wins WITH ln, wd, c_nam, 0, 1
tit = PADC("���",IIF(SET("CENTURY") = "ON", 10, 8),"�")+"�"+ ;
      PADC("����ঠ���",32,"�")+"Ă�������"+ ;
what_do = IIF(emp_l, "Add", "List")

IF WEXIST("H_Wind")
  HIDE WINDOW H_Wind
ENDIF
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    IF let_add
      ON KEY LABEL Ins Do Sw_Mode WITH "Add"
      ON KEY LABEL Tab Do Sw_Mode WITH "Importance"
    ENDIF

*
*    BROWSE - ����
*
    BROWSE KEY STR(c_code,8)            ;
           FIELDS DOSS_DATE:H="",       ;
                  DOSS_TIT:32:H="",     ;
                  IMP_MARK = IIF(EMPTY(IMPORTANCE), "        ", "��᮪�� "):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Sh_User()              ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Add"    && �������� ������ � ����

    DO Cor_Doss WITH c_code, c_nam, "A"
    what_do = "List"
    IF emp_l
      IF .NOT. SEEK(STR(c_code,8))
        what_do = "Exit"
      ENDIF
    ENDIF
    emp_l = .F.
    
  CASE what_do = "Choose"    && ��ᬮ�� ����

    DO Cor_Doss WITH c_code, c_nam
    what_do = "List"
    
  CASE what_do = "Importance" && ��������

    REPLACE DOSS999.IMPORTANCE WITH IIF(DOSS999.IMPORTANCE=" ", "!", " ")
    what_do = "List"
    
  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
USE
SELECT PERS999
USE
SELECT (sel_sav)

RELEASE MENU (menu_name) EXTENDED
sel_sav = WOUTPUT()
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
IF WEXIST("H_Wind")
  SHOW WINDOW H_Wind
ENDIF
IF .NOT. EMPTY(sel_sav)
  ACTIVATE WINDOW (sel_sav)
ENDIF

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                  �ணࠬ�� ���������� �㭪� � ����.                  �
*�                                                                        �
*����������������������������������������������������� 25.05.97 20:55:00 ͼ
PROCEDURE Cor_Doss
PARAMETERS c_code, c_nam, flags
PRIVATE to_add, to_corr

mode = "V"
to_add  = .F.
to_corr = .F.
IF TYPE("flags") = "C"
  DO CASE
  CASE "A" $ flags
    to_add  = .T.
    to_corr = .T.
  CASE "C" $ flags
    to_corr = .T.
  ENDCASE
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

*����������������������������������������������������������������Ŀ
*�      ��������� ᮮ�饭�� ���������۱��������۱��������۱�      �
*�                      [ ] ��᮪�� ��������                      �
*��� ����� ᮮ�饭�� (Tab - ᫥�., Shift-Tab - �।. ���� ) ����Ŀ�
*�� ���������۱��������۱��������۱��������۱��������۱��������� ��
*�� ���������۱��������۱��������۱��������۱��������۱��������� ��
*�� ���������۱��������۱��������۱��������۱��������۱��������� ��
*�� ���������۱��������۱��������۱��������۱��������۱��������� ��
*�� ���������۱��������۱��������۱��������۱��������۱��������� ��
*�� ���������۱��������۱��������۱��������۱��������۱��������� ��
*�� ���������۱��������۱��������۱��������۱��������۱��������� ��
*�� ���������۱��������۱��������۱��������۱��������۱��������� ��
*����������������������������������������������������������������ٳ
*�                < OK Ctrl-W > < �⪠������ Esc >                �
*�                             < OK >                             �
*������������������������������������������������������������������

PRIVATE d_tit, d_mess, d_imp, ex     &&
ex   = 1             &&
IF to_add
  d_tit  = SPACE(32)
  d_mess = ""
  d_imp  = .F.
ELSE
  d_tit  = DOSS_TIT
  d_mess = DOSS_MESS
  d_imp  = .NOT. EMPTY(IMPORTANCE)
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 16, 68, c_nam

DO Sun_Bord WITH 4, 2, 13, 65, " ����� ᮮ�饭�� (Tab - ᫥�., Shift-Tab - �।. ���� ) "

*------------------------------------------------------------------------
*      ���� ����� ������
*

@ 2,  8 SAY "��������� ᮮ�饭��" GET d_tit WHEN to_corr
@ 3, 24 GET d_imp PICTURE "@*C ��᮪�� ��������" WHEN to_corr

IF to_corr
  @ 5, 4 EDIT d_mess SIZE 8, 60
  @ 14, 18 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "
ELSE
  @ 5, 4 EDIT d_mess SIZE 8, 60 NOMODIFY
  @ 14, 31 GET ex PICTURE "@*HT \ OK "
ENDIF

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                            ��室 �� ���.                             �
*�                                                                        �
*����������������������������������������������������� 25.05.97 21:31:28 ͼ
PROCEDURE Tst_Blank
PRIVATE mss

IF READKEY() % 256 = 12 .OR. ex = 2 .OR. .NOT. to_corr
  RETURN .T.
ENDIF

IF EMPTY(d_tit)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����饭�� ���� ���-� �������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(d_mess)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� � 祬, ᮡ�⢥���, �� ��� ��������?"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF to_add
  APPEND BLANK
ENDIF
REPLACE CUS_CODE   WITH c_code,     ;
        DOSS_DATE  WITH DATE(),     ;
        DOSS_TIT   WITH d_tit,      ;
        DOSS_MESS  WITH d_mess,     ;
        IMPORTANCE WITH IIF(d_imp, "!", " "), ;
        WHO        WITH user,       ;
        WHERE      WITH arm
RETURN .T.

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                         ���ଠ樮���� ��ப�.                         �
*�                                                                        �
*����������������������������������������������������� 25.05.97 22:16:20 ͼ
PROCEDURE Sh_User
PRIVATE u_name

ACTIVATE WINDOW (win_name+"_M")
IF .NOT. SEEK(WHO, "PERS999") .OR. WHO = 0
  u_name = IIF(WHO = 0, "��� ����!", "")
ELSE  
  u_name = ALLTRIM(PERS999.FAMILY)+" "+LEFT(PERS999.NAME,1)+"."+LEFT(PERS999.S_NAME,1)+"."
ENDIF
u_name = PADR(u_name, 29)
@ WROWS()-2, 3 SAY "���ଠ�� ���� "
@ ROW(), COL() SAY u_name COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS())

RETURN .T.