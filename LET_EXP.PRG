*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�       �ணࠬ�� �⢥ত���� ��室� � �ਥ��/���᪠ �����.           �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Let_Exp
PARAMETERS k_bill, r_bill, from_u, to_u
*
*  k_bill - ��� ����. ��� "���� �।�����" ( 䠩� CORRBILL );
*  r_bill - ��� ����. ��� "�������� ���죨" ( 䠩� CORRBILL );
*  from_u - ��� ���ᮢ�� ����樨 "������ ��������� �����" ( 䠩� MM_TYPES );
*  to_u   - ��� ���ᮢ�� ����樨 "�뤠� ��������� �����" ( 䠩� MM_TYPES ).
*
IF user = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"������, �� ��᪮� �� ����..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type   && ��� ������樨: 0 - ���⠭�����;
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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Let OF (menu_name) PROMPT "�롮�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Let OF (menu_name) Do Sw_Mode WITH "let"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

USE USERS ORDER TAG USER_NAME

SELECT 0
USE RP_MONEY ORDER TAG PREV

DO Use_Dummy

PRIVATE n_users, mss
SELECT USERS
SET RELATION TO USER_CODE INTO RP_MONEY
SET FILTER TO FOUND("RP_MONEY")
COUNT TO n_users
GO TOP

IF n_users = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ������� �������� ��। ��壠��ਥ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RELEASE MENU (menu_name) EXTENDED
  RETURN
ENDIF

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = n_users   && ������⢮ ������� ��ப BROWSE
wd = FSIZE("USER_NAME")

DO D_Wins WITH ln, wd, "���᮪ ������� ��������", 0, 0
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE FIELDS USER_NAME,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "let")
    ENDIF

  CASE what_do = "let"    && ��ᬮ�� ᯨ᪠

    DO Let
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                    ���᮪ ��࠭���� ������.                         �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Let

PRIVATE c_qnt 
DIMENSION c_qnt(1)
c_qnt = 0

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type   && ��� ������樨: 0 - ���⠭�����;
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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Let OF (menu_name) PROMPT "�⢥न��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Let OF (menu_name) Do Sw_Mode WITH "let"

DEFINE PAD Cup OF (menu_name) PROMPT "�����" KEY Ctrl-A, "F1"
ON SELECTION PAD Cup OF (menu_name) Do Sw_Mode WITH "cup"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
PRIVATE start, current, wd_rest

STORE USERS.USER_MONEY TO start, current

SELECT 0
USE CORRBILL ORDER TAG CODE

SELECT 0
DELETE FILE (tmpo_path+"TMP_LET.DBF")
DELETE FILE (tmpo_path+"TMP_LET.CDX")
CREATE DBF (tmpo_path+"TMP_LET.DBF")  ;
      ( LINK N(10), ;
        MARK C( 1) )
USE TMP_LET EXCLUSIVE
INDEX ON LINK TAG LINK

SELECT RP_MONEY

SEEK USERS.USER_CODE
SCAN REST WHILE WHO_REP = USERS.USER_CODE
  SELECT TMP_LET
  APPEND BLANK
  REPLACE LINK WITH RECNO("RP_MONEY")
  SELECT RP_MONEY
  current = current+TOTAL_R
ENDSCAN
SET RELATION TO CORR_BILL INTO CORRBILL, ;
                RECNO()   INTO TMP_LET
SEEK USERS.USER_CODE

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, tit, pic1, pic2, pic3
ln = 100          && ������⢮ ������� ��ப BROWSE
wd = IIF(SET("DATE") = "ON", 11, 9)+ ;
     FSIZE("PREV_NUM")+1+FSIZE("NAME","CORRBILL")+1+ ;
     2*FSIZE("TOTAL_R")+4

wd_rest = MIN(WCOLS("")-6-wd, FSIZE("NOTE"))
wd = wd+wd_rest
pic1 = "@Z "+REPLICATE("9", FSIZE("TOTAL_R")-3)+".99"
pic2 = "@S"+ALLTRIM(STR(wd_rest))+" "
pic3 = "@S"+ALLTRIM(STR(FSIZE("PREV_NUM")))+" "
DO D_Wins WITH ln, wd, "���� ��। ��壠��ਥ�", 1, 1
tit = "� "+  ;
      PADC("���", IIF(SET("DATE") = "ON", 10, 8))+" "+ ;
      PADC("N ��.",FSIZE("PREV_NUM"))+" "+  ;
      PADC("����. ���",FSIZE("NAME","CORRBILL"))+" "+ ;
      PADC("��室",FSIZE("TOTAL_R"))+" "+  ;
      PADC("���室",FSIZE("TOTAL_R"))+" "+  ;
      PADC("�ਬ�砭��", wd_rest)

@ 1, 2 SAY tit
DO Show_Foot
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && ��ᬮ�� ᯨ᪠

    stat_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1 Do Sw_Mode WITH "cup"

*
*    BROWSE - ����
*
    BROWSE FIELDS TMP_LET.MARK:H="",  ;
                  PREV_DAT:H="",      ;
                  f001 = LEFT(PREV_NUM,1)+ALLTRIM(SUBSTR(PREV_NUM,2)):H="":P=pic3, ;
                  CORRBILL.NAME:H="", ;
                  f002 = IIF(TOTAL_R>0, TOTAL_R, 0):H="":P=pic1,  ;
                  f003 = IIF(TOTAL_R<0,-TOTAL_R, 0):H="":P=pic1,  ;
                  NOTE:H="":R:P=pic2 FREEZE NOTE ;
           KEY USERS.USER_CODE     ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "let")
    ENDIF

  CASE what_do = "let"    && �⢥न��!

    DO Let_S
    DO Show_Foot
    what_do = "list"

  CASE what_do = "cup"    && ��᪫���� �� ���ࠬ

    = Cupury(" ", c_qnt, 0, -23)
    what_do = "list"

  OTHERWISE

    SELECT RP_MONEY
    SET RELATION TO
    DO Term_Let
    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")

SELECT CORRBILL
USE

SELECT TMP_LET
USE
DELETE FILE (tmpo_path+"TMP_LET.DBF")
DELETE FILE (tmpo_path+"TMP_LET.CDX")

SELECT USERS
RELEASE MENU (menu_name) EXTENDED

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                       ��楤�� �뢮�� �㬬.                          �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Show_Foot

PRIVATE t1, wd, t2, w_sav

w_sav = WOUTPUT()

ACTIVATE WINDOW (win_name+"_M") BOTTOM
t1 = "�㬬� �� ���. "+ALLTRIM(STR(start,12,2))
t2 = "���⮪ "+ALLTRIM(STR(current,12,2))
wd = FLOOR(WCOLS()/2)-3
t1 = PADR(t1, wd)
t2 = PADR(t2, wd)
@ WROWS()-2, 3 SAY t1+t2

IF EMPTY(w_sav)
  ACTIVATE SCREEN
ELSE
  ACTIVATE WINDOW (w_sav)
ENDIF

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                �।���⥫쭮� �⢥ত���� �ப� ����.              �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Let_S
PRIVATE mss, sel_sav

sel_sav = SELECT()
IF .NOT. EMPTY(TMP_LET.MARK)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ����⢨⥫쭮 �⬥�塞? [ �� ] [ ��� ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 1
    SELECT TMP_LET
    REPLACE MARK WITH " "
    SELECT (sel_sav)
  ENDIF
  start   = start-TOTAL_R
ELSE
  SELECT TMP_LET
  REPLACE MARK WITH "�"
  SELECT (sel_sav)
  start   = start+TOTAL_R
ENDIF

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                 �ணࠬ�� ��ࠡ�⪨ �⢥ত����.                      �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Term_Let

*
*   k_bill - �. ᯨ᮪ ��ࠬ��஢ �᭮���� �ணࠬ��
*
PRIVATE mss, nd, dt, tm
DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"  �⢥ত���? [ �� ] [ ��� ]"
mss(3) = ""

IF Out_Mess(5, "mss") = 2
  RETURN
ENDIF

SELECT 0
USE KASSA_M

SELECT 0
USE KASSA_S

SELECT TMP_LET
GO TOP

SCAN FOR .NOT. EMPTY(MARK)

  GO LINK IN RP_MONEY
  start = start+TOTAL_R
  IF RP_MONEY.CORR_BILL = k_bill     && � ����� �।����� ��� �� ���
    nd =  = Doc_Num("KASSA_M", "", arm, DATE())
    nd = sys_char+STR(nd,6)
    dt = DATE()
    tm = SECONDS()
    SELECT KASSA_M
    APPEND BLANK
*
* CODE   - ��� ���ᮢ�� ����樨 �� �������� ���죠� �㤠 �/��� ���⭮,
* C_BILL - ����. ��� "�������� ���죨".
*
    REPLACE DOC_NUM  WITH nd,      ;
            DOC_DATE WITH dt,      ;
            DOC_TIME WITH tm,      ;
            CODE     WITH IIF(RP_MONEY.TOTAL_R > 0, from_u, to_u),   ;
            REF_NUM  WITH RP_MONEY.PREV_NUM, ;
            REF_DATE WITH RP_MONEY.PREV_DAT, ;
            C_BILL   WITH r_bill,  ;
            SUM_R    WITH -RP_MONEY.TOTAL_R, ;
            NOTE     WITH USERS.USER_NAME,   ;
            WHO      WITH user,    ;
            WHERE    WITH arm
    SELECT RP_MONEY
    REPLACE MAIN_NUM  WITH nd,   ;
            MAIN_DAT  WITH dt,   ;
            MAIN_TIM  WITH tm,   ;
            WHO_GET   WITH user, ;
            WHERE_GET WITH arm
  ELSE
    SELECT RP_MONEY
    REPLACE WHO_GET   WITH user,   ;
            WHERE_GET WITH arm
  ENDIF
  SELECT TMP_LET

ENDSCAN

SELECT KASSA_M
USE

SELECT KASSA_S
USE

SELECT USERS
REPLACE USER_MONEY WITH start

RETURN