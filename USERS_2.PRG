*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                 ���甌� ���讌���皀�ォ 瓱痰ガ� "蹍��Θ"                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 29.01.97 17:19:41 夕
PROCEDURE Users_2
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
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*     ���甌� 甌矗磴�┴��
*
USE PERSONS ORDER TAG NAME
PRIVATE cli_c, cli_n, cli_q, cli_wd, mss
cli_q = 0
cli_wd = FSIZE("FAMILY")+6
SCAN
  cli_q = cli_q+1
  DIMENSION cli_c(cli_q), cli_n(cli_q)
  mss = ALLTRIM(FAMILY)
  IF .NOT. EMPTY(NAME)
    mss = mss+" "+LEFT(ALLTRIM(NAME),1)+". "
    IF .NOT. EMPTY(S_NAME)
      mss = mss+LEFT(ALLTRIM(S_NAME),1)+"."
    ENDIF
  ENDIF
  cli_c(cli_q) = CODE
  cli_n(cli_q) = PADR(mss, cli_wd)
ENDSCAN
RELEASE mss

USE
IF cli_q = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���甌� 甌矗磴�┴�� �竅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

RELEASE mss
USE S_RIGHTS 
PRIVATE rght_c, rght_n, rght_q, cnt
cnt = PADR("�モ ぎ痰祚�", FSIZE("NAME", "S_RIGHTS"))
rght_q = 1
DIMENSION rght_c(rght_q), rght_n(rght_q)
rght_c(rght_q) = 0
rght_n(rght_q) = cnt

SCAN
  rght_q = rght_q+1
  DIMENSION rght_c(rght_q), rght_n(rght_q)
  rght_c(rght_q) = RIGHT
  rght_n(rght_q) = NAME
ENDSCAN

IF rght_q = 1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���甌� ���� ぎ痰祚� �竅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

SET ORDER TO TAG RIGHT

DO Use_Dummy

SELECT 0
USE USERS
SCAN
  mss = ASCAN(cli_c, user_code)
  IF mss # 0
    REPLACE USER_NAME WITH cli_n(mss)
  ENDIF
ENDSCAN
SET ORDER TO TAG USER_NAME
SET RELATION TO S_RIGHTS INTO S_RIGHTS
GO TOP

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "��玻オ皋牀��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Corr"

DEFINE PAD Add  OF (menu_name) PROMPT "����※碎" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) Do Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
ON SELECTION PAD Corr OF (menu_name) Do Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = 100
wd = FSIZE("USER_NAME", "USERS")+FSIZE("NAME", "S_RIGHTS")+1

DO D_Wins WITH ln, wd, "���甌� ���讌���皀�ォ �牀������ �牀���", 1, 0
@ 1, 2 SAY PADR("���┼��",FSIZE("USER_NAME", "USERS")+1)+PADR("�牀▲�� ぎ痰祚�",FSIZE("NAME", "S_RIGHTS"))
what_do = IIF(BOF() .OR. EOF(), "Add", "list")

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   Do Sw_Mode WITH "Add"
    ON KEY LABEL Del   Do Sw_Mode WITH "Del"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS USER_NAME:H="",     ;
                  F001 = IIF(FOUND("S_RIGHTS"), S_RIGHTS.NAME, cnt):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Corr")
    ENDIF

  CASE what_do = "Corr"    && ��玻オ罔�

    DO Add_User WITH "Corr"
    what_do = "list"

  CASE what_do = "Add"     && ����←キ┘

    DO Add_User WITH "Add"
    what_do = IIF(BOF() .OR. EOF(), "Exit", "list")

  CASE what_do = "Del"     && ����キ┘

    IF RIGHTS # 0
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+ALLTRIM(USER_NAME)+" ━ゥ� ぎ痰祚 � �牀������ �����!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      REPLACE DATE_OFF WITH DATE()
    ENDIF
    what_do = IIF(BOF() .OR. EOF(), "Exit", "list")

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�              蹍������ ぎ��←キ��/��玻オ罔� ���讌���皀��               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 29.01.97 18:23:45 夕
PROCEDURE Add_User
PARAMETER mode

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

PRIVATE ex, wh, rg, pw

pw = SPACE(FSIZE("PASSWORD"))
IF mode = "Add"
  wh = 1
  rg = 1
ELSE
  wh = MAX(1, ASCAN(cli_c, USER_CODE))
  rg = MAX(1, ASCAN(rght_c, S_RIGHTS))
ENDIF
ex   = 1
*------------------------------------------------------------------------
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�      ���讌���皀��                      �牀▲�� ぎ痰祚�               �
*楽陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳笥陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖 旭旭屋旭異旭旭屋旭異旭旭屋旭異 崖 旭旭屋旭異旭旭屋旭異旭旭屋旭異旭 崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳拈陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�                           ��牀�� 旭旭屋旭                            �
*�                   < OK Ctrl-W > < �皖���碎瘴 Esc >                   �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 74, IIF(mode = "Add", "����←キ┘", "��玻オ罔�")+" ���讌���皀�� 瓱痰ガ�"
@ 2,  8 SAY "���讌���皀��"
@ 2, 42 SAY "�牀▲�� ぎ痰祚�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  2 GET wh PICTURE "@^" FROM cli_n  SIZE 3, 34 WHEN mode = "Add"
@ 3, 36 GET rg PICTURE "@^" FROM rght_n SIZE 3, 36
@ 6, 29 SAY "��牀��" GET pw
@ 7, 21 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_B()

IF ex = 1
  IF EMPTY(pw)
    pw = PASSWORD
  ELSE
    pw = SYS(15,lwr,pw)
    DO SetPrice WITH "superamv", pw
  ENDIF
  IF mode = "Add"
    APPEND BLANK
  ENDIF
  REPLACE USER_CODE WITH cli_c(wh),   ;
          USER_NAME WITH cli_n(wh),   ;
          PASSWORD  WITH pw,          ;
          S_RIGHTS  WITH rght_c(rg),  ;
          USER_SYS  WITH sys_char

ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                       蹍▲爲� �諷��� ├ ゛����.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 29.01.97 22:46:46 夕
PROCEDURE Tst_B
PRIVATE mss, rc, rc_sv, w_pw

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

rc = -1
IF mode = "Add" .AND. EMPTY(pw)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��牀�� ぎ�Ε� °碎!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ELSE
  rc = RECNO()
ENDIF

IF .NOT. EMPTY(pw)
  w_pw = SYS(15,lwr,pw)
  DO SetPrice WITH "superamv", w_pw
  rc_sv = RECNO()
  LOCATE FOR PASSWORD = w_pw .AND. RECNO() # rc
  IF rc_sv > 0 .AND. rc_sv <= RECCOUNT()
    GO rc_sv
  ENDIF
  IF FOUND()
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��� �爬ぅ矚� ���瓱���讚� ���瑙芟 〓�÷�Ε�┘ � �爬ゃ��碎 む磽�� ��牀��!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.
