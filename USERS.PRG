*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�     蹍������ ��玻オ罔� 甎�瓷� ���讌���皀�ォ.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Users
PRIVATE wd, ln, mss, acc_list

HIDE MENU Main_M
PUSH KEY CLEAR

SELECT 0
USE S_RIGHTS
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� 瓱痰ガ� �� �爛ぅ�キ� �� �き�� ��皀��爬� ぎ痰祚�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  POP KEY
  RETURN
ENDIF

DIMENSION acc_list(RECCOUNT()+1)
SCAN
  acc_list(RECNO()+1) = NAME
ENDSCAN
acc_list(1) = PADR("�モ ぎ痰祚�",FSIZE("NAME"))

USE USERS ORDER TAG USER_NAME
IF RECCOUNT() = 0
  DO Modi_User WITH "A"
ENDIF

DEFINE MENU Users IN SCREEN COLOR SCHEME 4
DEFINE PAD Corr OF Users PROMPT "��玻オ罔�"  KEY Tab, "Enter"
DEFINE PAD Add  OF Users PROMPT "����←キ┘" KEY Ins, "Ins"
DEFINE PAD Del  OF Users PROMPT "����キ┘/��瘁�." KEY Del, "Del"
DEFINE PAD Exit OF Users PROMPT "�諷��"      KEY W, "Esc"
ON SELECTION PAD Corr OF Users DO Modi_User WITH "C"
ON SELECTION PAD Add  OF Users DO Modi_User WITH "A"
ON SELECTION PAD Del  OF Users DO Del_User
ON SELECTION PAD Exit OF Users DO Exit

DO Use_Dummy

SELECT USERS

wd = FSIZE("USER_NAME")+1
ln = WROWS("")-8

@ 0, 0 SAY PADL("�キ� F10 ",WCOLS("")) COLOR SCHEME 13
SHOW MENU Users

ON KEY LABEL F10 DO To_Menu
ON KEY LABEL LeftMouse DO M_Proc
ON KEY LABEL Enter DO Modi_User WITH "C"
ON KEY LABEL Ins DO Modi_User WITH "A"
ON KEY LABEL Del DO Del_User

DO Def_Win WITH "W_Main", "W_Ext", "W_Int", ln, wd, "���讌���皀�� 瓱痰ガ�"

IF RECCOUNT() # 0

  ACTIVATE WINDOW W_Ext
  BROWSE FIELDS f00 = IIF(EMPTY(DATE_OFF), " ", "")+USER_NAME, DUMMY.F  ;
         FREEZE DUMMY.F NOMENU NORGRID WINDOW W_Int IN WINDOW W_Ext

ENDIF

CLOSE DATABASES
CLEAR WINDOWS
POP KEY
DEACTIVATE MENU Users
RELEASE MENU Users

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍������ ��玻オ罔�/ぎ��←キ�� ���讌���皀��.             �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Modi_User
PARAMETER corr_type

PRIVATE l0, ln0, c0, wd0, mss, i, c1, c2, c3, c4, wd1
PRIVATE trm, w_pw, w_rg, s

PUSH KEY CLEAR

DEFINE WINDOW H_Menu FROM 0, 0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW H_Menu

wd1 = LEN(acc_list(1))
ln0 = 11
wd0 = MAX(22+FSIZE("USER_NAME")+FSIZE("PASSWORD"), 28+wd1 )

c1 = FLOOR((wd0-14-FSIZE("USER_NAME")-FSIZE("PASSWORD"))/2)
c1 = c1+4
c2 = c1+10+FSIZE("USER_NAME")
c3 = FLOOR((wd0-20-wd1)/2)
c3 = c3+18
c4 = FLOOR((wd0-39)/2)
l0 = CEILING((WROWS("")-ln0)/2)
c0 = FLOOR((WCOLS("")-wd0)/2)

DEFINE WINDOW User FROM l0, c0 TO l0+ln0-1, c0+wd0-1 SHADOW NONE COLOR SCHEME 13
ACTIVATE WINDOW User
@ 0, 0 SAY PADC("��珥�腦� ���讌���皀��",wd0) COLOR SCHEME 5
@ 1, 1 TO WROWS()-1, WCOLS()-2
@ 3, c1-4 SAY "���"
@ 3, c2-7 SAY "��牀��"
@ 6, c3-18 SAY "��皀��爬� ぎ痰祚�"

w_pw = SPACE(FSIZE("PASSWORD"))
IF corr_type = "A"
  SCATTER MEMVAR BLANK
  m.user_sys = sys_char
  w_rg = 1
ELSE
  SCATTER MEMVAR
  w_rg = m.s_rights+1
ENDIF
trm = 1

@ 3, c1 GET m.user_name
@ 3, c2 GET w_pw VALID V_Func()
@ 5, c3 GET w_rg PICTURE "@^" FROM acc_list SIZE 3, wd1
@ 9, c4 GET trm PICTURE "@*HT \! ��縲���碎 Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE VALID V_Func()

IF trm = 1
  IF corr_type = "A"
    APPEND BLANK
    SELECT 0
    USE USERS ORDER TAG USER_CODE ALIAS U000 AGAIN
    GO BOTT
    IF BOF() .OR. EOF()
      m.user_code = 1
    ELSE
      m.user_code = U000.USER_CODE+1
    ENDIF
    USE
    SELECT USERS
  ENDIF
  IF .NOT. EMPTY(w_pw)
    w_pw = SYS(15,lwr,w_pw)
    DO SetPrice WITH "superamv", w_pw
    m.password = w_pw
  ENDIF
  m.s_rights = w_rg-1
  GATHER MEMVAR
ENDIF
RELEASE WINDOW User
RELEASE WINDOW H_Menu
POP KEY
DEACTIVATE MENU Users
RETURN

****************************************************************************
PROCEDURE V_Func
PARAMETERS tp

PRIVATE mss, s, rc

s = MOD(READKEY(),256)
IF s = 12 .OR. trm = 2   && �皖�����瘡 �� ├�キキ�� - �┴���� �牀▲牀�!
  trm = 2
  RETURN .T.
ENDIF

IF corr_type = "A" .AND. EMPTY(w_pw)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"鉑皰� ��牀��? - �カ讌�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
rc = RECNO()
s = SYS(15, lwr, w_pw)
DO SetPrice WITH "superamv", s
LOCATE FOR s = PASSWORD .AND. (RECNO() # rc .OR. corr_type = "A")
GO rc
IF FOUND()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����� ��牀�� �� ��ぎ�ぅ�. ��腑��? �� ��ぎ�ぅ� � ≡�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�            蹍������ 磴��キ��/〓瘁����←キ�� ���讌���皀��.            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Del_User

IF EMPTY(DATE_OFF)
  REPLACE DATE_OFF WITH DATE()
ELSE
  REPLACE DATE_OFF WITH {}
ENDIF
DEACTIVATE MENU Users
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ �諷��� ├ 粃��罔�.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Exit
CLOSE DATABASES
CLEAR WINDOWS
DEACTIVATE MENU Users
RELEASE MENU Users
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                   蹍������ �÷�｀皖� �謌��.                          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE M_Proc

IF MROW("") = 0
  ACTIVATE MENU Users
  DEACTIVATE MENU Users
ELSE
  IF WEXIST("W_Ext")
    IF MROW("W_Ext") >= 0 .AND. MCOL("W_Ext") >= 0
      KEYBOARD CHR(13)
    ENDIF
  ENDIF
ENDIF

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�            蹍������ ��皋※��罔� �キ� �� F10.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE To_Menu

ON KEY LABEL F10
ON KEY LABEL LeftMouse
ON KEY LABEL Enter
ON KEY LABEL Ins
ON KEY LABEL Del
ACTIVATE MENU Users
ON KEY LABEL F10 DO To_Menu
ON KEY LABEL LeftMouse DO M_Proc
ON KEY LABEL Enter DO Modi_User WITH "C"
ON KEY LABEL Ins DO Modi_User WITH "A"
ON KEY LABEL Del DO Del_User

RETURN
