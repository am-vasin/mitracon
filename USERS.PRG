*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ     เฎฃเ ฌฌ  ชฎเเฅชๆจจ แฏจแช  ฏฎซ์งฎข โฅซฅฉ.                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Users
PRIVATE wd, ln, mss, acc_list

HIDE MENU Main_M
PUSH KEY CLEAR

SELECT 0
USE S_RIGHTS
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" แจแโฅฌฅ ญฅ ฏเฅคฅซฅญฎ ญจ ฎคญฎฉ ช โฅฃฎเจจ คฎแโใฏ !"
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
acc_list(1) = PADR("ฅโ คฎแโใฏ ",FSIZE("NAME"))

USE USERS ORDER TAG USER_NAME
IF RECCOUNT() = 0
  DO Modi_User WITH "A"
ENDIF

DEFINE MENU Users IN SCREEN COLOR SCHEME 4
DEFINE PAD Corr OF Users PROMPT "ฎเเฅชๆจ๏"  KEY Tab, "Enter"
DEFINE PAD Add  OF Users PROMPT "ฎก ขซฅญจฅ" KEY Ins, "Ins"
DEFINE PAD Del  OF Users PROMPT "“ค ซฅญจฅ/ฎแแโ." KEY Del, "Del"
DEFINE PAD Exit OF Users PROMPT "๋ๅฎค"      KEY W, "Esc"
ON SELECTION PAD Corr OF Users DO Modi_User WITH "C"
ON SELECTION PAD Add  OF Users DO Modi_User WITH "A"
ON SELECTION PAD Del  OF Users DO Del_User
ON SELECTION PAD Exit OF Users DO Exit

DO Use_Dummy

SELECT USERS

wd = FSIZE("USER_NAME")+1
ln = WROWS("")-8

@ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS("")) COLOR SCHEME 13
SHOW MENU Users

ON KEY LABEL F10 DO To_Menu
ON KEY LABEL LeftMouse DO M_Proc
ON KEY LABEL Enter DO Modi_User WITH "C"
ON KEY LABEL Ins DO Modi_User WITH "A"
ON KEY LABEL Del DO Del_User

DO Def_Win WITH "W_Main", "W_Ext", "W_Int", ln, wd, "ฎซ์งฎข โฅซจ แจแโฅฌ๋"

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

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎฃเ ฌฌ  ชฎเเฅชๆจจ/คฎก ขซฅญจ๏ ฏฎซ์งฎข โฅซ๏.             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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
@ 0, 0 SAY PADC(" เโฎ็ช  ฏฎซ์งฎข โฅซ๏",wd0) COLOR SCHEME 5
@ 1, 1 TO WROWS()-1, WCOLS()-2
@ 3, c1-4 SAY "ฌ๏"
@ 3, c2-7 SAY " เฎซ์"
@ 6, c3-18 SAY " โฅฃฎเจ๏ คฎแโใฏ "

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
@ 9, c4 GET trm PICTURE "@*HT \! ‘ฎๅเ ญจโ์ Ctrl-W ;\? โช ง โ์แ๏ Esc "
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
IF s = 12 .OR. trm = 2   && โช ง ซจแ์ ฎโ จงฌฅญฅญจ๏ - ญจช ชจๅ ฏเฎขฅเฎช!
  trm = 2
  RETURN .T.
ENDIF

IF corr_type = "A" .AND. EMPTY(w_pw)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ใแโฎฉ ฏ เฎซ์? - ฅซ์ง๏!"
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
  mss(2) = CHR(0)+"’ ชฎฉ ฏ เฎซ์ ญฅ ฏฎคฎฉคฅโ. ฎ็ฅฌใ? ฅ ฏฎคฎฉคฅโ จ ขแฅ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ  ใค ซฅญจ๏/ขฎแแโ ญฎขซฅญจ๏ ฏฎซ์งฎข โฅซ๏.            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Del_User

IF EMPTY(DATE_OFF)
  REPLACE DATE_OFF WITH DATE()
ELSE
  REPLACE DATE_OFF WITH {}
ENDIF
DEACTIVATE MENU Users
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ข๋ๅฎค  จง ไใญชๆจจ.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Exit
CLOSE DATABASES
CLEAR WINDOWS
DEACTIVATE MENU Users
RELEASE MENU Users
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ฎกเ กฎโชจ ฌ๋่ชจ.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ   ชโจขจง ๆจจ ฌฅญ๎ ฏฎ F10.                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
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
