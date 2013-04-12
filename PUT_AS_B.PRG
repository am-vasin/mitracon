*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 ฅเฅค ็  กเฎญจ จ คฎซฃ  คซ๏ €‘.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Put_AS_B

PARAMETER arm_p     && (C1) ชฎค € , คซ๏ ชฎโฎเฎฃฎ ไฎเฌจเใฅโแ๏ แฏจแฎช,
                    && ฅแซจ ฎโแใโแโขใฅโ - ข๋กฎเ จง ฌฅญ๎,
                    && CHR(255) - โฅชใ้จฉ.

PRIVATE is_arm, arm_w

IF TYPE("arm_p") = "C"
  arm_w = IIF(arm_p = CHR(255), arm, arm_p)
  is_arm = .T.
ELSE
  is_arm = .F.
ENDIF

PRIVATE get_path, mss, what_do, ln, wd

HIDE MENU Main_M

DO Use_Dummy

SELECT 0
USE LIST_TIT ORDER TAG LST_ARM

SELECT 0
USE ARM_LST

IF is_arm
  LOCATE FOR ARM_C = arm_w
  IF .NOT. FOUND()
    DIMENSION mss(3)
    mss(1) = ""    
    mss(2) = CHR(0)+"ฅฏเ ขจซ์ญ๋ฉ ชฎค เ กฎ็ฅฃฎ ฌฅแโ !"    
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    CLOSE DATABASES
    RETURN
  ENDIF
  what_do = "lists"

  IF .NOT. SEEK(ARM_LST.ARM_C,"LIST_TIT")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  เ กฎ็จฌ ฌฅแโฎฌ ญฅ ็จแซจโแ๏ ญจ ฎคญฎฃฎ แฏจแช !"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    CLOSE DATABASES
    RETURN
  ENDIF
ELSE
  ln = MIN(RECCOUNT(), WROWS("")-6)
  wd = FSIZE("ARM_N")

  DO Def_Win WITH "Arms_M", "Arms_E", "Arms_I", ln, wd, "‘ฏจแฎช เ กฎ็จๅ ฌฅแโ"

  what_do = "arms"

  DEFINE MENU Put_As_Ord IN SCREEN COLOR SCHEME 4
  DEFINE PAD Choose OF Put_As_Ord PROMPT "๋กฎเ" KEY Ctrl-A, "Enter"
  DEFINE PAD Exit   OF Put_As_Ord PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"

  ON SELECTION PAD Choose OF Put_As_Ord DO Ch_Mode WITH "Put_As_Ord", "lists"
  ON SELECTION PAD Exit   OF Put_As_Ord DO Ch_Mode WITH "Put_As_Ord", "‘…!"
ENDIF

DEFINE MENU Sub_As_Ord IN SCREEN COLOR SCHEME 4
DEFINE PAD To_Fdd OF Sub_As_Ord PROMPT "๋กฎเ" KEY Ctrl-A, "Enter"
DEFINE PAD Return OF Sub_As_Ord PROMPT IIF(is_arm, "๋ๅฎค", "ฎงขเ โ") KEY Ctrl-A, "Esc"

ON SELECTION PAD To_Fdd OF Sub_As_Ord DO Ch_Mode WITH "Sub_As_Ord", "to_fdd"
ON SELECTION PAD Return OF Sub_As_Ord DO Ch_Mode WITH "Sub_As_Ord", "arms"

DO Entry_P WITH "", IIF(is_arm, "Sub_As_Ord", "Put_As_Ord")

ln = WROWS("")-6
wd = 1+FSIZE("LST_NAME","LIST_TIT")+FSIZE("LST_NUM","LIST_TIT")

DO Def_Win WITH "Sub_M", "Sub_E", "Sub_I", ln, wd, ""
HIDE WINDOW Sub_M
HIDE WINDOW Sub_E
HIDE WINDOW Sub_I

PRIVATE clr, i1, i2, s1

clr = SCHEME(13)
i1  = AT("/",clr)
s1  = LEFT(clr,i1-1)
IF RIGHT(s1,1) = "+"
  s1 = LEFT(s1,LEN(s1)-1)+"*"
ENDIF
i1 = AT("/",clr,3)+1
i2 = AT(",",clr,3)-i1
clr = STUFF(clr,i1,i2,s1)

DO WHILE what_do # "‘…!"
  DO CASE
  CASE what_do = "arms"
    SHOW MENU Put_As_Ord
    SELECT ARM_LST
    ON KEY LABEL F10 ACTIVATE MENU Put_As_Ord
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "Put_As_Ord", "Arms_"
    ACTIVATE WINDOW Arms_E
    BROWSE FIELDS ARM_N:H="" , ;
                  DUMMY.F:H=""   ;
           FREEZE DUMMY.F  ;
           NOMENU NOAPPEND NODELETE NOMODIFY NOCLEAR ;
           WINDOW Arms_I IN WINDOW Arms_E COLOR (clr)

*           NOMENU NORGRID NOAPPEND NODELETE NOMODIFY REST NOCLEAR ;
*
    ON KEY LABEL Enter
    ON KEY LABEL Home
    ON KEY LABEL End
    ON KEY LABEL LeftMouse
    ON KEY LABEL F10
    HIDE MENU Put_AS_Ord
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(70) COLOR SCHEME 13
    IF what_do = "arms"
      what_do = IIF(READKEY() % 256 = 12, "‘…!", "lists")
    ENDIF
    IF what_do = "lists" .AND. .NOT. SEEK(ARM_LST.ARM_C,"LIST_TIT")
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"  เ กฎ็จฌ ฌฅแโฎฌ ญฅ ็จแซจโแ๏ ญจ ฎคญฎฃฎ แฏจแช !"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      what_do = "arms"
    ENDIF
  CASE what_do = "lists"
    SHOW WINDOW Sub_M
    ACTIVATE WINDOW Sub_M
    @ 0, 0 SAY PADC(ALLTRIM(ARM_LST.ARM_N),WCOLS()) COLOR SCHEME 5
    SELECT LIST_TIT
    SHOW MENU Sub_AS_Ord
    ON KEY LABEL F10 ACTIVATE MENU Sub_As_Ord
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "Sub_As_Ord", "Sub_"
    ACTIVATE WINDOW Sub_E
    BROWSE KEY ARM_LST.ARM_C      ;
           FIELDS LST_NUM:H="" ,  ;
                  LST_NAME:H="" , ;
                  DUMMY.F:H=""    ;
           FREEZE DUMMY.F  ;
           NOMENU NORGRID NOAPPEND NODELETE NOMODIFY NOCLEAR ;
           WINDOW Sub_I IN WINDOW Sub_E COLOR (clr)
    ON KEY LABEL Enter
    ON KEY LABEL Home
    ON KEY LABEL End
    ON KEY LABEL LeftMouse
    ON KEY LABEL F10
    HIDE MENU Sub_AS_Ord
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(70) COLOR SCHEME 13

    IF what_do = "lists"
      what_do = IIF(READKEY() % 256 = 12, "arms", "to_fdd")
    ENDIF
    IF what_do = "arms"
      IF is_arm
        what_do = "‘…!"
      ELSE
        HIDE WINDOW Sub_M
        HIDE WINDOW Sub_E
        HIDE WINDOW Sub_I
      ENDIF
    ENDIF
  CASE what_do = "to_fdd"
    DO Send_St
    what_do = "lists"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

CLOSE DATABASES

RELEASE MENU Sub_As_Ord
DEACTIVATE MENU Sub_As_Ord
IF .NOT. is_arm
  DEACTIVATE MENU Put_As_Ord
  RELEASE MENU Put_As_Ord
  RELEASE WINDOW Arms_M
  RELEASE WINDOW Arms_E
  RELEASE WINDOW Arms_I
ENDIF

RELEASE WINDOW Sub_M
RELEASE WINDOW Sub_E
RELEASE WINDOW Sub_I

DO Exit_P WITH "", IIF(is_arm, "Sub_As_Ord", "Put_As_Ord")

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ        เฎฃเ ฌฌ  - ฎกเ กฎโ็จช ฌ๋่ชจ คซ๏ แฏจแชฎข.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE M_Proc_1
PARAMETER menu_nm, wind_nm
PRIVATE mc, mr, cr

*
*  €แจญๅเฎญญฎฅ ฌฅญ๎
*
IF MENU() == UPPER(menu_nm)
  RETURN
ENDIF

IF MROW("") = 0
  IF .NOT. MENU() == UPPER(menu_nm)
    ACTIVATE MENU (menu_nm)
  ENDIF
  RETURN
ENDIF

*
*  ชญฎ BROWSE
*
mc = MCOL(wind_nm+"E")
mr = MROW(wind_nm+"E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  แญฎขญฎฅ ฎชญฎ
*
mc = MCOL(wind_nm+"M")
mr = MROW("")
IF mc >= 0
  IF mr = WLROW(wind_nm+"E")-1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WLROW(wind_nm+"E")+WROWS(wind_nm+"E")
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฏฅเฅค ็จ แฎแโฎ๏ญจ๏ แฏจแช  ข ไ ฉซ.               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Send_St

PRIVATE f_nm, fil_arr, sel_sav, k_lst, s

PUSH KEY CLEAR

ON KEY LABEL LeftMouse DO LM_Proc

f_nm = SYS(3)

sel_sav = SELECT()
DO Wt_Mess WITH "ฎคฃฎโฎขช  แฏจแช  คซ๏ ฏฅเฅค ็จ"

SELECT 0
CREATE DBF (tmpo_path+f_nm) ;
      ( STO_CODE N( 7), ;
        PREFIX   C( 4), ;
        NAME     C(35), ;
        QTY      N( 7), ;
        QTY_DEBT N( 7), ;
        N_S      C( 1), ;
        N_SP     N( 6)  )
USE (tmpo_path+f_nm) ALIAS TMP0 EXCLUSIVE

SELECT 0
USE STOCK ORDER TAG CODE

SELECT 0
USE LIST_DET ORDER L_NAME

k_lst = STR(LIST_TIT.LST_NUM,6)

SEEK k_lst

SCAN REST WHILE k_lst = LIST_NUM FOR CODE >= 0
  IF .NOT. EMPTY(CODE)
    s = SEEK(CODE,"STOCK")
  ELSE
    s = .F.
  ENDIF
  SELECT TMP0
  APPEND BLANK
  REPLACE STO_CODE WITH LIST_DET.CODE, ;
          PREFIX   WITH IIF(s, STOCK.PREFIX, LIST_DET.PREFIX) , ;
          NAME     WITH IIF(s, STOCK.NAME,   LIST_DET.NAME),    ;
          QTY      WITH LIST_DET.QNT_REQ, ;
          QTY_DEBT WITH LIST_DET.QNT_DUE, ;
          N_S      WITH LIST_TIT.LST_ARM, ;
          N_SP     WITH LIST_TIT.LST_NUM

  SELECT LIST_DET

ENDSCAN

USE

SELECT STOCK
USE

SELECT TMP0
USE

SELECT (sel_sav)

DO Wt_Mess

DIMENSION fil_arr(1,3)
fil_arr(1,1) = tmpo_path+f_nm+".dbf"
fil_arr(1,2) = "debt_in.dbf"
fil_arr(1,3) = "‘ฎแโฎ๏ญจฅ แฏจแช  ฎโคฅซ  ง ช งฎข"

DO Sav_File WITH fil_arr

DELETE FILE (tmpo_path+f_nm+".dbf")

POP KEY

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     เฎฃเ ฌฌ  - ฎกเ กฎโ็จช ฌ๋่ชจ.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE LM_Proc

IF MROW() < 0 .OR. MCOL() < 0
  = INKEY("M")
ENDIF
RETURN
