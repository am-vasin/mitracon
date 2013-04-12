*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  เ กฎโ๋ แ ชซจฅญโ ฌจ.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Custs
PARAMETERS ext_menu, let_add, modi, stop_inf, place
*
*  ext_menu - จฌ๏ ขญฅ่ญฅฃฎ ฌฅญ๎ คซ๏ แฎๅเ ญฅญจ๏;
*  let_add  - เ งเฅ่ฅญจฅ คฎก ขซฅญจ๏;
*  modi     - เ งเฅ่ฅญจฅ ชฎเเฅชๆจจ;
*  stop_inf - .T. - ง ฏเฅโจโ์ จญไฎเฌ ๆจ๎;
*  place    - ชฎค ฏซฎ้ คชจ - ฏใแโฎ - ขแฅ ฏซฎ้ คชจ, ฎโแใโแโข. - โฎซ์ชฎ แขฎ๎
*
PRIVATE sel, ord, what_do, rc_sav, ex_val, let_flt, pl_wk, tmpRec, f_patt, sav_rc

IF TYPE("place") = "C"
  pl_wk = place
ELSE
  pl_wk = sys_char
ENDIF

sel = SELECT()
IF .NOT. stop_inf
  SELECT 0
  USE (base_path+"DOSSIER") ORDER TAG CUS_HIST DESCENDING AGAIN ALIAS DOSS_V
ENDIF
DO Use_Dummy
SELECT 0
USE (base_path+"CUS_BIL") ORDER TAG CUS_CODE ALIAS CB_0705 AGAIN
SELECT 0
USE (base_path+"CUS_TYPE") ORDER TAG CODE ALIAS CT_0705 AGAIN
SELECT 0
USE (base_path+"CLI_TYPE") ORDER TAG CUS_DEP ALIAS CLT_0705 AGAIN
SELECT ACCOUNT
ord = SET("ORDER")
IF .NOT. stop_inf
  SET RELATION TO STR(CUS_CODE,8) INTO DOSS_V
ENDIF
rc_sav = RECNO()

*
*    PAD - ฌฅญ๎ ฏฅเขฎฃฎ ใเฎขญ๏
*
DEFINE MENU Custs IN SCREEN COLOR SCHEME 4

IF modi .AND. (sys_char = "A" .OR. sale_mode = "Mitin")
  DEFINE PAD Choose OF Custs PROMPT "ฎเเฅชๆจ๏"    KEY Ctrl-E, "Enter"
  ON SELECTION PAD Choose OF Custs Do Ch_Mode WITH "Custs", "corr"
ELSE
  DEFINE PAD Choose OF Custs PROMPT "๋กฎเ"    KEY Ctrl-E, "Enter"
  ON SELECTION PAD Choose OF Custs Do Ch_Mode WITH "Custs", "exit"
ENDIF

IF .NOT. stop_inf
  DEFINE PAD Info    OF Custs PROMPT "ญไฎเฌ."
  ON PAD Info OF Custs ACTIVATE POPUP Info
  *
  *    POPUP - ฌฅญ๎ ขโฎเฎฃฎ ใเฎขญ๏
  *
  DEFINE POPUP Info MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  6 OF Info PROMPT "ซจญญฎฅ จฌ๏" && KEY Ctrl-A, "F1"
  DEFINE BAR  1 OF Info PROMPT "ฅฏฎงจโ " KEY Ctrl-A, "F1"
  DEFINE BAR  2 OF Info PROMPT "ซจฅญโ "  KEY Ctrl-A, "Ctrl-F1"
  DEFINE BAR  3 OF Info PROMPT "ฎแ์ฅ "  KEY Ctrl-A, "Ctrl-Enter"
  DEFINE BAR  4 OF Info PROMPT " แฏฎเโ "  KEY Ctrl-A, "F2"
  DEFINE BAR  5 OF Info PROMPT "แโฎเจ๏ คฅฏฎงจโ  "  KEY Ctrl-A, "Shift-F1"
  DEFINE BAR  7 OF Info PROMPT "ฎค ชซจฅญโ  "

  ON SELECTION BAR 6 OF Info DO Ch_Mode WITH "Custs", "longname"
  ON SELECTION BAR 1 OF Info DO Ch_Mode WITH "Custs", "info"
  ON SELECTION BAR 2 OF Info DO Ch_Mode WITH "Custs", "c_info"
  ON SELECTION BAR 3 OF Info DO Ch_Mode WITH "Custs", "dossier"
  ON SELECTION BAR 4 OF Info DO Ch_Mode WITH "Custs", "pasport"
  ON SELECTION BAR 5 OF Info DO Ch_Mode WITH "Custs", "acc_hist"
  ON SELECTION BAR 7 OF Info DO Ch_Mode WITH "Custs", "acc_code"

ENDIF

DEFINE PAD M_Seek   OF Custs PROMPT "ฎจแช"

ON PAD M_Seek  OF Custs ACTIVATE POPUP P_Seek
*
*    POPUP - ฌฅญ๎ ขโฎเฎฃฎ ใเฎขญ๏
*
DEFINE POPUP P_Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR  1 OF P_Seek PROMPT "ฎจแช ฏฎ ญ ็. แจฌขฎซ ฌ " KEY Ctrl-A, "Ctrl-F"
DEFINE BAR  2 OF P_Seek PROMPT "ฎ ่ กซฎญใ " KEY Ctrl-A, "F7"
DEFINE BAR  3 OF P_Seek PROMPT "ฎฏฎซญจโฅซ์ญ๋ฉ " KEY Ctrl-A, "Shift-F7"
DEFINE BAR  4 OF P_Seek PROMPT "ฎขโฎเ "     KEY Ctrl-A, "Ctrl-F7"
DEFINE BAR  5 OF P_Seek PROMPT "ฎจแช ฏฎ  "  KEY Ctrl-A, "F8"
DEFINE BAR  6 OF P_Seek PROMPT "ฎจแช ฏฎ E_mail "  && KEY Ctrl-A, "F8"

ON SELECTION BAR 1 OF P_Seek DO Ch_Mode WITH "Custs", "Fseek"
ON SELECTION BAR 2 OF P_Seek DO Ch_Mode WITH "Custs", "seek"
ON SELECTION BAR 3 OF P_Seek DO Ch_Mode WITH "Custs", "aseek"
ON SELECTION BAR 4 OF P_Seek DO Ch_Mode WITH "Custs", "sseek"
ON SELECTION BAR 5 OF P_Seek DO Ch_Mode WITH "Custs", "INNseek"
ON SELECTION BAR 6 OF P_Seek DO Ch_Mode WITH "Custs", "Eseek"

IF let_add && .AND. (sys_char = "A" .OR. sale_mode = "Mitin")
  DEFINE PAD Add    OF Custs PROMPT "ฎก ขจโ์"   KEY Ctrl-C, "Ins"
  ON SELECTION PAD Add    OF Custs Do Ch_Mode WITH "Custs", "add"
ENDIF

DEFINE PAD Abort  OF Custs PROMPT IIF(modi .AND. (sys_char = "A" .OR. sale_mode = "Mitin"), "๋ๅฎค", "โช ง") KEY Ctrl-F, "Esc"
ON SELECTION PAD Abort  OF Custs Do Ch_Mode WITH "Custs", "abort"

DO Entry_P WITH ext_menu, "Custs"

what_do = "list"
IF LEN(pl_wk) = 0
  SET ORDER TO TAG CUS_NAME
ELSE
  SET ORDER TO TAG PL_NAME
ENDIF

DO WHILE .T.
  DO CASE
  CASE what_do = "list"
    SELECT ACCOUNT
    SHOW MENU Custs
    DO Prp_Wins WITH 1
    ACTIVATE WINDOW Cus_E
    ON KEY LABEL Enter DO Ch_Mode WITH "Custs", IIF(modi .AND. (sys_char = "A" .OR. sale_mode = "Mitin"), "corr", "exit")
    ON KEY LABEL F7 DO Ch_Mode WITH "Custs", "seek"
    ON KEY LABEL Ctrl+F DO Ch_Mode WITH "Custs", "Fseek"
    ON KEY LABEL Ctrl+F7 DO Ch_Mode WITH "Custs", "sseek"
    ON KEY LABEL Shift+F7 DO Ch_Mode WITH "Custs", "aseek"
    ON KEY LABEL F8 DO Ch_Mode WITH "Custs", "INNseek"
    ON KEY LABEL F10 DO F10_Proc WITH "Custs"
    IF .NOT. stop_inf
      ON KEY LABEL F1 DO Ch_Mode WITH "Custs", "info"
      ON KEY LABEL Ctrl-F1 DO Ch_Mode WITH "Custs", "c_info"
      ON KEY LABEL Ctrl-Enter DO Ch_Mode WITH "Custs", "dossier"
      ON KEY LABEL F2 DO Ch_Mode WITH "Custs", "pasport"
      ON KEY LABEL Shift-F1 DO Ch_Mode WITH "Custs", "acc_hist"
    ENDIF
    IF let_add && .AND. (sys_char = "A" .OR. sale_mode = "Mitin")
      ON KEY LABEL Ins Do Ch_Mode WITH "Custs", "add"
    ENDIF
    ON KEY LABEL LeftMouse Do M_Pr_‘01
    IF LEN(pl_wk) = 0
      SET ORDER TO TAG CUS_NAME
    ELSE
      SET ORDER TO TAG PL_NAME
    ENDIF
    IF stop_inf
      BROWSE KEY pl_wk                  ;
             FIELDS ACCOUNT.CUS_NAME,   ;
                    DUMMY.F:H=""   ;
             FREEZE DUMMY.F NOEDIT NOMENU NORGRID NOAPPEND NOCLEAR ;
             WHEN Show_F()   ;
             WINDOW Cus_I IN WINDOW Cus_E
    ELSE
      BROWSE KEY pl_wk                  ;
             FIELDS ACCOUNT.CUS_NAME,   ;
             DOSS_M = Doss_M():H="":1,  ;
                    DUMMY.F:H=""   ;
             FREEZE DUMMY.F NOEDIT NOMENU NORGRID NOAPPEND NOCLEAR ;
             WHEN Show_F()   ;
             WINDOW Cus_I IN WINDOW Cus_E
    ENDIF
    ON KEY
*    RELEASE WINDOW Cus_M
*    RELEASE WINDOW Cus_E
*    RELEASE WINDOW Cus_I
    IF what_do = "list"
      what_do = "abort"
    ENDIF
  CASE what_do = "longname"
*    IF modi
      DO Cus_Name WITH CUS_CODE
*    ELSE
*      DO LongName WITH CUS_CODE
*    ENDIF
    what_do = "list"
  CASE what_do = "acc_code"
    WAIT WINDOW ALLTRIM(STR(CUS_CODE))
    what_do = "list"
  CASE what_do = "info"
    DO V_Note
    what_do = "list"
  CASE what_do = "dossier"
    DO Dossier WITH CUS_CODE
    what_do = "list"
  CASE what_do = "acc_hist"
    DO Acc_Hist WITH CUS_CODE
    what_do = "list"
  CASE what_do = "c_info"
    DO Wk_Fill WITH "Custs"
*    DO Cli_Info WITH "Custs", .T.
    what_do = "list"
  CASE what_do = "add"
    DO Modi_Cus WITH "A"
    what_do = "list"
  CASE what_do = "pasport"
    DO Pasport WITH ACCOUNT.CUS_CODE
    what_do = "list"
  CASE what_do = "corr"
    DO Modi_Cus WITH "C"
    what_do = "list"
  CASE what_do = "INNseek"
    m.tmpRec = Seek_INN()
    IF m.tmpRec # 0
      GO m.tmpRec IN ACCOUNT
    ENDIF
    what_do = "list"
  CASE what_do = "aseek"
    DO Seek_01
    IF Seek_Cus("!")
      what_do = IIF(modi .AND. (sys_char = "A" .OR. sale_mode = "Mitin"), "corr", "exit")
    ELSE
      what_do = "list"
    ENDIF
  CASE what_do = "Fseek"
    f_patt = Get_Shab("","ฎจแช ฏฎ ญ ็. แจฌขฎซ ฌ")
    IF .NOT. f_patt == "" 
      sav_rc = RECNO("ACCOUNT")
      IF .NOT. SEEK(pl_wk+SYS(15, lwr, f_patt), "ACCOUNT")
        PRIVATE mss
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"จ็ฅฃฎ ญฅ ก๋ซฎ ญ ฉคฅญฎ!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        IF BETWEEN(sav_rc, 1, RECCOUNT("ACCOUNT"))
          GO sav_rc IN ACCOUNT
        ENDIF
      ENDIF
    ENDIF
    what_do = "list"
  CASE what_do = "seek"
    IF Seek_Cus()
      what_do = IIF(modi .AND. (sys_char = "A" .OR. sale_mode = "Mitin"), "corr", "exit")
    ELSE
      what_do = "list"
    ENDIF
  CASE what_do = "sseek"
    IF Seek_Cus("!")
      what_do = IIF(modi .AND. (sys_char = "A" .OR. sale_mode = "Mitin"), "corr", "exit")
    ELSE
      what_do = "list"
    ENDIF
  CASE what_do = "Eseek"
    m.tmpRec = Seek_Eml()
    IF m.tmpRec # 0
      GO m.tmpRec IN ACCOUNT
    ENDIF
    what_do = "list"
  CASE what_do = "abort"
    SELECT ACCOUNT
    IF BETWEEN(rc_sav,1,RECCOUNT())
      GO rc_sav
    ENDIF
    ex_val = .F.
    EXIT
  OTHERWISE
    ex_val = .T.
    EXIT
  ENDCASE
ENDDO

RELEASE WINDOW Cus_M
RELEASE WINDOW Cus_E
RELEASE WINDOW Cus_I
RELEASE MENU Custs EXTENDED

DO Exit_P WITH ext_menu, "Custs"
SELECT ACCOUNT
SET ORDER TO &ord
IF .NOT. stop_inf
  SET RELATION TO
  SELECT DOSS_V
  USE
ENDIF
SELECT CT_0705
USE
SELECT CB_0705
USE
SELECT CLT_0705
USE
SELECT (sel)

RETURN ex_val

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ไฎเฌจเฎข ญจ๏ ฏฎคข ซ .                           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Show_F
PRIVATE tmpName, tmpKPP, tmpINN

ACTIVATE WINDOW CUS_M SAME
IF SEEK(STR(ACCOUNT.CUS_CODE)+STR(user_dep,3), "CLT_0705")
  tmpName = CLT_0705.TYPE
ELSE
  tmpName = "  0"
ENDIF
IF SEEK(ACCOUNT.CUS_CODE, "CB_0705")
  m.tmpKPP = CB_0705.KPP
  m.tmpINN = CB_0705.INN
ELSE
  m.tmpKPP = SPACE(FSIZE("KPP", "CB_0705"))
  m.tmpINN = SPACE(FSIZE("INN", "CB_0705"))
ENDIF
@ WROWS()-2, 10 SAY " "+m.tmpINN+"    "+m.tmpKPP

IF SEEK(STR(user_dep,3)+tmpName, "CT_0705")
  @ WROWS()-1, 10 SAY CT_0705.NAME
ELSE
  @ WROWS()-1, 10 SAY SPACE(FSIZE("NAME","CT_0705"))
ENDIF

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             เฎฃเ ฌฌ  ฏฎคฃฎโฎขชจ ฎชฎญ.                                บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prp_Wins
PARAMETERS md
PRIVATE ln, tit1, tit2, foot
DIMENSION tit2(1), foot(3)
*
*   แญฎขญฎฉ ฏฅเฅ็ฅญ์ ฏฎซฅฉ คซ๏ BROWSE
*
tit2(1) = PADC(" ฎชใฏ โฅซ์ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ",    ;
                 FSIZE("CUS_NAME","ACCOUNT"),"ฤ")
IF .NOT. stop_inf .AND. md = 1
  tit2(1) = tit2(1)+"ฤ"
ENDIF
foot(1) = ""
foot(2) = ""
foot(3) = PADR("    ’จฏ", LEN(tit2(1)))

ln = WROWS("")-6

*
*  ฎเเฅชโจเใฅฌ แ ใ็ฅโฎฌ ใแโ ญฎขซฅญญฎฃฎ ไจซ์โเ 
*


tit1 = "ฎชใฏ โฅซจ"

IF md = 1
  RELEASE WINDOW Cus_M
  RELEASE WINDOW Cus_E
  RELEASE WINDOW Cus_I
  DO Def_Win WITH "Cus_M", "Cus_E", "Cus_I", ln, LEN(tit2(1)), tit1, tit2, foot
ELSE
  RELEASE WINDOW Seek_M
  RELEASE WINDOW Seek_E
  RELEASE WINDOW Seek_I
  DO Def_Win WITH "Seek_M", "Seek_E", "Seek_I", MIN(ln,RECCOUNT("LINK")), ;
                         LEN(tit2(1)), tit1, tit2
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                        เฎฃเ ฌฌ  ฏฎจแช  ชซจฅญโ .                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Seek_Cus
PARAMETER prm
PRIVATE rc_sav, l, ln, wd, c, st, s, ord, repe, ret_v

repe = TYPE("prm") # "L"

DEFINE MENU Seek_Cus IN SCREEN COLOR SCHEME 4
DEFINE PAD Choose OF Seek_Cus PROMPT "๋กเ โ์"    KEY Ctrl-A, "Enter"
DEFINE PAD Abort  OF Seek_Cus PROMPT "โช ง โ์แ๏" KEY Ctrl-B, "Esc"

ON SELECTION PAD Choose OF Seek_Cus Do Ex_Proc WITH "Seek_Cus",13
ON SELECTION PAD Abort  OF Seek_Cus Do Ex_Proc WITH "Seek_Cus",27

DO Entry_P WITH "Custs", "Seek_Cus"
st = SPACE(FSIZE("CUS_NAME","ACCOUNT"))
ln = 5
wd = LEN(st)+6
l = CEILING((WROWS("")-ln)/2)
c = FLOOR((WCOLS("")-wd)/2)
ret_v = .T.
IF .NOT. repe
  DEFINE WINDOW Seek_Cus FROM l, c TO l+ln-1, c+wd-1 SHADOW NONE COLOR SCHEME 13
  ACTIVATE WINDOW Seek_Cus
  @ 0, 1 TO WROWS()-1, WCOLS()-2
  @ 0, 2 SAY PADC(" ขฅคจโฅ ่ กซฎญ คซ๏ ฏฎจแช  ", WCOLS()-4, "ฤ")
  ON KEY LABEL LeftMouse DO M_Pr_C03
  DO WHILE .T.
    @ 2, 3 GET st
    READ
    s = MOD(READKEY(),256)
    IF s = 12
      ret_v = .F.
      s = .F.
      EXIT
    ENDIF
    IF s = 15
      s= .NOT. EMPTY(st)
      EXIT
    ENDIF
  ENDDO

  RELEASE WINDOW Seek_Cus
ELSE
  s = .T.
ENDIF

rc_sav = RECNO("ACCOUNT")
IF s
  IF .NOT. repe
    DO Wt_Mess WITH "้ฅฌ..."
    st = SYS(15,lwr,ALLTRIM(st))
    SELECT LINK
    ZAP
    SELECT ACCOUNT
    SEEK pl_wk
    SCAN REST WHILE SUBSYS = pl_wk FOR st $ SYS(15,lwr,CUS_NAME)
      SELECT LINK
      APPEND BLANK
      REPLACE LNK WITH RECNO("ACCOUNT")
      SELECT ACCOUNT
    ENDSCAN
    DO Wt_Mess
  ENDIF
  IF RECCOUNT("LINK") = 0
    IF repe
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"จ็ฅฃฎ ญฅ ก๋ซฎ ญ ฉคฅญฎ!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      ret_v = .F.
    ELSE
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"ฅโ โ ชจๅ ชซจฅญโฎข!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      GO rc_sav
      ret_v = .F.
    ENDIF
  ELSE
    DO Prp_Wins WITH 2
    ACTIVATE WINDOW Seek_E
    SELECT ACCOUNT
    ord = SET("ORDER")
    SET ORDER TO 0
    SELECT LINK
    SET RELATION TO LNK INTO ACCOUNT
    GO TOP
    ON KEY LABEL ENTER KEYBOARD CHR(23)
    ON KEY LABEL LeftMouse DO M_Pr_C04
    ON KEY LABEL F1 DO Info WITH 1
    ON KEY LABEL Ctrl-F1 DO Info WITH 2
    BROWSE FIELDS ACCOUNT.CUS_NAME,   ;
                  DUMMY.F:H=""   ;
           FREEZE DUMMY.F NOEDIT NOMENU NORGRID NOAPPEND NOCLEAR  ;
           WINDOW Seek_I IN WINDOW Seek_E
    SET RELATION TO
    SELECT ACCOUNT
    SET ORDER TO &ord
    IF MOD(READKEY(),256) = 12
      ret_v = .F.
      GO rc_sav
    ENDIF
    RELEASE WINDOW Seek_M
    RELEASE WINDOW Seek_E
    RELEASE WINDOW Seek_I
  ENDIF
ENDIF

DO Exit_P WITH "Custs", "Seek_Cus"

SELECT ACCOUNT
RELEASE MENU Seek_Cus
what_do = "list"
RETURN ret_v

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎฃเ ฌฌ  ฏเฎขฅเชจ จฌฅญจ ชซจฅญโ .                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Tst_Name
PRIVATE mss

mss = MOD(READKEY(),256)
IF READKEY(1) # 1
  mss = 15
ENDIF
IF mss = 12
  ex = 2
  RETURN .T.
ENDIF
IF ex = 2
  RETURN .T.
ENDIF
IF .NOT. EMPTY(nm)
  RETURN .T.
ENDIF

DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"“ฆ ช ช-ญจกใค์ ชซจฅญโ  ญ งฎขจโฅ..."
mss(3) = ""
DO Out_Mess WITH 7,"mss"
RETURN .F.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ        เฎฃเ ฌฌ  - ฎกเ กฎโ็จช ฌ๋่ชจ คซ๏ แฏจแช  ชซจฅญโฎข.              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE M_Pr_‘01
PRIVATE mc, mr

*
*  €แจญๅเฎญญฎฅ ฌฅญ๎
*
IF MROW("") = 0
  IF .NOT. MENU() == "CUSTS"
    ACTIVATE MENU Custs
  ENDIF
  RETURN
ENDIF

*
*  ชญฎ BROWSE
*
mc = MCOL("Cus_E")
mr = MROW("Cus_E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  แญฎขญฎฅ ฎชญฎ
*
mc = MCOL("Cus_M")
mr = MROW("Cus_M")
IF mc >= 0 .AND. mr >= 0
  IF mr = 1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WROWS("Cus_M")-1
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ        เฎฃเ ฌฌ  - ฎกเ กฎโ็จช ฌ๋่ชจ คซ๏ ไจซ์โเ  ชซจฅญโฎข.             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE M_Pr_C02
PRIVATE mc, mr

*
*  €แจญๅเฎญญฎฅ ฌฅญ๎
*
IF MROW("") = 0
  IF .NOT. MENU() == "SET_F"
    ACTIVATE MENU Set_F
  ENDIF
  RETURN
ENDIF

*
*  ชญฎ แฏจแช 
*
mc = MCOL("Set_F")
mr = MROW("Set_F")
IF mc >= 0 .AND. mr >= 0
  IF INLIST(mr, 0, 1, WROWS("Set_F")-1) .OR. INLIST(mc, 0, WCOLS("Set_F")-1)
    = INKEY("M")
  ENDIF
  RETURN
ENDIF

= INKEY("M")

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ   เฎฃเ ฌฌ  - ฎกเ กฎโ็จช ฌ๋่ชจ คซ๏ ฏฎจแช  ชซจฅญโฎข ( ขขฎค ่ กซฎญ  ).  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE M_Pr_C03
PRIVATE mc, mr

*
*  €แจญๅเฎญญฎฅ ฌฅญ๎
*
IF MROW("") = 0
  IF .NOT. MENU() == "SEEK_CUS"
    ACTIVATE MENU Seek_Cus
  ENDIF
  RETURN
ENDIF

*
*  ชญฎ แฏจแช 
*
mc = MCOL("Seek_Cus")
mr = MROW("Seek_Cus")
IF mc >= 0 .AND. mr >= 0
  RETURN
ENDIF

= INKEY("M")

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ  เฎฃเ ฌฌ  - ฎกเ กฎโ็จช ฌ๋่ชจ คซ๏ ฏฎจแช  ชซจฅญโฎข ( ข๋กฎเ ชซจฅญโ  ).  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE M_Pr_C04
PRIVATE mc, mr

*
*  €แจญๅเฎญญฎฅ ฌฅญ๎
*
IF MROW("") = 0
  IF .NOT. MENU() == "SEEK_CUS"
    ACTIVATE MENU Seek_Cus
  ENDIF
  RETURN
ENDIF

*
*  ชญฎ BROWSE
*
mc = MCOL("Seek_E")
mr = MROW("Seek_E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  แญฎขญฎฅ ฎชญฎ
*
mc = MCOL("Seek_M")
mr = MROW("Seek_M")
IF mc >= 0 .AND. mr >= 0
  IF mr = 1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WROWS("Seek_M")-1
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

PROCEDURE Info
PARAMETERS md

DEFINE WINDOW w0001 FROM 0,0 TO 0, WCOLS("") NONE COLOR SCHEME 13
ACTIVATE WINDOW w0001
PUSH KEY CLEAR

IF md = 1
  DO V_Note
ELSE
   DO Wk_Fill WITH ""
*  DO Cli_Info WITH "", .F.
ENDIF

RELEASE WINDOW w0001
POP KEY

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                        ฎฌฅโช  ฎ ญ ซจ็จจ คฎแ์ฅ.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Doss_M

IF FOUND("DOSS_V")
  RETURN IIF(EMPTY(DOSS_V.IMPORTANCE), "๛", "!")
ENDIF

RETURN " "

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  ฎจแช ชซจฅญโ  ฏฎ , เ แ็ฅโญฎฌใ แ็ฅโใ               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Seek_01

PRIVATE shab, is_inn, s_sav, r_sav, w_seek

shab = SPACE(20)
is_inn = 1

IF .NOT. Get_P() .OR. EMPTY(shab)
  RETURN
ENDIF

s_sav = SELECT()
SELECT LINK
ZAP
SELECT 0
USE (base_path+"CUS_BIL") ORDER TAG CUS_CODE SHARED AGAIN ALIAS CB_20215
SELECT ACCOUNT
r_sav = RECNO()
shab = SYS(15, lwr, ALLTRIM(shab))
SEEK pl_wk
DO Wt_Mess WITH "้ฅฌ..."
SCAN REST WHILE SUBSYS = pl_wk
  IF .NOT. SEEK(CUS_CODE, "CB_20215")
    LOOP
  ENDIF
  IF is_inn = 1
    w_seek = SYS(15, lwr, CB_20215.INN)
  ELSE
    w_seek = SYS(15, lwr, CB_20215.ACC_BILL)
  ENDIF
  IF shab $ w_seek
    SELECT LINK
    APPEND BLANK
    REPLACE LNK WITH RECNO("ACCOUNT")
    SELECT ACCOUNT
  ENDIF
ENDSCAN
IF BETWEEN(r_sav, 1, RECCOUNT("ACCOUNT"))
  GO r_sav
ENDIF
SELECT CB_20215
USE
SELECT (s_sav)
DO Wt_Mess

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                            เ ฌฅโเ๋ ฏฎจแช                             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_P
*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃ ๆจจ
*

PRIVATE stat_type     && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
                                        1 - กซ ญช;
                                        2 - BROWSE - โ กซจๆ ;
                                        3 - BROWSE - แฏจแฎช.
PRIVATE what_do       && ฌ๏ เฅฆจฌ .
PRIVATE menu_name     && ฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎.
PRIVATE last_mouse    && เฅฌ๏ ฏฎแซฅคญฅฃฎ ญ ฆ โจ๏ งขฅเจญฎฉ ชญฎฏชจ.
PRIVATE win_name      && ฌ๏ ฎชญ  ( ฎชฎญ คซ๏ BROWSE ).

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ‘ฎคฅเฆ โฅซ์ญ ๏ ็ แโ์ ฏเฎฃเ ฌฌ๋:
*

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤ ฎจแช ข... ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ ( )   ( )  แ็ฅโญฎฌ แ็ฅโฅ ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ    ’ฅชแโ ....:....!....:....!    ณ
*ณ < OK Ctrl-W > < โช ง โ์แ๏ Esc > ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 8, 38, "ฎจแช"
DO Sun_Bord WITH  2,  3,  4, 34, " ฎจแช ข... "

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@ 3, 5 GET is_inn PICTURE "@*RH  ; แ็ฅโญฎฌ แ็ฅโฅ"
@ 5, 6 SAY "’ฅชแโ" GET shab
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1
