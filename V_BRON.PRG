*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      เฎฃเ ฌฌ  ฏเฎแฌฎโเ  กเฎญจ.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE V_Bron

*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃ ๆจจ
*

PRIVATE stat_type   && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
                                        1 - กซ ญช;
                                        2 - BROWSE - โ กซจๆ ;
                                        3 - BROWSE - แฏจแฎช.
PRIVATE what_do       && ฌ๏ เฅฆจฌ .
PRIVATE menu_name     && ฌ๏  แจญๅเฎญญฎฃฎ ฌฅญ๎.
PRIVATE last_mouse    && เฅฌ๏ ฏฎแซฅคญฅฃฎ ญ ฆ โจ๏ งขฅเจญฎฉ ชญฎฏชจ.
PRIVATE win_name      && ฌ๏ ฎชญ  ( ฎชฎญ คซ๏ BROWSE ).

DO Wt_Mess WITH "ฎคกฎเ ค ญญ๋ๅ..."

*
*    ฏฎซญ๏ฅฌ งญ ็ฅญจ๏ฌจ ฏฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏...
*
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ฏเฅคฅซ๏ฅฌ  แจญๅเฎญญฎฅ ฌฅญ๎
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD View OF (menu_name) PROMPT "เฎแฌฎโเ" KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF (menu_name) Do Sw_Mode WITH "view"

DEFINE PAD Info OF (menu_name) PROMPT "ญไฎเฌ ๆจ๏" KEY Ctrl-A, "F1"
ON SELECTION PAD Info OF (menu_name) Do Sw_Mode WITH "info"

DEFINE PAD Ords OF (menu_name) PROMPT " ๏ขชจ" KEY Ctrl-A, "F2"
ON SELECTION PAD Ords OF (menu_name) Do Sw_Mode WITH "ords"

DEFINE PAD Exit OF (menu_name) PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   โชเ๋ข ฅฌ ไ ฉซ๋ .., ใแโ ญ ขซจข ฅฌ แข๏งจ จ โ. ค.
*

DELETE FILE (tmpo_path+"LST_TMP.DBF")
CREATE DBF (tmpo_path+"LST_TMP.DBF") ( L_NUM N(6), B_SUM N(10,2) )

DO Use_Dummy

SELECT 0
USE (base_path+"USERS") ORDER TAG USER_CODE

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_NAME
SET FILTER TO QNT_REQ # 0

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM
SET RELATION TO LST_ARM  INTO ARM_LST
*
*   ฎคกจเ ฅฌ แฏจแชจ แ กเฎญ์๎
*
PRIVATE s000, n000, q000
SCAN FOR EMPTY(SAL_NUM) && .AND. ORD_NUM # 0
  s000 = 0
  q000 = .F.
  n000 = STR(LST_NUM,6)
  IF SEEK(n000,"LIST_DET")
    SELECT LIST_DET
    SCAN REST WHILE n000 = LIST_NUM
      IF QNT_REQ # 0
        s000 = s000+QNT_REQ*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)
        q000 = .T.
      ENDIF
    ENDSCAN
  ENDIF
  IF q000
    SELECT LST_TMP
    APPEND BLANK
    REPLACE L_NUM WITH LIST_TIT.LST_NUM, B_SUM WITH s000
  ENDIF
  SELECT LIST_TIT
ENDSCAN

SELECT LST_TMP
SET RELATION TO L_NUM INTO LIST_TIT

DO Wt_Mess

*
*    แ็ฅโ เ งฌฅเฎข ฎชญ 
*
PRIVATE ln, wd
ln = 100        && ฎซจ็ฅแโขฎ ขจคจฌ๋ๅ แโเฎช BROWSE
wd = FSIZE("FIRM", "LIST_TIT")+FSIZE("LST_NUM", "LIST_TIT")+FSIZE("LST_NAME", "LIST_TIT")+  ;
     FSIZE("B_SUM", "LST_TMP")+FSIZE("ARM_N", "ARM_LST")+4

DO D_Wins WITH ln, wd, "ฅเฅ็ฅญ์ แฏจแชฎข แ กเฎญ์๎", 1, 1
@ 1, 2 SAY PADC("N ‘ฏ.",FSIZE("LST_NUM", "LIST_TIT"))+" "+  ;
           PADC("”เฌ",FSIZE("FIRM", "LIST_TIT"))+" "+  ;
           PADC("ฌ๏ แฏจแช ",FSIZE("LST_NAME", "LIST_TIT"))+" "+ ;
           PADC("‘ใฌฌ  กเ.",FSIZE("B_SUM", "LST_TMP"))+" "+ ;
           PADC(" กฎ็ฅฅ ฌฅแโฎ",FSIZE("ARM_N", "ARM_LST"))
@ WROWS()-3, 3 SAY "ฎชใฏ โฅซ์"
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && เฎแฌฎโเ แฏจแช 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1 DO Sw_Mode WITH "info"
    ON KEY LABEL F2 Do Sw_Mode WITH "ords"

*
*    BROWSE - ฌฅญ๎
*
    BROWSE FIELDS L_NUM:H="",     ;
                  LIST_TIT.FIRM:H="",     ;
                  LIST_TIT.LST_NAME:H="", ;
                  B_SUM:H="":P="@Z ", ;
                  F001 = IIF(LIST_TIT.LST_ARM # "***",   ;
                                ARM_LST.ARM_N, "ก้ ๏ ฎกซ แโ์"):16:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_Cus(win_name)  ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "view")
    ENDIF

  CASE what_do = "view"    && เฎแฌฎโเ แฏจแช 

    DO View
    what_do = "list"

  CASE what_do = "info"    && ญไฎเฌ ๆจ๏ ฏฎ แฏจแชใ

    DO Info
    what_do = "list"

  CASE what_do = "ords"    &&  ๏ขชจ ญ  แชซ ค

    DO P_Ords
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
DELETE FILE (tmpo_path+"LST_TMP.DBF")
RELEASE MENU (menu_name) EXTENDED

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      จงใ ซจง ๆจ๏ ชซจฅญโ .                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Show_Cus
PARAMETERS win

PRIVATE s

IF .NOT. SEEK(LIST_TIT.CLI_CODE, "ACCOUNT")
  s = SPACE(FSIZE("CUS_NAME","ACCOUNT"))+SPACE(FSIZE("ACC_SUM","ACCOUNT")+1)
ELSE
  = SEEK(STR(ACCOUNT.CUS_CODE)+LIST_TIT.FIRM, "CUS_REST")
  s = ACCOUNT.CUS_NAME+" "
  IF LIST_TIT.IS_USD
    s = s+STR(CUS_REST.ACC_SUMD, FSIZE("ACC_SUMD","CUS_REST"),4)
  ELSE
    s = s+STR(CUS_REST.ACC_SUM, FSIZE("ACC_SUM","CUS_REST"),2)
  ENDIF
ENDIF
ACTIVATE WINDOW (win+"_M") SAME
@ WROWS()-2, 3 SAY s
ACTIVATE WINDOW (ALIAS())

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                            เฎแฌฎโเ กเฎญจ.                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE View

*
*  ฅเฅฌฅญญ๋ฅ แฎแโฎ๏ญจ๏ คซ๏ ญ ขจฃ ๆจจ
*

PRIVATE stat_type   && ’จฏ ญ ขจฃ ๆจจ: 0 - ญฅแโ ญค เโญ๋ฉ;
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
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ฏเฅคฅซ๏ฅฌ  แจญๅเฎญญฎฅ ฌฅญ๎
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   โชเ๋ข ฅฌ ไ ฉซ๋ .., ใแโ ญ ขซจข ฅฌ แข๏งจ จ โ. ค.
*
PRIVATE k
k = STR(LIST_TIT.LST_NUM,6)
SELECT LIST_DET
SEEK k

*
*    แ็ฅโ เ งฌฅเฎข ฎชญ 
*
PRIVATE ln, wd
ln = 100  && ฎซจ็ฅแโขฎ ขจคจฌ๋ๅ แโเฎช BROWSE
wd = FSIZE("PREFIX", "LIST_DET")+  ;
     FSIZE("NAME",   "LIST_DET")+  ;
     FSIZE("QNT_REQ","LIST_DET")+2
DO D_Wins WITH ln, wd, "N "+ALLTRIM(STR(LIST_TIT.LST_NUM))+": "+ALLTRIM(LIST_TIT.LST_NAME), 1, 0
@ 1, 2 SAY PADC("ฎงจๆจ๏           ",FSIZE("PREFIX", "LIST_DET")+  ;
     FSIZE("NAME",   "LIST_DET")+1)+" "+ ;
     PADC("ฎซ-ขฎ",FSIZE("QNT_REQ","LIST_DET"))

what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && เฎแฌฎโเ แฏจแช 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ฌฅญ๎
*
    BROWSE FIELDS PREFIX:H="", NAME:H="", QNT_REQ:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           KEY k ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

SELECT LST_TMP
RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")

RELEASE MENU (menu_name) EXTENDED

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                             ญไฎเฌ ๆจ๏.                               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Info

PRIVATE tit
PRIVATE win_name, v1, v2, v3

win_name = PROGRAM()

DEFINE WINDOW Hide_W FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW Hide_W

tit = "N "+ALLTRIM(STR(LIST_TIT.LST_NUM))+": "+ALLTRIM(LIST_TIT.LST_NAME)
DO D_Win_N WITH 11, 50, tit

v1 = DTOC(LIST_TIT.CR_DAT)
IF SEEK(LIST_TIT.WHERE_CR, "ARM_LST")
  v2 = ARM_LST.ARM_N
ELSE
  v2 = SPACE(16)
ENDIF

IF LIST_TIT.WHO_CR = 0
  v3 = PADR("‘€ แฎงค ซ!", 32)
ELSE
  IF SEEK(LIST_TIT.WHO_CR, "USERS")
    v3 = USERS.USER_NAME
  ELSE
    v3 = SPACE(32)
  ENDIF
ENDIF

@ 2, 2 SAY "                                              "
@ 3, 2 SAY "               ‘ฎงค ญ ..                "
@ 4, 2 SAY "                                              "
@ 5, 2 SAY "         กฎ็ฅฅ ฌฅแโฎ: ....:....!....:.       "
@ 6, 2 SAY "                                              "
@ 7, 2 SAY " โฎ แฎงค ซ: ....:....!....:....!....:....!.. "
@ 8, 2 SAY "                                              "
@ 9, 2 SAY "              ฆฌจโฅ ็โฎ-ญจกใค์...            "

@ 3, 24 SAY v1 COLOR (SCHEME(14,2))
@ 5, 25 SAY v2 COLOR (SCHEME(14,2))
@ 7, 15 SAY v3 COLOR (SCHEME(14,2))

= INKEY(0,"MH")

RELEASE WINDOW (win_name)

RELEASE WINDOW Hide_W

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                        ฅ็ โ์ ง ๏ขฎช...                               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE P_Ords

PRIVATE tit
PRIVATE win_name, n, l1, l2, l3, c
PUSH KEY CLEAR

win_name = PROGRAM()

DEFINE WINDOW Hide_W FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW Hide_W

tit = "N "+ALLTRIM(STR(LIST_TIT.LST_NUM))+": "+ALLTRIM(LIST_TIT.LST_NAME)

DO D_Win_N WITH 9, 50, tit

l1 = "แฅฃฎ ฏฎ แฏจแชใ แไฎเฌจเฎข ญฎ "+ALLTRIM(STR(LIST_TIT.ORD_NUM))+" ง ๏ขฎช."
l2 = "ฎโฎเใ๎ จง ญจๅ ฏฅ็ โ ฅฌ? NNN"
l3 = "Esc - ขฎงขเ โ..."
l1 = PADC(l1,WCOLS()-4)
l2 = PADC(l2,WCOLS()-4)
l3 = PADC(l3,WCOLS()-4)
c  = AT("NNN",l2)+1
@ 3, 2 SAY l1
@ 5, 2 SAY l2
@ 7, 2 SAY l3

n = 0
DO WHILE .T.
  ACTIVATE WINDOW (win_name)
  SET CURSOR ON
  @ 5, c GET n PICTURE "@Z 999" VALID V_N_Ord()
  READ
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
  IF new_ord
    DO Prn_OrdN WITH LIST_TIT.LST_NUM, n
  ELSE
    DO Prn_Ord WITH LIST_TIT.LST_NUM, n
  ENDIF
ENDDO

RELEASE WINDOW (win_name)

RELEASE WINDOW Hide_W

POP KEY

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ               เฎขฅเช  ชฎเเฅชโญฎแโจ ญฎฌฅเ  ง ๏ขชจ.                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE V_N_Ord

IF READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF n <= 0
  PRIVATE mss
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅงใแซฎขญฎ, ฎโเจๆ โฅซ์ญ๋ฅ ็จแซ  จ ญฎซ์ ข ฏเจเฎคฅ แใ้ฅแโขใ๎โ,"
  mss(3) = CHR(0)+"ญฎ ญฅ ช ช ญฎฌฅเ  ง ๏ขฎช!"
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF n > LIST_TIT.ORD_NUM
  PRIVATE mss
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"‘ฏฅๆจ ซ์ญฎ คซ๏ ญฅขญจฌ โฅซ์ญ๋ๅ:"
  mss(3) = CHR(0)+"แฅฃฎ ฏฎ แฏจแชใ แไฎเฌจเฎข ญฎ "+ALLTRIM(STR(LIST_TIT.ORD_NUM))+" ง ๏ขฎช!"
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
