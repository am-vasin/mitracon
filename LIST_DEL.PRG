*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ใค ซฅญจ๏ แฏจแชฎข.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE List_Del
PRIVATE l_bel, dat_clo, dat_cr, ex, ln, wd, arm_w

l_bel   = 1     && ’ฎซ์ชฎ แขฎจ
dat_clo = {}    &&  โ  ฏเฎค ฆจ
dat_cr  = {}    &&  โ  แฎงค ญจ๏
ex      = 1

ln = IIF(acc_level < 2, 12, 9)
wd = 46
DO D_W WITH "List", ln, wd, " ชจฅ แฏจแชจ ๅฎโจฌ ขจคฅโ์:", 0

@ 0, 0 SAY ""

? " ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
? " ณ          ‘ฎงค ญญ๋ฅ คฎ ..           ณ"
? " ณ                                          ณ"
? " ณ           ชเ๋โ๋ฅ คฎ  ..           ณ"
? " ณ                                          ณ"
IF acc_level < 2
  ? " ณ ฺฤ ฅเฅ็ฅญ์ แฏจแชฎข ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ"
  ? " ณ ณ ( ) ‘ขฎจ  ( ) ก้ฅฉ ฎกซ แโจ  ( ) แฅ ณ ณ"
  ? " ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ"
ENDIF
? " ณ     < OK Ctrl-W > < โช ง โ์แ๏ Esc >     ณ"
? " ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"

@ 2, 25 GET dat_cr  PICTURE "@D" WHEN Clr_Date(1) VALID T_Date()
@ 4, 25 GET dat_clo PICTURE "@D" WHEN Clr_Date(2) VALID T_Date()
IF acc_level < 2
  @ 7, 5 GET l_bel PICTURE "@*RH  ‘ขฎจ ;ก้ฅฉ ฎกซ แโจ ;แฅ"
  @ 9, 7 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
ELSE
  @ 6, 7 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
ENDIF
READ CYCLE
RELEASE WINDOW List
IF ex = 2
  RETURN
ENDIF

IF File_O(tmpo_path+"LINK_D.DBF")
  DELETE FILE (tmpo_path+"LINK_D.DBF")
ENDIF

CREATE DBF (tmpo_path+"LINK_D")  ;
       ( MRK     C ( 1),         ;
         L_NUM   C ( 6),         ;
         B_STAT  C ( 5),         ;
         M_STAT  C ( 5),         ;
         S_STAT  C ( 5),         ;
         CUS_NAME C(50)   )

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_NAME

SELECT 0
USE (base_path+"BILLS") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (base_path+"LIST_TIT")
IF l_bel = 3
  SET ORDER TO TAG LST_NUM
ELSE
  SET ORDER TO TAG LST_ARM
ENDIF

SET RELATION TO STR(LST_NUM,6) INTO LIST_DET,  ;
                CLI_CODE       INTO ACCOUNT,   ;
                BILL_YEAR+BILL_NUM INTO BILLS, ;
                LST_ARM        INTO ARM_LST

DO CASE
CASE l_bel = 1
  arm_w = arm
  SEEK arm_w
CASE l_bel = 2
  arm_w = "***"
  SEEK arm_w
OTHERWISE
  arm_w = ""
  GO TOP
ENDCASE
PRIVATE sw1, sw2    && sw1 - ญ ซจ็จฅ กเฎญจ, sw2 - ฏฎซญฎโ  กเฎญจ
PRIVATE b_txt, m_txt, s_txt

SCAN REST WHILE LST_ARM = arm_w

  IF .NOT. EMPTY(dat_clo) .AND. (SAL_DATE > dat_clo .OR. EMPTY(SAL_DATE))
    LOOP
  ENDIF
  
  IF .NOT. EMPTY(dat_cr) .AND. CR_DAT > dat_cr
    LOOP
  ENDIF
  
  sw1 = .F.
  sw2 = .T.
  IF EMPTY(SAL_NUM)
    SELECT LIST_DET
    SCAN REST WHILE LIST_NUM = STR(LIST_TIT.LST_NUM,6)
      IF QNT_REQ # 0
        sw1 = .T.
      ENDIF
      IF QNT # QNT_REQ
        sw2 = .F.
      ENDIF
    ENDSCAN
    SELECT LIST_TIT
    IF sw1
      b_txt = IIF(sw2, "ฎซญ.", "— แโ.")
    ELSE
      b_txt = "     "
    ENDIF
  ELSE
    b_txt = "เฎค."
  ENDIF
  m_txt = "     "
  IF .NOT. EMPTY(LIST_TIT.MAIL)
    m_txt = IIF(LIST_TIT.MAIL = "1", "…แโ์", "โฏเ.")
  ENDIF
  SELECT LINK_D
  IF FOUND("BILLS")
    s_txt = LEFT(BILLS.STATUS,5)
    IF EMPTY(s_txt)
      s_txt = "๋ฏ."
    ENDIF
  ELSE
    s_txt = "     "
  ENDIF
  APPEND BLANK
  REPLACE L_NUM    WITH STR(LIST_TIT.LST_NUM,6), ;
          B_STAT   WITH b_txt, ;
          M_STAT   WITH m_txt, ;
          S_STAT   WITH s_txt, ;
          CUS_NAME WITH IIF(FOUND("ACCOUNT"), ACCOUNT.CUS_NAME, " ")
  SELECT LIST_TIT

ENDSCAN
  
*
*    PAD - ฌฅญ๎ ฏฅเขฎฃฎ ใเฎขญ๏
*
DEFINE MENU List_M IN SCREEN COLOR SCHEME 4
DEFINE PAD Mark OF List_M PROMPT "โฌฅโจโ์"
DEFINE PAD View OF List_M PROMPT "เฎแฌฎโเ" KEY Ctrl-A, "Enter"
DEFINE PAD Dele OF List_M PROMPT "“ค ซจโ์" KEY Ctrl-A, "Del"
DEFINE PAD Exit OF List_M PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"

ON PAD Mark OF List_M ACTIVATE POPUP Mark
ON SELECTION PAD View OF List_M DO Ch_Mode WITH "List_M","View"
ON SELECTION PAD Dele OF List_M DO Ch_Mode WITH "List_M","Dele"
ON SELECTION PAD Exit OF List_M DO Ch_Mode WITH "List_M","Exit"

*
*    POPUP - ฌฅญ๎ ขโฎเฎฃฎ ใเฎขญ๏
*
DEFINE POPUP Mark MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR  1 OF Mark PROMPT "โฌฅโจโ์" KEY Ctrl-A, "F3"
DEFINE BAR  2 OF Mark PROMPT "โฌฅโจโ์ ขแฅ" KEY Ctrl-A, "+"
DEFINE BAR  3 OF Mark PROMPT "็จแโจโ์ ขแฅ" KEY Ctrl-A, "-"
DEFINE BAR  4 OF Mark PROMPT "ญขฅเโจเฎข โ์ ขแฅ" KEY Ctrl-A, "*"
ON SELECTION BAR 1 OF Mark DO Mark_1
ON SELECTION BAR 2 OF Mark DO Mark_ WITH "+"
ON SELECTION BAR 3 OF Mark DO Mark_ WITH "-"
ON SELECTION BAR 4 OF Mark DO Mark_ WITH "*"

*
*    PAD - ฌฅญ๎ ขโฎเฎฃฎ ใเฎขญ๏
*
DEFINE MENU View_M IN SCREEN COLOR SCHEME 4
DEFINE PAD Exit OF View_M PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF View_M DO Ch_Mode WITH "View_M","List"


IF File_O(base_path+"DEMAND.DBF")
  SELECT 0
  USE (base_path+"DEMAND.DBF") ORDER TAG WHERE
  IF File_O(base_path+"DEM_LINK.DBF")
    SELECT 0
    USE (base_path+"DEM_LINK.DBF") ORDER TAG LIST
  ENDIF
ENDIF

DO Use_Dummy

SELECT LIST_TIT
SET ORDER TO TAG LST_NUM
SELECT LINK_D
SET RELATION TO VAL(ALLTRIM(L_NUM)) INTO LIST_TIT
PRIVATE ln, wd, tt, ft, pic_0, pic_1, pic_2

DIMENSION tt(1)

tt(1) = PADC(" จฌฅญฎข ญจฅฤฤฤฤฤฤฤ", FSIZE("PREFIX","LIST_DET")+FSIZE("NAME","LIST_DET")+1,"ฤ")+"ฤ"+  ;
            +PADC("ฎซ-ขฎ", FSIZE("QNT","LIST_DET"),"ฤ")+"ฤ"+  ;
            +PADC("–ฅญ ", FSIZE("SAL_PRICE","LIST_DET"),"ฤ")+"ฤ"+  ;
            +PADC("ฤฤ‘ใฌฌ ", FSIZE("SAL_PRICE","LIST_DET")+2,"ฤ")

*            +PADC(" โ ", IIF(SET("CENTURY") = "ON", 10, 8),"ฤ")+"ฤ"+  ;
*

pic_0 = REPLICATE("9",FSIZE("QNT","LIST_DET"))
pic_1 = "@Z "+REPLICATE("9",FSIZE("SAL_PRICE","LIST_DET")-3)+".99"
pic_2 = "@Z "+REPLICATE("9",FSIZE("SAL_PRICE","LIST_DET")-1)+".99"

ln = WROWS("") - 7
wd = LEN(tt(1))
DO Def_Win WITH "View_M", "View_E", "View_I", ln, wd, "", tt
HIDE WINDOW View_M

DIMENSION tt(1), ft(4)

tt(1) = "ฤฤ"+PADC("N แฏ.", FSIZE("L_NUM"),"ฤ")+"ฤ"+  ;
            +PADC("ฌ๏ แฏจแช ฤฤฤฤ", FSIZE("LST_NAME","LIST_TIT"),"ฤ")+"ฤ"+  ;
            +PADC("ฤฤ‘ใฌฌ ", FSIZE("R_TOTAL","LIST_TIT"),"ฤ")+"ฤ"+  ;
            +PADC("‘็ฅโ",  FSIZE("B_STAT"),"ฤ")+"ฤ"+  ;
            +PADC("เฎญ์", FSIZE("S_STAT"),"ฤ")+"ฤ"+  ;
            +PADC("ฎ็โ ", FSIZE("M_STAT"),"ฤ")

*            +PADC(" โ ", IIF(SET("CENTURY") = "ON", 10, 8),"ฤ")+"ฤ"+  ;
*

ft(1) = ""
ft(2) = PADR("ฤฤฤฤฎชใฏ โฅซ์",FSIZE("CUS_NAME","ACCOUNT"),"ฤ")+"ฤ"+ ;
        PADR(" ก. ฌฅแโฎ",FSIZE("ARM_N","ARM_LST"),"ฤ")
ft(2) = PADR(ft(2),LEN(tt(1)),"ฤ")
ft(3) = ""
ft(4) = REPLICATE("ฤ",LEN(tt(1)))


ln = WROWS("") - 10
wd = LEN(tt(1))
DO Def_Win WITH "List_M", "List_E", "List_I", ln, wd, ;
                "‘ฏจแชจ - ฏเฅโฅญคฅญโ๋ ญ  ใค ซฅญจฅ", tt, ft
ACTIVATE WINDOW List_M
@ WROWS()-3, 1 SAY "ฺ"
@ WROWS()-3, WCOLS()-2 SAY "ฟ"
@ WROWS()-2, 1 SAY "ณ"
@ WROWS()-2, WCOLS()-2 SAY "ณ"
@ WROWS()-1, 1 SAY "ภ"
@ WROWS()-1, WCOLS()-2 SAY "ู"



what_do = "List"
DO WHILE .T.
  DO CASE
  CASE what_do = "List"     && ‘ฏจแชจ
    ON KEY LABEL F3 DO Mark_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL "+" DO Mark_ WITH "+"
    ON KEY LABEL "-" DO Mark_ WITH "-"
    ON KEY LABEL "*" DO Mark_ WITH "*"
    ON KEY LABEL Del DO Ch_Mode WITH "List_M","Dele"
    ON KEY LABEL F10 ACTIVATE MENU List_M
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU List_M
    ACTIVATE WINDOW List_E
    BROWSE FIELDS MRK:H="", ;
                  L_NUM:H="",             ;
                  LIST_TIT.LST_NAME:H="", ;
                  LIST_TIT.R_TOTAL:H="",  ;
                  S_STAT:H="",            ;
                  B_STAT:H="",            ;
                  M_STAT:H="",            ;
                  DUMMY.F:H=""            ;
           FREEZE DUMMY.F WHEN Show_C()   ;
           NOMENU NORGRID NOAPPEND NODELETE NOMODIFY NOCLEAR ;
           WINDOW List_I IN WINDOW List_E
    ON KEY LABEL F3
    ON KEY LABEL Enter
    ON KEY LABEL "+"
    ON KEY LABEL "-"
    ON KEY LABEL "*"
    ON KEY LABEL Home
    ON KEY LABEL End
    ON KEY LABEL F10
    HIDE MENU List_M
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    IF what_do = "List"
      what_do = IIF(READKEY()%256=12, "Exit", "View")
    ENDIF
  CASE what_do = "View"     && ‘ฎคฅเฆจฌฎฅ แฏจแช 
    ON KEY LABEL F10 ACTIVATE MENU View_M
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
    ACTIVATE WINDOW View_M TOP
    @ 0, 0 SAY PADC(ALLTRIM(L_NUM)+" "+ALLTRIM(LIST_TIT.LST_NAME), WCOLS()) COLOR SCHEME 5
    SHOW MENU View_M
    SELECT LIST_DET
    ACTIVATE WINDOW View_E
    BROWSE FIELDS PREFIX:H="",            ;
                  NAME:H="",              ;
                  QNT:H="":P=pic_0,       ;
                  SAL_PRICE:H="":P=pic_1, ;
                  SUM000 = QNT*SAL_PRICE:H="":P=pic_2, ;
                  DUMMY.F:H=""            ;
           FREEZE DUMMY.F                 ;
           KEY    LINK_D.L_NUM            ;
           NOMENU NORGRID NOAPPEND NOMODIFY NODELETE NOCLEAR ;
           WINDOW View_I IN WINDOW View_E
    SELECT LINK_D
    ON KEY LABEL F10
    ON KEY LABEL Home
    ON KEY LABEL End
    HIDE MENU View_M
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    HIDE WINDOW View_E
    HIDE WINDOW View_M
    what_do = "List"
  CASE what_do = "Dele"     && “ค ซฅญจฅ
    DO Delete
    what_do = "List"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO
RELEASE MENU List_M EXTENDED
RELEASE MENU View_M EXTENDED
RELEASE WINDOW List_M
RELEASE WINDOW List_E
RELEASE WINDOW List_I

CLOSE DATABASES
DELETE FILE (tmpo_path+"LINK_D.DBF")

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ขๅฎค  ข ฏฎซฅ ค โ๋.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Clr_Date
PARAMETER n

IF n = 1
  ON KEY LABEL SpaceBar dat_cr = {}
ELSE
  ON KEY LABEL SpaceBar dat_clo = {}
ENDIF

@ WROWS()-1, 0 SAY PADC('"เฎกฅซ" - ฎ็จแโจโ์ ค โใ', WCOLS())

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ข๋ๅฎค  จง ฏฎซ๏ ค โ๋.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE T_Date

ON KEY LABEL SpaceBar
@ WROWS()-1, 0 SAY SPACE(WCOLS())

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ข๋ค ็จ จฌฅญจ ชซจฅญโ .                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Show_C

ACTIVATE WINDOW List_M BOTTOM
IF .NOT. FOUND("ACCOUNT")
  @ WROWS()-2, 2 SAY SPACE(FSIZE("CUS_NAME", "ACCOUNT"))+" "+ARM_LST.ARM_N
ELSE
  @ WROWS()-2, 2 SAY ACCOUNT.CUS_NAME+" "+ARM_LST.ARM_N
ENDIF

ACTIVATE WINDOW (ALIAS())
RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ               เฎฃเ ฌฌ  ใแโ ญฎขชจ/แญ๏โจ๏ ฎโฌฅโชจ.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Mark_1

REPLACE MRK WITH IIF(MRK = " ", "๛", " ")
SKIP
DEACTIVATE MENU List_M

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ               เฎฃเ ฌฌ  ฌ แแฎขฎฉ ใแโ ญฎขชจ/แญ๏โจ๏ ฎโฌฅโชจ.            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Mark_
PARAMETERS mode
PRIVATE rc

rc = RECNO()

DO CASE
CASE mode = "+"
  REPLACE ALL MRK WITH "๛"
CASE mode = "-"
  REPLACE ALL MRK WITH " "
CASE mode = "*"
  REPLACE ALL MRK WITH IIF(MRK = " ", "๛", " ")
ENDCASE
GO rc
DEACTIVATE MENU List_M
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     เฎฃเ ฌฌ  ใค ซฅญจ๏ แฏจแชฎข.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Delete
PRIVATE sw, rc

sw = .T.
rc = RECNO()

SCAN ALL
  IF .NOT. EMPTY(MRK)
    DO Del_List WITH 2
    DELETE
    sw = .F.
  ENDIF
ENDSCAN

GO rc
IF sw
  DO Del_List WITH 2
  DELETE
  SKIP
ENDIF

RETURN
