*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ  “แโ ญฎขชจ แชจคชจ คซ๏ "็ใฆจๅ" แฏจแชฎข.            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Strng_Di

PRIVATE cl_w, i001, i002, cl_w1, mss, what_do, ln, wd, tt, ft
PRIVATE p_cli, p_arm

IF acc_level > 1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"“  แ ญฅโ แ ญชๆจจ ญ  ใโขฅฆคฅญจฅ ๆฅญ."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

cl_w = SCHEME(13)
i001 = AT(",", cl_w, 6)+1
i002 = AT(",", cl_w, 7)-i001
cl_w1 = STUFF(cl_w, i001, i002, SCHEME(14,1))
cl_w  = STUFF(cl_w, i001, i002, SCHEME(14,2))

HIDE MENU Main_M

DEFINE MENU L_Sale IN SCREEN COLOR SCHEME 4
DEFINE PAD LS_List OF L_Sale PROMPT "‘ฏจแฎช" KEY Ctrl-A, "Enter"
DEFINE PAD LS_Info OF L_Sale PROMPT "ญไฎเฌ ๆจ๏" KEY Ctrl-A, "F9"
DEFINE PAD LS_Let  OF L_Sale PROMPT "“โขฅเคจโ์" KEY Ctrl-A, "Tab"
DEFINE PAD LS_Exit OF L_Sale PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"

ON SELECTION PAD LS_List OF L_Sale Do Ch_Mode WITH "L_Sale","list"
ON SELECTION PAD LS_Info OF L_Sale Do Ch_Mode WITH "L_Sale","info"
ON SELECTION PAD LS_Let  OF L_Sale Do Ch_Mode WITH "L_Sale","let"
ON SELECTION PAD LS_Exit OF L_Sale Do Ch_Mode WITH "L_Sale","exit"

DO Use_Dummy

DO Use_Link

SELECT 0
USE ACCOUNT ORDER TAG CUS_CODE

SELECT 0
USE ARM_LST ORDER TAG ARM_C

SELECT 0
USE LIST_TIT
SET RELATION TO LST_ARM INTO ARM_LST, CLI_CODE INTO ACCOUNT

DO Prp_Link
SELECT LINK
SET RELATION TO LNK INTO LIST_TIT

what_do = "main"
DIMENSION tt(1), ft(5)
wd = FSIZE("LST_NUM","LIST_TIT")+FSIZE("LST_NAME","LIST_TIT")+25
*     FSIZE("PRESENT","LIST_TIT")+FSIZE("R_TOTAL","LIST_TIT")+5
tt(1) = "ฤฤ"+ ;
        PADC("ฤฤ NN ",FSIZE("LST_NUM","LIST_TIT"),"ฤ")+"ฤ"+ ;
        PADC(" ฌ๏ แฏจแช  ฤฤฤฤฤฤฤฤฤฤ",FSIZE("LST_NAME","LIST_TIT"),"ฤ")+"ฤ"+ ;
        PADC("ฤ ‘ใฌฌ  ",10,"ฤ")+"ฤ"+ ;
        PADC("ฤ ฎค เฎช ",10,"ฤ")

ft(1) = ""
ft(2) = ""
ft(3) = PADR("ฎชใฏ โฅซ์",LEN(tt(1)))
ft(4) = PADR(" กฎ็ฅฅ ฌฅแโฎ",LEN(tt(1)))
ft(5) = ""

ln = WROWS("")-11

DO Def_Win WITH "WLS_M", "WLS_E", "WLS_I", ln, wd, ;
       "‘ฏจแชจ คซ๏ ใโขฅเฆคฅญจ๏", tt, ft
@ WROW()-4, 1 TO WROW()-1, WCOL()-2
p_cli = 13
p_arm = 16
SELECT LINK
DO WHILE RECCOUNT("LINK") # 0
  DO CASE
  CASE what_do = "main"
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU L_Sale
    ACTIVATE WINDOW "WLS_E"
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "L_Sale", "WLS_M", "WLS_E"
    ON KEY LABEL F10   DO Menu_Ch
    ON KEY LABEL Tab   DO Ch_Mode WITH "L_Sale","let"
    ON KEY LABEL Enter DO Ch_Mode WITH "L_Sale","list"
    ON KEY LABEL F9    DO Ch_Mode WITH "L_Sale","info"
    BROW FIELDS f00 = IIF(EMPTY(LIST_TIT.WHO_LET), " ", "๛"):H="", ;
                LIST_TIT.LST_NUM:H="",  ;
                LIST_TIT.LST_NAME:H="", ;
                LIST_TIT.R_TOTAL:H="":P="@Z 9999999.99",  ;
                LIST_TIT.PRESENT:H="":P="@Z 9999999.99",  ;
                DUMMY.F:H=""            ;
         FREEZE DUMMY.F ;
         WHEN Show_I()  ;
         NOMENU NORGRID NOAPPEND NOCLEAR WINDOW WLS_I IN WINDOW WLS_E
    ON KEY LABEL LeftMouse
    ON KEY LABEL Tab
    ON KEY LABEL Enter
    ON KEY LABEL F9
    IF what_do = "main"
      what_do = "exit"
    ENDIF
  CASE what_do = "let"
    DO Let_Sale
    what_do = "main"
  CASE what_do = "info"
    DO Dis_Info
    what_do = "main"
  CASE what_do = "list"
    DO Show_Lst
    what_do = "main"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

RELEASE WINDOW WLS_M
RELEASE WINDOW WLS_E
RELEASE WINDOW WLS_I
RELEASE MENU L_Sale
CLOSE DATABASES
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ขๅฎค  ข ง ฏจแ์.                           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Show_I
PRIVATE w_n, w_a

ACTIVATE WINDOW WLS_M BOTTOM

IF FOUND("ACCOUNT")
  w_n = ACCOUNT.CUS_NAME
ELSE
  w_n = PADR("ฎข๋ฉ", FSIZE("CUS_NAME","ACCOUNT"))
ENDIF

IF LIST_TIT.LST_ARM = "***"
  w_a = PADR("ก้จฉ แฏจแฎช", FSIZE("ARM_N","ARM_LST"))
ELSE
  w_a = ARM_LST.ARM_N
ENDIF
@ WROWS()-3, p_cli SAY w_n COLOR (SCHEME(14,2))
@ WROWS()-2, p_arm SAY w_a COLOR (SCHEME(14,2))

ACTIVATE WINDOW (ALIAS())

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎฃเ ฌฌ  ฏฎคฃฎโฎขชจ แฏจแชฎข.                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prp_Link

PRIVATE mss

SELECT LINK
ZAP
SELECT LIST_TIT
SCAN
  IF EMPTY(IS_DIS) .AND. EMPTY(IS_SPEC) .AND. EMPTY(PRESENT)
    LOOP
  ENDIF
  IF .NOT. EMPTY(SAL_NUM)   && .OR. .NOT. EMPTY(WHO_LET)
    LOOP
  ENDIF
  SELECT LINK
  APPEND BLANK
  REPLACE LNK WITH RECNO("LIST_TIT")
  SELECT LIST_TIT
ENDSCAN

IF RECCOUNT("LINK") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ แฏจแชฎข, โเฅกใ๎้จๅ ใโขฅเฆคฅญจ๏!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

RETURN


*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎฃเ ฌฌ   ชโจขจง ๆจจ ฌฅญ๎.                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Menu_Ch

PUSH KEY CLEAR
ACTIVATE MENU L_Sale
POP KEY

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎฃเ ฌฌ  ข๋ขฎค  ฎญไฎเฌ ๆจจ ฏฎ แใฌฌ ฌ.                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Dis_Info

PRIVATE sel, k_w, s11, s12, s21, s22, ln, wd, l, c

PUSH KEY CLEAR
ACTIVATE SCREEN
@ 0,0 SAY SPACE(WCOLS()) COLOR SCHEME 13
sel = SELECT()
SELECT 0
USE LIST_DET ORDER TAG L_NAME
k_w = STR(LIST_TIT.LST_NUM,6)
STORE 0 TO s11, s12, s21, s22
SEEK k_w
SCAN REST WHILE LIST_NUM = k_w
  IF .NOT. EMPTY(IS_SPEC)
    s11 = s11+QNT*DEF_PRICE
    S12 = s12+QNT*SAL_PRICE
  ELSE
    s21 = s21+QNT*DEF_PRICE
    S22 = s22+QNT*SAL_PRICE
  ENDIF
ENDSCAN
USE
SELECT (sel)

ln = 11
wd = 47
DO D_W WITH "W_Info", ln, wd, "‘ใฌฌ๋ ฏฎ ฏฎงจๆจ๏ฌ", 0

@ 1, 1 SAY "ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
@ 2, 1 SAY "ณ               ณ ญ ็. ๆฅญ ๅ ณ ๆฅญ ๅ ฏเฎค.ณ"
@ 3, 1 SAY "รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤด"
@ 4, 1 SAY "ณ แฎ แฏฅๆๆฅญ ฌจ ณ             ณ             ณ"
@ 5, 1 SAY "รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤด"
@ 6, 1 SAY "ณ ฎแโ ซ์ญ๋ฌ     ณ             ณ             ณ"
@ 7, 1 SAY "รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤด"
@ 8, 1 SAY "ณ โฎฃฎ         ณ             ณ             ณ"
@ 9, 1 SAY "ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤู"
@10, 0 SAY PADC(" ฆฌจโฅ ซ๎กใ๎ ชซ ขจ่ใ", WCOLS())

@ 4,19 SAY TRANSFORM(s11, "@Z 9999999.99")
@ 4,33 SAY TRANSFORM(s12, "@Z 9999999.99")

@ 6,19 SAY TRANSFORM(s21, "@Z 9999999.99")
@ 6,33 SAY TRANSFORM(s22, "@Z 9999999.99")

@ 8,19 SAY TRANSFORM(s11+s21, "@Z 9999999.99")
@ 8,33 SAY TRANSFORM(s12+s22, "@Z 9999999.99")

ln = SET("CURSOR") = "ON"
SET CURSOR OFF
= INKEY(0,"M")
IF ln
  SET CURSOR ON
ENDIF

RELEASE WINDOW W_Info

POP KEY
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ        เฎฃเ ฌฌ  - ฎกเ กฎโ็จช ฌ๋่ชจ คซ๏ แฏจแชฎข.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE M_Proc_1
PARAMETER menu_nm, m_win, e_win
PRIVATE mc, mr

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
mc = MCOL(e_win)
mr = MROW(e_win)
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  แญฎขญฎฅ ฎชญฎ
*
mc = MCOL(m_win)
mr = MROW(m_win)
IF mc >= 0 .AND. mr >= 0
  IF mr = 1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WROWS(m_win)-1
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎฃเ ฌฌ  ฏเฎแฌฎโเ  แฏจแช .                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Show_Lst
PRIVATE ln, wd, tt, ft, tm, sel, k_w

PUSH KEY CLEAR
ACTIVATE SCREEN
@ 0,0 SAY SPACE(WCOLS()) COLOR SCHEME 13
sel = SELECT()
SELECT 0
USE STOCK ORDER TAG CODE
SELECT 0
USE LIST_DET ORDER TAG L_NAME
SET RELATION TO CODE INTO STOCK
k_w = STR(LIST_TIT.LST_NUM,6)

DIMENSION tt(1), ft(3)
wd = FSIZE("PREFIX","LIST_DET")+25+FSIZE("QNT","LIST_DET")+34
tt(1) = PADR("ฤฤฤฤ จฌฅญฎข ญจฅ",FSIZE("PREFIX","LIST_DET")+  ;
             26,"ฤ")+"ฤ"+ ;
        PADC("ฎซ-ขฎ",FSIZE("QNT","LIST_DET"),"ฤ")+"ฤ"+ ;
        PADC(" ็.ๆฅญ ",10,"ฤ")+"ฤ"+ ;
        PADC("–ฅญ  ฏเฎค.",10,"ฤ")+"ฤ‘ช.ฤ‘ฏฅๆ."

ft(1) = ""
ft(2) = ""
ft(3) = "Esc - ข๋ๅฎค"
tm = "N "+ALLTRIM(k_w)+" "+ALLTRIM(LIST_TIT.LST_NAME)
ln = WROWS("")-11

DO Def_Win WITH "LS_M", "LS_E", "LS_I", ln, wd, ;
       tm, tt, ft
ACTIVATE WINDOW "LS_E"
ON KEY LABEL LeftMouse DO M_Proc_1 WITH "", "LS_M", "LS_E"
BROW FIELDS f0000 = IIF(FOUND("STOCK"), STOCK.PREFIX, LIST_DET.PREFIX):H="",  ;
            f0001 = IIF(FOUND("STOCK"), ;
              Nm_Part(STOCK.NAME), ;
              Nm_Part(LIST_DET.NAME)):25:H="",  ;
            LIST_DET.QNT:H="", ;
            LIST_DET.DEF_PRICE:10:H="", ;
            LIST_DET.SAL_PRICE:10:H="", ;
            f0002 = IIF(LIST_DET.SAL_PRICE#LIST_DET.DEF_PRICE .AND. EMPTY(IS_SPEC), " ๛ ", "   "):H="", ;
            f0003 = IIF(LIST_DET.SAL_PRICE#LIST_DET.DEF_PRICE .AND. .NOT. EMPTY(IS_SPEC), "  ๛  ", "     "):H="", ;
            DUMMY.F:H=""            ;
         FREEZE DUMMY.F ;
         WHEN Draw_Foot()    ;
         KEY k_w        ;
         NOMENU NORGRID NOAPPEND WINDOW LS_I IN WINDOW LS_E
RELEASE WINDOW LS_M
RELEASE WINDOW LS_E
RELEASE WINDOW LS_I
USE
SELECT STOCK
USE
SELECT (sel)
POP KEY

RETURN

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  Draw_Foot     งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                   จแใฅฌ คฎฏฎซญจโฅซ์ญใ๎ จญไฎเฌ ๆจ๎.                    ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 20.10.1998 ฤฤู
PROCEDURE Draw_Foot

ACTIVATE WINDOW LS_M SAME

*
*   ฎโ งคฅแ์, เจแใฅฌ...
*
@ WROWS()-2, 2 SAY IIF(FOUND("STOCK"), STOCK.PREFIX, LIST_DET.PREFIX) COLOR SCHEME 1
@ ROW(), COL()+1 SAY IIF(FOUND("STOCK"), STOCK.NAME,LIST_DET.NAME) COLOR SCHEME 1
@ ROW(), COL()+1 SAY IIF(FOUND("STOCK"), STOCK.PRODUCER,LIST_DET.PRODUCER) COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
