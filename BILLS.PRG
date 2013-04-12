*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  เ กฎโ๋ แฎ แ็ฅโ ฌจ.                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Bills

PRIVATE c_dat, c_stat, flt1, flt2, flt3, rc, pl_code, max_ord

max_ord = 0
IF File_O(base_path+"MAX_ORD.MEM")
  RESTORE FROM (base_path+"MAX_ORD.MEM") ADDITIVE
ENDIF

pl_code = G_Pl()
IF EMPTY(pl_code)
  RETURN
ENDIF

*
*    PAD - ฌฅญ๎ ฏฅเขฎฃฎ ใเฎขญ๏
*
DEFINE MENU B_Sale IN SCREEN COLOR SCHEME 4
DEFINE PAD Pay    OF B_Sale PROMPT "ฏซ โ "
DEFINE PAD Seek   OF B_Sale PROMPT "ฎจแช"
DEFINE PAD Print  OF B_Sale PROMPT "ฅ็ โ์ แ็ฅโ "
DEFINE PAD Filter OF B_Sale PROMPT "”จซ์โเ" KEY Ctrl-A, "F6"
DEFINE PAD Delete OF B_Sale PROMPT "“ค ซจโ์" KEY Ctrl-A, "Del"
DEFINE PAD Exit   OF B_Sale PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"

ON PAD Pay  OF B_Sale ACTIVATE POPUP Pay
ON PAD Seek OF B_Sale ACTIVATE POPUP Seek_B
ON SELECTION PAD Print  OF B_Sale DO Ch_Mode WITH "B_Sale","print"
ON SELECTION PAD Filter OF B_Sale DO Ch_Mode WITH "B_Sale","filter"
ON SELECTION PAD Delete OF B_Sale DO Del_Bill
ON SELECTION PAD Exit   OF B_Sale DO Ex_Proc WITH "B_Sale"

*
*    POPUP - ฌฅญ๎ ขโฎเฎฃฎ ใเฎขญ๏
*
DEFINE POPUP Pay MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR  1 OF Pay PROMPT "เฅคข เจโฅซ์ญ๋ฅ ค ญญ๋ฅ " KEY Ctrl-A, "F3"
DEFINE BAR  2 OF Pay PROMPT "ฎ แ็ฅโใ " KEY Ctrl-A, "Enter"
DEFINE BAR  3 OF Pay PROMPT "ฅง แ็ฅโ  " KEY Ctrl-A, "Ins"
ON SELECTION BAR 1 OF Pay DO Set_Prev
ON SELECTION BAR 2 OF Pay DO Ch_Mode WITH "B_Sale","int_pay"
ON SELECTION BAR 3 OF Pay DO Ch_Mode WITH "B_Sale","new_pay"

DEFINE POPUP Seek_B MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR  1 OF Seek_B PROMPT "ฎ ชซจฅญโใ " KEY Ctrl-A, "F7"
DEFINE BAR  2 OF Seek_B PROMPT "ฎ แใฌฌฅ " KEY Ctrl-A, "F8"
ON SELECTION BAR 1 OF Seek_B DO Ch_Mode WITH "B_Sale","seek_c"
ON SELECTION BAR 2 OF Seek_B DO Ch_Mode WITH "B_Sale","seek_s"

HIDE MENU Main_M
DO Entry_P WITH "", "B_Sale"

USE ACCOUNT ORDER TAG CUS_CODE

DO Use_Dummy
DO Use_Link

SELECT 0
USE BILLS ORDER TAG HRON
SET FILTER TO LEFT(DOC_NUM,1) = pl_code
SET RELATION TO CUS_CODE INTO ACCOUNT
GO BOTTOM
*IF BOF() .OR. EOF()
*  PRIVATE mss
*  DIMENSION mss(3)
*  mss(1) = ""
*  mss(2) = CHR(0)+"ฅ ใค ซฎแ์ ญ ฉโจ ญจ ฎคญฎฃฎ แ็ฅโ !"
*  mss(3) = ""
*  DO Out_Mess WITH 7, "mss"
*  
*  CLOSE DATABASES
*
*  DO Exit_P WITH "", "B_Sale"
*  RETURN
*ENDIF

c_dat  = 0
c_stat = 0
flt1   = .T.
flt2   = .T.
flt3   = .T.
DO Prep_W

what_do = "b_list"
DO WHILE .T.

  DO CASE
  CASE what_do = "b_list"     && ‘ฏจแฎช แ็ฅโฎข
    ON KEY LABEL F10 DO F10_Proc WITH "B_Sale"
    ON KEY LABEL F3    DO Set_Prev
    ON KEY LABEL Del   DO Del_Bill
    ON KEY LABEL Enter DO Ch_Mode WITH "B_Sale","int_pay"
    ON KEY LABEL Ins   DO Ch_Mode WITH "B_Sale","new_pay"
    ON KEY LABEL F6    DO Ch_Mode WITH "B_Sale","filter"
    ON KEY LABEL F7    DO Ch_Mode WITH "B_Sale","seek_c"
    ON KEY LABEL F8    DO Ch_Mode WITH "B_Sale","seek_s"
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "B_Sale"
    ACTIVATE WINDOW Bills_E
    BROWSE FOR Fltr(flt1, flt2, flt3)   ;
           FIELDS f001 = Doc_N():11:H="" , ;
                  BILL_SUM:H="",  ;
                  f002 = IIF(FOUND("ACCOUNT"), ACCOUNT.CUS_NAME ,  ;
                  BILLS.CUS_NAME):50:H= "", DUMMY.F:H=""   ;
           FREEZE DUMMY.F  ;
           WHEN Show_D()   ;
           NOMENU NORGRID NOAPPEND NODELETE NOMODIFY REST NOCLEAR ;
           WINDOW Bills_I IN WINDOW Bills_E

    ON KEY
    IF what_do = "b_list"
      what_do = "exit"
    ENDIF
    ON KEY
  CASE what_do = "seek_c"     && ขฎค ชฎญโฅชแโ  คซ๏ ฏฎจแช  ชซจฅญโ 
    what_do = IIF(Prep_C(), "choose", "b_list")
  CASE what_do = "seek_s"     && ขฎค แใฌฌ คซ๏ ฏฎจแช  ฏฎ แใฌฌฅ
    what_do = IIF(Prep_S(), "choose", "b_list")
  CASE what_do = "choose"     && ๋กฎเ จง ญ ฉคฅญญฎฃฎ
    DO Choose
    what_do = "b_list"
  CASE what_do = "filter"     && “แโ ญฎขช  ไจซ์โเ 
    DO Set_Flt
    what_do = "b_list"
  CASE what_do = "int_pay"    && ขฎค ฏซ โฅฆชจ ฏฎ แ็ฅโใ
    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Inp_Pay WITH "B_Sale", .T.
    ENDIF
    what_do = "b_list"
  CASE what_do = "new_pay"    && ขฎค ฏซ โฅฆชจ กฅง แ็ฅโ 
    DO Inp_Pay WITH "B_Sale", .F.
    what_do = "b_list"
  CASE what_do = "print"    && ฅ็ โ์ แ็ฅโ 
    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_Bill
    ENDIF
    what_do = "b_list"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

RELEASE WINDOW Bills_M
RELEASE WINDOW Bills_E
RELEASE WINDOW Bills_I
CLOSE DATABASES

DO Exit_P WITH "", "B_Sale"
RETURN

*************************************************************************
PROCEDURE Doc_N
RETURN LEFT(BILLS.DOC_NUM,4)+"-"+ALLTRIM(RIGHT(BILLS.DOC_NUM,6))

*************************************************************************
PROCEDURE Show_D

IF FOUND("ACCOUNT")
  IF .NOT. EMPTY(ACCOUNT.DATE_OFF)
    WAIT " €…! ซจฅญโ ใค ซฅญ! " WINDOW NOWAIT
  ENDIF
ENDIF
ACTIVATE WINDOW Bills_M BOTTOM
@ WROWS()-1, c_dat  SAY TRANSFORM(BILLS.DOC_DATE,"@D") COLOR SCHEME 1
@ WROWS()-1, c_stat SAY BILLS.STATUS COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS())

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ฎคฃฎโฎขชจ ฎชฎญ.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prep_W

PRIVATE l, c, ln, wd, tit1, tit2, foot
DIMENSION tit2(1)
*
*   แญฎขญฎฉ ฏฅเฅ็ฅญ์ ฏฎซฅฉ คซ๏ BROWSE
*
tit2(1) = PADC("ฤN ‘็ฅโ ",FSIZE("DOC_NUM" ,"BILLS")+1,"ฤ")+"ฤ"+ ;
          PADC("ฤฤ‘ใฌฌ ",FSIZE("BILL_SUM" ,"BILLS"),"ฤ")+"ฤ"+ ;
          PADC("ซจฅญโฤฤฤฤฤฤฤฤ",FSIZE("CUS_NAME" ,"BILLS"),"ฤ")
ln = WROWS("")-7
tit1 = "‘ฏจแฎช แ็ฅโฎข"
DIMENSION foot(2)
foot(1) = ""
foot(2) = " โ                      ‘ฎแโฎ๏ญจฅ 0123456789"

DO Def_Win WITH "Bills_M", "Bills_E", "Bills_I", ln, LEN(tit2(1)), tit1, tit2, foot
c = PADC(foot(2),WCOLS())
@ WROWS()-1, 0 SAY c
c_dat  = AT(" โ ", c)+4
c_stat = AT("0123456789", c)-1

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ        เฎฃเ ฌฌ  - ฎกเ กฎโ็จช ฌ๋่ชจ คซ๏ แฏจแชฎข.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE M_Proc_1
PARAMETER menu_nm
PRIVATE mc, mr

*
*  €แจญๅเฎญญฎฅ ฌฅญ๎
*
IF MENU() == UPPER(menu_nm)
  RETURN
ENDIF

IF MROW("") = 0
  ACTIVATE MENU (menu_nm)
  RETURN
ENDIF

*
*  ชญฎ BROWSE
*
mc = MCOL("Bills_E")
mr = MROW("Bills_E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  แญฎขญฎฅ ฎชญฎ
*
mc = MCOL("Bills_M")
mr = MROW("Bills_M")
IF mc >= 0 .AND. mr >= 0
  IF mr = 1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WROWS("Bills_M")-2
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ใแโ ญฎขชจ ไจซ์โเ .                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_Flt

PRIVATE f1, f2, f3, ex, rc_sav

DO Entry_P WITH "B_Sale", ""

f1 = flt1
f2 = flt2
f3 = flt3
ex = 1

rc_sav = RECNO()

ON KEY LABEL LeftMouse DO Ms_Lock
DO D_W WITH "Filter", 8, 67, "”จซ์โเ", 1

@ 3, 5 GET f1 PICTURE "@*C ฅฎฏซ ็ฅญญ๋ฅ"
@ 3,24 GET f2 PICTURE "@*C เฅคข. ค ญญ๋ฅ"
@ 3,44 GET f3 PICTURE "@*C ฏซ ็ฅญญ๋ฅ แ็ฅโ "
@ 5,15 GET ex PICTURE "@*HT \! ‘ฎๅเ ญจโ์ Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE VALID Empt_List()

RELEASE WINDOW Filter

DO Exit_P WITH "B_Sale", ""

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ฏฎค ขซฅญจ๏ ฌ๋่ชจ ขญฅ ฎชญ .                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Ms_Lock

IF MROW() < 0 .OR. MCOL() < 0
  = INKEY("M")
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                         ”จซ์โเ แ็ฅโฎข.                                บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Fltr
PARAMETERS f01, f02, f03

DO CASE
CASE EMPTY(BILLS.STATUS)
  RETURN f01
CASE ALLTRIM(BILLS.STATUS) = "ฏซ ็ฅญฎ"
  RETURN f03
OTHERWISE
  RETURN f02
ENDCASE

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     € ญฅ ฏใแโ ซจ แฏจแฎช?                              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Empt_List

IF ex = 2
  RETURN .T.
ENDIF

PRIVATE err_m
IF .NOT. f1 .AND. .NOT. f2 .AND. .NOT. f3
  DIMENSION err_m(4)
  err_m(1) = ""
  err_m(2) = CHR(0)+" ่ฅ ฆฅซ ญจฅ ญจ็ฅฃฎ ญฅ ขจคฅโ์ ฏฎญ๏โญฎ จ ขแฅ ฆฅ..."
  err_m(3) = CHR(0)+"๋กฅเจโฅ ๅฎโ์ ็โฎ-ญจกใค์."
  err_m(4) = ""
  DO Out_Mess WITH 7, "err_m"
  RETURN .F.
ENDIF
IF .NOT. Fltr(f1, f2, f3)
  LOCATE FOR Fltr(f1, f2, f3)
  IF .NOT. FOUND()
    GO rc_sav
    DIMENSION err_m(3)
    err_m(1) = ""
    err_m(2) = CHR(0)+"ฅโ แ็ฅโฎข, แฎฎโขฅโแโขใ๎้จๅ  ่ฅฌใ ใแซฎขจ๎."
    err_m(3) = ""
    DO Out_Mess WITH 7, "err_m"
    RETURN .F.
  ENDIF
ENDIF
flt1 = f1
flt2 = f2
flt3 = f3

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                          “ค ซฅญจฅ แ็ฅโ .                              บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Del_Bill

DELETE
DEACTIVATE MENU B_Sale
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ          “แโ ญฎขช /แญ๏โจฅ ฏฎฌฅโชจ ฎ ฏเฅคข. แขฅคฅญจ๏ๅ.                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_Prev

DO CASE
CASE EMPTY(BILLS.STATUS)
  REPLACE BILLS.STATUS WITH "เฅคข."
  = Show_D()
CASE ALLTRIM(BILLS.STATUS) = "ฏซ ็ฅญฎ"
  PRIVATE err_m
  DIMENSION err_m(3)
  err_m(1) = ""
  err_m(2) = CHR(0)+" ชจฅ ฏเฅคข เจโฅซ์ญ๋ฅ ค ญญ๋ฅ? ‘็ฅโ ใฆฅ ฎฏซ ็ฅญ!"
  err_m(3) = ""
OTHERWISE
  REPLACE BILLS.STATUS WITH "   "
  = Show_D()
ENDCASE

DEACTIVATE MENU B_Sale

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎฃเ ฌฌ  ฏฎคฃฎโฎขชจ ไ ฉซ  LINK คซ๏ ชซจฅญโ .             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prep_C

PRIVATE shab, ex, rc_sav, nm

DO Entry_P WITH "B_Sale", ""

ex = 1
rc_sav = RECNO()
shab = SPACE(50)

ON KEY LABEL LeftMouse DO Ms_Lock
DO D_W WITH "Seek_W", 8, 76, "ฎจแช แ็ฅโ  ฏฎ ชซจฅญโใ", 1

@ 3, 4 SAY " กซฎญ คซ๏ ฏฎจแช " GET shab
@ 5,20 GET ex PICTURE "@*HT \! แช โ์ Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE

RELEASE WINDOW Seek_W

IF EMPTY(shab) .OR. READKEY()%256 = 12
  ex = 2
ENDIF

IF ex = 1
  SELECT LINK
  ZAP
  DO Wt_Mess WITH "้ฅฌ..."
  shab = ALLTRIM(shab)
  shab = SYS(15,lwr,shab)
  SELECT BILLS
  SCAN 
    IF .NOT. Fltr(flt1, flt2, flt3)
      LOOP
    ENDIF
    nm = IIF(FOUND("ACCOUNT"), ACCOUNT.CUS_NAME , BILLS.CUS_NAME)
    nm = SYS(15,lwr,nm)
    IF shab $ nm
      SELECT LINK
      APPEND BLANK
      REPLACE LNK WITH RECNO("BILLS")
      SELECT BILLS
    ENDIF
  ENDSCAN
  GO rc_sav
  DO Wt_Mess
  IF RECCOUNT("LINK") = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"จ็ฅฃฎ ญฅ ญ ฉคฅญฎ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    ex = 2
  ENDIF
ENDIF

DO Exit_P WITH "B_Sale", ""

RETURN ex = 1

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ           เฎฃเ ฌฌ  ฏฎคฃฎโฎขชจ ไ ฉซ  LINK คซ๏ แใฌฌ.                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prep_S

PRIVATE sum0, sum1, ex, rc_sav, pic

DO Entry_P WITH "B_Sale", ""

ex = 1
rc_sav = RECNO()
sum0 = 0
sum1 = 0
pic = "@Z 9999999.99"

ON KEY LABEL LeftMouse DO Ms_Lock
DO D_W WITH "Seek_W", 8, 42, "ฎจแช แ็ฅโ  ฏฎ แใฌฌฅ", 1

@ 3, 5 SAY "‘ใฌฌ  แ" GET sum0 PICTURE pic
@ 3,24 SAY "ฏฎ"      GET sum1 PICTURE pic

@ 5,3 GET ex PICTURE "@*HT \! แช โ์ Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE VALID V_Sum()

RELEASE WINDOW Seek_W

IF ex = 2 .OR. READKEY()%256 = 12
  ex = 2
ENDIF

IF ex = 1
  SELECT LINK
  ZAP
  DO Wt_Mess WITH "้ฅฌ..."
  SELECT BILLS
  SCAN
    IF .NOT. Fltr(flt1, flt2, flt3) .OR. .NOT. BETWEEN(BILL_SUM, sum0, sum1)
      LOOP
    ENDIF
    SELECT LINK
    APPEND BLANK
    REPLACE LNK WITH RECNO("BILLS")
    SELECT BILLS
  ENDSCAN
  GO rc_sav
  DO Wt_Mess
  IF RECCOUNT("LINK") = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"จ็ฅฃฎ ญฅ ญ ฉคฅญฎ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    ex = 2
  ENDIF
ENDIF

DO Exit_P WITH "B_Sale", ""

RETURN ex = 1

***************************************************************************
PROCEDURE V_Sum
PRIVATE mss

IF ex = 2 .OR. READKEY()%256 = 12
  ex = 2
  RETURN .T.
ENDIF

IF sum0 < 0 .OR. sum1 < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"โเจๆ โฅซ์ญ ๏ แใฌฌ ? ’ ช ญฅ ก๋ข ฅโ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF sum0 > sum1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ็ ซ์ญ ๏ แใฌฌ  กฎซ์่ฅ ชฎญฅ็ญฎฉ? —โฎ-โฎ งคฅแ์ ญฅ โ ช!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ            เฎฃเ ฌฌ  ข๋กฎเ  จง แไฎเฌจเฎข ญญฎฃฎ แฏจแช .                บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Choose

PRIVATE rc

rc = RECNO()

DEFINE MENU Choose IN SCREEN COLOR SCHEME 4
DEFINE PAD Select OF Choose PROMPT "๋กฎเ" KEY Ctrl-A, "Enter"
DEFINE PAD Abort  OF Choose PROMPT "โช ง" KEY Ctrl-A, "Esc"

*ON SELECTION PAD Select OF Choose KEYBOARD CHR(27)+CHR(13)
*ON SELECTION PAD Abort  OF Choose KEYBOARD CHR(27)+CHR(27)
ON SELECTION PAD Select OF Choose DO Out_Menu WITH CHR(13)
ON SELECTION PAD Abort  OF Choose DO Out_Menu WITH CHR(27)


SET ORDER TO 0

SELECT LINK
SET RELATION TO LNK INTO BILLS
DO Entry_P WITH "B_Sale", "Choose"

ON KEY LABEL Enter KEYBOARD CHR(23)
ON KEY LABEL Home GO TOP
ON KEY LABEL End  GO BOTTOM
ON KEY LABEL LeftMouse DO M_Proc_1 WITH "Choose"
ACTIVATE WINDOW Bills_E
BROWSE FIELDS f001 = Doc_N():11:H="" , ;
              BILLS.BILL_SUM:H="",  ;
              f002 = IIF(FOUND("ACCOUNT"), ACCOUNT.CUS_NAME ,  ;
              BILLS.CUS_NAME):50:H= "", DUMMY.F:H=""   ;
       FREEZE DUMMY.F  ;
       WHEN Show_D()   ;
       NOMENU NORGRID NOAPPEND NODELETE NOMODIFY REST NOCLEAR ;
       WINDOW Bills_I IN WINDOW Bills_E
ON KEY LABEL Enter
ON KEY LABEL Home
ON KEY LABEL End
ON KEY LABEL LeftMouse
SET RELATION TO
SELECT BILLS
SET ORDER TO TAG HRON
IF READKEY() % 256 = 12
  GO rc
ENDIF
DO Exit_P WITH "B_Sale", "Choose"
RETURN

*********************************************************************
PROCEDURE Out_Menu
PARAMETER c

KEYBOARD c
DEACTIVATE MENU Choose
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      เฎฃเ ฌฌ  ข๋กฎเ  ฏซฎ้ คชจ.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE G_Pl

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

PRIVATE ex, pl_w, ln, wd, c1, c2
                       &&   ก๊๏ขซ๏ฅฌ จ ง ฏฎซญ๏ฅฌ ฏฎซ๏ กซ ญช 
ex   = 1               &&
pl_w = 1               &&
USE (base_path+"PLACES")
ln = RECCOUNT()+5
wd = MAX(38, FSIZE("P_NAME")+12)
c1 = FLOOR((wd-FSIZE("P_NAME")-4)/2)
c2 = FLOOR((wd-32)/2)

*    ๋กฅเจโฅ ฏซฎ้ คชใ
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ  ( ) ซฎ้ คช  1                  ณ
*ณ  ( ) ซฎ้ คช  2                  ณ
*ณ  ..............                  ณ
*ณ  ( ) ซฎ้ คช  N                  ณ
*ณ                                  ณ
*ณ < OK Ctrl-W > < โช ง โ์แ๏ Esc > ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, "๋กฅเจโฅ ฏซฎ้ คชใ"

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
DO G_Place WITH 2, c1, pl_w
@ WROWS()-2, c2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE

IF ex = 1
  GO pl_w
  pl_w = P_CHAR
ELSE
  pl_w = ""
ENDIF
USE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN pl_w
