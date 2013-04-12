*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎแฌฎโเ ฏซ โฅฆญ๋ๅ ฏฎเใ็ฅญจฉ.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE View_Pay

PRIVATE d1, d2, p_line, p_numb

USE PLACES
p_numb = RECCOUNT()
p_line = ""
FOR d1 = 1 TO p_numb
  p_line = p_line+CHR(ASC("A")+d1-1)
ENDFOR
USE

d1 = {}
d2 = DATE()

*
*    PAD - ฌฅญ๎ ฏฅเขฎฃฎ ใเฎขญ๏
*
DEFINE MENU W_Pay IN SCREEN COLOR SCHEME 4
DEFINE PAD Seek  OF W_Pay PROMPT "ซจฅญโ" KEY Ctrl-A, "F7"
DEFINE PAD Fltr  OF W_Pay PROMPT "”จซ์โเ" KEY Ctrl-A, "F3"
DEFINE PAD Print OF W_Pay PROMPT "ฅ็ โ์" KEY Ctrl-A, "Enter"
DEFINE PAD Exit  OF W_Pay PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"

ON SELECTION PAD Seek  OF W_Pay DO Ch_Mode WITH "W_Pay","seek"
ON SELECTION PAD Fltr  OF W_Pay DO Ch_Mode WITH "W_Pay","fltr"
ON SELECTION PAD Print OF W_Pay DO Ch_Mode WITH "W_Pay","print"
ON SELECTION PAD Exit  OF W_Pay DO Ex_Proc WITH "W_Pay"

HIDE MENU Main_M
DO Entry_P WITH "", "W_Pay"

DO Use_Dummy
DO Use_Link

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
SELECT 0
USE (base_path+"BILLS") ORDER TAG HRON
SELECT INCMONEY
SET RELATION TO CUS_CODE INTO ACCOUNT,   ;
                BILL_FIRM+LEFT(DTOS(BILL_DAT),4)+BILL_NUM INTO BILLS 

GO BOTTOM
DO Prep_W

PRIVATE what_do, fl, cus_
what_do = "b_list"
cus = 0
fl = .F.
DO WHILE .T.

  DO CASE
  CASE what_do = "b_list"     && ‘ฏจแฎช แ็ฅโฎข
    SHOW MENU W_Pay
    ON KEY LABEL Enter DO Ch_Mode WITH "W_Pay","print"
    ON KEY LABEL F7    DO Ch_Mode WITH "W_Pay","seek"
    ON KEY LABEL F3    DO Ch_Mode WITH "W_Pay","fltr"
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "W_Pay"
    ACTIVATE WINDOW Pay_E
    IF fl
      SET ORDER TO TAG CUS_CODE
    ELSE
      SET ORDER TO TAG HRON
    ENDIF
    BROWSE FOR DOC_TYPE = "1" .AND. LEFT(DOC_NUM,1) $ p_line ;
               .AND. BETWEEN(ENTER_DAT, d1, d2)  ;
           FIELDS f001 = Doc_N(INCMONEY.DOC_NUM,1):11:H="" , ;
                  INCMONEY.DOC_DATE:H="" , ;
                  f002 = Doc_N(INCMONEY.BILL_NUM,4):11:H="" , ;
                  INCMONEY.ENTER_DOC:H="" , ;
                  INCMONEY.ENTER_DAT:H="" , ;
                  INCMONEY.MONEY_R:H="" , ;
                  DUMMY.F:H=""   ;
           FREEZE DUMMY.F  ;
           WHEN Show_Cus() ;
           KEY IIF(fl, cus_, "")         ;
           NOMENU NORGRID NOAPPEND NODELETE NOMODIFY REST NOCLEAR ;
           WINDOW Pay_I IN WINDOW Pay_E

    ON KEY LABEL F7
    ON KEY LABEL F3
    ON KEY LABEL Enter
    ON KEY LABEL Ins
    ON KEY LABEL Home
    ON KEY LABEL End
    ON KEY LABEL LeftMouse
    IF what_do = "b_list"
      what_do = "exit"
    ENDIF
  CASE what_do = "seek"       && “แโ ญฎขช  ไจซ์โเ  ฏฎ ชซจฅญโใ
    fl = Custs("W_Pay", .F., .F.)
    cus_ = ACCOUNT.CUS_CODE
    what_do = "b_list"
  CASE what_do = "print"      && ฅ็ โ์ ฏซ โฅฆชจ
    DO P_Inc_O WITH DOC_NUM, DOC_DATE, DOC_TYPE
    what_do = "b_list"
  CASE what_do = "fltr"       && “แโ ญฎขช  ไจซ์โเ 
    DO S_Fltr
    what_do = "b_list"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

CLOSE DATABASES
CLEAR WIND
DO Exit_P WITH "", "W_Pay"
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ฎคฃฎโฎขชจ ฎชฎญ.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prep_W

PRIVATE l, c, ln, wd, tit1, tit2, foot, dat_ln
DIMENSION tit2(1)
*
*   แญฎขญฎฉ ฏฅเฅ็ฅญ์ ฏฎซฅฉ คซ๏ BROWSE
*
dat_ln = IIF(SET("CENTURY") = "ON", 10, 8)
tit2(1) = PADC("ฤN ฎช_โ ",FSIZE("DOC_NUM" ,"INCMONEY")+1,"ฤ")+"ฤ"+ ;
          PADC(" โ ",dat_ln,"ฤ")+"ฤ"+ ;
          PADC("ฤN ‘็ฅโ ",FSIZE("BILL_NUM" ,"INCMONEY")+1,"ฤ")+"ฤ"+ ;
          PADC("ซ โ. ฏฎเใ็ฅญจฅ",FSIZE("ENTER_DOC" ,"INCMONEY"),"ฤ")+"ฤ"+ ;
          PADC(" โ ",dat_ln,"ฤ")+"ฤ"+ ;
          PADC("ฤฤ‘ใฌฌ ",FSIZE("MONEY_R" ,"INCMONEY"),"ฤ")
ln = WROWS("")-7
tit1 = "‘ฏจแฎช ฏซ โฅฆฅฉ"
DIMENSION foot(2)
foot(1) = ""
foot(2) = PADR("   ซจฅญโ",LEN(tit2(1)))

DO Def_Win WITH "Pay_M", "Pay_E", "Pay_I", ln, LEN(tit2(1)), tit1, tit2, foot

RETURN

****************************************************************************
PROCEDURE Doc_N
PARAMETER s, n
RETURN LEFT(s,n)+"-"+ALLTRIM(SUBSTR(s,n+1))

****************************************************************************
PROCEDURE Show_Cus

ACTIVATE WINDOW Pay_M BOTTOM
@ WROWS()-1, 12 SAY ACCOUNT.CUS_NAME
ACTIVATE WINDOW (ALIAS())
RETURN .T.

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
mc = MCOL("Pay_E")
mr = MROW("Pay_E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  แญฎขญฎฅ ฎชญฎ
*
mc = MCOL("Pay_M")
mr = MROW("Pay_M")
IF mc >= 0 .AND. mr >= 0
  IF mr = 1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WROWS("Pay_M")-2
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                   “แโ ญฎขช  ไจซ์โเ  จ เ แ็ฅโ แใฌฌ๋.                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 26.01.97 11:20:17 อผ
PROCEDURE S_Fltr

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

PRIVATE ex, d1_w, d2_w, s, ln, ln1, l_ex, wk_list
                     &&   ก๊๏ขซ๏ฅฌ จ ง ฏฎซญ๏ฅฌ ฏฎซ๏ กซ ญช 
DIMENSION wk_list(p_numb)
FOR ex = 1 TO p_numb
  wk_list(ex) = CHR(ASC("A")+ex-1) $ p_line
ENDFOR

ex   = 1             &&

*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
*
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤ ญโฅเข ซ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ฺฤ ซฎ้ คชจ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ                           ณ ณ                              ณ ณ
*ณ ณ  ็ ซ์ญ ๏ ค โ  .. ณ ณ ( ) ....:....!....:....!.... ณ ณ
*ณ ณ                           ณ ณ                              ณ ณ
*ณ ณ ฎญฅ็ญ ๏ ค โ   .. ณ ณ                              ณ ณ
*ณ ณ                           ณ ณ                              ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ               < OK Ctrl-W > < โช ง โ์แ๏ Esc >                 ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

ln1  = p_numb+4
ln   = ln1+4
l_ex = 6+p_numb

DO D_Win_N WITH ln, 68, "”จซ์โเ"

DO Sun_Bord WITH 2, 3, 8, 31
@  2, 5 SAY " ญโฅเข ซ "

DO Sun_Bord WITH 2, 32, 2+ln1-1, 64
@  2, 34 SAY " ซฎ้ คชจ "

d1_w = d1
d2_w = d2
*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@ 4,  5 SAY " ็ ซ์ญ ๏ ค โ " GET d1_w PICTURE "@D"
@ 6,  5 SAY "ฎญฅ็ญ ๏ ค โ  " GET d2_w PICTURE "@D"
DO G_Place WITH 4, 35, wk_list

@ l_ex, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE VALID Tst_List()

IF ex = 1
  d1 = d1_w
  d2 = d2_w
  SUM FOR DOC_TYPE = "1" .AND. LEFT(DOC_NUM,1) $ p_line .AND. ;
       BETWEEN(ENTER_DAT, d1, d2) MONEY_R TO s
  @ 2, 2 CLEAR TO WROWS()-2, WCOLS()-3
  @ 5, 2 SAY PADC("‘ใฌฌ  "+ALLTRIM(STR(s,15,2)), WCOLS()-4)
  @ 6, 2 SAY PADC(" ฆฌจโฅ ็โฎ-ญจกใค์...", WCOLS()-4)
  = INKEY(0,"MH")
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                 เฎขฅเช  ชฎเเฅชโญฎแโจ แฏจแช  ฏซฎ้ คฎช.                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 02.04.97 11:07:02 อผ
PROCEDURE Tst_List

PRIVATE l_tmp, i
IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

l_tmp = ""
FOR i = 1 TO p_numb
  IF wk_list(i)
    l_tmp = l_tmp+CHR(ASC("A")+i-1)
  ENDIF
ENDFOR

IF EMPTY(l_tmp)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"…แซจ  แ ญฅ จญโฅเฅแใฅโ ญจ ฎคญ  ฏซฎ้ คช ?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

p_line = l_tmp

RETURN .T.
