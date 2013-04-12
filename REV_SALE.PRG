*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ           เฎฃเ ฌฌ  ขฎงขเ โ  ฏเฎค ญญ๋ๅ ฏฎงจๆจฉ.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Rev_Sale
PARAMETER new_mode

PRIVATE seek_type, rc_no, tmpNalog
PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2, ch_today
shab1 = "abcdefghijklmnopqrstuvwxyz กขฃคฅ๑ฆงจฉชซฌญฎฏเแโใไๅๆ็่้๊๋์ํ๎๏"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ€…๐‘’“”•–—"
f_c000  = def_firm
f_c_old = def_firm
f_n000  = ""
ch_today = .F.	&& ฎ ็ฅชใ จ แฅฃฎคญ๏!

tmpNalog = 0

IF .NOT. EMPTY(MENU())
  HIDE MENU (MENU())
ENDIF
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13

seek_type = Chs_Type()

DO CASE
CASE seek_type = 1  && ้ฅฌ ฏฎ ญฎฌฅเใ ญ ชซ คญฎฉ
  rc_no = Get_NN()
CASE seek_type = 2  && ้ฅฌ ฎโ ฏฎงจๆจจ โฎข เ 
  rc_no = Seek_Pos()
CASE seek_type = 3  && ้ฅฌ ฏฎ ชซจฅญโใ โ. ฅ. ฏฎชใฏ โฅซ๎
  rc_no = Get_Cus()
OTHERWISE           && User ฎโช ง ซแ๏
  CLOSE DATABASES
  RETURN
ENDCASE
CLOSE DATABASES
IF rc_no = 0
  RETURN
ENDIF
DO Rev_Lst WITH rc_no
CLOSE DATABASES

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    ๋กฎเ  โจฏ  ฏฎจแช  ญ ชซ คญฎฉ.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Chs_Type
*
*  ฎงขเ ้ ฅฌ:
*          0 - ญจช ช ญฅ จ้ฅฌ, ฎโช ง ซจแ์;
*          1 - ฏฎ ญฎฌฅเใ ญ ชซ คญฎฉ;
*          2 - ฏฎ ฏฎงจๆจจ โฎข เ ;
*          3 - ฏฎ ชซจฅญโใ.
*

*                     ฎจแช ญ ชซ คญฎฉ
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ                                                        ณ
*ณ  ( ) ฎ ญฎฌฅเใ  ( ) ฎ ฏฎงจๆจจ โฎข เ   ( ) ฎ ชซจฅญโใ  ณ
*ณ                                                        ณ
*ณ            < OK Ctrl-W > < โช ง โ์แ๏ Esc >            ณ
*ณ                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
*

PRIVATE ret_val, ex, ln, wd

ln = 8
wd = 60
ex = 1
ret_val = 1

DO D_W WITH "Chs_Type", ln, wd, " ช จ้ฅฌ ญ ชซ คญใ๎", 1

@ 3, 4 GET ret_val PICTURE "@*RH ฎ ญฎฌฅเใ ;ฎ ฏฎงจๆจจ โฎข เ  ;ฎ ชซจฅญโใ"
@ 5,14 GET ex      PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
READ CYCLE
RELEASE WINDOW Chs_Type

RETURN IIF(ex = 2, 0, ret_val)

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    ฎจแช ญ ชซ คญฎฉ ฏฎ ฅฅ ญฎฌฅเใ.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_NN
*
*  ฎงขเ ้ ฅฌ ญฎฌฅเ ง ฏจแจ ข SALE_TIT - 0 ญฅใค ็  จซจ ฎโช ง.
*

PRIVATE sel_rem, ye, t_doc, d_n, let_year, ex, mss, ln, wd, ret_v

DEFINE WINDOW H_W_123 FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW   WINDOW H_W_123

sel_rem = SELECT()
SELECT 0
PUSH KEY CLEAR

ret_v = 0   && ฎงขเ ้ ฅฌฎฅ งญ ็ฅญจฅ,
            &&    ฎชฎญ็ โฅซ์ญฎ ใแโ ญ ขซจข ฅโแ๏ ข ฏเฎฃเ ฌฌฅ Test_Form
t_doc = 1   && ้ฅฌ ญ ชซ คญใ๎ ( 2 - แฏจแฎช )
ex    = 1
d_n   = SPACE(11)
ye    = YEAR(DATE())
let_year = .F.

*            ฎชใฌฅญโ
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ                                       ณ
*ณ      ( )  ชซ คญ ๏    ( ) ‘ฏจแฎช      ณ
*ณ                                       ณ
*ณ     ฎค 9999    ฎฌฅเ ....:....!.     ณ
*ณ                                       ณ
*ณ เฅคฏเจ๏โจฅ AAA AAAAAAAAAAAAAAAA (F1) ณ
*ณ                                       ณ
*ณ   < OK Ctrl-W > < โช ง โ์แ๏ Esc >    ณ
*ณ                                       ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
*
ln = 12
wd = 43
ex = 1

DO D_W WITH "Get_NN", ln, wd, "ฎชใฌฅญโ", 1

@ 3, 8 GET t_doc PICTURE "@*RH  ชซ คญ ๏   ;‘ฏจแฎช" VALID Set_Year()
@ 5, 7 SAY "ฎค" GET ye PICTURE "@Z 9999" WHEN Need_Ye() VALID Test_Ye()
@ 5,19 SAY "ฎฌฅเ" GET d_n VALID Test_Num(.T.)
DO Get_Firm WITH 7, 3, enab_firm
@ 9, 5 GET ex    PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
READ CYCLE VALID Test_Form()

RELEASE WINDOW Get_NN
RELEASE WINDOW H_W_123

RETURN ret_v

*********************************************************************
PROCEDURE Set_Year   && “แโ ญฎขช  งญ ็ฅญจ๏ คซ๏ ฃฎค  ฏฎ โจฏใ คฎชใฌฅญโ 

IF t_doc = 1 .AND. ye = 0    &&  ชซ คญ ๏, ญใฆฅญ ฃฎค
  ye = YEAR(DATE())
  SHOW GET ye
  RETURN .T.
ENDIF

IF t_doc = 2 .AND. ye # 0    && ‘ฏจแฎช - ฃฎค ญฅ ญใฆฅญ
  ye = 0
  SHOW GET ye
  RETURN .T.
ENDIF

*********************************************************************
PROCEDURE Need_Ye    && € ญ คฎ ซจ ขขฎคจโ์ ฃฎค?

IF t_doc = 2
  RETURN .F.
ENDIF

IF .NOT. let_year
  let_year = .T.
  RETURN .F.
ENDIF
RETURN .T.

*********************************************************************
PROCEDURE Test_Ye   &&  ช ขขฅคฅญ ฃฎค? เ ขจซ์ญฎ ซจ?
IF READKEY() % 256 = 12 .OR. ex = 2 .OR. t_doc = 2
  RETURN .T.
ENDIF
DO CASE
CASE ye < 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+'ฎชใฌฅญโฎข แฎ ขเฅฌฅญ "คฎ ญ. ํ." ญฅ แฎๅเ ญจซฎแ์!'
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
CASE ye < 30
  ye = 2000+ye
CASE ye < 100
  ye = 1900+ye
CASE ye < 1900
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+STR(ye,4)+' ฃฎค?! โฎ, ฏเฎแโจโฅ, คฎ จซจ ฏฎแซฅ ฎฆคฅญจ๏ •เจแโ ?'
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDCASE
SHOW GET ye
IF ye < YEAR(DATE())-20
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+STR(ye,4)+' ฃฎค?! ใ ํโฎ ชฎฃค  ฅ้ฅ ก๋ซฎ!'
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF  
IF ye > YEAR(DATE())
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+STR(ye,4)+' ฃฎค?! ใ ํโฎ ชฎฃค  ฅ้ฅ กใคฅโ!'
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF  

RETURN .T.

*********************************************************************
PROCEDURE Test_Num   &&  ช ญฎฌฅเ คฎชใฌฅญโ ? เ ขจซ์ญฎ ซจ?
PARAMETERS fld

PRIVATE v, res

IF READKEY() % 256 = 12 .OR. ex = 2 && User ฎโฎ ขแฅฃฎ ฎโช ง ซแ๏!
  RETURN .T.
ENDIF

IF fld .AND. EMPTY(d_n) && …้ฅ ญฅ ข๋ๅฎค - ฌฎฆญฎ ฎแโ ขจโ์ ฏใแโ๋ฌ
  RETURN .T.
ENDIF

res = .T.

IF t_doc = 1  &&  ชซ คญ ๏
  IF SUBSTR(d_n,5,1) # "-"
    res = .F.
  ENDIF
  IF LEFT(d_n,1) # sys_char
    res = .F.
  ENDIF
  v = SUBSTR(d_n,2,3)
  IF .NOT. v $ RIGHT(STR(1000+VAL(v),4),3)+"***"
    res = .F.
  ENDIF
  v = ALLTRIM(SUBSTR(d_n,6))
  IF ALLTRIM(STR(VAL(v))) # v
    res = .F.
  ENDIF
  IF .NOT. res
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"’ ชจๅ ญฎฌฅเฎข ญ ชซ คญ๋ๅ ญฅ ก๋ข ฅโ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
ENDIF

IF t_doc = 2  && ‘ฏจแฎช
  v = ALLTRIM(d_n)
  IF LEN(v) > 6
    res = .F.
  ELSE
    IF ALLTRIM(STR(VAL(v))) # v
      res = .F.
    ENDIF
    IF VAL(v) <= 0
      res = .F.
    ENDIF
  ENDIF
  IF .NOT. res
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"’ ชจๅ ญฎฌฅเฎข แฏจแชฎข ญฅ ก๋ข ฅโ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
ENDIF

RETURN res

*********************************************************************
PROCEDURE Test_Form   && ้ฅฌ คฎชใฌฅญโ...
PRIVATE sal_n, sal_d

IF READKEY() % 256 = 12 .OR. ex = 2 && User ฎโฎ ขแฅฃฎ ฎโช ง ซแ๏!
  RETURN .T.
ENDIF

IF .NOT. Test_Ye()
  RETURN .F.
ENDIF

IF .NOT. Test_Num(.T.)
  RETURN .F.
ENDIF

IF t_doc = 2    && ้ฅฌ แญ ็ ซ  แฏจแฎช
  USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LST_000 AGAIN
  IF .NOT. SEEK(VAL(ALLTRIM(d_n)))
    USE
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"ฅโ แฏจแช  แ ญฎฌฅเฎฌ "+ALLTRIM(d_n)+"!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  IF EMPTY(SAL_NUM)
    USE
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"‘ฏจแฎช "+ALLTRIM(d_n)+" ฅ้ฅ ญฅ ฏเฎค ญ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  f_c000 = FIRM
  sal_n = SAL_NUM
  sal_d = YEAR(SAL_DATE)
  USE
ELSE
  sal_n = LEFT(d_n,4)+PADL(ALLTRIM(SUBSTR(d_n,6)),6)
  sal_d = ye
ENDIF

USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS SAL_000 AGAIN
IF .NOT. SEEK(f_c000+STR(sal_d,4)+sal_n)
  USE
  sal_n = LEFT(sal_n,4)+"-"+ALLTRIM(SUBSTR(sal_n,5))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ชซ คญ ๏ "+sal_n+" ญฅ ญ ฉคฅญ !"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
IF TOTAL_R < 0
  USE
  sal_n = LEFT(sal_n,4)+"-"+ALLTRIM(SUBSTR(sal_n,5))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ชซ คญ ๏ "+sal_n+" แข๏ง ญ  แ ขฎงขเ โฎฌ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
*---------------------------------------------------------------
IF CUS_CODE< 0
  USE
  sal_n = LEFT(sal_n,4)+"-"+ALLTRIM(SUBSTR(sal_n,5))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ชซ คญ ๏ "+sal_n+" แข๏ง ญ  แฎ แฏฅๆชซจฅญโฎฌ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
*--------------------------------------------------------------
ret_v = RECNO()
USE

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    ฎจแช ญ ชซ คญฎฉ ฏฎ ชซจฅญโใ.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Cus

PRIVATE acc_use, dummy_use, link_use, sel_sav, c_c, c_n, ex, mss

sel_sav = SELECT()

*
*   โชเ๋ข ฅฌ ( ฅแซจ โเฅกใฅโแ๏ ) ไ ฉซ๋ LINK, DUMMY จ ชซจฅญโฎข.
*
IF USED("LINK")
  link_use = .T.
ELSE
  link_use = .F.
  DO Use_Link
ENDIF

IF USED("DUMMY")
  dummy_use = .T.
ELSE
  dummy_use = .F.
  DO Use_Dummy
ENDIF

IF USED("ACCOUNT")
  acc_use = .T.
  SELECT ACCOUNT
ELSE
  acc_use = .F.
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME AGAIN
  
ENDIF

ex = Custs("", .F., .F.)  && ๋กจเ ฅฌ ชซจฅญโ 
IF ex           && ๋กเ ซจ ชซจฅญโ !
  c_c = CUS_CODE
  c_n = CUS_NAME
ELSE            && User ฎโช ง ซแ๏!
  c_c = 0
  c_n = ""
ENDIF

*
*     ชเ๋ข ฅฌ ( ฅแซจ ฏเจ ขๅฎคฅ ญฅ ก๋ซจ ฎโชเ๋โ๋ ) ไ ฉซ๋ ชซจฅญโฎข จ DUMMY
*
IF .NOT. acc_use
  USE
ENDIF

IF .NOT. dummy_use
  SELECT DUMMY
  USE
ENDIF

IF c_c = 0  && …แซจ User ฎโช ง ซแ๏, โฎ ง ชเ๋ข ฅฌ จ ไ ฉซ LINK จ ข๋ช โ๋ข ฅฌแ๏.
  IF .NOT. link_use
    SELECT LINK
    USE
  ENDIF
  SELECT (sel_sav)
  RETURN 0
ENDIF
SELECT LINK
ZAP

*
*   โชเ๋ข ฅฌ ไ ฉซ ง ฃฎซฎขชฎข ญ ชซ คญ๋ๅ จ ไฎเฌจเใฅฌ ไ ฉซ LINK.
*
SELECT 0
USE SALE_TIT ORDER TAG CUS_CODE ALIAS ST000 AGAIN
SEEK c_c
SCAN REST WHILE CUS_CODE = c_c FOR TOTAL_R > 0  && ”ฎเฌจเใฅฌ แฏจแฎช ญ ชซ คญ๋ๅ
  SELECT LINK
  APPEND BLANK
  REPLACE LNK WITH RECNO("ST000")
  SELECT ST000
ENDSCAN
USE
IF RECCOUNT("LINK") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"โฎฌใ ชซจฅญโใ ญจ็ฅฃฎ ญฅ ฏเฎค ข ซจ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  IF .NOT. link_use
    SELECT LINK
    USE
  ENDIF
  RETURN 0  
ENDIF

ex = Chs_Nkl("", ALLTRIM(c_n))

IF .NOT. link_use
  SELECT LINK
  USE
ENDIF

SELECT (sel_sav)
RETURN ex

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                          ๋กฎเ ญ ชซ คญฎฉ.                             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Chs_Nkl
PARAMETERS ext_menu, tit_m
*
*  ext_menu - จฌ๏ ขญฅ่ญฅฃฎ ฌฅญ๎;
*  tit_m    - ง ฃฎซฎขฎช แฏจแช  ญ ชซ คญ๋ๅ.
*
*   …—€…: ฏเจ ขๅฎคฅ คฎซฆฅฌ ก๋โ์ ฎโชเ๋โ จ ง ฏฎซญฅญ ไ ฉซ LINK!
*

PRIVATE ln1, wd1, ln2, wd2, dummy_use, sel_sav, ex_v
PRIVATE fl01, fl11, fl12, fl13, fl14, fl21, fl22, fl23, fl24, tt, ft

sel_sav = SELECT()

DO Entry_P WITH "", ""
*
*   โชเ ข ฅฌ ไ ฉซ๋ DUMMY, STOCK (STO00), SALE (SALE00), 
*      ACCOUNT (USERS0), SALE_TIT (SALE01)
*
IF USED("DUMMY")
  dummy_use = .T.
ELSE
  dummy_use = .F.
  DO Use_Dummy
ENDIF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO00 AGAIN

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS SALE00 AGAIN
SET RELATION TO CODE INTO STO00

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS USERS0 AGAIN

SELECT 0
USE (base_path+"SALE_TIT") ALIAS SALE01 AGAIN
SET RELATION TO CUS_CODE INTO USERS0 
SELECT LINK
SET RELATION TO LNK INTO SALE01

*
*   ฅญ๎ คซ๏ ฏฅเฅ็ญ๏ ญ ชซ คญ๋ๅ
*
DEFINE MENU Inv_Lst IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF Inv_Lst PROMPT "๋กเ โ์"    KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF Inv_Lst DO Ch_Mode WITH "Inv_Lst", "choose"

DEFINE PAD View OF Inv_Lst PROMPT "เฎแฌฎโเฅโ์"  KEY Ctrl-A, "Tab"
ON SELECTION PAD View OF Inv_Lst DO Ch_Mode WITH "Inv_Lst", "invoice"

DEFINE PAD Cource OF Inv_Lst PROMPT "ใเแ"  KEY Ctrl-A, "F4"
ON SELECTION PAD Cource OF Inv_Lst DO Ch_Mode WITH "Inv_Lst", "cource"

DEFINE PAD Abort  OF Inv_Lst PROMPT "โช ง โ์แ๏" KEY Ctrl-A, "Esc"
ON SELECTION PAD Abort  OF Inv_Lst DO Ch_Mode WITH "Inv_Lst", "exit"
PRIVATE menu_name
menu_name = "Inv_Lst"

*
*   ฅญ๎ คซ๏ แฎคฅเฆจฌฎฃฎ ญ ชซ คญฎฉ
*
DEFINE MENU View_Inv IN SCREEN COLOR SCHEME 4

DEFINE PAD Return OF View_Inv PROMPT "ฎงขเ โ" KEY Ctrl-A, "Esc"
ON SELECTION PAD Return OF View_Inv DO Ch_Mode WITH "View_Inv", "lst"

fl01 = FSIZE("FIRM", "SALE01")+1         && ฎค ฏเฅคฏเจ๏โจ๏
fl11 = FSIZE("DOC_NUM", "SALE01")+1      && ฎฌฅเ คฎชใฌฅญโ  (+1 ฏฎค โจเฅ)
fl12 = IIF(SET("CENTURY")="ON", 10, 8)   &&  โ 
*fl13 = FSIZE("CUS_NAME", "SALE01")      &&  งข ญจฅ ญ ชซ คญฎฉ
fl14 = FSIZE("TOTAL_R", "SALE01")        && ‘ใฌฌ  ฏฎ ญ ชซ คญฎฉ

fl21 = FSIZE("PREFIX", "STO00")+ ;
       FSIZE("NAME", "STO00")+   ;
       FSIZE("PRODUCER", "STO00")+2      &&  จฌฅญฎข ญจฅ
fl22 = FSIZE("QNT", "SALE00")            && ฎซจ็ฅแโขฎ
fl23 = FSIZE("PRICE_R", "SALE00")        && –ฅญ 
fl24 = FSIZE("TOTAL_R", "SALE01")        && ‘ใฌฌ 

wd1 = fl01+fl11+fl12+fl14+2
wd2 = fl21+fl22+fl23+3
ln1 = WROWS("")-11
ln2 = WROWS("")-9

DIMENSION tt(1), ft(1)
tt(1) = PADC(" จฌฅญฎข ญจฅฤฤฤฤฤฤฤฤฤฤฤ", fl21+1,"ฤ")+ ;
        PADC("ฎซ-ขฎฤ", fl22+1,"ฤ")+                 ;
        PADC("ฤฤ–ฅญ ",  fl23,"ฤ")
*        PADC("ฤฤฤ‘ใฌฌ ",fl24,"ฤ")
ft(1) = ""

DO Def_Win WITH "W_N_M", "W_N_E", "W_N_I", ln2, wd2, "", tt, ft
HIDE WINDOW W_N_M
HIDE WINDOW W_N_E
HIDE WINDOW W_N_I

DIMENSION tt(1), ft(2)
tt(1) = PADC("”เฌ",  fl01+1,"ฤ")+     ;
        PADC("N คฎช-โ ", fl11+1,"ฤ")+ ;
        PADC(" โ ", fl12+1,"ฤ")+     ;
        PADC("ฤฤฤ‘ใฌฌ ",fl14,"ฤ")

*        PADC(" งข ญจฅฤฤฤฤฤ", fl13+1,"ฤ")+   ;
*

ft(1) = ""
ft(2) = ""
DO Def_Win WITH "W_T_M", "W_T_E", "W_T_I", ln1, wd1, ALLTRIM(tit_m), tt, ft

PRIVATE what_do

what_do = "lst"
ex_v = 0
DO WHILE .T.
  DO CASE
  CASE what_do = "lst"      && ฅเฅ็ฅญ์ ญ ชซ คญ๋ๅ
    ON KEY LABEL Tab DO Ch_Mode WITH "Inv_Lst","invoice"
    ON KEY LABEL Enter DO Ch_Mode WITH "Inv_Lst","choose"
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL F4   DO Cource WITH SALE01.FIRM, SALE01.DOC_NUM, SALE01.DOC_DATE
    ON KEY LABEL F10 ACTIVATE MENU Inv_Lst
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "Inv_Lst", "W_T_"
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU Inv_Lst
    SELECT LINK
    HIDE WINDOW W_N_M
    HIDE WINDOW W_N_E
    SHOW WINDOW W_T_M SAME
    ACTIVATE WINDOW W_T_E SAME
    BROWSE FIELDS   ;
        SALE01.FIRM:H="",      ;
        f001 = LEFT(SALE01.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(SALE01.DOC_NUM,5)):11:H="",  ;
        SALE01.DOC_DATE:H="",  ;
        SALE01.TOTAL_R:H="",   ;
        DUMMY.F:H=""    ;
        FREEZE DUMMY.F  ;
        WHEN Show_C()   ;
        NOMENU NORGRID NOMODIFY NOCLEAR WINDOW W_T_I IN WINDOW W_T_E

*        SALE01.CUS_NAME:H="",  ;
*

    ON KEY
    IF what_do = "lst"
      what_do = "exit"
    ENDIF
  CASE what_do = "invoice"      &&  ชซ คญ ๏
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "View_Inv", "W_N_"
    ON KEY LABEL F10 ACTIVATE MENU View_Inv
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU View_Inv
    SELECT SALE00
    SEEK SALE01.FIRM+STR(YEAR(SALE01.DOC_DATE),4)+SALE01.DOC_NUM
    HIDE WINDOW W_T_M
    HIDE WINDOW W_T_E
    SHOW WINDOW W_N_M SAME 
    ACTIVATE WINDOW W_N_M SAME
    @ 0, 0 SAY PADC( SALE01.FIRM+"/"+ ;
        LEFT(SALE01.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(SALE01.DOC_NUM,5))+" ฎโ "+  ;
        TRANSFORM(SALE01.DOC_DATE,"@D")+" "+  ;
        ALLTRIM(SALE01.CUS_NAME),WCOLS()    ) COLOR SCHEME 5
    ACTIVATE WINDOW W_N_E
    BROWSE FIELDS   ;
        STO00.PREFIX:H="",  ;
        STO00.NAME:H="",  ;
        STO00.PRODUCER:H="",  ;
        SALE00.QNT:H="",  ;
        SALE00.PRICE_R:H="",   ;
        DUMMY.F:H=""    ;
        FREEZE DUMMY.F  ;
        KEY SALE01.FIRM+STR(YEAR(SALE01.DOC_DATE),4)+SALE01.DOC_NUM ;
        NOMENU NORGRID NOMODIFY NOCLEAR WINDOW W_N_I IN WINDOW W_N_E
    what_do = "lst"
    ON KEY
  CASE what_do = "choose"      && User ข๋กเ ซ
    ex_v = RECNO("SALE01")
    what_do = "OUT!"
  CASE what_do = "cource"      && ใเแ
    DO Cource WITH SALE01.FIRM, SALE01.DOC_NUM, SALE01.DOC_DATE
    what_do = "lst"
  CASE what_do = "exit"        && User ฎโช ง ซแ๏
    ex_v = 0
    what_do = "OUT!"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO
RELEASE WINDOW W_T_M
RELEASE WINDOW W_T_E
RELEASE WINDOW W_T_I
RELEASE WINDOW W_N_M
RELEASE WINDOW W_N_E
RELEASE WINDOW W_N_I
RELEASE MENU View_Inv
RELEASE MENU Inv_Lst

*
*    ชเ ข ฅฌ ไ ฉซ๋ DUMMY, STOCK (STO00), SALE (SALE00), 
*      ACCOUNT (USERS0), SALE_TIT (SALE01)
*
IF .NOT. dummy_use
  SELECT DUMMY
  USE
ENDIF
SELECT STO00
USE
SELECT SALE00
USE
SELECT USERS0
USE
SELECT SALE01
USE

DO Exit_P WITH "", ""

SELECT (sel_sav)

RETURN ex_v

**********************************************************************
PROCEDURE Show_C
PRIVATE w

ACTIVATE WINDOW W_T_M SAME
*IF .NOT. FOUND("USERS0")
*  w = ""
*ELSE
*  w = ALLTRIM(USERS0.CUS_NAME)
*ENDIF
w = SALE01.CUS_NAME
@ WROWS()-1, 0 SAY PADC(w,WCOLS())
ACTIVATE WINDOW (ALIAS())
RETURN .T.

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
*บ                    ฎจแช ญ ชซ คญฎฉ ฏฎ โฎข เใ.                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Seek_Pos

PRIVATE sel_sav, dummy_use, link_use, rc_ret
sel_sav = SELECT()
*
*   โชเ ข ฅฌ ไ ฉซ๋ DUMMY, LINK STOCK (STO01)
*
IF USED("DUMMY")
  dummy_use = .T.
ELSE
  dummy_use = .F.
  DO Use_Dummy
ENDIF

IF USED("LINK")
  link_use = .T.
ELSE
  link_use = .F.
  DO Use_Link
ENDIF

*
*   ฅญ๎ คซ๏ ญฎฌฅญชซ โใเ๋
*
DEFINE MENU Sto IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF Sto PROMPT "๋กเ โ์"  KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF Sto DO Ch_Mode WITH "Sto", "choose"

DEFINE PAD Seek   OF Sto PROMPT "ฎจแช ฏฎ ชฎญโฅชแโใ"  KEY Ctrl-A, "F7"
ON SELECTION PAD Seek   OF Sto DO Ch_Mode WITH "Sto", "seek"

DEFINE PAD Abort  OF Sto PROMPT "โช ง โ์แ๏" KEY Ctrl-A, "Esc"
ON SELECTION PAD Abort  OF Sto DO Ch_Mode WITH "Sto", "exit"

SELECT 0
USE STOCK ORDER TAG NAME ALIAS STO01 AGAIN

PRIVATE what_do, ln, wd, tt, ft
ln = WROWS("")-7
*wd = FSIZE("PREFIX","STO01")+FSIZE("NAME","STO01")+FSIZE("PRODUCER","STO01")+2
wd = FSIZE("PREFIX","STO01")+50+FSIZE("PRODUCER","STO01")+2
DO Def_Win WITH "Sto_M", "Sto_E", "Sto_I", ln, wd, "ฎฌฅญชซ โใเ "

what_do = "stock"

PRIVATE prf_flt, stock_f, prf_show
PRIVATE prf_wd, fill_chr, double, rc_flt, jmp, r_must
prf_wd = 50
seek_m = "seek"
jmp    = .F.
r_must = 0
fill_chr = "ฑ"

SET CONFIRM OFF

rc_ret = 0
DO WHILE .T.
  DO CASE
  CASE what_do = "stock"
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU Sto
    seek_m = "exit"
    rc_flt  = 0
    double  = 0
    r_must  = 0
    prf_show = ""
    prf_flt  = ""
    stock_f = .T.
    prf_wd  = 50
    ACTIVATE WINDOW Sto_M
    @ 1, 4 SAY PADR(prf_flt,prf_wd,"ฑ") COLOR SCHEME 13
    prf_show = prf_flt
    ACTIVATE WINDOW Sto_E
    ON READERROR DO Jmp
    ON KEY LABEL Enter DO Ch_Mode WITH "Sto", "choose"
    ON KEY LABEL F7 DO Ch_Mode WITH "Sto", "seek"
    ON KEY LABEL F10 ACTIVATE MENU Sto
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "Sto", "Sto_"
    BROWSE FIELDS STO01.PREFIX:H="", ;
                  STO01.NAME:50:H="",   ;
                  STO01.PRODUCER:H="",   ;
                  DUMMY.F:H="":V=Sk():F   ;
            WHEN New_Rec()  ;
            FREEZE DUMMY.F ;
            NOMENU NORGRID NOAPPEND NOCLEAR WINDOW Sto_I IN WINDOW Sto_E COLOR SCHEME 13
    ON READERROR = 1
    ON KEY LABEL Enter
    ON KEY LABEL F7
    ON KEY LABEL F10
    ON KEY LABEL Home
    ON KEY LABEL End
    ON KEY LABEL LeftMouse
    DEACTIVATE MENU Sto
    ACTIVATE SCREEN
    IF what_do = "stock"
      what_do = "‘…!"
    ENDIF
  CASE what_do = "seek"
    DO Cnt_Seek
    what_do = "stock"
  CASE what_do = "choose"
    rc_ret = Nkl_L(STO01.CODE)
    IF rc_ret = 0
      what_do = "stock"
    ELSE
      what_do = "‘…!"
    ENDIF
  OTHERWISE
    EXIT
  ENDCASE
ENDDO
SELECT STO01
USE
IF .NOT. dummy_use
  SELECT DUMMY
  USE
ENDIF

IF .NOT. link_use
  SELECT LINK
  USE
ENDIF
RELEASE WINDOW Sto_M
RELEASE WINDOW Sto_E
RELEASE WINDOW Sto_I
RELEASE MENU Sto

SELECT (sel_sav)
RETURN rc_ret

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    ๋ๅฎค จง ไจชโจขญฎฃฎ ฏฎซ๏.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sk
PRIVATE rk, lk, rc_old, r_w, prf_rem

SELECT STO01
IF double # 0
  double = 0
  RETURN .T.     && CHRSAW() .OR. r_must = 0
ENDIF

prf_rem = prf_flt
rk = MOD(READKEY(), 256)
IF INLIST(LASTKEY(), 1, 6)
  RETURN .T.
ENDIF

DO CASE
CASE rk = 0    && Back Space ! Left Arrow
  IF LEN(prf_flt) = 0
    RETURN .T.
  ELSE
    prf_flt = LEFT(prf_flt,LEN(prf_flt)-1)
*    double = 1
  ENDIF
CASE rk = 15   && ‘จฌขฎซ
  IF LEN(prf_flt) = prf_wd
    RETURN .T.
  ENDIF
  prf_flt = prf_flt+DUMMY.F
OTHERWISE
  RETURN .T.
ENDCASE

rc_flt = 0
rc_old = RECNO()
r_must = rc_old
stock_f = SEEK(prf_flt)
IF .NOT. stock_f
  prf_flt = prf_rem
ELSE
  r_must = RECNO()
  DO Show_Prf
  IF r_must = rc_old
*    double = 0
  ENDIF
ENDIF

RETURN r_must = rc_old    && .NOT. stock_f

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    ฅเฅฌฅ้ฅญจฅ ฏฎ ก งฅ.                               บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Jmp

jmp = .T.
double = IIF(CHRSAW(), 0, 1)
GO r_must
r_must = 0

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ข๋แขฅโชจ ฏเฅไจชแ .                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Show_Prf

PRIVATE l_s, c_s, wn

ACTIVATE WINDOW Sto_M SAME
@ 1, 4 SAY PADR(prf_flt,prf_wd,"ฑ") COLOR SCHEME 13
prf_show = prf_flt
ACTIVATE WINDOW (ALIAS())
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                            ๅฎค ข ง ฏจแ์.                             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE New_Rec

IF .NOT. prf_flt == "" .AND. .NOT. jmp
  ACTIVATE WINDOW Sto_M BOTTOM
  prf_flt = ""
  @ 1, 4 SAY PADR(prf_flt,prf_wd,"ฑ") COLOR SCHEME 13
  prf_show = prf_flt
  double  = 0
  r_must  = 0
  stock_f = .T.
  ACTIVATE WINDOW (ALIAS())
ENDIF

jmp = .F.
RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ               เฎๆฅคใเ  ชฎญโฅชแโญฎฃฎ ฏฎจแช .                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cnt_Seek

PRIVATE rc_sav, ln, wd, shab, ex, c1, c2, mss

PUSH KEY CLEAR

rc_sav = RECNO()

ln = 5
shab = SPACE(FSIZE("NAME"))
ex = 1
wd = MAX(LEN(shab),32)+8
c1 = FLOOR((wd-LEN(shab))/2)
c2 = FLOOR((wd-32)/2)
DEFINE WINDOW HW001 FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW HW001

DO D_W WITH "Cnt_Seek", ln, wd, "ขฅคจโฅ ่ กซฎญ คซ๏ ฏฎจแช ", 1

@ 2, c1 GET shab
@ 3, c2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
READ CYCLE

RELEASE WINDOW Cnt_Seek
IF ex = 2 .OR. EMPTY(shab)
  POP KEY
  RETURN
ENDIF

DO Wt_Mess WITH "้ฅฌ..."
SELECT LINK
ZAP
SELECT STO01
shab = SYS(15, lwr, ALLTRIM(shab))
SCAN FOR shab $ SYS(15, lwr, NAME)
  SELECT LINK
  APPEND BLANK
  REPLACE LNK WITH RECNO("STO01")
  SELECT STO01
ENDSCAN
DO Wt_Mess

IF RECCOUNT("LINK") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"จ็ฅฃฎ ญฅ ญ ฉคฅญฎ!"
  mss(3) = ""
  POP KEY
  GO rc_sav
  RETURN
ENDIF

ln = WROWS("")-11
wd = FSIZE("PREFIX","STO01")+FSIZE("NAME","STO01")+FSIZE("PRODUCER","STO01")+2
DO Def_Win WITH "Seek_M", "Seek_E", "Seek_I", ln-4, wd, "๋กฅเจโฅ ฏฎงจๆจ๎"
SET ORDER TO 0
SELECT LINK
SET RELATION TO LNK INTO STO01
GO TOP
ACTIVATE WINDOW Seek_E
ON KEY LABEL Enter KEYBOARD CHR(23)
BROWSE FIELDS STO01.PREFIX:H="",  ;
              STO01.NAME:H="",    ;
              STO01.PRODUCER:H="",;
              DUMMY.F:H=""        ;
       FREEZE DUMMY.F ;
       NOMENU NORGRID NOAPPEND NOCLEAR WINDOW Seek_I IN WINDOW Seek_E COLOR SCHEME 13
POP KEY
SET RELATION TO
SELECT STO01
SET ORDER TO TAG NAME
RELEASE WINDOW Seek_M
RELEASE WINDOW Seek_E
RELEASE WINDOW Seek_I
RELEASE WINDOW HW001

IF READKEY() % 256 = 12
  GO rc_sav
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ           เฎๆฅคใเ  ไฎเฌจเฎข ญจ๏ แฏจแช  จ ข๋กฎเ  ญ ชซ คญฎฉ.           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Nkl_L
PARAMETERS code_p

PRIVATE sel_sav, rc_w, tit_w

sel_sav = SELECT()

tit_w = ALLTRIM(STO01.PREFIX+" "+STO01.NAME)

SELECT LINK
ZAP

SELECT 0
USE (BASE_PATH+"SALE_TIT") ORDER TAG DOC_NUM ALIAS SL_T AGAIN

SELECT 0
USE (BASE_PATH+"SALE") ORDER TAG CODE ALIAS SL AGAIN
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO SL_T
SET FILTER TO SL_T.CUS_CODE >= 0
SEEK code_p

SCAN REST WHILE CODE = code_p FOR QNT > 0 .AND. LEFT(DOC_NUM,1) = sys_char
  SELECT LINK
  APPEND BLANK
  REPLACE LNK WITH RECNO("SL_T")
  SELECT SL
ENDSCAN

USE

SELECT SL_T
USE

SELECT LINK
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฎงจๆจ๏ ญฅ ฏเฎค ข ซ แ์."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (sel_sav)
  RETURN 0
ENDIF

rc_w = Chs_Nkl("Sto", tit_w)

SELECT (sel_sav)

RETURN rc_w

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ               เฎๆฅคใเ  ไฎเฌจเฎข ญจ๏ แฏจแช  ขฎงขเ โ .                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Rev_Lst
PARAMETERS rc_tit    && ฎฌฅเ ง ฏจแจ ข ไ ฉซฅ ง ฃฎซฎขชฎข ญ ชซ คญ๋ๅ

PRIVATE ln, wd, tit_m, tit_w, foot_w, doc_n, d_nam, doc_d, rem_val, nds_p ;
        cus_t, cus_c, cus_n, sum_d, sum_r, mss, c_sch, i001, i002, transm
PRIVATE is_u, flg_f
is_u = .F.
flg_f = .T.

IF new_mode
  DO Get_Valuta
ENDIF

IF rc_tit = 0
  RETURN
ENDIF

DELETE FILE (tmpo_path+"REV_TMP.DBF")
DELETE FILE (tmpo_path+"REV_TMP.CDX")
CREATE DBF (tmpo_path+"REV_TMP") ;
     ( CODE       N( 7),   ;
       PARTY      C(12),   ;
       ADDRESS    C( 8),   ;
       PREFIX     C( 4),   ;
       NAME       C(35),   ;
       PRODUCER   C( 8),   ;
       QNT_SAL    N( 6),   ;
       QNT_REP    N( 6),   ;
       QNT_REV    N( 6),   ;
       PRICE_TYPE C( 1),   ;
       ACT_FIRM   C( 3),   ;
       ACT_NUM    C(10),   ;
       ACT_DATE   D,       ;
       GTD_NUM    C(26),   ;
       COUNTRY    C(16),   ;
       COUNTRYC   C( 3),   ;
       GTD_LINE   C( 4),   ;
       PRICE_S    N(12,5), ;
       PRICE_S0   N( 8,2), ;
       PRICE_S_D  N(10,4), ;
       PRICE      N(12,5), ;
       PRICE_0    N( 8,2), ;
       PRICE_D    N(10,4), ;
       R_NO       N(10)  )
USE (tmpo_path+"REV_TMP") EXCLUSIVE
INDEX ON ADDRESS TAG ADDRESS
SET ORDER TO

SELECT 0
USE (base_path+"SALE_TIT") ALIAS SAL_T AGAIN
GO rc_tit
f_c000 = FIRM
doc_n = DOC_NUM
doc_d = DOC_DATE
cus_c = CUS_CODE
nds_p = NDS_
cus_t = NEW_TYPE
*cus_t = "0"
d_nam = CUS_NAME
cus_n = ""
sum_d = 0
sum_r = 0
transm = .NOT. EMPTY(TRANSMIT_C)
ch_today = PAY_TYPE="—" .AND. PAY_DATE = DATE()

IF SUBSTR(doc_n,2,3) = "***" .AND. doc_d = DATE()
  flg_f = .F.
ENDIF 

tit_m = f_c000+"/"+LEFT(doc_n,4)+"-"+ALLTRIM(SUBSTR(doc_n,5))+" ฎโ "+   ;
        TRANSFORM(doc_d,"@D")+" "+  ;
        ALLTRIM(CUS_NAME)
IF .NOT. EMPTY(cus_c)
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS AGAIN
  IF SEEK(cus_c)
    cus_n = ALLTRIM(CUS_NAME)
  ENDIF
ENDIF

USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_REV AGAIN
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS SAL_REV AGAIN
SET RELATION TO CODE INTO STO_REV
SEEK f_c000+LEFT(DTOS(doc_d),4)+doc_n

SCAN REST WHILE DOC_NUM = doc_n .AND. DOC_DATE = doc_d .AND. FIRM = f_c000
  SELECT REV_TMP
  APPEND BLANK
  REPLACE CODE       WITH SAL_REV.CODE,    ;
          ADDRESS    WITH STO_REV.ADDRESS, ;
          PREFIX     WITH STO_REV.PREFIX,  ;
          NAME       WITH STO_REV.NAME,    ;
          PRODUCER   WITH STO_REV.PRODUCER,;
          QNT_SAL    WITH SAL_REV.QNT,     ;
          QNT_REP    WITH SAL_REV.QNT_REP, ;
          PRICE_TYPE WITH SAL_REV.PRICE_TYPE,;
          ACT_NUM    WITH SAL_REV.ACT_NUM, ;
          ACT_DATE   WITH SAL_REV.ACT_DATE,;
          ACT_FIRM   WITH SAL_REV.ACT_FIRM,;
          PARTY      WITH SAL_REV.PARTY,   ;
          GTD_NUM    WITH SAL_REV.GTD_NUM, ;
          COUNTRY    WITH SAL_REV.COUNTRY, ;
          COUNTRYC   WITH SAL_REV.COUNTRYC,;
          GTD_LINE   WITH SAL_REV.GTD_LINE,;
          PRICE_S    WITH SAL_REV.PRICE_R, ;
          PRICE_S0   WITH SAL_REV.PRICE_0, ;
          PRICE_S_D  WITH SAL_REV.PRICE_D, ;
          PRICE      WITH SAL_REV.PRICE_R, ;
          PRICE_0    WITH SAL_REV.PRICE_0, ;
          PRICE_D    WITH SAL_REV.PRICE_D, ;
          R_NO       WITH RECNO("SAL_REV")
  SELECT SAL_REV
  sum_d = sum_d+IIF(is_u, QNT*PRICE_D, ROUND(QNT*PRICE_R,2))
ENDSCAN
USE

SELECT STO_REV
USE

SELECT REV_TMP
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ชซ คญ ๏ ฏใแโ ..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"REV_TMP.DBF")
  DELETE FILE (tmpo_path+"REV_TMP.DCX")
  RETURN
ENDIF
PRIVATE p1, p2, p3, p4, p5, p6, p_sum
p1 = FSIZE("PREFIX")
p2 = 25 && FSIZE("NAME")
p3 = FSIZE("QNT_SAL")
p4 = FSIZE("QNT_REV")
p5 = IIF(is_u, FSIZE("PRICE_S_D"), FSIZE("PRICE_S"))
p6 = p5+2

wd = p1+p2+p3+p4+p5+p6+5
ln = WROWS("")-8
DIMENSION tit_w(2), foot_w(2)
tit_w(1) = ""
tit_w(2) = ;
 PADC(" จฌฅญฎข ญจฅฤฤฤฤฤฤฤฤ", p1+p2+2,"ฤ")+   ;
      PADC("เฎค.", p3+1,"ฤ")+PADC("ฎงขเ.", p4+1,"ฤ")+ ;
      PADC("ฤฤฤ–ฅญ ", p5+1,"ฤ")+PADC("ฤฤฤ‘ใฌฌ ", p6,"ฤ")
foot_w(1) = ""
foot_w(2) = PADR("  "+cus_n,wd)

DO Def_Win WITH "Rev_M", "Rev_E", "Rev_I", ln, wd, tit_m, tit_w, foot_w
p_sum = WCOLS()-12
@ 1, 2 SAY "‘ใฌฌ  ฏฎ ญ ชซ คญฎฉ "+STR(sum_d,10,IIF(is_u,4, 2))
@ 1, p_sum-16 SAY "‘ใฌฌ  ขฎงขเ โ  "

c_sch = SCHEME(13)
i001 = AT(",", c_sch, 6)+1
i002 = AT(",", c_sch, 7)-i001
c_sch = STUFF(c_sch, i001, i002, SCHEME(14,2))

rem_val = 0

PRIVATE menu_name
menu_name = "Rev_Lst"

DEFINE MENU Rev_Lst IN SCREEN COLOR SCHEME 4

DEFINE PAD Rev_All OF Rev_Lst PROMPT "ฅเญใโ์ ขแฅ "    KEY Ctrl-A, "F5"
ON SELECTION PAD Rev_All OF Rev_Lst DO Rev_All

IF new_mode
  DEFINE PAD RePrice OF Rev_Lst PROMPT "ฅเฅฎๆฅญจโ์"    KEY Ctrl-A, "F2"
  ON SELECTION PAD RePrice OF Rev_Lst DO RePrice
ENDIF

DEFINE PAD Cource OF Rev_Lst PROMPT "ใเแ"    KEY Ctrl-A, "F4"
ON SELECTION PAD Cource OF Rev_Lst DO Cource WITH f_c000, doc_n, doc_d

DEFINE PAD Exit  OF Rev_Lst PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit  OF Rev_Lst DO Exit_Rev
ACTIVATE SCREEN
@ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
SHOW MENU Rev_Lst

ON KEY LABEL F5 DO Rev_All
ON KEY LABEL F10 ACTIVATE MENU Rev_Lst
ON KEY LABEL F4 DO Cource WITH f_c000, doc_n, doc_d
IF new_mode
  ON KEY LABEL F2 DO Reprice
ENDIF
ON KEY LABEL LeftMouse DO M_Proc_1 WITH "Rev_Lst", "Rev_"

ACTIVATE WINDOW Rev_E

IF is_u
  BROWSE FIELDS PREFIX:H="":W=.F.,  ;
                Brw_Name=Nm_Part(NAME):25:H="":W=.F.,    ;
                QNT_SAL:H="":W=.F.,   ;
                QNT_REV:H="":W=Sav_V():V=Tst_V():P="@Z 99999",           ;
                PRICE_S_D:H="":W=new_mode.AND.Sav_V_P():V=Tst_V_P(),      ;
                F_WK_REV = QNT_REV*PRICE_S_D:P="@Z 99999.9999":H="":W=.F. ;
         WHEN Show_F()  ;
         NOMENU NORGRID NOAPPEND NODELETE NOCLEAR ;
         WINDOW Rev_I IN WINDOW Rev_E COLOR (c_sch)
ELSE
  BROWSE FIELDS PREFIX:H="":W=.F., ;
                Brw_Name=Nm_Part(NAME):25:H="":W=.F.,   ;
                QNT_SAL:H="":W=.F.,   ;
                QNT_REV:H="":W=Sav_V():V=Tst_V():P="@Z 99999",           ;
                PRICE_S:H="":W=new_mode.AND.Sav_V_P():V=Tst_V_P(),        ;
                F_WK_REV = QNT_REV*PRICE_S:P="@Z 99999999.99999":H="":W=.F. ;
         WHEN Show_F()  ;
         NOMENU NORGRID NOAPPEND NODELETE NOCLEAR ;
         WINDOW Rev_I IN WINDOW Rev_E COLOR (c_sch)
ENDIF

ON KEY
DEACTIVATE MENU Rev_Lst 
RELEASE MENU Rev_Lst 
RELEASE WINDOW Rev_M
RELEASE WINDOW Rev_E
RELEASE WINDOW Rev_I
CALCULATE SUM(QNT_REV) TO rem_val

CLOSE DATABASES
IF rem_val # 0
  DO Exp_Rev
ENDIF

DELETE FILE (tmpo_path+"REV_TMP.DBF")
DELETE FILE (tmpo_path+"REV_TMP.CDX")

RETURN

**************************************************************************
PROCEDURE Sav_V_P   && ‘ฎๅเ ญฅญจฅ ญ ็ ซ์ญฎฃฎ งญ ็ฅญจ๏
rem_val = IIF(is_u, PRICE_S_D, PRICE_S)
RETURN .T.

**************************************************************************
PROCEDURE Sav_V   && ‘ฎๅเ ญฅญจฅ ญ ็ ซ์ญฎฃฎ งญ ็ฅญจ๏
rem_val = QNT_REV
RETURN .T.

**************************************************************************
PROCEDURE Tst_V   && กเ กฎโช  ขขฅคฅญญฎฃฎ งญ ็ฅญจ๏

IF QNT_REV > QNT_SAL
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"๋ ฏ๋โ ฅโฅแ์ ขฅเญใโ์ กฎซ์่ฅ, ็ฅฌ ก๋ซฎ ฏเฎค ญฎ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  ACTIVATE WINDOW (ALIAS())
  RETURN .F.
ENDIF

IF QNT_REV < 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅ็์ จคฅโ ฎ ขฎงขเ โฅ,   ญฅ ฎ ฏเฎค ฆฅ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  ACTIVATE WINDOW (ALIAS())
  RETURN .F.
ENDIF

IF QNT_REV # rem_val
  IF is_u
    sum_r = sum_r+QNT_REV*PRICE_S_D-rem_val*PRICE_S_D
  ELSE
    sum_r = sum_r+ROUND(QNT_REV*PRICE_S,2)-ROUND(rem_val*PRICE_S,2)
  ENDIF
  ACTIVATE WINDOW Rev_M
  @ 1, p_sum SAY sum_r PICTURE IIF(is_u, "@Z 999999.9999","@Z 9999999.99")
  ACTIVATE WINDOW (ALIAS())
ENDIF
RETURN .T.

**************************************************************************
PROCEDURE Tst_V_P   && กเ กฎโช  ขขฅคฅญญฎฃฎ งญ ็ฅญจ๏
PRIVATE tmpVal

IF is_u
  IF PRICE_S_D <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"–ฅญ  ‘…€ กฎซ์่ฅ ญใซ๏!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    ACTIVATE WINDOW (ALIAS())
    RETURN .F.
  ENDIF

  IF PRICE_S_D # rem_val
    sum_r = sum_r+QNT_REV*PRICE_S_D-rem_val*QNT_REV
    REPLACE PRICE_S  WITH Round_6(PRICE_S_D*kurs)
    REPLACE PRICE_S0 WITH (PRICE_S/(100+nds_flt))*100
    ACTIVATE WINDOW Rev_M
    @ 1, p_sum SAY sum_r PICTURE "@Z 999999.9999"
    ACTIVATE WINDOW (ALIAS())
  ENDIF
ELSE
  IF PRICE_S <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"–ฅญ  ‘…€ กฎซ์่ฅ ญใซ๏!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    ACTIVATE WINDOW (ALIAS())
    RETURN .F.
  ENDIF

  IF PRICE_S # rem_val
    REPLACE PRICE_S WITH ROUND_6(PRICE_S)
    sum_r = sum_r+ROUND(QNT_REV*PRICE_S,2)-ROUND(rem_val*QNT_REV,2)
    REPLACE PRICE_S_D WITH PRICE_S_D/kurs
    REPLACE PRICE_S0  WITH (PRICE_S/(100+nds_flt))*100
    ACTIVATE WINDOW Rev_M
    @ 1, p_sum SAY sum_r PICTURE "@Z 9999999.99"
    ACTIVATE WINDOW (ALIAS())
  ENDIF
ENDIF

RETURN .T.

**************************************************************************

PROCEDURE Exit_Rev   && ๋ๅฎค ็ฅเฅง ฌฅญ๎
KEYBOARD CHR(27)
DEACTIVATE MENU Rev_Lst
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                         ฅเญใโ์ ขแฅ!                                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Rev_All
PRIVATE rc_sav

rc_sav = RECNO()
sum_r = IIF(new_mode, 0, sum_d)
SCAN
  REPLACE QNT_REV WITH QNT_SAL
  IF new_mode
    IF is_u
      sum_r = sum_r+QNT_REV*PRICE_S_D
    ELSE
      sum_r = sum_r+ROUND(QNT_REV*PRICE_S,2)
    ENDIF
  ENDIF
ENDSCAN
GO rc_sav
ACTIVATE WINDOW Rev_M
@ 1, p_sum SAY sum_r PICTURE IIF(is_u, "@Z 999999.9999", "@Z 9999999.99")
ACTIVATE WINDOW (ALIAS())
DEACTIVATE MENU Rev_Lst
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                             Footer                                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Show_F
PRIVATE w000
ACTIVATE WINDOW Rev_M SAME
@ WROWS()-2, 2 SAY PREFIX COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY NAME COLOR SCHEME 1
@ WROWS()-2, COL()+1 SAY PRODUCER COLOR SCHEME 1
IF new_mode
  IF is_u
    w000 = ALLTRIM(STR(PRICE_D,10,4))
  ELSE
    w000 = ALLTRIM(STR(PRICE,12,5))
  ENDIF
  @ WROWS()-2, COL()+1 SAY "–ฅญ  ฏเ. "+w000
ENDIF
ACTIVATE WINDOW (ALIAS())
RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                          ฅเฅฎๆฅญช                                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE RePrice
PRIVATE rc_sav, prcnt, p_d, p_r, p_0
prcnt = 0
DO Get_Prc
IF EMPTY(prcnt)
  RETURN
ENDIF

rc_sav = RECNO()
sum_r = 0
SCAN
  IF is_u
    p_d = ROUND(PRICE_D*(100-prcnt)/100,4)
    p_r = ROUND_6(p_d*kurs)
    p_0 = p_r*100/(100+nds_flt)
    sum_r = sum_r+QNT_REV*p_d
  ELSE
    p_r = ROUND_6(PRICE*(100-prcnt)/100)
    p_d = ROUND(p_r/kurs,4)
    p_0 = p_r*100/(100+nds_flt)
    sum_r = sum_r+ROUND(QNT_REV*p_r,2)
  ENDIF
  REPLACE PRICE_S_D WITH p_d, PRICE_S WITH p_r, PRICE_0 WITH p_0
ENDSCAN
GO rc_sav
ACTIVATE WINDOW Rev_M
@ 1, p_sum SAY sum_r PICTURE IIF(is_u, "@Z 999999.9999", "@Z 9999999.99")
ACTIVATE WINDOW (ALIAS())
DEACTIVATE MENU Rev_Lst
SHOW MENU Rev_Lst
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     เฎๆฅคใเ  เ แๅฎค  แฏจแช .                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Exp_Rev
PRIVATE ln, wd, rev_type, let_type, p_doc_l, p_doc_i, p_doc_f, p_doc_o, ex
PRIVATE sum_rr, sum_rd, sum_r0, nlg_sr, nlg_sd, tmpNote
PRIVATE p_exp_o
PRIVATE c_save, p_prev, q, p_d, p_r, p_0, ord_note
PRIVATE is_ch, chk_date, chk_time
chk_date = DATE()
chk_time = SECONDS()
is_ch = .F.
IF sale_mode = "MEGA"
  is_ch = SUBSTR(doc_n, 2, 3) = "***" .AND. rev_type = 1 .AND. doc_d = DATE()
ENDIF

USE (tmpo_path+"REV_TMP")
sum_rd = 0
sum_rr = 0
sum_r0 = 0
c_save = 0
ord_note = SPACE(40)
tmpNote = SPACE(16)
q = 0
p_prev = 0
SCAN FOR QNT_REV # 0
  IF new_mode
    IF is_u
      p_d = PRICE_S_D
      p_r = ROUND_6(p_d*kurs)
      p_0 = p_r*100/(100+nds_flt)
    ELSE
      p_r = PRICE_S
      p_d = ROUND(p_r/kurs,4)
      p_0 = p_r*100/(100+nds_flt)
    ENDIF
    REPLACE PRICE_S WITH p_r, PRICE_S_D WITH p_d, PRICE_S0 WITH p_0
  ENDIF
  sum_rd = sum_rd+PRICE_S_D*QNT_REV
  sum_r0 = sum_r0+PRICE_S0*QNT_REV
  IF CODE # c_save .OR. PRICE_S # p_prev
    sum_rr = sum_rr+ROUND(q*p_prev,2)
    q = QNT_REV
    p_prev = PRICE_S
    c_save = CODE
  ELSE
    q = q+QNT_REV
  ENDIF
ENDSCAN
sum_rr = sum_rr+ROUND(q*p_prev,2)
nlg_sd = ROUND(sum_rd*G_Nalog(f_c000)/100,4)
nlg_sr = ROUND(sum_rr*G_Nalog(f_c000)/100,4)

let_type = cus_c # 0
rev_type = IIF(is_u, 2, 1)
p_doc_l = .T.
p_doc_i = .F.
p_doc_f = .F.
p_doc_o = .F.
p_exp_o = cus_c # 0

ex = 1

DEFINE WINDOW H_Win FROM 0,0 TO 0,WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW H_Win
ln = 21
wd = 47
DO D_W WITH "Ret_Money", ln, wd, " แ็ฅโ แ ชซจฅญโฎฌ", 1

*           แ็ฅโ แ ชซจฅญโฎฌ
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ    ‘ใฌฌ  9999999.99เใก  (999999.9999$)    ณ
*ณ ซฎฃ 99.9% 999999.9999$ จโฎฃฎ 999999.9999$ณ
*ณ     ฺฤ ฎงขเ โ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ     ณ
*ณ     ณ   ( ) ฅญฅฃ  ( )   คฅฏฎงจโ   ณ     ณ
*ณ     ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู     ณ
*ณ     ฺฤ เจ็จญ  ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ     ณ
*ณ     ณ       ....:....!....:.        ณ     ณ
*ณ     ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู     ณ
*ณ     ฺฤ ฅ็ โ์ คฎชใฌฅญโฎข ฤฤฤฤฤฤฤฤฤฤฤฟ     ณ
*ณ     ณ [ ]  ชซ คญ ๏                 ณ     ณ
*ณ     ณ [ ]  แๅฎคญ๋ฉ ฎเคฅเ ( —ฅช )   ณ     ณ
*ณ     ณ [ ] ‘็ฅโ-ไ ชโใเ               ณ     ณ
*ณ     ณ [ ]  ๏ขช  ญ  แชซ ค           ณ     ณ
*ณ     ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู     ณ
*ณฺฤ เจฌฅ็ ญจฅ คซ๏ ง ๏ขชจ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟณ
*ณณ....:....!....:....!....:....!....:....! ณณ
*ณภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤูณ
*ณ     < OK Ctrl-W > < โช ง โ์แ๏ Esc >      ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

IF is_u
  @ 2, 6 SAY "‘ใฌฌ "+STR(sum_rd,12,4)+"$  ("+STR(sum_rr,10,2)+"เใก)"
  @ 3, 2 SAY " ซฎฃ "+STR(G_Nalog(f_c000),4,1)+"% "+STR(nlg_sd,11,4)+"$ โฎฃฎ "
  @ 3, COL() SAY STR(nlg_sd+sum_rd,11,4) COLOR (SCHEME(13,6))
  @ 3, COL() SAY "$"
  IF .NOT. LetMoney(f_c000, "3")
    let_type = .F.
    rev_type = 2
  ENDIF
ELSE
  @ 2, 6 SAY "‘ใฌฌ "+STR(sum_rr,11,2)+"เใก  ("+STR(sum_rd,11,4)+"$)"
  @ 3, 2 SAY " ซฎฃ "+STR(G_Nalog(f_c000),4,1)+"% "+STR(nlg_sr,11,2)+"เ โฎฃฎ "
  @ 3, COL() SAY STR(nlg_sr+sum_rr,11,2) COLOR (SCHEME(13,6))
  @ 3, COL() SAY "เ"
  IF .NOT. LetMoney(f_c000, " ")
    let_type = .F.
    rev_type = 2
  ENDIF
ENDIF
@  4, 7 TO 6, 39
@  4, 9 SAY " ฎงขเ โ "
@  7, 7 TO 9, 39
@  7, 9 SAY " เจ็จญ  "
@ 10, 7 TO 15, 39
@ 10, 9 SAY " ฅ็ โ์ คฎชใฌฅญโฎข "
@ 16, 2 TO 18, 44
@ 16, 4 SAY " เจฌฅ็ ญจฅ คซ๏ ง ๏ขชจ "
@  5,11 GET rev_type PICTURE "@*RH ฅญฅฃ   ;  คฅฏฎงจโ" WHEN let_type .AND. .NOT. is_u VALID Set_Sw()
@  8,15 GET tmpNote
@ 11, 9 GET p_doc_i PICTURE "@*C  ชซ คญ ๏"
@ 12, 9 GET p_exp_o PICTURE "@*C  แๅฎคญ๋ฉ ฎเคฅเ ( —ฅช )" WHEN rev_type = 1 .OR. is_u
@ 13, 9 GET p_doc_f PICTURE "@*C ‘็ฅโ-ไ ชโใเ " WHEN flg_f .OR. rev_type = 2
@ 14, 9 GET p_doc_o PICTURE "@*C  ๏ขช  ญ  แชซ ค"
@ 17, 3 GET ord_note
@ 19, 7 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
READ CYCLE VALID Exp_Base()

RELEASE WINDOW H_Win
RELEASE WINDOW Ret_Money

CLOSE DATABASES
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 เฎฃเ ฌฌ  ข๋ๅฎค  จง ฏฎซ๏ โจฏ  เ แ็ฅโ .                บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Set_Sw

IF ex = 2
  RETURN .T.
ENDIF

IF rev_type = 2
  p_exp_o = .F.
  SHOW GET p_exp_o
ENDIF

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎๆฅคใเ  เ แๅฎค  ฏฎ ก งฅ ค ญญ๋ๅ.                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Exp_Base

PRIVATE kass_r, kass_usd, tmpNum, pay_type, mss_sum
mss_sum = 0
m.pay_type = ""
SELECT 0
USE (base_path+"ARM_LST") ALIAS A1703 AGAIN
GO arm_rec
kass_usd = KASSA_USD
kass_r = KASSA
USE
IF is_u
  DO Exp_Base1
  RETURN
ENDIF

PRIVATE o_num, b_num, mon_num, inv_n, c1, c2, kassa_i, p_type, q_rep, sum_d0
PRIVATE o_cnt, l_cnt, tmpZone, curZone, p_ln, is_kkm
PRIVATE nom_f, type_f
p_ln = 20
IF ex = 2
  RETURN
ENDIF

*
*   ฎซใ็ ฅฌ ญฎฌฅเ  คฎชใฌฅญโฎข:
*
p_type = " "
IF rev_type = 1
  IF cus_t = "3"
    p_type = "3"
    mon_num = Doc_Num("MONEY_$", f_c000, arm, DATE())
    mon_num = sys_char+STR(mon_num,9)
  ELSE
    mon_num = "          "
    IF ch_today		&& is_ch
      PRIVATE chk_err
      chk_err = 0
      is_kkm = FirmKKM(f_c000)
      IF EMPTY(is_kkm)
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"—ฅช จ ค ญญฎฅ ฏเฅคฏเจ๏โจฅ ญฅแฎขฌฅแโจฌ๋!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        RETURN .F.
      ENDIF
      is_kkm = VAL(ALLTRIM(is_kkm))
      IF File_O(base_path+"TO_KKM.DBF") .AND. is_kkm # 0
        DO Do_Check WITH -sum_r0, -sum_rr+sum_r0, -nlg_sr, mon_num, chk_date, chk_time, chk_err
        IF .NOT. EMPTY(chk_err)
          DIMENSION mss(3)
          mss(1) = ""
          mss(2) = CHR(0)+"่จกช  "+ALLTRIM(STR(chk_err))+" ฏเจ ฏฎฏ๋โชฅ ฏเฎกจโ์ ็ฅช"
          mss(3) = ""
          DO Out_Mess WITH 7, "mss"
          RETURN .F.
        ENDIF
      ELSE
        mon_num = Doc_Num("CHECK", f_c000, arm, DATE())
        mon_num = sys_char+STR(mon_num,9)
      ENDIF
      m.pay_type = "—"
    ELSE
      mon_num = Doc_Num("EXP_ORDER", f_c000, arm, DATE())
      mon_num = sys_char+STR(-mon_num,9)
      m.pay_type = " "
    ENDIF
  ENDIF
ELSE
  mon_num = ""
ENDIF
SELECT 0
o_num = Doc_Num("LISTS", f_c000, arm, DATE())

IF is_ch
  b_num = 0
ELSE
  IF cus_t = "3"
    b_num = Doc_Num("BILL_F$", f_c000, arm, DATE())
  ELSE
    b_num = Doc_Num("BILL_F", f_c000, arm, DATE())
  ENDIF
ENDIF

IF is_ch .AND. doc_d = DATE()   && แฅฃค  ญ ชซ คญ ๏
  inv_n = Doc_Num("G_CHECK", f_c000, arm, DATE())
ELSE
  inv_n = Doc_Num("SALE", f_c000, arm, DATE())
ENDIF
IF rev_type = 1              && ฅญ์ฃจ ็ฅเฅง ช แแใ
  USE (base_path+"ARM_LST") ALIAS ARM_9204 AGAIN
  GO arm_rec
  IF cus_t = "3"
    REPLACE KASSA_USD WITH KASSA_USD-sum_rd
  ELSE
    REPLACE KASSA WITH KASSA-sum_rr-nlg_sr
  ENDIF
ENDIF

inv_n = sys_char+IIF(is_ch, "***", CHRTRAN(arm," ","0"))+STR(inv_n,6)

*
*   ”ฎเฌจเใฅฌ ง ๏ขชใ ญ  แชซ ค
*
USE (base_path+"ZONES") ORDER TAG ZONE
SELECT 0
USE (base_path+"ORD_T")
SELECT 0
USE (base_path+"ORD_D")
SELECT REV_TMP
curZone = CHR(0)+CHR(0)
o_cnt = 0
l_cnt = 0
SCAN FOR .NOT. EMPTY(QNT_REV)
  tmpZone = WhatZone(ADDRESS)
  IF l_cnt >= 20 .OR. curZone # tmpZone
    o_cnt = o_cnt+1
    curZone = tmpZone
    SELECT ORD_T
    IF o_cnt # 0
      REPLACE SIZE_NEG WITH l_cnt
    ENDIF
    APPEND BLANK
    REPLACE LIST_NUM  WITH STR(o_num,6), ;
            LIST_NAME WITH "€’ "+d_nam, ;
            ORD_NUM   WITH o_cnt,   ;
            ORD_DATE  WITH DATE(),  ;
            ORD_TIME  WITH TIME(),  ;
            REQ_DATE  WITH DATE(),  ;
            REQ_TIME  WITH TIME(),  ;
            QUEUE     WITH queuetype,;
            ZONE      WITH curZone, ;
            NOTE      WITH ord_note,;
            WHO       WITH user,    ;
            WHERE     WITH arm
    l_cnt = 0
  ENDIF
  l_cnt = l_cnt+1
  SELECT ORD_D
  APPEND BLANK
  REPLACE LIST_NUM WITH STR(o_num,6), ;
          ORD_NUM  WITH o_cnt,        ;
          CODE     WITH REV_TMP.CODE, ;
          QNT      WITH -REV_TMP.QNT_REV
  SELECT REV_TMP
ENDSCAN
SELECT ORD_T
IF o_cnt # 0
  REPLACE SIZE_NEG WITH l_cnt
ENDIF
USE
SELECT ZONES
USE
SELECT ORD_D
USE


*
*    “ๅฎค คฅญฅฃ จง ช แแ๋ จซจ ฏเจๅฎค ญ  คฅฏฎงจโ ชซจฅญโ 
*

IF rev_type = 1             && ฅญ์ฃจ ข ช แแใ...
  IF .NOT. EMPTY(mon_num)   &&        ญ คฎ โจแญใโ์ คฎชใฌฅญโ จซจ... ญจ็ฅฃฎ!
    tmpNalog = G_Nalog(f_c000)
    USE (base_path+"INCMONEY")
    APPEND BLANK
    REPLACE FIRM     WITH f_c000,     ;
            DOC_TYPE WITH m.pay_type, ;
            DOC_NUM  WITH mon_num,    ;
            DOC_DATE WITH chk_date,   ;
            DOC_TIME WITH chk_time,   ;
            ENTER_DAT WITH chk_date,  ;
            SALE_NUM WITH inv_n,      ;
            SALE_DATE WITH DATE(),    ;
            CUS_CODE WITH cus_c,      ;
            CUS_NAME WITH cus_n,      ;
            MONEY_R  WITH -sum_rr,    ;
            MONEY_D  WITH -sum_rd,    ;
            NDS_     WITH nds_p,      ;
            MONEY_0  WITH -sum_r0,    ;
            NL_PRC   WITH tmpNalog,   ;
            N_OF_SALE WITH -nlg_sr,   ;
            WHO      WITH user,       ;
            WHERE    WITH arm
    IF DOC_TYPE = "—"
      mss_sum = MONEY_R
    ENDIF
    IF DOC_TYPE = "—" .AND. Is_Ur(CUS_CODE) .AND. YEAR(DOC_DATE) < 2006
      IF MONEY_R > 0 .OR. MONEY_D > 0
        tmpNum = Doc_Num("INC_ORDER", FIRM, arm, LEFT(DTOS(DATE()),4))
      ELSE
        tmpNum = Doc_Num("EXP_ORDER", FIRM, arm, LEFT(DTOS(DATE()),4))
        tmpNum = -tmpNum
      ENDIF
      REPLACE ENTER_DOC WITH CHR(0)+sys_char+STR(tmpNum,9)
    ENDIF
  ENDIF
  IF nlg_sr # 0
    USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F99_9204 AGAIN
    IF SEEK(f_c000)
      REPLACE NALOG WITH NALOG-nlg_sr
    ENDIF
  ENDIF
ELSE                       &&          ... ฏเจญ๏โ์ ญ  คฅฏฎงจโ!

  USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS CRST_9204 AGAIN
  IF .NOT. SEEK(STR(cus_c)+f_c000)
    APPEND BLANK
    REPLACE CUS_CODE WITH cus_c, FIRM WITH f_c000
  ENDIF
  REPLACE ACC_SUMD  WITH ACC_SUMD+IIF(cus_t = "3", sum_rd, 0), ;
          ACC_SUM   WITH ACC_SUM +IIF(cus_t # "3", sum_rr, 0)

ENDIF
*
*     ฃฎซฎขฎช ญ ชซ คญฎฉ
*
tmpNalog = G_Nalog(f_c000)
USE (base_path+"SALE_TIT")
APPEND BLANK
REPLACE FIRM      WITH f_c000,       ;
        DOC_NUM   WITH inv_n,        ;
        DOC_DATE  WITH DATE(),       ;
        DOC_TIME  WITH SECONDS(),    ;
        NEW_DOC   WITH sys_char+STR(b_num,9), ;
        NEW_TYPE  WITH cus_t,        ;
        SAL_FLAGS WITH IIF(is_ch, "              * ", ""), ;
        CUS_CODE  WITH cus_c,        ;
        NOTE      WITH tmpNote,      ;
        CUS_NAME  WITH d_nam,        ;
        REQ_DOC   WITH doc_n,        ;
        REQ_DATE  WITH doc_d,        ;
        LIST_NUM  WITH STR(o_num,6), ;
        LIST_DATE WITH DATE(),       ;
        PAY_TYPE  WITH m.pay_type,   ;
        PAY_DOC   WITH mon_num,      ;
        PAY_DATE  WITH DATE(),       ;
        NDS_      WITH nds_p,        ;
        NSP_      WITH tmpNalog,     ;
        TOTAL_0   WITH -sum_r0,      ;
        TOTAL_R   WITH -sum_rr,      ;
        KASSA     WITH IIF(rev_type = 1, -sum_rr, 0), ;
        TOTAL_D   WITH -sum_rd,      ;
        WHO_LET   WITH user,         ;
        WHO       WITH user,         ;
        WHERE     WITH arm

  nom_f = NEW_DOC
  type_f = NEW_TYPE
*
*    ‘ฎคฅเฆจฌฎฅ ญ ชซ คญฎฉ จ โฅชใ้ฅฅ ญ ซจ็จฅ
*
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE
SELECT 0
*USE (base_path+"ACT_S") ORDER TAG INC_LIST
USE (base_path+"ACT_S") ORDER TAG PARTY
SELECT 0
USE (base_path+"SALE") AGAIN

SELECT REV_TMP
sum_d0 = 0
SCAN FOR .NOT. EMPTY(QNT_REV)
  = SEEK(REV_TMP.CODE, "STOCK")
  SELECT SALE
  APPEND BLANK
  IF sale_mode = "Mitracon"
    q_rep = REV_TMP.QNT_REV
  ELSE
    q_rep = MIN(REV_TMP.QNT_REV, REV_TMP.QNT_REP)
  ENDIF
  REPLACE FIRM       WITH f_c000,       ;
          DOC_NUM    WITH inv_n,        ;
          DOC_DATE   WITH DATE(),       ;
          CODE       WITH REV_TMP.CODE, ;
          QNT        WITH -REV_TMP.QNT_REV, ;
          PRICE_D_L  WITH IIF(own_val = "$", STOCK.PRICE, ;
                                            STOCK.PRICE_R/kurs_b), ;
          PRICE_R_L  WITH IIF(own_val = "$", Clc_Rub(STOCK.PRICE), ;
                                            STOCK.PRICE_R), ;
          PRICE_TYPE WITH REV_TMP.PRICE_TYPE, ;
          PRICE_R    WITH REV_TMP.PRICE_S,    ;
          PRICE_D    WITH REV_TMP.PRICE_S_D,  ;
          PRICE_0    WITH REV_TMP.PRICE_S0,   ;
          ACT_NUM    WITH REV_TMP.ACT_NUM,    ;
          ACT_DATE   WITH REV_TMP.ACT_DATE,   ;
          ACT_FIRM   WITH REV_TMP.ACT_FIRM,   ;
          PARTY      WITH REV_TMP.PARTY,      ;
          GTD_NUM    WITH REV_TMP.GTD_NUM,    ;
          COUNTRY    WITH REV_TMP.COUNTRY,    ;
          COUNTRYC   WITH REV_TMP.COUNTRYC,   ;
          GTD_LINE   WITH REV_TMP.GTD_LINE,   ;
          WHERE      WITH arm
  sum_d0 = sum_d0 + QNT*PRICE_D
  IF q_rep # 0
    GO REV_TMP.R_NO
    REPLACE QNT_REP WITH QNT_REP-q_rep
  ENDIF
  SELECT STOCK
  REPLACE QNT     WITH QNT+REV_TMP.QNT_REV, ;
          QNT_REP WITH QNT_REP+q_rep
  SELECT ACT_S
*  SEEK STR(SALE.CODE,7)+SALE.ACT_FIRM+DTOS(SALE.ACT_DATE)+SALE.ACT_NUM
  SEEK SALE.PARTY
  REPLACE QNT_REST WITH QNT_REST+REV_TMP.QNT_REV, ;
          REST_REP WITH REST_REP+q_rep
  SELECT REV_TMP
ENDSCAN

DO Cr_List WITH SALE_TIT.FIRM, SALE_TIT.DOC_NUM, SALE_TIT.DOC_DATE
SELECT SALE_TIT
USE
SELECT STOCK
USE
SELECT SALE
USE
SELECT ACT_S
USE

IF .NOT. EMPTY(inv_n)
  DO SendToVK WITH f_c000, inv_n, DATE()
ENDIF

IF .NOT. EMPTY(mon_num) .AND. m.pay_type = "—"
  PRIVATE mss000
  DIMENSION mss000[3]
  mss000[1] = ""
  mss000[2] = CHR(0)+"€…! ‘ไฎเฌจเฎข ญ ช แแฎข๋ฉ ็ฅช ญ  แใฌฌใ "+ALLTRIM(STR(-sum_rr, 15, 2))+"."
  mss000[3] = ""
  DO Out_Mess WITH 5, "mss000"
ENDIF

IF p_doc_i
  DO Prn_Inv WITH f_c000, inv_n, DATE()	&&, "I"
ENDIF
IF p_doc_f
  DO P_Bill_F WITH f_c000, nom_f, DATE(), type_f
ENDIF
IF p_doc_o
  IF new_ord
    DO Prn_OrdN WITH o_num, 1
  ELSE
    DO Prn_Ord WITH o_num, 1
  ENDIF
ENDIF
IF .NOT. EMPTY(mon_num)
  IF p_exp_o               && เจๅฎคญ๋ฉ ฎเคฅเ
    DO P_Money  WITH f_c000, m.pay_type, mon_num, DATE()
  ENDIF
ENDIF

RELEASE WINDOW H_Win
RELEASE WINDOW Ret_Money

IF is_ch .AND. ex = 1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"€ ็ฅช  ? เฎกจข ฅฌ !"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎฃเ ฌฌ  ข๋ขฎค  ชใเแ  คซ๏ ญ ชซ คญฎฉ.                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cource
PARAMETERS df, dn, dd
PRIVATE mss

ACTIVATE SCREEN

DO Crs_Inv WITH df, dn, dd

SHOW MENU (menu_name)
ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  Get_Valuta    งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                           ฏเฅคฅซ๏ฅฌ ข ซ๎โใ.                           ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 17.01.2000 ฤฤู
PROCEDURE Get_Valuta

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

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ                   ณ
*ณ < USD > < ใกซจ > ณ
*ณ                   ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 6, 23, " ซ๎โ "

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*


@ 3, 3 GET ex PICTURE "@*HT \ USD ;\ ใกซจ "

READ CYCLE
is_u = ex = 1
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                           เฎๆฅญโ ฏฅเฅฎๆฅญชจ                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Prc
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
*ณ                                  ณ
*ณ     “ฌฅญ์่จโ์ ๆฅญ๋ ญ  9999%      ณ
*ณ                                  ณ
*ณ < OK Ctrl-W > < โช ง โ์แ๏ Esc > ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, "ฅเฅฎๆฅญช "
@ 3, 7 SAY "“ฌฅญ์่จโ์ ๆฅญ๋ ญ  9999%"
*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*

@ 3, 25 GET prcnt PICTURE "9999"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE

IF ex # 1
  *
  * โเ ก โ๋ข ฅฌ กซ ญช
  prcnt = 0
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 เฎๆฅคใเ  เ แๅฎค  ฏฎ ก งฅ ค ญญ๋ๅ USD.                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Exp_Base1

PRIVATE o_num, b_num, mon_numr, mon_numd, inv_n, c1, c2, kassa_i, p_type, q_rep, sum_d0
PRIVATE o_cnt, l_cnt, tmpZone, curZone, p_ln, tmpNum
p_ln = 20
IF ex = 2
  RETURN
ENDIF

*
*   ฎซใ็ ฅฌ ญฎฌฅเ  คฎชใฌฅญโฎข:
*

SELECT 0
o_num = Doc_Num("LISTS", f_c000, arm, DATE())

IF is_ch
  b_num = 0
ELSE
  IF cus_t = "3"
    b_num = Doc_Num("BILL_F$", f_c000, arm, DATE())
  ELSE
    b_num = Doc_Num("BILL_F", f_c000, arm, DATE())
  ENDIF
ENDIF

p_type = " "
mon_numd = Doc_Num("MONEY_$", f_c000, arm, DATE())
mon_numd = sys_char+STR(mon_numd,9)
IF is_ch
  mon_numr = Doc_Num("CHECK", f_c000, arm, DATE())
  mon_numr = sys_char+STR(mon_num,9)
ELSE
  mon_numr = Doc_Num("EXP_ORDER", f_c000, arm, DATE())
  mon_numr = sys_char+STR(-mon_numr,9)
ENDIF

IF is_ch
  inv_n = Doc_Num("G_CHECK", f_c000, arm, DATE())
ELSE
  inv_n = Doc_Num("SALE", f_c000, arm, DATE())
ENDIF

inv_n = sys_char+IIF(is_ch, "***", CHRTRAN(arm," ","0"))+STR(inv_n,6)

*
*   ”ฎเฌจเใฅฌ ง ๏ขชใ ญ  แชซ ค
*
USE (base_path+"ZONES") ORDER TAG ZONE
SELECT 0
USE (base_path+"ORD_T")
SELECT 0
USE (base_path+"ORD_D")
SELECT REV_TMP
curZone = CHR(0)+CHR(0)
o_cnt = 0
l_cnt = 0
SCAN FOR .NOT. EMPTY(QNT_REV)
  tmpZone = WhatZone(ADDRESS)
  IF l_cnt >= 20 .OR. curZone # tmpZone
    o_cnt = o_cnt+1
    curZone = tmpZone
    SELECT ORD_T
    IF o_cnt # 0
      REPLACE SIZE_NEG WITH l_cnt
    ENDIF
    APPEND BLANK
    REPLACE LIST_NUM  WITH STR(o_num,6), ;
            LIST_NAME WITH "€’ "+d_nam, ;
            ORD_NUM   WITH o_cnt,  ;
            ORD_DATE  WITH DATE(), ;
            ORD_TIME  WITH TIME(), ;
            REQ_DATE  WITH DATE(), ;
            REQ_TIME  WITH TIME(), ;
            QUEUE     WITH queuetype,;
            NOTE      WITH ord_note,;
            ZONE      WITH curZone,;
            WHO       WITH user,   ;
            WHERE     WITH arm
    l_cnt = 0
  ENDIF
  l_cnt = l_cnt+1
  SELECT ORD_D
  APPEND BLANK
  REPLACE LIST_NUM WITH STR(o_num,6), ;
          ORD_NUM  WITH o_cnt,        ;
          CODE     WITH REV_TMP.CODE, ;
          QNT      WITH -REV_TMP.QNT_REV
  SELECT REV_TMP
ENDSCAN
SELECT ORD_T
IF o_cnt # 0
  REPLACE SIZE_NEG WITH l_cnt
ENDIF
USE
SELECT ZONES
USE
SELECT ORD_D
USE

*
*    “ๅฎค คฅญฅฃ จง ช แแ๋ จซจ ฏเจๅฎค ญ  คฅฏฎงจโ ชซจฅญโ 
*
tmpNalog = G_Nalog(f_c000)
USE (base_path+"INCMONEY")
APPEND BLANK    &&  แๅฎค เใกซฅฉ
REPLACE FIRM     WITH f_c000, ;
        DOC_TYPE WITH IIF(is_ch, "—", " "), ;
        DOC_NUM  WITH mon_numr,   ;
        DOC_DATE WITH DATE(),     ;
        DOC_TIME WITH SECONDS(),  ;
        ENTER_DAT WITH DATE(),    ;
        SALE_NUM WITH inv_n,      ;
        SALE_DATE WITH DATE(),    ;
        CUS_CODE WITH cus_c,      ;
        CUS_NAME WITH cus_n,      ;
        MONEY_R  WITH -sum_rr,    ;
        MONEY_D  WITH -sum_rd,    ;
        NDS_     WITH nds_p,      ;
        MONEY_0  WITH -sum_r0,    ;
        NL_PRC   WITH tmpNalog,   ;
        N_OF_SALE WITH -nlg_sr,   ;
        WHO      WITH user,       ;
        WHERE    WITH arm
IF DOC_TYPE = "—" .AND. Is_Ur(CUS_CODE) .AND. YEAR(DOC_DATE) < 2006
  IF MONEY_R > 0 .OR. MONEY_D > 0
    tmpNum = Doc_Num("INC_ORDER", FIRM, arm, LEFT(DTOS(DATE()),4))
  ELSE
    tmpNum = Doc_Num("EXP_ORDER", FIRM, arm, LEFT(DTOS(DATE()),4))
    tmpNum = -tmpNum
  ENDIF
  REPLACE ENTER_DOC WITH CHR(0)+sys_char+STR(tmpNum,9)
ENDIF

APPEND BLANK    && เจๅฎค USD
REPLACE FIRM     WITH f_c000, ;
        DOC_TYPE WITH "3",    ;
        DOC_NUM  WITH mon_numd,   ;
        DOC_DATE WITH DATE(),     ;
        DOC_TIME WITH SECONDS(),  ;
        ENTER_DAT WITH DATE(),    ;
        CUS_CODE WITH cus_c,      ;
        CUS_NAME WITH cus_n,      ;
        MONEY_R  WITH sum_rr+nlg_sr, ;
        MONEY_D  WITH sum_rd+nlg_sd,;
        MONEY_0  WITH sum_rd+nlg_sd,;
        WHO      WITH user,       ;
        WHERE    WITH arm
IF nlg_sr # 0
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F99_9204 AGAIN
  IF SEEK(f_c000)
    REPLACE NALOG WITH NALOG-nlg_sr
  ENDIF
ENDIF
&& ฅฏฎงจโ
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS CRST_9204 AGAIN
IF .NOT. SEEK(STR(cus_c)+f_c000)
  APPEND BLANK
  REPLACE CUS_CODE WITH cus_c, FIRM WITH f_c000
ENDIF
REPLACE ACC_SUMD WITH ACC_SUMD+sum_rd+nlg_sd
*
*     ฃฎซฎขฎช ญ ชซ คญฎฉ
*
tmpNalog = G_Nalog(f_c000)
USE (base_path+"SALE_TIT")
APPEND BLANK
REPLACE FIRM      WITH f_c000,       ;
        DOC_NUM   WITH inv_n,        ;
        DOC_DATE  WITH DATE(),       ;
        DOC_TIME  WITH SECONDS(),    ;
        NEW_DOC   WITH sys_char+STR(b_num,9), ;
        NEW_TYPE  WITH cus_t,        ;
        CUS_CODE  WITH cus_c,        ;
        CUS_NAME  WITH d_nam,        ;
        REQ_DOC   WITH doc_n,        ;
        REQ_DATE  WITH doc_d,        ;
        LIST_NUM  WITH STR(o_num,6), ;
        LIST_DATE WITH DATE(),       ;
        PAY_TYPE  WITH IIF(is_ch, "—", p_type), ;
        PAY_DOC   WITH mon_numr,     ;
        PAY_DATE  WITH DATE(),       ;
        NDS_      WITH nds_p,        ;
        NSP_      WITH tmpNalog,     ;
        TOTAL_0   WITH -sum_r0,      ;
        TOTAL_R   WITH -sum_rr,      ;
        KASSA     WITH IIF(rev_type = 1, -sum_rr, 0), ;
        TOTAL_D   WITH -sum_rd,      ;
        WHO_LET   WITH user,         ;
        WHO       WITH user,         ;
        WHERE     WITH arm

nom_f = NEW_DOC
type_f = NEW_TYPE

*
*    ‘ฎคฅเฆจฌฎฅ ญ ชซ คญฎฉ จ โฅชใ้ฅฅ ญ ซจ็จฅ
*
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE
SELECT 0
*USE (base_path+"ACT_S") ORDER TAG INC_LIST
USE (base_path+"ACT_S") ORDER TAG PARTY
SELECT 0
USE (base_path+"SALE") AGAIN

SELECT REV_TMP
sum_d0 = 0
SCAN FOR .NOT. EMPTY(QNT_REV)
  = SEEK(REV_TMP.CODE, "STOCK")
  SELECT SALE
  APPEND BLANK
  q_rep = MIN(REV_TMP.QNT_REV, REV_TMP.QNT_REP)
  REPLACE FIRM       WITH f_c000,       ;
          DOC_NUM    WITH inv_n,        ;
          DOC_DATE   WITH DATE(),       ;
          CODE       WITH REV_TMP.CODE, ;
          QNT        WITH -REV_TMP.QNT_REV, ;
          PRICE_D_L  WITH IIF(own_val = "$", STOCK.PRICE, ;
                                            STOCK.PRICE_R/kurs_b), ;
          PRICE_R_L  WITH IIF(own_val = "$", Clc_Rub(STOCK.PRICE), ;
                                            STOCK.PRICE_R), ;
          PRICE_TYPE WITH REV_TMP.PRICE_TYPE, ;
          PRICE_R    WITH REV_TMP.PRICE_S,    ;
          PRICE_D    WITH REV_TMP.PRICE_S_D,  ;
          PRICE_0    WITH REV_TMP.PRICE_S0,   ;
          ACT_NUM    WITH REV_TMP.ACT_NUM,    ;
          ACT_DATE   WITH REV_TMP.ACT_DATE,   ;
          PARTY      WITH REV_TMP.PARTY,      ;
          ACT_FIRM   WITH REV_TMP.ACT_FIRM,   ;
          GTD_NUM    WITH REV_TMP.GTD_NUM,    ;
          COUNTRY    WITH REV_TMP.COUNTRY,    ;
          COUNTRYC   WITH REV_TMP.COUNTRYC,   ;
          GTD_LINE   WITH REV_TMP.GTD_LINE,   ;
          WHERE      WITH arm
  sum_d0 = sum_d0 + QNT*PRICE_D
  IF q_rep # 0
    GO REV_TMP.R_NO
    REPLACE QNT_REP WITH QNT_REP-q_rep
  ENDIF
  SELECT STOCK
  REPLACE QNT     WITH QNT+REV_TMP.QNT_REV, ;
          QNT_REP WITH QNT_REP+q_rep
  SELECT ACT_S
*  SEEK STR(SALE.CODE,7)+SALE.ACT_FIRM+DTOS(SALE.ACT_DATE)+SALE.ACT_NUM
  SEEK SALE.PARTY
  REPLACE QNT_REST WITH QNT_REST+REV_TMP.QNT_REV, ;
          REST_REP WITH REST_REP+q_rep
  SELECT REV_TMP
ENDSCAN

SELECT SALE_TIT
USE
SELECT STOCK
USE
SELECT SALE
USE
SELECT ACT_S
USE
*
*   แแ  เ กฎ็ฅฃฎ ฌฅแโ 
*
PRIVATE doc1, doc2, doc_n00
USE (base_path+"KASSA") ALIAS KSS_9212 AGAIN
doc1 = Doc_Num("KASSA", f_c000, arm, DATE())
doc1 = sys_char+CHRTRAN(arm," ","0")+STR(doc1,6)
APPEND BLANK
REPLACE DOC_NUM    WITH doc1,      ;
        DOC_DATE   WITH DATE(),    ;
        DOC_TIME   WITH SECONDS(), ;
        VALUTA     WITH "",       ;
        TOTAL      WITH -sum_rr-nlg_sr, ;
        MONEY_REST WITH 0,         ;
        KASSA_REST WITH kass_r,    ;
        WHO_REP    WITH user,      ;
        WHO_GET    WITH user,      ;
        WHERE      WITH arm

doc2 = Doc_Num("KASSA", f_c000, arm, DATE())
doc2 = sys_char+CHRTRAN(arm," ","0")+STR(doc2,6)
APPEND BLANK
REPLACE DOC_NUM    WITH doc2,      ;
        DOC_DATE   WITH DATE(),    ;
        DOC_TIME   WITH SECONDS(), ;
        VALUTA     WITH "$",       ;
        TOTAL      WITH sum_rd+nlg_sd, ;
        MONEY_REST WITH 0,         ;
        KASSA_REST WITH kass_usd,  ;
        WHO_REP    WITH user,      ;
        WHO_GET    WITH user,      ;
        WHERE      WITH arm
*
*   แญฎขญ ๏ ช แแ 
*
USE (base_path+"KASSA_M") ALIAS KSS_9212 AGAIN
doc_n00 = Doc_Num("KASSA_M", f_c000, arm, DATE())
doc_n00 = sys_char+STR(doc_n00,6)
APPEND BLANK
REPLACE DOC_NUM    WITH doc_n00,;
        DOC_DATE   WITH DATE(), ;
        DOC_TIME   WITH SECONDS(),;
        CODE       WITH 1,      ;
        REF_NUM    WITH doc1,   ;
        REF_DATE   WITH DATE(), ;
        SUM_R      WITH -sum_rr-nlg_sr,;
        NOTE       WITH d_nam,  ;
        WHO        WITH user,   ;
        WHERE      WITH arm
doc_n00 = Doc_Num("KASSA_M", f_c000, arm, DATE())
doc_n00 = sys_char+STR(doc_n00,6)
APPEND BLANK
REPLACE DOC_NUM    WITH doc_n00,;
        DOC_DATE   WITH DATE(), ;
        DOC_TIME   WITH SECONDS(),  ;
        CODE       WITH 1,      ;
        REF_NUM    WITH doc2,   ;
        REF_DATE   WITH doc_d,  ;
        SUM_D      WITH sum_rd+nlg_sd,;
        NOTE       WITH d_nam,  ;
        WHO        WITH user,   ;
        WHERE      WITH arm

doc_n00 = Doc_Num("KASSA_M", f_c000, arm, DATE())
doc_n00 = sys_char+STR(doc_n00,6)
APPEND BLANK
REPLACE DOC_NUM    WITH doc_n00, ;
        DOC_DATE   WITH DATE(),  ;
        DOC_TIME   WITH SECONDS(),  ;
        CODE       WITH 2,      ;
        SUM_R      WITH sum_rr+nlg_sr,;
        SUM_D      WITH -sum_rd-nlg_sd,;
        NOTE       WITH d_nam,  ;
        KURS_FL    WITH (sum_rr+nlg_sr)/(sum_rd+nlg_sd),;
        WHO        WITH user,   ;
        WHERE      WITH arm
USE

IF .NOT. EMPTY(inv_n)
  DO SendToVK WITH f_c000, inv_n, DATE()
ENDIF
IF p_doc_i
  DO Prn_Inv WITH f_c000, inv_n, DATE()	&&, "I"
ENDIF
IF p_doc_f
  DO P_Bill_F WITH f_c000, nom_f, DATE(), type_f
ENDIF
IF p_doc_o
  IF new_ord
    DO Prn_OrdN WITH o_num, 1
  ELSE
    DO Prn_Ord WITH o_num, 1
  ENDIF
ENDIF
IF p_exp_o               && เจๅฎคญ๋ฉ ฎเคฅเ
  DO P_Money  WITH f_c000, IIF(is_ch, "—", p_type), mon_numr, DATE()
  DO P_Money  WITH f_c000, "3", mon_numd, DATE()
ENDIF
RETURN

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  G_Nalog       งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                      ๋็จแซฅญจฅ ญ ซฎฃ  แ ฏเฎค ฆ.                       ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 21.01.2002 ฤฤู
PROCEDURE G_Nalog
PARAMETERS tmpFirm

PRIVATE s_sav, tmpUr, tmpPers

tmpUr = .T.
IF cus_c # 0
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"ACCOUNT") SHARED AGAIN ALIAS CUS_2121 ORDER TAG CUS_CODE
  SEEK cus_c
  tmpUr = EMPTY(MARK1)
  USE
  SELECT (s_sav)
ENDIF

*IF is_ch .OR. .NOT. tmpUr
IF .NOT. tmpUr
  tmpPers = GetNalog(tmpFirm)
ELSE
  tmpPers = 0
ENDIF

RETURN tmpPers
