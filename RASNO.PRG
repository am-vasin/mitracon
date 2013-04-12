*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                         เฎฃเ ฌฌ  " งญฎฃซ แจ๏"                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Rasno

PRIVATE seek_type, rc_no, kode,k_name
PRIVATE f_c000, f_n000, f_c001, f_c_old
PRIVATE shab1, shab2

WAIT WIND "ฏฅเ ๆจ๏ ฎโฌฅญฅญ " NOWAIT
RETURN

shab1 = "abcdefghijklmnopqrstuvwxyz กขฃคฅ๑ฆงจฉชซฌญฎฏเแโใไๅๆ็่้๊๋์ํ๎๏"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ€…๐‘’“”•–—"
f_c000  = def_firm
f_c_old = def_firm
f_n000  = ""
kode = -10                     &&  ‘…–”€€ !!!!!!!!!!!!!!!
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
SEEK kode
k_name = CUS_NAME
USE
*HIDE MENU (MENU())
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
  IF v # RIGHT(STR(1000+VAL(v),4),3)
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

IF CUS_CODE < 0
  USE
  sal_n = LEFT(sal_n,4)+"-"+ALLTRIM(SUBSTR(sal_n,5))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ชซ คญ ๏ "+sal_n+" แข๏ง ญ  แฎ แฏฅๆชซจฅญโฎฌ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

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

DEFINE PAD Return OF View_Inv PROMPT " งญฎฃซ แจ๏" KEY Ctrl-A, "Esc"
ON SELECTION PAD Return OF View_Inv DO Ch_Mode WITH "View_Inv", "lst"

fl01 = FSIZE("FIRM", "SALE01")+1         && ฎค ฏเฅคฏเจ๏โจ๏
fl11 = FSIZE("DOC_NUM", "SALE01")+1      && ฎฌฅเ คฎชใฌฅญโ  (+1 ฏฎค โจเฅ)
fl12 = IIF(SET("CENTURY")="ON", 10, 8)   &&  โ 
*fl13 = FSIZE("CUS_NAME", "SALE01")      &&  งข ญจฅ ญ ชซ คญฎฉ
fl14 = FSIZE("TOTAL_R", "SALE01")        && ‘ใฌฌ  ฏฎ ญ ชซ คญฎฉ

fl21 = FSIZE("PREFIX", "STO00")+ ;
       FSIZE("NAME", "STO00")+1          &&  จฌฅญฎข ญจฅ
fl22 = FSIZE("QNT", "SALE00")            && ฎซจ็ฅแโขฎ
fl23 = FSIZE("PRICE_R", "SALE00")        && –ฅญ 
fl24 = FSIZE("TOTAL_R", "SALE01")        && ‘ใฌฌ 

wd1 = fl01+fl11+fl12+fl14+3
wd2 = fl21+fl22+fl23+fl24+3
ln1 = WROWS("")-11
ln2 = WROWS("")-9

DIMENSION tt(1), ft(1)
tt(1) = PADC(" จฌฅญฎข ญจฅฤฤฤฤฤฤฤฤฤฤฤ", fl21+1,"ฤ")+ ;
        PADC("ฎซ-ขฎฤ", fl22+1,"ฤ")+                 ;
        PADC("ฤฤ–ฅญ ",  fl23+1,"ฤ")+                 ;
        PADC("ฤฤฤ‘ใฌฌ ",fl24,"ฤ")
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
        SALE00.QNT:H="",  ;
        SALE00.PRICE_R:H="",   ;
        f123 = SALE00.QNT*SALE00.PRICE_R:10:H="",   ;
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
wd = FSIZE("PREFIX","STO01")+FSIZE("NAME","STO01")+FSIZE("PRODUCER","STO01")+2
DO Def_Win WITH "Sto_M", "Sto_E", "Sto_I", ln, wd, "ฎฌฅญชซ โใเ "

what_do = "stock"

PRIVATE prf_flt, stock_f, prf_show
PRIVATE prf_wd, fill_chr, double, rc_flt, jmp, r_must
prf_wd = FSIZE("NAME","STO01")
seek_m = "seek"
jmp    = .F.
r_must = 0
fill_chr= "ฑ"

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
    prf_wd  = FSIZE("NAME","STO01")
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
                  STO01.NAME:H="",   ;
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
wd = FSIZE("PREFIX","STO01")+FSIZE("NAME","STO01")+1

DO Def_Win WITH "Seek_M", "Seek_E", "Seek_I", ln-4, wd, "๋กฅเจโฅ ฏฎงจๆจ๎"
SET ORDER TO 0
SELECT LINK
SET RELATION TO LNK INTO STO01
GO TOP
ACTIVATE WINDOW Seek_E
ON KEY LABEL Enter KEYBOARD CHR(23)
BROWSE FIELDS STO01.PREFIX:H="", ;
              STO01.NAME:H="",   ;
              DUMMY.F:H=""       ;
       FREEZE DUMMY.F ;
       NOMENU NORGRID NOMODIFY NOAPPEND NOCLEAR WINDOW Seek_I IN WINDOW Seek_E COLOR SCHEME 13
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
*บ               เฎๆฅคใเ  ไฎเฌจเฎข ญจ๏ แฏจแช  ญฅคฎแโ ็จ                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Rev_Lst
PARAMETERS rc_tit    && ฎฌฅเ ง ฏจแจ ข ไ ฉซฅ ง ฃฎซฎขชฎข ญ ชซ คญ๋ๅ

PRIVATE ln, wd, tit_m, tit_w, foot_w, doc_n, d_nam, doc_d, rem_val, nds_p ;
        cus_t, cus_c, cus_n, sum_d, sum_r, mss, c_sch, i001, i002, transm

IF rc_tit = 0
  RETURN
ENDIF

DELETE FILE (tmpo_path+"RASNO.DBF")
DELETE FILE (tmpo_path+"RASNO.CDX")
CREATE DBF (tmpo_path+"RASNO") ;
     ( CODE       N( 7),   ;
       PREFIX     C( 4),   ;
       NAME       C(35),   ;
       PRODUCER   C(8),    ;
       QNT_SAL    N( 6),   ;
       QNT_REV    N( 6),   ;
       PRICE_S    N(12,5), ;
       PRICE_S_D  N(10,4))
       
USE (tmpo_path+"RASNO") EXCLUSIVE alias Rev_tmp

SELECT 0
USE (base_path+"SALE_TIT") ALIAS SAL_T AGAIN
GO rc_tit
f_c001 = FIRM
doc_n = DOC_NUM
doc_d = DOC_DATE
cus_c = CUS_CODE
*nds_p = NDS_
*cus_t = NEW_TYPE
*cus_t = "0"
d_nam = CUS_NAME
cus_n = ""
sum_d = TOTAL_R
sum_r = 0
*transm = .NOT. EMPTY(TRANSMIT_C)
tit_m = f_c001+"/"+LEFT(doc_n,4)+"-"+ALLTRIM(SUBSTR(doc_n,5))+" ฎโ "+   ;
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
SEEK f_c001+LEFT(DTOS(doc_d),4)+doc_n

SCAN REST WHILE DOC_NUM = doc_n .AND. DOC_DATE = doc_d .AND. FIRM = f_c001
  SELECT REV_TMP
  APPEND BLANK
  REPLACE CODE       WITH SAL_REV.CODE,    ;
          PREFIX     WITH STO_REV.PREFIX,  ;
          NAME       WITH STO_REV.NAME,    ;
          PRODUCER   WITH STO_REV.PRODUCER,;
          QNT_SAL    WITH SAL_REV.QNT,     ;
          PRICE_S    WITH SAL_REV.PRICE_R, ;
          PRICE_S_D  WITH SAL_REV.PRICE_D
  SELECT SAL_REV
ENDSCAN
USE

SELECT REV_TMP
SET RELATION TO CODE INTO STO_REV
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ชซ คญ ๏ ฏใแโ ..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"RASNO.DBF")
  DELETE FILE (tmpo_path+"RASNO.DCX")
  RETURN
ENDIF

PRIVATE p1, p2, p3, p4, p5, p6, p7,p_sum
p1 = FSIZE("PREFIX")
p2 = FSIZE("NAME")
p3 = FSIZE("PRODUCER")
p4 = FSIZE("QNT_SAL")
p5 = FSIZE("QNT_REV")
p6 = FSIZE("PRICE_S")
p7 = FSIZE("QNT","STO_REV")

*p6 = 10

wd = p1+p2+p3+p4+p5+p6+p7+6
ln = WROWS("")-8
DIMENSION tit_w(2), foot_w(2)
tit_w(1) = ""
tit_w(2) = ;
 PADC(" จฌฅญฎข ญจฅฤฤฤฤฤฤฤฤ", p1+p2+2,"ฤ")+PADC("”จเฌ ฤฤฤ", p3+1,"ฤ")+ ;
      PADC("เฎค.", p4+1,"ฤ")+;
      PADC("ฤ ซจ็.", p7+1,"ฤ")+ ;
      PADC("ฤฎซจ็.", p5+1,"ฤ")+ ;
      PADC("ฤฤฤ–ฅญ ", p6,"ฤ")
foot_w(1) = ""
foot_w(2) = PADR("  "+cus_n,wd)
DO Def_Win WITH "Rev_M", "Rev_E", "Rev_I", ln, wd, tit_m, tit_w, foot_w
p_sum = WCOLS()-12
@ 1, 2 SAY "‘ใฌฌ  ฏฎ ญ ชซ คญฎฉ "+STR(sum_d,10,2)
@ 1, p_sum-16 SAY "‘ใฌฌ  ญฅคฎแโ ็จ"

c_sch = SCHEME(13)
i001 = AT(",", c_sch, 6)+1
i002 = AT(",", c_sch, 7)-i001
c_sch = STUFF(c_sch, i001, i002, SCHEME(14,2))

rem_val = 0

PRIVATE menu_name
menu_name = "Rev_Lst"

DEFINE MENU Rev_Lst IN SCREEN COLOR SCHEME 4

DEFINE PAD Cource OF Rev_Lst PROMPT "ใเแ"    KEY Ctrl-A, "F4"
ON SELECTION PAD Cource OF Rev_Lst DO Cource WITH f_c000, doc_n, doc_d

DEFINE PAD Exit  OF Rev_Lst PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit  OF Rev_Lst DO Exit_Rev
ACTIVATE SCREEN
@ 0, 0 SAY PADL("ฅญ๎ F10 ",WCOLS()) COLOR SCHEME 13
SHOW MENU Rev_Lst

ON KEY LABEL F10 ACTIVATE MENU Rev_Lst
ON KEY LABEL F4 DO Cource WITH f_c000, doc_n, doc_d
ON KEY LABEL LeftMouse DO M_Proc_1 WITH "Rev_Lst", "Rev_"

ACTIVATE WINDOW Rev_E
BROWSE FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="", QNT_SAL:H="",STO_REV.QNT:H="",;
              QNT_REV:H="":W=Sav_V():V=Tst_V():P="@Z 999999",                  ;
              PRICE_S:H="" ;
       FREEZE QNT_REV  ;
       NOMENU NORGRID NOAPPEND NODELETE NOCLEAR ;
       WINDOW Rev_I IN WINDOW Rev_E COLOR (c_sch)

ON KEY
DEACTIVATE MENU Rev_Lst 
RELEASE MENU Rev_Lst 
RELEASE WINDOW Rev_M
RELEASE WINDOW Rev_E
RELEASE WINDOW Rev_I
CALCULATE SUM(QNT_REV) TO rem_val

CLOSE DATABASES
IF rem_val # 0
  PRIVATE w_l, nt, to_print 
  w_l= 1
  nt = SPACE(16)
  to_print = .F.
  IF RASNO__()
    DO Exp_List
  ENDIF
  
ENDIF
CLOSE DATABASES
DELETE FILE (tmpo_path+"RASNO.DBF")
DELETE FILE (tmpo_path+"RASNO.CDX")

RETURN

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
  mss(2) = CHR(0)+"โฎ กฎซ์่ฅ, ็ฅฌ ก๋ซฎ ฏเฎค ญฎ ฏฎ ญ ชซ คญฎฉ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  ACTIVATE WINDOW (ALIAS())
  RETURN .F.
ENDIF

IF QNT_REV >STO_REV.QNT
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"“ ญ แ แโฎซ์ชฎ ญฅโ !!!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  ACTIVATE WINDOW (ALIAS())
  RETURN .F.
ENDIF


IF QNT_REV < 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅ็์ จคฅโ ฎ เ งญฎฃซ แจจ,   ญฅ ฎ ฏเฎค ฆฅ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  ACTIVATE WINDOW (ALIAS())
  RETURN .F.
ENDIF

IF QNT_REV # rem_val
  sum_r = sum_r+ROUND(QNT_REV*PRICE_S,2)-ROUND(rem_val*PRICE_S,2)
  ACTIVATE WINDOW Rev_M
  @ 1, p_sum SAY sum_r PICTURE "@Z 9999999.99"
  ACTIVATE WINDOW (ALIAS())
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
REPLACE ALL QNT_REV WITH QNT_SAL
GO rc_sav
sum_r = sum_d
ACTIVATE WINDOW Rev_M
@ 1, p_sum SAY sum_r PICTURE "@Z 9999999.99"
ACTIVATE WINDOW (ALIAS())
DEACTIVATE MENU Rev_Lst
RETURN


*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                            เฎค ฆ  แฏจแช .                             บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 04.09.97 14:36:39 อผ
PROCEDURE Sale_L

PRIVATE doc_n1, doc_s, doc_s_d, q_rest, q_exp, st_ord, delta, mss, q_pos,q_w, q_r
USE (base_path+"STOCK") 
*st_ord = "TAG CODE" $ SET("ORDER")
SET ORDER TO TAG CODE

SELECT 0
USE (base_path+"ACT_S") ORDER TAG SALE

SELECT 0
USE (base_path+"SPEC_SUM") ORDER TAG POS_CUS

SELECT 0
USE (base_path+"SALE")

SELECT 0
USE (tmpo_path+"RASNO")

*SEEK RECNO("CORR_TIT")
doc_n1 = ""
doc_s = 0
doc_s_d = 0
SCAN FOR  QNT_REV > 0
  = SEEK(CODE,"STOCK")
  q_rest = MIN(QNT_REV, STOCK.QNT)
  
  delta = QNT_REV-q_rest     && คฅแ์ แฎกจเ ฅฌ โฎ, ็โฎ ญฅคฎจงเ แๅฎคฎข ซจ!
  *
  *  ฎคกจเ ฅฌ ฏฎ แฅเจ๏ฌ
  *
  SELECT ACT_S
  SEEK STR(RASNO.CODE,7)
  q_pos = 0
  SCAN REST WHILE CODE = RASNO.CODE 
    q_w = MIN(QNT_REST, q_rest)
    q_r = MIN(REST_REP, q_w)
    REPLACE LAST_SALE WITH DATE(), ;
            QNT_REST  WITH QNT_REST-q_w, ;
            REST_REP WITH REST_REP-q_r
    IF EMPTY(doc_n1)   &&  คฎ ฏฎซใ็จโ์ ญฎฌฅเ คฎชใฌฅญโ !
      doc_n1 = Doc_Num("SALE", f_c000, arm, DATE())
      doc_n1 = sys_char+CHRTRAN(arm," ","0")+STR(doc_n1,6)
    ENDIF
    SELECT SALE
    APPEND BLANK
    REPLACE FIRM      WITH f_c000,        ;
            DOC_NUM   WITH doc_n1,        ;
            DOC_DATE  WITH DATE(),        ;
            CODE      WITH RASNO.CODE,    ;
            QNT       WITH q_w,           ;
            QNT_REP   WITH q_r,           ;
            PRICE_D_L WITH STOCK.PRICE,   ;
            PRICE_R_L WITH STOCK.PRICE_R, ;
            ACT_FIRM  WITH ACT_S.FIRM,    ;
            ACT_NUM   WITH ACT_S.DOC_NUM, ;
            ACT_DATE  WITH ACT_S.DOC_DATE,;
            PARTY     WITH ACT_S.PARTY,   ;
            GTD_NUM   WITH ACT_S.GTD_NUM, ;
            COUNTRY   WITH ACT_S.COUNTRY, ;
            COUNTRYC  WITH ACT_S.COUNTRYC,;
            GTD_LINE  WITH ACT_S.GTD_LINE,;
            WHERE     WITH arm
    q_pos = q_pos+QNT
    doc_s_d = doc_s_d+QNT*PRICE_D

    SELECT STOCK
    REPLACE QNT WITH QNT-q_w, QNT_SPEC WITH QNT_SPEC+q_w, QNT_REP WITH QNT_REP-q_r

    SELECT SPEC_SUM
    IF .NOT. SEEK(STR(STOCK.CODE,7)+STR(kode,8))
      DO New_Rec1 WITH STOCK.CODE, kode
    ENDIF
    REPLACE QNT WITH QNT+q_w
    IF QNT = 0
      REPLACE CODE WITH 0, CUS_CODE WITH 0
    ENDIF

    SELECT ACT_S
    q_rest = q_rest-q_w
    IF q_rest < 1
      EXIT
    ENDIF
  ENDSCAN
  doc_s = doc_s+ROUND(q_pos*SALE.PRICE_R,2)
  delta = delta+q_rest
  IF delta # 0
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"ฎงจๆจ๏ "+ALLTRIM(STOCK.PREFIX)+" "+  ;
                               ALLTRIM(STOCK.NAME)+" "+    ;
                               ALLTRIM(STOCK.PRODUCER)+":"
    mss(3) = CHR(0)+"ญฅ ๅข โ ฅโ "+ALLTRIM(STR(delta))+" ่โใช."
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
  SELECT RASNO
ENDSCAN

SELECT ACT_S
USE

SELECT SPEC_SUM
USE

SELECT SALE
USE

IF .NOT. EMPTY(doc_n1)
  USE (base_path+"SALE_TIT")
  APPEND BLANK
  REPLACE FIRM      WITH f_c000,        ;
          DOC_NUM   WITH doc_n1,        ;
          DOC_DATE  WITH DATE(),        ;
          DOC_TIME  WITH SECONDS(),     ;
          REQ_DOC   WITH doc_n,         ;
          REQ_DATE  WITH doc_d,         ;
          NEW_DOC   WITH f_c001,        ;
          CUS_CODE  WITH kode,          ;
          CUS_NAME  WITH k_name,        ;
          LIST_NUM  WITH "******",      ;
          NOTE      WITH nt,            ;
          WHO_LET   WITH w_l,           ;
          WHO       WITH user,          ;
          WHERE     WITH arm
ENDIF
USE

IF .NOT. EMPTY(doc_s)
  USE (base_path+"CUS_REST") ORDER TAG CUS_CODE
  IF .NOT. SEEK(STR(CORR_TIT.REAS_CODE)+CORR_TIT.FIRM)
    APPEND BLANK
    REPLACE CUS_CODE WITH CORR_TIT.REAS_CODE, ;
            FIRM     WITH CORR_TIT.FIRM
  ENDIF
  REPLACE ACC_SUM WITH ACC_SUM-doc_s
  USE
ENDIF

RETURN doc_n1

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                         เจๅฎค/เ แๅฎค แฏจแช .                          บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 04.09.97 19:01:06 อผ
PROCEDURE Exp_List

PRIVATE d_d, doc_i, doc_e, prn_i, prn_e, mss, f_v
*DO Wt_Mess WITH "”ฎเฌจเใฅฌ คฎชใฌฅญโ๋."

d_d = DATE()
*   doc_i = Inc_L()
doc_e = Sale_L()
IF .NOT. EMPTY(doc_e)
  DO SendToVK WITH f_c000, doc_e, DATE()  
  IF to_print
    DO Prn_Inv WITH f_c000, doc_e, DATE()  
  ENDIF  
ENDIF

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ              ฎก ขซฅญจฅ ง ฏจแจ ข ฌฅคซฅญญฎ เ แโใ้จฉ ไ ฉซ.               บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 06.04.98 13:59:08 อผ
PROCEDURE New_Rec1
PARAMETERS s_c, c_c

PRIVATE arr_w, to_add

SCATTER TO arr_w BLANK
arr_w(1) = s_c
arr_w(2) = c_c

to_add = .T.

DO WHILE SEEK("      0       0")
  IF LOCK()
    to_add = .F.
    EXIT
  ENDIF
ENDDO

IF to_add
  APPEND FROM ARRAY arr_w
ELSE
  GATHER FROM arr_w
ENDIF

UNLOCK

RETURN

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  Tst_F         งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                            ๋ๅฎค จง กซ ญช .                            ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 09.03.1999 ฤฤู
PROCEDURE Tst_F

PRIVATE mss
IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"๋ ญฅ ใช ง ซจ ฏเฅคฏเจ๏โจฅ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

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
