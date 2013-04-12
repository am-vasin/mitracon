*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ New_Ver       งเ กฎโ็จช €ญคเฅฉ  แจญ           06.02.98 14:13:53 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ               “แโ ญฎขช  ญฎขฎฉ ขฅเแจจ ฏเฎฃเ ฌฌ๋ ฏเฎค ฆจ.                บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE New_Ver

PRIVATE wk_dir, src_dir, n_old, n_new, ex, max_sw, f_name

max_sw = 5

wk_dir = PADR(SET("DEFAULT")+CURDIR(),60)
n_old = Flt_Sw(ALLTRIM(wk_dir))

src_dir = PADR("D:\VASIN\SALE\SALE\",60)
n_new  = IIF(n_old > 4, 1, n_old+1)

ex = 1

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

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤ คฅ ญ ๅฎคจโแ๏ ญฎข ๏ ขฅเแจ๏ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ ....:....!....:....!....:....!....:....!....:....!....:....! ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ ฺฤ ใค  ใแโ ญ ขซจข ฅฌ ญฎขใ๎ ขฅเแจ๎ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ ....:....!....:....!....:....!....:....!....:....!....:....! ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ               ’ฅชใ้ฅฅ ฏฎซฎฆฅญจฅ ฏฅเฅชซ๎็ โฅซ๏ 99                 ณ
*ณ                                                                  ณ
*ณ               ฎขฎฅ ฏฎซฎฆฅญจฅ ฏฅเฅชซ๎็ โฅซ๏   99                 ณ
*ณ                                                                  ณ
*ณ                 < OK Ctrl-W > < โช ง โ์แ๏ Esc >                 ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 14, 70, "“แโ ญฎขช  ญฎขฎฉ ขฅเแจจ ฏเฎฃเ ฌฌ๋ €"
DO Sun_Bord WITH 2, 3, 4, 66, " คฅ ญ ๅฎคจโแ๏ ญฎข ๏ ขฅเแจ๏ "
DO Sun_Bord WITH 5, 3, 7, 66, " ใค  ใแโ ญ ขซจข ฅฌ ญฎขใ๎ ขฅเแจ๎ "
*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@  3,  5 GET src_dir VALID Tst_Dir1()
@  6,  5 GET wk_dir  VALID Tst_Dir2()
@  8, 17 SAY "’ฅชใ้ฅฅ ฏฎซฎฆฅญจฅ ฏฅเฅชซ๎็ โฅซ๏" GET n_old ; 
              PICTURE "@Z 99" WHEN .F.
@ 10, 17 SAY "ฎขฎฅ ฏฎซฎฆฅญจฅ ฏฅเฅชซ๎็ โฅซ๏  " GET n_new ;
              PICTURE "@Z 99" VALID Tst_Sw()

@ 12, 19 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE VALID Tst_Form()

IF ex = 1
  f_name = "SALE"+ALLTRIM(STR(n_new))+".APP"
  DELETE FILE (ALLTRIM(wk_dir)+f_name)
  COPY FILE (ALLTRIM(src_dir)+"SALE.APP") TO (ALLTRIM(wk_dir)+f_name)
  FOR i = 1 TO max_sw
    DELETE FILE (ALLTRIM(wk_dir)+ALLTRIM(STR(i))+".SW")
  ENDFOR
  CREATE DBF (ALLTRIM(wk_dir)+ALLTRIM(STR(n_new))+".SW") (FFF C(1))
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      เฎขฅเช  ฏใโจ แ APP-ไ ฉซฎฌ.                       บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 06.02.98 14:39:33 อผ
PROCEDURE Tst_Dir1
PRIVATE mss

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF .NOT. File_O(ALLTRIM(src_dir)+"SALE.APP")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ใช ง ญญฎฉ คจเฅชโฎเจจ ญฅโ ไ ฉซ  ฏเจซฎฆฅญจ๏!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                        เฎขฅเช  เ กฎ็ฅฃฎ ฏใโจ.                         บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 06.02.98 14:39:33 อผ
PROCEDURE Tst_Dir2
PRIVATE mss

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF .NOT. Is_Dir(ALLTRIM(wk_dir))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"’ ชฎฉ คจเฅชโฎเจจ ขฎขแฅ ญฅ แใ้ฅแโขใฅโ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

n_old = Flt_Sw(ALLTRIM(wk_dir))
n_new  = IIF(n_old > 4, 1, n_old+1)

SHOW GET n_old

SHOW GET n_new

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                     เฎขฅเช  ญฎฌฅเ  ญฎขฎฉ ขฅเแจจ.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 06.02.98 14:39:33 อผ
PROCEDURE Tst_Sw
PRIVATE mss

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF .NOT. BETWEEN(n_new, 1, max_sw)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฎฌฅเ ขฅเแจจ คฎซฆฅญ ก๋โ์ ฎโ 1 คฎ "+  ;
           ALLTRIM(STR(max_sw))+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF n_new = n_old
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฎฌฅเ ขฅเแจจ คฎซฆฅญ ฎโซจ็ โ์แ๏ ฎโ โฅชใ้ฅฃฎ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                ฏเฅคฅซ๏ฅฌ โฅชใ้ฅฅ ฏฎซฎฆฅญจฅ ใช ง โฅซ๏.                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 06.02.98 14:23:08 อผ
PROCEDURE Flt_Sw
PARAMETER pth
PRIVATE i, v

v = 0
FOR i = 1 TO max_sw
  IF File_O(ALLTRIM(pth)+ALLTRIM(STR(i))+".SW")
    v = i
    EXIT
  ENDIF
ENDFOR

RETURN v

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                        เฎขฅเช  ไฎเฌ๋ ข ๆฅซฎฌ.                         บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 06.02.98 14:39:33 อผ
PROCEDURE Tst_Form
PRIVATE mss

IF .NOT. Tst_Sw()
  RETURN .F.
ENDIF

IF .NOT. Tst_Dir1()
  RETURN .F.
ENDIF

RETURN .T.
