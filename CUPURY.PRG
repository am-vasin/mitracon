*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Cupury        งเ กฎโ็จช €ญคเฅฉ  แจญ           24.12.97 13:48:45 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                             ‘็ฅโ็จช ชใฏ๎เ.                             บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cupury
PARAMETERS valuta, val_list, mov_l, mov_c
*
* valuta   - ข ซ๎โ  ( "$" - คฎซซ เ๋, ข ซ๎กฎฌ คเใฃฎฌ แซใ็ ฅ - เใกซจ );
* val_list - แฏจแฎช ชฎซจ็ฅแโข;
* mov_l   - แคขจฃ ฎชญ  ฏฎ ขฅเโจช ซจ;
* mov_c   - แคขจฃ ฎชญ  ฏฎ ฃฎเจงฎญโ ซจ.
*
PRIVATE IsUSD, DeltaL, DeltaC

IsUSD = ""
IF TYPE("valuta") = "C"
  IsUSD = IIF(valuta = "$", "$", "")
ENDIF

IF IsUSD = "$"
  DO Prep_Cup WITH "$"
ELSE
  DO Prep_Cup WITH ""
ENDIF
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

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤฤ ใฏ๎เ  ฤฤฤฤฤ ฎซ-ขฎ ฤฟ ณ
*ณ ณ  000000000000  000000  ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ    โฎฃฎ  999 999 999.99   ณ
*ณ< OK ^W > < โช ง โ์แ๏ Esc >ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE ex, res_sum, ln_arr, tit, det_n, i, mov1, mov2
*
*     งฌฅเ ฏฎคขจฆชจ ฎชญ 
*
mov1 = 0
mov2 = 0
IF TYPE("mov_l") = "N"
  mov1 = mov_l
ENDIF
IF TYPE("mov_c") = "N"
  mov2 = mov_c
ENDIF

ex      = 1
res_sum = 0
*     ฃฎซฎขฎช ฎชญ 
tit     = IIF(IsUSD = "$", "ฎซซ เ๋", "ใกซจ")
ln_arr  = IIF(IsUSD = "$", ALEN(CupUsdN), ALEN(CupRubN))
DIMENSION det_n(ln_arr)
det_n = 0
i = "VAL_LIST(" + ALLTRIM(STR(ln_arr)) + ")"
IF TYPE(i) = "N"
  FOR i = 1 TO ln_arr
    ln = val_list(i)
    det_n(i) = IIF(TYPE("ln") = "N", MAX(ln,0), 0)
    res_sum = res_sum + IIF(IsUSD = "$", CupUsdW(i), CupRubW(i))*det_n(i)
  ENDFOR
ENDIF
ln = ln_arr+7

*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH ln, 32, tit
MOVE WINDOW (win_name) BY mov1, mov2
DO Sun_Bord WITH 2, 3, 3+ln_arr, 28
@ 2, 6 SAY " ใฏ๎เ  "
@ 2,19 SAY " ฎซ-ขฎ "
@ WROWS()-3, 6 SAY "โฎฃฎ"
*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
FOR i = 1 TO ln_arr
  @ 2+i, 6 SAY IIF(IsUSD = "$", CupUsdN(i), CupRubN(i))+" "  ;
           GET det_n(i) PICTURE "@Z 999999" VALID Calc_V() WHEN Fill_W()
ENDFOR
@ WROWS()-2, 2 GET ex PICTURE "@*HT \! OK ^W ;\? โช ง โ์แ๏ Esc " ;
                              WHEN Fill_W()

DO Fill_W
DO Calc_V

READ CYCLE

IF ex = 1
  IF TYPE("val_list") # "U"
    DIMENSION val_list(ln_arr)
    FOR i = 1 TO ln_arr
      val_list(i) = det_n(i)
    ENDFOR
  ENDIF
ELSE
  IF TYPE("val_list") # "U"
    DIMENSION val_list(ln_arr)
    FOR i = 1 TO ln_arr
      val_list(i) = 0
    ENDFOR
  ENDIF
  res_sum = 0
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN res_sum

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                 ฎคฃฎโฎขช  ฌ แแจขฎข แ ฎฏจแ ญจฅฌ ชใฏ๎เ.                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 24.12.97 14:05:14 อผ
PROCEDURE Prep_Cup
PARAMETER CType  && ’จฏ ชใฏ๎เ

PRIVATE sel_sav, i

IF CType = "$"
  IF TYPE("CupUsdN") = "U"    && ฅโ ฎฏจแ ญจ๏ ชใฏ๎เ - แฎงค โ์!
    PUBLIC CupUsdN, CupUsdW, CupUsdC
    sel_sav = SELECT()
    SELECT 0
    USE (base_path+"CUPURY") ORDER TAG VISUAL
    i = 0
    SEEK CType
    SCAN REST WHILE CUPURY.VALUTA = CType
      i = i+1
      DIMENSION CupUsdN(i), CupUsdW(i), CupUsdC(i)
      CupUsdN(i) = CUPURY.NAME
      CupUsdW(i) = CUPURY.WAIGHT
      CupUsdC(i) = CUPURY.CODE
    ENDSCAN
    USE
    SELECT (sel_sav)
  ENDIF
ELSE
  IF TYPE("CupRubN") = "U"    && ฅโ ฎฏจแ ญจ๏ ชใฏ๎เ - แฎงค โ์!
    PUBLIC CupRubN, CupRubW, CupRubC
    sel_sav = SELECT()
    SELECT 0
    USE (base_path+"CUPURY") ORDER TAG VISUAL
    i = 0
    SEEK CType
    SCAN REST WHILE CUPURY.VALUTA = CType
      i = i+1
      DIMENSION CupRubN(i), CupRubW(i), CupRubC(i)
      CupRubN(i) = CUPURY.NAME
      CupRubW(i) = CUPURY.WAIGHT
      CupRubC(i) = CUPURY.CODE
    ENDSCAN
    USE
    SELECT (sel_sav)
  ENDIF
ENDIF

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                              แ็ฅโ แใฌฌ๋.                              บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 24.12.97 16:29:24 อผ
PROCEDURE Calc_V
PRIVATE i
res_sum = 0

IF IsUSD = "$"
  FOR i = 1 TO ln_arr
    res_sum = res_sum+CupUsdW(i)*det_n(i)
  ENDFOR
ELSE
  FOR i = 1 TO ln_arr
    res_sum = res_sum+CupRubW(i)*det_n(i)
  ENDFOR
ENDIF

@ WROWS()-3, 13 SAY res_sum PICTURE "999 999 999.99"

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                              แชเ แช  ฎชญ .                            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 24.12.97 16:29:24 อผ
PROCEDURE Fill_W

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤฤ ใฏ๎เ  ฤฤฤฤฤ ฎซ-ขฎ ฤฟ ณ
*ณ ณ  000000000000  000000  ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ    โฎฃฎ  999 999 999.99   ณ
*ณ< OK ^W > < โช ง โ์แ๏ Esc >ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
@ 3, 4 FILL TO 2+ln_arr, 27 COLOR SCHEME 13

RETURN .T.
