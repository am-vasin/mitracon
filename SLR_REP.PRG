*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Slr_Rep       งเ กฎโ็จช €ญคเฅฉ  แจญ           06.11.97 10:29:18 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                          ‘ฏเ ขช  ฏฎ ง เฏซ โฅ.                          บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Slr_Rep
PRIVATE usd_h, usd_r, usd_d, rub_r, kurs_w, is_prev

DIMENSION usd_h(2), usd_r(2), usd_d(2), rub_r(2)
kurs_w = kurs_mmvb

DO Prep_V

is_prev = 1
DO WHILE .T.
  IF View_Slr()
    is_prev = IIF(is_prev=1, 2, 1)
  ELSE
    EXIT
  ENDIF
ENDDO

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ฎคฃฎโฎขช  ค ญญ๋ๅ.                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 06.11.97 10:29:42 อผ
PROCEDURE Prep_V
PRIVATE w_h, w_r, w_d

usd_h = 0
usd_r = 0
usd_d = 0
rub_r = 0

USE (base_path+"PS_STAT")
SCAN
  *
  *   เฏซ โ 
  *
  w_h = Crpt_2(REST)
  w_h = MAX(w_h,0)
  IF PAY_VAL = "$"
    w_d = Crpt_2(PAY_SUM)
    w_d = MIN(w_d, w_h)
    w_r = w_h-w_d
  ELSE
    w_r = Crpt_2(PAY_SUM)
    w_r = MIN(w_r, w_h)
    w_d = w_h-w_r
  ENDIF
  usd_h(2) = usd_h(2)+w_h
  usd_r(2) = usd_r(2)+w_r
  usd_d(2) = usd_d(2)+w_d
  rub_r(2) = rub_r(2)+ROUND(w_r*kurs_w,2)
  
  *
  *  €ข ญแ
  *
  w_h = Crpt_2(PREV_SUM)
  w_d = Crpt_2(PREV_SUMD)
  w_r = w_h-w_d
  usd_h(1) = usd_h(1)+w_h
  usd_r(1) = usd_r(1)+w_r
  usd_d(1) = usd_d(1)+w_d
  rub_r(1) = rub_r(1)+ROUND(w_r*kurs_w,2)
ENDSCAN

USE

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                         เฎแฌฎโเ เฅงใซ์โ โฎข.                          บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 06.11.97 10:44:47 อผ
PROCEDURE View_Slr

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

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ          แฅฃฎ 9999999.99           ณ
*ณ                                     ณ
*ณ  เใกซ๏ๅ 9999999.99 ฏฎ ชใเแใ 99.999 ณ
*ณ ฺฤ โฎฃฎ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ  9999999.99 $   9999999999 เใก  ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ< ฅเฅชซ. Ctrl-W > < โช ง โ์แ๏ Esc >ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE w_h, w_r, w_d, r_r, w_k
PRIVATE ex
ex = 1
w_k = kurs_w
w_h = usd_h(is_prev)
w_r = usd_r(is_prev)
w_d = usd_d(is_prev)
r_r = rub_r(is_prev)

*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 10, 41, IIF(is_prev=1, "€ข ญแ", " เฏซ โ ")
DO Sun_Bord WITH 5, 3, 7, 37, " โฎฃฎ "
@ 6, 32 SAY "เใก"
*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@ 2, 12 SAY "แฅฃฎ" GET w_h PICTURE "@Z 9999999.99" VALID Recalc()
@ 4,  3 SAY " เใกซ๏ๅ" GET w_r PICTURE "@Z  9999999.99" VALID Recalc()
@ 4, 23 SAY "ฏฎ ชใเแใ" GET w_k PICTURE "@Z  99.999" VALID Recalc()
@ 6,  6 GET w_d PICTURE "@Z 9999999.99" WHEN .F.
@ 6, 17 SAY "$  " GET r_r PICTURE "@Z 9999999.99" WHEN .F.
@ 8,  2 GET ex PICTURE "@*HT \! ฅเฅชซ. Ctrl-W ;\? โช ง โ์แ๏ Esc "
READ CYCLE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                               ฅเฅแ็ฅโ.                                บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 06.11.97 11:23:02 อผ
PROCEDURE Recalc

w_d = w_h-w_r
r_r = ROUND(w_r*w_k,2)
SHOW GET w_d
SHOW GET r_r

RETURN .T.
