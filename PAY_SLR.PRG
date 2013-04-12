*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Pay_Slr       งเ กฎโ็จช €ญคเฅฉ  แจญ           31.10.97 11:52:09 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                           ๋ฏซ โ  ง เฏซ โ๋.                            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Pay_Slr
PARAMETERS ps_code, pay_type, ye_p, mn_p, total_pay

PRIVATE sel_sav, ye, mn, mss, h_usd, r_usd, r_rub, kurs_w
PRIVATE tmpPay
PRIVATE q_usd, q_rub
DIMENSION q_usd(1), q_rub(1)
tmpPay = .F.

q_rub = 0
q_usd = 0

h_usd  = 0
r_usd  = 0
r_rub  = 0
kurs_w = kurs_b

sel_sav = SELECT()

SELECT 0
USE (base_path+"MOULD") ORDER TAG MONTH ALIAS MLD_PAY AGAIN
  
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS PS_PAY AGAIN
SEEK ps_code
  
SELECT 0
USE (base_path+"PS_STAT") ORDER TAG CODE ALIAS STT_PAY AGAIN
IF .NOT. SEEK(ps_code)
  h_usd = Crpt_1(0,10)
  r_usd = Crpt_1(0,5)
  APPEND BLANK
  REPLACE CODE      WITH ps_code,   ;
          START     WITH h_usd,     ;
          REST      WITH h_usd,     ;
          SALARY    WITH PS_PAY.SALARY, ;
          PREV_SUM  WITH r_usd,     ;
          PREV_SUMD WITH r_usd,     ;
          PAY_SUM   WITH r_usd,     ;
          PAY_VAL   WITH "",       ;
          S_DATE    WITH PS_PAY.DATE_IN
  h_usd = 0
  r_usd = 0
ENDIF

IF TYPE("ye_p") = "N"
  ye = ye_p
ELSE
  ye = YEAR(DATE())
ENDIF

IF TYPE("mn_p") = "N"
  mn = mn_p
ELSE
  mn = MONTH(DATE())
ENDIF

DO CASE
CASE pay_type = "1"
  DO S_Pay_1 WITH ps_code
CASE pay_type = "2"
  DO S_Pay_2 WITH ps_code, ye, mn
ENDCASE

SELECT PS_PAY
USE
SELECT STT_PAY
USE
SELECT MLD_PAY
USE
SELECT (sel_sav)
RETURN tmpPay

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ๋ฏซ โ   ข ญแ .                              บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 29.10.97 19:03:23 อผ
PROCEDURE S_Pay_1
PARAMETERS ps_code

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
*                   ซจฅญโ
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ                                           ณ
*ณ         ‘ใฌฌ  ช ฎฏซ โฅ 99999.99$          ณ
*ณ                                           ณ
*ณ ง ญจๅ ข เใกซ๏ๅ 99999.99$ ฏฎ ชใเแใ 99.999 ณ
*ณ                                           ณ
*ณ     โฎฃฎ 99999.99$   9999999.99 เใก.     ณ
*ณ                                           ณ
*ณ ใฏ๎เ๋:  USD:  999999   ใกซจ: 9999999.99 ณ
*ณ                                           ณ
*ณ    เจฌฅ็ ญจฅ ....:....!....:....!....    ณ
*ณ ฺฤ[ ] ฅ็ โ์ จแโฎเจจ ญ ็จแซฅญจฉฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ      ‘ .. ฏฎ ..       ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ     < OK Ctrl-W > < โช ง โ์แ๏ Esc >      ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
*ณ                < OK >                     ณ
PRIVATE ex, nt, w_usd, inv, to_print, dt0, dt1, tmpBuff
nt = SPACE(24)
ex   = 1
inv  = 1
IF TYPE("total_pay") # "N"
  h_usd  = Crpt_2(STT_PAY.PREV_SUM)
ELSE
  h_usd  = total_pay
ENDIF
r_usd  = Crpt_2(STT_PAY.PREV_SUMD)
r_rub  = ROUND(r_usd*kurs_w,2)
w_usd  = h_usd-r_usd
to_print = .F.
dt0    = DATE()-60
dt1    = DATE()
tmpBuff = Get_Sys("SlrDate_1")
IF .NOT. EMPTY(tmpBuff)
  dt0 = CTOD(ALLTRIM(LEFT(tmpBuff,10)))
  dt1 = CTOD(ALLTRIM(SUBSTR(tmpBuff,11)))
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 17, 47, ALLTRIM(PS_PAY.FAMILY)+" "+  ;
              LEFT(PS_PAY.NAME,1)+". "+LEFT(PS_PAY.S_NAME,1)+"."
@ 7, 35 SAY "เใก"
@ 9, 3 SAY "ใฏ๎เ๋:  USD:           ใกซจ:"
DO Sun_Bord WITH WROWS()-5, 3, WROWS()-3, 43
*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@  9, 11 GET inv PICTURE "@*IH ;" SIZE 1, 6, 10 VALID Clc_C(9,34,18)
@ 11,  6 SAY "เจฌฅ็ ญจฅ" GET nt
@ WROWS()-5, 5 GET to_print PICTURE "@*C ฅ็ โ์ จแโฎเจจ ญ ็จแซฅญจฉ"
@ WROWS()-4, 10 SAY "‘" GET dt0 PICTURE "@D" WHEN to_print
@ WROWS()-4, 23 SAY "ฏฎ" GET dt1 PICTURE "@D" WHEN to_print
IF TYPE("total_pay") # "N"
  @ WROWS()-2,  7 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
ELSE
  @ WROWS()-2,  FLOOR(WCOLS()/2)-3 GET ex PICTURE "@*HT \ OK "
ENDIF
@  3, 11 SAY "‘ใฌฌ  ช ฎฏซ โฅ" GET h_usd PICTURE "@Z 99999.99" VALID Clc_E() ;
                                            WHEN TYPE("total_pay") # "N"
@  5,  3 SAY "ง ญจๅ ข เใกซ๏ๅ" GET r_usd PICTURE "@Z 99999.99" VALID Clc_E()
@  5, 27 SAY "$ ฏฎ ชใเแใ" GET kurs_w PICTURE "@Z 99.999" VALID Clc_E()
@  7,  7 SAY "โฎฃฎ" GET w_usd PICTURE "@Z 99999.99" WHEN .F.
@  7, 21 SAY "$  " GET r_rub PICTURE "@Z 9999999.99" WHEN .F.

READ CYCLE

IF ex = 1 .AND. (h_usd # 0 .OR. r_usd # 0)
  DO Bd_Pay WITH STT_PAY.CODE, h_usd, r_usd, kurs_w, nt, r_usd, kurs_w
  tmpPay = .T.
ENDIF
IF to_print .AND. ex = 1
  DO Per1_Sal WITH STT_PAY.CODE, dt0, dt1
ENDIF
DO Put_Sys WITH "SlrDate_1", PADR(DTOC(dt0),10)+DTOC(dt1)

*--------------------------------------------------------------------------

ON KEY
POP KEY
RELEASE WINDOW (win_name)
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ฅเฅแ็ฅโ งญ ็ฅญจฉ.                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 31.10.97 17:14:01 อผ
PROCEDURE Clc_E

r_rub  = ROUND(r_usd*kurs_w,2)
w_usd  = h_usd-r_usd

SHOW GET r_rub
SHOW GET w_usd

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                             ฎคแ็ฅโ ชใฏ๎เ.                             บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 31.10.97 18:04:49 อผ
PROCEDURE Clc_C
PARAMETERS rw, cl_r, cl_d
PRIVATE v

IF inv = 1
  v = Cupury("$", q_usd, 0, 23)
  @ rw, cl_d SAY TRANSFORM(v, "@Z 999999")
ELSE
  v = Cupury(" ", q_rub, 0, -23)
  @ rw, cl_r SAY TRANSFORM(v, "@Z 9999999.99")
ENDIF

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ๋ฏซ โ  ง เฏซ โ๋.                            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 29.10.97 19:03:23 อผ
PROCEDURE S_Pay_2
PARAMETERS ps_code, ye, mn

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

*             < ’จฏ ญ ็จแซฅญจ๏ >
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณฺฤ ชซ๎็จโ์ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟณ
*ณณ               ฺฤฤฤฤฤฤฤฤฤฤฟฺฤฤฤฤฤฤฟ               ณณ
*ณณ ฑฑฑฑฑ คญฅฉ ง  ณ ....:... ณณ 1234 ณ ฏฎ ฑฑฑฑฑฑฑ.ฑฑ ณณ
*ณณ               ภฤฤฤฤฤฤฤฤฤฤูภฤฤฤฤฤฤู               ณณ
*ณณ                                 แฅฃฎ ฑฑฑฑฑฑฑ.ฑฑ ณณ
*ณภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤูณ
*ณ แโ. 999999.99  ฅเฅๅ. 999999.99 โฎฃฎ 9999999.99  ณ
*ณ                        โฎฃฎ ข๋ฏซ โจโ์ ฑฑฑฑฑฑฑ.ฑฑ  ณ
*ณ                                                    ณ
*ณ    ง ญจๅ ข เใกซ๏ๅ ฑฑฑฑฑฑฑ.ฑฑ $ ฏฎ ชใเแใ ฑฑฑฑฑฑ    ณ
*ณ                                                    ณ
*ณ        โฎฃฎ 9999999.99 $   9999999.99 เใก.        ณ
*ณ                                                    ณ
*ณ     ใฏ๎เ๋:  USD:  999999   ใกซจ: 9999999.99      ณ
*ณ                                                    ณ
*ณ        เจฌฅ็ ญจฅ ....:....!....:....!....         ณ
*ณ     ฺฤ[ ] ฅ็ โ์ จแโฎเจจ ญ ็จแซฅญจฉฤฤฤฤฤฤฤฤฤฟ      ณ
*ณ     ณ      ‘ .. ฏฎ ..       ณ      ณ
*ณ     ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู      ณ
*ณ          < OK Ctrl-W > < โช ง โ์แ๏ Esc >          ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู
PRIVATE ex, nt, w_usd, inv, rst
PRIVATE ex, mn_list, ye_list, i, ye_numb, ye_w, mn_w, total, prc, q
PRIVATE rst_0, rst_sv, to_print, dt0, dt1, tmpBuff

to_print = .F.
dt0    = DATE()-60
dt1    = DATE()
tmpBuff = Get_Sys("SlrDate_2")
IF .NOT. EMPTY(tmpBuff)
  dt0 = CTOD(ALLTRIM(LEFT(tmpBuff,10)))
  dt1 = CTOD(ALLTRIM(SUBSTR(tmpBuff,11)))
ENDIF
ex   = 1
inv  = 1
rst_0 = Crpt_2(STT_PAY.REST)
rst_sv = Crpt_2(STT_PAY.SAV_REST)
h_usd  = rst_0-rst_sv
rst    = h_usd
IF STT_PAY.PAY_VAL = "$"
  w_usd = Crpt_2(STT_PAY.PAY_SUM)
  r_usd = h_usd-w_usd
ELSE
  r_usd = Crpt_2(STT_PAY.PAY_SUM)
  w_usd = h_usd-r_usd
ENDIF
r_rub  = ROUND(r_usd*kurs_w,2)
ye_numb = MIN(YEAR(DATE())-2, ye)
ex      = MAX(YEAR(DATE())+1, ye)
DIMENSION ye_list(ex-ye_numb+1)
FOR i = 1 TO ex-ye_numb+1
  ye_list(i) = STR(ye_numb+i-1,4)
ENDFOR
nt = SPACE(24)
total = 0
q     = 0
DIMENSION mn_list(12)
FOR i = 1 TO 12
  mn_list(i) = Month_R(i)
ENDFOR
mn_w = mn
ye_w = ASCAN(ye_list, STR(ye,4))

prc   = Set_Pr0()
total= 0
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 23, 56, ALLTRIM(PS_PAY.FAMILY)+" "+  ;
              LEFT(PS_PAY.NAME,1)+". "+LEFT(PS_PAY.S_NAME,1)+"."
DO Sun_Bord WITH 2, 2, 7, 53, " ชซ๎็จโ์ "
DO Sun_Bord WITH WROWS()-5, 3, WROWS()-3, 52
@  4, 10 SAY "คญฅฉ ง "
@  8,  3 SAY "แโ. "+TRANSFORM(rst_0, "@Z 999999.99")
@  8, 19 SAY "ฅเฅๅ."+TRANSFORM(rst_sv, "@Z 999999.99")
@  8, 36 SAY "โฎฃฎ "+TRANSFORM(rst, "@Z 9999999.99")
@ 15,  7 SAY "ใฏ๎เ๋:  USD:           ใกซจ:"
@ 13, 42 SAY "เใก."

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@ 15, 15 GET inv PICTURE "@*IH ;" SIZE 1, 6, 10 VALID Clc_C(15,38,22)
@ 17, 10 SAY "เจฌฅ็ ญจฅ" GET nt
@ WROWS()-5, 5 GET to_print PICTURE "@*C ฅ็ โ์ จแโฎเจจ ญ ็จแซฅญจฉ"
@ WROWS()-4, 14 SAY "‘" GET dt0 PICTURE "@D" WHEN to_print
@ WROWS()-4, 27 SAY "ฏฎ" GET dt1 PICTURE "@D" WHEN to_print
@ WROWS()-2, 12 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
@  4,  4 GET q PICTURE "@Z 99999" VALID Set_Val(1)
@  3, 18 GET mn_w PICTURE "@^ " FROM mn_list SIZE 1, 12 VALID Set_Val(2)
@  3, 30 GET ye_w PICTURE "@^ " FROM ye_list SIZE 1, 8 VALID Set_Val(2)
@  4, 39 SAY "ฏฎ" GET prc PICTURE "@Z 9999999.99" VALID Set_Val(3)
@  6, 36 SAY "แฅฃฎ" GET total PICTURE "@Z 9999999.99" VALID Set_Val(4)
@  9, 26 SAY "โฎฃฎ ข๋ฏซ โจโ์" GET h_usd PICTURE "@Z 9999999.99" VALID Set_Val(5)
@ 11,  6 SAY "ง ญจๅ ข เใกซ๏ๅ" GET r_usd PICTURE "@Z 9999999.99" VALID Set_Val(6)
@ 11, 33 SAY "$ ฏฎ ชใเแใ" GET kurs_w PICTURE "@Z 99.999" VALID Set_Val(7)
@ 13, 10 SAY "โฎฃฎ" GET w_usd PICTURE "@Z 9999999.99" WHEN .F.
@ 13, 27 SAY "$  " GET r_rub PICTURE "@Z 9999999.99" WHEN .F.

READ CYCLE

IF ex = 1 .AND. h_usd # 0
  DO Bd_Pay WITH STT_PAY.CODE, h_usd, r_usd, kurs_w, nt, r_usd, kurs_w
  tmpPay = .T.
ENDIF
IF to_print .AND. ex = 1
  DO Per1_Sal WITH STT_PAY.CODE, dt0, dt1
ENDIF
DO Put_Sys WITH "SlrDate_2", PADR(DTOC(dt0),10)+DTOC(dt1)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                   แ็ฅโ ๆฅญ๋ ญ  ฎแญฎข ญจจ ช ซฅญค เ๏.                   บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 30.10.97 09:30:02 อผ
PROCEDURE Set_Pr0

PRIVATE sw, wg, d

d = 0
sw = .T.
IF .NOT. SEEK(ye_list(ye_w)+STR(mn_w,2), "MLD_PAY")
  sw = .F.
ELSE
  IF RECNO("MLD_PAY") = RECCOUNT("MLD_PAY")
    sw = .F.
  ELSE
    d = OCCURS("", MLD_PAY.DAYS)
  ENDIF
ENDIF

IF sw
  wg = ROUND(Crpt_2(STT_PAY.SALARY)/d,2)
ELSE
  wg = 0
  WAIT WINDOW "€…!  ซฅญค เ์ ง  "+Month_R(mn_w)+" "+ye_list(ye_w)+ ;
              " ญฅ ใโขฅเฆคฅญ จซจ ญฅ แใ้ฅแโขใฅโ!"
ENDIF

RETURN wg

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ฅเฅแ็ฅโ งญ ็ฅญจฉ.                           บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 03.11.97 16:06:05 อผ
PROCEDURE Set_Val
PARAMETER mode

IF mode = 2
  prc = Set_Pr0()
  SHOW GET prc
ENDIF

IF mode < 4
  total = q*prc
  SHOW GET total
ENDIF
  
IF mode < 5
  h_usd = total+rst
  SHOW GET h_usd
ENDIF

IF mode < 6
  IF STT_PAY.PAY_VAL = "$"
    w_usd = Crpt_2(STT_PAY.PAY_SUM)
    r_usd = h_usd-w_usd
  ELSE
    r_usd = Crpt_2(STT_PAY.PAY_SUM)
    w_usd = h_usd-r_usd
  ENDIF
  SHOW GET w_usd
  SHOW GET r_usd
ENDIF

IF mode < 7
  w_usd = h_usd-r_usd
  SHOW GET w_usd
ENDIF

r_rub = ROUND(r_usd*kurs_w,2)
SHOW GET r_rub

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                       โเ กฎโช  ข ก ง ๅ ค ญญ๋ๅ.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 04.11.97 11:46:15 อผ
PROCEDURE Bd_Pay
PARAMETERS ps_c, h_pay, r_pay, kurs_w, nt, r_usd, kurs_w

PRIVATE s_sav, d_d, d_ts, d_tt, dn_k1, dn_k2, dn_s, nm

s_sav = SELECT()

nm = ALLTRIM(PS_PAY.FAMILY)+" "+LEFT(PS_PAY.NAME,1)+" "+LEFT(PS_PAY.S_NAME,1)
SELECT 0
USE (base_path+"SLR_MOVE") ORDER TAG DOC_NUM DESCENDING ALIAS S_MV AGAIN

SELECT 0
USE (base_path+"KASSA_M") ALIAS K_MV AGAIN

SELECT 0
USE (base_path+"KASSA_S") ALIAS K_ST AGAIN

d_d  = DATE()
d_ts = SECONDS()
d_tt = TIME()

dn_k1 = Doc_Num("KASSA_M", "", arm, DATE())
* ’ ช ๏ ๅจโเ ๏ ชฎญแโเใชๆจ๏ ญ  แซใ็ ฉ ญ ซจ็จ๏ จซจ ฎโแใโแโขจ๏ ชฎญขฅเโ ๆจจ.
REPLACE SUM_R    WITH SUM_R-ROUND(r_pay*kurs_w,2), ;
        SUM_D    WITH SUM_D-h_pay+r_pay
UNLOCK
USE
dn_k2 = 0
IF r_pay # 0 .AND. kurs_w # 0
  dn_k2 = Doc_Num("KASSA_M", "", arm, DATE())
ENDIF
dn_k1 = sys_char+STR(dn_k1,6)
dn_k2 = sys_char+STR(dn_k2,6)

SELECT S_MV
IF BOF() .OR. EOF()
  dn_s = 1
ELSE
  IF YEAR(DOC_DATE) = YEAR(d_d)
    dn_s = VAL(ALLTRIM(DOC_NUM))+1
  ELSE
    dn_s = 1
  ENDIF
ENDIF
dn_s = STR(dn_s,7)
APPEND BLANK
REPLACE DOC_NUM  WITH dn_s,   ;
        DOC_DATE WITH d_d,    ;
        DOC_TIME WITH d_tt,   ;
        CODE     WITH -4,     ;
        PS_CODE  WITH ps_c,   ;
        TOTAL    WITH Crpt_1(-h_pay,10,2), ;
        R_PART   WITH Crpt_1(r_usd,10,2),  ;
        COURCE   WITH Crpt_1(kurs_w,7,3),  ;
        K_DOC    WITH dn_k1,  ;
        K_DATE   WITH d_d,    ;
        NOTE     WITH nt,     ;
        WHO      WITH user,   ;
        WHERE    WITH arm
USE
SELECT K_MV
APPEND BLANK
REPLACE DOC_NUM  WITH dn_k1, ;
        DOC_DATE WITH d_d,   ;
        DOC_TIME WITH d_ts,  ;
        CODE     WITH 3,     ;
        REF_NUM  WITH dn_s,  ;
        REF_DATE WITH d_d,   ;
        SUM_D    WITH -h_pay,;
        NOTE     WITH nm,    ;
        WHO      WITH user,  ;
        WHERE    WITH arm
IF r_pay # 0 .AND. kurs_w # 0
  APPEND BLANK
  REPLACE DOC_NUM  WITH dn_k2, ;
          DOC_DATE WITH d_d,   ;
          DOC_TIME WITH d_ts,  ;
          CODE     WITH 2,     ;
          SUM_D    WITH r_pay, ;
          SUM_R    WITH -ROUND(r_pay*kurs_w,2),;
          KURS_FL  WITH kurs_w,;
          WHO      WITH user,  ;
          WHERE    WITH arm
ENDIF

USE

SELECT STT_PAY
d_d = Crpt_2(REST)-h_pay
REPLACE REST WITH Crpt_1(d_d,10,2)

SELECT (s_sav)
RETURN
