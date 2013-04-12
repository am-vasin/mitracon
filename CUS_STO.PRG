*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Cus_Sto       งเ กฎโ็จช €ญคเฅฉ  แจญ           22.10.97 10:22:32 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                         ฎฌฅญชซ โใเ  ชซจฅญโ .                          บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cus_Sto

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
PRIVATE ex, c_cod, c_nam, dt_0, dt_1, d0, d1
DO Use_Link
DO Use_Dummy
SELECT 0
USE (base_path+"PARMS")
STORE DAT_START TO dt_0, d0
STORE DATE() TO dt_1, d1
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
ex    = 1
c_cod = 0
c_nam = SPACE(50)

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤ ซจฅญโ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ ฐฐฐฐฑฐฐฐฐÛฐฐฐฐฑฐฐฐฐÛฐฐฐฐฑฐฐฐฐÛฐฐฐฐฑฐฐฐฐÛฐฐฐฐฑฐฐฐฐÛ ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ        ฺฤ ญโฅเข ซ ค โ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ        ณ
*ณ        ณ      แ .. ฏฎ ..      ณ        ณ
*ณ        ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู        ณ
*ณ            < OK Ctrl-W > < โช ง โ์แ๏ Esc >            ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 10, 60, "ฎชใฏชจ ชซจฅญโ  ง  ฏฅเจฎค"
DO Sun_Bord WITH 2,  3,  4, 56, " ซจฅญโ "
DO Sun_Bord WITH 5, 10,  7, 49, " ญโฅเข ซ ค โ "

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@ 3,  5 GET c_nam WHEN G_Cust()
@ 6, 17 SAY "แ"  GET dt_0 PICTURE "@D" VALID Tst_d0()
@ 6, 30 SAY "ฏฎ" GET dt_1 PICTURE "@D" VALID Tst_d1()
@ 8, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE VALID Tst_Blank()

IF ex = 1 .AND. .NOT. EMPTY(c_cod)
  DO P_Doc
ENDIF
*--------------------------------------------------------------------------

POP KEY
CLOSE DATABASES
RELEASE WINDOW (win_name)
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                             ๋กฎเ ชซจฅญโ .                             บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 22.10.97 10:47:32 อผ
PROCEDURE G_Cust

IF Custs("", .F., .F., .F.)
  c_cod = ACCOUNT.CUS_CODE
  c_nam = ACCOUNT.CUS_NAME
  SHOW GET c_nam
ENDIF
RETURN .F.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                 เฎขฅเช  ชฎเเฅชโญฎแโจ ญ ็ ซ์ญฎฉ ค โ๋.                  บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 22.10.97 10:49:55 อผ
PROCEDURE Tst_D0
PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF dt_0 < d0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฌฅฅฌ ค ญญ๋ฅ โฎซ์ชฎ แ "+DTOC(d0)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                 เฎขฅเช  ชฎเเฅชโญฎแโจ ชฎญฅ็ญฎฉ ค โ๋.                   บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 22.10.97 10:49:55 อผ
PROCEDURE Tst_D1
PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF dt_1 > d1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"‘ฅฃฎคญ๏ โฎซ์ชฎ "+DTOC(d1)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                        เฎขฅเช  กซ ญช  ข ๆฅซฎฌ.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 22.10.97 10:53:30 อผ
PROCEDURE Tst_Blank

PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF dt_1 < dt_0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"๋ ใช ง ซจ ญ ็ ซ์ญใ๎ ค โใ กฎซ์่ฅ ชฎญฅ็ญฎฉ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF c_cod = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"๋ โ ช จ ญฅ ฎฏเฅคฅซจซจแ์ แ ชซจฅญโฎฌ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ฅ็ โ์ คฎชใฌฅญโ .                            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 22.10.97 11:15:35 อผ
PROCEDURE P_Doc
PRIVATE dt_w, arr_w, n, s_w

DO Prep_Tmp
SELECT CUS_STO
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ซจฅญโ ญจ็ฅฃฎ ญฅ ชใฏจซ ง  ใช ง ญญ๋ฉ ฏฅเจฎค..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"CUS_STO.DBF")
  DELETE FILE (tmpo_path+"CUS_STO.CDX")
  RETURN
ENDIF
SET ORDER TO NAME
GO TOP
dt_w = SALE_DATE
n = 0
s_w = 0
SCAN
  IF dt_w # SALE_DATE
    n = n+1
    DIMENSION arr_w(n,2)
    arr_w(n,1) = dt_w
    arr_w(n,2) = s_w
    dt_w = SALE_DATE
    s_w  = 0
  ENDIF
  s_w = s_w + TOTAL
ENDSCAN
n = n+1
DIMENSION arr_w(n,2)
arr_w(n,1) = dt_w
arr_w(n,2) = s_w

FOR s_w = 1 TO n
  APPEND BLANK
  REPLACE SALE_DATE WITH arr_w(s_w,1), ;
          TOTAL     WITH arr_w(s_w,2)
ENDFOR

DO Print_W

CLOSE DATABASES
DELETE FILE (tmpo_path+"CUS_STO.DBF")
DELETE FILE (tmpo_path+"CUS_STO.CDX")
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ฎคฃฎโฎขช  ขเฅฌฅญญฎฃฎ ไ ฉซ .                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 22.10.97 11:16:04 อผ
PROCEDURE Prep_Tmp

DO Wt_Mess WITH "ฎคฃฎโฎขช  ค ญญ๋ๅ..."
SELECT 0
DELETE FILE (tmpo_path+"CUS_STO.DBF")
DELETE FILE (tmpo_path+"CUS_STO.CDX")
CREATE DBF (tmpo_path+"CUS_STO.DBF") ;
  ( SALE_DATE  D,     ;
    CODE       N( 7), ;
    PREFIX     C( 4), ;
    NAME       C(35), ;
    PRODUCER   C( 8), ;
    QNT        N( 6), ;
    PRICE      N( 8,2), ;
    TOTAL      N(15,2)  )
USE (tmpo_path+"CUS_STO.DBF") EXCLUSIVE
INDEX ON DTOS(SALE_DATE)+IIF(EMPTY(NAME), REPLICATE(CHR(255),35), NAME)+  ;
                         PREFIX+PRODUCER TAG NAME ADDITIVE
INDEX ON DTOS(SALE_DATE)+STR(CODE,7) TAG CODE ADDITIVE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE

SEEK c_cod
SCAN REST WHILE CUS_CODE = c_cod FOR BETWEEN(DOC_DATE, dt_0, dt_1)
  SELECT SALE
  SEEK SALE_TIT.FIRM+LEFT(DTOS(SALE_TIT.DOC_DATE),4)+SALE_TIT.DOC_NUM
  SCAN REST WHILE SALE_TIT.FIRM = FIRM .AND. SALE_TIT.DOC_NUM = DOC_NUM .AND. SALE_TIT.DOC_DATE = DOC_DATE
    IF .NOT. SEEK(DTOS(DOC_DATE)+STR(CODE,7), "CUS_STO") .OR. ;
       CUS_STO.PRICE # PRICE_R
      SELECT CUS_STO
      APPEND BLANK
      REPLACE SALE_DATE WITH SALE.DOC_DATE, ;
              CODE      WITH SALE.CODE,     ;
              PREFIX    WITH STOCK.PREFIX,  ;
              NAME      WITH STOCK.NAME,    ;
              PRODUCER  WITH STOCK.PRODUCER,;
              QNT       WITH SALE.QNT,      ;
              PRICE     WITH SALE.PRICE_R,  ;
              TOTAL     WITH SALE.QNT*SALE.PRICE_R
    ELSE
      SELECT CUS_STO
      REPLACE QNT       WITH QNT+SALE.QNT, ;
              PRICE     WITH SALE.PRICE_R, ;
              TOTAL     WITH TOTAL+SALE.QNT*SALE.PRICE_R
    ENDIF
    SELECT SALE
  ENDSCAN
  SELECT SALE_TIT
ENDSCAN
DO Wt_Mess

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                   ฅฏฎแเฅคแโขฅญญฎ ฏฅ็ โ์ คฎชใฌฅญโ .                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 22.10.97 12:06:35 อผ
PROCEDURE Print_W

*
*  doc_n  - ญฎฌฅเ ฏเจฅฌญฎฃฎ  ชโ ;
*  doc_d  - ซจกฎ ค โ  ญ ชซ คญฎฉ ซจกฎ ฃฎค ข แจฌขฎซ์ญฎฌ จซจ ็จแซฎขฎฌ ไฎเฌ โฅ;
*  d_type - โจฏ คฎชใฌฅญโ : "H" - ฏเจฅฌญ๋ฉ  ชโ, "B" - โฎซ์ชฎ ฏเจๅฎค,
*                          "D" - โฎซ์ชฎ เ แๅฎฆคฅญจ๏. ฎ ใฌฎซ็ ญจ๎ ฏฅ็ โ ฅโแ๏
*                           โฎซ์ชฎ ฏเจๅฎค, ฅแซจ ญฎฌฅเ ญ ชซ คญฎฉ ฏใแโ จซจ
*                           ฏเจฅฌญ๋ฉ  ชโ ข ฏเฎโจขญฎฌ แซใ็ ฅ.
*

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_p, f_p
PRIVATE t_d, f_d, d_year, ffeed, sw, p_drctry
PRIVATE sel_rem, f_name, sb, n_p_det, n_pg, f_pg, s_pg, str_w, nds, doc_tit
PRIVATE k00

sel_rem = SELECT()

DIMENSION sb(5,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Date_0}"
sb( 2,2) = DTOC(dt_0)

sb( 3,1) = "{Date_1}"
sb( 3,2) = DTOC(dt_1)

sb( 4,1) = "{Customer}"
sb( 4,2) = ALLTRIM(c_nam)

sb( 5,1) = "{Total_Doc    }"
sb( 5,2) = ""

n = ALEN(sb,1)

SELECT 0
USE DOC_FORM

LOCATE FOR "CUS_STO" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR

n = MEMLINES(PAGE_F)
DIMENSION f_p(n)
FOR i = 1 TO n
  f_p(i) = MLINE(PAGE_F,i)
ENDFOR

n = MEMLINES(DOC_H)
DIMENSION t_d(n)
FOR i = 1 TO n
  t_d(i) = MLINE(DOC_H,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR
USE

SELECT CUS_STO

DO Ini_Prn WITH "‘ฏจแฎช ฏเฎค ฆ ชซจฅญโใ", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

n_p_det = p_ln-MAX(ALEN(t_p),ALEN(t_d))-MAX(ALEN(f_p),ALEN(f_d))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = CEILING(RECCOUNT()/n_p_det)
ENDIF

PRIVATE s_d
STORE 0 TO s_d

FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1
    EJECT
    FOR i = 1 TO ALEN(t_p)
      str_w = t_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(t_d)
      str_w = t_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ENDIF

  FOR i = 1 TO n_p_det
    IF .NOT. EMPTY(NAME)
      str_w = DTOC(SALE_DATE)+" "+PREFIX+" "+NAME+" "+PRODUCER+ ;
              STR(QNT,7)+STR(PRICE,9,2)+STR(TOTAL,16,2)
    ELSE
      str_w = PADL(" โฎฃฎ ง  ", LEN(PREFIX+NAME+PRODUCER)+19, "-")+  ;
                   DTOC(SALE_DATE)+STR(TOTAL,16,2)
      s_d = s_d+TOTAL
    ENDIF
    @ PROW()+1,0 SAY str_w
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR

  IF f_pg = n_pg
    sb(5, 2) = STR(s_d, 15, 2)
    FOR i = 1 TO ALEN(f_d)
      str_w = f_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR

f_name = SYS(3)
COPY FOR .NOT. EMPTY(NAME) TO (tmpo_path+f_name)

USE

DO Term_Prn WITH "", tmpo_path+f_name

DELETE FILE (tmpo_path+f_name+".DBF")
SELECT (sel_rem)
RETURN
