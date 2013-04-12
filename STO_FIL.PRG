*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Sto_Fil       งเ กฎโ็จช €ญคเฅฉ  แจญ           23.10.97 19:04:35 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                      ฅคฎฌฎแโ์ ญ ซจ็จ๏ ข ไจซจ ซฅ.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sto_Fil
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   โชเ๋ข ฅฌ ไ ฉซ๋ .., ใแโ ญ ขซจข ฅฌ แข๏งจ จ โ. ค.
*
DO Prp_Tit    && ฎคฃฎโฎขช  แฏจแช  ไจซจ ซฎข
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ ญจ ฎคญฎฃฎ ไจซจ ซ !"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"FIL_TIT.DBF")
  DELETE FILE (tmpo_path+"FIL_TIT.CDX")
  RETURN
ENDIF
DO Use_Dummy
SELECT FIL_TIT

*
*   ฏเฅคฅซ๏ฅฌ  แจญๅเฎญญฎฅ ฌฅญ๎
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "ฅ็ โ์" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*    แ็ฅโ เ งฌฅเฎข ฎชญ 
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ฎซจ็ฅแโขฎ ขจคจฌ๋ๅ แโเฎช BROWSE
wd = 55+IIF(SET("CENTURY") = "ON", 10, 8)

DO D_Wins WITH ln, wd, "”จซจ ซ๋", 0, 0
what_do = "List"

@ 1, 3 SAY "  งข ญจฅ "
@ 1, 54 SAY " –ฅญ๋ ใแโ."

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && เฎแฌฎโเ แฏจแช 

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    SELECT FIL_TIT

*
*    BROWSE - ฌฅญ๎
*
    BROWSE FIELDS ;
           FIL_NAME:H="":50, ;
           VALUTA:H="":3,    ;
           DATE_SET:H="",    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && ฅ็ โ์ ขฅคฎฌฎแโจ ญ ซจ็จ๏

    DO Prn_List
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
DELETE FILE (tmpo_path+"FIL_TIT.DBF")
DELETE FILE (tmpo_path+"FIL_TIT.CDX")
DELETE FILE (tmpo_path+"FIL_LST.DBF")
DELETE FILE (tmpo_path+"FIL_LST.CDX")
RELEASE MENU (menu_name) EXTENDED

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ            ฎคฃฎโฎขช  ขเฅฌฅญญฎฃฎ ไ ฉซ  แ ฏฅเฅ็ญฅฌ ไจซจ ซฎข.            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 23.10.97 19:14:44 อผ
PROCEDURE Prp_Tit
PRIVATE mss

DELETE FILE (tmpo_path+"FIL_TIT.DBF")
DELETE FILE (tmpo_path+"FIL_TIT.CDX")

CREATE DBF (tmpo_path+"FIL_TIT.DBF")  ;
        (  FIL_CODE N( 8),  ;
           FIL_NAME C(50),  ;
           VALUTA   C( 3),  ;
           DATE_SET D )
INDEX ON SYS(15, lwr, FIL_NAME) TAG NAME

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"RYN_L_T")

SCAN
  IF .NOT. SEEK(CUS_CODE, "ACCOUNT")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+" ฎก้ฅฌ แฏจแชฅ ญฅโ ไจซจ ซ  "+CUS_NAME+"!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    LOOP
  ENDIF
  SELECT FIL_TIT
  APPEND BLANK
  REPLACE FIL_CODE WITH RYN_L_T.CUS_CODE,  ;
          FIL_NAME WITH ACCOUNT.CUS_NAME,  ;
          VALUTA   WITH IIF(RYN_L_T.VALUTA = "$", "USD", "ใก"), ;
          DATE_SET WITH RYN_L_T.DATE_P
  SELECT RYN_L_T
ENDSCAN

USE
SELECT ACCOUNT
USE
SELECT FIL_TIT

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                       ฅ็ โ์ ขฅคฎฌฎแโจ ญ ซจ็จ๏.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 23.10.97 19:37:47 อผ
PROCEDURE Prn_List
PRIVATE mss

DO Wt_Mess WITH "ฎคฃฎโฎขช  ค ญญ๋ๅ..."
DO Prep_Tmp
DO Wt_Mess
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ไจซจ ซฎฌ ญจ็ฅฃฎ ญฅ ็จแซจโแ๏!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"FIL_LST.DBF")
  DELETE FILE (tmpo_path+"FIL_LST.CDX")
  RETURN
ENDIF

DO Print

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ฎคฃฎโฎขช  ขเฅฌฅญญฎฃฎ ไ ฉซ .                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 23.10.97 19:40:23 อผ
PROCEDURE Prep_Tmp

DELETE FILE (tmpo_path+"FIL_LST.DBF")
DELETE FILE (tmpo_path+"FIL_LST.CDX")
SELECT 0
IF FIL_TIT.VALUTA = "USD"
  CREATE DBF (tmpo_path+"FIL_LST") ;
   ( CODE     N( 7), ;
     PREFIX   C( 4), ;
     NAME     C(35), ;
     PRODUCER C( 8), ;
     QNT      N( 6), ;
     PRICE    N(10,4),;
     TOTAL    N(15,4) )
ELSE
  CREATE DBF (tmpo_path+"FIL_LST") ;
   ( CODE     N( 7), ;
     PREFIX   C( 4), ;
     NAME     C(35), ;
     PRODUCER C( 8), ;
     QNT      N( 6), ;
     PRICE    N( 8), ;
     TOTAL    N(15) )
ENDIF

INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"RYN_L") ORDER TAG CUS_CODE

SEEK FIL_TIT.FIL_CODE

SCAN REST WHILE CUS_CODE = FIL_TIT.FIL_CODE
  IF .NOT. SEEK(CODE, "STOCK")
    LOOP
  ENDIF
  SELECT FIL_LST
  APPEND BLANK
  REPLACE CODE     WITH STOCK.CODE,     ;
          PREFIX   WITH STOCK.PREFIX,   ;
          NAME     WITH STOCK.NAME,     ;
          PRODUCER WITH STOCK.PRODUCER, ;
          QNT      WITH RYN_L.QNT,      ;
          PRICE    WITH IIF(FIL_TIT.VALUTA="USD",RYN_L.PRICE_D,RYN_L.PRICE_R), ;
          TOTAL    WITH IIF(FIL_TIT.VALUTA="USD",RYN_L.PRICE_D,RYN_L.PRICE_R)*RYN_L.QNT
  SELECT RYN_L
ENDSCAN

USE

SELECT STOCK
USE

SELECT FIL_LST
GO TOP

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                       ฅ็ โ์ ขฅคฎฌฎแโจ ญ ซจ็จ๏.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 23.10.97 19:52:13 อผ
PROCEDURE Print

PRIVATE sb, p_ln, fnt, n_cp, lft, t_p, f_p, t_d, f_d, n, i, ffeed, pic1, pic2
DIMENSION sb(6,2)

*{Pgs}           - ‘โเ ญจๆ /ขแฅฃฎ แโเ ญจๆ
*{Customer}      - ”จซจ ซ
*{Dat1  }        -  โ  ใแโ ญฎขชจ ๆฅญ
*{Valuta}        -  ซ๎โ 
*{Sum_P        } - ‘ใฌฌ  ญ  แโเ ญจๆฅ
*{Sum_D        } - ‘ใฌฌ  ฏฎ คฎชใฌฅญโใ

sb(1,1) = "{Pgs}"
sb(1,2) = ""

sb(2,1) = "{Customer}"
sb(2,2) = ALLTRIM(FIL_TIT.FIL_NAME)

sb(3,1) = "{Dat1  }"
sb(3,2) = TRANSFORM(FIL_TIT.DATE_SET,"@D ")

sb(4,1) = "{Valuta}"
sb(4,2) = IIF(FIL_TIT.VALUTA = "USD", "ข USD", "ข เใกซ๏ๅ")

sb(5,1) = "{Sum_P        }"
sb(5,2) = ""

sb(6,1) = "{Sum_D        }"
sb(6,2) = ""

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "STO_FIL" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
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
SELECT FIL_LST
GO TOP
DO Wt_Mess

PRIVATE s_page, s_doc
PRIVATE n_pg, f_pg, n_det, j, stw, p_drctry

n_det = p_ln-ALEN(t_p)-ALEN(f_p)-ALEN(f_d)
n_pg  = CEILING(RECCOUNT()/n_det)

IF FIL_TIT.VALUTA = "USD"
  pic1 = "@Z 99999.9999"
  pic2 = "@Z 9999999999.9999"
ELSE
  pic1 = "@Z 9999999.99"
  pic2 = "@Z 999999999999.99"
ENDIF

DO Wt_Mess WITH "ฎคฃฎโฎขช  คฎชใฌฅญโ ..."

STORE 0 TO s_page, s_doc
DO Ini_Prn WITH " ซจ็จฅ ข ไจซจ ซฅ", p_ln, lft, n_cp, fnt, ffeed, p_drctry
FOR f_pg = 1 TO n_pg
  IF f_pg # 1
    EJECT
  ENDIF
  s_page = 0
  sb(1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  IF f_pg = 1
    FOR i = 1 TO ALEN(t_d)
      stw = t_d(i)
      FOR j = 1 TO 6
        stw = STRTRAN(stw, sb(j,1), sb(j,2))
      ENDFOR
      @ PROW()+1, 0 SAY stw
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(t_p)
      stw = t_p(i)
      FOR j = 1 TO 6
        stw = STRTRAN(stw, sb(j,1), sb(j,2))
      ENDFOR
      @ PROW()+1, 0 SAY stw
    ENDFOR
  ENDIF
  FOR i = 1 TO n_det
    stw = PREFIX+" "+NAME+" "+PRODUCER+" "+TRANSFORM(QNT,"@Z 999999")+" "+ ;
          TRANSFORM(PRICE, pic1)+" "+TRANSFORM(TOTAL, pic2)
    @ PROW()+1, 0 SAY stw
    s_page = s_page+TOTAL
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
  s_doc = s_doc+s_page

  sb(5,2) = TRANSFORM(s_page, pic2)
  sb(6,2) = TRANSFORM(s_doc, pic2)

  IF f_pg # n_pg 
    FOR i = 1 TO ALEN(f_p)
      stw = f_p(i)
      FOR j = 1 TO 6
        stw = STRTRAN(stw, sb(j,1), sb(j,2))
      ENDFOR
      @ PROW()+1, 0 SAY stw
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_d)
      stw = f_d(i)
      FOR j = 1 TO 6
        stw = STRTRAN(stw, sb(j,1), sb(j,2))
      ENDFOR
      @ PROW()+1, 0 SAY stw
    ENDFOR
  ENDIF
ENDFOR

PRIVATE f_name
f_name = SYS(3)
COPY TO (tmpo_path+f_name)

DO Wt_Mess

DO Term_Prn WITH "", tmpo_path+f_name+"DBF"
DELETE FILE (tmpo_path+f_name+"DBF")

USE
DELETE FILE (tmpo_path+"FIL_LST.DBF")
DELETE FILE (tmpo_path+"FIL_LST.CDX")

RETURN
