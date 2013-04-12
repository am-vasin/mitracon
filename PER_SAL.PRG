*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Per_Sal       งเ กฎโ็จช €ญคเฅฉ  แจญ           15.01.98 15:53:12 บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                   ‘ฏเ ขช  ฏฎ ง เฏซ โฅ (แฎโเใคญจชจ).                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Per_Sal
PRIVATE d0, d1

SET DATE GERMAN
d1 = DATE()
d0 = CTOD("01"+SUBSTR(DTOC(d1),3))

IF .NOT. G_Period()
  RETURN
ENDIF

DO Prep_Tmp

DO Prn_Doc

CLOSE DATABASES

DELETE FILE (tmpo_path+"SLR_TMP.DBF")
DELETE FILE (tmpo_path+"SLR_TMP.DBF")
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ             เฎฃเ ฌฌ  จญโฅเ ชโจขญฎฃฎ ขขฎค  จญโฅเข ซ  ค โ.              บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 15.01.98 15:58:48 อผ
PROCEDURE G_Period

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

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ                                                        ณ
*ณ    ็ ซ์ญ ๏ ค โ  ..  ฎญฅ็ญ ๏ ค โ  ..  ณ
*ณ                                                        ณ
*ณ                                                        ณ
*ณ            < OK Ctrl-W > < โช ง โ์แ๏ Esc >            ณ
*ณ                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE ex
ex   = 1             &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 9, 60, " ค ฉโฅ จญโฅเข ซ ค โ"

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*
@ 3,  5 SAY " ็ ซ์ญ ๏ ค โ " GET d0 PICTURE "@D" VALID Tst_1(d0)
@ 3, 32 SAY "ฎญฅ็ญ ๏ ค โ " GET d1 PICTURE "@D" VALID Tst_1(d1)
@ 5, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE VALID Tst_1()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                    เฎขฅเช  ชฎเเฅชโญฎแโจ จญโฅเข ซ .                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 15.01.98 16:05:37 อผ
PROCEDURE Tst_1
PARAMETER dt
PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF
IF TYPE("dt") = "D"
  IF dt > DATE()
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"ฎฌจซใฉโฅ, "+DTOC(dt)+" ฅ้ฅ จ ญฅ ญ แโใฏจซฎ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ELSE
  IF d0 > d1
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+" ็ ซ์ญ ๏ ค โ  กฎซ์่ฅ ชฎญฅ็ฎฉ? —โฎ-โฎ งคฅแ์ ญฅ โ ช..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ฎคฃฎโฎขช  ขเฅฌฅญญฎฃฎ ไ ฉซ .                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 15.01.98 16:23:20 อผ
PROCEDURE Prep_Tmp
PRIVATE rst

USE (base_path+"SLR_TYPE") ORDER TAG CODE
SELECT 0
USE (base_path+"SLR_MOVE")
SET RELATION TO CODE INTO SLR_TYPE
SELECT 0

DELETE FILE (tmpo_path+"SLR_TMP.DBF")
DELETE FILE (tmpo_path+"SLR_TMP.DBF")
CREATE DBF (tmpo_path+"SLR_TMP.DBF") ;
  ( PS_CODE N( 5), ;
    D_DATE  D,     ;
    D_TIME  C( 8), ;
    D_NUM   C( 7), ;
    OPER_N  C(24), ;
    QNT     N(5,2),;
    UNIT    C(12), ;
    PRICE   N(8,2),;
    TOTAL   N(8,2),;
    R_PART  N(8,2),;
    COURCE  N(8,3),;
    YEAR    N( 4), ;
    MONTH   N( 2), ;
    NOTE    C(24), ;
    REST    N(8,2)  )

INDEX ON STR(PS_CODE,5)+DTOS(D_DATE)+D_TIME TAG ORD

SELECT SLR_MOVE
SCAN
  SELECT SLR_TMP
  APPEND BLANK
  REPLACE PS_CODE WITH SLR_MOVE.PS_CODE,  ;
          D_DATE  WITH SLR_MOVE.DOC_DATE, ;
          D_TIME  WITH SLR_MOVE.DOC_TIME, ;
          D_NUM   WITH SLR_MOVE.DOC_NUM,  ;
          OPER_N  WITH SLR_TYPE.NAME,     ;
          QNT     WITH SLR_MOVE.QNT,      ;
          UNIT    WITH SLR_MOVE.UNIT,     ;
          YEAR    WITH SLR_MOVE.YEAR,     ;
          MONTH   WITH SLR_MOVE.MONTH,    ;
          NOTE    WITH SLR_MOVE.NOTE,     ;
          PRICE   WITH Crpt_2(SLR_MOVE.PRICE), ;
          R_PART  WITH Crpt_2(SLR_MOVE.R_PART),;
          COURCE  WITH Crpt_2(SLR_MOVE.COURCE),;
          TOTAL   WITH Crpt_2(SLR_MOVE.TOTAL)
  SELECT SLR_MOVE
ENDSCAN

USE
SELECT SLR_TYPE

USE (base_path+"PS_STAT")

SCAN FOR SEEK(STR(CODE,5), "SLR_TMP")
  rst = Crpt_2(PS_STAT.START)
  SELECT SLR_TMP
  SCAN REST WHILE PS_CODE = PS_STAT.CODE
    rst = rst+TOTAL
    IF BETWEEN(D_DATE, d0, d1)
      REPLACE REST WITH rst
    ELSE
      DELETE
    ENDIF
  ENDSCAN
  SELECT PS_STAT
ENDSCAN

USE

RETURN
*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                           ฅ็ โ์ คฎชใฌฅญโ .                            บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 15.01.98 17:39:40 อผ
PROCEDURE Prn_Doc
PRIVATE p_ln, sb, fnt, n_cp, lft, ffeed, n, i, l_flt, str_w, t_p, p_drctry
PRIVATE distr

DIMENSION sb(3,2)

sb( 1,1) = "{Client  }"
sb( 1,2) = ""

sb( 2,1) = "{S_Date}"
sb( 2,2) = DTOC(d0)

sb( 3,1) = "{E_Date}"
sb( 3,2) = DTOC(d1)

SELECT 0
USE DOC_FORM
LOCATE FOR "SLR_REP" = ALLTRIM(UPPER(DOC_NAME))
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
USE

USE (base_path+"PERSONS") ORDER TAG DEP_NAME
SET RELATION TO STR(CODE,5) INTO SLR_TMP

DO Ini_Prn WITH " เฏซ โ ", p_ln, lft, n_cp, fnt, ffeed, p_drctry
l_flt = 0
SCAN FOR FOUND("SLR_TMP")
  sb( 1,2) = ALLTRIM(FAMILY)+" "+ALLTRIM(NAME)+" "+ALLTRIM(S_NAME)
  FOR i = 1 TO ALEN(t_p)
    str_w = t_p(i)
    FOR j = 1 TO ALEN(sb,1)
      str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
    ENDFOR
    IF l_flt = 0
      @ PROW(), PCOL() SAY str_w
    ELSE
      @ PROW()+1, 0 SAY str_w
    ENDIF
*    IF p_ln # 0
*      IF l_flt % p_ln = 0 .AND. l_flt # 0
*        EJECT
*      ENDIF
*    ENDIF
    l_flt = l_flt+1
  ENDFOR
  SELECT SLR_TMP
  SCAN REST WHILE PS_CODE = PERSONS.CODE
    IF EMPTY(R_PART)
      distr = ""
    ELSE
      distr = " ("+ALLTRIM(STR(ABS(TOTAL)-R_PART,10,2))+","+ ;
                   ALLTRIM(STR(R_PART,10,2))+"*"+ ;
                   ALLTRIM(STR(COURCE,8,3))+"="+  ;
                   ALLTRIM(STR(ROUND(R_PART*COURCE,2),8,3))+")"
    ENDIF
    str_w = DTOC(D_DATE)+" "+D_NUM+" "+OPER_N+" "+  ;
            TRANSFORM(QNT,"@Z 999.99")+" "+UNIT+" "+   ;
            TRANSFORM(PRICE,"@Z 99999.99")+" "+     ;
            TRANSFORM(TOTAL,"@Z 99999.99")+" "+     ;
            TRANSFORM(REST,"99999.99")+distr
*    IF p_ln # 0
*      IF l_flt % p_ln = 0 .AND. l_flt # 0
*        EJECT
*      ENDIF
*    ENDIF
    l_flt = l_flt+1
    @ PROW()+1, 0 SAY str_w
    str_w = IIF(.NOT. EMPTY(YEAR), "           "+STR(MONTH,2)+"."+STR(YEAR,4), ;
                                   "                   ")
    str_w = str_w+IIF(.NOT. EMPTY(NOTE), "        เจฌฅ็ ญจฅ "+NOTE, "")
    IF .NOT. EMPTY(str_w)
      l_flt = l_flt+1
      @ PROW()+1, 0 SAY str_w
    ENDIF
  ENDSCAN
  SELECT PERSONS
ENDSCAN
USE

DO Term_Prn WITH ""

RETURN
