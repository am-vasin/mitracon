*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º          à®£à ¬¬  ¯¥ç â¨ § ï¢ª¨ ­  áª« ¤ ¢ ­®¢®¬ ä®à¬ â¥.            º
*º                                                                       º
*º         „®¡ ¢«¥­¨ï „à ç¥¢  ®â¬¥ç¥­ë && STATWARN    (çã¢. ª áâ â¨ª¥)   º
*º         „àã£¨¥ ¤®¡.„à ç¥¢  ®â¬¥ç¥­ë && LASTWARN    (¯®á«¥¤­ïï ¤¥â.)   º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Prn_OrdN

PARAMETERS d_n, add_n
*
*  d_n   - ­®¬¥à á¯¨áª  ( ç¨á«® );
*  add_n - ­®¬¥à § ï¢ª¨ ( ç¨á«® ).
*
PRIVATE p_ln, fnt, n_cp, lft, n, i, j, stw, t_p, e_p, tmp_f, ffeed, p_drctry
PRIVATE sel_rem, sb, delim, cnt, n_p, n_u, prc_00, cnt_addr, statwrn && STATWARN
PRIVATE lt_addr,lastpiece && LASTWARN
lt_addr = " "

DO N_P_Ord WITH STR(d_n, 6), add_n
RETURN

DIMENSION sb(14,2)

sel_rem = SELECT()

*SELECT 0
*USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_1205 AGAIN
*IF SEEK (d_n)
*  lt_addr = LT_1205.ADDRESS
*ENDIF
lt_addr = ListAddr(d_n, 36)
*USE

SELECT 0
IF FILE_O(base_path+"O_BOUND.DBF")
  USE (base_path+"O_BOUND") ORDER TAG H_BOUND
  i = 0
  SCAN
    i = i + 1
    DIMENSION prc_00(i,2)
    prc_00(i,1) = H_BOUND
    prc_00(i,2) = CHAR_C
  ENDSCAN
ENDIF

USE (base_path+"ORD_T") ORDER TAG DOC_NUM AGAIN ALIAS TMP000
SEEK STR(d_n,6)+STR(add_n,3)
IF .NOT. FOUND()
  USE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"„®ªã¬¥­â ­¥ ­ ©¤¥­ "+ALLTRIM(STR(d_n,6))+"/"+ALLTRIM(STR(add_n,3))
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (sel_rem)
  RETURN
ENDIF
sb(1,1) = "{Ord_No  }"
sb(1,2) = ALLTRIM(STR(d_n,6))+"/"+ALLTRIM(STR(add_n,3))
sb(1,2) = PADR(sb(1,2),LEN(sb(1,1)))

sb(2,1) = "{Date    }"
sb(2,2) = TRANSFORM(ORD_DATE,"@D")
sb(2,2) = PADR(sb(2,2),LEN(sb(2,1)))

sb(3,1) = "{Ord_Name                      }"

sb(4,1) = "{C}"
sb(4,2) = "   "

sb(5,1) = "{Arm           }"
sb(5,2) = WHERE

sb(6,1) = "{Salesman                      }"
sb(6,2) = WHO

sb(7,1) = "{Stockman                      }"
sb(7,2) = STO_MARK

sb(13,1) = "{Collector                     }"
sb(13,2) = CHECK_S

sb(8,1) = "{Time  }"
sb(8,2) = ORD_TIME

sb(9,1) = "{Note                  }"
sb(9,2) = NOTE

sb(10,1) = "{Z}"
sb(10,2) = IIF(ZONE = "**", " à.", IIF(ZONE == " ", " ¥â", " "+ZONE+" "))

sb(11,1) = "{Time ready  }"
sb(11,2) = IIF(EMPTY(REQ_DATE), PADR("¥ á®¡¨à âì!", 14), ;
               DTOC(REQ_DATE)+" "+LEFT(REQ_TIME,5) )
sb(12,1) = "{LIST_ADDR                         }"
sb(12,2) = lt_addr

sb(3,2) = LIST_NAME
sb(3,2) = PADR(sb(3,2),LEN(sb(3,1)))
delim = ALLTRIM(sb(1,2))+" ®â "+ALLTRIM(sb(2,2))
delim = PADC(delim,44,"-")+"-"+PADC(delim,29,"-")
delim = REPLICATE("-",73)

USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS TMP000
IF sb(6,2) = 0
  sb(6,2) = "  "
ELSE
  SEEK sb(6,2)
  IF FOUND()
    sb(6,2) = ALLTRIM(FAMILY)
  ELSE
    sb(6,2) = "?"
  ENDIF
ENDIF
sb(6,2) = PADR(sb(6,2),LEN(sb(6,1)))

IF sb(7,2) = 0
  sb(7,2) = "  "
ELSE
  SEEK sb(7,2)
  IF FOUND()
    sb(7,2) = ALLTRIM(FAMILY)
  ELSE
    sb(7,2) = "?"
  ENDIF
ENDIF
sb(7,2) = PADR(sb(7,2),LEN(sb(7,1)))

sb(13,2) = VAL(sb(13,2))
IF sb(13,2) = 0
  sb(13,2) = "  "
ELSE
  SEEK sb(13,2)
  IF FOUND()
    sb(13,2) = ALLTRIM(FAMILY)
  ELSE
    sb(13,2) = "?"
  ENDIF
ENDIF
sb(13,2) = PADR(sb(13,2),LEN(sb(13,1)))

sb(14,1) = "{Men_List                      }"
IF USED("LIST_TIT")
  sb(14,2) = LIST_TIT.WHO_CR
  IF sb(14,2) = 0
    sb(14,2) = " "
  ELSE
    SEEK sb(14,2)
    IF FOUND()
      sb(14,2) = ALLTRIM(FAMILY)
    ELSE
      sb(14,2) = "?"
    ENDIF
  ENDIF
  sb(14,2) = PADR(sb(14,2),LEN(sb(14,1)))
ELSE
  sb(14,2) = " "
ENDIF  

USE (base_path+"ARM_LST") AGAIN ALIAS TMP000
LOCATE FOR sb(5,2) = ARM_C
IF FOUND()
  sb(5,2) = ARM_N
ELSE
  sb(5,2) = ""
ENDIF
sb(5,2) = PADR(sb(5,2),LEN(sb(5,1)))

USE (base_path+"DOC_FORM")
LOCATE FOR "ORDER_W" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(DOC_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(DOC_H,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION e_p(n)
FOR i = 1 TO n
  e_p(i) = MLINE(DOC_F,i)
ENDFOR
USE

tmp_f = tmpo_path+SYS(3)
CREATE DBF (tmp_f)  ;
  ( ADDRESS C( 8), ;
    NAME    C(30), ;
    QNT     N( 7), ;
    PROD    C( 8), ;
    QNT_R   N( 7), ;
    ATTEN   C( 3), ;
    A_ADDR  C(16)  )                          && STATWARN+LASTWARN
USE (tmp_f) ALIAS P_TMP
INDEX ON ADDRESS TAG ADDRESS

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE AGAIN ALIAS S000

SELECT 0
USE (base_path+"ORD_D") ORDER TAG DOC_NUM AGAIN ALIAS TMP000
SET RELATION TO CODE INTO S000
SEEK STR(d_n,6)+STR(add_n,3)
SCAN REST WHILE LIST_NUM = STR(d_n,6) .AND. ORD_NUM = add_n
  statwrn=STATWARN(S000.PREFIX, S000.NAME, S000.PRODUCER) && STATWARN
  lastpiece=(S000.QNT=0) && LASTWARN
  SELECT P_TMP
  APPEND BLANK
  IF Sale_Mode = 'MEGA'
    REPLACE NAME    WITH S000.PREFIX+" "+S000.NAME, ;
          ADDRESS WITH S000.ADDRESS, ;
          A_ADDR  WITH S000.MEMO_ST, ;
          PROD    WITH S000.PRODUCER,;
          QNT_R   WITH S000.QNT_ROUND,;
          ATTEN   WITH P_Bound(S000.PRICE)+;
                  IIF(lastpiece,'#',' ')+;
                  IIF(statwrn,'@',' '),;
          QNT     WITH TMP000.QNT                         && STATWARN
  ELSE
    REPLACE NAME    WITH S000.PREFIX+" "+S000.NAME, ;
          ADDRESS WITH S000.ADDRESS, ;
          A_ADDR  WITH S000.MEMO_ST, ;
          PROD    WITH S000.PRODUCER,;
          QNT_R   WITH S000.QNT_ROUND,;
          ATTEN   WITH P_Bound(S000.PRICE)+;
                  IIF(statwrn,'@',' '),;
          QNT     WITH TMP000.QNT                         && STATWARN
  ENDIF
  SELECT TMP000
ENDSCAN
USE
SELECT S000
USE

DO Ini_Prn WITH "‡ ï¢ª  ­  áª« ¤ N "+sb(1,2), p_ln, lft, n_cp, fnt, ffeed, p_drctry

FOR i = 1 TO ALEN(t_p)
  stw = t_p(i)
  FOR j = 1 TO ALEN(sb,1)
    stw = STRTRAN(stw, sb(j,1), sb(j,2))
  ENDFOR
  IF i = 1
    @ PROW(), PCOL() SAY stw
  ELSE
    @ PROW()+1, 0 SAY stw
  ENDIF
ENDFOR

SELECT P_TMP
cnt = 0
GO TOP
cnt_addr = LEFT(ADDRESS,4)
SCAN

  IF QNT_R > 1
    n_p = FLOOR(ABS(QNT)/QNT_R)
    n_u = ABS(QNT)-n_p*QNT_R
    n_p = " ("+ALLTRIM(STR(n_p))+"X"+ALLTRIM(STR(QNT_R))+"+"+ALLTRIM(STR(n_u))+") "
  ELSE
    n_p = ""
  ENDIF
  n_p = ALLTRIM(NAME)+"__"+ALLTRIM(STR(QNT,7))+n_p       && STATWARN
  n_p = PADR(n_p, 42)                                     && STATWARN
  stw = V_Addr(ADDRESS)+ATTEN+n_p+" "+PROD+" "+IIF(EMPTY(SUBSTR(A_ADDR,10,7)),LEFT(A_ADDR,9),LEFT(A_ADDR,8)+"+")     && ATTEN
*  stw = ADDRESS+ATTEN+NAME+" "-STR(QNT,6)+" "+PROD+" "+STR(QNT_R,6)+" "+A_ADDR
  IF LEFT(ADDRESS,4) = cnt_addr
    @ PROW()+1, 0 SAY stw
  ELSE
    @ PROW()+1, 0 SAY delim
    @ PROW()+1, 0 SAY stw
  ENDIF
*  cnt = (cnt+1)%5
*  IF cnt = 0
  cnt_addr = LEFT(ADDRESS,4)
ENDSCAN
USE
DELETE FILE (tmp_f+".dbf")
DELETE FILE (tmp_f+".cdx")

FOR i = 1 TO ALEN(e_p)
  stw = e_p(i)
  FOR j = 1 TO ALEN(sb,1)
    stw = STRTRAN(stw, sb(j,1), sb(j,2))
  ENDFOR
  @ PROW()+1, 0 SAY stw
ENDFOR
DO Term_Prn WITH ""

SELECT (sel_rem)
RETURN