*ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»
*º                                                                       º
*º      à®£à ¬¬  ¯¥ç â¨ áã¬¬ à­®© § ï¢ª¨ ­  áª« ¤ ¢ ­®¢®¬ ä®à¬ â¥.      º
*º                                                                       º
*ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼
PROCEDURE Prn_OrdS

PARAMETERS d_n
*
*  d_n   - ­®¬¥à á¯¨áª  ( ç¨á«® );
*
PRIVATE mss
DIMENSION mss(3)
mss[1] = ""
mss[2] = CHR(0)+"â®â ¤®ªã¬¥­â ãáâ à¥« ¨ ãâà â¨« á¢®î  ªâã «ì­®áâì."
mss[3] = ""
DO Out_Mess WITH 7, "mss"
RETURN

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, stw, t_p, e_p, tmp_f, ffeed, p_drctry
PRIVATE sel_rem, sb, delim, cnt, n_p, n_u, prc_00, i_list, e_list, cnt_addr
PRIVATE prev_lst, d_n0, tmpVal, lt_addr
prev_lst = .F.
d_n0 = d_n
lt_addr = " "
IF TYPE("mark_lst") = "N"
  IF TYPE("mark_ord") = "C"
    IF .NOT. EMPTY(mark_ord)
      prev_lst = .T.
      d_n0 = mark_lst
    ENDIF
  ENDIF
ENDIF

DIMENSION sb(13,2)

sel_rem = SELECT()
STORE "" TO i_list, e_list

*SELECT 0
*USE (base_path+"LIST_TIT.DBF") ORDER TAG LST_NUM ALIAS LT_1205 AGAIN
*IF SEEK (d_n0)
*  lt_addr = LT_1205.ADDRESS
*ENDIF
*USE
lt_addr = ListAddr(d_n0, 36)

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
SEEK STR(d_n0,6)
IF .NOT. FOUND()
  USE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"„®ªã¬¥­â ­¥ ­ ©¤¥­ "+ALLTRIM(STR(d_n0,6))+"/"+ALLTRIM(STR(add_n0,3))
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (sel_rem)
  RETURN
ENDIF

IF .NOT. prev_lst
  SCAN REST WHILE LIST_NUM = STR(d_n0,6)
    IF EMPTY(CHECK_S) 
      IF RIGHT(i_list,1) # "."
        i_list = i_list+" "+ALLTRIM(STR(ORD_NUM))+","
        IF LEN(i_list) > 55 
          i_list = i_list+"..."
        ENDIF
      ENDIF
    ELSE
      IF RIGHT(e_list,1) # "."
        e_list = e_list+" "+ALLTRIM(STR(ORD_NUM))+","
        IF LEN(e_list) > 55
          e_list = e_list+"..."
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
ELSE
  i_list = mark_ord
  SCAN REST WHILE LIST_NUM = STR(d_n0,6)
    tmpVal = " "+ALLTRIM(STR(ORD_NUM))+","
    IF .NOT. tmpVal $ i_list
      IF LEN(e_list) > 55
        e_list = e_list+"..."
        EXIT
      ENDIF
      e_list = e_list+" "+ALLTRIM(STR(ORD_NUM))+","
    ENDIF
  ENDSCAN
ENDIF

IF EMPTY(i_list)
  USE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"„«ï á¯¨áª  "+ALLTRIM(STR(d_n0,6))+" ¢á¥ § ï¢ª¨ ãâ¢¥à¦¤¥­ë ¨«¨ ¢ ­ ¡®à¥!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (sel_rem)
  RETURN
ENDIF
SEEK STR(d_n0,6)

sb(1,1) = "{OrdN}"
sb(1,2) = ALLTRIM(STR(d_n0,6))
sb(1,2) = PADR(sb(1,2),LEN(sb(1,1)))

sb(2,1) = "{Date    }"
sb(2,2) = TRANSFORM(ORD_DATE,"@D")
sb(2,2) = PADR(sb(2,2),LEN(sb(2,1)))

sb(3,1) = "{Ord_Name                      }"
sb(3,2) = LIST_NAME
sb(3,2) = PADR(sb(3,2),LEN(sb(3,1)))

sb(4,1) = "{C}"
sb(4,2) = "   "

sb(5,1) = "{Arm           }"
sb(5,2) = WHERE

sb(6,1) = "{Salesman                      }"
sb(6,2) = WHO

sb(7,1) = "{Stockman                      }"
sb(7,2) = STO_MARK

sb(8,1) = "{Time  }"
sb(8,2) = ORD_TIME

sb(9,1) = "{Note                  }"
sb(9,2) = NOTE

sb(10,1) = "{Inc_List}"
sb(10,2) = SUBSTR(i_list,2)
sb(10,2) = LEFT(sb(10,2), LEN(sb(10,2))-1)

sb(11,1) = "{Exp_List}"
IF EMPTY(e_list)
  sb(11,2) = ""
ELSE
  sb(11,2) = SUBSTR(e_list,2)
  sb(11,2) = LEFT(sb(11,2), LEN(sb(11,2))-1)
ENDIF

sb(12,1) = "{Time ready  }"
sb(12,2) = IIF(EMPTY(REQ_DATE), PADR("¥ á®¡¨à âì!", 14), ;
               DTOC(REQ_DATE)+" "+LEFT(REQ_TIME,5) )

sb(13,1) = "{LIST_ADDR                         }"
sb(13,2) = lt_addr

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

USE (base_path+"ARM_LST") AGAIN ALIAS TMP000
LOCATE FOR sb(5,2) = ARM_C
IF FOUND()
  sb(5,2) = ARM_N
ELSE
  sb(5,2) = ""
ENDIF
sb(5,2) = PADR(sb(5,2),LEN(sb(5,1)))

USE (base_path+"DOC_FORM")
LOCATE FOR "ORDER_S" == ALLTRIM(UPPER(DOC_NAME))
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
CREATE DBF (tmp_f) ;
  ( CODE    N( 7), ;
    ADDRESS C( 8), ;
    NAME    C(30), ;
    QNT     N( 6), ;
    PROD    C( 8), ;
    QNT_R   N( 6), ;
    ATTEN   C(1),  ;
    A_ADDR  C(16)  )
USE (tmp_f) ALIAS P_TMP
INDEX ON ADDRESS TAG ADDRESS
INDEX ON CODE TAG CODE ADDITIVE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE AGAIN ALIAS S000

SELECT 0
USE (base_path+"ORD_D") ORDER TAG DOC_NUM AGAIN ALIAS TMP000
SET RELATION TO CODE INTO S000
SEEK STR(d_n0,6)
SCAN REST WHILE LIST_NUM = STR(d_n0,6)
  IF .NOT. " "+ALLTRIM(STR(ORD_NUM))+"," $ i_list
    LOOP
  ENDIF
  SELECT P_TMP
  IF SEEK(TMP000.CODE)
    REPLACE QNT WITH QNT+TMP000.QNT
  ELSE
    APPEND BLANK
    REPLACE CODE    WITH S000.CODE, ;
            NAME    WITH S000.PREFIX+" "+S000.NAME, ;
            ADDRESS WITH S000.ADDRESS, ;
            A_ADDR  WITH S000.MEMO_ST, ;
            PROD    WITH S000.PRODUCER,;
            QNT_R   WITH S000.QNT_ROUND,;
            ATTEN   WITH P_Bound(S000.PRICE),;
            QNT     WITH TMP000.QNT
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
SET FILTER TO QNT # 0
SET ORDER TO ADDRESS
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
  n_p = ALLTRIM(NAME)+"___"+ALLTRIM(STR(QNT,6))+n_p
  n_p = PADR(n_p, 42)
  stw = V_Addr(ADDRESS)+ATTEN+n_p+" "+PROD+" "+IIF(EMPTY(SUBSTR(A_ADDR,10,7)),LEFT(A_ADDR,9),LEFT(A_ADDR,8)+"+")  && ATTEN
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
