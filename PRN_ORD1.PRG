*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�         蹍������ �ョ�皋 ��錢�� �� 瓷��� � 筮爼�皀 甎�瓷�.            �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Prn_Ord1
PARAMETERS d_n, add_n
*
*  d_n   - ���ム 甎�瓷� ( 腮甄� );
*  add_n - ���ム ��錢�� ( 腮甄� ).
*

PRIVATE mss
DIMENSION mss(3)
mss[1] = ""
mss[2] = CHR(0)+"�皰� ぎ�祠キ� 竅��爛� � 竄��皋� 瓣�� ��矣��讚�痰�."
mss[3] = ""
DO Out_Mess WITH 7, "mss"
RETURN

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, stw, t_p, e_p, tmp_f, ffeed, p_drctry
PRIVATE sel_rem, sb, delim, cnt, prc_00
DIMENSION sb(12,2)

sel_rem = SELECT()

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

USE ORD_T ORDER TAG DOC_NUM AGAIN ALIAS TMP000
SEEK STR(d_n,6)+STR(add_n,3)
IF .NOT. FOUND()
  USE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���祠キ� �� ���ぅ� "+ALLTRIM(STR(d_n,6))+"/"+ALLTRIM(STR(add_n,3))
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

sb(12,1) = "{Collector                     }"
sb(12,2) = CHECK_S

sb(8,1) = "{Time  }"
sb(8,2) = ORD_TIME

sb(9,1) = "{Note                  }"
sb(9,2) = NOTE

sb(10,1) = "{Z}"
sb(10,2) = IIF(ZONE = "**", " ��.", IIF(ZONE == "  ", " �モ", " "+ZONE+" "))

sb(11,1) = "{Time ready  }"
sb(11,2) = IIF(EMPTY(REQ_DATE), PADR("�� 甌；��碎!", 14), ;
               DTOC(REQ_DATE)+" "+LEFT(REQ_TIME,5) )

sb(3,2) = LIST_NAME
sb(3,2) = PADR(sb(3,2),LEN(sb(3,1)))
delim = ALLTRIM(sb(1,2))+" �� "+ALLTRIM(sb(2,2))
delim = PADC(delim,44,"-")+"-"+PADC(delim,29,"-")
delim = REPLICATE("-",73)

USE PERSONS ORDER TAG CODE AGAIN ALIAS TMP000
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

sb(12,2) = VAL(sb(12,2))
IF sb(12,2) = 0
  sb(12,2) = "  "
ELSE
  SEEK sb(12,2)
  IF FOUND()
    sb(12,2) = ALLTRIM(FAMILY)
  ELSE
    sb(12,2) = "?"
  ENDIF
ENDIF
sb(12,2) = PADR(sb(12,2),LEN(sb(12,1)))

USE ARM_LST AGAIN ALIAS TMP000
LOCATE FOR sb(5,2) = ARM_C
IF FOUND()
  sb(5,2) = ARM_N
ELSE
  sb(5,2) = ""
ENDIF
sb(5,2) = PADR(sb(5,2),LEN(sb(5,1)))

USE DOC_FORM
LOCATE FOR "ORDER1" == ALLTRIM(UPPER(DOC_NAME))
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
CREATE DBF (tmp_f) (PREFIX C(4), NAME C(35), PRODUCER C(8), ;
                    ADDRESS C( 9), QNT N( 6))
USE (tmp_f) ALIAS P_TMP
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT 0
USE STOCK ORDER TAG CODE AGAIN ALIAS S000

SELECT 0
USE ORD_D ORDER TAG DOC_NUM AGAIN ALIAS TMP000
SET RELATION TO CODE INTO S000
SEEK STR(d_n,6)+STR(add_n,3)
SCAN REST WHILE LIST_NUM = STR(d_n,6) .AND. ORD_NUM = add_n
  SELECT P_TMP
  APPEND BLANK
  REPLACE PREFIX   WITH S000.PREFIX,;
          NAME     WITH S000.NAME,  ;
          PRODUCER WITH S000.PRODUCER,;
          ADDRESS  WITH S000.ADDRESS+IIF(EMPTY(S000.MEMO_ST), " ", "+"), ;
          QNT      WITH TMP000.QNT
  SELECT TMP000
ENDSCAN
USE
SELECT S000
USE

DO Ini_Prn WITH "��錢�� �� 瓷��� N "+sb(1,2), p_ln, lft, n_cp, fnt, ffeed, p_drctry

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
SCAN
  stw = V_Addr(ADDRESS)+" "+PREFIX+" "+NAME+" "+PRODUCER+STR(QNT,7)
  @ PROW()+1, 0 SAY stw
  cnt = (cnt+1)%5
  IF cnt = 0
    @ PROW()+1, 0 SAY delim
  ENDIF
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
