*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ฏฅ็ โจ แ็ฅโ -ไ ชโใเ๋.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE P_BF_Fr1
PARAMETERS doc_n, doc_d, doc_t
*
*  doc_n - ญฎฌฅเ คฎชใฌฅญโ ;
*  doc_d - ซจกฎ ค โ  คฎช-โ  ซจกฎ ฃฎค ข แจฌขฎซ์ญฎฌ จซจ ็จแซฎขฎฌ ไฎเฌ โฅ;
*  doc_t - โจฏ คฎช-โ .
*

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_d, t_p, f_p, f_d, d_year, k00
PRIVATE sel_rem, f_name, sb, str_w, nds, nn
PRIVATE dep_nam, ffeed, p_drctry

sel_rem = SELECT()

IF TYPE("doc_d") = "D"
  d_year = LEFT(DTOS(doc_d),4)
ENDIF
IF TYPE("doc_d") = "N"
  d_year = STR(doc_d,4)
ENDIF
IF TYPE("doc_d") = "C"
  d_year = doc_d
ENDIF

PRIVATE sum_w1, sum_w2, sum_w3, num_type
PRIVATE d_n, d_d, ord_txt, l_face, l_dep

SELECT 0
IF TYPE("doc_t") = "C"
  USE (base_path+"SALE_TIT") ORDER TAG NEW_DOC ALIAS BIL_TTT AGAIN
  k00 = d_year+doc_t+doc_n
ELSE
  USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS BIL_TTT AGAIN
  k00 = d_year+doc_n
ENDIF
IF .NOT. SEEK(k00)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ แ็ฅโ -ไ ชโใเ๋ แ ญฎฌฅเฎฌ "+d_n
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
d_n = LEFT(doc_n,1)+"-"+ALLTRIM(SUBSTR(doc_n,2))
sum_w2 = TOTAL_NDS
sum_w1 = TOTAL_0
sum_w3 = TOTAL_R
sum_w1 = STR(sum_w1,10,2)
sum_w2 = STR(sum_w2,10,2)
sum_w3 = STR(sum_w3,10,2)

d_d = TRANSFORM(DOC_DATE,"@D")

dep_nam = CUS_NAME

DIMENSION sb(20,2)

sb( 1,1) = "{Doc_No   }"
sb( 1,2) = d_n

sb( 2,1) = "{Doc_Dat }"
sb( 2,2) = d_d

sb( 3,1) = "{Department                                      }"
sb( 3,2) = CUS_NAME

sb( 4,1) = "{Address                                                   }"
sb( 4,2) = ADDRESS

sb( 5,1) = "{Phone         }"
sb( 5,2) = PHONE

sb( 6,1) = "{Acc_Bill}"
sb( 6,2) = ACC_BILL

sb( 7,1) = "{Bank                                  }"
sb( 7,2) = BANK

sb( 8,1) = "{Corr_Bil}"
sb( 8,2) = CORR_BILL

sb( 9,1) = "{BIK     }"
sb( 9,2) = BIK

sb(10,1) = "{City          }"
sb(10,2) = CITY

sb(11,1) = "{INN       }"
sb(11,2) = INN

sb(12,1) = "{OKO}"
sb(12,2) = OKOHX

sb(13,1) = "{OKPO  }"
sb(13,2) = OKPO

sb(14,1) = "{Tot_Sum1}"
sb(14,2) = sum_w1

sb(15,1) = "{Tot_Sum2}"
sb(15,2) = sum_w2

sb(16,1) = "{Tot_Sum3}"
sb(16,2) = sum_w3

sb(17,1) = "{Goods                 }"
sb(17,2) = GOODS

sb(18,1) = "{Invoice }"
sb(18,2) = INV_NUM

sb(19,1) =;
"{Note                                                                  }"
sb(19,2) = NOTE

sb(20,1) ="{Prc}"
sb(20,2) = STR(PERCENT,5,1)

n = ALEN(sb,1)
*FOR i = 1 TO n
*  sb(i,2) = PADR(sb(i,2), LEN(sb(i,1)))
*ENDFOR

USE DOC_FORM
LOCATE FOR "B_F_FREE"+IIF(doc_t = " ", "", "*") == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(DOC_H)
DIMENSION t_d(n)
FOR i = 1 TO n
  t_d(i) = MLINE(DOC_H,i)
ENDFOR

DO Ini_Prn WITH "‘็ฅโ-ไ ชโใเ  N "+d_n, p_ln, lft, n_cp, fnt, ffeed, p_drctry

nn = RECCOUNT()

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
USE

DO Term_Prn WITH ""

SELECT (sel_rem)
RETURN
