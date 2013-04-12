*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ   ฌ๏ ไ ฉซ  P_B_Req       งเ กฎโ็จช €ญคเฅฉ  แจญ                      บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                    ฅ็ โ์ โเฅกฎข ญจ๏ ข กใๅฃ ซโฅเจ๎.                    บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ 24.12.1998 ออผ
PROCEDURE P_B_Req

PARAMETERS doc_n, doc_d

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, l_num, sw, ffeed, p_drctry
PRIVATE t_d, d_year, n, sb, str_w
PRIVATE sel_rem

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

SELECT 0
USE (base_path+"BUHG_REQ") ORDER TAG DOC_NUM ALIAS _8C24 AGAIN
IF .NOT. SEEK(d_year+doc_n)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ โเฅกฎข ญจ๏ แ ญฎฌฅเฎฌ "+d_n
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS C00_8C24 AGAIN
SEEK _8C24.CUS_CODE

DIMENSION sb(17,2)
sb(1,1) = "{Ord_No}"
sb(1,2) = ALLTRIM(_8C24.DOC_NUM)

sb(2,1) = "{Ord_Dat }"
sb(2,2) = DTOC(_8C24.DOC_DATE)

sb(3,1) = "{List_N}"
sb(3,2) = ALLTRIM(STR(_8C24.LST_NUM))

sb(4,1) = "{Customer}"
sb(4,2) = ALLTRIM(C00_8C24.CUS_NAME)

sb(5,1) = "{D_Type}"
sb(5,2) = IIF(_8C24.DEP_TYPE = 1, "/", "—ฅช")

sb(6,1) = "{Cource}"
sb(6,2) = ALLTRIM(STR(_8C24.W_COURCE,7,3))

sb(7,1) = "{Get_USD   }"
sb(7,2) = ALLTRIM(STR(_8C24.USD_INC,13,4))

sb(8,1) = "{USD-Rub}"
sb(8,2) = ALLTRIM(STR(_8C24.USD_INC_R,10,2))

sb(9,1) = "{Get_Rub}"
sb(9,2) = ALLTRIM(STR(_8C24.R_INC,10,2))

sb(10,1) = "{Move_Ind   }"
sb(10,2) = ALLTRIM(STR(_8C24.IND_EXP,13,4))

sb(11,1) = "{Ind-Rub}"
sb(11,2) = ALLTRIM(STR(_8C24.IND_INC,10,2))

sb(17,1) = "{Cr Date }"
sb(17,2) = DTOC(_8C24.CREDIT_D)
sb(12,1) = "{Credit}"
sb(12,2) = ALLTRIM(STR(_8C24.CREDIT,10,2))
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_8C24 AGAIN
SEEK _8C24.WHO_REQ

sb(13,1) = "{Who_Req                     }"
sb(13,2) = IIF(_8C24.WHO_REQ = 0, "‘ ฌ!", ALLTRIM(P_8C24.FAMILY)+" "+ ;
                                          LEFT(P_8C24.NAME,1)+". "+   ;
                                          LEFT(P_8C24.S_NAME,1)+".")
sb(13,2) = PADR(sb(13,2), LEN(sb(13,1)))

sb(14,1) = "{Who_Inp                     }"
IF EMPTY(_8C24.DATE_INP)
  sb(14,2) = ""
ELSE
  SEEK _8C24.WHO_INP
  sb(14,2) = IIF(_8C24.WHO_INP = 0, "‘ ฌ!", ALLTRIM(P_8C24.FAMILY)+" "+ ;
                                            LEFT(P_8C24.NAME,1)+". "+   ;
                                            LEFT(P_8C24.S_NAME,1)+".")
ENDIF
sb(14,2) = PADR(sb(14,2), LEN(sb(14,1)))

sb(15,1) = "{Date_Inp}"
sb(15,2) = DTOC(_8C24.DATE_INP)


sb(16,1) = "{Firm    }"
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F00_8C24 AGAIN
SEEK _8C24.FIRM
sb(16,2) = ALLTRIM(L_NAME)

n = ALEN(sb,1)

USE DOC_FORM
LOCATE FOR "BUHG_REQ" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(DOC_H)
DIMENSION t_d(n)
FOR i = 1 TO n
  t_d(i) = MLINE(DOC_H,i)
ENDFOR
USE

SELECT _8C24
USE

DO Ini_Prn WITH "’เฅก. ข กใๅฃ. N "+sb(1,2), p_ln, lft, n_cp, fnt, ffeed, p_drctry

FOR i = 1 TO ALEN(t_d)
  str_w = t_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2))
  ENDFOR
  IF i = 1
    @ PROW(), PCOL() SAY str_w
  ELSE
    @ PROW()+1, 0 SAY str_w
  ENDIF
ENDFOR

DO Term_Prn WITH ""

SELECT (sel_rem)
RETURN
