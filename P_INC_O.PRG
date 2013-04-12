*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║              Программа печати прихода денег ( всякого ).              ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE P_Inc_O
PARAMETERS d_num, d_date, d_type
*
*  d_num  - номер приходного ордера;
*  d_date - дата приходного ордера;
*  d_type - тип приходного ордера.
*

PRIVATE d_year, k00, sw, sb, sel_rem, str_w, str_w1, face_nam
PRIVATE psp_no, psp_sr, psp_l1, psp_l2, psp_l3, frm

PRIVATE p_ln, fnt, n_cp, lft, t_p, doc_tp, doc_nm, ffeed, p_drctry

doc_tp = " "

IF TYPE("d_date") = "D"
  d_year = LEFT(DTOS(d_date),4)
ENDIF
IF TYPE("d_date") = "N"
  d_year = STR(d_date,4)
ENDIF
IF TYPE("d_date") = "C"
  d_year = d_date
ENDIF

sel_rem = SELECT()
PRIVATE sum_w, sum_d, sum_p, d_n, d_d, inv_n, inv_d, i, n, j
*IF d_type = "2"
*  d_n = VAL(ALLTRIM(SUBSTR(d_num,2)))
*  d_n = LEFT(d_num,1)+ALLTRIM(STR(IIF(d_n > 500, 200+(d_n-200)%300, d_n)))
*ELSE
  d_n = LEFT(d_num,1)+"-"+ALLTRIM(SUBSTR(d_num,2))
*ENDIF

SELECT 0
USE INCMONEY ORDER TAG DOC_NUM ALIAS ORD_TTT AGAIN

k00 = d_type+d_year+d_num
IF .NOT. SEEK(k00)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет документа с номером "+d_n
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
frm = FIRM

DO CASE
CASE MONEY_R => 0 .AND. d_type = " "
  doc_tp = "MONEY_ORD"
  doc_nm = "Прих. ордер N"
CASE MONEY_R < 0 .AND. d_type = " "
  doc_tp = "MONEY_EXP"
  doc_nm = "Расх. ордер N"
CASE MONEY_R => 0 .AND. d_type = "2"
  doc_tp = "MONEY_ORD*"
  doc_nm = "Прих. док-т N"
CASE MONEY_R < 0 .AND. d_type = "2"
  doc_tp = "MONEY_EXP*"
  doc_nm = "Расх. док-т N"
CASE MONEY_R => 0 .AND. d_type = "1"
  doc_tp = "PAY1"
  doc_nm = "Прих. Б/Н N"
CASE MONEY_R < 0 .AND. d_type = "1"
  doc_tp = "PAY1-"
  doc_nm = "Расх. Б/Н N"
CASE MONEY_R => 0 .AND. d_type = "3"
  doc_tp = "PAY2"
  doc_nm = "Приход USD Б/Н N"
CASE MONEY_R < 0 .AND. d_type = "3"
  doc_tp = "PAY2-"
  doc_nm = "Расход USD Б/Н N"
ENDCASE

d_d   = TRANSFORM(IIF(d_type = "1", DOC_DATE, ENTER_DAT),"@D")
sum_w = MONEY_R
sum_d = MONEY_D
inv_n = "б/н"
inv_d = TRANSFORM(SALE_DATE,"@D")
face_nam = CONTACT
psp_no  = PSP_SER
psp_sr  = PSP_NUM
psp_l1  = PSP_WH1
psp_l2  = PSP_WH2
psp_l3  = PSP_WH3
dep_nam = CUS_NAME
IF (EMPTY(face_nam) .OR. EMPTY(psp_no)) .AND. INLIST(DOC_TYPE, " ", "2");
                    .AND. sum_w < 0
  DO G_Contact
ENDIF

sum_p = Capital(Num2RStr(FLOOR(ABS(sum_w))))+" руб. "+ ;
STR((ABS(sum_w)-FLOOR(ABS(sum_w)))*100,2)+" коп."

DIMENSION sb(34,2)

sb( 1, 1) = "{N_Ord    }"
sb( 1, 2) = d_n

sb( 2, 1) = "{D_Ord   }"
sb( 2, 2) = d_d

sb( 3, 1) = "{Department                          }"
sb( 3, 2) = ALLTRIM(dep_nam)

str_w = sb( 3, 2)
str_w1 = ""

sb( 4, 1) = "{Dep1          }"
i = ""
DO Break_Str WITH str_w, i, str_w1, LEN(sb(4,1))

sb( 4, 2)   = i
str_w = str_w1

sb( 5, 1) = "{Dep2                     }"
DO Break_Str WITH str_w, i, str_w1, LEN(sb(5,1))
sb( 5, 2) = i
str_w = str_w1

sb( 6, 1) = "{Dep3                     }"
DO Break_Str WITH str_w, i, str_w1, LEN(sb(6,1))
sb( 6, 2) = i

sb( 7, 1) = "{Face                }"
sb( 7, 2) = face_nam

sb( 8, 1) = "{Inv_No   }"
sb( 8, 2) = inv_n

sb( 9, 1) = "{Inv_Date}"
sb( 9, 2) = inv_d

sb(10, 1) = "{Sum     }"
sb(10, 2) = STR(ABS(sum_w),LEN(sb(10, 1)),2)

str_w = sum_p
str_w1 = ""

sb(11, 1) = "{Str_Sum_S1               }"
DO Break_Str WITH str_w, i, str_w1, LEN(sb(11,1))
sb(11, 2) = i
str_w = str_w1

sb(12, 1) = "{Str_Sum_S2               }"
DO Break_Str WITH str_w, i, str_w1, LEN(sb(12,1))
sb(12, 2) = i
str_w = str_w1

sb(13, 1) = "{Str_Sum_S3               }"
DO Break_Str WITH str_w, i, str_w1, LEN(sb(13,1))
sb(13, 2) = i
str_w = str_w1

sb(14, 1) = "{Str_Sum_S4               }"
DO Break_Str WITH str_w, i, str_w1, LEN(sb(14,1))
sb(14, 2) = i

str_w = sum_p
str_w1 = ""

sb(15, 1) = "{Str_Sum1                                       }"
DO Break_Str WITH str_w, i, str_w1, LEN(sb(15,1))
sb(15, 2) = i
str_w = str_w1

sb(16, 1) = "{Str_Sum2                                       }"
DO Break_Str WITH str_w, i, str_w1, LEN(sb(16,1))
sb(16, 2) = i
str_w = str_w1

sb(17, 1) = "{Str_Sum3                                       }"
DO Break_Str WITH str_w, i, str_w1, LEN(sb(17,1))
sb(17, 2) = i
str_w = str_w1

sb(18, 1) = "{Str_Sum4                                       }"
DO Break_Str WITH str_w, i, str_w1, LEN(sb(18,1))
sb(18, 2) = i

sb(19, 1) = "{Bill_N  }"
sb(19, 2) = LEFT(BILL_NUM,4)+"-"+ALLTRIM(SUBSTR(BILL_NUM,5))

sb(20, 1) = "{Date2   }"
sb(20, 2) = TRANSFORM(BILL_DAT, "@D")

sb(21, 1) = "{Note          }"
sb(21, 2) = ENTER_DOC

sb(22, 1) = "{Date3   }"
sb(22, 2) = TRANSFORM(ENTER_DAT, "@D")

sb(23, 1) = "{Sum_D   }"
sb(23, 2) = STR(ABS(sum_d),LEN(sb(23, 1)),4)

sb(24, 1) = "{NDS     }"
sb(24, 2) = STR(ROUND(0.2*ABS(sum_w)/1.2,2), LEN(sb(24, 1)),2)

sb(25, 1) = "{Who           }"

sb(26,1) = "{Cource}"
IF MONEY_D # 0
  sb(26,2) = STR(ROUND(ABS(MONEY_R/MONEY_D),3),8,3)
ELSE
  sb(26,2) = "        "
ENDIF

p_ln = WHO
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE AGAIN ALIAS TMPFIRM
SEEK frm
sb(32, 1) = "{Руководитель          }"
sb(32, 2) = BOSS
sb(33, 1) = "{Гл. бухгалтер         }"
sb(33, 2) = ABAK
sb(34, 1) = "{Кассир                }"
sb(34, 2) = KASSIR

USE USERS ORDER TAG USER_CODE ALIAS USER0 AGAIN
IF p_ln = 0
  sb(25, 2) = "Сам!"
ELSE
  IF SEEK(p_ln)
    sb(25, 2) = USER_NAME
  ELSE
    sb(25, 2) = ""
  ENDIF
ENDIF

sb(27, 1) = "{Пасп N}"
sb(27, 2) = psp_no

sb(28, 1) = "{ПасС}"
sb(28, 2) = psp_sr

sb(29, 1) = "{Паспорт выдан 1                       }"
sb(29, 2) = psp_l1

sb(30, 1) = "{Паспорт выдан 2                       }"
sb(30, 2) = psp_l2

sb(31, 1) = "{Паспорт выдан 3                       }"
sb(31, 2) = l3

n = ALEN(sb,1)

FOR i = 1 TO n
  sb(i,2) = PADR(sb(i,2), LEN(sb(i,1)))
ENDFOR

USE DOC_FORM

LOCATE FOR doc_tp == ALLTRIM(UPPER(DOC_NAME))
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

USE

DO Ini_Prn WITH "Прих. ордер N "+sb(1,2), p_ln, lft, n_cp, fnt, ffeed, p_drctry

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

DO Term_Prn WITH ""

SELECT (sel_rem)
RETURN

*************************************************************************
PROCEDURE G_Contact

PRIVATE ln, wd, ex, mss, c1

IF .NOT. EMPTY(face_nam) .AND. .NOT. EMPTY(psp_no)
  RETURN
ENDIF

DO Pasport WITH CUS_CODE, dep_nam, face_nam, psp_sr, psp_no, psp_l1, psp_l2, psp_l3

PRIVATE s_sav

REPLACE CONTACT  WITH face_nam, ; 
        CUS_NAME WITH dep_nam,  ;
        PSP_NUM  WITH psp_no,   ;
        PSP_SER  WITH psp_sr,   ;
        PSP_WH1  WITH psp_l1,   ;
        PSP_WH2  WITH psp_l2,   ;
        PSP_WH3  WITH psp_l3

IF .NOT. EMPTY(SALE_NUM)
  s_sav = SELECT()
  SELECT 0
  USE SALE_TIT ORDER TAG DOC_NUM ALIAS S12345 AGAIN
  IF SEEK(LEFT(DTOS(ORD_TTT.SALE_DATE),4)+ORD_TTT.SALE_NUM)
    REPLACE CONTACT WITH face_nam, CUS_NAME WITH dep_nam
  ENDIF
  USE
  SELECT (s_sav)
ENDIF

RETURN .T.
