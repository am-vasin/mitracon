*********************************************************************
*                                                                   *
*                          Печать акта доставки                     *
*                                                                   *
*********************************************************************
PROCEDURE P_Act_D
PARAMETERS f_code, doc_n, doc_d
*  f_code   - фирма;
*  doc_n    - номер документа;
*  doc_d    - либо дата накладной либо год в символьном или числовом формате;
*

IF EMPTY(doc_n)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Счета-фактуры нет! И быть не может!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_d, t_p, f_p, f_d, d_year, k00, p_drctry
PRIVATE sel_rem, f_name, sb, n_p_det, n_p_det1, n_pg, f_pg, s_pg, str_w, nds, nn
PRIVATE dep_nam, dep_cod, ffeed, a_code, r_sv, sum_w_n, sum_w_0, sum_w_h
PRIVATE rem_firm, rem_doc, rem_date, rem_type
PRIVATE wDoc, wFirm, wDate
PRIVATE servNm, sum_p_n, no_act
STORE 0 TO sum_w_n, sum_w_0, sum_w_h
servNm = ""

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

PRIVATE sum_w1, sum_w2, sum_w3, sum_p1, sum_p2, sum_p3
PRIVATE d_n, d_d, ord_txt, l_face, l_dep
d_n = LEFT(doc_n,1)+"-"+ALLTRIM(SUBSTR(doc_n,2))

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS INV_TTT AGAIN
k00 = f_code+d_year+doc_n
IF .NOT. SEEK(k00)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет счета-фактуры с номером "+d_n
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_TEST AGAIN
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS SALE_TEST AGAIN
no_act = .T.
SEEK k00
SCAN REST WHILE FIRM+LEFT(DTOS(DOC_DATE), 4)+DOC_NUM = k00
  = SEEK(CODE, "STO_TEST")
  IF .NOT. EMPTY(STO_TEST.WK_SW2)
    no_act = .F.
    EXIT
  ENDIF
ENDSCAN
USE IN STO_TEST
USE IN SALE_TEST
SELECT INV_TTT
IF no_act
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Накладная не содержит услуг"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
wFirm = FIRM
wDoc  = DOC_NUM
wDate = DOC_DATE
d_d = TRANSFORM(DOC_DATE,"@D")
a_code = WHO
dep_cod = CUS_CODE
IF EMPTY(dep_cod)
  dep_nam = CUS_NAME
ELSE
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC000 AGAIN
  SEEK dep_cod
  dep_nam = CUS_NAME
ENDIF
PRIVATE c_parms
DIMENSION c_parms(13)
c_parms = ""
DO Prep_Cus
IF ALIAS() = "ACC000"
  USE
ENDIF

DO Wt_Mess WITH "Подготовка документа"
DO Prep_Lst
DO Wt_Mess

DIMENSION sb(47,2)
sb( 1,1) = "{Doc_No   }"
sb( 1,2) = d_n

sb( 2,1) = "{Doc_Dat }"
sb( 2,2) = d_d

sb( 3,1) = "{Department                                      }"
sb( 3,2) = c_parms(1)

sb( 4,1) = "{Address                                                   }"
sb( 4,2) = c_parms(2)

sb( 5,1) = "{Phone         }"
sb( 5,2) = c_parms(3)

sb( 6,1) = "{Acc_Bill}"
sb( 6,2) = c_parms(4)

sb( 7,1) = "{Bank                                  }"
sb( 7,2) = c_parms(5)

sb( 8,1) = "{Corr_Bil}"
sb( 8,2) = c_parms(6)

sb( 9,1) = "{BIK     }"
sb( 9,2) = c_parms(7)

sb(10,1) = "{City          }"
sb(10,2) = c_parms(8)

sb(11,1) = "{INN       }"
sb(11,2) = c_parms(9)

sb(12,1) = "{OKO}"
sb(12,2) = c_parms(10)

sb(13,1) = "{OKPO  }"
sb(13,2) = c_parms(11)

sb(14,1) = "{Note                                                                  }"
sb(14,2) = c_parms(12)

sb(15,1) = "{КПП покуп.    }"
sb(15,2) = c_parms(13)

sum_p_n = Capital(Num2RStr(FLOOR(ABS(sum_w_h))))+" руб. "+ ;
STR((ABS(sum_w_h)-FLOOR(ABS(sum_w_h)))*100,2)+" коп."

sb(16,1) = "{Cумма прописью 1                      }"
str_w = ""
i = ""
DO Break_Str WITH sum_p_n, i, str_w, LEN(sb(16,1))
sb(16,2)   = i
sum_p_n = str_w

sb(17,1) = "{Cумма прописью 2                      }"
i = ""
DO Break_Str WITH sum_p_n, i, str_w, LEN(sb(16,1))
sb(17,2)   = i
sum_p_n = str_w

sb(18,1) = "{Cумма прописью 3                      }"
i = ""
DO Break_Str WITH sum_p_n, i, str_w, LEN(sb(16,1))
sb(18,2)   = i

sb(19,1) = "{Hаименование услуги                        }"
sb(19,2) = PADR(servNm, LEN(sb(19,1)))

SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS FRM_BILL AGAIN
SEEK INV_TTT.FIRM

sb(20, 1) = "{Руководитель          }"
sb(20, 2) = BOSS
sb(21, 1) = "{Гл. бухгалтер         }"
sb(21, 2) = ABAK
sb(22, 1) = "{Кассир                }"
sb(22, 2) = KASSIR

sb(23,1) = "{КПП прод.     }"
sb(23,2) = KPP

sb(24,1) = "{Firm                                            }"
sb(24,2) = ALLTRIM(L_NAME)

sb(25,1) = "{F_Address                                       }"
sb(25,2) = ALLTRIM(ADDRESS)

sb(26,1) = "{F_Phone   }"
sb(26,2) = ALLTRIM(PHONE)

sb(27,1) = "{F_INN   }"
sb(27,2) = INN
USE

sb(28,1) = "{%NDS}"
sb(28,2) = STR(INV_TTT.NDS_,5,1)
*-----------------------------------------
sb(29,1) = "{Sum0    }"
sb(29,2) = STR(sum_w_0,10,2)

sb(30,1) = "{SumnNDS }"
sb(30,2) = STR(sum_w_n,10,2)

sb(31,1) = "{SumTot  }"
sb(31,2) = STR(sum_w_h,10,2)
*-----------------------------------------
sb(32,1) = "{Autor                       }"
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF a_code = 0
  sb(32,2) = "САМ!"
ELSE
  IF SEEK (a_code)
    sb(32,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
  ELSE
    sb(32,2) = "?!!"
  ENDIF
ENDIF
sb(32,2) = PADR(sb(32,2),LEN(sb(32,1)))

USE

*
*  Атрибуты головного предприятия
sb(33,1) = ;
"{MDepartment                                     }"
sb(34,1) = ;
"{MAddress                                                  }"
sb(35,1) = ;
"{MPhone        }"
sb(36,1) = ;
"{MAcc_Bil}"
sb(37,1) = ;
"{MBank                 }"
sb(38,1) = ;
"{MCor_Bil}"
sb(39,1) = ;
"{MBIK    }"
sb(40,1) = ;
"{MCity         }"
sb(41,1) = ;
"{MINN      }"
sb(42,1) = ;
"{MOK}"
sb(43,1) = ;
"{MOKPO }"
sb(44,1) = ;
"{MКПП покуп.   }"

PRIVATE depMain
USE (base_path+"CUS_BIL") SHARED AGAIN ALIAS CB_6907 ORDER TAG INN
IF EMPTY(sb(15, 2))		&& Нет ИНН
  depMain = dep_cod
ELSE
  SEEK sb(15, 2)
  LOCATE REST WHILE sb(15, 2) = INN FOR MAIN_FIRM
  IF FOUND()
    depMain = CUS_CODE
  ELSE
    depMain = dep_cod
  ENDIF
ENDIF

IF depMain = dep_cod		&& Нет головного предпр.
                            &&   копируем атрибуты клиента
  sb(33,2) = sb( 3,2)
  sb(34,2) = sb( 4,2)
  sb(35,2) = sb( 5,2)
  sb(36,2) = sb( 6,2)
  sb(37,2) = sb( 7,2)
  sb(38,2) = sb( 8,2)
  sb(39,2) = sb( 9,2)
  sb(40,2) = sb(10,2)
  sb(41,2) = sb(11,2)
  sb(42,2) = sb(12,2)
  sb(43,2) = sb(13,2)
  sb(44,2) = sb(15,2)
ELSE
  sb(33,2) = CUS_NAME
  sb(34,2) = ADDRESS
  sb(35,2) = PHONE
  sb(36,2) = ACC_BILL
  sb(37,2) = BANK
  sb(38,2) = CORR_BILL
  sb(39,2) = BIK
  sb(40,2) = CITY
  sb(41,2) = INN
  sb(42,2) = OKOHX
  sb(43,2) = OKPO
  sb(44,2) = KPP
  IF EMPTY(CUS_NAME)
    USE (base_path+"CUS_BIL") SHARED AGAIN ALIAS AC_6907 ORDER TAG CUS_CODE
    SEEK depMain
    sb(33,2) = CUS_NAME
  ENDIF
ENDIF
USE

sum_p_n = Capital(Num2RStr(FLOOR(ABS(sum_w_n))))+" руб. "+ ;
STR((ABS(sum_w_n)-FLOOR(ABS(sum_w_n)))*100,2)+" коп."
sb(45,1) = "{Cумма HДС прописью 1                      }"
str_w = ""
i = ""
DO Break_Str WITH sum_p_n, i, str_w, LEN(sb(16,1))
sb(45,2)   = i
sum_p_n = str_w

sb(46,1) = "{Cумма HДС прописью 2                      }"
i = ""
DO Break_Str WITH sum_p_n, i, str_w, LEN(sb(16,1))
sb(46,2)   = i
sum_p_n = str_w

sb(47,1) = "{Cумма HДС прописью 3                      }"
i = ""
DO Break_Str WITH sum_p_n, i, str_w, LEN(sb(16,1))
sb(47,2)   = i

n = ALEN(sb,1)
SELECT INV_TTT
USE DOC_FORM

LOCATE FOR "ACT_D" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(DOC_H)
IF n > 0
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_H)
  IF n > 0
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)
IF n > 0
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_F)
IF n > 0
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ENDIF
USE

DO Ini_Prn WITH "Акт N "+d_n, p_ln, lft, n_cp, fnt, ffeed, p_drctry

&& Заголовок первой страницы
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

DO Term_Prn WITH "", "", dep_cod, "ACT_D", wFirm, wDoc, wDate

SELECT (sel_rem)

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║              Программа подготовки содержимого списка.                 ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Prep_Lst

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST000 AGAIN
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS S000 AGAIN
SET RELATION TO CODE INTO ST000
SEEK INV_TTT.FIRM+LEFT(DTOS(INV_TTT.DOC_DATE),4)+INV_TTT.DOC_NUM
SCAN REST WHILE FIRM = INV_TTT.FIRM .AND. DOC_NUM = INV_TTT.DOC_NUM .AND. DOC_DATE = INV_TTT.DOC_DATE
  IF EMPTY(ST000.WK_SW2)
    LOOP
  ENDIF
  IF EMPTY(servNm)
    servNm = ALLTRIM(ST000.PREFIX)+ALLTRIM(ST000.NAME)+" "+ALLTRIM(ST000.PRODUCER)
  ENDIF
  sum_w_0 = sum_w_0+S000.PRICE_0*S000.QNT
  sum_w_h = sum_w_h+ROUND(S000.PRICE_R*S000.QNT,2)
ENDSCAN
USE
SELECT ST000
USE
sum_w_n = sum_w_h-sum_w_0

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║             Программа формирования атрибутов клиента.                 ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Prep_Cus
PRIVATE sel_sav

sel_sav = SELECT()
SELECT 0
USE CUS_BIL ORDER TAG CUS_CODE
IF EMPTY(dep_cod)
  DELETE FILE (tmpo_path+"CUS_BIL.DBF")
  DELETE FILE (tmpo_path+"CUS_BIL.CDX")
  COPY STRUCTURE TO (tmpo_path+"CUS_BIL") WITH CDX
  USE (tmpo_path+"CUS_BIL") ORDER TAG CUS_CODE
ENDIF

*DO Cus_Bil WITH dep_cod, dep_nam

IF .NOT. EMPTY(dep_cod)
  IF .NOT. SEEK(dep_cod)
    c_parms( 1) = dep_nam
    USE
    SELECT (sel_sav)
    RETURN
  ENDIF
ENDIF

IF EMPTY(CUS_NAME)
  c_parms( 1) = dep_nam
ELSE
  c_parms( 1) = CUS_NAME
ENDIF
c_parms( 2) = ADDRESS
c_parms( 3) = PHONE
c_parms( 4) = ACC_BILL
c_parms( 5) = BANK
c_parms( 6) = CORR_BILL
c_parms( 7) = BIK
c_parms( 8) = CITY
c_parms( 9) = INN
c_parms(10) = OKOHX
c_parms(11) = OKPO
c_parms(12) = NOTE
c_parms(13) = KPP
USE
SELECT (sel_sav)
IF EMPTY(dep_cod)
  DELETE FILE (tmpo_path+"CUS_BIL.DBF")
  DELETE FILE (tmpo_path+"CUS_BIL.CDX")
ENDIF

RETURN
