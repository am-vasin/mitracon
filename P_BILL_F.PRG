*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                  Программа печати счета-фактуры.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE P_Bill_F
PARAMETERS f_code, doc_n, doc_d, doc_t, old_mode
*
*  f_code   - фирма;
*  doc_n    - номер документа;
*  doc_d    - либо дата накладной либо год в символьном или числовом формате;
*  doc_t    - тип док-та;
*  old_mode - не выделять услуги.
*
PRIVATE tmp_mode, inv_n

tmp_mode = old_mode
tmp_mode = .T.

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
PRIVATE dep_nam, dep_cod, ffeed, a_code, r_sv, sum_w_n
PRIVATE rem_firm, rem_doc, rem_date, rem_type
PRIVATE wDoc, wFirm, wDate
PRIVATE is_serv
is_serv = .F.

PRIVATE fs_names
IF File_O(base_path+"FS_FLAGS.MEM")
  RESTORE FROM (base_path+"FS_FLAGS") ADDITIVE
ELSE
  DIMENSION fs_names(8)
  fs_names(1) = SPACE(16)
  fs_names(2) = SPACE(16)
  fs_names(3) = SPACE(16)
  fs_names(4) = SPACE(16)
  fs_names(5) = SPACE(16)
  fs_names(6) = SPACE(16)
  fs_names(7) = SPACE(16)
  fs_names(8) = SPACE(16)
ENDIF

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
*IF doc_t = "0"
*  d_n = VAL(ALLTRIM(SUBSTR(doc_n,2)))
*  d_n = IIF(d_n > 500, 200+(d_n-200)%300, d_n)
*  d_n = LEFT(doc_n,1)+"-"+ALLTRIM(STR(d_n))
*ELSE
  d_n = LEFT(doc_n,1)+"-"+ALLTRIM(SUBSTR(doc_n,2))
*ENDIF

SELECT 0
*
*  Специальные надписи для ГТД
*
PRIVATE l_spec, gtd_spec, cnt_spec, lst_spec
USE (base_path+"B_GROUPS") ALIAS BG_0317 AGAIN
l_spec = 0
SCAN
  l_spec = l_spec+1
  DIMENSION gtd_spec(l_spec), cnt_spec(l_spec), lst_spec(l_spec)
  gtd_spec(l_spec) = GTD_NUM
  cnt_spec(l_spec) = COUNTRY
  lst_spec(l_spec) = DEV_LIST
ENDSCAN

USE (base_path+"SALE_TIT") ORDER TAG NEW_DOC ALIAS INV_TTT AGAIN
k00 = f_code+d_year+doc_t+doc_n
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
inv_n = DOC_NUM
wFirm = FIRM
wDoc  = DOC_NUM
wDate = DOC_DATE
rem_firm = FIRM
rem_doc  = PAY_DOC
rem_date = PAY_DATE
rem_type = PAY_TYPE
sum_w_n = ROUND(MIN(ABS(KASSA),ABS(TOTAL_R))*NSP_/100,2)
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

f_name = SYS(3)
DO Wt_Mess WITH "Подготовка документа"
DO Prep_Lst WITH f_name
DO Wt_Mess

DIMENSION sb(61,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Pg_Sum1 }"
sb( 2,2) = ""

sb( 3,1) = "{Pg_Sum2 }"
sb( 3,2) = ""

sb( 4,1) = "{Pg_Sum3 }"
sb( 4,2) = ""

sb( 5,1) = "{Doc_No   }"
sb( 5,2) = d_n

sb( 6,1) = "{Doc_Dat }"
sb( 6,2) = d_d

sb( 7,1) = "{Department                                      }"
sb( 7,2) = c_parms(1)

sb( 8,1) = "{Address                                                   }"
sb( 8,2) = c_parms(2)

sb( 9,1) = "{Phone         }"
sb( 9,2) = c_parms(3)

sb(10,1) = "{Acc_Bill}"
sb(10,2) = c_parms(4)

sb(11,1) = "{Bank                                  }"
sb(11,2) = c_parms(5)

sb(12,1) = "{Corr_Bil}"
sb(12,2) = c_parms(6)

sb(13,1) = "{BIK     }"
sb(13,2) = c_parms(7)

sb(14,1) = "{City          }"
sb(14,2) = c_parms(8)

sb(15,1) = "{INN       }"
sb(15,2) = c_parms(9)

sb(16,1) = "{OKO}"
sb(16,2) = c_parms(10)

sb(17,1) = "{OKPO  }"
sb(17,2) = c_parms(11)

sb(18,1) = "{Tot_Sum1}"
sb(18,2) = ""

sb(19,1) = "{Tot_Sum2}"
sb(19,2) = ""

sb(20,1) = "{Tot_Sum3}"
sb(20,2) = ""

sb(21,1) = "{Note                                                                  }"
sb(21,2) = c_parms(12)

sb(47,1) = "{КПП покуп.    }"
sb(47,2) = c_parms(13)

SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS FRM_BILL AGAIN
SEEK INV_TTT.FIRM

sb(44, 1) = "{Руководитель          }"
sb(44, 2) = BOSS
sb(45, 1) = "{Гл. бухгалтер         }"
sb(45, 2) = ABAK
sb(46, 1) = "{Кассир                }"
sb(46, 2) = KASSIR

sb(48,1) = "{КПП прод.     }"
sb(48,2) = KPP

sb(22,1) = "{Firm                                            }"
sb(22,2) = ALLTRIM(L_NAME)

sb(23,1) = "{F_Address                                       }"
sb(23,2) = ALLTRIM(ADDRESS)

sb(24,1) = "{F_Phone   }"
sb(24,2) = ALLTRIM(PHONE)

sb(25,1) = "{F_INN   }"
sb(25,2) = INN
USE

sb(26,1) = "{%NDS}"
sb(26,2) = STR(INV_TTT.NDS_,5,1)
*-----------------------------------------
sb(27,1) = "{SumN    }"
sb(27,2) = STR(sum_w_n,10,2)

sb(28,1) = "{Tot_SumN}"
sb(28,2) = STR(INV_TTT.TOTAL_R+SIGN(INV_TTT.TOTAL_R)*sum_w_n,10,2)

sb(29,1) = "{%NSP}"
sb(29,2) = ALLTRIM(STR(INV_TTT.NSP_,5,1))
*-----------------------------------------
sb(30,1) = "{Autor                       }"
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF a_code = 0
  sb(30,2) = "САМ!"
ELSE
  IF SEEK (a_code)
    sb(30,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
  ELSE
    sb(30,2) = "?!!"
  ENDIF
ENDIF
sb(30,2) = PADR(sb(30,2),LEN(sb(30,1)))


sb(31,1) = "{Flag 1            }"
sb(31,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 1, 1)), SPACE(LEN(sb(31,1))), fs_names(1))

sb(32,1) = "{Flag 2            }"
sb(32,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 2, 1)), SPACE(LEN(sb(32,1))), fs_names(2))

sb(33,1) = "{Flag 3            }"
sb(33,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 3, 1)), SPACE(LEN(sb(33,1))), fs_names(3))

sb(34,1) = "{Flag 4            }"
sb(34,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 4, 1)), SPACE(LEN(sb(34,1))), fs_names(4))

sb(35,1) = "{Flag 5            }"
sb(35,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 5, 1)), SPACE(LEN(sb(35,1))), fs_names(5))

sb(36,1) = "{Flag 6            }"
sb(36,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 6, 1)), SPACE(LEN(sb(36,1))), fs_names(6))

sb(37,1) = "{Flag 7            }"
sb(37,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 7, 1)), SPACE(LEN(sb(37,1))), fs_names(7))

sb(38,1) = "{Flag 8            }"
sb(38,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 8, 1)), SPACE(LEN(sb(38,1))), fs_names(8))

sb(39,1) = "{P. Date }"
sb(39,2) = "          "

sb(40,1) = "{P1. Date}"
sb(40,2) = "          "

sb(41,1) = "{P. Doc   }"
sb(41,2) = "           "

sb(42,1) = "{Ext. Pay          }"
sb(42,2) = "                "

sb(43,1) = "{Pay Type                 }"
sb(43,2) = "                           "

USE (base_path+"PARMS") SHARED AGAIN IN 0
sb(61,1) = "{Prikaz                       }"
sb(61,2) = ALLTRIM(PARMS.PRIKAZ)
sb(61,2) = LEFT(sb(61,2), LEN(sb(61,1)))
USE IN PARMS

IF .NOT. EMPTY(rem_doc)
  USE (base_path+"INCMONEY") ORDER TAG DOC_NUM ALIAS INV_PAY AGAIN
  IF SEEK(rem_firm+rem_type+LEFT(DTOS(rem_date),4)+rem_doc)
    IF .NOT. EMPTY(CUS_BANK)
      USE (base_path+"CUS_BANK") ORDER TAG BANK_CODE ALIAS ADD_BANK AGAIN IN 0
      IF SEEK(STR(INV_PAY.CUS_CODE)+STR(CUS_BANK), "ADD_BANK")
        sb(10,2) = ADD_BANK.ACC_BILL
        sb(11,2) = ADD_BANK.BANK
        sb(12,2) = ADD_BANK.CORR_BILL
        sb(13,2) = ADD_BANK.BIK
        sb(14,2) = ADD_BANK.CITY
      ENDIF
      USE IN ADD_BANK
    ENDIF

    sb(39,2) = DTOC(DOC_DATE)

    sb(40,2) = DTOC(IIF(EMPTY(ENTER_DAT), DOC_DATE, ENTER_DAT))

    sb(41,2) = LEFT(DOC_NUM,1)+ALLTRIM(SUBSTR(DOC_NUM,2))
    sb(41,2) = PADR(sb(41,2),11)

    sb(42,2) = ENTER_DOC

    sb(43,2) = "                           "
    DO CASE
    CASE EMPTY(DOC_TYPE)
      IF "-" $ DOC_NUM
        sb(43,2) = "Расходный ордер            "
      ELSE
        sb(43,2) = "Приходный ордер            "
      ENDIF
    CASE DOC_TYPE = "1"
      sb(43,2) = "Безналичная оплата         "
    CASE DOC_TYPE = "3"
      sb(43,2) = "Индексируемый платеж       "
    CASE DOC_TYPE = "Ч"
      sb(43,2) = "Оплата по чеку             "
    CASE DOC_TYPE = "Б"
      sb(43,2) = "Оплата через сбербанк      "
    ENDCASE
  ENDIF
ENDIF

USE

*
*  Атрибуты головного предприятия
sb(49,1) = ;
"{MDepartment                                     }"
sb(50,1) = ;
"{MAddress                                                  }"
sb(51,1) = ;
"{MPhone        }"
sb(52,1) = ;
"{MAcc_Bil}"
sb(53,1) = ;
"{MBank                 }"
sb(54,1) = ;
"{MCor_Bil}"
sb(55,1) = ;
"{MBIK    }"
sb(56,1) = ;
"{MCity         }"
sb(57,1) = ;
"{MINN      }"
sb(58,1) = ;
"{MOK}"
sb(59,1) = ;
"{MOKPO }"
sb(60,1) = ;
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
  sb(49,2) = sb( 7,2)
  sb(50,2) = sb( 8,2)
  sb(51,2) = sb( 9,2)
  sb(52,2) = sb(10,2)
  sb(53,2) = sb(11,2)
  sb(54,2) = sb(12,2)
  sb(55,2) = sb(13,2)
  sb(56,2) = sb(14,2)
  sb(57,2) = sb(15,2)
  sb(58,2) = sb(16,2)
  sb(59,2) = sb(17,2)
  sb(60,2) = sb(47,2)
ELSE
  sb(49,2) = CUS_NAME
  sb(50,2) = ADDRESS
  sb(51,2) = PHONE
  sb(52,2) = ACC_BILL
  sb(53,2) = BANK
  sb(54,2) = CORR_BILL
  sb(55,2) = BIK
  sb(56,2) = CITY
  sb(57,2) = INN
  sb(58,2) = OKOHX
  sb(59,2) = OKPO
  sb(60,2) = KPP
  IF EMPTY(CUS_NAME)
    USE (base_path+"CUS_BIL") SHARED AGAIN ALIAS AC_6907 ORDER TAG CUS_CODE
    SEEK depMain
    sb(49,2) = CUS_NAME
  ENDIF
ENDIF
USE

n = ALEN(sb,1)
*FOR i = 1 TO n
*  sb(i,2) =PADR(sb(i,2), LEN(sb(i,1)))
*ENDFOR
*-----------------------------------------------
PRIVATE new_format
new_format = 1
IF YEAR(doc_d) <= 2000
  new_format = 0
ENDIF
*IF YEAR(doc_d) = 3000  && Резервный формат счет-фактуры - "широкий";
*  new_format = 2       && в детальную строку позиции входят также
*ENDIF                  && "Страна происхождения" и "Номер ГТД"
*-----------------------------------------------
SELECT INV_TTT
USE DOC_FORM

IF new_format = 0
  LOCATE FOR "BILL_F"+IIF(doc_t = " ", "", "*") == ALLTRIM(UPPER(DOC_NAME))
ENDIF
IF new_format = 1
  LOCATE FOR "BILL_F1"+IIF(doc_t = " ", "", "*") == ALLTRIM(UPPER(DOC_NAME))
ENDIF
IF new_format = 2
  LOCATE FOR "BILL_F2"+IIF(doc_t = " ", "", "*") == ALLTRIM(UPPER(DOC_NAME))
ENDIF

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

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR
USE

DO Ini_Prn WITH "Счет-фактура N "+d_n, p_ln, lft, n_cp, fnt, ffeed, p_drctry

SELECT TMP_LST
GO TOP
nn = RECCOUNT()*2
n_p_det  = p_ln-ALEN(t_d)-MAX(ALEN(f_p),ALEN(f_d))
n_p_det1 = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
n_pg = 1

IF n_p_det <= 0  && Забили всю страницу под заголовки... ни каких страниц!
  n_p_det = RECCOUNT()*2+1
ELSE
  nn = MAX(nn-n_p_det, 0)
  n_pg = 1+CEILING(nn/n_p_det1)
ENDIF
nn = 0
r_sv = 0
STORE 0 TO sum_w1, sum_w2, sum_w3
IF n_pg # 0
FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1   && Заголовок НЕ первой страницы
    EJECT
    FOR i = 1 TO ALEN(t_p)
      n_p_det = n_p_det1
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
  ELSE        && Заголовок первой страницы
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
  STORE 0 TO sum_p1, sum_p2, sum_p3
*
*   Детальная часть страницы
*
  i = 0
  DO WHILE .T.
    IF r_sv # RECNO()
      r_sv = RECNO()
      nn = nn+1
      IF new_format = 0
        @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+"         шт. "+ ;
                       STR(QNT,7)+STR(PRC_0,8,2)+"   0"+STR(S0,11,2)+ ;
                       " 0"+sb(26,2)+STR(S1-S0,11,2)+STR(S1,11,2)
      ENDIF
      IF new_format = 1
        @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+" шт "+  ;
                     STR(QNT,7)+STR(PRC_0,8,2)+STR(S0,11,2)+"   0   "+  ;
                     sb(26,2)+STR(S1-S0,11,2)+STR(S1,11,2)
      ENDIF
      IF new_format = 2
        @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+" шт. "+  ;
                     STR(QNT,7)+STR(PRC_0,8,2)+STR(S0,11,2)+" 0 "+  ;
                     sb(26,2)+STR(S1-S0,11,2)+STR(S1,11,2)+"  "+ ;
                     COUNTRY+" "+GTD_NUM+"/"+ ;
                     GTD_LINE	&&+STR(CONT_GTD.QNT,7)
      ENDIF
      sum_p1 = sum_p1+S0
      sum_p2 = sum_p2+S1-S0
      sum_p3 = sum_p3+S1
      i = i+1
      IF  i >= IIF(f_pg = 1, n_p_det, n_p_det1)
        EXIT
      ENDIF
    ENDIF
    IF new_format = 0
      @ PROW()+1,0 SAY "    Страна "+COUNTRY+"    ГТД N "+GTD_NUM+"/"+GTD_LINE	&&+STR(CONT_GTD.QNT,7)
    ENDIF
    IF new_format = 1
&&      @ PROW()+1,43 SAY "Страна "+COUNTRY+"    ГТД N "+GTD_NUM+"/"+GTD_LINE	&&+STR(CONT_GTD.QNT,7)
      @ PROW()+1,0 SAY SPACE(50)+  ;
          PADL("Страна "+COUNTRY+" ГТД N "+GTD_NUM+"/"+GTD_LINE,70)
    ENDIF
    i = i+1
    SKIP
    IF EOF("TMP_LST")
      EXIT
    ENDIF
    IF  i >= IIF(f_pg = 1, n_p_det, n_p_det1)
      EXIT
    ENDIF
  ENDDO
*
*    Окончание страницы
*
  sum_w1 = sum_w1+sum_p1
  sum_w2 = sum_w2+sum_p2
  sum_w3 = sum_w3+sum_p3
  sb( 2,2) =PADR(STR(sum_p1,10,2), LEN(sb(2,1)))
  sb( 3,2) =PADR(STR(sum_p2,10,2), LEN(sb(3,1)))
  sb( 4,2) =PADR(STR(sum_p3,10,2), LEN(sb(4,1)))
  IF f_pg = n_pg
    sb(18,2) =PADR(STR(sum_w1,10,2), LEN(sb(18,1)))
    sb(19,2) =PADR(STR(sum_w2,10,2), LEN(sb(19,1)))
    sb(20,2) =PADR(STR(sum_w3,10,2), LEN(sb(20,1)))
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
ENDIF

PRIVATE tmpName
tmpName = SYS(3)
COPY FIELDS PREFIX, NAME, PRODUCER, QNT, PRC_0, PRC, GTD_NUM,  ;
            COUNTRY, GTD_LINE TO (tmpo_path+tmpName)
USE

DO Term_Prn WITH "", (tmpo_path+tmpName+".DBF"), dep_cod, "BILLF", wFirm, wDoc, wDate

DELETE FILE (tmpo_path+tmpName+".DBF")
DELETE FILE (tmpo_path+f_name+".DBF")
DELETE FILE (tmpo_path+f_name+".CDX")

SELECT (sel_rem)

IF is_serv
  DO P_Act_D WITH f_code, inv_n, doc_d
ENDIF

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║              Программа подготовки содержимого списка.                 ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Prep_Lst
PARAMETERS f_nam
PRIVATE p1, p2, c_rem, j, tmpPoint, tmpVal
PRIVATE name_out, pref_out                     && NEW 7.02.2002


SELECT 0
CREATE DBF (tmpo_path+f_nam);
	(	CODE		N( 7),	;
		PREFIX		C( 4),	;
		NAME		C(35),	;
		PRODUCER	C( 8),	;
		GTD_NUM		C(26),	;
		COUNTRY		C(16),	;
		GTD_LINE	C( 4),	;
		QNT			N( 7),	;
		PRC_0		N( 8,2),;
		PRC			N(12,5),;
		S0			N(10,2),;
		S1			N(10,2)	)
USE (tmpo_path+f_nam) ALIAS TMP_LST EXCLUSIVE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST000 AGAIN
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS S000 AGAIN
SET RELATION TO CODE INTO ST000
SEEK INV_TTT.FIRM+LEFT(DTOS(INV_TTT.DOC_DATE),4)+INV_TTT.DOC_NUM
c_rem = 0
SCAN REST WHILE FIRM = INV_TTT.FIRM .AND. DOC_NUM = INV_TTT.DOC_NUM .AND. DOC_DATE = INV_TTT.DOC_DATE
  IF .NOT. tmp_mode
    IF .NOT. EMPTY(ST000.WK_SW2)
      is_serv = .T.
      LOOP
    ENDIF
  ENDIF
  p1 = S000.PRICE_0
  P2 = S000.PRICE_R
  SELECT TMP_LST
*  IF c_rem = S000.CODE
*    REPLACE QNT     WITH QNT+S000.QNT
*    REPLACE S0      WITH p1*QNT,  ;
*            S1      WITH ROUND(p2*QNT,2)
*  ELSE
     APPEND BLANK
     name_out = ST000.NAME                        && NEW 7.02.2002
     pref_out = ST000.PREFIX                      && NEW 7.02.2002
     REPLACE CODE    WITH S000.CODE,  ;
             PREFIX  WITH pref_out,   ;
             NAME    WITH name_out,   ;
             PRODUCER WITH ST000.PRODUCER, ;
             QNT     WITH S000.QNT,  ;
             PRC_0   WITH p1,        ;
             PRC     WITH p2,        ;
             S0      WITH p1*S000.QNT, ;
             S1      WITH ROUND(p2*S000.QNT,2)
    tmpPoint = 1
    tmpVal = " "+ALLTRIM(STR(ST000.PRO_CODE))+" "
    IF l_spec > 1
      FOR j = 2 TO l_spec
        IF tmpVal $ lst_spec(j)
          tmpPoint = j
          EXIT
        ENDIF
      ENDFOR
    ENDIF
    IF EMPTY(S000.GTD_NUM) .OR. tmpPoint # 1
      REPLACE CODE    WITH S000.CODE,          ;
              GTD_NUM WITH gtd_spec(tmpPoint), ;
              COUNTRY WITH cnt_spec(tmpPoint), ;
              QNT     WITH S000.QNT
    ELSE
      REPLACE CODE     WITH S000.CODE,    ;
              GTD_NUM  WITH S000.GTD_NUM, ;
              COUNTRY  WITH S000.COUNTRY, ;
              GTD_LINE WITH S000.GTD_LINE,;
              QNT      WITH S000.QNT
    ENDIF
    c_rem = S000.CODE
*  ENDIF
  SELECT S000
ENDSCAN
USE
SELECT ST000
USE
SELECT TMP_LST

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

DO Cus_Bil WITH dep_cod, dep_nam

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
