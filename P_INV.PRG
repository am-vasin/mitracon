*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                   Программа печати накладной.                         ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE P_Inv
PARAMETERS f_code, inv_n, inv_d, flg, old_mode  && tAltName && NEW 7.02.2002
*
*  f_code - код предприятия;
*  inv_n  - номер накладной ( внутренний формат );
*  inv_d  - либо дата накладной либо год в символьном или числовом формате;
*  flg    - необязательный параметр. Может содержать цифру - формат накладной и "I" - внутр. формат
*  tAltName - необязательный параметр, если tAltname = .T., то вместо имен поз. печатаются альт.имена
*

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, l_num, d_ver
PRIVATE t_p, f_p, f_d, d_year, k00, sw, ffeed, p_drctry, d_date
PRIVATE sel_rem, f_name, sb, n_p_det, n_pg, f_pg, s_pg, str_w, a_code
PRIVATE nds, nn, c_w, is_c
PRIVATE tCheck
PRIVATE wDoc, wFirm, wDate, is_serv

is_serv = .F.

*if TYPE("tAltName") = 'N'                        && NEW 7.02.2002
*    tAltName = .T.                               && NEW 7.02.2002
*endif                                            && NEW 7.02.2002
*tAltName = tAltName and (Sale_Mode = 'MEGA')     && NEW 7.02.2002

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

is_c = SUBSTR(inv_n, 2, 3) = "***"
sel_rem = SELECT()

IF TYPE("inv_d") = "D"
  d_year = LEFT(DTOS(inv_d),4)
ENDIF
IF TYPE("inv_d") = "N"
  d_year = STR(inv_d,4)
ENDIF
IF TYPE("inv_d") = "C"
  d_year = inv_d
ENDIF

d_ver = ""
sw = .F.
IF TYPE("flg") = "C"
  sw = "I" $ UPPER(flg)
ENDIF

PRIVATE sum_w, sum_w0, sum_p, sum_p0
PRIVATE d_n, d_d, d_n_f, r_n, r_d, ord_txt, l_face, l_dep
d_n = LEFT(inv_n,4)+"-"+ALLTRIM(RIGHT(inv_n,6))

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS INV_TTT AGAIN
k00 = f_code+d_year+inv_n
IF .NOT. SEEK(k00)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет накладной с номером "+d_n
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
wFirm = FIRM
wDoc  = DOC_NUM
wDate = DOC_DATE
a_code = WHO
IF sw
  d_n = "б/н"
ENDIF
r_n = REQ_DOC
d_d = TRANSFORM(DOC_DATE,"@D")
d_n_f = NEW_DOC
d_n_f = LEFT(d_n_f,1)+"-"+ALLTRIM(RIGHT(d_n_f,9))
d_date = DOC_DATE
r_d = TRANSFORM(REQ_DATE,"@D")
sum_w  = 0
sum_w0 = 0
sum_w_n = ROUND(MIN(ABS(KASSA),ABS(TOTAL_R))*NSP_/100,2)
*sum_p_n = Capital(Num2RStr(FLOOR(ABS(TOTAL_R)+ABS(sum_w_n))))+" руб. "+ ;
*STR((ABS(TOTAL_R)+ABS(sum_w_n)-FLOOR(ABS(TOTAL_R)+ABS(sum_w_n)))*100,2)+" коп."

l_num = LIST_NUM

IF EMPTY(PAY_TYPE) .AND. .NOT. EMPTY(PAY_DOC)
  ord_txt = "к п/о N "+LEFT(PAY_DOC,4)+"-"+ALLTRIM(RIGHT(PAY_DOC,6))+ ;
            " от "+TRANSFORM(PAY_DATE,"@D")
ELSE
  ord_txt = ""
ENDIF

l_dep  = CUS_CODE = 0  && Можно ли корректировать предприятие
IF EMPTY(CUS_CODE)
*  dep_nam = ALLTRIM(CUS_NAME)
  dep_nam = CUS_NAME
ELSE
  dep_nam = CUS_CODE
ENDIF
face_nam = CONTACT
l_face = EMPTY(face_nam)    && Можно ли корректировать фамилию

IF .NOT. EMPTY(CUS_CODE)
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC000 AGAIN
  SEEK dep_nam
  dep_nam = ALLTRIM(CUS_NAME)
  USE
ENDIF

SELECT INV_TTT
IF CUS_CODE >= 0 .AND. .NOT. is_c
  DO C_Names
ENDIF

c_w = CUS_CODE

f_name = SYS(3)
DO Wt_Mess WITH "Подготовка списка"
DO Prep_Inv WITH f_name
DO Wt_Mess

SELECT INV_TTT
nds   = STR(sum_w-sum_w0,10,2)
sum_p   = Capital(Num2RStr(FLOOR(ABS(sum_w0))))+" руб. "+ ;
STR((ABS(sum_w0)-FLOOR(ABS(sum_w0)))*100,2)+" коп."
sum_p_n = Capital(Num2RStr(FLOOR(ABS(sum_w))))+" руб. "+ ;
STR((ABS(sum_w)-FLOOR(ABS(sum_w)))*100,2)+" коп."
sum_w = STR(sum_w,10,2)
sum_w0 = STR(sum_w0,10,2)

DIMENSION sb(42,2)   && NEW 22.12.1999 - увеличил длину массива на 1
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Pg_Sum  }"
sb( 2,2) = ""

sb( 3,1) = "{Inv_No   }"
sb( 3,2) = d_n

sb( 4,1) = "{Inv_Dat }"
sb( 4,2) = d_d

sb( 42,1) = "{B_F_No    }"
sb( 42,2) = d_n_f

sb( 5,1) = "{Order                            }"
sb( 5,2) = ord_txt

sb( 6,1) = "{Department                                      }"
sb( 6,2) = dep_nam

sb( 7,1) = "{Face                                            }"
sb( 7,2) = face_nam

sb( 8,1) = "{Tot_Sum }"
sb( 8,2) = sum_w

sb( 9,1) = "{Sum_Str1                                                   }"
str_w = ""
i = ""
DO Break_Str WITH sum_p, i, str_w, LEN(sb(9,1))
sb( 9,2)   = i
sum_p = str_w

sb(10,1) = "{Sum_Str2                                                   }"
i = ""
DO Break_Str WITH sum_p, i, str_w, LEN(sb(10,1))
sb(10,2)   = i
sum_p = str_w

sb(11,1) = "{NDS     }"
sb(11,2)   = nds

sb(12,1) = "{List}"
sb(12,2)   = l_num

sb(13,1) = "{Req_No   }"
sb(13,2) = d_n

sb(14,1) = "{Req_Dat }"
sb(14,2) = r_d

sb(15,1) = "{Tot_Sum0}"
sb(15,2) = sum_w0

sb(16,1) = "{%NDS}"
sb(16,2) = ALLTRIM(STR(NDS_,5,1))

sb(17,1) = "{Firm                                            }"
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS FIRM_INV AGAIN
SEEK f_code

sb(36, 1) = "{Руководитель          }"
sb(36, 2) = BOSS
sb(37, 1) = "{Гл. бухгалтер         }"
sb(37, 2) = ABAK
sb(38, 1) = "{Кассир                }"
sb(38, 2) = KASSIR

sb(17,2) = ALLTRIM(L_NAME)

sb(18,1) = "{Pg_Sum0 }"
sb(18,2) = ""

sb(19,1) = "{Pg_NDS  }"
sb(19,2) = ""

sb(20,1) = "{SumN    }"
sb(20,2) = STR(sum_w_n,10,2)

sb(21,1) = "{Tot_SumN}"
sb(21,2) = STR(INV_TTT.TOTAL_R+SIGN(INV_TTT.TOTAL_R)*sum_w_n,10,2)

sb(22,1) = "{%NSP}"
sb(22,2) = ALLTRIM(STR(INV_TTT.NSP_,5,1))

sb(23,1) = "{Sum_Str1_N                                                 }"
str_w = ""
i = ""
DO Break_Str WITH sum_p_n, i, str_w, LEN(sb(23,1))
sb(23,2)   = i
sum_p_n = str_w

sb(24,1) = "{Sum_Str2_N                                                 }"
i = ""
DO Break_Str WITH sum_p_n, i, str_w, LEN(sb(24,1))
sb(24,2)   = i

sb(25,1) = "{Autor                       }"
USE (base_path+"PERSONS.DBF") ORDER TAG CODE ALIAS C9903 AGAIN
IF a_code = 0
  sb(25,2) = "САМ!"
ELSE
  IF SEEK (a_code)
    sb(25,2) = ALLTRIM(FAMILY)+" "+LEFT(NAME,1)+" "+LEFT(S_NAME,1)
  ELSE
    sb(25,2) = "?!!"
  ENDIF
ENDIF
sb(25,2) = PADR(sb(25,2),LEN(sb(25,1)))
n = ALEN(sb,1)
*FOR i = 1 TO n
*  sb(i,2) = PADR(sb(i,2), LEN(sb(i,1)))
*ENDFOR

sb(26,1) = "{Chk_No }"                                && NEW 22.12.1999
sb(26,2) = LEFT(inv_n,1)+"-"+ALLTRIM(RIGHT(inv_n,6))  && NEW 22.12.1999

sb(27,1) = "{Flag 1            }"
sb(27,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 1, 1)), SPACE(LEN(sb(27,1))), fs_names(1))

sb(28,1) = "{Flag 2            }"
sb(28,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 2, 1)), SPACE(LEN(sb(28,1))), fs_names(2))

sb(29,1) = "{Flag 3            }"
sb(29,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 3, 1)), SPACE(LEN(sb(29,1))), fs_names(3))

sb(30,1) = "{Flag 4            }"
sb(30,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 4, 1)), SPACE(LEN(sb(30,1))), fs_names(4))

sb(31,1) = "{Flag 5            }"
sb(31,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 5, 1)), SPACE(LEN(sb(31,1))), fs_names(5))

sb(32,1) = "{Flag 6            }"
sb(32,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 6, 1)), SPACE(LEN(sb(32,1))), fs_names(6))

sb(33,1) = "{Flag 7            }"
sb(33,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 7, 1)), SPACE(LEN(sb(33,1))), fs_names(7))

sb(34,1) = "{Flag 8            }"
sb(34,2) = IIF(EMPTY(SUBSTR(INV_TTT.SAL_FLAGS, 8, 1)), SPACE(LEN(sb(34,1))), fs_names(8))

sb(35,1) = "{Note              }"
sb(35,2) = INV_TTT.NOTE
USE
sb(39,1) = "{Pay_Dt  }"
sb(39,2) = SPACE(10)
sb(40,1) = "{Pay_Num       }"
sb(40,2) = SPACE(16)
sb(41,1) = "{Pay Type      }"
sb(41,2) = SPACE(20)
IF .NOT. EMPTY(INV_TTT.PAY_DOC)
  USE base_path+"INCMONEY" AGAIN ORDER TAG DOC_NUM ALIAS P_1831
  IF SEEK(INV_TTT.FIRM+INV_TTT.PAY_TYPE+LEFT(DTOS(INV_TTT.PAY_DATE),4)+INV_TTT.PAY_DOC)
    sb(41,2) = PADR(Pay_Type(DOC_TYPE, MONEY_R),20)
    IF DOC_TYPE $ "Б1"
      sb(39,2) = DTOC(ENTER_DAT)
      sb(40,2) = ENTER_DOC
    ELSE
      sb(39,2) = DTOC(DOC_DATE)
      sb(40,2) = PADR(LEFT(DOC_NUM,1)+ALLTRIM(SUBSTR(DOC_NUM,2)), 16)
    ENDIF
  ENDIF
  USE
ENDIF
SELECT INV_TTT
USE
IF c_w < 0
  USE (base_path+"DOC_FORM")
  LOCATE FOR "CORR_EXP" == ALLTRIM(UPPER(DOC_NAME))
ELSE
  IF is_c
    USE (base_path+"DOC_FORM")
    LOCATE FOR "G_CHECK" == ALLTRIM(UPPER(DOC_NAME))
  ELSE
    IF TYPE("flg") = "C"
      USE (base_path+"DOC_FORM")
      LOCATE FOR "INVOICE"+(flg) == ALLTRIM(UPPER(DOC_NAME))
      IF .NOT. FOUND()
        USE
      ENDIF
    ENDIF
    IF .NOT. USED("DOC_FORM")
      IF choose_inv == "-"
        DO Ch_Inv
      ELSE
        USE (base_path+"DOC_FORM")
        LOCATE FOR "INVOICE"+ALLTRIM(choose_inv) == ALLTRIM(UPPER(DOC_NAME))
        IF .NOT. FOUND()
          LOCATE FOR "INVOICE" == ALLTRIM(UPPER(DOC_NAME))
        ENDIF
      ENDIF
    ENDIF
  ENDIF
ENDIF
tCheck = "INVOICEC" == UPPER(ALLTRIM(DOC_FORM.DOC_NAME))
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

SELECT TMP_LST

DO Ini_Prn WITH "Накладная N "+d_n, p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
nn = 0

n_p_det = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))-1
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = MAX(CEILING(RECCOUNT()/n_p_det),1)
ENDIF

FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1
    EJECT
  ENDIF
  FOR i = 1 TO ALEN(t_p)
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
  sum_p = 0
  sum_p0 = 0
  IF RECCOUNT() # 0
    FOR i = 1 TO n_p_det
      nn = nn+1
      IF tCheck
        @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+" "+STR(QNT,7)+STR(ROUND(SUM_T/QNT,4),10,4)+STR(SUM_T,11,2)
      ELSE
        IF is_c
          @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+" "+STR(QNT,7)+STR(PRC_0,8,2)+STR(SUM_0,11,2)
        ELSE
          @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+" "+STR(QNT,7)+STR(PRC_0,8,2)+STR(SUM_0,11,2)+STR(SUM_T,11,2)
        ENDIF
      ENDIF
      sum_p = sum_p+TMP_LST.SUM_T
      sum_p0 = sum_p0+TMP_LST.SUM_0
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
*  USE
  str_w = STR(sum_p,10,2)
  sb( 2,2) = str_w
  sb( 2,2) =PADR(sb(2,2), LEN(sb(2,1)))
  str_w = STR(sum_p0,10,2)
  sb(18,2) = str_w
  sb(18,2) =PADR(sb(18,2), LEN(sb(18,1)))
  str_w = STR(sum_p-sum_p0,10,2)
  sb(19,2) = str_w
  sb(19,2) =PADR(sb(19,2), LEN(sb(19,1)))
  IF f_pg = n_pg
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
DELETE FILE (tmpo_path+"TMP_INV0.DBF")
COPY TO (tmpo_path+"TMP_INV0")
USE

DO Term_Prn WITH "", tmpo_path+"TMP_INV0.DBF", c_w, "INV", wFirm, wDoc, wDate

DELETE FILE (tmpo_path+f_name+".DBF")
DELETE FILE (tmpo_path+f_name+".CDX")
IF USED("FIRM_INV")
  SELECT FIRM_INV
  USE
ENDIF
SELECT (sel_rem)
*IF is_serv
*  DO P_Act_D WITH f_code, inv_n, inv_d
*ENDIF

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║              Программа подготовки содержимого списка.                 ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Prep_Inv
PARAMETERS f_nam
PRIVATE c_sav, p_sav
PRIVATE name_out, pref_out                     && NEW 7.02.2002

*IF File_O(base_path + "Altname.DBF")           && NEW 7.02.2002
*    SELECT 0                                   && NEW 7.02.2002
*    USE base_path + "AltName.DBF" ORDER Name   && NEW 7.02.2002
*ENDIF                                          && NEW 7.02.2002
*IF File_O(base_path + "AltPref.DBF")           && NEW 7.02.2002
*    SELECT 0                                   && NEW 7.02.2002
*    USE base_path + "AltPref.DBF" ORDER Prefix && NEW 7.02.2002
*ENDIF
SELECT 0
CREATE DBF (tmpo_path+f_nam) (PREFIX   C( 4), ;
                              NAME     C(35), ;
                              PRODUCER C( 8), ;
                              QNT      N( 7), ;
                              PRC_0    N( 8,2), ;
                              PRC      N(12,5), ;
                              SUM_0    N(10,2), ;
                              SUM_T    N(10,2))
USE (tmpo_path+f_nam) ALIAS TMP_LST
INDEX ON NAME+PREFIX+PRODUCER TAG NAME

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST000 AGAIN
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS S000 AGAIN
SET RELATION TO CODE INTO ST000
= SEEK(k00)
c_sav = 0
p_sav = 0
SCAN REST WHILE DOC_NUM = inv_n .AND. FIRM = f_code .AND. DOC_DATE = d_date
  IF .NOT. old_mode
    IF .NOT. EMPTY(ST000.WK_SW2)
      is_serv = .T.
      LOOP
    ENDIF
  ENDIF
  SELECT TMP_LST
*  IF c_sav = S000.CODE .AND. p_sav = S000.PRICE_R
*    REPLACE QNT   WITH QNT+S000.QNT
*    REPLACE SUM_0 WITH SUM_0+S000.PRICE_0*S000.QNT,   ;
*            SUM_T WITH SUM_T+ROUND(S000.PRICE_R*S000.QNT,2)
*    sum_w  = sum_w+ROUND(S000.PRICE_R*S000.QNT,2)
*    sum_w0 = sum_w0+S000.PRICE_0*S000.QNT
*  ELSE
    APPEND BLANK
    * Если
    * - Стоит крестик "Альт.названия"(tAltName = .T.)
    * - Prefix найден в AltPref.DBF,
    * - Name найдено в AltName.DBF
    * то подменяем Name на AltName.AltName, а Prefix на AltPref.AltPref
    name_out = ST000.NAME                        && NEW 7.02.2002
    pref_out = ST000.PREFIX                      && NEW 7.02.2002
*    IF USED("AltName") and USED("AltPref") and tAltName && NEW 7.02.2002
*      IF SEEK(pref_out,"AltPref") and SEEK(name_out,"AltName") && NEW 7.02.2002
*        name_out = AltName.AltName               && NEW 7.02.2002
*        pref_out = AltPref.AltPref               && NEW 7.02.2002
*      ENDIF                                      && NEW 7.02.2002
*    ENDIF                                        && NEW 7.02.2002
    REPLACE PREFIX   WITH pref_out,  ;
            NAME     WITH name_out,  ;
            PRODUCER WITH ST000.PRODUCER,;
            QNT      WITH S000.QNT,      ;
            PRC_0    WITH S000.PRICE_0,  ;
            PRC      WITH S000.PRICE_R,  ;
            SUM_0    WITH S000.PRICE_0*S000.QNT, ;
            SUM_T    WITH ROUND(S000.PRICE_R*S000.QNT,2)
    sum_w  = sum_w+ROUND(S000.PRICE_R*S000.QNT,2)
    sum_w0 = sum_w0+S000.PRICE_0*S000.QNT
*  ENDIF
  c_sav = S000.CODE
  p_sav = S000.PRICE_R
  SELECT S000
ENDSCAN
USE
SELECT ST000
USE
*IF USED("AltName")    && NEW 7.02.2002
*  SELECT AltName      && NEW 7.02.2002
*  USE                 && NEW 7.02.2002
*ENDIF                 && NEW 7.02.2002
*IF USED("AltPref")    && NEW 7.02.2002
*  SELECT AltPref      && NEW 7.02.2002
*  USE                 && NEW 7.02.2002
*ENDIF                 && NEW 7.02.2002
SELECT TMP_LST

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║           Программа ввода имени клиента и предприятия.                ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE C_Names

PRIVATE ln, wd, ex, mss, c1

IF .NOT. l_face
  RETURN
ENDIF

ln = 8
wd = 68
ex = 1

*
*┌────────────────────────────────────────────────────────────────┐
*│ Организация ....:....!....:....!....:....!....:....!....:....! │
*│                                                                │
*│               Через кого ....:....!....:....!..                │
*│                                                                │
*│               < OK Ctrl-W > < Отказаться Esc >                 │
*└────────────────────────────────────────────────────────────────┘

DO D_W WITH "W_000", ln, wd, "Заполните!", 1
@ 2,  3 SAY "Организация" GET dep_nam WHEN l_dep
@ 4, 17 SAY "Через кого" GET face_nam
@ 6, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE VALID Empt_N()

RELEASE WINDOW W_000
RETURN

**********************************************************************
PROCEDURE Empt_N
PRIVATE s_sav

IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF EMPTY(face_nam)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  Пустое имя, это правильно? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(7,"mss") = 2
    RETURN .F.
  ENDIF
ENDIF

REPLACE CONTACT WITH face_nam, CUS_NAME WITH dep_nam

IF .NOT. EMPTY(PAY_DOC) .AND. INLIST(PAY_TYPE, " ", "2")
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"INCMONEY") ORDER TAG DOC_NUM ALIAS S12345 AGAIN
  IF SEEK(INV_TTT.PAY_TYPE+LEFT(DTOS(INV_TTT.PAY_DATE),4)+INV_TTT.PAY_DOC)
    REPLACE CONTACT WITH face_nam, CUS_NAME WITH dep_nam
  ENDIF
  USE
  SELECT (s_sav)
ENDIF

RETURN .T.
