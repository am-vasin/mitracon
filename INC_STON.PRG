*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ ” ฉซ Inc_StoN      งเ กฎโ็จช €ญคเฅฉ  แจญ              18.10.2011     บ
*วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ
*บ                                                                        บ
*บ                     ‘ขฅเช  ญฎฌฅญชซ โใเ๋ จ ญ ซจ็จ๏.                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Inc_StoN

PRIVATE pth_inc, sups_n, sups_c, n_sups, sup_point, p_refr, po_refr, o_refr
PRIVATE mss, own_cd, own_vl, sup_cd, sup_nm, arr_w, pr_dat, st_dat
PRIVATE to_cont, rec_cont, rec_cont1, count_cont, cont_mode, interr, tmpCode, code_ln
PRIVATE sw1, sw2, sw3, sw4, d_t, w_t, name_off
PRIVATE dsc_main, dsc_brend, lst_brend, ln_brend, tmp_Name, trr_File

USE (base_path+"OUR_DISC") SHARED AGAIN
ln_brend = 0
LOCATE FOR IS_MAIN
dsc_main = 1-DISC_PRC/100
lst_brend = ""
SCAN FOR .NOT. IS_MAIN
  ln_brend = ln_brend+1
  DIMENSION dsc_brend(ln_brend)
  dsc_brend(ln_brend) = 1-DISC_PRC/100
  lst_brend = lst_brend+STR(BREND)
ENDSCAN
USE

m.d_t = LEFT(Table_D(),66)
m.w_t = LEFT(Table_W(),66)
name_off = 35

USE (base_path+"CLASSES") ORDER TAG CODE IN 0
m.code_ln = FSIZE("CODE", "CLASSES")
USE IN CLASSES

interr = .F.
m.to_cont = .F.
rec_cont = 1
rec_cont1 = 1
count_cont = 0
p_refr = .T.
po_refr = .F.
o_refr = .F.
sup_point = 1
sup_cd = 0
pth_inc = ""

USE (base_path+"CUSTOMER")
n_sups = 0
SCAN FOR SUP # " "
  IF CODE # 10
    LOOP
  ENDIF
  n_sups = n_sups+1
  DIMENSION sups_n(n_sups), sups_c(n_sups)
  sups_n(n_sups) = B_NAME
  sups_c(n_sups) = CODE
ENDSCAN

LOCATE FOR SELF # " "
own_cd = CODE
USE (base_path+"PARMS")
own_vl = VALUTA
cont_mode = Is_Break()
IF cont_mode = 3
  USE
  RETURN
ENDIF
IF cont_mode = 1
  DO Sto_Refr
*  IF .NOT. interr
*    DO Diffs
*  ENDIF
  RETURN
ENDIF

*
*    ้ฅฌ ฏฎแ๋ซชใ...
*
DO WHILE .T.
  pth_inc = Int_Dir(pth_inc, "คฅ ไ ฉซ๋ ฏฎแโ ข้จช ?")
  IF EMPTY(pth_inc)
    RETURN
  ENDIF
  m.sw1 = File_O(pth_inc+"MTR_NOM.DBF")
  m.sw2 = File_O(pth_inc+"MTR_NOM.FPT")
  m.sw3 = File_O(pth_inc+"MTR-IPG.CSV")
  m.sw4 = File_O(pth_inc+"MTR-BT.CSV")
  
  IF .NOT. m.sw1 .OR. .NOT. m.sw2 .OR. .NOT. m.sw3 .OR. .NOT. m.sw4
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+" ใช ง ญญฎฉ  ฌจ คจเฅชโฎเจจ ญฅโ ไ ฉซฎข ฏฎแโ ข้จช !"
    mss(3) = CHR(0)+"  —โฎ คฅซ ฅฌ? [ ้ฅฌ ข คเใฃฎฌ ฌฅแโฅ ] [ โช ง๋ข ฅฌแ๏ ]"
    mss(4) = ""
    IF Out_Mess(7, "mss") = 1
      LOOP
    ENDIF
    RETURN
  ENDIF
  EXIT
ENDDO


pr_dat = {}
= ADIR(arr_w, pth_inc+"MTR_NOM.DBF")
st_dat = arr_w(1,3)

IF .NOT. Inp_Mode()	&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  RETURN
ENDIF

IF File_O(pth_inc+"TERRA.CSV")
  m.trr_file = pth_inc+"TERRA.CSV"
ELSE
  m.trr_file = pth_inc+"TERRA.DBF"
ENDIF
DO Cr_DMS WITH pth_inc+"DKO", m.trr_file, pth_inc+"MIT.DBF", pth_inc+"MITR_MEG.DBF"

*
*  กญฎขซ๏ฅฌ DEVELOP
*
DO Wt_Mess WITH "เฅญค๋"
USE (base_path+"DEVELOP") ORDER TAG CODE
SELECT 0
DELETE FILE (pth_inc+"DEV_S.DBF")
CREATE DBF (pth_inc+"DEV_S") ;
	(	CODE	N(FSIZE("CODE",   "DEVELOP")),	;
	    B_NAME	C(FSIZE("B_NAME", "DEVELOP")),	;
	    L_NAME	C(FSIZE("L_NAME", "DEVELOP"))	)
APPEND FROM (pth_inc+"MTR-BT.CSV") DELIMITED WITH "
REPLACE ALL B_NAME WITH CHRTRAN(B_NAME, m.w_t, m.d_t), ;
            L_NAME WITH CHRTRAN(L_NAME, m.w_t, m.d_t)
GO TOP
DELETE

SCAN
  SELECT DEVELOP
  IF .NOT. SEEK(DEV_S.CODE)
    APPEND BLANK
    REPLACE CODE WITH DEV_S.CODE, DATE_ON WITH DATE()
  ENDIF
  REPLACE B_NAME WITH DEV_S.B_NAME, ;
          L_NAME WITH DEV_S.L_NAME
  SELECT DEV_S
ENDSCAN
USE
SELECT DEVELOP
DELETE FILE (pth_inc+"DEV_S.DBF")

*  กญฎขซ๏ฅฌ CLASSES
DO Wt_Mess
DO Wt_Mess WITH "ซ แแ๋"

USE (base_path+"CLASSES") ORDER TAG CODE
SELECT 0

DELETE FILE (pth_inc+"CLASS_S.DBF")
CREATE DBF (pth_inc+"CLASS_S") ;
	(	CODE	C(FSIZE("CODE", "CLASSES")+2),	;
	    NAME	C(FSIZE("NAME", "CLASSES"))		)
APPEND FROM (pth_inc+"MTR-IPG.CSV") DELIMITED WITH "
REPLACE ALL CODE WITH CHRTRAN(CODE, m.w_t, m.d_t), ;
            NAME WITH CHRTRAN(NAME, m.w_t, m.d_t)
GO TOP
DELETE
SCAN
  m.tmpCode = AT("4", CLASS_S.CODE)
  m.tmpCode = ALLTRIM(SUBSTR(CLASS_S.CODE,m.tmpCode+1))
  m.tmpCode = PADR(m.tmpCode, m.code_ln)
  SELECT CLASSES
  IF .NOT. SEEK(m.tmpCode)
    APPEND BLANK
    REPLACE CODE WITH m.tmpCode
  ENDIF
  REPLACE NAME WITH CLASS_S.NAME
  SELECT CLASS_S
ENDSCAN
USE
SELECT CLASSES
USE
DELETE FILE (pth_inc+"CLASS_S.DBF")
DO Wt_Mess

*IF Cls_Refr(sup_cd, pth_inc+"CLASSES", 0)

DO Sto_Refr
*  IF .NOT. interr
*    DO Diffs
*  ENDIF

*ENDIF
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                     ญโฅเ ชโจขญ๋ฉ ขขฎค ฏ เ ฌฅโเฎข.                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 10.06.98 14:22:01 อผ
PROCEDURE Inp_Mode

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

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ                    ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ          ณ
*ณ          ฎแโ ข้จช ณ....:....!....:.ณ          ณ
*ณ                    ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู          ณ
*ณ ฎฌฅญชซ โใเ  ฎโ ..  –ฅญ๋ ฎโ .. ณ
*ณ                                                ณ
*ณ  [ ] กญฎขซฅญจฅ ๆฅญ   [ ] ข โ. ็. แฎกแโขฅญญ๋ๅ  ณ
*ณ                                                ณ
*ณ            [ ] กญฎขจโ์ ฏเฅคซฎฆฅญจฅ            ณ
*ณ                                                ณ
*ณ        < OK Ctrl-W > < โช ง โ์แ๏ Esc >        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE ex
                     &&   ก๊๏ขซ๏ฅฌ จ ง ฏฎซญ๏ฅฌ ฏฎซ๏ กซ ญช 
ex   = 1             &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 11, 52, "“แโ ญฎขจโฅ ฏ เ ฌฅโเ๋ แขฅเชจ"
@ 3, 12 SAY "ฎแโ ข้จช"
@ 5,  3 SAY "ฎฌฅญชซ โใเ  ฎโ "+DTOC(st_dat)
@ 5, 31 SAY "–ฅญ๋ ฎโ "+DTOC(pr_dat)
*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*

@  2, 22 GET sup_point PICTURE "@^" FROM sups_n WHEN n_sups > 1
*@  7,  4 GET p_refr PICTURE "@*C กญฎขซฅญจฅ ๆฅญ" VALID Set_PO()
*@  7, 25 GET po_refr PICTURE "@*C ข โ. ็. แฎกแโขฅญญ๋ๅ" WHEN p_refr
@  7, 14 GET o_refr PICTURE "@*C กญฎขจโ์ ฏเฅคซฎฆฅญจฅ"
@  9, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE

sup_cd = sups_c(sup_point)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

********************************************************************
PROCEDURE Set_PO
IF .NOT. p_refr
  po_refr = .F.
  SHOW GET po_refr
ENDIF

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                        กญฎขซฅญจฅ ญฎฌฅญชซ โใเ๋.                        บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 11.06.98 15:42:47 อผ
PROCEDURE Sto_Refr

PRIVATE pr_r, pr_d, pr_o_r, pr_o_d, c_sup, is_off, cnt, k_pr, old_pr
PRIVATE tmpQnt, tmpFunc, tmpImage, tmpDSheet, tmpDateOn, tmpStopPur, tmpDateOff
PRIVATE tmpSeg, tmpAut, tmpCor, tmpWg, tmpClass, dt_reg, tm_reg
PRIVATE DMS_Code, DMS_Sz, new_name, name_ln, h_count, cnt_a, cnt_m, len_nm
PRIVATE e_of_s, is_mod, emp_dt

m.e_of_s = .F.
new_name = ""
m.tmpQnt = 0
m.tmpClass = ""
m.pr_d = 0
m.pr_r = 0
c_sup = sup_cd
STORE "" TO dt_reg, tm_reg
IF p_refr
*  SELECT 0
*  DELETE FILE (base_path+"NEW_PRC.DBF")
*  DELETE FILE (base_path+"NEW_PRC.CDX")
*  CREATE DBF (base_path+"NEW_PRC")  ;
*     ( CODE N(7), OLD_PRC N(12,4))
*  INDEX ON CODE TAG CODE
  USE (base_path+"NEW_PRC")
ENDIF

SELECT 0
DO Use_DMS
IF USED("DMS")
  SELECT DMS
  SET ORDER TO TAG CODE
  DMS_Sz = FSIZE("CODE")
ENDIF

SELECT 0
USE (base_path+"STO_LOG")
SELECT 0
USE (base_path+"DEVELOP") ORDER TAG B_NAME
m.len_nm = FSIZE("B_NAME")
SELECT 0
USE (base_path+"ORD_SEG") ORDER TAG SEGMENT
SELECT 0
USE (base_path+"BREND_SP") ORDER TAG CODE
SELECT 0
USE (path_comm+"CORPUS") ORDER TAG CODE
SELECT 0
USE (path_comm+"SUBJECT") ORDER TAG STO_CODE
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE
name_ln = FSIZE("NAME")

is_off = .F.
IF File_O(base_path+"OFFER.DBF") .AND. o_refr
  is_off = .T.
  SELECT 0
  USE (base_path+"OFFER") ORDER TAG CODE ALIAS OFFER_C
  SET FILTER TO SUP_CODE = c_sup
  name_off = FSIZE("NAME")

  SELECT 0
  USE (base_path+"OFFER") ORDER TAG NAME ALIAS OFFER_N AGAIN
  SET FILTER TO SUP_CODE = c_sup
ENDIF

SELECT 0
USE (pth_inc+"MTR_NOM") ALIAS STO_S
INDEX ON CODE TO (m.tmpo_path+"STO_S.IDX") FOR CODE # 0 UNIQUE
SET RELATION TO LEFT(PRODUCER, m.len_nm) INTO DEVELOP
IF to_cont
  IF rec_cont < 0
    GO BOTT
    SKIP
  ELSE
    GO rec_cont
  ENDIF
  IF rec_cont1 < 0
    GO BOTT IN STOCK
    SKIP IN STOCK
  ELSE
    GO rec_cont1 IN STOCK
  ENDIF
ELSE
  GO TOP
ENDIF
DO New_Rec

DO Wt_Mess WITH "กญฎขซฅญจฅ ญฎฌฅญชซ โใเ๋, Esc - ฏเฅเข โ์..."
cnt = count_cont
cnt_a = 0
cnt_m = 0
m.h_count = ALLTRIM(STR(RECCOUNT()))
m.emp_dt = CTOD("")
DO WHILE .T.
  IF m.e_of_s .AND. (EOF("STOCK") .OR. STOCK.CODE > 9500000)
    EXIT
  ENDIF
  IF cnt % 100 = 0
    IF cnt % 1000 = 0
      IF INKEY() = 27
        IF To_Interr()
          EXIT
        ENDIF
      ENDIF
    ENDIF
    @ WROWS()-1 , 0 SAY PADC(ALLTRIM(STR(cnt))+"/"+;
                             ALLTRIM(STR(cnt_m))+"/"+;
                             ALLTRIM(STR(cnt_a))+"/"+h_count,WCOLS())
*    @ WROWS()-1 , 0 SAY PADC(ALLTRIM(STR(cnt))+"/"+ ;
*                             ALLTRIM(STR(cnt_a)+"/"+ ;
*                             ALLTRIM(STR(cnt_m))+"/"+ ;
*                             h_count,WCOLS())
    cnt_a = 0
    cnt_m = 0
  ENDIF

  cnt = cnt+1
  *
  *   ” ฉซ ฏเฅคซฎฆฅญจฉ, ฅแซจ ฎญ ฅแโ์...
  *
  DO CASE
  CASE STOCK.CODE < STO_S.CODE .OR. m.e_of_s	&& “ค ซ๏ฅฌ!!!
    tmpDateOff = IIF(STOCK.QNT = 0      .AND. ;
                     STOCK.QNT_SAVE = 0 .AND. ;
                     STOCK.QNT_REZ  = 0 .AND. ;
                     STOCK.QNT_REAL = 0 .AND. ;
                     STOCK.QNT_SPEC = 0 .AND. ;
                     STOCK.CODE < 9500000, DATE(), {})
    SELECT STOCK
    IF EMPTY(DATE_OFF)
      REPLACE DATE_OFF WITH tmpDateOff
    ENDIF
    SKIP
    SELECT STO_S
    LOOP
  CASE STOCK.CODE > STO_S.CODE
    SELECT STOCK
    APPEND BLANK
    REPLACE CODE WITH STO_S.CODE, DATE_ON WITH DATE()
    m.cnt_a = cnt_a+1
  ENDCASE
  DO New_Rec
  IF is_off
    IF SEEK(CODE,"OFFER_C")
      SELECT OFFER_C
    ELSE
      SELECT OFFER_N
      IF .NOT. SEEK(new_name+STO_S.ROHS+STO_S.PRODUCER,"OFFER_N")
        APPEND BLANK
      ENDIF
    ENDIF
    IF SUP_CODE # c_sup .OR. ;
       CODE     # STO_S.CODE .OR.     ;
       .NOT. ALLTRIM(PREFIX) == ALLTRIM(STO_S.ROHS) .OR. ;
       .NOT. ALLTRIM(NAME)   == ALLTRIM(LEFT(new_name, name_off)) .OR.  ;
       PRO_CODE # DEVELOP.CODE
      REPLACE SUP_CODE WITH c_sup,          ;
              CODE     WITH STO_S.CODE,     ;
              PREFIX   WITH STO_S.ROHS,     ;
              NAME     WITH new_name,       ;
              PRODUCER WITH STO_S.PRODUCER, ;
              PRO_CODE WITH DEVELOP.CODE
    ENDIF
    REPLACE PRICE_R  WITH m.pr_r,         ;
            PRICE_D  WITH m.pr_d,         ;
            SUP_QNT  WITH tmpQnt,         ;
            DATE_INF WITH DATE()
  ENDIF
  *
  *   ฎคญฎขซ๏ฅฌ STOCK
  *
  SELECT STOCK
  IF .NOT. EMPTY(DATE_OFF) .AND. EMPTY(WHO_DEL)
    REPLACE DATE_OFF WITH m.emp_dt
  ENDIF

  IF STO_S.ROHS   # STOCK.PREFIX .OR. ;
     new_name     # STOCK.NAME .OR.   ;
     DEVELOP.CODE # STOCK.PRO_CODE
     SELECT STO_LOG
     APPEND BLANK
     REPLACE CODE     WITH STOCK.CODE,     ;
             OLD_PRF  WITH STOCK.PREFIX,   ;
             OLD_NAM  WITH STOCK.NAME,     ;
             OLD_PRO  WITH STOCK.PRO_CODE, ;
             NEW_PRF  WITH STO_S.ROHS,     ;
             NEW_NAM  WITH new_name,       ;
             NEW_PRO  WITH DEVELOP.CODE,   ;
             DAT_CORR WITH DATE(),         ;
             WHERE_C  WITH CHR(255) 
     cnt_m = cnt_m+1
  ENDIF
  SELECT STOCK
  IF .NOT. ALLTRIM(PREFIX) == ALLTRIM(STO_S.ROHS) .OR. ;
     .NOT. NAME == new_name .OR.     ;
     .NOT. ALLTRIM(PRODUCER) == ALLTRIM(STO_S.PRODUCER) .OR. ;
     PRO_CODE  # DEVELOP.CODE .OR. ;
     .NOT. ALLTRIM(CLASS) == ALLTRIM(tmpClass)
     cnt_m = cnt_m+1

    REPLACE PREFIX    WITH STO_S.ROHS,     ;
            NAME      WITH new_name,       ;
            PRODUCER  WITH STO_S.PRODUCER, ;
            NUM_PART  WITH CHRTRAN(new_name, n_digit, ""), ;
            PRO_CODE  WITH DEVELOP.CODE,   ;
            CLASS     WITH tmpClass
  ENDIF
  IF pr_d = 0
    pr_d = PRICE_B
    pr_r = PRICE_B_R
  ENDIF
  old_pr = PRICE_B
  IF PRICE_B   # pr_d .OR. ;
     PRICE_B_R # pr_r .OR. ;
     QNT_STORE # tmpQnt

    REPLACE PRICE_B   WITH pr_d,  ;
            PRICE_B_R WITH pr_r,  ;
            QNT_STORE WITH tmpQnt
  ENDIF
  IF old_pr # pr_d
    IF EMPTY(dt_reg)
      dt_reg = DATE()
      tm_reg = TIME()
      SELECT 0
      USE (base_path+"NEW_PRCT")
      APPEND BLANK
      REPLACE DATE_REG WITH dt_reg,		;
              TIME_REG WITH tm_reg,		;
              WHO      WITH user
      USE
    ENDIF
    SELECT NEW_PRC
    APPEND BLANK
    REPLACE DATE_REG WITH dt_reg,		;
            TIME_REG WITH tm_reg,		;
            CODE     WITH STO_S.CODE,	;
            OLD_PRC  WITH old_pr,		;
            NEW_PRC  WITH pr_d
  ENDIF
  SELECT SUBJECT
  IF .NOT. SEEK(STO_S.CODE)
    APPEND BLANK
    REPLACE STO_CODE  WITH STO_S.CODE
  ENDIF
  m.tmpDSheet = DATASHEET
  IF TYPE("STO_S.DATASHEET")="C"
    m.tmpDSheet = ALLTRIM(STO_S.DATASHEET)
  ENDIF
  m.tmpImage = IMAGE
  IF TYPE("STO_S.SF_IMAGE")="C"
    m.tmpImage = ALLTRIM(STO_S.SF_IMAGE)
  ENDIF
  
  m.tmpFunc = ""
  IF TYPE("STO_S.SF_FUNC")="M"
    m.tmpFunc = ALLTRIM(STO_S.SF_FUNC)
  ENDIF
  IF EMPTY(m.tmpFunc)
    m.tmpFunc = ALLTRIM(FUNCTION)
  ENDIF
  tmpSeg  = IIF(TYPE("STO_S.SEGMENT")="C", STO_S.SEGMENT, SEGMENT)
  tmpSeg  = PADR(tmpSeg, FSIZE("SEGMENT", "ORD_SEG"))
  tmpSeg  = SYS(15, lwr, tmpSeg)
  tmpAut  = IIF(TYPE("STO_S.AUTHOR")="C", STO_S.AUTHOR, AUTHOR)
  tmpCor  = IIF(TYPE("STO_S.CORPUS")="C", STO_S.CORPUS, CORPUS)
  tmpCCor = IIF(TYPE("STO_S.COR_CODE")="N", STO_S.COR_CODE, COR_CODE)

  IF COR_CODE  # m.tmpCCor  .OR.       ;
     .NOT. ALLTRIM(DATASHEET) == ALLTRIM(m.tmpDSheet) .OR.;
     .NOT. ALLTRIM(FUNCTION) == ALLTRIM(m.tmpFunc) .OR.   ;
     .NOT. ALLTRIM(IMAGE) == ALLTRIM(m.tmpImage) .OR.     ;
     .NOT. ALLTRIM(CORPUS) == ALLTRIM(m.tmpCor) .OR.      ;
     .NOT. ALLTRIM(AUTHOR) == ALLTRIM(m.tmpAut) .OR.      ;
     .NOT. ALLTRIM(SEGMENT) == ALLTRIM(m.tmpSeg)
    REPLACE COR_CODE  WITH m.tmpCCor,      ;
            DATASHEET WITH m.tmpDSheet,    ;
            FUNCTION  WITH m.tmpFunc,      ;
            IMAGE     WITH m.tmpImage,     ;
            CORPUS    WITH m.tmpCor,       ;
            AUTHOR    WITH m.tmpAut,       ;
            SEGMENT   WITH m.tmpSeg
     cnt_m = cnt_m+1
  ENDIF
  IF TYPE("STO_S.COR_CODE") = "N"		&& ฎเฏใแ
    SELECT CORPUS
    IF .NOT. SEEK(STO_S.COR_CODE, "CORPUS")
      APPEND BLANK
      REPLACE CODE   WITH IIF(TYPE("STO_S.COR_CODE")="N", STO_S.COR_CODE, 0)
    ENDIF
    tmpWg  = IIF(TYPE("STO_S.WEIGHT")="N", STO_S.WEIGHT, WEIGHT)
    tmpCor = IIF(TYPE("STO_S.CORPUS")="C", STO_S.CORPUS, NAME)
    IF .NOT. ALLTRIM(tmpCor) == ALLTRIM(NAME) .OR. ;
       WEIGHT # tmpWg
      REPLACE NAME   WITH tmpCor, ;
              WEIGHT WITH tmpWg
    ENDIF
  ENDIF
  IF .NOT. SEEK(UPPER(tmpSeg), "ORD_SEG")
    SELECT ORD_SEG
    APPEND BLANK
    REPLACE SEGMENT WITH tmpSeg
  ENDIF

  IF .NOT. EOF("STOCK") .AND. STOCK.CODE < 9500000
    SKIP IN STOCK
  ENDIF
  SELECT STO_S
  IF .NOT. m.e_of_s
    SKIP IN STO_S
    m.e_of_s = EOF("STO_S")
  ENDIF

ENDDO

IF is_off
  SELECT OFFER_C
  USE
  SELECT OFFER_N
  USE
ENDIF
IF p_refr
  SELECT NEW_PRC
  USE
ENDIF

*DO Wt_Mess
*IF .NOT. interr
*  DO Wt_Mess WITH "็จแโช  ฎแโ โชฎข..."
*
*  DELETE FILE (m.tmpo_path+"STO_S.IDX")
*  SELECT STO_S
*  INDEX ON CODE TO (m.tmpo_path+"STO_S.IDX")
*  SELECT STOCK
*
*  SCAN FOR QNT_STORE # 0
*    IF .NOT. SEEK(CODE, "STO_S")
*      REPLACE QNT_STORE WITH 0
*    ENDIF
*  ENDSCAN
*ENDIF
USE IN STO_S
DELETE FILE (m.tmpo_path+"STO_S.IDX")
*DO Wt_Mess

SELECT STOCK
USE
SELECT CORPUS
USE
SELECT DEVELOP
USE
SELECT SUBJECT
USE
SELECT STO_LOG
USE
*SELECT DIFFS
*IF .NOT. interr
*  INDEX ON NAME+PREFIX+PRODUCER TAG NAME ADDITIVE
*ENDIF
*USE
*SELECT TU_6114
*USE

DO Wt_Mess

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                 เฎขฅเช  ญ  ฏเฅเ๋ข ญจฅ เฅฃซ ฌฅญโ .                     บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 17.06.2011 ออออออออผ
PROCEDURE Is_Break
PRIVATE mss_cont, stat_type

IF File_O(base_path+"INC_STO.DBF")
  DIMENSION mss_cont[4]
  mss_cont[1] = ""
  mss_cont[2] = CHR(0)+"เฅค๋คใ้จฉ เฅฃซ ฌฅญโ ก๋ซ ฏเฅเข ญ. —โฎ คฅซ โ์?"
  mss_cont[3] = CHR(0)+"[เฎคฎซฆจโ์] [H ็ โ์ แ ญ ็ ซ ] [โช ง โ์แ๏]"
  mss_cont[4] = ""
  stat_type = Out_Mess(5, "mss_cont")
  IF stat_type = 3
    RETURN 3
  ENDIF
  IF stat_type = 1
    USE (base_path+"INC_STO") SHARED AGAIN IN 0
    m.to_cont = .T.
    rec_cont   = INC_STO.PARM_REC
    rec_cont1  = INC_STO.PARM_REC1
    count_cont = INC_STO.PARM_COUNT
    sup_cd     = INC_STO.PARM_SUP
    p_refr     = INC_STO.PARM_P
    po_refr    = INC_STO.PARM_PO
    o_refr     = INC_STO.PARM_O
    pth_inc    = ALLTRIM(INC_STO.PARM_PTH)
    USE IN INC_STO
    DELETE FILE (base_path+"INC_STO.DBF")
    m.sw1 = File_O(pth_inc+"MTR_NOM.DBF")
    m.sw2 = File_O(pth_inc+"MTR_NOM.FPT")
    m.sw3 = File_O(pth_inc+"MTR-IPG.CSV")
    m.sw4 = File_O(pth_inc+"MTR-BT.CSV")
  
    IF .NOT. m.sw1 .OR. .NOT. m.sw2 .OR. .NOT. m.sw3 .OR. .NOT. m.sw4
      DIMENSION mss_cont[3]
      mss_cont[1] = ""
      mss_cont[2] = CHR(0)+"” ฉซ๋ ฏฎแโ ข้จช  ฎโ ฏเฅค๋คใ้ฅฃฎ เฅฃซ ฌฅญโ  ใโเ ็ฅญ๋..."
      mss_cont[3] = ""
      DO Out_Mess WITH 5, "mss_cont"
      stat_type = 2
    ENDIF
  ENDIF
  DELETE FILE (base_path+"INC_STO.DBF")
  RETURN stat_type
ENDIF

RETURN 2

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                        เฅเ๋ข ญจฅ ฏฎ Esc.                              บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 17.06.2011 ออออออออผ
PROCEDURE To_Interr
PRIVATE mss, s_sav

DIMENSION mss[3]
mss[1] = ""
mss[2] = CHR(0)+" [เฎคฎซฆจโ์] [ ขฅเ่จโ์] "
mss[3] = ""
IF Out_Mess(5, "mss") = 1
  RETURN .F.
ENDIF
interr = .T.
m.s_sav = SELECT()
SELECT 0
DELETE FILE (base_path+"INC_STO.DBF")
CREATE DBF (base_path+"INC_STO")	;
	(	PARM_REC	N(10),	;
		PARM_REC1	N(10),	;
		PARM_COUNT	N(10),	;
		PARM_SUP	N(10),	;
		PARM_P		L,		;
		PARM_PO		L,		;
		PARM_O		L,		;
		PARM_PTH	C(100)	)
APPEND BLANK
REPLACE	PARM_REC	WITH IIF(m.e_of_s, -1, RECNO("STO_S")),	;
		PARM_REC1	WITH IIF(EOF("STOCK"), -1, RECNO("STOCK")),	;
		PARM_COUNT	WITH cnt,				;
		PARM_SUP	WITH sup_cd,			;
		PARM_P		WITH p_refr,			;
		PARM_PO		WITH po_refr,			;
		PARM_O		WITH o_refr,			;
		PARM_PTH	WITH pth_inc
USE
SELECT (s_sav)
RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                       ฎข ๏ ง ฏจแ์ ข STO_S                             บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 17.06.2011 ออออออออผ
PROCEDURE New_Rec
PRIVATE tmpK

new_name = PADR(STO_S.NAME, name_ln)
*IF STO_S.QNT_STOCK > 0
*  tmpQnt = MIN(STO_S.QNT_STOCK, 999999)
*ELSE
*  tmpQnt = MAX(STO_S.QNT_STOCK, -99999)
*ENDIF
tmpQnt = MIN(STO_S.QNT_STOCK, 999999)
m.tmpClass = AT("4", STO_S.CLASS)
m.tmpClass = ALLTRIM(SUBSTR(STO_S.CLASS,m.tmpClass+1))
m.tmpClass = PADR(m.tmpClass, m.code_ln)
m.pr_d = STO_S.PRICE1

IF USED("DMS")
  IF STOCK.QNT = 0 .AND. STOCK.QNT_SAVE = 0 .AND. STOCK.QNT_REAL = 0
    IF tmpQnt = 0 .OR. pr_d = 0
      DMS_Code = ALLTRIM(STR(STO_S.CODE))
      DMS_Code = PADR(DMS_Code, DMS_Sz)
      IF SEEK(DMS_Code, "DMS")
        pr_d = DMS.PRICE1
      ENDIF
    ENDIF
  ENDIF
ENDIF

IF DEVELOP.CODE # 0
  IF SEEK(DEVELOP.CODE, "BREND_SP")
    tmpQnt = BREND_SP.QNT
  ENDIF
ENDIF
m.tmpK = Disc_K(DEVELOP.CODE)
pr_d = ROUND(pr_d*m.tmpK, 4)
pr_r = Clc_Rub(pr_d)

RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                        ฎํไไจๆจฅญโ แชจคชจ                              บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 17.06.2011 ออออออออผ
PROCEDURE Disc_K
PARAMETERS tmpBrend
PRIVATE i

IF ln_brend = 0
  RETURN dsc_main
ENDIF

i = AT(STR(tmpBrend), lst_brend)
IF i = 0
  RETURN dsc_main
ENDIF

i = (i-1)/10+1
RETURN dsc_brend(i)
