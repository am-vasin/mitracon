*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                     เฎฃเ ฌฌ  ฎโ็ฅโ  …€€’€.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE R_Report

PRIVATE w_path, a_dir_t, f_names, n_1, n_2, f_num, d_nam, t_nam
PRIVATE w, w1, i, j, mss, c_code, c_name, c_vl, pdf, prf, pds, prs
PRIVATE sum_rf, sum_df, sum_rs, sum_ds, srf, sdf, srs, sds
PRIVATE f_cb, f_nb, f_cw, f_nw, is_check1, is_check2
PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2
PRIVATE rep_n, rep_d, repeate
PRIVATE max_1, max_2
STORE .F. TO is_check1, is_check2
max_1 = 0
max_2 = 0
rep_n = ""
rep_d = {}
repeate = .F.
shab1 = "abcdefghijklmnopqrstuvwxyz กขฃคฅ๑ฆงจฉชซฌญฎฏเแโใไๅๆ็่้๊๋์ํ๎๏"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ€…๐‘’“”•–—"
IF File_O(base_path+"B_FIRM.MEM")
  RESTORE FROM (base_path+"B_FIRM.MEM") ADDITIVE
ELSE
  f_c000 = def_firm
ENDIF
f_cb = f_c000
IF File_O(base_path+"W_FIRM.MEM")
  RESTORE FROM (base_path+"W_FIRM.MEM") ADDITIVE
ELSE
  f_c000 = def_firm
ENDIF
f_cw = f_c000
f_n000 = ""
f_c_old= ""
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE AGAIN
SEEK f_cb
f_nb = B_NAME
SEEK f_cw
f_nw = B_NAME

*
*  ฎซใ็ ฅฌ ฏใโ์ ช ไ ฉซใ ฎโ็ฅโ 
*
i = Wh_Mode()
STORE 0 TO c_vl, sum_rf, sum_df, sum_rs, sum_ds
t_nam = ""
d_nam = ""
c_code = 0
c_name = ""
DO CASE
CASE i = "exit"
  RETURN
CASE i = "FDD"    && ‘ คจแชฅโ๋
  w_path = Where_F("คฅ จ้ฅฌ ฎโ็ฅโ เฅ ซจง โฎเ ?")
  IF w_path = CHR(0)
    RETURN
  ENDIF
  IF LEN(w_path) > 0
    w_path = IIF(RIGHT(w_path,1) = "\", w_path, w_path+"\")
  ENDIF

  *
  *   ‘ฌฎโเจฌ ญ ซจ็จฅ แฎฎโขฅโแโขใ๎้จๅ ไ ฉซฎข
  *

  n_1 = ADIR(a_dir_t, w_path+"ST*.DBF")
  USE ACCOUNT ORDER TAG CUS_CODE

  IF n_1 # 0   && —โฎ-โฎ ฅแโ์, ง โฅข ฅฌ จแ็ฅเฏ๋ข ๎้ใ๎ ฏเฎขฅเชใ...
    f_num = 0
    j = 1
    n_2 = 0
    DO WHILE TYPE("a_dir_t(j,1)") = "C"
      *   แฅ ซจ แจฌขฎซ๋ จฌฅญจ ฏฎแซฅ โเฅโ์ฅฃฎ - ๆจไเ๋
      i = 3
      w = SUBSTR(a_dir_t(j,1),i,1)
      DO WHILE w # "."
        IF ISDIGIT(w)
          i = i+1
          w = SUBSTR(a_dir_t(j,1),i,1)
          LOOP
        ENDIF
        w = "*"   && ฅฏเ ขจซ์ญฎฅ จฌ๏ ไ ฉซ 
        EXIT
      ENDDO
      IF w = "*" .OR. SUBSTR(a_dir_t(j,1),3,1) = "."  && ฎแซฅ st - ญฅ ็จแซฎ
        = ADEL(a_dir_t, j)                            &&    จซจ ฏใแโฎ
        n_2 = n_2+1
        LOOP
      ENDIF
      w1 = STUFF(a_dir_t(j,1), 2, 1, "L")
      IF .NOT. File_O(w_path+w1)           && ฅโ คฅโ ซ์ญฎฃฎ ไ ฉซ 
        = ADEL(a_dir_t, j)                            &&    จซจ ฏใแโฎ
        n_2 = n_2+1
        LOOP
      ENDIF
      w = AT(".", w1)
      w = SUBSTR(w1, 3, w-3)
      c_code = VAL(w)
      IF .NOT. SEEK(c_code)
        = ADEL(a_dir_t, j)
        n_2 = n_2+1
        LOOP
      ENDIF
      IF ACC_TYPE # 4
        = ADEL(a_dir_t, j)
        n_2 = n_2+1
        LOOP
      ENDIF
      f_num = f_num+1
      DIMENSION f_names(f_num)
      f_names(f_num) = ACCOUNT.CUS_NAME
      j = j+1
    ENDDO
    n_1 = n_1 - n_2
  ENDIF

  IF n_1 = 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"ฅโ โ ฌ ญจช ชจๅ ฎโ็ฅโฎข เฅ ซจง โฎเฎข!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    CLOSE DATABASES
    RETURN
  ENDIF
  DIMENSION a_dir_t(n_1, 4)

  *
  *   ฎช ง๋ข ฅฌ, ็โฎ ญ ่ซจ, แ โฅฌ, ็โฎ ก๋ ๎งฅเ ข๋กเ ซ
  *

  PRIVATE ln, wd

  ln = MIN(WROWS("")-5, f_num+3)
  wd = LEN(f_names(1))+4

  DO D_W WITH "Lst", ln, wd, "ฅ ซจง โฎเ๋:", 0

  i = 1
  DO WHILE .T.
    i = MAX(i, 1)
    i = MIN(i, f_num)
    @ 1, 1 MENU f_names, f_num, ln-3
    READ MENU TO i
    w = READKEY()%256
    IF w = 12
      CLOSE DATABASES
      RELEASE WINDOW Lst
      RETURN
    ENDIF
    IF w = 15
      RELEASE WINDOW Lst
      c_name = ALLTRIM(f_names(i))
      EXIT
    ENDIF
  ENDDO


  n_1 = a_dir_t(i, 1)           && ฌ๏ ไ ฉซ  ง ฃฎซฎขชฎข
  n_2 = STUFF(n_1, 2, 1, "L")   && ฌ๏ ไ ฉซ  แฎคฅเฆจฌฎฃฎ
  w = AT(".", n_1)
  w = SUBSTR(n_1, 3, w-3)
  c_code = VAL(w)

  DO Use_Dummy

  SELECT 0
  USE (base_path+"STOCK") ORDER TAG CODE

  SELECT 0
  USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE
  IF .NOT. SEEK(c_code)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ไจซจ ซฎฌ ญจ็ฅฃฎ ญฅ ็จแซจโแ๏!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    CLOSE DATABASES
    RETURN
  ENDIF
  c_vl = VALUTA
  DIMENSION mss(5)

  mss(1) = ""
  mss(2) = CHR(0)+'”จซจ ซ "'+c_name+'"'
  mss(3) = CHR(0)+"–ฅญ๋ ใแโ ญฎขซฅญ๋ "+TRANSFORM(DATE_P,"@D ")+  ;
                  IIF(c_vl = "$", " ข USD.", "ข เใกซ๏ๅ.")
  mss(4) = CHR(0)+"  เฎคฎซฆ ฅฌ เ กฎโใ? [   ] [ ฅโ ]"
  mss(5) = ""

  IF Out_Mess(5,"mss") = 2
    CLOSE DATABASES
    RETURN
  ENDIF

  USE (base_path+"RYN_L") ORDER TAG C_CODE
  w  = FSIZE("CUS_CODE")
  w1 = FSIZE("CODE")
  IF .NOT. SEEK(STR(c_code,w))
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  ไจซจ ซฎฌ ญจ็ฅฃฎ ญฅ ็จแซจโแ๏!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    CLOSE DATABASES
    RETURN
  ENDIF

  *
  *  ”ฎเฌจเใฅฌ ฏฎคเฎกญ๋ฉ คฅโ ซ์ญ๋ฉ ไ ฉซ
  *
  SELECT 0
  *  ” ฉซ ฎ่จกฎช
  IF File_O(tmpo_path+"ERR_TMP.DBF")
    DELETE FILE (tmpo_path+"ERR_TMP.DBF")
  ENDIF
  CREATE DBF (tmpo_path+"ERR_TMP.DBF")  ;
       ( CODE      N( 7),   ;
         PREFIX    C( 4),   ;
         NAME      C(35),   ;
         PRODUCER  C( 8),   ;
         MESS      C(40)   )

  *  ฅโ ซ์ญ๋ฉ ไ ฉซ
  SELECT 0

  d_nam = "rrd"+RIGHT(STR(100000+c_code,6),5)
  t_nam = "rrt"+RIGHT(STR(100000+c_code,6),5)

  IF File_O(tmp_path+d_nam+".DBF")
    DELETE FILE (tmp_path+d_nam+".DBF")
    DELETE FILE (tmp_path+d_nam+".CDX")
  ENDIF
  CREATE DBF (tmp_path+d_nam+".DBF")  ;
       ( DOC_NUM   C(10),   ;
         FIRM      C( 3),   ;
         DOC_DATE  D,       ;
         CODE      N( 7),   ;
         QNT       N( 6),   ;
         PRICE_R_F N(12,5), ;
         PRICE_D_F N(12,4), ;
         PRICE_R_S N(12,5), ;
         PRICE_D_S N(12,4)   )
  USE (tmp_path+d_nam+".DBF") EXCLUSIVE ALIAS RRD_TMP

  INDEX ON FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM TAG DOC_NUM

  PRIVATE k_div    && ฎํไไจๆจฅญโ คซ๏ เ งญ๋ๅ เใกซฅฉ (คฎ จ ฏฎแซฅ คฅญฎฌ.)
  PRIVATE n_format && เจงญ ช ญฎขฎฃฎ ไฎเฌ โ 
  SELECT 0
  USE (w_path+n_2) ALIAS D_INP
  k_div = See_Rub()    && ฎํไไจๆจฅญโ คซ๏ เ งญ๋ๅ เใกซฅฉ (คฎ จ ฏฎแซฅ คฅญฎฌ.)
  n_format = FSIZE("FIRM") # 0

  SET RELATION TO CODE INTO STOCK,   ;
                  STR(c_code,w)+STR(CODE,w1) INTO RYN_L

  GO TOP
  SCAN
    IF .NOT. FOUND("STOCK")
      SELECT ERR_TMP
      APPEND BLANK
      REPLACE NAME WITH "ฅฎฏเฅคฅซฅญ ",   ;
              MESS WITH "ฅโ ข ญฎฌฅญชซ โใเฅ - จฃญฎเจเใฅฌ!"
      SELECT D_INP
      LOOP
    ENDIF
    IF c_vl = "$"
      prf = Clc_Rub(D_INP.PRICE_D)
      pdf = D_INP.PRICE_D
    ELSE
      prf = ROUND(D_INP.PRICE_R/k_div,2)
      pdf = 0
    ENDIF
    IF prf <= 0
      SELECT ERR_TMP
      APPEND BLANK
      REPLACE CODE     WITH STOCK.CODE,    ;
              PREFIX   WITH STOCK.PREFIX,  ;
              NAME     WITH STOCK.NAME,    ;
              PRODUCER WITH STOCK.PRODUCER,;
              MESS     WITH "ใแโ ๏ ๆฅญ !"
      SELECT D_INP
    ENDIF
    IF .NOT. FOUND("RYN_L")
      prs = STOCK.PRICE_R
      pds = STOCK.PRICE
    ELSE
      IF c_vl = "$"
        prs = Clc_Rub(RYN_L.PRICE_D)
        pds = RYN_L.PRICE_D
      ELSE
        prs = RYN_L.PRICE_R
        pds = 0
      ENDIF
    ENDIF
    IF TYPE("DOC_NUM") = "C"
      i = DOC_NUM
    ELSE
      i = "0"+CHRTRAN(WHERE," ","0")+STR(DOC_NUM,6)
    ENDIF
    SELECT RRD_TMP
    APPEND BLANK
    REPLACE DOC_NUM   WITH i,              ;
            FIRM      WITH IIF(n_format, D_INP.FIRM, ""), ;
            DOC_DATE  WITH D_INP.DOC_DATE, ;
            CODE      WITH D_INP.CODE,     ;
            QNT       WITH D_INP.QNT,      ;
            PRICE_R_F WITH prf,            ;
            PRICE_D_F WITH pdf,            ;
            PRICE_R_S WITH prs,            ;
            PRICE_D_S WITH pds
    SELECT D_INP
  ENDSCAN
  USE

  SELECT ERR_TMP
  IF RECCOUNT("ERR_TMP") # 0
    DO Show_Err
  ENDIF
  USE
  DELETE FILE (tmpo_path+"ERR_TMP.DBF")

  *
  *   ”ฎเฌจเใฅฌ ไ ฉซ ง ฃฎซฎขชฎข
  *
  IF File_O(tmp_path+t_nam+".DBF")
    DELETE FILE (tmp_path+t_nam+".DBF")
    DELETE FILE (tmp_path+t_nam+".CDX")
  ENDIF
  CREATE DBF (tmp_path+t_nam+".DBF")  ;
       ( LST_TYPE  C( 1),   ;
         DOC_NUM   C(10),   ;
         FIRM      C( 3),   ;
         DOC_DATE  D,       ;
         CUS_CODE  N( 8),   ;
         CUS_NAME  C(50),   ;
         DOC_NAME  C(16),   ;
         PAY_TYPE  C( 6),   ;
         TOTAL_R_F N(13,2), ;
         TOTAL_D_F N(13,4), ;
         TOTAL_R_S N(13,2), ;
         TOTAL_D_S N(13,4), ;
         VALUTA    C( 1),   ;
         NOTE      C(16)  )

  USE (tmp_path+t_nam+".DBF") EXCLUSIVE ALIAS RRT_TMP

  INDEX ON FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM TAG DOC_NUM

  SELECT 0
  USE (w_path+n_1) ALIAS T_INP
  SCAN
    IF FSIZE("N_REP") # 0
      rep_n = STR(N_REP,10)
      rep_d = D_REP
    ENDIF
    IF TYPE("DOC_NUM") = "C"
      i = DOC_NUM
    ELSE
      i = "0"+CHRTRAN(WHERE," ","0")+STR(DOC_NUM,6)
    ENDIF
    IF n_format
      j = IIF("0" $ FIRM, "", "๛")
    ELSE
      DO CASE
      CASE .NOT. EMPTY(SW_NAL)
        j = " ก/ญ"
      CASE ORD_NO # 0
        j = STR(ORD_NO,6)
      OTHERWISE
        j = " "
      ENDCASE
    ENDIF
    SELECT RRT_TMP
    APPEND BLANK
    REPLACE LST_TYPE WITH IIF(EMPTY(j), " ", "๛"), ;
            FIRM     WITH IIF(n_format, T_INP.FIRM, ""), ;
            DOC_NUM  WITH i,              ;
            DOC_DATE WITH T_INP.DOC_DATE, ;
            DOC_NAME WITH T_INP.CUS_NAME, ;
            VALUTA   WITH c_vl,           ;
            PAY_TYPE WITH j
    SELECT T_INP
  ENDSCAN
  USE
  IF .NOT. EMPTY(rep_n)
    USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE
    SEEK c_code
    SCAN REST WHILE CUS_CODE = c_code FOR EMPTY(LIST_NUM)
      IF rep_n = REQ_DOC .AND. rep_d = REQ_DATE
        repeate = .T.
        EXIT
      ENDIF
    ENDSCAN
    USE
  ENDIF

  SELECT RRD_TMP
  GO TOP
  i = IIF(n_format, FIRM, "   ")+LEFT(DTOS(DOC_DATE),4)+DOC_NUM
  STORE 0 TO sum_rf, sum_df, sum_rs, sum_ds, srf, sdf, srs, sds
  SCAN
    IF FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM # i
      IF .NOT. SEEK(i, "RRT_TMP")
        CLOSE DATABASES
        DELETE FILE (tmp_path+t_nam+".DBF")
        DELETE FILE (tmp_path+d_nam+".DBF")
        DELETE FILE (tmp_path+t_nam+".CDX")
        DELETE FILE (tmp_path+d_nam+".CDX")
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+' ฌ ขแใ็จซจ "ชเจขฎฉ" ฎโ็ฅโ - ญฅ ๅข โ ฅโ ง ฃฎซฎขชฎข!'
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        RETURN
      ENDIF
      SELECT RRT_TMP
      REPLACE TOTAL_R_F WITH srf, ;
              TOTAL_D_F WITH sdf, ;
              TOTAL_R_S WITH srs, ;
              TOTAL_D_S WITH sds
      sum_rf = sum_rf+srf
      sum_df = sum_df+sdf
      sum_rs = sum_rs+srs
      sum_ds = sum_ds+sds
      STORE 0 TO srf, sdf, srs, sds
      SELECT RRD_TMP
      i = FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM
    ENDIF
    srf = srf+ROUND(PRICE_R_F*QNT,2)
    sdf = sdf+PRICE_D_F*QNT
    srs = srs+ROUND(PRICE_R_S*QNT,2)
    sds = sds+PRICE_D_S*QNT
  ENDSCAN
  IF .NOT. SEEK(i, "RRT_TMP")
    CLOSE DATABASES
    DELETE FILE (tmp_path+t_nam+".DBF")
    DELETE FILE (tmp_path+d_nam+".DBF")
    DELETE FILE (tmp_path+t_nam+".CDX")
    DELETE FILE (tmp_path+d_nam+".CDX")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+' ฌ ขแใ็จซจ "ชเจขฎฉ" ฎโ็ฅโ - ญฅ ๅข โ ฅโ ง ฃฎซฎขชฎข!'
    DO Out_Mess WITH 7, "mss"
    RETURN
  ENDIF
  SELECT RRT_TMP
  REPLACE TOTAL_R_F WITH srf, ;
          TOTAL_D_F WITH sdf, ;
          TOTAL_R_S WITH srs, ;
          TOTAL_D_S WITH sds
  sum_rf = sum_rf+srf
  sum_df = sum_df+sdf
  sum_rs = sum_rs+srs
  sum_ds = sum_ds+sds
CASE i = "continue"
  IF .NOT. Ch_Rest()
    RETURN
  ENDIF
CASE i = "manual"
  IF .NOT. Man_Inp()
    CLOSE DATABASES
    RETURN
  ENDIF
ENDCASE
*d_nam = "rrd"+RIGHT(STR(100000+c_code,6),5)
*t_nam = "rrt"+RIGHT(STR(100000+c_code,6),5)
IF repeate
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"€…! โฎโ ฎโ็ฅโ ใฆฅ ฏเจญจฌ ซจ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

*
*  เฎแฌฎโเ ฏฅเฅ็ญ๏ ญ ชซ คญ๋ๅ จ ชฎเเฅชๆจ๏ ฎโฌฅโฎช
*
SELECT RRD_TMP
SET RELATION TO CODE INTO STOCK
DO Int_List    && เฎแฌฎโเ จ ชฎเเฅชๆจ๏ ญ ชซ คญ๋ๅ

CLOSE DATABASES
DIMENSION mss(4)
mss(1) = ""
mss(2) = CHR(0)+" ช ฏฎแโใฏจฌ แ ง ฃฎโฎขชฎฉ?"
mss(3) = CHR(0)+"   [ ‘ไฎเฌจเใฅฌ ฏฎ ญจฌ ญ ชซ คญ๋ฅ] [ โซฎฆจฌ ] [  กใคฅฌ ]"
mss(4) = ""
i = Out_Mess(5, "mss")

IF i = 1
  *
  *  คฅแ์ คฎซฆญ  ก๋โ์ ฏเฎค ฆ !
  *
  DO Prp_L2    && เฎแฌฎโเ จ ฏเฎค ฆ  คขใๅ แฏจแชฎข
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"LIST_2.DBF")
  DELETE FILE (tmpo_path+"LIST_2.CDX")
  RETURN
ENDIF
IF i = 2
  RETURN
ENDIF

DELETE FILE (tmp_path+t_nam+".DBF")
DELETE FILE (tmp_path+d_nam+".DBF")
DELETE FILE (tmp_path+t_nam+".CDX")
DELETE FILE (tmp_path+d_nam+".CDX")

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎฃเ ฌฌ  ฏเฎแฌฎโเ  คฅไฅชโญ๋ๅ ฏฎงจๆจฉ.                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Show_Err
PRIVATE ln, wd, tt, ft

DIMENSION tt(1), ft(1)
tt(1) = PADC(" ฎงจๆจ๏ ฤฤฤฤฤฤฤ",  FSIZE("PRODUCER")+FSIZE("PREFIX")+FSIZE("NAME")+3,"ฤ")+ ;
        PADC(" เจฌฅ็ ญจฅ ฤฤฤฤฤฤฤฤฤฤฤ", 20,"ฤ")
ft(1) = PADC(" ซ๏ ฏเฎคฎซฆฅญจ๏ เ กฎโ๋ ญ ฆฌจโฅ Esc, ฅ็ โ์ - Tab ", LEN(tt(1)), "ฤ")
ln = MIN(WROWS("")-8, RECCOUNT())
wd = LEN(tt(1))

ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13

DO Def_Win WITH "Err_M", "Err_E", "Err_I", ln, wd,  ;
                "‘ฏจแฎช ญฅชฎเเฅชโญ๋ๅ ฏฎงจๆจฉ", tt, ft

PUSH KEY CLEAR
ACTIVATE WINDOW Err_E
ON KEY LABEL Tab DO P_Err
BROWSE FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="", MESS:20:H="",  ;
              DUMMY.F FREEZE DUMMY.F              ;
              NOMENU NOMODIFY WINDOW Err_I IN WINDOW Err_E

ON KEY
POP KEY
RELEASE WINDOW Err_I
RELEASE WINDOW Err_E
RELEASE WINDOW Err_M

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎฃเ ฌฌ  ฏเฎแฌฎโเ  คฅไฅชโญ๋ๅ ฏฎงจๆจฉ.                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE View_Err
PRIVATE ln, wd, tt, ft

DIMENSION tt(1), ft(1)
tt(1) = PADC(" ฎงจๆจ๏ ฤฤฤฤฤฤฤ", 49,"ฤ")+"ฤฤโ็ฅโฤฤเฎค."
ft(1) = PADC(" Esc - ฏเฎคฎซฆจโ์, Tab - ฏฅ็ โ์ ", LEN(tt(1)), "ฤ")
ln = MIN(WROWS("")-8, RECCOUNT())
wd = LEN(tt(1))

ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13

DO Def_Win WITH "Err_M", "Err_E", "Err_I", ln, wd,  ;
                "‘ฏจแฎช ญฅชฎเเฅชโญ๋ๅ ฏฎงจๆจฉ", tt, ft

PUSH KEY CLEAR
ON KEY LABEL Tab DO P_Err WITH .T.
ACTIVATE WINDOW Err_E
BROWSE FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="", QNT_REQ:H="", QNT_SAL:H="", ;
              DUMMY.F FREEZE DUMMY.F              ;
              NOMENU NOMODIFY WINDOW Err_I IN WINDOW Err_E
ON KEY
POP KEY

RELEASE WINDOW Err_I
RELEASE WINDOW Err_E
RELEASE WINDOW Err_M

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ    เฎฃเ ฌฌ  ฏเฎแฌฎโเ  จ ชฎเเฅชๆจจ ฏฅเฅ็ญ๏ ญ ชซ คญ๋ๅ เฅ ซจง โฎเ .     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Int_List
PRIVATE ln, wd, tt, ft, flt_vl, what_do

DEFINE MENU Int_List IN SCREEN COLOR SCHEME 4
DEFINE PAD Mark OF Int_List PROMPT "ฎฌฅโช "
ON PAD Mark OF Int_List ACTIVATE POPUP Mark

  DEFINE POPUP Mark MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Mark PROMPT "ญขฅเโจเฎข โ์" KEY Ctrl-A, "F3"
  ON SELECTION BAR 1 OF Mark DO Sw_Mark

  DEFINE BAR  2 OF Mark PROMPT "โฌฅโจโ์ ขแฅ" KEY Ctrl-A, "F6"
  ON SELECTION BAR 2 OF Mark DO Mark_All WITH "๛"

  DEFINE BAR  3 OF Mark PROMPT "็จแโจโ์ ขแฅ" KEY Ctrl-A, "F7"
  ON SELECTION BAR 3 OF Mark DO Mark_All WITH " "

IF c_vl = "$"
  DEFINE PAD Valuta OF Int_List PROMPT " ซ๎โ " KEY Ctrl-A, "F4"
  ON SELECTION PAD Valuta OF Int_List DO Ch_Mode WITH "Int_List","valuta"
ENDIF
DEFINE PAD View OF Int_List PROMPT "เฎแฌฎโเ" KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF Int_List DO Ch_Mode WITH "Int_List","view"
DEFINE PAD Div OF Int_List PROMPT "ฅญ์ฃจ" KEY Ctrl-A, "F5"
ON SELECTION PAD Div OF Int_List DO Show_Div
DEFINE PAD Exit OF Int_List PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF Int_List DO Ch_Mode WITH "Int_List","exit"

DIMENSION tt(1), ft(2)
SELECT RRT_TMP
tt(1) = "ฤฤ"+PADC("N ฎช-โ ", FSIZE("DOC_NUM")+1,"ฤ")+"ฤ"+ ;
             PADC(" โ ", IIF(SET("CENTURY")="ON", 10, 8),"ฤ")+"ฤ"+ ;
             PADC("ฎชใฏ โฅซ์", FSIZE("DOC_NAME"),"ฤ")+"ฤ"+ ;
             PADC("ฏซ โ ", FSIZE("PAY_TYPE"),"ฤ")+"ฤ"+ ;
             PADC("‘ใฌฌ  แชซ.", 13,"ฤ")+"ฤ"+ ;
             PADC("‘ใฌฌ  เซ.", 13,"ฤ")+"ฤฤ"
ft(1) = ""
ft(2) = "                    โฎฃฎ"
ln = MIN(WROWS("")-9, RECCOUNT())
wd = LEN(tt(1))
DO Def_Win WITH "List_M", "List_E", "List_I", ln, wd,   ;
                " ชซ คญ๋ฅ เฅ ซจง โฎเ ", tt, ft
what_do = "list"
flt_vl = c_vl
IF flt_vl = "$"
  @ WROWS()-1, 49 SAY STR(sum_ds,13,4)+STR(sum_df,11,4)
ELSE
  @ WROWS()-1, 49 SAY STR(sum_rs,13,2)+STR(sum_rf,11,2)
ENDIF
GO TOP
DO WHILE .T.
  DO CASE
  CASE what_do = "list"
    DO Prp_Menu WITH "Int_List"
    ON KEY LABEL F10 ACTIVATE MENU Int_List
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F3 DO Sw_Mark
    ON KEY LABEL F5 DO Show_Div
    ON KEY LABEL F6 DO Mark_All WITH "๛"
    ON KEY LABEL F7 DO Mark_All WITH " "
    
    IF c_vl = "$"
      ON KEY LABEL F4 DO Ch_Mode WITH "Int_List","valuta"
    ENDIF
    ACTIVATE WINDOW List_E
    IF flt_vl = "$"
      BROWSE FIELDS LST_TYPE:H="",          ;
                    f001 = LEFT(DOC_NUM,4)+"-"+ALLTRIM(RIGHT(DOC_NUM,6)):11:H="", ;
                    DOC_DATE:H="":P="@D ",  ;
                    DOC_NAME:H="",          ;
                    PAY_TYPE:H="",          ;
                    TOTAL_D_S,              ;
                    TOTAL_D_F,              ;
                    f002 = IIF(TOTAL_D_S = TOTAL_D_F, " ", "๛"), ;
                    DUMMY.F:H=""  FREEZE DUMMY.F   ;
                    NOMODIFY NOMENU NOCLEAR WINDOW List_I IN WINDOW List_E
    ELSE
      BROWSE FIELDS LST_TYPE:H="",          ;
                    f001 = LEFT(DOC_NUM,4)+"-"+ALLTRIM(RIGHT(DOC_NUM,6)):11:H="", ;
                    DOC_DATE:H="":P="@D ",  ;
                    DOC_NAME:H="",          ;
                    PAY_TYPE:H="",          ;
                    TOTAL_R_S,              ;
                    TOTAL_R_F,              ;
                    f002 = IIF(TOTAL_D_S = TOTAL_D_F, " ", "๛"), ;
                    DUMMY.F:H=""  FREEZE DUMMY.F   ;
                    NOMODIFY NOMENU NOCLEAR WINDOW List_I IN WINDOW List_E
    ENDIF
    ON KEY
    HIDE MENU Int_List
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    IF what_do = "list"
      IF READKEY() % 256 = 12
        what_do = "exit"
      ELSE
        what_do = "view"
      ENDIF
    ENDIF
  CASE what_do = "view"
    DO View_Doc
    what_do = "list"
  CASE what_do = "valuta"
    flt_vl = IIF(flt_vl = "$", "", "$")
    ACTIVATE WINDOW List_M BOTTOM
    IF flt_vl = "$"
      @ WROWS()-1, 49 SAY STR(sum_ds,13,4)+STR(sum_df,11,4)
    ELSE
      @ WROWS()-1, 49 SAY STR(sum_rs,13,2)+STR(sum_rf,11,2)
    ENDIF
    what_do = "list"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

RELEASE WINDOW List_M
RELEASE WINDOW List_E
RELEASE WINDOW List_I
DEACTIVATE MENU Int_List
RELEASE MENU Int_List EXTENDED

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  จญขฅเโจเฎข ญจ๏ ฏฎฌฅโชจ.                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sw_Mark

REPLACE LST_TYPE WITH IIF(LST_TYPE = " ", "๛", " ")
DEACTIVATE MENU Int_List

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                               โฌฅโจโ์ ขแฅ!                           บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Mark_All
PARAMETERS mrk
PRIVATE rc

rc = RECNO()
REPLACE ALL LST_TYPE WITH mrk
IF BETWEEN(rc, 1, RECCOUNT())
  GO rc
ENDIF

DEACTIVATE MENU Int_List

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ         เฎฃเ ฌฌ  ฏเฎแฌฎโเ  แฎคฅเฆจฌฎฃฎ ญ ชซ คญฎฉ เฅ ซจง โฎเ .        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE View_Doc
PRIVATE ln, wd, tt, ft, what_do, tit_m, k

DEFINE MENU View_Doc IN SCREEN COLOR SCHEME 4
DEFINE PAD Exit OF View_Doc PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF View_Doc DO Ch_Mode WITH "View_Doc","exit"

tit_m = " ชซ คญ ๏ N "+LEFT(DOC_NUM,4)+"-"+ALLTRIM(RIGHT(DOC_NUM,6))+ ;
        " ฎโ "+TRANSFORM(DOC_DATE,"@D ")+": "+ALLTRIM(DOC_NAME)

DIMENSION tt(1), ft(2)

tt(1) = ;
"ฤฤฤฤฤฤฤ จฌฅญฎข ญจฅฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฎซ-ขฎฤฤฤฤฤ–ฅญ  แช.ฤฤฤฤฤ–ฅญ  เซ."
*PPPP NNNNNNNNNNNNNNNNNNNNNNNNNnnnnnnnnnn 999999 999999999999 999999999999
ft(1) = ""
ft(2) = "    โฎฃฎ ฏฎ แชซ คใ           , ฏฎ ฎโ็ฅโใ           "
ln = WROWS("")-9
wd = LEN(tt(1))
DO Def_Win WITH "Det_M", "Det_E", "Det_I", ln, wd, tit_m, tt, ft
what_do = "list"
flt_vl = c_vl
IF flt_vl = "$"
  @ WROWS()-1, 24 SAY STR(TOTAL_D_S,13,4)
  @ WROWS()-1, 46 SAY STR(TOTAL_D_F,13,4)
ELSE
  @ WROWS()-1, 24 SAY STR(TOTAL_R_S,13,2)
  @ WROWS()-1, 46 SAY STR(TOTAL_R_F,13,2)
ENDIF
k = FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM
SELECT RRD_TMP

GO TOP

DO WHILE .T.
  DO CASE
  CASE what_do = "list"
    DO Prp_Menu WITH "View_Doc"
    ON KEY LABEL F10 ACTIVATE MENU View_Doc
    ACTIVATE WINDOW Det_E
    IF flt_vl = "$"
      BROWSE FIELDS STOCK.PREFIX:H="",  ;
                    STOCK.NAME:H="",    ;
                    QNT:H="",           ;
                    PRICE_D_S:H="",     ;
                    PRICE_D_F:H="",     ;
                    f001 = IIF(PRICE_D_S = PRICE_D_F, " ", "๛"):H="", ;
                    DUMMY.F:H=""  FREEZE DUMMY.F KEY k  ;
                    NOMODIFY NOMENU NOCLEAR WINDOW Det_I IN WINDOW Det_E
    ELSE
      BROWSE FIELDS STOCK.PREFIX:H="",  ;
                    STOCK.NAME:H="",    ;
                    QNT:H="",           ;
                    PRICE_R_S:H="",     ;
                    PRICE_R_F:H="",     ;
                    f001 = IIF(PRICE_R_S = PRICE_R_F, " ", "๛"):H="", ;
                    DUMMY.F:H=""  FREEZE DUMMY.F KEY k  ;
                    NOMODIFY NOMENU NOCLEAR WINDOW Det_I IN WINDOW Det_E
    ENDIF
    ON KEY
    HIDE MENU View_Doc
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    IF what_do = "list"
      what_do = "exit"
    ENDIF
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

RELEASE WINDOW Det_M
RELEASE WINDOW Det_E
RELEASE WINDOW Det_I
DEACTIVATE MENU View_Doc
RELEASE MENU View_Doc EXTENDED
SELECT RRT_TMP

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    แชซ คช  คฅญฅฃ ฏฎ โจฏ ฌ ญ ชซ คญ๋ๅ.                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Show_Div
PRIVATE ln, wd, tit, rc_sav, s11, s12, s21, s22

DEFINE WINDOW Hide_Lin FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW Hide_Lin TOP
ln = 9
wd = 35
tit = " แชซ. คฅญฅฃ ฏฎ โจฏ ฌ ญ ชซ คญ๋ๅ"

DO D_W WITH "Show_Div", ln, wd, tit, 0

@ 0, 1 SAY ""
? " ฺฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฟ"
? " ณ’จฏ ญ ชซ.ณ  ‘ชซ ค   ณฅ ซจง โฎเณ"
? " รฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤด"
? " ณ  โฌ.   ณ          ณ          ณ"
? " รฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤด"
? " ณ ฅ ฎโฌ. ณ          ณ          ณ"
? " ภฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤู"
? "        ฆฌจโฅ ซ๎กใ๎ ชซ ขจ่ใ"
rc_sav = RECNO()
STORE 0 TO s11, s12, s21, s22

SCAN
  IF flt_vl = "$"
    IF EMPTY(LST_TYPE)
      s21 = s21+TOTAL_D_S
      s22 = s22+TOTAL_D_F
    ELSE
      s11 = s11+TOTAL_D_S
      s12 = s12+TOTAL_D_F
    ENDIF
  ELSE
    IF EMPTY(LST_TYPE)
      s21 = s21+TOTAL_R_S
      s22 = s22+TOTAL_R_F
    ELSE
      s11 = s11+TOTAL_R_S
      s12 = s12+TOTAL_R_F
    ENDIF
  ENDIF
ENDSCAN

GO rc_sav

pic = IIF(flt_vl = "$", "@Z 9999999.99", "@Z 9999999.99")

rc_sav = SET("CURSOR") = "ON"
SET CURSOR OFF

@ 4, 12 SAY s11 PICTURE pic
@ 4, 23 SAY s12 PICTURE pic
@ 6, 12 SAY s21 PICTURE pic
@ 6, 23 SAY s22 PICTURE pic

= INKEY(0, "M")

RELEASE WINDOW Hide_Lin
RELEASE WINDOW Show_Div

IF rc_sav
  SET CURSOR ON
ENDIF
DEACTIVATE MENU Int_List

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      เฎฃเ ฌฌ  ข๋กฎเ  เฅฆจฌ .                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Wh_Mode

PRIVATE ln, wd, mode

mode = 1
ln = 9
wd = 36
DO D_W WITH "Wh_Mode", ln, wd, "“ช ฆจโฅ เฅฆจฌ:", 0

@ 0, 0 SAY ""
? " ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
? " ณ                                ณ"
? " ณ   < ขฎค แ คจแชฅโ๋         >   ณ"
? " ณ   < ขฎค ขเใ็ญใ๎           >   ณ"
? " ณ   < เฎคฎซฆจโ์ ฎกเ กฎโชใ   >   ณ"
? " ณ   < โช ง โ์แ๏ (Esc)       >   ณ"
? " ณ                                ณ"
? " ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"

@ 3, 5 GET mode PICTURE   ;
"@*T \ ขฎค แ คจแชฅโ๋         ;"+ ;
     " ขฎค ขเใ็ญใ๎           ;"+ ;
     " เฎคฎซฆจโ์ ฎกเ กฎโชใ   ;"+ ;
     "\? โช ง โ์แ๏ (Esc)       "

READ CYCLE
CLEAR WINDOW Wh_Mode

DO CASE
CASE mode = 4 .OR. READKEY()%256 = 12
  RETURN "exit"
CASE mode = 1
  RETURN "FDD"
CASE mode = 2
  RETURN "manual"
CASE mode = 3
  RETURN "continue"
ENDCASE

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ข๋กฎเ  ง ฃฎโฎขชจ.                         บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Ch_Rest

PRIVATE r_len, ln, wd, a_dir, a_names, a_codes, a_ind, mss, l_len, i, j, w

r_len = ADIR(a_dir, tmp_path+"RRT?????.DBF")

IF r_len = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ ญจ ฎคญฎฉ ง ฃฎโฎขชจ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

USE ACCOUNT ORDER TAG CUS_CODE

l_len = 0
FOR i = 1 TO r_len
  w = a_dir(i,1)
  FOR j = 4 TO 8
    IF .NOT. ISDIGIT(SUBSTR(w,j,1))
      LOOP
    ENDIF
  ENDFOR
  IF .NOT. File_O(tmp_path+LEFT(w,9)+"CDX")
    LOOP
  ENDIF
  w = STUFF(w, 3, 1, "D")
  IF .NOT. File_O(tmp_path+w)
    LOOP
  ENDIF
  IF .NOT. File_O(tmp_path+LEFT(w,9)+"CDX")
    LOOP
  ENDIF
  w = VAL(SUBSTR(w, 4, 5))
  IF .NOT. SEEK(w)
    LOOP
  ENDIF
  IF ACC_TYPE # 4
    LOOP
  ENDIF
  l_len = l_len+1
  DIMENSION a_names(l_len), a_codes(l_len), a_ind(l_len)
  a_names(l_len) = CUS_NAME
  a_codes(l_len) = w
  a_ind(l_len)   = i
ENDFOR

IF l_len = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ฅโ ญจ ฎคญฎฉ ง ฃฎโฎขชจ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN .F.
ENDIF

ln = MIN(WROWS("")-8, l_len+3)
wd = FSIZE("CUS_NAME")+4

DO D_W WITH "Ch_Rest", ln, wd, "๋กฅเจโฅ ง ฃฎโฎขชใ", 0
i = 1
DO WHILE .T.
  i = MAX(i, 1)
  i = MIN(i, l_len)
  @ 1,1 MENU a_names, l_len, WROWS()-3
  READ MENU TO i
  IF READKEY() % 256 = 12
    RELEASE WINDOW Ch_Rest
    CLOSE DATABASES
    RETURN .F.
  ENDIF
  IF READKEY() % 256 = 15
    EXIT
  ENDIF
ENDDO
RELEASE WINDOW Ch_Rest

DO Use_Dummy

SELECT 0
USE STOCK ORDER TAG CODE

SELECT 0
USE RYN_L ORDER TAG C_CODE

c_name = ALLTRIM(a_names(i))
c_code = a_codes(i)
i = a_ind(i)
w = a_dir(i, 1)
t_nam = LEFT(w,8)

SELECT 0
USE (tmp_path+w) ORDER TAG DOC_NUM ALIAS RRT_TMP EXCLUSIVE
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" ฃฎโฎขช  ฏใแโ !"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmp_path+w)
  DELETE FILE (tmp_path+LEFT(w,9)+"CDX")
  w = STUFF(w, 3, 1, "D")
  DELETE FILE (tmp_path+w)
  DELETE FILE (tmp_path+LEFT(w,9)+"CDX")
  RETURN .F.
ENDIF  

c_vl = VALUTA
STORE 0 TO sum_rf, sum_df, sum_rs, sum_ds
SUM TOTAL_R_F, TOTAL_D_F, TOTAL_R_S, TOTAL_D_S TO  ;
    sum_rf,    sum_df,    sum_rs,    sum_ds

w = STUFF(w, 3, 1, "D")
SELECT 0
USE (tmp_path+w) ORDER TAG DOC_NUM ALIAS RRD_TMP EXCLUSIVE
d_nam = LEFT(w,8)

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  เใ็ญฎฃฎ ขขฎค  แฏจแช .                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Man_Inp

PRIVATE r_list, r_numb, r_codes, i, ln, wd, mss
PRIVATE s1, s2, what_do

USE ACCOUNT ORDER TAG ACC_TYPE

SEEK "4"
r_numb = 0
SCAN REST WHILE ACC_TYPE = 4
  r_numb = r_numb+1
  DIMENSION r_list(r_numb), r_codes(r_numb)
  r_list(r_numb)  = CUS_NAME
  r_codes(r_numb) = CUS_CODE
ENDSCAN

SET ORDER TO TAG CUS_CODE

*
*   ๋กจเ ฅฌ
*
ln = MIN(WROWS("")-8, r_numb+3)
wd = FSIZE("CUS_NAME")+4

DO D_W WITH "Ch_Real", ln, wd, "๋กฅเจโฅ เฅ ซจง โฎเ ", 0
i = 1
DO WHILE .T.
  i = MAX(i, 1)
  i = MIN(i, r_numb)
  @ 1,1 MENU r_list, r_numb, WROWS()-3
  READ MENU TO i
  IF READKEY() % 256 = 12
    RELEASE WINDOW Ch_Real
    CLOSE DATABASES
    RETURN .F.
  ENDIF
  IF READKEY() % 256 = 15
    EXIT
  ENDIF
ENDDO
RELEASE WINDOW Ch_Real
c_code = r_codes(i)
c_name = ALLTRIM(r_list(i))

DO Use_Dummy

SELECT 0
USE RYN_L_T ORDER TAG CUS_CODE
IF .NOT. SEEK(c_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"€ ง  เฅ ซจง โฎเฎฌ ญจ็ฅฃฎ ญฅ ็จแซจโแ๏..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN .F.
ENDIF

c_vl = VALUTA
DIMENSION mss(5)
mss(1) = ""
mss(2) = CHR(0)+'ซจฅญโ "'+ALLTRIM(r_list(i))+'".'
mss(3) = CHR(0)+"–ฅญ๋ ใแโ ญฎขซฅญ๋ "+TRANSFORM(DATE_P,"@E ")+" ข "+  ;
         IIF(c_vl = "$", "USD.", "เใกซ๏ๅ.")
mss(4) = CHR(0)+"  เฎคฎซฆ ฅฌ เ กฎโใ? [   ] [ ฅโ ]"
mss(5) = ""
IF Out_Mess(5, "mss") = 2
  CLOSE DATABASES
  RETURN .F.
ENDIF

t_nam = "RRT"+RIGHT(STR(100000+c_code,6),5)
d_nam = "RRD"+RIGHT(STR(100000+c_code,6),5)

SELECT 0
USE RYN_L ORDER TAG C_CODE

SELECT 0
IF .NOT. File_O(tmp_path+t_nam+".DBF") .OR.   ;
   .NOT. File_O(tmp_path+t_nam+".CDX") .OR.   ;
   .NOT. File_O(tmp_path+d_nam+".DBF") .OR.   ;
   .NOT. File_O(tmp_path+d_nam+".CDX")

  DELETE FILE (tmp_path+t_nam+".DBF")
  DELETE FILE (tmp_path+t_nam+".CDX")
  DELETE FILE (tmp_path+d_nam+".DBF")
  DELETE FILE (tmp_path+d_nam+".CDX")
  CREATE DBF (tmp_path+t_nam+".DBF")  ;
       ( LST_TYPE  C( 1),   ;
         DOC_NUM   C(10),   ;
         FIRM      C( 3),   ;
         DOC_DATE  D,       ;
         CUS_CODE  N( 8),   ;
         CUS_NAME  C(50),   ;
         DOC_NAME  C(16),   ;
         PAY_TYPE  C( 6),   ;
         TOTAL_R_F N(13,2), ;
         TOTAL_D_F N(13,4), ;
         TOTAL_R_S N(13,2), ;
         TOTAL_D_S N(13,4), ;
         VALUTA    C( 1),   ;
         NOTE      C(16)  )

  USE (tmp_path+t_nam+".DBF") EXCLUSIVE ALIAS RRT_TMP
  INDEX ON FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM TAG DOC_NUM

ELSE
  USE (tmp_path+t_nam+".DBF") EXCLUSIVE ALIAS RRT_TMP ORDER TAG DOC_NUM
ENDIF

REPLACE ALL VALUTA WITH c_vl

SELECT 0
IF .NOT. File_O(tmp_path+d_nam+".DBF") .OR.   ;
   .NOT. File_O(tmp_path+d_nam+".CDX")

  CREATE DBF (tmp_path+d_nam+".DBF")  ;
       ( DOC_NUM   C(10),   ;
         FIRM      C( 3),   ;
         DOC_DATE  D,       ;
         CODE      N( 7),   ;
         QNT       N( 6),   ;
         PRICE_R_F N(12,2), ;
         PRICE_D_F N(12,4), ;
         PRICE_R_S N(12,2), ;
         PRICE_D_S N(12,4)   )

  USE (tmp_path+d_nam+".DBF") EXCLUSIVE ALIAS RRD_TMP
  INDEX ON FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM TAG DOC_NUM

ELSE
  USE (tmp_path+d_nam+".DBF") EXCLUSIVE ALIAS RRD_TMP ORDER TAG DOC_NUM
ENDIF

SET RELATION TO STR(c_code,5)+STR(CODE,7) INTO RYN_L

REPLACE ALL FOR FOUND("RYN_L")   ;
            PRICE_R_S WITH RYN_L.PRICE_R, ;
            PRICE_D_S WITH RYN_L.PRICE_D
SET RELATION TO
INDEX ON LEFT(DTOS(DOC_DATE),4)+DOC_NUM+STR(CODE,7) TAG CODE ADDITIVE

SELECT RRT_TMP
SCAN
  SELECT RRD_TMP
  SEEK LEFT(DTOS(RRT_TMP.DOC_DATE),4)+RRT_TMP.DOC_NUM
  SUM PRICE_R_S*QNT, PRICE_D_S*QNT  ;
      REST WHILE DOC_DATE = RRT_TMP.DOC_DATE .AND. DOC_NUM = RRT_TMP.DOC_NUM ;
      TO s1, s2
  SELECT RRT_TMP
  REPLACE TOTAL_R_S WITH s1,   ;
          TOTAL_D_S WITH s2
ENDSCAN

SELECT 0
USE STOCK ORDER TAG CODE

SELECT 0
DELETE FILE (tmpo_path+"R_STOCK.DBF")
DELETE FILE (tmpo_path+"R_STOCK.CDX")

CREATE DBF (tmpo_path+"R_STOCK.DBF")  ;
       ( CODE      N( 7),   ;
         PREFIX    C( 4),   ;
         NAME      C(35),   ;
         QNT_S     N( 6),   ;
         QNT       N( 6),   ;
         PRICE_R   N( 8,2), ;
         PRICE_D   N(10,4)   )

USE (tmpo_path+"R_STOCK.DBF")
INDEX ON NAME+PREFIX TAG NAME

SELECT RYN_L
SET RELATION TO CODE INTO STOCK
SEEK STR(c_code,5)
SCAN REST WHILE CUS_CODE = c_code
  SELECT R_STOCK
  APPEND BLANK
  REPLACE CODE    WITH RYN_L.CODE,   ;
          PREFIX  WITH STOCK.PREFIX, ;
          NAME    WITH STOCK.NAME,   ;
          QNT_S   WITH RYN_L.QNT,    ;
          PRICE_R WITH RYN_L.PRICE_R,;
          PRICE_D WITH RYN_L.PRICE_D
  SELECT RYN_L
ENDSCAN
SET RELATION TO

DEFINE MENU T_List IN SCREEN COLOR SCHEME 4
DEFINE PAD Edit OF T_List PROMPT "ฅค ชโจเฎข โ์" KEY Ctrl-A, "Enter"
ON SELECTION PAD Edit OF T_List DO Ch_Mode WITH "T_List","d_list"
DEFINE PAD Del OF T_List PROMPT "“ค ซจโ์" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF T_List DO Ch_Mode WITH "T_List","del_t"
DEFINE PAD Add OF T_List PROMPT "ฎก ขจโ์" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF T_List DO Ch_Mode WITH "T_List","add_t"
DEFINE PAD Exit OF T_List PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF T_List DO Ch_Mode WITH "T_List","exit"

SELECT RRT_TMP
DIMENSION tt(1)
tt(1) = PADC("N คฎช-โ ",FSIZE("DOC_NUM")+1,"ฤ")+"ฤ"+ ;
        PADC(" โ ",IIF(SET("DATE")="ON", 10, 8),"ฤ")+"ฤ"+ ;
        PADC("ซจฅญโ",FSIZE("DOC_NAME"),"ฤ")+"ฤ"+ ;
        PADC("‘ใฌฌ ",FSIZE("TOTAL_R_S"),"ฤ")+"ฤ"

ln = WROWS("")-8
wd = LEN(tt(1))

DO Def_Win WITH "T_L_M", "T_L_E", "T_L_I", ln, wd, ;
                " ชซ คญ๋ฅ เฅ ซจง โฎเ ", tt

what_do = IIF(R_COUNT()#0, "t_list", "add_t")

DO WHILE .T.

  DO CASE
  CASE what_do = "t_list"
    DO Prp_Menu WITH "T_List"
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Del DO Ch_Mode WITH "T_List","del_t"
    ON KEY LABEL Ins DO Ch_Mode WITH "T_List","add_t"
    ON KEY LABEL F10 ACTIVATE MENU T_List
    ACTIVATE WINDOW T_L_E
    IF c_vl = "$"
      BROWSE FIELDS f001 = LEFT(DOC_NUM,4)+"-"+ALLTRIM(RIGHT(DOC_NUM,6)):11:H="", ;
                    DOC_DATE:H="":P="@D ",  ;
                    DOC_NAME:H="",          ;
                    TOTAL_D_S,              ;
                    DUMMY.F:H=""  FREEZE DUMMY.F   ;
                    NOMODIFY NOMENU NOCLEAR WINDOW T_L_I IN WINDOW T_L_E
    ELSE
      BROWSE FIELDS f001 = LEFT(DOC_NUM,4)+"-"+ALLTRIM(RIGHT(DOC_NUM,6)):11:H="", ;
                    DOC_DATE:H="":P="@D ",  ;
                    DOC_NAME:H="",          ;
                    TOTAL_R_S,              ;
                    DUMMY.F:H=""  FREEZE DUMMY.F   ;
                    NOMODIFY NOMENU NOCLEAR WINDOW T_L_I IN WINDOW T_L_E
    ENDIF
    ON KEY
    HIDE MENU T_List
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    IF what_do = "t_list"
      IF READKEY() % 256 = 12
        what_do = "exit"
      ELSE
        what_do = "d_list"
      ENDIF
    ENDIF
  CASE what_do = "d_list"
    DO D_List
    what_do = "t_list"
  CASE what_do = "add_t"
    DO Add_T
    what_do = IIF(R_COUNT() = 0, "exit", "t_list")
  CASE what_do = "del_t"
    DO Del_T
    what_do = IIF(R_COUNT() = 0, "exit", "t_list")
  OTHERWISE
    EXIT
  ENDCASE

ENDDO

RELEASE WINDOW T_L_M
RELEASE WINDOW T_L_E
RELEASE WINDOW T_L_I
DEACTIVATE MENU T_List
RELEASE MENU T_List EXTENDED

*
*     ซฅฅ ขแฅ ฏฎค็จแโจโ์, เ งฎกเ โ์แ๏ แ โฅฌ, ็โฎ ฎแโ ซฎแ์ ข ็ แโจ แใฌฌ
*       จ ขฎฎก้ฅ ญ ซจ็จ๏ แฏจแช .
*
* ฅเฅแ็จโ โ์ แใฌฌ๋, ใค ซจโ์ TAG, ง ชเ๋โ์ จ ใค ซจโ์ R_STOCK(DBF,CDX)
*
SELECT RYN_L_T
USE
SELECT R_STOCK
USE
DELETE FILE (tmpo_path+"R_STOCK.DBF")
DELETE FILE (tmpo_path+"R_STOCK.CDX")
SELECT RRD_TMP
SET ORDER TO TAG DOC_NUM
DELETE TAG CODE
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 ฎก ขซฅญจฅ ญ ชซ คญฎฉ เฅ ซจง โฎเ .                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Add_T
PRIVATE d_num, d_nam, ex, rc_sav, w, ln, wd

IF R_COUNT() = 0
  rc_sav = 0
  d_num = "0000     1"
ELSE
  rc_sav = RECNO()
  GO BOTTOM
  d_num = DOC_NUM
  w = VAL(ALLTRIM(SUBSTR(d_num, 5)))+1
  d_num = LEFT(d_num,4)+STR(w,6)
ENDIF

DO D_W WITH "Add_T", 8, 42, "‘ฎงค ญจฅ ญ ชซ คญฎฉ", 0

d_nam = SPACE(16)
ex = 1

@ 0, 0 SAY ""
? " ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
? " ณ                                      ณ"
? " ณ       ชซ คญ ๏ ....:....!....:.      ณ"
? " ณ                                      ณ"
? " ณ   < OK Ctrl-W > < โช ง โ์แ๏ Esc >   ณ"
? " ณ                                      ณ"
? " ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"

@ 3, 8 SAY " ชซ คญ ๏" GET d_nam
@ 5, 5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
READ CYCLE
RELEASE WINDOW Add_T

IF ex = 1
  APPEND BLANK
  REPLACE DOC_NUM  WITH d_num,  ;
          DOC_DATE WITH DATE(), ;
          DOC_NAME WITH d_nam
ELSE
  IF rc_sav # 0
    GO rc_sav
  ENDIF
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    ฎคแ็ฅโ ง ฏจแฅฉ ข ก งฅ ค ญญ๋ๅ.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE R_COUNT
PRIVATE rcs, j

rcs = RECNO()
COUNT TO j
IF BETWEEN(rcs, 1, RECCOUNT())
  GO rcs
ENDIF
RETURN j

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                       ฎเเฅชๆจ๏ แฏจแช  ญ ชซ คญฎฉ.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE D_List
PRIVATE k, sr, sd, p1, p2, what_do, ln, wd, tt, tit, l_price

*
*    ”ฎเฌจเใฅฌ ง ฃฎโฎขชใ จง ญฎฌฅญชซ โใเ๋ เฅ ซจง โฎเ 
*

SELECT R_STOCK
k = LEFT(DTOS(RRT_TMP.DOC_DATE),4)+RRT_TMP.DOC_NUM
SCAN
  IF SEEK(k+STR(CODE,7),"RRD_TMP")
    REPLACE QNT WITH RRD_TMP.QNT
  ELSE
    REPLACE QNT WITH 0
  ENDIF
ENDSCAN

DIMENSION tt(1)

tt(1) = PADC(" จฌฅญฎข ญจฅฤฤฤฤฤฤฤฤฤ",40,"ฤ")+"ฤ"+  ;
        PADC("ฤ ซจ็.",6,"ฤ")+"ฤ"+                ;
        PADC("ฤฤ–ฅญ ",10,"ฤ")+"ฤ"+                  ;
        PADC("ฤโ็ฅโ",6,"ฤ")

ln  = WROWS("")-8
wd  = LEN(tt(1))
tit = " ชซ คญ ๏ N "+LEFT(RRT_TMP.DOC_NUM,4)+"-"+   ;
                     ALLTRIM(RIGHT(RRT_TMP.DOC_NUM,6))

DEFINE MENU D_List IN SCREEN COLOR SCHEME 4
DEFINE PAD Seek OF D_List PROMPT "ฎจแช" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF D_List DO Seek_1
DEFINE PAD L_Price OF D_List PROMPT "–ฅญ๋" KEY Ctrl-A, "F3"
ON SELECTION PAD L_Price OF D_List DO Sw_Price
DEFINE PAD Exit OF D_List PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF D_List DO Ch_Mode WITH "Int_List","exit"

DO Def_Win WITH "D_L_M", "D_L_E", "D_L_I", ln, wd, tit, tt
PUSH KEY CLEAR

what_do = "list"
l_price = .F.
DO WHILE .T.

SELECT R_STOCK
DO CASE
CASE what_do = "list"
    DO Prp_Menu WITH "D_List"
    ON KEY LABEL F10 ACTIVATE MENU D_List
    ON KEY LABEL F3 DO Sw_Price
    ON KEY LABEL F7 DO Seek_1
    ACTIVATE WINDOW D_L_E
    IF c_vl = "$"
      BROWSE FIELDS PREFIX:W=.F.:H="",  ;
                    NAME:W=.F.:H="",    ;
                    QNT_S:W=.F.:H="":P="@Z 999999",   ;
                    PRICE_D:H="":W=l_price:P="@Z 99999.9999", ;
                    QNT:H="":P="@Z 999999":V=R_Qnt()  ;
                    NOMENU NOCLEAR WINDOW D_L_I IN WINDOW D_L_E
    ELSE
      BROWSE FIELDS PREFIX:W=.F.:H="",  ;
                    NAME:W=.F.:H="",    ;
                    QNT_S:H="":W=.F.:P="@Z 999999",   ;
                    PRICE_R:W=l_price:H="":P="@Z 9999999.99", ;
                    QNT:H="":V=R_Qnt():P="@Z 999999"  ;
                    NOMENU NOCLEAR WINDOW D_L_I IN WINDOW D_L_E
    ENDIF
    ON KEY
    HIDE MENU D_List
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    IF what_do = "list"
      what_do = "exit"
    ENDIF
  OTHERWISE
    EXIT
  ENDCASE

ENDDO
POP KEY

RELEASE WINDOW D_L_M
RELEASE WINDOW D_L_E
RELEASE WINDOW D_L_I
DEACTIVATE MENU D_List
RELEASE MENU D_List EXTENDED


*
*    โ ซชจข ฅฌ ฎกเ โญฎ...
*

STORE 0 TO sr, sd
SELECT RRD_TMP
SET ORDER TO TAG DOC_NUM
SEEK RRT_TMP.FIRM+k
DELETE REST WHILE RRT_TMP.FIRM+k = RRD_TMP.FIRM+LEFT(DTOS(RRD_TMP.DOC_DATE),4)+RRD_TMP.DOC_NUM
SELECT R_STOCK
SCAN FOR QNT # 0
  SELECT RRD_TMP
  IF c_vl = "$"
    p2 = R_STOCK.PRICE_D
    p1 = Clc_Rub(p2)
  ELSE
    p1 = R_STOCK.PRICE_R
    p2 = 0
  ENDIF
  
  APPEND BLANK
  REPLACE DOC_NUM   WITH RRT_TMP.DOC_NUM,  ;
          DOC_DATE  WITH RRT_TMP.DOC_DATE, ;
          CODE      WITH R_STOCK.CODE,     ;
          QNT       WITH R_STOCK.QNT,      ;
          PRICE_R_F WITH 0,                ;
          PRICE_D_F WITH 0,                ;
          PRICE_R_S WITH p1,               ;
          PRICE_D_S WITH p2
  sr = sr+QNT*PRICE_R_S
  sd = sd+QNT*PRICE_D_S
  SKIP
  SELECT R_STOCK
ENDSCAN
SELECT RRD_TMP
SET ORDER TO TAG CODE

SELECT RRT_TMP
REPLACE TOTAL_R_F WITH 0,  ;
        TOTAL_D_F WITH 0,  ;
        TOTAL_R_S WITH sr, ;
        TOTAL_D_S WITH sd, ;
        VALUTA    WITH c_vl

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                       ฎจแช ฏฎ ญ ็ ซ์ญ๋ฌ กใชข ฌ.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Seek_1
PRIVATE nn, ln, wd, ex, rc, mss

DEFINE WINDOW H_Win FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW H_Win
ln = 8
wd = 40
DO D_W WITH "Seek_1", ln, wd, " ค ฉโฅ ญ ็ ซ์ญ๋ฅ กใชข๋ คซ๏ ฏฎจแช ", 0
@ 0, 0 SAY ""
? " ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
? " ณ                                    ณ"
? " ณ      ....:....1....:....2....      ณ"
? " ณ                                    ณ"
? " ณ  < OK Ctrl-W > < โช ง โ์แ๏ Esc >  ณ"
? " ณ                                    ณ"
? " ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"

PUSH KEY CLEAR
nn = SPACE(24)
ex = 1

@ 3, 8 GET nn
@ 5, 4 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "
READ CYCLE

IF ex = 1
  rc = RECNO()
  IF .NOT. SEEK(ALLTRIM(nn))
    IF BETWEEN(rc, 1, RECCOUNT())
      GO rc
    ENDIF
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"ฅโ โ ชจๅ ฏฎงจๆจฉ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
ENDIF

POP KEY
RELEASE WINDOW H_Win
RELEASE WINDOW Seek_1
DEACTIVATE MENU D_List

RETURN
*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  งเฅ่จโ์/ง ฏเฅโจโ์ ชฎเเฅชๆจ๎ ๆฅญ.                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sw_Price

l_price = .NOT. l_price
DEACTIVATE MENU D_List

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎขฅเช  ชฎเเฅชโญฎแโจ ชฎซจ็ฅแโข .                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE R_Qnt

IF QNT > QNT_S .AND. QNT > 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"    เฅ ซจง โฎเฎฌ แโฎซ์ชฎ ญฅ ็จแซจโแ๏!"
  mss(3) = ""
  DO Out_Mess WITH 7,"mss"
  RETURN .F.
ENDIF

IF .F. &&&&&&&&&&&&&&&&& QNT < 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ๋ ฏฎชใฏ ฅฌ? ฅโ, ญฅโ จ ญฅโ!"
  mss(3) = ""
  DO Out_Mess WITH 7,"mss"
  RETURN .F.
ENDIF

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                          “ค ซฅญจฅ ญ ชซ คญฎฉ.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Del_T
PRIVATE k

k = LEFT(DTOS(DOC_DATE),4)+DOC_NUM

SELECT RRD_TMP
SEEK k
DELETE REST WHILE k = LEFT(DTOS(DOC_DATE),4)+DOC_NUM
SELECT RRT_TMP
DELETE

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ       เฎฃเ ฌฌ  ฏฎคฃฎโฎขชจ, ฏเฎแฌฎโเ  จ ชฎเเฅชๆจจ คขใๅ แฏจแชฎข.       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Prp_L2

PRIVATE l_t, lst1, lst2

USE STOCK ORDER TAG CODE
SELECT 0
USE (tmp_path+t_nam+".DBF") EXCLUSIVE ORDER TAG DOC_NUM ALIAS RRT_TMP
SELECT 0
USE (tmp_path+d_nam+".DBF") EXCLUSIVE ORDER TAG DOC_NUM ALIAS RRD_TMP
SELECT 0
DELETE FILE (tmpo_path+"LIST_2.DBF")
DELETE FILE (tmpo_path+"LIST_2.CDX")
CREATE DBF (tmpo_path+"LIST_2")  ;
       ( L_TYPE    C( 1),   ;
         CODE      N( 7),   ;
         QNT       N( 6),   ;
         PREFIX    C( 4),   ;
         NAME      C(35),   ;
         PRICE_R_L N(12,5), ;
         PRICE_D_L N(12,4), ;
         PRICE_R_F N(12,5), ;
         PRICE_D_F N(12,4), ;
         PRICE_R_S N(12,5), ;
         PRICE_D_S N(12,4)  )
USE (tmpo_path+"LIST_2") EXCLUSIVE ALIAS LIST_2

INDEX ON L_TYPE+NAME+PREFIX TAG NAME
INDEX ON L_TYPE+STR(CODE,7) TAG CODE ADDITIVE

SET ORDER TO TAG CODE

SELECT RRD_TMP
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO RRT_TMP, ;
                CODE INTO STOCK

lst1 = .F.
lst2 = .F.

SCAN
  l_t = IIF(EMPTY(RRT_TMP.LST_TYPE), "1", "2")
  IF l_t = "1"
    lst1 = .T.
  ELSE
    lst2 = .T.
  ENDIF
  SELECT LIST_2
  IF .NOT. SEEK(l_t+STR(RRD_TMP.CODE,7),"LIST_2")
    APPEND BLANK
  ENDIF
  REPLACE L_TYPE    WITH l_t,   ;
          CODE      WITH RRD_TMP.CODE,  ;
          QNT       WITH QNT+RRD_TMP.QNT,   ;
          PREFIX    WITH STOCK.PREFIX,  ;
          NAME      WITH STOCK.NAME,    ;
          PRICE_R_L WITH STOCK.PRICE_R, ;
          PRICE_D_L WITH STOCK.PRICE,   ;
          PRICE_R_F WITH RRD_TMP.PRICE_R_F, ;
          PRICE_D_F WITH RRD_TMP.PRICE_D_F, ;
          PRICE_R_S WITH RRD_TMP.PRICE_R_S, ;
          PRICE_D_S WITH RRD_TMP.PRICE_D_S
  SELECT RRD_TMP
ENDSCAN
SET RELATION TO

IF .NOT. lst1 .AND. .NOT. lst2
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"โ็ฅโ ฏใแโ!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"LIST_2.DBF")
  DELETE FILE (tmpo_path+"LIST_2.CDX")
  DELETE FILE (tmp_path+t_nam+".DBF")
  DELETE FILE (tmp_path+t_nam+".CDX")
  DELETE FILE (tmp_path+d_nam+".DBF")
  DELETE FILE (tmp_path+d_nam+".CDX")
  RETURN
ENDIF
IF View_2()
  DO Sale_2    && เฎค ฆ 
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ       เฎฃเ ฌฌ , ฏเฎแฌฎโเ  จ ชฎเเฅชๆจจ ฎคญฎฃฎ จง คขใๅ แฏจแชฎข.        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Lst_2
PARAMETERS k

PRIVATE ln, wd, tt, flt_vl, what_do, pr, pd, pq

SELECT LIST_2
IF c_vl = "$"
  SCAN ALL
    REPLACE PRICE_R_F WITH Clc_Rub(PRICE_D_F),  ;
            PRICE_R_S WITH Clc_Rub(PRICE_D_S)
  ENDSCAN
ENDIF
pr = "@Z 9999999.99"
pd = "@Z 99999.9999"
pq = "@Z 999999"

flt_vl = c_vl
DIMENSION tt(1)
tt(1) = ;
"ฤฤฤ จฌฅญฎข ญจฅฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฎซ-ขฎฤฤฤฤฤเ ฉแฤฤฤฤ–ฅญ  ‘ช.ฤฤฤฤ–ฅญ  .ฤ"

ln = WROWS("")-8
wd = LEN(tt(1))
DO Def_Win WITH "W0_M", "W0_E", "W0_I", ln, wd, "‘ฏจแฎช "+k, tt
what_do = "list"
SEEK k
DEFINE MENU Lst_2 IN SCREEN COLOR SCHEME 4
IF c_vl = "$"
  DEFINE PAD Valuta OF Lst_2 PROMPT " ซ๎โ " KEY Ctrl-A, "F4"
  ON SELECTION PAD Valuta OF Lst_2 DO Ch_Mode WITH "Lst_2","valuta"
ENDIF
DEFINE PAD Div OF Lst_2 PROMPT "ฅญ์ฃจ" KEY Ctrl-A, "F5"
ON SELECTION PAD Div OF Lst_2 DO Show_Sum
DEFINE PAD Exit OF Lst_2 PROMPT "๋ๅฎค" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF Lst_2 DO Ch_Mode WITH "Lst_2","exit"

DO WHILE .T.

  DO CASE
  CASE what_do = "list"
    DO Prp_Menu WITH "Lst_2"
    ON KEY LABEL F10 ACTIVATE MENU Lst_2
    IF c_vl = "$"
      ON KEY LABEL F4 DO Ch_Mode WITH "Lst_2","valuta"
    ENDIF
    ON KEY LABEL F5 DO Show_Sum
    ACTIVATE WINDOW W0_E
    IF flt_vl = "$"
      BROWSE FIELDS PREFIX:H="":W=.F.,            ;
                    Brw_Name=Nm_Part(NAME):25:H="":W=.F., ;
                    QNT:H="":P=pq:W=.F.,          ;
                    STOCK.PRICE:H="":P=pd:W=.F.,  ;
                    PRICE_D_S:H="":P=pd:V=Cl_P(), ;
                    PRICE_D_F:H="":P=pd:V=Cl_P(), ;
                    f0001 = IIF(PRICE_D_F = PRICE_D_S, " ", "๛"):H="":W=.F. ;
                    KEY k                         ;
                    NOMENU NOCLEAR NOAPPEND NODELETE  ;
                    WINDOW W0_I IN WINDOW W0_E
    ELSE
      BROWSE FIELDS PREFIX:H="":W=.F.,            ;
                    Brw_Name=Nm_Part(NAME):25:H="":W=.F., ;
                    QNT:H="":P=pq:W=.F.,          ;
                    STOCK.PRICE_R:H="":P=pr:W=.F.,;
                    PRICE_R_S:H="":P=pr:V=Cl_P0(), ;
                    PRICE_R_F:H="":P=pr:V=Cl_P0(), ;
                    f0001 = IIF(PRICE_R_F = PRICE_R_S, " ", "๛"):H="":W=.F. ;
                    KEY k                         ;
                    NOMENU NOCLEAR NOAPPEND NODELETE  ;
                    WINDOW W0_I IN WINDOW W0_E
    ENDIF
    ON KEY
    IF what_do = "list"
      what_do = "exit"
    ENDIF
  CASE what_do = "valuta"
    flt_vl = IIF(flt_vl="$", "", "$")
    what_do = "list"
  OTHERWISE
    EXIT
  ENDCASE

ENDDO

RELEASE WINDOW W0_M
RELEASE WINDOW W0_E
RELEASE WINDOW W0_I
DEACTIVATE MENU Lst_2
RELEASE MENU Lst_2 EXTENDED
ln = WOUTPUT()
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
IF .NOT. EMPTY(ln)
  ACTIVATE WINDOW (ln)
ENDIF

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ฏฅเฅแ็ฅโ  เใกซฅข๋ๅ ๆฅญ.                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cl_P

REPLACE PRICE_R_F WITH Clc_Rub(PRICE_D_F),  ;
        PRICE_R_S WITH Clc_Rub(PRICE_D_S)
RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 เฎฃเ ฌฌ  ฏฅเฅแ็ฅโ  คฎซซ เฎข๋ๅ ๆฅญ.                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cl_P0

REPLACE PRICE_D_F WITH PRICE_R_F/kurs,  ;
        PRICE_D_S WITH PRICE_R_S/kurs

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 เฎฃเ ฌฌ  ข๋ค ็จ แใฌฌ ฏฎ แฏจแชใ.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Show_Sum

PRIVATE rc_sav, ss, sr, pic, crsr

rc_sav = RECNO()

ss = 0
sr = 0

SEEK k

IF flt_vl = "$"
  SUM REST WHILE L_TYPE = k QNT*PRICE_D_S, QNT*PRICE_D_F TO ss, sr
  pic = "@Z 9999999.9999"
ELSE
  SUM REST WHILE L_TYPE = k ROUND(QNT*PRICE_R_S,2), ROUND(QNT*PRICE_R_F,2) TO ss, sr
  pic = "@Z 999999999.99"
ENDIF
GO rc_sav

DEFINE WINDOW H_W FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
SHOW WINDOW H_W

DO D_W WITH "Show_Sum", 7, 35, "‘ใฌฌ  ฏฎ แฏจแชใ "+k, 0

@ 0, 0 SAY ""
*         ‘ใฌฌ  ฏฎ แฏจแชใ N
? " ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤยฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
? " ณฎ ๆฅญ ฌ แชซ ค ณฎ ๆฅญ ฌ เฅ ซ. ณ"
? " รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤลฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด"
? " ณ 999999999.99  ณ 999999999.99  ณ"
? " ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤมฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"
? "        ฆฌจโฅ ซ๎กใ๎ ชซ ขจ่ใ"

@ 4, 3 SAY ss PICTURE pic
@ 4,19 SAY sr PICTURE pic
crsr = SET("CURSOR") = "ON"
SET CURSOR OFF
= INKEY(0,"M")
IF crsr
  SET CURSOR ON
ENDIF

RELEASE WINDOW H_W
RELEASE WINDOW Show_Sum

DEACTIVATE MENU Lst_2

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                            เฎค ฆ ...                                 บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Sale_2
PRIVATE ss, sr, sw_price, p_nkl, s_money, p_money, inc_d, inc_t
PRIVATE n_sal, d_sal, n_rep, d_rep, s0, sum_nal

SELECT RRT_TMP
USE

SELECT RRD_TMP

USE (base_path+"RYN_L") ORDER TAG C_CODE

SELECT 0
USE (base_path+"SALE")

SELECT 0
USE (base_path+"SALE_TIT")

SELECT 0
USE (base_path+"ACT_S") ORDER TAG SALE

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ARM_LST")
GO arm_rec

DELETE FILE (tmpo_path+"R_R_ERR.DBF")
DELETE FILE (tmpo_path+"R_R_ERR.CDX")
  
IF lst1 .OR. lst2
  SELECT 0

  CREATE DBF (tmpo_path+"R_R_ERR.DBF")  ;
    ( CODE     N( 7),  ;
      PREFIX   C( 4),  ;
      NAME     C(35),  ;
      PRODUCER C( 8),  ;
      QNT_REQ  N( 6),  ;
      QNT_SAL  N( 6)  )
  USE (tmpo_path+"R_R_ERR.DBF") ALIAS TMP_ERR
ENDIF

SELECT LIST_2

STORE 0 TO n_sal, d_sal, n_rep, d_rep
*
*    เฎค ฅฌ ฏฅเข๋ฉ แฏจแฎช
*
s0 = 0
inc_d = ""
inc_t = ""
IF lst1
  sw_price = 1
  p_nkl    = .F.
  s_money  = .T.
  p_money  = .T.

  SEEK "1"

  IF c_vl = "$"
    SUM REST WHILE L_TYPE = "1" ROUND(QNT*Clc_Rub(PRICE_D_S),2), ROUND(QNT*Clc_Rub(PRICE_D_F),2) TO ss, sr
  ELSE
    SUM REST WHILE L_TYPE = "1" ROUND(QNT*PRICE_R_S,2), ROUND(QNT*PRICE_R_F,2) TO ss, sr
  ENDIF
  sum_nal = 0
  
  IF .NOT. SEEK(STR(c_code)+f_cb, "CUS_REST")
    SELECT CUS_REST
    APPEND BLANK
    REPLACE CUS_CODE WITH c_code, FIRM WITH f_cb
    SELECT LIST_2
  ENDIF
  DO Get_Mode2 WITH 1
  PRIVATE numbs, dates, i00
  DIMENSION numbs(1)
  numbs(1) = ""
  DIMENSION dates(1)
  IF Make_Inv(1, is_check1)
*  IF s_money
*    DO Send_M
*  ENDIF
    IF p_nkl
      DO Prn_Inv WITH f_cb, n_sal, d_sal
    ENDIF
    IF sum_nal # 0
      FOR j00 = 1 TO ALEN(numbs)
        DO BigOrd WITH f_cb, numbs(j00), dates(j00)
      ENDFOR
      DO ViewBig WITH max_1
      SELECT BIGORD
      SCAN
        dates(RECNO()) = DOC_DATE
      ENDSCAN
      USE
      DELETE FILE (tmpo_path+"BIGORD.DBF")
      SELECT BIGDOC
      USE
      SELECT CUS_SUM
      USE
      IF p_nkl
        FOR i00 = 1 TO ALEN(numbs)
          DO P_Money WITH f_cb, IIF(is_check1, "—", " "), numbs(i00), dates(i00)  && inc_t
        ENDFOR
      ENDIF
    ENDIF
*  IF p_money
*    DO P_Rep WITH n_rep, d_rep
*  ENDIF

  ELSE
    lst2 = .F.
  ENDIF
ENDIF

SELECT LIST_2

STORE 0 TO n_sal, d_sal, n_rep, d_rep
*
*    เฎค ฅฌ ขโฎเฎฉ แฏจแฎช
*
s0 = 0
IF lst2
  sw_price = 1
  p_nkl    = .F.

  SEEK "2"

  IF c_vl = "$"
    SUM REST WHILE L_TYPE = "2" ROUND(QNT*Clc_Rub(PRICE_D_S),2), ROUND(QNT*Clc_Rub(PRICE_D_F),2) TO ss, sr
  ELSE
    SUM REST WHILE L_TYPE = "2" ROUND(QNT*PRICE_R_S,2), ROUND(QNT*PRICE_R_F,2) TO ss, sr
  ENDIF
  sum_nal = 0
  IF .NOT. SEEK(STR(c_code)+f_cw, "CUS_REST")
    SELECT CUS_REST
    APPEND BLANK
    REPLACE CUS_CODE WITH c_code, FIRM WITH f_cw
    SELECT LIST_2
  ENDIF
  DO Get_Mode2 WITH 2
  PRIVATE numbs, dates, i00
  DIMENSION numbs(1)
  numbs(1) = ""
  DIMENSION dates(1)
  IF Make_Inv(2, is_check2)
    IF p_nkl
      DO Prn_Inv WITH f_cw, n_sal, d_sal
    ENDIF
    IF sum_nal # 0
      FOR j00 = 1 TO ALEN(numbs)
        DO BigOrd WITH f_cw, numbs(j00), dates(j00)
      ENDFOR
      DO ViewBig WITH max_2
      SELECT BIGORD
      SCAN
        dates(RECNO()) = DOC_DATE
      ENDSCAN
      USE
      DELETE FILE (tmpo_path+"BIGORD.DBF")
      SELECT BIGDOC
      USE
      SELECT CUS_SUM
      USE
      IF p_nkl
        FOR i00 = 1 TO ALEN(numbs)
          DO P_Money WITH f_cw, IIF(is_check2, "—", " "), numbs(i00), dates(i00) && inc_t
        ENDFOR
      ENDIF
    ENDIF
  ENDIF
ENDIF

IF File_O(tmpo_path+"R_R_ERR.DBF")
  DO Use_Dummy
  SELECT TMP_ERR
  IF RECCOUNT() # 0
    DO View_Err
  ENDIF
ENDIF

CLOSE DATABASES
DELETE FILE (tmpo_path+"R_R_ERR.DBF")
DELETE FILE (tmpo_path+"R_R_ERR.CDX")
DELETE FILE (tmp_path+t_nam+".DBF")
DELETE FILE (tmp_path+d_nam+".DBF")
DELETE FILE (tmp_path+t_nam+".CDX")
DELETE FILE (tmp_path+d_nam+".CDX")
DELETE FILE (tmpo_path+"LIST_2.DBF")
DELETE FILE (tmpo_path+"LIST_2.CDX")

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  แฎงค ญจ๏ ญ ชซ คญฎฉ.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Make_Inv
PARAMETERS l_t, is_check     && ’จฏ แฏจแช 
PRIVATE k1, k2, qr, qs, t_sal, p_d, p_r, new_d, rest_sal, mss
PRIVATE s_w0, s_w, s_w_d, q_saled, key_act, chk_date, chk_time, chk_err
PRIVATE firm_w, tmpNalog
PRIVATE to_kassa
PRIVATE tmpNum, tmpType
PRIVATE mss_sum
mss_sum = 0
firm_w = IIF(l_t = 1, f_cb, f_cw)

k1 = IIF(l_t = 1, "1", "2")
k2 = STR(c_code,5)
s0 = IIF(sw_price = 1, ss, sr)

inc_d = ""
inc_t = ""
chk_date = DATE()
chk_time = SECONDS()
chk_err  = 0

IF sum_nal # 0
  tmpType = IIF(is_check, "—", " ")
  IF .NOT. LetMoney(firm_w, tmpType)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+" ซจ็ญ๋ฅ คฅญ์ฃจ ง ฏเฅ้ฅญ๋ คซ๏ ค ญญฎฃฎ ฏเฅคฏเจ๏โจ๏!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  PRIVATE max_ord, rst, rst_0, rst_n, rst_d, ord_cnt, sgn
  PRIVATE doc_w, sums, sum_w, j00

  SELECT 0
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9B23 AGAIN
  SEEK firm_w
  max_ord = IIF(FSIZE("M_SUM")=0 .OR. is_check, 0, M_SUM)
  IF l_t = 1
    max_1 = max_ord
  ELSE
    max_2 = max_ord
  ENDIF
  USE
  sgn = SIGN(sum_nal)
  rst   = ABS(sum_nal)
  rst_0 = ROUND(100*rst/(nds_flt+100),2)
  rst_n = ROUND(G_Nalog(firm_w)*rst/100,2)
  rst_d = ROUND(rst/kurs,4)
  ord_cnt  = 0
  IF rst < max_ord .OR. max_ord = 0 .OR. is_check
    DIMENSION numbs(1), sums(1,4), dates(1)
    ord_cnt  = 1
    numbs(1) = 0
    sums(1,1) = rst
    sums(1,2) = rst_0
    sums(1,3) = rst_n
    sums(1,4) = rst_d
    dates(1)  = DATE()
  ELSE
    DO WHILE rst > 0
      IF rst < max_ord
        sum_w = rst
      ELSE
        sum_w = max_ord-ROUND(0.1*RAND()*max_ord, 0)
      ENDIF
      ord_cnt = ord_cnt+1
      rst = rst-sum_w
      DIMENSION numbs(ord_cnt), sums(ord_cnt,4), dates(ord_cnt)
      numbs(ord_cnt) = 0
      sums(ord_cnt,1)  = sum_w
      dates(ord_cnt) = DATE()+ord_cnt-1
      IF rst # 0
        sums(ord_cnt,2)  = ROUND(sum_w/(100+nds_flt)*100, 2)
        rst_0 = rst_0-sums(ord_cnt,2)
        sums(ord_cnt,3)  = ROUND(sum_w*G_Nalog(firm_w)/100, 2)
        rst_n = rst_n-sums(ord_cnt,3)
        sums(ord_cnt,4)  = ROUND(sum_w/kurs,4)
        rst_d = rst_d-sums(ord_cnt,4)
      ELSE
        sums(ord_cnt,2)  = rst_0
        sums(ord_cnt,3)  = rst_n
        sums(ord_cnt,4)  = rst_d
      ENDIF
    ENDDO
  ENDIF

  IF is_check
    PRIVATE is_kkm
    is_kkm = FirmKKM(firm_w)
    IF EMPTY(is_kkm)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"  ค ญญฎฌ ฏเฅคฏเจ๏โจจ ญฅโ ช แแฎขฎฃฎ  ฏฏ เ โ !"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      RETURN .F.
    ENDIF
    is_kkm = VAL(ALLTRIM(is_kkm))
    IF File_O(base_path+"TO_KKM.DBF") .AND. is_kkm # 0
      DO Do_Check WITH sums(1,2), sums(1,1)-sums(1,2), sums(1,3), inc_d, chk_date, chk_time, chk_err
      IF .NOT. EMPTY(chk_err)
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"่จกช  "+ALLTRIM(STR(chk_err))+" ฏเจ ฏฎฏ๋โชฅ ฏเฎกจโ์ ็ฅช"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        IF USED("C_9122")
          USE IN C_9122
        ENDIF
        RETURN .F.
      ENDIF
    ELSE
      inc_d = Doc_Num("CHECK", firm_w, arm, DATE())
      inc_d = sys_char+STR(inc_d,9)
    ENDIF
  ENDIF

  SELECT 0
  USE (base_path+"INCMONEY") ALIAS M_9122 AGAIN

  to_kassa = 0
  FOR j00 = 1 TO ord_cnt
    IF is_check
      doc_w = inc_d
    ELSE
      IF sgn < 0
        doc_w = Doc_Num("EXP_ORDER", firm_w, arm, DATE()+j00-1)
        doc_w = sys_char+STR(-doc_w,9)
      ELSE
        doc_w = Doc_Num("INC_ORDER", firm_w, arm, DATE()+j00-1)
        doc_w = sys_char+STR(doc_w,9)
      ENDIF
    ENDIF
    numbs(j00) = doc_w
    APPEND BLANK
    tmpNalog = G_Nalog(firm_w)
    REPLACE FIRM      WITH firm_w,    ;
            DOC_NUM   WITH numbs(j00),;
            DOC_TYPE  WITH IIF(is_check, "—", ""), ;
            DOC_DATE  WITH DATE()+j00-1,;
            ENTER_DAT WITH DATE()+j00-1,;
            DOC_TIME  WITH SECONDS(), ;
            CUS_CODE  WITH c_code,    ;
            CUS_NAME  WITH c_name,    ;
            MONEY_0   WITH sgn*sums(j00,2), ;
            NDS_      WITH nds_flt    ;
            NL_PRC    WITH tmpNalog,  ;
            N_OF_SALE WITH sgn*sums(j00,3),;
            MONEY_R   WITH sgn*sums(j00,1),;
            MONEY_D   WITH sgn*sums(j00,4),;
            WHO       WITH user,      ;
            WHERE     WITH arm
    IF DOC_TYPE = "—"
      mss_sum = MONEY_R
    ENDIF
    IF DOC_TYPE = "—" .AND. Is_Ur(CUS_CODE) .AND. YEAR(DOC_DATE) < 2006
      IF MONEY_R > 0 .OR. MONEY_D > 0
        tmpNum = Doc_Num("INC_ORDER", FIRM, arm, LEFT(DTOS(DATE()),4))
      ELSE
        tmpNum = Doc_Num("EXP_ORDER", FIRM, arm, LEFT(DTOS(DATE()),4))
        tmpNum = -tmpNum
      ENDIF
      REPLACE ENTER_DOC WITH CHR(0)+sys_char+STR(tmpNum,9)
    ENDIF
    inc_d = numbs(j00)
    to_kassa = to_kassa+MONEY_R+N_OF_SALE
  ENDFOR
  SELECT 0
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9209 AGAIN
  SEEK firm_w
  tmpNalog = G_Nalog(firm_w)
  REPLACE NALOG WITH ROUND(tmpNalog*sum_nal/100,2)
  USE
  DO Send_M
ENDIF

*IF is_check
*  new_d = ""
*ELSE
  new_d = Doc_Num("BILL_F", firm_w, arm, DATE())
  new_d = sys_char+STR(new_d,9)
*ENDIF

d_sal = DATE()
t_sal = SECONDS()
*IF is_check
*  n_sal = Doc_Num("G_CHECK", IIF(l_t = 1, f_cb, f_cw), arm, d_sal)
*  n_sal = sys_char+"***"+STR(n_sal,6)
*ELSE
  n_sal = Doc_Num("SALE", IIF(l_t = 1, f_cb, f_cw), arm, d_sal)
  n_sal = sys_char+CHRTRAN(arm," ","0")+STR(n_sal,6)
*ENDIF
IF sum_nal # 0
  SELECT M_9122
  SET ORDER TO DOC_NUM
  FOR j00 = 1 TO ord_cnt
    SEEK firm_w+IIF(is_check, "—", " ")+LEFT(DTOS(DATE()+j00-1), 4)+numbs(j00)
    REPLACE SALE_NUM  WITH n_sal, ;
            SALE_DATE WITH d_sal
  ENDFOR
  USE
ENDIF

SELECT LIST_2
SEEK k1
s_w = 0
s_w_0 = 0
s_w_d = 0
SCAN REST WHILE L_TYPE = k1
  key_act = STR(CODE,7)+firm_w
  rest_sal = LIST_2.QNT
  IF .NOT. SEEK(CODE, "STOCK")
    SELECT LIST_2
    LOOP
  ENDIF
  IF rest_sal < 0    && ฎงขเ โ
  
  *
  *    ‘ฎงค ฅฌ (ฅแซจ ญ คฎ) ฏ เโจ๎ กฅง ฏเจๅฎค  จ ชซ คฅฌ ข ญฅฅ โฎข เ
  *
    SELECT ACT_S
    SET ORDER TO TAG INC_LIST
    IF .NOT. SEEK(STR(LIST_2.CODE,7)+firm_w+"        **********")
      APPEND BLANK
      REPLACE FIRM     WITH firm_w,        ;
              DOC_NUM  WITH "**********",  ;
              INC_TYPE WITH "0",           ;
              FLG_BUY  WITH "B",           ;
              CODE     WITH LIST_2.CODE,   ;
              LAST_SALE WITH DATE(),       ;
              QNT_REST WITH -LIST_2.QNT
    ELSE
      REPLACE QNT_REST WITH QNT_REST-LIST_2.QNT, LAST_SALE WITH DATE()
    ENDIF
    SET ORDER TO TAG SALE
    *
    *   แ็ฅโ ๆฅญ จ ชฎเเฅชๆจ๏ ญ ซจ็จ๏ ข ไจซจ ซฅ
    *
    SELECT RYN_L
    IF sw_price = 1
      p_d = LIST_2.PRICE_D_S
      p_r = LIST_2.PRICE_R_S
    ELSE
      p_d = LIST_2.PRICE_D_F
      p_r = LIST_2.PRICE_R_F
    ENDIF
    IF .NOT. SEEK(k2+STR(LIST_2.CODE,7))
      APPEND BLANK
      REPLACE CUS_CODE WITH c_code,      ;
              CODE     WITH LIST_2.CODE, ;
              QNT      WITH -LIST_2.QNT, ;
              PRICE_D  WITH p_d,         ;
              PRICE_R  WITH p_r
    ELSE
      REPLACE QNT WITH QNT-LIST_2.QNT
    ENDIF
    *
    *   ... จ ฏเฎค ฅฌ!
    SELECT SALE
    APPEND BLANK
    REPLACE DOC_NUM   WITH n_sal,         ;
            FIRM      WITH firm_w,        ;
            DOC_DATE  WITH d_sal,         ;
            CODE      WITH LIST_2.CODE,   ;
            QNT       WITH LIST_2.QNT,    ;
            PRICE_D_L WITH STOCK.PRICE,   ;
            PRICE_R_L WITH STOCK.PRICE_R, ;
            PRICE_D   WITH IIF(EMPTY(p_d), p_r/kurs, p_d), ;
            PRICE_R   WITH p_r,           ;
            PRICE_0   WITH 100*p_r/(100+nds_flt), ;
            ACT_FIRM  WITH firm_w,        ;
            ACT_NUM   WITH ACT_S.DOC_NUM, ;
            ACT_DATE  WITH ACT_S.DOC_DATE,;
            PARTY     WITH ACT_S.PARTY,   ;
            GTD_NUM   WITH ACT_S.GTD_NUM, ;
            COUNTRY   WITH ACT_S.COUNTRY, ;
            COUNTRYC  WITH ACT_S.COUNTRYC,;
            GTD_LINE  WITH ACT_S.GTD_LINE,;
            WHERE     WITH arm
    s_w = s_w + ROUND(QNT*PRICE_R,2)
    s_w_0 = s_w_0 + QNT*PRICE_0
    s_w_d = s_w_d + QNT*PRICE_D
    SELECT STOCK
    REPLACE QNT_REAL WITH QNT_REAL-LIST_2.QNT
    SELECT LIST_2
    LOOP
  ENDIF
    
  q_saled = 0      && ‘็ฅโ็จช ฏเฎค ญญฎฃฎ ชฎซ-ข 
  DO WHILE rest_sal > 0
    IF .NOT. Ch_Part()
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"ญจฌ ญจฅ! "+ALLTRIM(STOCK.PREFIX)+" "+ALLTRIM(STOCK.NAME)
      mss(3) = CHR(0)+"ญฅ ฏเฎค ญ  ฏฎซญฎโ์๎. ‘ใฌฌ๋ ข ญ ชซ คญฎฉ จ / ญฅ แฎขฏ คใโ!"
      mss(4) = ""
      DO Out_Mess WITH 7, "mss"
      EXIT
    ENDIF
    qs = MIN(rest_sal, ACT_S.QNT_REST)
    qr = MIN(ACT_S.REST_REP, qs)
    qr = IIF(qr < 0, 0, qr)
    rest_sal = rest_sal-qs
    *
    *  ‘ฏจแ๋ข ฅฌ แ เฅ ซจง โฎเ 
    SELECT RYN_L
    IF .NOT. SEEK(k2+STR(LIST_2.CODE,7))
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"ญจฌ ญจฅ! "+ALLTRIM(STOCK.PREFIX)+" "+ALLTRIM(STOCK.NAME)
      mss(3) = CHR(0)+"ญฅ ฏเฎค ญ  ฏฎซญฎโ์๎.   เฅ ซจง โฎเฎฌ แโฎซ์ชฎ ญฅ ็จแซจโแ๏!"
      mss(4) = ""
      DO Out_Mess WITH 7, "mss"
      EXIT
    ELSE
      IF QNT < qs
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"ญจฌ ญจฅ! "+ALLTRIM(STOCK.PREFIX)+" "+ALLTRIM(STOCK.NAME)
        mss(3) = CHR(0)+"ญฅ ฏเฎค ญ  ฏฎซญฎโ์๎.   เฅ ซจง โฎเฎฌ แโฎซ์ชฎ ญฅ ็จแซจโแ๏!"
        mss(4) = ""
        DO Out_Mess WITH 7, "mss"
        qs = MAX(QNT, 0)
        IF qs = 0
          EXIT
        ENDIF
        rest_sal = 0
      ENDIF
    ENDIF
    REPLACE QNT WITH QNT-qs
    IF QNT = 0
      DELETE
    ENDIF
    *
    *  โเ ก โ๋ข ฅฌ ญ ซจ็จฅ ข ฏ เโจจ โฎข เ 
    SELECT ACT_S
    REPLACE QNT_REST WITH QNT_REST-qs,  ;
            REST_REP WITH REST_REP-qr,   ;
            LAST_SALE WITH DATE()
    *
    *   ฏเฅคฅซ๏ฅฌ, ฏฎ ช ชจฌ ๆฅญ ฌ ฏเฎค ฅฌ...
    IF sw_price = 1
      p_d = LIST_2.PRICE_D_S
      p_r = LIST_2.PRICE_R_S
    ELSE
      p_d = LIST_2.PRICE_D_F
      p_r = LIST_2.PRICE_R_F
    ENDIF
    *
    *   ... จ ฏเฎค ฅฌ!
    SELECT SALE
    APPEND BLANK
    REPLACE DOC_NUM   WITH n_sal,         ;
            FIRM      WITH firm_w,        ;
            DOC_DATE  WITH d_sal,         ;
            CODE      WITH LIST_2.CODE,   ;
            QNT       WITH qs,            ;
            QNT_REP   WITH qr,            ;
            PRICE_D_L WITH STOCK.PRICE,   ;
            PRICE_R_L WITH STOCK.PRICE_R, ;
            PRICE_D   WITH IIF(EMPTY(p_d), p_r/kurs, p_d), ;
            PRICE_R   WITH p_r,           ;
            PRICE_0   WITH 100*p_r/(100+nds_flt), ;
            ACT_FIRM  WITH ACT_S.FIRM,    ;
            ACT_NUM   WITH ACT_S.DOC_NUM, ;
            ACT_DATE  WITH ACT_S.DOC_DATE,;
            PARTY     WITH ACT_S.PARTY,   ;
            GTD_NUM   WITH ACT_S.GTD_NUM, ;
            COUNTRY   WITH ACT_S.COUNTRY, ;
            COUNTRYC  WITH ACT_S.COUNTRYC ;
            GTD_LINE  WITH ACT_S.GTD_LINE,;
            WHERE     WITH arm
    s_w_d = s_w_d + QNT*PRICE_D
    s_w_0 = s_w_0 + QNT*PRICE_0
    q_saled = q_saled+qs
    SELECT STOCK
    REPLACE QNT_REAL WITH QNT_REAL-qs, QNT_REP WITH QNT_REP-qr
  ENDDO
  s_w = s_w + ROUND(q_saled*SALE.PRICE_R,2)
  IF q_saled # LIST_2.QNT
    SELECT TMP_ERR
    APPEND BLANK
    REPLACE CODE     WITH LIST_2.CODE,   ;
            PREFIX   WITH STOCK.PREFIX,  ;
            NAME     WITH STOCK.NAME,    ;
            PRODUCER WITH STOCK.PRODUCER,;
            QNT_REQ  WITH LIST_2.QNT,    ;
            QNT_SAL  WITH q_saled
  ENDIF
  SELECT LIST_2

ENDSCAN

tmpNalog = G_Nalog(firm_w)
SELECT SALE_TIT
APPEND BLANK
REPLACE FIRM      WITH firm_w,  ;
        DOC_NUM   WITH n_sal,   ;
        DOC_DATE  WITH d_sal,   ;
        DOC_TIME  WITH t_sal,   ;
        NEW_DOC   WITH new_d,   ;
        REQ_DOC   WITH rep_n,   ;
        REQ_DATE  WITH rep_d,   ;
        PAY_TYPE  WITH IIF(is_check .AND. .NOT. EMPTY(sum_nal), "—", ""), ;
        PAY_DOC   WITH IIF(sum_nal # 0, numbs(1), ""), ;
        PAY_DATE  WITH IIF(sum_nal # 0, DATE(), {}),   ;
        SAL_FLAGS WITH IIF(is_check, "              * ", ""), ;
        CUS_CODE  WITH c_code,  ;
        CUS_NAME  WITH c_name,  ;
        TOTAL_R   WITH s_w,     ;
        KASSA     WITH MIN(sum_nal, s_w), ;
        TOTAL_0   WITH s_w_0,   ;
        NDS_      WITH nds_flt, ;
        NSP_      WITH tmpNalog,;
        TOTAL_D   WITH s_w_d,   ;
        WHO       WITH user,    ;
        WHERE     WITH arm

REPLACE CUS_REST.ACC_SUM WITH CUS_REST.ACC_SUM - s_w

DO SendToVK WITH firm_w, n_sal, d_sal

DO CheckRem WITH mss_sum

RETURN .T.

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                ฅเฅค ็  คฅญฅฃ ข ช แแใ ฏเฅคฏเจ๏โจ๏.                    บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Send_M
PRIVATE t_rep

SELECT ARM_LST
d_rep = DATE()
t_rep = SECONDS()
n_rep = Doc_Num("KASSA", firm_w, arm, DATE())

&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
&&REPLACE LAST_REP WITH n_rep,   ;
&&        KASSA    WITH KASSA-s0      && คฅแ์ ข ฏฎแซฅคแโขจจ ใกเ โ์!
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
*
*   ฎเเฅชโจเใฅฌ โฎซ์ชฎ แ็ฅโ็จช คฎชใฌฅญโฎข,   คฅญ์ฃจ ใโขฅเฆค ฅโ ช แแจเ!
*
n_rep = sys_char+CHRTRAN(arm," ","0")+STR(n_rep,6)

SELECT 0
PRIVATE kass_rest
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS A_L_000 AGAIN
SEEK arm
kass_rest = KASSA
REPLACE KASSA WITH KASSA+to_kassa

USE (base_path+"KASSA")
APPEND BLANK
REPLACE DOC_NUM    WITH n_rep,   ;
        DOC_DATE   WITH d_rep,   ;
        DOC_TIME   WITH t_rep,   ;
        MONEY_REST WITH 0,       ;
        KASSA_REST WITH kass_rest,;
        TOTAL      WITH to_kassa,;
        NOTE       WITH c_name,  ;
        WHO_REP    WITH user,    ;
        WHERE      WITH arm

USE

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ             “ช ง ญจ๏ ฎโญฎแจโฅซ์ญฎ ฏเฎค ฆจ ขโฎเฎฃฎ แฏจแช .             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Mode2
PARAMETER l_t
PRIVATE ln, wd, ex, pic, s_dep, s_sav, ch_av, let_check, h_sum, p_nlg, is_check

s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_0A11 AGAIN
SEEK IIF(l_t = 1, f_cb, f_cw)
let_check = LET_WCHK
IF l_t = 1
  is_check1 = DEF_WCHK
  is_check  = is_check1
ELSE
  is_check2 = DEF_WCHK
  is_check  = is_check2
ENDIF
p_nlg = G_Nalog(F_0A11.FIRM_CODE)
USE
h_sum = sum_nal+ROUND(p_nlg*sum_nal/100,2)
ln = 16
wd = 47
ex =  1
DO D_W WITH "Get_Mode2", ln, wd, "‘ฏจแฎช N "+IIF(l_t = 1, "1", "2")+", ฏเฎค ฆ ", 0
@ 0, 0 SAY ""

*              ‘ฏจแฎช N 2, ฏเฎค ฆ 
? " ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ"
? " ณ  ฺฤ เฎค ฅฌ ฏฎ ๆฅญ ฌ: ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ  ณ"
? " ณ  ณ  ( ) ‘ชซ ค                          ณ  ณ"
? " ณ  ณ  ( ) ”จซจ ซ                         ณ  ณ"
? " ณ  ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู  ณ"
? " ณ ฺฤ ฅฏฎงจโ ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ"
? " ณ ณ ‘ใฌฌ  ญ  คฅฏฎงจโฅ   99 999 999 999.99 ณ ณ"
? " ณ ณ ‘ใฌฌ  ฏฎ ญ ชซ คญฎฉ  99 999 999 999.99 ณ ณ"
? " ณ ณ   คฅฏฎงจโ          99 999 999 999.99 ณ ณ"
? " ณ ณ โฎฃฎ แ ‘         99 999 999 999.99 ณ ณ"
? " ณ ณ แโ โฎช ญ  คฅฏฎงจโฅ 99 999 999 999.99 ณ ณ"
? " ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ"
? " ณ    [ ] —ฅช      [ ] ฅ็ โ์ คฎชใฌฅญโฎข     ณ"
? " ณ               < OK Ctrl-W >               ณ"
? " ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู"

pic = "@*RV ‘ชซ ค   ["+ALLTRIM(STR(ss,15,2))+"];”จซจ ซ  ["+ALLTRIM(STR(sr,15,2))+"]"

s_dep = CUS_REST.ACC_SUM
@  7, 25 SAY s_dep PICTURE "@Z 99 999 999 999.99"
@  8, 25 SAY IIF(sw_price=1, ss, sr)  PICTURE "@Z 99 999 999 999.99"
@ 11, 25 SAY s_dep - IIF(sw_price=1, ss, sr) PICTURE "@Z 99 999 999 999.99"

@  3,  7 GET sw_price  PICTURE pic VALID P_S()
@  9, 25 GET sum_nal   PICTURE "@Z 99 999 999 999.99" VALID P_S()
@ 10, 25 GET h_sum     PICTURE "@Z 99 999 999 999.99" WHEN .F.
@ 13,  6 GET is_check  PICTURE "@*C —ฅช" WHEN let_check VALID P_S()
@ 13, 19 GET p_nkl     PICTURE "@*C ฅ็ โ์ คฎชใฌฅญโฎข"
@ 14, 17 GET ex        PICTURE "@*T \! OK Ctrl-W "
READ CYCLE

IF l_t = 1
  is_check1 = is_check
ELSE
  is_check2 = is_check
ENDIF

RELEASE WINDOW Get_Mode2
SELECT CUS_REST
REPLACE CUS_REST.ACC_SUM WITH CUS_REST.ACC_SUM + sum_nal
SELECT (s_sav)

RETURN
*********************************************************************
PROCEDURE P_S

p_nlg = G_Nalog(IIF(l_t = 1, f_cb, f_cw))
@  7, 25 SAY s_dep PICTURE "@Z 99 999 999 999.99"
@  8, 25 SAY IIF(sw_price=1, ss, sr)  PICTURE "@Z 99 999 999 999.99"
@ 11, 25 SAY s_dep - IIF(sw_price=1, ss, sr) + sum_nal PICTURE "@Z 99 999 999 999.99"
h_sum = sum_nal+ROUND(p_nlg*sum_nal/100,2)
SHOW GET h_sum

RETURN .T.

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                 ‘ฌฎโเจฌ ช ชจฅ เใกซจ: แโ เ๋ฅ จซจ ญฎข๋ฅ!                 บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 25.12.97 13:30:22 อผ
PROCEDURE See_Rub
PARAMETER nm
PRIVATE a_field, n

n = AFIELDS(a_field)
n = ASCAN(a_field, "PRICE_R")
IF n = 0
  RETURN 1
ENDIF
n = (n-1)/4+1

RETURN IIF(a_field(n,4) = 0, 1000, 1)

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  View_2        งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                   เฎแฌฎโเ ฎคญฎฃฎ จซจ คขใๅ แฏจแชฎข.                    ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 08.02.1999 ฤฤู
PROCEDURE View_2
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

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ ฺฤ ‘ฏจแฎช 1 ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ [ ] เฎแฌฎโเ   [ ] ....:....!....:. [ ] เจฅฌ ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ ฺฤ ‘ฏจแฎช 2 ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ ณ
*ณ ณ [ ] เฎแฌฎโเ   [ ] ....:....!....:. [ ] เจฅฌ ณ ณ
*ณ ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู ณ
*ณ          < OK Ctrl-W > < โช ง โ์แ๏ Esc >         ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

PRIVATE ex, view1, view2, c_f1, c_f2, enab_1, enab_2
enab_1 = lst1
enab_2 = lst2
STORE .F. TO view1, view2, c_f1, c_f2
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 10, 55, " ฃฎโฎขชจ ญ ชซ คญ๋ๅ"
DO Sun_Bord WITH  2,  3,  4, 51, " ‘ฏจแฎช 1 "
DO Sun_Bord WITH  5,  3,  7, 51, " ‘ฏจแฎช 2 "
@ 3,  9 SAY "เฎแฌฎโเ"
@ 3, 24 SAY f_nb
*------------------------------------------------------------------------
@ 6,  9 SAY "เฎแฌฎโเ"
@ 6, 24 SAY f_nw
*      ขฎค ฏฎซฅฉ กซ ญช 
*

@ 3,  5 GET view1 PICTURE "@*C " WHEN enab_1 VALID View_L(1)
@ 3, 20 GET c_f1 PICTURE "@*C " WHEN enab_1 VALID Ch_F(1)
@ 3, 41 GET lst1 PICTURE "@*C เจฅฌ" WHEN enab_1

@ 6,  5 GET view2 PICTURE "@*C " WHEN enab_2 VALID View_L(2)
@ 6, 20 GET c_f2 PICTURE "@*C " WHEN enab_2 VALID Ch_F(2)
@ 6, 41 GET lst2 PICTURE "@*C เจฅฌ" WHEN enab_2

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE

IF ex = 1
  *
  * โเ ก โ๋ข ฅฌ กซ ญช
  f_c000 = f_cb
  SAVE ALL LIKE f_c000 TO (base_path+"B_FIRM.MEM")
  f_c000 = f_cw
  SAVE ALL LIKE f_c000 TO (base_path+"W_FIRM.MEM")
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  View_L        งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                    ฏใแช ฏเฎฃเ ฌฌ๋ ฏเฎแฌฎโเ  แฏจแช .                   ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 08.02.1999 ฤฤู
PROCEDURE View_L
PARAMETERS n_l

IF n_l = 1
  DO Lst_2 WITH "1"
  view1 = .F.
  SHOW GET view1
ELSE
  DO Lst_2 WITH "2"
  view2 = .F.
  SHOW GET view2
ENDIF

RETURN .T.

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  Ch_F          งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                           ๋กฎเ ฏเฅคฏเจ๏โจ๏.                           ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 08.02.1999 ฤฤู
PROCEDURE Ch_F
PARAMETERS n_l

IF n_l = 1
  DO Ch_Firm WITH f_cb, f_nb
  c_f1 = .F.
  SHOW GET c_f1
  @ 3, 24 SAY f_nb
ELSE
  DO Ch_Firm WITH f_cw, f_nw
  c_f2 = .F.
  SHOW GET c_f2
  @ 6, 24 SAY f_nw
ENDIF

RETURN .T.

**************************************************************
*
*    ๋กฎเ ฏ เโจจ
*
PROCEDURE Ch_Part
key_act = LEFT(key_act, 7)
IF .NOT. SEEK(key_act, "ACT_S")
  IF LEN(key_act) > 7
    key_act = LEFT(key_act, 7)
    IF .NOT. SEEK(key_act, "ACT_S")
      RETURN .F.
    ENDIF
  ELSE
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  P_Err         งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                             ฅ็ โ์ ฎ่จกฎช.                             ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 14.03.2000 ฤฤู
PROCEDURE P_Err
PARAMETER qnt_mode
PRIVATE p_ln, fnt, n_cp, lft, n, i, j, l_num, d_ver
PRIVATE t_p, f_p, f_d, d_year, k00, sw, ffeed, p_drctry, d_date
PRIVATE s_sav, f_name, sb, n_p_det, n_pg, f_pg, s_pg, str_w, a_code
PRIVATE nds, nn, c_w, is_c, r_sav

r_sav = RECNO()

DIMENSION sb(3,2)   && NEW 22.12.1999 - ใขฅซจ็จซ คซจญใ ฌ แแจข  ญ  1
sb( 1,1) = "{‘โ/‘โๆ}"
sb( 1,2) = ""

sb( 2,1) = "{ โ     }"
sb( 2,2) = DTOC(DATE())

sb( 3,1) = "{ซจฅญโ                                          }"
sb( 3,2) = c_name

s_sav = SELECT()
SELECT 0

USE (base_path+"DOC_FORM")
LOCATE FOR IIF(qnt_mode, "P_ERR1", "P_ERR") == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
IF n = 0
  DIMENSION t_p(1)
  t_p(1) = ""
ELSE
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)
IF n = 0
  DIMENSION f_p(1)
  f_p(1) = ""
ELSE
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_H)
IF n = 0
  DIMENSION t_d(1)
  t_d(1) = ""
ELSE
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_F)
IF n = 0
  DIMENSION f_d(1)
  f_d(1) = ""
ELSE
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ENDIF
USE

SELECT (s_sav)

DO Ini_Prn WITH "ฅชฎเเฅชโญ๋ฅ ฏฎงจๆจจ", p_ln, lft, n_cp, fnt, ffeed, p_drctry

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
  ELSE
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
  IF RECCOUNT() # 0
    FOR i = 1 TO n_p_det
      nn = nn+1
      IF qnt_mode
        @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+STR(QNT_REQ,6)+STR(QNT_SAL,6)
      ELSE
        @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+" "+ALLTRIM(MESS)
      ENDIF
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
*  USE
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

PRIVATE tmpFName
tmpFName = SYS(3)
COPY TO (tmpo_path+tmpFName)
DO Term_Prn WITH "", tmpo_path+tmpFName+".DBF", c_code

SELECT (s_sav)
GO r_sav
RETURN

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ   เฎๆฅคใเ  G_Nalog       งเ กฎโ็จช €ญคเฅฉ  แจญ                      ณ
*รฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤด
*ณ                                                                        ณ
*ณ                      ๋็จแซฅญจฅ ญ ซฎฃ  แ ฏเฎค ฆ.                       ณ
*ณ                                                                        ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ 21.01.2002 ฤฤู
PROCEDURE G_Nalog
PARAMETERS tmpFirm

PRIVATE s_sav, tmpUr, tmpPers

tmpUr = .T.
IF c_code # 0
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"ACCOUNT") SHARED AGAIN ALIAS CUS_2121 ORDER TAG CUS_CODE
  SEEK c_code
  tmpUr = EMPTY(MARK1)
  USE
  SELECT (s_sav)
ENDIF

*IF is_check .OR. .NOT. tmpUr
IF .NOT. tmpUr
  tmpPers = GetNalog(tmpFirm)
ELSE
  tmpPers = 0
ENDIF

RETURN tmpPers
