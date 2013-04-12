*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 เฎฃเ ฌฌ  ฏเจฅฌ  ฎโ็ฅโ  ไจซจ ซ .                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Receive0

PRIVATE c_name, c_code, pth_w, crs, s_sav, l, c

DO Prep_TD    && ‘ฎงค ฅฌ จซจ ฎ็จ้ ฅฌ โเ ญงจโญใ๎ คจเฅชโฎเจ๎
pth_w = tmpo_path+"TRANSMIT\"

IF .NOT. Get_File(pth_w)
  RETURN
ENDIF

c_code = 0
c_name = ""
crs    = SET("CURSOR") = "ON"
SET CURSOR OFF

ACTIVATE SCREEN
SAVE SCREEN TO s_sav

l = CEILING((WROWS()-4)/2)
c = FLOOR((WCOLS()-21)/2)
@ l+1, c+2 FILL TO l+4, c+22 COLOR (SCHEME(1,8))
@ l,   c SAY SPACE(21) COLOR SCHEME 5
@ l+1, c SAY PADC(" แฏ ชฎขช !",21) COLOR SCHEME 5
@ l+2, c SAY PADC("คฅฌ-แ...",21) COLOR SCHEME 5
@ l+3, c SAY SPACE(21) COLOR SCHEME 5
c = "! ARJ E -V -Y "+pth_w+"F_REPORT "+pth_w+" > nul"
&c
c = "!DEL "+pth_w+"F_REPORT.A*"
&c
RESTORE SCREEN FROM s_sav

IF .NOT. Get_ID(pth_w)
  RETURN
ENDIF

SELECT 0

DO Get_Custs    && ซจฅญโ๋

*DO Get_Acts     && เจฅฌญ๋ฅ  ชโ๋

DO Get_Money    && ซ โฅฆจ

DO Get_Sales    &&  ชซ คญ๋ฅ

DO Get_Bills    && ‘็ฅโ 

DO Get_Kassa    && โ็ฅโ๋ ฏเฎค ขๆฎข

DO Get_Dem      && ‘ฏเฎแ

DO Cmp_Stock    && ‘เ ขญฅญจฅ ญ ซจ็จ๏

DO Put_ID WITH pth_w

CLEAR WINDOWS
CLOSE DATABASES

IF crs
  SET CURSOR ON
ENDIF
RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ขขฎค  ชซจฅญโฎข.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Custs
PRIVATE win_name, arr_w, skp, n_in, n_out, pl_f

win_name = PROGRAM()

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ  ....:....!....:....!....:....!....:....!....:....!  ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

DO D_Win_N WITH 4, 58, "ซจฅญโ๋"

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

IF .NOT. File_O(pth_w+"ACCOUNT.DBF")
  @ 2, 3 SAY PADC("ฅโ ชซจฅญโฎข!  ฆฌจโฅ ็โฎ-ญจกใค์...",  WCOLS()-6)
  = INKEY(10,"MH")
  USE
  RELEASE WINDOW (win_name)
  RETURN
ENDIF

SELECT 0
USE (pth_w+"ACCOUNT") ALIAS SRS

STORE 0 TO n_in, n_out

SCAN
  n_in = n_in+1
  @ 2, 4 SAY CUS_NAME
  IF .NOT. SEEK(CUS_CODE,"ACCOUNT")
    WAIT WINDOW "€…! ‘โเ ญญ๋ฉ ชฎค!  ฆฌจโฅ ็โฎ-ญจกใค์..."
    LOOP
  ENDIF
  IF SUBSYS # ACCOUNT.SUBSYS
    WAIT WINDOW "€…! ‘โเ ญญ ๏ ฏซฎ้ คช !  ฆฌจโฅ ็โฎ-ญจกใค์..."
    LOOP
  ENDIF
  n_out = n_out+1
  IF ACCOUNT.DATE_CORR < SRS.DATE_CORR
    REPLACE ACCOUNT.CUS_NAME   WITH SRS.CUS_NAME,  ;
            ACCOUNT.DATE_ON    WITH SRS.DATE_ON,   ;
            ACCOUNT.WHO        WITH SRS.WHO,       ;
            ACCOUNT.WHERE      WITH IIF(LEN(SRS.WHERE) = 1,RIGHT(STR(ASC(SRS.WHERE)),3),SRS.WHERE),;
            ACCOUNT.DATE_CORR  WITH SRS.DATE_CORR, ;
            ACCOUNT.WHO_CORR   WITH SRS.WHO_CORR,  ;
            ACCOUNT.WHERE_CORR WITH IIF(LEN(SRS.WHERE_CORR) = 1,RIGHT(STR(ASC(SRS.WHERE_CORR)),3),SRS.WHERE_CORR)
  ENDIF
ENDSCAN

USE (base_path+"CUS_BIL") ORDER TAG CUS_CODE
SELECT 0
USE (pth_w+"CUS_BIL") ALIAS SRS
SCAN
  IF .NOT. SEEK(CUS_CODE, "ACCOUNT")
    LOOP
  ENDIF
  IF SEEK(CUS_CODE, "CUS_BIL")
    IF CUS_BIL.DATE_CORR < SRS.DATE_CORR
      SCATTER TO arr_w
      pl_f = ASCAN(arr_w,WHERE_CORR)
      IF LEN(arr_w(pl_f)) = 1
        arr_w(pl_f) = RIGHT(STR(ASC(arr_w(pl_f))),3)
      ENDIF
      SELECT CUS_BIL
      GATHER FROM arr_w
      SELECT SRS
    ENDIF
  ELSE
    SCATTER TO arr_w
    pl_f = ASCAN(arr_w, WHERE_CORR)
    IF LEN(arr_w(pl_f)) = 1
      arr_w(pl_f) = RIGHT(STR(ASC(arr_w(pl_f))),3)
    ENDIF
    SELECT CUS_BIL
    APPEND FROM ARRAY arr_w
    SELECT SRS
  ENDIF
ENDSCAN

SELECT CUS_BIL
USE (base_path+"CLI_INFO") ORDER TAG CUS_CODE
SELECT SRS
USE (pth_w+"CLI_INFO") ALIAS SRS
SCAN
  IF .NOT. SEEK(CUS_CODE, "ACCOUNT")
    LOOP
  ENDIF
  IF SEEK(CUS_CODE, "CLI_INFO")
    SCATTER TO arr_w
    pl_f = ASCAN(arr_w, WHERE)
    IF LEN(arr_w(pl_f)) = 1
      arr_w(pl_f) = RIGHT(STR(ASC(arr_w(pl_f))),3)
    ENDIF
    SELECT CLI_INFO
    GATHER FROM arr_w
    SELECT SRS
  ELSE
    SCATTER TO arr_w
    pl_f = ASCAN(arr_w, WHERE)
    IF LEN(arr_w(pl_f)) = 1
      arr_w(pl_f) = RIGHT(STR(ASC(arr_w(pl_f))),3)
    ENDIF
    SELECT CLI_INFO
    APPEND FROM ARRAY arr_w
    SELECT SRS
  ENDIF
ENDSCAN

USE
SELECT ACCOUNT
USE
SELECT CLI_INFO
USE
@ 2, 2 SAY PADC("เฎ็จโ ญฎ "+ALLTRIM(STR(n_in))+",  ง ฏจแ ญฎ "+ ;
                ALLTRIM(STR(n_out))+".", WCOLS()-4)
skp = FLOOR((WCOLS()-23)/2)
@ 3, skp SAY "  ฆฌจโฅ ็โฎ-ญจกใค์... "
= INKEY(15,"HM")
RELEASE WINDOW (win_name)

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 เฎฃเ ฌฌ  ขขฎค  ฏเจฅฌญ๋ๅ  ชโฎข.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Acts
PRIVATE win_name, arr_w, skp, n_in, n_out

win_name = PROGRAM()

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ      เจฅฌญ๋ฉ  ชโ N 99999 ฎโ 99.99.9999    ณ
*ณ                                            ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

DO D_Win_N WITH 5, 48, "เจฅฌญ๋ฅ  ชโ๋"

USE (base_path+"ACT_F_T") ORDER TAG DOC_NUM

IF .NOT. File_O(pth_w+"ACT_S.DBF") .OR. .NOT. File_O(pth_w+"ACT_S_T.DBF")
  @ 2, 3 SAY PADC("ฅโ ไ ฉซ  ฏเจฅฌญ๋ๅ  ชโฎข!",   WCOLS()-6)
  @ 3, 3 SAY PADC(" ฆฌจโฅ ็โฎ-ญจกใค์...", WCOLS()-6)
  = INKEY(10,"MH")
  USE
  RELEASE WINDOW (win_name)
ENDIF

SELECT 0
USE (base_path+"ACT_F")

SELECT 0
USE (pth_w+"ACT_S")

SELECT 0
USE (pth_w+"ACT_S_T")

STORE 0 TO n_in, n_out
SCAN
  n_in = n_in+1
  @ 2, 8 SAY "เจฅฌญ๋ฉ  ชโ N "+ALLTRIM(STR(DOC_NUM))+" ฎโ "+TRANSFORM(DOC_DATE,"@D")
  skp = SEEK(LEFT(DTOS(DOC_DATE),4)+STR(CUS_CODE,5)+STR(DOC_NUM,5), "ACT_F_T")
  @ 3, 2 SAY PADC(IIF(skp, "เฎฏใแช ฅฌ", " ฏจแ๋ข ฅฌ"), WCOLS()-4)
  IF .NOT. skp
    n_out = n_out+1
    SCATTER TO arr_w
    SELECT ACT_F_T
    APPEND FROM ARRAY arr_w
  ENDIF
  SELECT ACT_S
  SCAN REST WHILE DOC_NUM = ACT_S_T.DOC_NUM .AND. DOC_DATE = ACT_S_T.DOC_DATE
    IF .NOT. skp
      SCATTER TO arr_w
      DIMENSION arr_w(ALEN(arr_w)+1)
      = AINS(arr_w, 3)
      arr_w(3) = ACT_F_T.CUS_CODE
      SELECT ACT_F
      APPEND FROM ARRAY arr_w
      SELECT ACT_S
    ENDIF
  ENDSCAN
  SELECT ACT_S_T
ENDSCAN

USE
SELECT ACT_S
USE
SELECT ACT_F
USE
SELECT ACT_F_T
USE
@ 2, 2 CLEAR TO WROWS()-2, WCOLS()-3
@ 2, 2 SAY PADC("เฎ็จโ ญฎ "+ALLTRIM(STR(n_in))+" คฎชใฌฅญโฎข, ง ฏจแ ญฎ "+ ;
                ALLTRIM(STR(n_out))+".", WCOLS()-4)
@ 3, 2 SAY PADC(" ฆฌจโฅ ็โฎ-ญจกใค์...", WCOLS()-4)
= INKEY(15,"HM")
RELEASE WINDOW (win_name)

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                   เฎฃเ ฌฌ  ขขฎค  ญ ชซ คญ๋ๅ.                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Sales
PRIVATE win_name, arr_w, skp, i_, n_in, n_out, f_val

win_name = PROGRAM()

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ        ชซ คญ ๏ N 99999 ฎโ 99.99.9999      ณ
*ณ                                            ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

DO D_Win_N WITH 5, 48, " ชซ คญ๋ฅ"

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS HOME_TIT

SELECT 0
USE (base_path+"SALE") ALIAS HOME
i_ = 1
DO WHILE .T.
  IF FIELD(i_) = "QNT_REP"
    EXIT
  ENDIF
  i_ = i_+1
ENDDO

SET EXCLUSIVE ON

DELETE FILE (tmpo_path+"INT_SALE.DBF")
DELETE FILE (tmpo_path+"INT_SALE.CDX")

COPY STRUCTURE TO (tmpo_path+"INT_SALE")
SELECT 0
USE (tmpo_path+"INT_SALE")
INDEX ON CODE TAG CODE

SET EXCLUSIVE OFF

SELECT 0
USE (pth_w+"SALE")

SELECT 0
USE (pth_w+"SALE_TIT")

STORE 0 TO n_in, n_out
SCAN
  n_in = n_in+1
  @ 2, 7 SAY " ชซ คญ ๏ N "+LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5))+" ฎโ "+TRANSFORM(DOC_DATE,"@D")
  skp = SEEK(FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM, "HOME_TIT")
  @ 3, 2 SAY PADC(IIF(skp, "เฎฏใแช ฅฌ", " ฏจแ๋ข ฅฌ"), WCOLS()-4)
  IF LEN(WHERE) = 1
    f_val = RIGHT(STR(ASC(WHERE)),3)
  ELSE
    f_val = WHERE
  ENDIF
  IF .NOT. SEEK(f_val,"ARM_LST")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"ฅโ เ กฎ็ฅฃฎ ฌฅแโ  แ ชฎคฎฌ "+f_val+"!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    LOOP
  ENDIF
  IF .NOT. SEEK(CUS_CODE, "ACCOUNT") .AND. CUS_CODE # 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"ฅ ญ ฉคฅญ ชซจฅญโ แ ชฎคฎฌ "+ALLTRIM(STR(CUS_CODE))+"!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    LOOP
  ENDIF
  IF .NOT. skp
    n_out = n_out+1
    DO Copy_N WITH "HOME_TIT"
*    REPLACE TOTAL_D WITH 0
*       ‘ • ……  INCMONEY!
*    IF SALE_TIT.CUS_CODE = 0
*      REPLACE ARM_LST.KASSA WITH ARM_LST.KASSA+KASSA
*    ENDIF
    IF CUS_CODE # 0
      SELECT CUS_REST
      IF .NOT. SEEK(STR(SALE_TIT.CUS_CODE)+SALE_TIT.FIRM)
        APPEND BLANK
        REPLACE CUS_CODE WITH SALE_TIT.CUS_CODE, ;
                FIRM     WITH SALE_TIT.FIRM
      ENDIF
      IF SALE_TIT.NEW_TYPE = " "
        REPLACE ACC_SUM WITH ACC_SUM-SALE_TIT.TOTAL_R
      ELSE
        REPLACE ACC_SUMD WITH ACC_SUMD-SALE_TIT.TOTAL_D
      ENDIF

*             ACCOUNT.BUY_SUM  WITH ACCOUNT.BUY_SUM+TOTAL_R, ;
*             ACCOUNT.BUY_SUMD WITH ACCOUNT.BUY_SUM+TOTAL_D
              
    ENDIF
      
  ENDIF
  SELECT SALE
  SCAN REST WHILE FIRM = SALE_TIT.FIRM .AND. DOC_NUM = SALE_TIT.DOC_NUM .AND. DOC_DATE = SALE_TIT.DOC_DATE
    IF .NOT. skp
      DO Copy_N WITH "HOME"
      REPLACE QNT_REP WITH 0
      SELECT INT_SALE
      IF .NOT. SEEK(SALE.CODE)
        APPEND BLANK
      ENDIF
      REPLACE CODE WITH SALE.CODE, ;
              QNT  WITH QNT+SALE.QNT
      SELECT SALE
    ENDIF
  ENDSCAN
  IF .NOT. skp
    DO SendToVK WITH HOME_TIT.FIRM, HOME_TIT.DOC_NUM, HOME_TIT.DOC_DATE
  ENDIF
  SELECT SALE_TIT
ENDSCAN

@ 2, 2 CLEAR TO WROWS()-2, WCOLS()-3
@ 2, 2 SAY PADC("เฎ็จโ ญฎ "+ALLTRIM(STR(n_in))+" คฎชใฌฅญโฎข, ง ฏจแ ญฎ "+ ;
                ALLTRIM(STR(n_out))+".", WCOLS()-4)
@ 3, 2 SAY PADC(" ฆฌจโฅ ็โฎ-ญจกใค์...", WCOLS()-4)
= INKEY(15,"HM")
RELEASE WINDOW (win_name)

USE
SELECT SALE
USE
SELECT ACCOUNT
USE
SELECT CUS_REST
USE
SELECT ARM_LST
IF RECCOUNT("INT_SALE") = 0
  USE
  SELECT INT_SALE
  USE
  SELECT HOME_TIT
  USE
  SELECT HOME
  USE
  DELETE FILE (tmpo_path+"INT_SALE.DBF")
  DELETE FILE (tmpo_path+"INT_SALE.CDX")
  RETURN
ENDIF
PRIVATE d_n, d_d, d_t
SEEK arm
d_d = DATE()
d_t = SECONDS()
d_n = Doc_Num("SALE", def_firm, arm, d_d)
d_n = sys_char+CHRTRAN(arm," ","0")+STR(d_n,6)
SELECT 0
DELETE FILE (tmpo_path+"ERR_SAL.DBF")
DELETE FILE (tmpo_path+"ERR_SAL.CDX")
CREATE DBF (tmpo_path+"ERR_SAL.DBF") ;
  ( CODE      N( 7),  ;
    PREFIX    C( 4),  ;
    NAME      C(35),  ;
    PRODUCER  C( 8),  ;
    Q_REQ     N( 6),  ;
    Q_SAL     N( 6)  )
INDEX ON NAME+PREFIX TAG NAME

SELECT 0
USE (base_path+"RYN_L") ORDER TAG C_CODE
SELECT 0
USE (base_path+"ACT_S") ORDER TAG SALE
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE
SELECT INT_SALE

PRIVATE q_rest, q_rep, q_doc, q_sale

PRIVATE key_act
SCAN
  = SEEK(CODE,"STOCK")
  IF .NOT. SEEK(STR(c_code, 5)+STR(CODE,7), "RYN_L")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+'ฎงจๆจ๏ "'+ALLTRIM(STOCK.PREFIX)+" "+ALLTRIM(STOCK.NAME)+'" ง  ชซจฅญโฎฌ ญฅ ็จแซจโแ๏!'
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SELECT RYN_L
    APPEND BLANK
    REPLACE CUS_CODE WITH c_code, ;
            CODE     WITH INT_SALE.CODE,    ;
            PRICE_D  WITH INT_SALE.PRICE_D, ;
            PRICE_R  WITH INT_SALE.PRICE_R
    SELECT INT_SALE
  ENDIF
  q_rest = INT_SALE.QNT
  key_act = STR(INT_SALE.CODE,7)+def_firm
  q_sale = 0
  IF q_rest < 0
    SELECT ACT_S
    SET ORDER TO TAG INC_LIST
    IF .NOT. SEEK(STR(INT_SALE.CODE,7)+def_firm+"        **********")
      APPEND BLANK
      REPLACE FIRM      WITH def_firm,      ;
              DOC_NUM   WITH "**********",  ;
              FLG_BUY   WITH "B",           ;
              CODE      WITH INT_SALE.CODE, ;
              LAST_SALE WITH d_d,           ;
              QNT_REST  WITH -INT_SALE.QNT
    ELSE
      REPLACE QNT_REST WITH QNT_REST-INT_SALE.QNT, LAST_SALE WITH DATE()
    ENDIF
    SET ORDER TO TAG SALE
    *
    *  ฎเเฅชๆจ๏ ญ ซจ็จ๏ ข ไจซจ ซฅ
    *
    SELECT RYN_L
    REPLACE QNT WITH QNT-INT_SALE.QNT

    SELECT HOME
    APPEND BLANK
    REPLACE FIRM      WITH def_firm,;
            DOC_NUM   WITH d_n,    ;
            DOC_DATE  WITH d_d,    ;
            CODE      WITH INT_SALE.CODE, ;
            QNT       WITH INT_SALE.QNT,  ;
            PRICE_D_L WITH STOCK.PRICE,   ;
            PRICE_R_L WITH Clc_Rub(STOCK.PRICE), ;
            ACT_FIRM  WITH ACT_S.FIRM,    ;
            ACT_NUM   WITH ACT_S.DOC_NUM, ;
            ACT_DATE  WITH ACT_S.DOC_DATE,;
            PARTY     WITH ACT_S.PARTY,   ;
            GTD_NUM   WITH ACT_S.GTD_NUM, ;
            COUNTRY   WITH ACT_S.COUNTRY, ;
            COUNTRYC  WITH ACT_S.COUNTRYC,;
            GTD_LINE  WITH ACT_S.GTD_LINE,;
            WHERE     WITH arm

    SELECT STOCK
    REPLACE QNT_REAL WITH QNT_REAL-INT_SALE.QNT
    SELECT INT_SALE
    LOOP
  ENDIF
  DO WHILE q_rest > 0
    IF .NOT. Ch_Part()
      EXIT
    ENDIF
    q_doc = MIN(q_rest, ACT_S.QNT_REST)
    q_rep = MIN(ACT_S.REST_REP, q_doc)
    q_rest = q_rest-q_doc
    SELECT HOME
    APPEND BLANK
    REPLACE FIRM      WITH def_firm, ;
            DOC_NUM   WITH d_n,    ;
            DOC_DATE  WITH d_d,    ;
            CODE      WITH INT_SALE.CODE, ;
            QNT       WITH q_doc,         ;
            QNT_REP   WITH q_rep,         ;
            PRICE_D_L WITH STOCK.PRICE,   ;
            PRICE_R_L WITH Clc_Rub(STOCK.PRICE), ;
            ACT_FIRM  WITH ACT_S.FIRM,    ;
            ACT_NUM   WITH ACT_S.DOC_NUM, ;
            ACT_DATE  WITH ACT_S.DOC_DATE,;
            GTD_NUM   WITH ACT_S.GTD_NUM, ;
            PARTY     WITH ACT_S.PARTY,   ;
            COUNTRY   WITH ACT_S.COUNTRY, ;
            COUNTRYC  WITH ACT_S.COUNTRYC,;
            GTD_LINE  WITH ACT_S.GTD_LINE,;
            WHERE     WITH arm
    SELECT STOCK
    q_sale = q_sale+q_doc
    REPLACE QNT_REP  WITH QNT_REP-q_rep,  ;
            QNT_REAL WITH QNT_REAL-q_doc
    SELECT ACT_S
    REPLACE QNT_REST WITH QNT_REST-q_doc,  ;
            REST_REP WITH REST_REP-q_rep,    ;
            LAST_SALE WITH d_d
    SELECT RYN_L
    REPLACE RYN_L.QNT WITH RYN_L.QNT-q_doc
    SELECT INT_SALE
  ENDDO
  IF INT_SALE.QNT # q_sale
    SELECT ERR_SAL
    APPEND BLANK
    REPLACE CODE     WITH INT_SALE.CODE,  ;
            PREFIX   WITH STOCK.PREFIX,   ;
            NAME     WITH STOCK.NAME,     ;
            PRODUCER WITH STOCK.PRODUCER, ;
            Q_REQ    WITH INT_SALE.QNT,   ;
            Q_SAL    WITH q_sale
    SELECT INT_SALE
  ENDIF
ENDSCAN
SELECT HOME_TIT
APPEND BLANK
REPLACE FIRM      WITH def_firm, ;
        DOC_NUM   WITH d_n,    ;
        DOC_DATE  WITH d_d,    ;
        DOC_TIME  WITH d_t,    ;
        NEW_TYPE  WITH "0",    ;
        CUS_CODE  WITH c_code, ;
        CUS_NAME  WITH c_name, ;
        CUS_TYPE  WITH 4,      ;
        SEND_M    WITH -1,     ;
        WHO       WITH user,   ;
        WHERE     WITH arm
USE
SELECT STOCK
USE
SELECT RYN_L
USE
SELECT ERR_SAL
IF RECCOUNT() # 0
  DO V_Err
ENDIF
USE
DELETE FILE (tmpo_path+"ERR_SAL.DBF")
DELETE FILE (tmpo_path+"ERR_SAL.CDX")

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                    เฎฃเ ฌฌ  ขขฎค  แ็ฅโฎข.                            บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Bills
PRIVATE win_name, arr_w, skp, mss, n_in, n_out, pl_f

win_name = PROGRAM()

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ     ฎชใฌฅญโ N 99999999999 ฎโ 99.99.9999   ณ
*ณ                                            ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

DO D_Win_N WITH 5, 48, "‘็ฅโ "

USE (base_path+"BILLS") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"BILLS_D")

SELECT 0
USE (pth_w+"BILLS_D") ALIAS SOURCE_D

SELECT 0
USE (pth_w+"BILLS") ALIAS SOURCE

STORE 0 TO n_in, n_out
SCAN
  n_in = n_in+1
  @ 2, 7 SAY "ฎชใฌฅญโ N "+LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5))+" ฎโ "+TRANSFORM(DOC_DATE,"@D")
  skp = SEEK(FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM, "BILLS")
  @ 3, 2 SAY PADC(IIF(skp, "เฎฏใแช ฅฌ", " ฏจแ๋ข ฅฌ"), WCOLS()-4)
  IF skp
    SELECT SOURCE_D
    SCAN REST WHILE SOURCE.FIRM = FIRM .AND. SOURCE.DOC_NUM = DOC_NUM .AND. SOURCE.DOC_DATE = DOC_DATE
    ENDSCAN
    SELECT SOURCE
    LOOP
  ENDIF

  n_out = n_out+1
  SCATTER TO arr_w
  pl_f = ASCAN(arr_w, WHERE)
  IF LEN(arr_w(pl_f)) = 1
    arr_w(pl_f) = RIGHT(STR(ASC(arr_w(pl_f))),3)
  ENDIF
  SELECT BILLS
  APPEND FROM ARRAY arr_w

  SELECT SOURCE_D
  SCAN REST WHILE SOURCE.FIRM = FIRM .AND. SOURCE.DOC_NUM = DOC_NUM .AND. SOURCE.DOC_DATE = DOC_DATE
    SCATTER TO arr_w
    SELECT BILLS_D
    APPEND FROM ARRAY arr_w
    SELECT SOURCE_D
  ENDSCAN
  SELECT SOURCE
ENDSCAN

USE
SELECT BILLS
USE
SELECT BILLS_D
USE
SELECT SOURCE_D
USE

@ 2, 2 CLEAR TO WROWS()-2, WCOLS()-3
@ 2, 2 SAY PADC("เฎ็จโ ญฎ "+ALLTRIM(STR(n_in))+" คฎชใฌฅญโฎข, ง ฏจแ ญฎ "+ ;
                ALLTRIM(STR(n_out))+".", WCOLS()-4)
@ 3, 2 SAY PADC(" ฆฌจโฅ ็โฎ-ญจกใค์...", WCOLS()-4)
= INKEY(15,"HM")
RELEASE WINDOW (win_name)

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                       เฎฃเ ฌฌ  ฏซ โฅฆฅฉ.                             บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Money
PRIVATE win_name, arr_w, skp, mss, n_in, n_out, f_val, pl_f

win_name = PROGRAM()

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ     ฎชใฌฅญโ N 99999999999 ฎโ 99.99.9999   ณ
*ณ                                            ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

DO D_Win_N WITH 5, 48, "ซ โฅฆจ"

USE (base_path+"INCMONEY") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE

SELECT 0
USE (pth_w+"INCMONEY") ALIAS SOURCE

STORE 0 TO n_in, n_out
SCAN
  n_in = n_in+1
  @ 2, 7 SAY "ฎชใฌฅญโ N "+LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5))+" ฎโ "+TRANSFORM(DOC_DATE,"@D")
  skp = SEEK(FIRM+DOC_TYPE+LEFT(DTOS(DOC_DATE),4)+DOC_NUM, "INCMONEY")
  @ 3, 2 SAY PADC(IIF(skp, "เฎฏใแช ฅฌ", " ฏจแ๋ข ฅฌ"), WCOLS()-4)
  IF skp
    LOOP
  ENDIF
  IF CUS_CODE # 0
    IF .NOT. SEEK(CUS_CODE, "ACCOUNT")
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"ฅโ ชซจฅญโ  แ ชฎคฎฌ "+ALLTRIM(STR(CUS_CODE))
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      LOOP
    ENDIF
  ENDIF

  IF LEN(WHERE) = 1
    f_val = RIGHT(STR(ASC(WHERE)),3)
  ELSE
    f_val = WHERE
  ENDIF
  IF .NOT. SEEK(f_val, "ARM_LST")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"ฅโ เ กฎ็ฅฃฎ ฌฅแโ  แ ชฎคฎฌ "+f_val+"!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    LOOP
  ENDIF

  IF .NOT. SEEK(FIRM, "FIRMS")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"ฅโ ฏเฅคฏเจ๏โจ๏ แ ชฎคฎฌ "+FIRM
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    LOOP
  ENDIF

  n_out = n_out+1
  SCATTER TO arr_w
  pl_f = ASCAN(arr_w, WHERE)
  IF LEN(arr_w(pl_f)) = 1
    arr_w(pl_f) = RIGHT(STR(ASC(arr_w(pl_f))),3)
  ENDIF
  SELECT INCMONEY
  APPEND FROM ARRAY arr_w
  SELECT CUS_REST
  IF INCMONEY.CUS_CODE # 0
    IF .NOT. SEEK(STR(INCMONEY.CUS_CODE)+INCMONEY.FIRM)
      APPEND BLANK
      REPLACE CUS_CODE WITH INCMONEY.CUS_CODE, ;
              FIRM     WITH INCMONEY.FIRM
    ENDIF
    DO CASE
    CASE INCMONEY.DOC_TYPE $ " 1—"
      REPLACE ACC_SUM WITH ACC_SUM+INCMONEY.MONEY_R
    CASE INCMONEY.DOC_TYPE = "3"
      REPLACE ACC_SUMD WITH ACC_SUMD+INCMONEY.MONEY_D
    ENDCASE
  ENDIF
  SELECT INCMONEY
  IF DOC_TYPE $ " —"
    REPLACE ARM_LST.KASSA WITH ARM_LST.KASSA+MONEY_R+N_OF_SALE
  ENDIF
  IF DOC_TYPE = "3"
    REPLACE ARM_LST.KASSA_USD WITH ARM_LST.KASSA_USD+MONEY_D+N_OF_SALE
  ENDIF
  REPLACE FIRMS.NALOG WITH FIRMS.NALOG+N_OF_SALE
  SELECT SOURCE
ENDSCAN

USE
SELECT INCMONEY
USE
SELECT ACCOUNT
USE
SELECT CUS_REST
USE
SELECT ARM_LST
USE

@ 2, 2 CLEAR TO WROWS()-2, WCOLS()-3
@ 2, 2 SAY PADC("เฎ็จโ ญฎ "+ALLTRIM(STR(n_in))+" คฎชใฌฅญโฎข, ง ฏจแ ญฎ "+ ;
                ALLTRIM(STR(n_out))+".", WCOLS()-4)
@ 3, 2 SAY PADC(" ฆฌจโฅ ็โฎ-ญจกใค์...", WCOLS()-4)
= INKEY(15,"HM")
RELEASE WINDOW (win_name)

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                เฎฃเ ฌฌ  ขขฎค  ฎโ็ฅโ  ฏเฎค ขๆฎข.                      บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Kassa
PRIVATE win_name, arr_w, skp, mss, n_in, n_out, pl_f

win_name = PROGRAM()

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ     ฎชใฌฅญโ N 99999999999 ฎโ 99.99.9999   ณ
*ณ                                            ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

DO D_Win_N WITH 5, 48, "โ็ฅโ๋ ฏเฎค ขๆฎข"

USE (base_path+"KASSA") ORDER TAG DOC_NUM

SELECT 0
USE (pth_w+"KASSA") ALIAS SOURCE

STORE 0 TO n_in, n_out
SCAN
  n_in = n_in+1
  @ 2, 7 SAY "ฎชใฌฅญโ N "+LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5))+" ฎโ "+TRANSFORM(DOC_DATE,"@D")
  skp = SEEK(LEFT(DTOS(DOC_DATE),4)+DOC_NUM, "KASSA")
  @ 3, 2 SAY PADC(IIF(skp, "เฎฏใแช ฅฌ", " ฏจแ๋ข ฅฌ"), WCOLS()-4)
  IF skp
    LOOP
  ENDIF

  n_out = n_out+1
  SCATTER TO arr_w
  pl_f = ASCAN(arr_w, WHERE)
  IF LEN(arr_w(pl_f)) = 1
    arr_w(pl_f) = RIGHT(STR(ASC(arr_w(pl_f))),3)
  ENDIF
  SELECT KASSA
  APPEND FROM ARRAY arr_w
  SELECT SOURCE
ENDSCAN

USE
SELECT KASSA
USE

@ 2, 2 CLEAR TO WROWS()-2, WCOLS()-3
@ 2, 2 SAY PADC("เฎ็จโ ญฎ "+ALLTRIM(STR(n_in))+" คฎชใฌฅญโฎข, ง ฏจแ ญฎ "+ ;
                ALLTRIM(STR(n_out))+".", WCOLS()-4)
@ 3, 2 SAY PADC(" ฆฌจโฅ ็โฎ-ญจกใค์...", WCOLS()-4)
= INKEY(15,"HM")
RELEASE WINDOW (win_name)

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                      เฎฃเ ฌฌ  ขขฎค  แฏเฎแ .                          บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Dem
PRIVATE mss, i_rec, i_pass, i, dif, arr_w, mss, w, let_p, pl_f

*DIMENSION mss(3)
*mss(1) = ""
*mss(2) = CHR(0)+" เจญจฌ ฅฌ แฏเฎแ? [   ] [ ฅโ ]"
*mss(3) = ""

*IF Out_Mess(5, "mss") = 2
*  RETURN
*ENDIF

USE (base_path+"DEMAND") ORDER TAG RECEIVE
SELECT 0
USE (pth_w+"DEMAND") ALIAS SOURCE

dif = .F.
i_rec = 0
i_pass = 0
IF FCOUNT("SOURCE") # FCOUNT("DEMAND")
  dif = .T.
ELSE
  FOR i = 1 TO FCOUNT()
    IF .NOT. FIELD(i,"SOURCE") == FIELD(i,"DEMAND")
      dif = .T.
      EXIT
    ENDIF
    IF FIELD(i,"SOURCE") == "PASSED"
      i_pass = i
    ENDIF
    IF FIELD(i,"SOURCE") == "RECEIVE"
      i_rec = i
    ENDIF
  ENDFOR
ENDIF
IF dif .OR. i_pass = 0 .OR. i_rec = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"—โฎ-โฎ ญฅ โ ช แฎ แโเใชโใเฎฉ ไ ฉซ  แฏเฎแ , ฏใแโ์ เ งเ กฎโ็จช เ งกฅเฅโแ๏..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE 
  SELECT DEMAND
  USE
  RETURN
ENDIF

let_p = "*****"
SCAN
  IF let_p # PASSED
    IF SEEK(PASSED,"DEMAND")
      LOOP
    ENDIF
    let_p = PASSED
  ENDIF
  SCATTER TO arr_w
  pl_f = ASCAN(arr_w, WHERE)
  IF LEN(arr_w(pl_f)) = 1
    arr_w(pl_f) = RIGHT(STR(ASC(arr_w(pl_f))),3)
  ENDIF
  arr_w(i_rec) = arr_w(i_pass)
  arr_w(i_pass) = ""
  SELECT DEMAND
  APPEND FROM ARRAY arr_w
  SELECT SOURCE
ENDSCAN

USE
SELECT DEMAND
USE

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎฃเ ฌฌ  ขขฎค  จ เ แฏ ชฎขชจ ARJ ไ ฉซ .                  บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_File
PARAMETER pth

PRIVATE get_pth, i, mss, mss1, nm_w, w, is_fdd

get_pth = FromDisk(pth+"F_REPORT.ARJ")

IF EMPTY(get_pth)
  RETURN .F.
ENDIF

is_fdd = .F.
IF LEN(get_pth) = 3
  IF INLIST(UPPER(LEFT(get_pth,2)), "A:", "B:")
    is_fdd = .T.
  ENDIF
ENDIF
IF is_fdd
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"…้ฅ กใคฅโ คจแชฅโ ? [   ] [ ฅโ ]"
  mss(3) = ""
  i = 1
  DO WHILE .T.
    nm_w = "F_REPORT.A"+RIGHT(STR(100+i,3),2)
    IF Out_Mess(5, "mss") = 2
      EXIT
    ENDIF
    w = get_pth
    CALL IsDisk WITH w
    IF w # get_pth
      DIMENSION mss1(3)
      mss1(1) = ""
      mss1(2) = CHR(0)+"๋ ญฅ ใแโ ญฎขจซจ คจแชฅโใ!"
      mss1(3) = ""
      DO Out_Mess WITH 7,"mss1"
      LOOP
    ENDIF
    IF .NOT. File_O(get_pth+nm_w)
      DIMENSION mss1(3)
      mss1(1) = ""
      mss1(2) = CHR(0)+"๋ ใแโ ญฎขจซจ ญฅ โใ คจแชฅโใ!"
      mss1(3) = ""
      DO Out_Mess WITH 7,"mss1"
      LOOP
    ENDIF
    COPY FILE (get_pth+nm_w) TO (pth+nm_w)
    i = i+1
  ENDDO
ELSE
  i = 1
  DO WHILE .T.
    nm_w = "F_REPORT.A"+RIGHT(STR(100+i,3),2)
    w = get_pth
    IF .NOT. File_O(get_pth+nm_w)
      EXIT
    ENDIF
    DO Wt_Mess WITH "จ่ฅฌ "+nm_w
    COPY FILE (get_pth+nm_w) TO (pth+nm_w)
    i = i+1
  ENDDO
ENDIF

DIMENSION mss(4)
mss(1) = ""
mss(2) = CHR(0)+"แฅ แ็จโ ญฎ ฏเ ขจซ์ญฎ?  ็จญ ฅฌ ฎกเ กฎโชใ?"
mss(3) = CHR(0)+"  [   ] [ ฅโ, ฎโช ฆฅฌแ๏! ]"
mss(4) = ""
RETURN Out_Mess(5, "mss") = 1

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ               เฎฃเ ฌฌ  ง ฏจแจ ฏเจฅฌ  ข ฏเฎโฎชฎซ.                     บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Put_ID
PARAMETERS pth  && ใโ์ ช ฏ ชฅโใ.

PRIVATE mss, i_code, i_corr, i_self, arr_w, n, i, w

SELECT ID_SEND
n = FCOUNT()
STORE 0 TO i_code, i_corr, i_self
FOR i = 1 TO n
  w = FIELD(i)
  IF w == "TR_CODE"
    i_code = i
  ENDIF
  IF w == "ID_CORR"
    i_corr = i
  ENDIF
  IF w == "ID_SELF"
    i_self = i
  ENDIF
ENDFOR

SCATTER TO arr_w

arr_w(i_code) = "RF"
w = arr_w(i_corr)
arr_w(i_corr) = arr_w(i_self)
arr_w(i_self) = w

SELECT TR_LOG
APPEND FROM ARRAY arr_w

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                 เฎฃเ ฌฌ  แขฅเชจ ญฎฌฅญชซ โใเ๋.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Cmp_Stock
PRIVATE win_name, arr_w, skp, n_in, n_out, vl, tmpQnt

win_name = PROGRAM()

*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ                                            ณ
*ณ                                            ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

DO D_Win_N WITH 5, 48, "ฎฌฅญชซ โใเ "

IF .NOT. File_O(pth_w+"STOCK.DBF")
  @ 2, 3 SAY PADC("ฅโ ไ ฉซ  ญฎฌฅญชซ โใเ๋!",   WCOLS()-6)
  @ 3, 3 SAY PADC(" ฆฌจโฅ ็โฎ-ญจกใค์...", WCOLS()-6)
  = INKEY(10,"MH")
  RELEASE WINDOW (win_name)
ENDIF

USE (pth_w+"STOCK")

DIMENSION arr_w(12, 4)

arr_w( 1,1) = "CODE"
arr_w( 1,2) = "N"
arr_w( 1,3) = FSIZE("CODE")
arr_w( 1,4) = 0

arr_w( 2,1) = "NAME"
arr_w( 2,2) = "C"
arr_w( 2,3) = FSIZE("NAME")
arr_w( 2,4) = 0

arr_w( 3,1) = "PREFIX"
arr_w( 3,2) = "C"
arr_w( 3,3) = FSIZE("PREFIX")
arr_w( 3,4) = 0

arr_w( 4,1) = "PRODUCER"
arr_w( 4,2) = "C"
arr_w( 4,3) = FSIZE("PRODUCER")
arr_w( 4,4) = 0

arr_w( 5,1) = "QNT"
arr_w( 5,2) = "N"
arr_w( 5,3) = FSIZE("QNT")
arr_w( 5,4) = 0

arr_w( 6,1) = "QNT_M"
arr_w( 6,2) = "N"
arr_w( 6,3) = FSIZE("QNT")
arr_w( 6,4) = 0

arr_w( 7,1) = "PR_R"
arr_w( 7,2) = "N"
arr_w( 7,3) = FSIZE("PRICE_R")
arr_w( 7,4) = 2

arr_w( 8,1) = "PR_D"
arr_w( 8,2) = "N"
arr_w( 8,3) = FSIZE("PRICE_B")
arr_w( 8,4) = 4

arr_w( 9,1) = "PR_R_S"
arr_w( 9,2) = "N"
arr_w( 9,3) = FSIZE("PRICE_R")
arr_w( 9,4) = 2

arr_w(10,1) = "PR_D_S"
arr_w(10,2) = "N"
arr_w(10,3) = FSIZE("PRICE_B")
arr_w(10,4) = 4

arr_w(11,1) = "PR_R_M"
arr_w(11,2) = "N"
arr_w(11,3) = FSIZE("PRICE_R")
arr_w(11,4) = 2

arr_w(12,1) = "PR_D_M"
arr_w(12,2) = "N"
arr_w(12,3) = FSIZE("PRICE_B")
arr_w(12,4) = 3

SET EXCLUSIVE ON
IF File_O(log_path+"CMP_STO.DBF")
  DELETE FILE (log_path+"CMP_STO.DBF")
ENDIF

SELECT 0
CREATE DBF (log_path+"CMP_STO.DBF") FROM ARRAY arr_w
USE (log_path+"CMP_STO.DBF")

INDEX ON CODE TAG CODE
SET EXCLUSIVE OFF

SELECT STOCK
SCAN FOR QNT # 0 .OR. QNT_SAVE # 0 .OR. QNT_REAL # 0 .OR. QNT_REZ # 0

  skp = ALLTRIM(PREFIX+" "+NAME)
  @ 2, 3 SAY PADC(skp, WCOLS()-6)
  IF FSIZE("QNT_REZ", "STOCK") # 0
    tmpQnt = STOCK.QNT+STOCK.QNT_REZ+STOCK.QNT_SAVE+STOCK.QNT_REAL
  ELSE
    tmpQnt = STOCK.QNT+STOCK.QNT_SAVE+STOCK.QNT_REAL
  ENDIF

  SELECT CMP_STO
  APPEND BLANK
  REPLACE CODE   WITH STOCK.CODE,    ;
          NAME   WITH STOCK.NAME,    ;
          PREFIX WITH STOCK.PREFIX,  ;
          PRODUCER WITH STOCK.PRODUCER, ;
          QNT    WITH tmpQnt,        ;
          PR_D   WITH STOCK.PRICE_B, ;
          PR_R   WITH STOCK.PRICE_B_R, ;
          PR_D_S WITH STOCK.PRICE  , ;
          PR_R_S WITH STOCK.PRICE_R
          
  SELECT STOCK

ENDSCAN

SELECT STOCK
USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE
vl = .F.
IF SEEK(c_code)
  vl = VALUTA = "$"
  USE (base_path+"STOCK") ORDER TAG CODE
  SELECT 0
  USE (base_path+"RYN_L") ORDER TAG C_CODE
  SEEK STR(c_code,5)
  SCAN REST WHILE c_code = CUS_CODE FOR QNT # 0
    SELECT CMP_STO
    IF .NOT. SEEK(RYN_L.CODE)
      = SEEK(RYN_L.CODE, "STOCK")
      APPEND BLANK
      REPLACE CODE   WITH RYN_L.CODE,   ;
              NAME   WITH STOCK.NAME,   ;
              PREFIX WITH STOCK.PREFIX, ;
              QNT_M  WITH RYN_L.QNT,    ;
              PR_D_M WITH RYN_L.PRICE_D,;
              PR_R_M WITH RYN_L.PRICE_R
    ELSE
      REPLACE QNT_M  WITH RYN_L.QNT,    ;
              PR_D_M WITH RYN_L.PRICE_D,;
              PR_R_M WITH RYN_L.PRICE_R
    ENDIF
    SELECT RYN_L
  ENDSCAN

ENDIF

USE

PRIVATE ch_p, eq_p, eq_q

STORE .T. TO ch_p, eq_p, eq_q, mss

CLEAR WINDOW (win_name)

SELECT CMP_STO

IF Get_Flg()    && —โฎ ฏเฎขฅเ๏ฅฌ?
  *
  *   ”ฎเฌจเฎข ญจฅ ไ ฉซ  เ แๅฎฆคฅญจฉ
  *
  IF RECCOUNT() # 0
    DO View_Diff
    SELECT CMP_STO
  ELSE
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+" แๅฎฆคฅญจฉ ญฅโ!"
    mss(3) = ""
    DO Out_Mess WITH 5, "mss"
  ENDIF
ENDIF

USE

DELETE FILE (log_path+"CMP_STO.DBF")

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ข๋กฎเ  ฏฎซฅฉ แขฅเชจ.                       บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE Get_Flg

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

PRIVATE ex           &&
                     &&   ก๊๏ขซ๏ฅฌ จ ง ฏฎซญ๏ฅฌ ฏฎซ๏ กซ ญช 
ex   = 1             &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       &&   ขแ๏ชจฉ ฏฎฆ เญ๋ฉ แซใ็ ฉ!
DO Prp_Nav_2
DO D_Win_N WITH 10, 47, "’จฏ๋ เ แๅฎฆคฅญจฉ"

*------------------------------------------------------------------------
*      ขฎค ฏฎซฅฉ กซ ญช 
*

*             ’จฏ๋ เ แๅฎฆคฅญจฉ
*ฺฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฟ
*ณ                                           ณ
*ณ  [ ] ฎ ชฎซจ็ฅแโขใ                        ณ
*ณ  [ ] ฎ ๆฅญฅ                              ณ
*ณ  [ ] ฅแฎขฏ คฅญจฅ ๆฅญ ฏเฎค ฆจ จ ฏฎแโ ขชจ  ณ
*ณ                                           ณ
*ณ     < OK Ctrl-W > < โช ง โ์แ๏ Esc >      ณ
*ณ                                           ณ
*ภฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤู

@ 3, 4 GET eq_q PICTURE "@*C ฎ ชฎซจ็ฅแโขใ"
@ 4, 4 GET eq_p PICTURE "@*C ฎ ๆฅญฅ"
@ 5, 4 GET ch_p PICTURE "@*C ฅแฎขฏ คฅญจฅ ๆฅญ ฏเฎค ฆจ จ ฏฎแโ ขชจ"

@ 7, 7 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? โช ง โ์แ๏ Esc "

READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF ex = 1 .AND. (eq_q .OR. eq_p .OR. ch_p)
  RETURN .T.
ELSE
  RETURN .F.
ENDIF
*--------------------------------------------------------------------------

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ฏฅ็ โจ เ แๅฎฆคฅญจฉ.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE View_Diff
PRIVATE sv, arr_w

SELECT CMP_STO
*USE (log_path+"CMP_STO")

IF File_O(tmpo_path+"VIEW000.DBF") .OR. File_O(tmpo_path+"VIEW000.CDX")
  DELETE FILE (tmpo_path+"VIEW000.DBF")
  DELETE FILE (tmpo_path+"VIEW000.CDX")
ENDIF
SELECT CMP_STO
COPY STRUCTURE TO (tmpo_path+"VIEW000")

SELECT 0
USE (tmpo_path+"VIEW000") EXCLUSIVE

INDEX ON NAME+PREFIX TAG NAME

SELECT CMP_STO

SCAN
  *
  *   เฎขฅเ๏ฅฌ, ฅแโ์ ซจ เ แๅฎฆคฅญจ๏ จง ง ฏเฎ่ฅญญฎฃฎ แฏจแช 
  *
  sv = .F.
  IF eq_q .AND. QNT # QNT_M
    sv = .T.
  ENDIF
  IF eq_p
    IF vl
      sv = sv .OR. PR_D # PR_D_M
    ELSE
      sv = sv .OR. PR_R # PR_R_M
    ENDIF
  ENDIF
  IF ch_p
    IF vl
      sv = sv .OR. PR_D # PR_D_S
    ELSE
      sv = sv .OR. PR_R # PR_R_S
    ENDIF
  ENDIF
  IF .NOT. sv
    LOOP
  ENDIF
  SCATTER TO arr_w
  SELECT VIEW000
  APPEND FROM ARRAY arr_w
  SELECT CMP_STO

ENDSCAN

SELECT VIEW000
IF RECCOUN() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" แๅฎฆคฅญจฉ ญฅโ!"
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
ELSE
  DO P_Diff WITH eq_q, eq_p, ch_p, vl
  SELECT VIEW000
ENDIF

USE

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ                  เฎฃเ ฌฌ  ฏฅ็ โจ เ แๅฎฆคฅญจฉ.                        บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE P_Diff

PARAMETERS eq_q, eq_p, ch_p, vl
PRIVATE d_d, ffeed

SET ORDER TO TAG NAME

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, t_p, f_p, p_drctry
PRIVATE sel_rem, sb, str_w

= ADIR(str_w, tmpo_path+"VIEW000.DBF")
d_d = str_w(1, 3)
RELEASE str_w

*{Title}     ‘โเฎช  ง ฃฎซฎขช 
*{Underline} ฎค็ฅเชจข ญจฅ
*{Date}       โ 
*{Filial}    ”จซจ ซ
*{Page}      ‘โเ ญจๆ 

DIMENSION sb(5,2)
sb( 1,1) = "{Title}"
sb( 1,2) = ""

sb( 2,1) = "{Underline}"
sb( 2,2) = ""

sb( 3,1) = "{Date}"
sb( 3,2) = DTOC(d_d)

sb( 4,1) = "{Filial}"
sb( 4,2) = c_name

sb( 5,1) = "{Page}"
sb( 5,2) = ""

IF eq_q
  sb(1,2) = sb(1,2)+" ฎซ. ” ฎซ. "
  sb(2,2) = sb(2,2)+"--------------"
ENDIF

IF ch_p
  sb(1,2) = sb(1,2)+" –ฅญ  เ."
  sb(2,2) = sb(2,2)+"---------"
ENDIF
  
IF ch_p .OR. eq_p
  sb(1,2) = sb(1,2)+" –ฅญ  ฎแ"
  sb(2,2) = sb(2,2)+"---------"
ENDIF

IF eq_p
  sb(1,2) = sb(1,2)+" –ฅญ  ญ."
  sb(2,2) = sb(2,2)+"---------"
ENDIF

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "DIFF" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
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

USE
DO Ini_Prn WITH " แๅฎฆคฅญจ๏ ฎโ "+sb(3,2), p_ln, lft, n_cp, fnt, ffeed, p_drctry

PRIVATE n_page, l_page, fl_page
l_page = p_ln-ALEN(t_p)-ALEN(f_p)
SELECT VIEW000
i = 0
fl_page = 0
SCAN
  i = i+1
  IF i % l_page = 1
    fl_page = fl_page+1
    IF fl_page # 1
      FOR n = 1 TO ALEN(f_p)
        str_w = f_p(n)
        FOR j = 1 TO ALEN(sb,1)
          str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
        ENDFOR
        @ PROW()+1, 0 SAY str_w
      ENDFOR
      EJECT
    ENDIF
    sb(5,2) = ALLTRIM(STR(fl_page))
    FOR n = 1 TO ALEN(t_p)
      str_w = t_p(n)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF n = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ENDIF
  str_w = PREFIX+" "+NAME+" "+PRODUCER

  IF eq_q
    str_w = str_w+" "+TRANSFORM(QNT,"@Z 999999")+" "+TRANSFORM(QNT_M,"@Z 999999")
  ENDIF

  IF ch_p
    IF vl
      str_w = str_w+" "+TRANSFORM(PR_D_S,"@Z 99999.9999")
    ELSE
      str_w = str_w+" "+TRANSFORM(PR_R_S,"@Z 99999.99")
    ENDIF
  ENDIF
  
  IF ch_p .OR. eq_p
    IF vl
      str_w = str_w+" "+TRANSFORM(PR_D,"@Z 99999.9999")
    ELSE
      str_w = str_w+" "+TRANSFORM(PR_R,"@Z 99999.99")
    ENDIF
  ENDIF

  IF eq_p
    IF vl
      str_w = str_w+" "+TRANSFORM(PR_D_M,"@Z 99999.9999")
    ELSE
      str_w = str_w+" "+TRANSFORM(PR_R_M,"@Z 99999.99")
    ENDIF
  ENDIF
  @ PROW()+1, 0 SAY str_w

ENDSCAN

FOR n = 1 TO ALEN(f_p)
  str_w = f_p(n)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR
COPY TO (tmpo_path+"REC_TMP0")
DO Term_Prn WITH "", tmpo_path+"REC_TMP0"
DELETE FILE (tmpo_path+"REC_TMP0")
RETURN

*ษออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                        บ
*บ                      ฎฏจเฎข ญจฅ ง ฏจแจ ฏฎ จฌฅญจ.                      บ
*บ                                                                        บ
*ศออออออออออออออออออออออออออออออออออออออออออออออออออออ 20.05.97 17:49:46 อผ
PROCEDURE Copy_N
PARAMETER als
PRIVATE ALL
SCATTER MEMVAR
IF LEN(m.where) = 1
  m.where = RIGHT(STR(ASC(m.where)),3)
ENDIF
SELECT (als)
APPEND BLANK
GATHER MEMVAR

RETURN

*ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป
*บ                                                                       บ
*บ              เฎฃเ ฌฌ  ฏเฎแฌฎโเ  คฅไฅชโญ๋ๅ ฏฎงจๆจฉ.                   บ
*บ                                                                       บ
*ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ
PROCEDURE V_Err
PRIVATE ln, wd, tt, ft

DO Use_Dummy
SELECT ERR_SAL

DIMENSION tt(1), ft(1)
tt(1) = PADC(" ฎงจๆจ๏ ฤฤฤฤฤฤฤ", 39, "ฤ")+"ฤฤโ็ฅโฤฤเฎค."
ft(1) = PADC(" ซ๏ ฏเฎคฎซฆฅญจ๏ เ กฎโ๋ ญ ฆฌจโฅ Esc ", LEN(tt(1)), "ฤ")
ln = MIN(WROWS("")-8, RECCOUNT())
wd = LEN(tt(1))

ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13

DO Def_Win WITH "Err_M", "Err_E", "Err_I", ln, wd,  ;
                "‘ฏจแฎช ญฅชฎเเฅชโญ๋ๅ ฏฎงจๆจฉ", tt, ft

ACTIVATE WINDOW Err_E
BROWSE FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="", Q_REQ:H="", Q_SAL:H="", ;
              DUMMY.F FREEZE DUMMY.F              ;
              NOMENU NOMODIFY WINDOW Err_I IN WINDOW Err_E

RELEASE WINDOW Err_I
RELEASE WINDOW Err_E
RELEASE WINDOW Err_M
SELECT DUMMY
USE
SELECT ERR_SAL

RETURN

**************************************************************
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
