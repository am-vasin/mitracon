*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Inc_Sto      ��о�｀砒┴ ��むォ ��瓱�           10.06.98 11:15:04 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     �▲爲� ���キ���矣琺 � ����腮�.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Inc_Sto

PRIVATE pth_inc, sups_n, sups_c, n_sups, sup_point, p_refr, po_refr, o_refr
PRIVATE mss, own_cd, own_vl, sup_cd, sup_nm, arr_w, pr_dat, st_dat

*
*    �薀� ��瘠���...
*
pth_inc = ""
DO WHILE .T.
  pth_inc = Int_Dir(pth_inc, "�ぅ ����� ��痰�∫┴�?")
  IF EMPTY(pth_inc)
    RETURN
  ENDIF
  IF .NOT. File_O(pth_inc+"STO_S.DBF") .OR. ;
     .NOT. File_O(pth_inc+"STO_S.FPT") .OR. ;
     .NOT. File_O(pth_inc+"CLASSES.DBF") .OR. ;
     .NOT. File_O(pth_inc+"DIV_S.DBF") .OR. ;
     .NOT. File_O(pth_inc+"DEV_S.DBF")
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"� 礫������� ���� え爛�皰爬� �モ ������ ��痰�∫┴�!"
    mss(3) = CHR(0)+"  �皰 ぅ��ガ? [ �薀� � む磽�� �メ皀 ] [ �皖�щ��ガ瘴 ]"
    mss(4) = ""
    IF Out_Mess(7, "mss") = 1
      LOOP
    ENDIF
    RETURN
  ENDIF
  EXIT
ENDDO

USE (base_path+"CUSTOMER")
n_sups = 0
SCAN FOR SUP # " "
  n_sups = n_sups+1
  DIMENSION sups_n(n_sups), sups_c(n_sups)
  sups_n(n_sups) = B_NAME
  sups_c(n_sups) = CODE
ENDSCAN

LOCATE FOR SELF # " "
own_cd = CODE
USE (base_path+"PARMS")
own_vl = VALUTA

pr_dat = {}
IF File_O(pth_inc+"STO_S_T.DBF")
  SELECT 0
  USE (pth_inc+"STO_S_T")
  IF CUS_CODE # own_cd
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"��絎Ε, 艪� �� え瓷モ� �� ���� ���キ���矣��..."
    mss(3) = CHR(0)+"  蹍ぎ���ガ 艪キ┘? [ �モ! ] [ �� ]"
    mss(4) = ""
    IF Out_Mess(7, "mss") = 1
      USE
      SELECT PARMS
      USE
      RETURN
    ENDIF
  ENDIF
  own_vl = VALUTA
  pr_dat = DATE_P
  USE
  SELECT PARMS
  REPLACE VALUTA WITH own_vl
ENDIF
USE
= ADIR(arr_w, pth_inc+"STO_S.DBF")
st_dat = arr_w(1,3)
p_refr = .T.
po_refr = .F.
o_refr = .F.
sup_point = 1

IF .NOT. Inp_Mode()
  RETURN
ENDIF

*
*  ｡��←錺� DEVELOP
*
USE (base_path+"DEVELOP") ORDER TAG CODE
SELECT 0
USE (pth_inc+"DEV_S")
SCAN
  SELECT DEVELOP
  IF .NOT. SEEK(DEV_S.CODE)
    APPEND BLANK
    REPLACE CODE WITH DEV_S.CODE, DATE_ON WITH DATE()
  ENDIF
  REPLACE B_NAME WITH DEV_S.B_NAME, ;
          L_NAME WITH DEV_S.L_NAME
  IF FSIZE("LAT_NAME", "DEVELOP") # 0 .AND. FSIZE("LAT_NAME", "DEV_S") # 0
    REPLACE LAT_NAME WITH DEV_S.LAT_NAME
  ENDIF
  IF FSIZE("SPEC", "DEVELOP") # 0 .AND. FSIZE("SPEC", "DEV_S") # 0
    REPLACE SPEC WITH DEV_S.SPEC
  ENDIF
  IF FSIZE("FOLDER", "DEVELOP") # 0 .AND. FSIZE("FOLDER", "DEV_S") # 0
    REPLACE FOLDER WITH DEV_S.FOLDER
  ENDIF
  SELECT DEV_S
ENDSCAN
USE
SELECT DEVELOP

*
*  ｡��←錺� DIVISION
*
USE (base_path+"DIVISION") ORDER TAG CODE
SELECT 0
USE (pth_inc+"DIV_S")
SCAN
  SELECT DIVISION
  IF .NOT. SEEK(DIV_S.CODE)
    APPEND BLANK
    REPLACE CODE WITH DIV_S.CODE, DATE_ON WITH DATE()
  ENDIF
  REPLACE B_NAME WITH DIV_S.B_NAME, ;
          L_NAME WITH DIV_S.L_NAME
  SELECT DIV_S
ENDSCAN
USE
SELECT DIVISION

*
*  ｡��←錺� CLASSES
*
USE (base_path+"CLASSES") ORDER TAG CODE
SELECT 0
USE (pth_inc+"CLASSES") ALIAS CLASS_S
SCAN
  SELECT CLASSES
  IF .NOT. SEEK(CLASS_S.CODE)
    APPEND BLANK
    REPLACE CODE WITH CLASS_S.CODE
  ENDIF
  REPLACE NAME     WITH CLASS_S.NAME,     ;
          ENG_NAME WITH CLASS_S.ENG_NAME, ;
          DIV_CODE WITH CLASS_S.DIV_CODE, ;
          DIV_NAME WITH CLASS_S.DIV_NAME
  SELECT CLASS_S
ENDSCAN
USE
SELECT CLASSES
USE

DO Sto_Refr
DO Diffs

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                     ��皀���皋↓覃 □�� �����モ牀�.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 10.06.98 14:22:01 夕
PROCEDURE Inp_Mode

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                    敖陳陳陳陳陳陳陳朕          �
*�          ��痰�∫┴ �....:....!....:.�          �
*�                    青陳陳陳陳陳陳陳潰          �
*� ���キ���矣�� �� ��.��.����  �キ� �� ��.��.���� �
*�                                                �
*�  [ ] ｡��←キ┘ 罐�   [ ] � �. �. 甌＝癶キ�諷  �
*�                                                �
*�            [ ] ｡��※碎 �爛か�Ε�┘            �
*�                                                �
*�        < OK Ctrl-W > < �皖���碎瘴 Esc >        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 13, 52, "�痰���※皀 �����モ琺 瓣ム��"
@ 3, 12 SAY "��痰�∫┴"
@ 5,  3 SAY "���キ���矣�� �� "+DTOC(st_dat)
@ 5, 31 SAY "�キ� �� "+DTOC(pr_dat)
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@  2, 22 GET sup_point PICTURE "@^" FROM sups_n WHEN n_sups > 1
@  7,  4 GET p_refr PICTURE "@*C ｡��←キ┘ 罐�" VALID Set_PO()
@  7, 25 GET po_refr PICTURE "@*C � �. �. 甌＝癶キ�諷" WHEN p_refr
@  9, 14 GET o_refr PICTURE "@*C ｡��※碎 �爛か�Ε�┘"
@ 11, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

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

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                        ｡��←キ┘ ���キ���矣琺.                        �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 11.06.98 15:42:47 夕
PROCEDURE Sto_Refr

PRIVATE pr_r, pr_d, pr_o_r, pr_o_d, c_sup, is_off, cnt, k_pr, is_new, old_pr
PRIVATE tmpQnt, tmpFunc, tmpImage, tmpDSheet
c_sup = sups_c(sup_point)
IF p_refr
  SELECT 0
  DELETE FILE (base_path+"NEW_PRC.DBF")
  DELETE FILE (base_path+"NEW_PRC.CDX")
  CREATE DBF (base_path+"NEW_PRC")  ;
     ( CODE N(7), OLD_PRC N(12,4))
  INDEX ON CODE TAG CODE
  USE (base_path+"NEW_PRC")
ENDIF

SELECT 0
DELETE FILE (log_path+"DIFFS.DBF")
DELETE FILE (log_path+"DIFFS.CDX")

CREATE DBF (log_path+"DIFFS")  ;
  ( CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    PRICE    N(10,4),;
    QNT_O    N( 6), ;
    QNT_S    N( 6)  )

SELECT 0
USE (base_path+"STO_LOG")
SELECT 0
USE (path_comm+"CORPUS") ORDER TAG CODE
SELECT 0
USE (path_comm+"SUBJECT") ORDER TAG STO_CODE
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE
k_pr = IIF(Mnt_Size("PRICE_R") = 0, 1000, 1)

is_off = .F.
IF File_O(base_path+"OFFER.DBF") .AND. o_refr
  is_off = .T.
  SELECT 0
  USE (base_path+"OFFER") ORDER TAG CODE ALIAS OFFER_C
  SET FILTER TO SUP_CODE = c_sup

  SELECT 0
  USE (base_path+"OFFER") ORDER TAG NAME ALIAS OFFER_N AGAIN
  SET FILTER TO SUP_CODE = c_sup
ENDIF

SELECT 0
USE (pth_inc+"STO_S")
k_pr = k_pr/IIF(Mnt_Size("PRICE_R") = 0, 1000, 1)

DO Wt_Mess WITH "｡��←キ┘ ���キ���矣琺..."
cnt = 0
SCAN
  IF cnt % 100 = 0
    @ WROWS()-1 , 0 SAY PADC(ALLTRIM(STR(cnt)),WCOLS())
  ENDIF
  cnt = cnt+1
  *
  *   ���� �爛か�Ε�┤, メ�� �� メ碎...
  *
  IF is_off
    IF SEEK(CODE,"OFFER_C")
      SELECT OFFER_C
    ELSE
      SELECT OFFER_N
      IF .NOT. SEEK(NAME+PREFIX+PRODUCER,"OFFER_N")
        APPEND BLANK
      ENDIF
    ENDIF
    IF STO_S.QNT_STORE > 0
      tmpQnt = MIN(STO_S.QNT_STORE, 999999)
    ELSE
      tmpQnt = MAX(STO_S.QNT_STORE, -99999)
    ENDIF
    REPLACE SUP_CODE WITH c_sup,   ;
            CODE     WITH STO_S.CODE, ;
            PREFIX   WITH STO_S.PREFIX, ;
            NAME     WITH STO_S.NAME, ;
            PRODUCER WITH STO_S.PRODUCER, ;
            PRO_CODE WITH STO_S.PRO_CODE, ;
            PRICE_R  WITH STO_S.PRICE_R*k_pr, ;
            PRICE_D  WITH STO_S.PRICE, ;
            SUP_QNT  WITH tmpQnt,      ;
            DATE_INF WITH DATE(),      ;
            DATE_ON  WITH STO_S.DATE_ON
  ENDIF
  *
  *   ��き�←錺� STOCK
  *
  SELECT STOCK
  IF .NOT. SEEK(STO_S.CODE)    && �モ � ���キ���矣爛 筥�����...
*    IF STO_S.QNT # 0           &&       �� �� �━ 腮甄�矚�...
      APPEND BLANK             &&          �ゃ碎! 鉑碎 Л�モ!
*    ELSE
*      SELECT STO_S
*      LOOP
*    ENDIF
  ENDIF
  pr_r = PRICE_B_R
  pr_d = PRICE_B
  pr_o_r = PRICE_R
  pr_o_d = PRICE
  is_new = .F.
  IF p_refr                   && ��皋� �´�※碎 罐��
    IF STO_S.PRICE_B # 0 .AND. own_vl = "$" .OR. ;
       STO_S.PRICE_B_R # 0 .AND. own_vl # "$"
      IF own_vl = "$"           && �キ� � ぎ������...
        pr_d = STO_S.PRICE_B
        pr_r = Clc_Rub(pr_d)
        is_new = PRICE_B # pr_d
        old_pr = PRICE_B
      ELSE
        pr_d = STO_S.PRICE_B
        pr_r = STO_S.PRICE_B_R*k_pr
        is_new = PRICE_B_R # pr_r
        old_pr = PRICE_B_R
      ENDIF
      IF po_refr                && � ��Ε ����!
	    pr_o_r = pr_r
	    pr_o_d = pr_d
	  ENDIF
	ELSE		&& �砒モ�諷 罐� �モ, ��痰�※� 絎碎 皰, 艪� メ碎...
	  IF QNT_REP = 0
        IF own_vl = "$"           && �キ� � ぎ������...
          pr_d = STO_S.PRICE
          pr_r = Clc_Rub(pr_d)
          is_new = PRICE_B # pr_d
          old_pr = PRICE_B
        ELSE
          pr_d = STO_S.PRICE
          pr_r = STO_S.PRICE_R*k_pr
          is_new = PRICE_B_R # pr_r
          old_pr = PRICE_B_R
        ENDIF
      ENDIF
	ENDIF
  ENDIF

  IF STO_S.PREFIX   # STOCK.PREFIX .OR. ;
     STO_S.NAME     # STOCK.NAME .OR.   ;
     STO_S.PRO_CODE # STOCK.PRO_CODE
     SELECT STO_LOG
     APPEND BLANK
     REPLACE CODE     WITH STOCK.CODE,     ;
             OLD_PRF  WITH STOCK.PREFIX,   ;
             OLD_NAM  WITH STOCK.NAME,     ;
             OLD_PRO  WITH STOCK.PRO_CODE, ;
             NEW_PRF  WITH STO_S.PREFIX,   ;
             NEW_NAM  WITH STO_S.NAME,     ;
             NEW_PRO  WITH STO_S.PRO_CODE, ;
             DAT_CORR WITH DATE(),         ;
             WHERE_C  WITH CHR(255) 
  ENDIF
  SELECT STOCK
  IF STO_S.QNT_STORE > 0
    tmpQnt = MIN(STO_S.QNT_STORE, 999999)
  ELSE
    tmpQnt = MAX(STO_S.QNT_STORE, -99999)
  ENDIF
  REPLACE CODE      WITH STO_S.CODE,     ;
          PREFIX    WITH STO_S.PREFIX,   ;
          NAME      WITH STO_S.NAME,     ;
          PRODUCER  WITH STO_S.PRODUCER, ;
          PRO_CODE  WITH STO_S.PRO_CODE, ;
          DIV_CODE  WITH STO_S.DIV_CODE, ;
          CLASS     WITH STO_S.CLASS,    ;
          PRICE_B   WITH pr_d,           ;
          PRICE_B_R WITH pr_r,           ;
          PRICE     WITH pr_o_d,         ;
          PRICE_R   WITH pr_o_r,         ;
          QNT_STORE WITH tmpQnt,         ;
          STOP_PUR  WITH IIF(FSIZE("STOP_PUR","STO_S") = 0,STOP_PUR,STO_S.STOP_PUR),;
          DATE_OFF  WITH IIF(QNT = 0      .AND. ;
                             QNT_SAVE = 0 .AND. ;
                             QNT_REZ  = 0 .AND. ;
                             QNT_SPEC = 0 .AND. ;
                             QNT_REAL = 0, STO_S.DATE_OFF, {})
  IF FSIZE("DATE_ON", "STO_S") # 0
    REPLACE DATE_ON WITH STO_S.DATE_ON
  ENDIF
  IF EMPTY(DATE_ON)
    REPLACE DATE_ON WITH DATE()
  ENDIF
  IF is_new
    SELECT NEW_PRC
    APPEND BLANK
    REPLACE CODE WITH STO_S.CODE, OLD_PRC WITH old_pr
  ENDIF
  *
  *    ��痳�Δキ��
  *
  IF STO_S.QNT # 0
    SELECT DIFFS
    APPEND BLANK
    REPLACE CODE     WITH STO_S.CODE, ;
            PREFIX   WITH STO_S.PREFIX, ;
            NAME     WITH STO_S.NAME, ;
            PRODUCER WITH STO_S.PRODUCER, ;
            PRICE    WITH STO_S.PRICE_B, ;
            QNT_S    WITH STO_S.QNT
  ENDIF
  SELECT SUBJECT
  IF .NOT. SEEK(STO_S.CODE)
    APPEND BLANK
    REPLACE STO_CODE  WITH STO_S.CODE
  ENDIF
  m.tmpDSheet = DATASHEET
  IF TYPE("STO_S.DATASHEET")="C"
    m.tmpDSheet = STO_S.DATASHEET
  ENDIF
  m.tmpImage = IMAGE
  IF TYPE("STO_S.IMAGE")="C"
    m.tmpImage = STO_S.IMAGE
  ENDIF
  
  m.tmpFunc = FUNCTION
  IF TYPE("STO_S.FUNC")="M"
    m.tmpFunc = STO_S.FUNC
  ELSE
    IF TYPE("STO_S.FUNCTION")="C"
      m.tmpFunc = STO_S.FUNCTION
    ENDIF
  ENDIF
  REPLACE PREFIX    WITH STO_S.PREFIX,   ;
          NAME      WITH STO_S.NAME,     ;
          PRODUCER  WITH STO_S.PRODUCER, ;
          COR_CODE  WITH IIF(TYPE("STO_S.COR_CODE")="N", STO_S.COR_CODE, 0), ;
          DATASHEET WITH m.tmpDSheet,                                        ;
          FUNCTION  WITH m.tmpFunc,                                          ;
          IMAGE     WITH m.tmpImage,                                         ;
          CORPUS    WITH IIF(TYPE("STO_S.CORPUS")="C", STO_S.CORPUS, ""),    ;
          AUTHOR    WITH IIF(TYPE("STO_S.AUTHOR")="C", STO_S.AUTHOR, ""),    ;
          SEGMENT   WITH IIF(TYPE("STO_S.SEGMENT")="C", STO_S.SEGMENT, "")
  IF TYPE("STO_S.COR_CODE") = "N"		&& ��牆竅
    SELECT CORPUS
    IF .NOT. SEEK(STO_S.COR_CODE, "CORPUS")
      APPEND BLANK
      REPLACE CODE   WITH IIF(TYPE("STO_S.COR_CODE")="N", STO_S.COR_CODE, 0)
    ENDIF
      REPLACE NAME   WITH IIF(TYPE("STO_S.CORPUS")="C", STO_S.CORPUS, NAME),     ;
              WEIGHT WITH IIF(TYPE("STO_S.WEIGHT")="N", STO_S.WEIGHT, WEIGHT)
  ENDIF
  SELECT STO_S
ENDSCAN
USE
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

DO Wt_Mess
DO Wt_Mess WITH "��ぅ���罔�..."
SELECT DIFFS
INDEX ON CODE TAG CODE
DO Wt_Mess
DO Wt_Mess WITH "�▲爲� ����腮�..."
SELECT 0
USE (base_path+"ACT_S") ORDER TAG DOC_NUM
IF FSIZE("REST_REP") # 0
  SELECT 0
  USE (base_path+"ACT_S_T") ORDER TAG SUP_CODE
  IF sale_mode = "Mitracon"
    GO TOP
  ELSE
    SEEK c_sup
  ENDIF
  SCAN REST WHILE IIF(sale_mode = "Mitracon", c_sup <= 99999999, c_sup = SUP_CODE)  && 踸ガ�襯 ��硅 ��痰�∫┴�...
    SELECT ACT_S
    SEEK ACT_S_T.FIRM+LEFT(DTOS(ACT_S_T.DOC_DATE),4)+ACT_S_T.DOC_NUM
    SCAN REST WHILE ACT_S_T.FIRM = FIRM .AND.     ;
                    DOC_DATE = ACT_S_T.DOC_DATE .AND. ;
                    DOC_NUM  = ACT_S_T.DOC_NUM    ;
              FOR QNT_REP # 0
      SELECT DIFFS
      IF .NOT. SEEK(ACT_S.CODE)
        = SEEK(ACT_S.CODE, "STOCK")
        APPEND BLANK
        REPLACE CODE     WITH STOCK.CODE,   ;
                PREFIX   WITH STOCK.PREFIX, ;
                NAME     WITH STOCK.NAME,   ;
                PRODUCER WITH STOCK.PRODUCER
      ENDIF
      REPLACE QNT_O WITH QNT_O+ACT_S.QNT_REP
      SELECT ACT_S
    ENDSCAN
    SELECT ACT_S_T
  ENDSCAN

  USE
  SELECT ACT_S
  USE
ELSE
  USE
  SELECT STOCK
  SCAN FOR QNT_REP # 0
    SELECT DIFFS
    IF .NOT. SEEK(STOCK.CODE)
      APPEND BLANK
      REPLACE CODE     WITH STOCK.CODE,   ;
              PREFIX   WITH STOCK.PREFIX, ;
              NAME     WITH STOCK.NAME,   ;
              PRODUCER WITH STOCK.PRODUCER
    ENDIF
    REPLACE QNT_O WITH STOCK.QNT_REP
    SELECT STOCK
  ENDSCAN
ENDIF

SELECT STOCK
USE
SELECT STO_LOG
USE
SELECT DIFFS
INDEX ON NAME+PREFIX+PRODUCER TAG NAME ADDITIVE
USE
DO Wt_Mess

RETURN
