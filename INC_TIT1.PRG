*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Inc_Tit1     ��о�｀砒┴ ��むォ ��瓱�           06.05.97 09:39:47 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 ��爼�牀���┘ ����皰→� �爬ガ���� ����.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Inc_Tit1

*
*   ��� �牀������ ぎ�Ν� °碎 ��爛ぅ�キ� �ムガキ�襯:
*      - prod_c   - ��瘁│ ��ぎ� 筥爼 �牀├〓え皀�ォ;
*      - prod_n   - ��瘁│ ━キ 筥爼 �牀├〓え皀�ォ;
*
PRIVATE f_c000, f_n000, b_c000, b_n000, b_sw000, f_c_old
PRIVATE shab1, shab2, tmp_D_Code, sw_doc, prc_type
shab1 = "abcdefghijklmnopqrstuvwxyz�、�ぅ颱Ж�������珀矣粤肄蓍裨跋鈿"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"
PRIVATE sw_0001, sw_0002
sw_0001 = .F.
sw_0002 = .F.
tmp_D_Code = 0
sw_doc = .F.
prc_type = 1

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
f_c000 = def_firm
f_n000 = ""
f_c_old= def_firm

*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   踳く爬閧┘ 旭� 旭旭屋旭異旭旭屋 (F1)    �
*�   敖 [ ] ��痰�∫┴ 陳陳陳陳陳陳陳陳陳朕    �
*�   �         旭旭屋旭異旭旭屋          �    �
*�   青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�    �
*楽� [ ] �����き�� 陳陳陳陳陳陳陳陳陳陳陳陳陳審
*崖 ���ム 旭旭屋旭異旭旭屋旭異 �� 旭旭屋旭異 崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�   敖 �キ� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕   �
*�   � ( ) �モ    ( ) �磧��   ( ) �����琺 �   �
*�   青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰   �
*楽� 踸�ョ��┘ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖旭旭屋旭異旭旭屋旭異旭旭屋旭異旭旭屋旭異旭崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�      < OK Ctrl-W > < �皖���碎瘴 Esc >      �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*
IF RECCOUNT("TMP_INC") # 0
  SELECT TMP_INC
  ZAP
  SELECT INC_TIT
ENDIF
IF RECCOUNT("TMP_NEW") # 0
  SELECT TMP_NEW
  ZAP
  SELECT INC_TIT
ENDIF

PRIVATE ex, arr_w, tit_rec, code_w, rc_sav, sup_sw, tmpGTD, buy_ryn, is_file
                     &&   ｡褊←錺� � ������錺� ���� ゛����
ex   = 1             &&
sup_sw = .F.
code_w = 0
tmpGTD = .T.
buy_ryn = 1
is_file = 1
*------------------------------------------------------------------------
*
*   ��紜�硅��ガ ����瘡 ������→�
*
rc_sav = RECNO()
SCATTER TO arr_w BLANK
LOCATE FOR EMPTY(SUP_CODE) .AND. EMPTY(LOCKED)
IF FOUND()
  GATHER FROM arr_w
ELSE
  APPEND BLANK
ENDIF
REPLACE SUP_NAME WITH "",        ;
        FILE_TYPE WITH 1,        ;
        LOCKED WITH arm,         ;
        BUY WITH IIF(sale_mode = "Mitracon", 2, 1),  ;
        GTD_NUM WITH "",         ;
        COUNTRY WITH "",         ;
        GTD_TYPE WITH 1
tit_rec = RECNO()

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 17, 48, "������〓� ����皰→� �爬ガ���� ����"
DO Sun_Bord WITH 3, 5, 5, 41, " [ ] ��痰�∫┴ "
DO Sun_Bord WITH 6, 2, 8, 45, " [ ] �����き�� "
DO Sun_Bord WITH 9, 6, 11, 42, " �キ� "
DO Sun_Bord WITH 12, 2, 14, 45, " 踸�ョ��┘ "
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

DO Get_Firm WITH 2, 5, enab_firm

@  3,  8 GET sup_sw PICTURE "@*C ��痰�∫┴" VALID Get_Supl()
@  4, 15 GET INC_TIT.SUP_NAME WHEN .F.
@  6,  5 GET sw_doc PICTURE "@*C �����き��" VALID G_Inv()
&&@  8, 15 GET is_file PICTURE "@*RH �モ  ;�痰�" VALID Clr_Inv()
&&@ 10, 10 GET tmpGtd PICTURE "@*C ��� ぎ痰祚�� か� □���"
&&      VALID Clr_F() WHEN INC_TIT.FILE_TYPE = 1 .AND. .F. && �┴����!
@  7,  4 SAY "���ム" GET INC_TIT.INC_DOC WHEN .NOT. sw_doc
@  7, 31 SAY "��" GET INC_TIT.INC_DATE WHEN  .NOT. sw_doc
&&@ 11, 10 GET buy_ryn PICTURE "@*RH ���祚��  ;����├�罔�" WHEN .NOT. sw_doc
@ 10, 7 GET m.prc_type PICTURE "@*RH �モ  ; �磧�� ; �����琺" WHEN .NOT. sw_doc
@ 13, 3 GET INC_TIT.NOTE PICTURE "@S42"
@ 15,  8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
READ CYCLE VALID Get_Inv0()

REPLACE INC_TIT.FIRM WITH f_c000

SELECT STOCK
SET ORDER TO TAG CODE

SELECT INC_TIT

IF ex = 1
  DO CASE
  CASE m.prc_type = 1
    m.prc_type = ""
  CASE m.prc_type = 2
    m.prc_type = "�"
  CASE m.prc_type = 3
    m.prc_type = "$"
  ENDCASE
  PRIVATE sw
  REPLACE SUP_CODE   WITH code_w,                  ;
          FILE_TYPE  WITH is_file,                 ;
          GTD_TYPE   WITH IIF(tmpGTD, 1, 0),       ;
          IS_PRICE   WITH m.prc_type,              ;
          ENAB_PRICE WITH .NOT. EMPTY(m.prc_type), ;
          NDS_       WITH IIF(.NOT. m.prc_type $ " $", m.nds_flt, 0), ;
          BUY        WITH buy_ryn
*
*    ������錺� ����皰→� �����き��
  SELECT TMP_INC
  SCAN
    SCATTER TO arr_w
    SELECT INC_LIST
    GO TOP
    IF .NOT. BOF() .AND. .NOT. EOF() .AND. EMPTY(VAL(LIST_NO))
      GATHER FROM arr_w
    ELSE
      APPEND FROM ARRAY arr_w
    ENDIF
    SELECT TMP_INC
  ENDSCAN
  SELECT INC_LIST
  SET ORDER TO TAG L_CODE

*
*    ������錺� ����皰→� �メ���め��� ��Ж罔�
  SELECT TMP_NEW
  SCAN FOR .NOT. DELETED()
    SCATTER TO arr_w
    SELECT INC_NEW
    GO TOP
    IF .NOT. EMPTY(VAL(LIST_NO)) .OR. BOF() .OR. EOF()
      APPEND BLANK            &&
    ENDIF
    GATHER FROM arr_w
    SELECT TMP_NEW
  ENDSCAN

  SELECT INC_LIST
  GO TOP
  IF EOF() .OR. BOF() .OR. .NOT. EMPTY(VAL(LIST_NO))
    APPEND BLANK
  ENDIF
  REPLACE LIST_NO WITH STR(tit_rec,5)
  SELECT 0
  USE (log_path+"DUE_LOG")
  COPY STRU TO (tmp_path+"DL"+ALLTRIM(STR(tit_rec)))
  USE
  SELECT INC_TIT
  REPLACE LOCKED WITH "***"
ELSE
  BLANK
  IF rc_sav > 0 .AND. rc_sav < RECCOUNT()
    GO rc_sav
  ENDIF
ENDIF

*--------------------------------------------------------------------------
POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                               �┓ ���.                                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様� 14.02.2000 11:13:38 夕
PROCEDURE Clr_F

REPLACE INC_TIT.GTD_NUM WITH "", ;
        INC_TIT.COUNTRY WITH ""
SHOW GET INC_TIT.GTD_NUM
SHOW GET INC_TIT.COUNTRY

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           �襦�� ��痰�∫┴�.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.05.97 11:13:38 夕
PROCEDURE Get_Supl

IF .NOT. sup_sw
  RETURN .T.
ENDIF

sup_sw = .F.
PRIVATE tmpName1, tmpName2
tmpName1 = INC_TIT.SUP_NAME
tmpName2 = ""


DO Get_Sup WITH code_w, tmpName1, tmpName2

REPLACE SUP_NAME WITH tmpName1, SUP_CODE WITH code_w

SHOW GET sup_sw
SHOW GET INC_TIT.SUP_NAME

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      蹍������ 艪キ�� �����き��.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.05.97 15:57:27 夕
PROCEDURE Get_Inv0
PARAMETER md

*IF .NOT. md
  IF ex = 2
    RETURN .T.
  ENDIF

  IF EMPTY(code_w)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� �� 礫����� ��痰�∫┴�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF

  IF EMPTY(f_c000)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� �� 礫����� �爛く爬閧┘!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
*  RETURN .T.
*ENDIF

DO CASE
CASE INC_TIT.INC_CODE # 0 				   && CMP_DOCS!
  IF .NOT. Get_InvN()      && �� �����-皰 �爬腮�� �����き�� �� �牀腮����
    ACTIVATE WINDOW (win_name)
    REPLACE INC_TIT.INC_DOC   WITH " ", ;
            INC_TIT.INC_CODE  WITH 0,   ;
            INC_TIT.INC_DATE  WITH {},  ;
            INC_TIT.FILE_TYPE WITH 1
    tmp_D_Code = 0
    sw_doc = .F.
    m.prc_type = 1
    SHOW GET INC_TIT.INC_DOC
    SHOW GET INC_TIT.INC_DATE
    SHOW GET INC_TIT.FILE_TYPE
    SHOW GET sw_doc
    SHOW GET m.prc_type
    SELECT TMP_INC
    ZAP
    SELECT INC_TIT
    ex = 2
    RETURN .T.
  ENDIF
  SHOW GET INC_TIT.INC_DOC
  SHOW GET INC_TIT.INC_DATE
  RETURN .T.
CASE is_file = 1    && �モ ����� �����き��!
*  REPLACE INC_TIT.INC_DOC WITH " ",  INC_TIT.INC_DATE WITH {}
*  SHOW GET INC_TIT.INC_DOC
*  SHOW GET INC_TIT.INC_DATE
  SELECT TMP_INC
  ZAP
  SELECT INC_TIT
  RETURN .T.
CASE is_file = 2    && ���� � 筮爼�皀 �゜キ� � 筥������!
  IF .NOT. Get_Inv1()     && �� �����-皰 �爬腮�� �����き�� �� �牀腮����
    ACTIVATE WINDOW (win_name)
    REPLACE INC_TIT.INC_DOC   WITH " ", ;
            INC_TIT.INC_DATE  WITH {},  ;
            INC_TIT.FILE_TYPE WITH 1
    SHOW GET INC_TIT.INC_DOC
    SHOW GET INC_TIT.INC_DATE
    SHOW GET INC_TIT.FILE_TYPE
    SELECT TMP_INC
    ZAP
    SELECT INC_TIT
    ex = 2
    RETURN .T.
  ENDIF
  SHOW GET INC_TIT.INC_DOC
  SHOW GET INC_TIT.INC_DATE
  RETURN .T.
CASE INC_TIT.FILE_TYPE = 3    && ���� � 筮爼�皀 Sing!
  IF .NOT. Get_Inv2()     && �� �����-皰 �爬腮�� �����き�� �� �牀腮����
    ACTIVATE WINDOW (win_name)
    REPLACE INC_TIT.INC_DOC   WITH " ", ;
            INC_TIT.INC_DATE  WITH {},  ;
            INC_TIT.FILE_TYPE WITH 1
    SHOW GET INC_TIT.INC_DOC
    SHOW GET INC_TIT.INC_DATE
    SHOW GET INC_TIT.FILE_TYPE
    SELECT TMP_INC
    ZAP
    SELECT INC_TIT
    ex = 2
    RETURN .T.
  ENDIF
  SHOW GET INC_TIT.INC_DOC
  SHOW GET INC_TIT.INC_DATE
  RETURN .T.
CASE INC_TIT.FILE_TYPE = 4    && ���� � 筮爼�皀 ��!
  IF .NOT. Get_Inv3()     && �� �����-皰 �爬腮�� �����き�� �� �牀腮����
    ACTIVATE WINDOW (win_name)
    REPLACE INC_TIT.INC_DOC   WITH " ", ;
            INC_TIT.INC_DATE  WITH {},  ;
            INC_TIT.FILE_TYPE WITH 1
    SHOW GET INC_TIT.INC_DOC
    SHOW GET INC_TIT.INC_DATE
    SHOW GET INC_TIT.FILE_TYPE
    SELECT TMP_INC
    ZAP
    SELECT INC_TIT
    ex = 2
    RETURN .T.
  ENDIF
  SHOW GET INC_TIT.INC_DOC
  SHOW GET INC_TIT.INC_DATE
  RETURN .T.
ENDCASE

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�              �〓� �����き�� � 筮爼�皀 �゜キ� � 筥������.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.05.97 16:10:22 夕
PROCEDURE Get_Inv1

PRIVATE pth, mss, g_0, c_0, l_0, mon_type
l_0 = ""
mon_type = " "

pth = Int_Dir("", "�ぅ �薀� �����き竡")
IF EMPTY(pth)  && �皖�����瘡...
  RETURN .F.
ENDIF

IF .NOT. File_O(pth+"SAL_S_T.DBF") .OR. .NOT. File_O(pth+"SAL_S.DBF")
  IF .NOT. File_O(pth+"RYN_M_T.DBF") .OR. .NOT. File_O(pth+"RYN_M.DBF")  ;
     .OR. .NOT. File_O(pth+"RYN_M.FPT")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��������� ���� え爛�皰爬� �� 甌ぅ爨�� �����き諷!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ELSE
    SELECT 0
    USE (pth+"RYN_M_T.DBF")
    buy_ryn = 2
    SHOW GET buy_ryn
  ENDIF
ELSE
  SELECT 0
  USE (pth+"SAL_S_T.DBF") ALIAS RYN_M_T
  buy_ryn = 1
  SHOW GET buy_ryn
  mon_type = "$"
  IF FSIZE("CURRENCY") # 0
    mon_type = IIF("USD" $ UPPER(CURRENCY), "$", "�")
  ENDIF
ENDIF
PRIVATE tmp_pr, tmp_key, i, prdc, prdn
i = 0
SCAN FOR SUP_CODE = code_w
  i = i+1
  DIMENSION tmp_pr(i), tmp_key(i,5)
  tmp_pr(i)  = STR(DOC_NUM,10)+" "+DTOC(DOC_DATE)
  tmp_key(i,1) = DOC_NUM
  tmp_key(i,2) = DOC_DATE
  IF FSIZE("GTD_NUM") # 0
    tmp_key(i,3) = GTD_NUM
  ELSE
    tmp_key(i,3) = SPACE(20)
  ENDIF
  IF FSIZE("COUNTRY") # 0
    tmp_key(i,4) = COUNTRY
  ELSE
    tmp_key(i,4) = SPACE(16)
  ENDIF
  IF FSIZE("GTD_LINE") # 0
    tmp_key(i,5) = GTD_LINE
  ELSE
    tmp_key(i,5) = ""
  ENDIF
ENDSCAN

USE
SELECT INC_TIT
IF i = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"� ����� �モ �� �き�� �����き�� �� 礫�������� ��痰�∫┴�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

*IF i > 1
  i = Ch_Inv()       && �襦���ガ �����き竡 ├ 甎�瓷�
*ENDIF

IF i = 0
  is_file = 1
  SHOW GET is_file
  RETURN .F.
ENDIF

SELECT TMP_INC
ZAP
SELECT 0
USE (base_path+"DEVELOP") ORDER TAG CODE ALIAS DEV_0607 AGAIN
SELECT 0
IF File_O(pth+"SAL_S.DBF") 
  USE (pth+"SAL_S.DBF") ALIAS RYN_M
ELSE
  USE (pth+"RYN_M.DBF")
ENDIF

*REPLACE INC_TIT.GTD_TYPE WITH IIF(EMPTY(tmp_key(i,3)), 3, 2),  ;
*        INC_TIT.GTD_NUM  WITH tmp_key(i,3),  ;
*        INC_TIT.COUNTRY  WITH tmp_key(i,4)
SCAN FOR DOC_NUM = tmp_key(i,1) .AND. DOC_DATE = tmp_key(i,2)
  prdc = PRO_CODE
  = SEEK(prdc, "DEV_0607")
  prdn = LEFT(DEV_0607.B_NAME,8)
  = SEEK(CODE, "STOCK")
  IF .NOT. FOUND("STOCK")   && ����� ��Ж罔� - ぎ��←錺�!
    SELECT STOCK
    APPEND BLANK
    REPLACE CODE      WITH RYN_M.CODE,    ;
            PRO_CODE  WITH RYN_M.PRO_CODE,;
            PREFIX    WITH RYN_M.PREFIX,  ;
            NAME      WITH RYN_M.NAME,    ;
            PRODUCER  WITH prdn,          ;
            PREFIX1   WITH SeekName(RYN_M.PREFIX),;
            NAME1     WITH SeekName(RYN_M.NAME),  ;
            PRODUCER1 WITH SeekName(prdn),        ;
            NUM_PART  WITH RYN_M.NUM_PART,;
            DIV_CODE  WITH RYN_M.DIV_CODE,;
            QNT_ROUND WITH 1,             ;
            DATE_ON   WITH RYN_M.DATE_ON
  ENDIF
  IF EMPTY(DATE_ON)
    REPLACE DATE_ON WITH DATE()
  ENDIF
  IF .NOT. EMPTY(STOCK.DATE_OFF)
    REPLACE STOCK.DATE_OFF WITH {}
  ENDIF
  g_0 = RYN_M.GTD_NUM
  c_0 = RYN_M.COUNTRY
  IF FSIZE("GTD_LINE", "RYN_M") # 0
    l_0 = RYN_M.GTD_LINE
  ENDIF
  SELECT TMP_INC
  APPEND BLANK
  REPLACE CODE      WITH STOCK.CODE,    ;
          PREFIX    WITH STOCK.PREFIX,  ;
          NAME      WITH STOCK.NAME,    ;
          PRODUCER  WITH STOCK.PRODUCER,;
          QNT_INV   WITH RYN_M.QNT,     ;
          LIST_NO   WITH STR(tit_rec,5),;
          GTD_NUM   WITH g_0,           ;
          GTD_LINE  WITH l_0,           ;
          COUNTRY   WITH c_0
  IF TYPE("RYN_M.PRICE") = "N"
    REPLACE PRICE_0 WITH RYN_M.PRICE, ;
            PRICE   WITH RYN_M.PRICE
  ENDIF
  SELECT RYN_M
ENDSCAN
USE
SELECT DEV_0607
USE

SELECT INC_TIT
REPLACE INC_DOC  WITH STR(tmp_key(i,1),10),  ;
        INC_DATE WITH tmp_key(i,2),          ;
        IS_PRICE WITH mon_type

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                �〓� �����き�� ├ ����� � 筮爼�皀 Sing.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 13.05.97 17:56:00 夕
PROCEDURE Get_Inv2
PRIVATE pth, mss, sel_sav, sw, tmp_pr, wd, n, n_d

pth = Int_File("", "DBF", "�ぅ �薀� �����き竡")
IF EMPTY(pth)  && �皖�����瘡...
  RETURN .F.
ENDIF
*
*   蹍▲瑙ガ 甌�癶モ痰※� 痰珮�矣琺
*
sel_sav = SELECT()
SELECT 0
USE (pth) ALIAS SING
sw = .T.
IF FSIZE("STO_CODE") < 5 .OR. TYPE("STO_CODE") # "N"
  sw = .F.
ENDIF
IF FSIZE("PREFIX") # 4 .OR. TYPE("PREFIX") # "C"
  sw = .F.
ENDIF
IF FSIZE("NAME") # 25 .OR. TYPE("NAME") # "C"
  sw = .F.
ENDIF
IF FSIZE("PRODUCER") # 8 .OR. TYPE("PRODUCER") # "C"
  sw = .F.
ENDIF
IF FSIZE("QTY") < 3 .OR. TYPE("QTY") # "N"
  sw = .F.
ENDIF
IF FSIZE("BOXNO") < 3 .OR. .NOT. TYPE("BOXNO") $ "NC"
  sw = .F.
ENDIF
IF .NOT. sw
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰 �� ���� � �����き覓�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_sav)
  RETURN .F.
ENDIF

*
*      ��爼�珮ガ 甎�甌� �����き諷 � �襦���ガ �き� ├ ���...
*
wd = FSIZE("BOXNO")

SELECT BOXNO FROM (pth) INTO CURSOR TMP_C GROUP BY BOXNO
SELECT TMP_C
n = RECCOUNT()
IF n = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���� �� 甌ぅ爨�� �� �き�� �����き��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT SING
  USE
  SELECT (sel_sav)
  RETURN .F.
ENDIF

DIMENSION tmp_pr(n)
n = 1
SCAN
  tmp_pr(n) = IIF(TYPE("BOXNO") = "N", STR(BOXNO,wd), BOXNO)
  n = n+1
ENDSCAN
IF n > 1
  n = Ch_Inv()
  IF n = 0  && �皖�����瘡...
    USE
    SELECT SING
    USE
    SELECT (sel_sav)
    RETURN .F.
  ENDIF
ENDIF
n_d = STR(n, FSIZE("INC_DOC", "INC_TIT"))
n   = IIF(TYPE("BOXNO") = "N", VAL(ALLTRIM(tmp_pr(n))), tmp_pr(n))
n_d = IIF(TYPE("N") = "N", STR(n, 10), n)
USE

*
*  ��爼�珮ガ 甎�瓷� � ����皰→�� �爬絎�� � ���諷 ��Ж罔�.
*
SELECT SING
SCAN FOR BOXNO = n
  sw = .F.                && 踸Л�� 瓷��め��� ��Ж罔�
  IF STO_CODE # 0         && �薀� �� ��ゃ
    sw = SEEK(STO_CODE, "STOCK")
  ENDIF
  IF .NOT. sw             && �� ��ゃ �� ��茫�, �牀＜ガ �瓷�碎 �� ━キ�...
    SELECT STOCK
    SET ORDER TO TAG NAME_P
    sw = SEEK(SING.NAME+SING.PREFIX+SING.PRODUCER)
    SET ORDER TO TAG CODE
  ENDIF
  IF sw         && ��Ж罔� ���ぅ�� � ���キ���矣爛!
    SELECT TMP_INC
    APPEND BLANK
    REPLACE CODE      WITH STOCK.CODE,    ;
            PREFIX    WITH STOCK.PREFIX,  ;
            NAME      WITH STOCK.NAME,    ;
            PRODUCER  WITH STOCK.PRODUCER,;
            QNT_INV   WITH SING.QTY,      ;
            LIST_NO   WITH STR(tit_rec,5)
  ELSE
    SELECT TMP_NEW
    APPEND BLANK
    REPLACE PREFIX    WITH SING.PREFIX,  ;
            NAME      WITH SING.NAME,    ;
            PRODUCER  WITH SING.PRODUCER,;
            QNT       WITH SING.QTY,     ;
            LIST_NO   WITH STR(tit_rec,5)
  ENDIF
  SELECT SING
ENDSCAN
USE

SELECT INC_TIT
REPLACE INC_DOC WITH n_d

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                       �襦�� �����き�� ├ 甎�瓷�.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.05.97 16:29:04 夕
PROCEDURE Ch_Inv
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

PRIVATE point, n_h, n_m, ex
*------------------------------------------------------------------------

n_h = ALEN(tmp_pr)
n_m = MIN(n_h, WROWS("")-12)
point = 1
PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH n_m+3, LEN(tmp_pr(1))+4, "���-硅"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

DO WHILE .T.
  point = MAX(point,1)
  point = MIN(point,n_h)
  @ 1, 1 MENU tmp_pr, n_h, n_m
  READ MENU TO point
  ex = READKEY() % 256
  IF ex = 15
    EXIT
  ENDIF
  IF ex = 12
    point = 0
    EXIT
  ENDIF
ENDDO

POP KEY
RELEASE WINDOW (win_name)
RETURN point
*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                    �腮痰�� ���ム� � ��硅 �����き��.                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 25.09.97 10:34:04 夕
PROCEDURE Clr_Inv

REPLACE INC_TIT.INC_DOC WITH "", INC_TIT.INC_DATE WITH {}
SHOW GET INC_TIT.INC_DOC
SHOW GET INC_TIT.INC_DATE
tmp_D_Code = 0
*IF is_file # 1
*  buy_ryn = 0
*ELSE
*  buy_ryn = 1
*ENDIF
*SHOW GET buy_ryn
*DO Get_Inv0 WITH .T.

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                      �〓� �����き�� � 筮爼�皀 ��.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 06.05.97 16:10:22 夕
PROCEDURE Get_Inv3

PRIVATE mss, sel_sav, g_0, c_0

IF .NOT. File_O(path_comm+"INVOICET.DBF") .OR. .NOT. File_O(path_comm+"INVOICED.CDX") .OR. ;
   .NOT. File_O(path_comm+"INVOICET.DBF") .OR. .NOT. File_O(path_comm+"INVOICED.CDX")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 磴�モ瘴 ���皋 ����� � �����き覓�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
sel_sav = SELECT()
SELECT 0
USE (path_comm+"INVOICET")
SET FILTER TO EMPTY(DATE_INC)

IF .NOT. Ch_Inv3()
  USE
  SELECT (sel_sav)
  RETURN .F.
ENDIF

REPLACE INC_TIT.INC_DOC  WITH INVOICET.EXP_NUM, ;
        INC_TIT.INC_DATE WITH INVOICET.EXP_DATE
IF FSIZE("GTD_NUM", "INVOICET") = 0
  REPLACE INC_TIT.GTD_TYPE WITH 3
ELSE
  REPLACE INC_TIT.GTD_TYPE WITH IIF(EMPTY(INVOICET.GTD_NUM),3, 2), ;
          INC_TIT.GTD_NUM  WITH INVOICET.GTD_NUM, ;
          INC_TIT.COUNTRY  WITH INVOICET.COUNTRY
ENDIF  

USE (path_comm+"INVOICED") ORDER TAG EXP_NUM
SELECT TMP_INC
ZAP
SELECT INVOICED
SEEK LEFT(DTOS(INC_TIT.INC_DATE),4)+INC_TIT.INC_DOC

g_0 = SPACE(20)
c_0 = SPACE(16)
IF INC_TIT.GTD_TYPE = 2
  g_0 = INC_TIT.GTD_NUM
  c_0 = INC_TIT.COUNTRY
ENDIF

SCAN REST WHILE EXP_NUM = INC_TIT.INC_DOC .AND. EXP_DATE = INC_TIT.INC_DATE
    
  IF INC_TIT.GTD_TYPE = 3 .AND. FSIZE("GTD_NUM") # 0
    g_0 = GTD_NUM
    c_0 = COUNTRY
  ENDIF
  sw = .F.                && 踸Л�� 瓷��め��� ��Ж罔�
  IF STO_CODE # 0         && �薀� �� ��ゃ
    sw = SEEK(STO_CODE, "STOCK")
  ENDIF
  IF .NOT. sw             && �� ��ゃ �� ��茫�, �牀＜ガ �瓷�碎 �� ━キ�...
    SELECT STOCK
    SET ORDER TO TAG NAME_P
    sw = SEEK(INVOICED.NAME+INVOICED.PREFIX+INVOICED.PRODUCER)
    SET ORDER TO TAG CODE
  ENDIF
  IF sw         && ��Ж罔� ���ぅ�� � ���キ���矣爛!
    SELECT TMP_INC
    APPEND BLANK
    REPLACE CODE      WITH STOCK.CODE,    ;
            IDP       WITH INVOICED.PURSYS+STR(INVOICED.IDP), ;
            PREFIX    WITH STOCK.PREFIX,  ;
            NAME      WITH STOCK.NAME,    ;
            PRODUCER  WITH STOCK.PRODUCER,;
            QNT_INV   WITH INVOICED.QTY,  ;
            LIST_NO   WITH STR(tit_rec,5),;
            GTD_NUM   WITH g_0,           ;
            COUNTRY   WITH c_0,           ;
            INV_REF   WITH RECNO("INVOICED")
  ELSE
    SELECT TMP_NEW
    APPEND BLANK
    REPLACE PREFIX    WITH INVOICED.PREFIX,  ;
            NAME      WITH INVOICED.NAME,    ;
            PRODUCER  WITH INVOICED.PRODUCER,;
            IDP       WITH INVOICED.PURSYS+STR(INVOICED.IDP), ;
            QNT       WITH INVOICED.QTY,     ;
            LIST_NO   WITH STR(tit_rec,5),   ;
            GTD_NUM   WITH g_0,              ;
            COUNTRY   WITH c_0,              ;
            INV_REF   WITH RECNO("INVOICED")
  ENDIF
  SELECT INVOICED

ENDSCAN
USE

SELECT (sel_sav)

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Inv3      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�               �襦�� �����き�� 矗モ譽�� 皋�� (筮爼�� ��).               �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 02.11.1998 陳�
PROCEDURE Ch_Inv3

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
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
DO Use_Dummy
SELECT INVOICET
GO BOTTOM

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, ret_w
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = IIF(SET("CENTURY") = "ON", 21,  19)

DO D_Wins WITH ln, wd, "���甌� �����き諷", 0, 0
what_do = "List"

ret_w = .F.
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS EXP_NUM:H="", EXP_DATE:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && 蹍甃�矗 甎�瓷�

    ret_w = .T.
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*

RETURN ret_w

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_GTD      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                               �襦�� ���.                               �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 14.02.2000 陳�
PROCEDURE Get_GTD

PRIVATE tmpVal, point
IF sw_0001
  sw_0001 = .F.
  SHOW GET sw_0001
  tmpVal = Ch_GTD(INC_TIT.GTD_NUM)
  point = AT(CHR(13), tmpVal)
  IF point = 0
    REPLACE INC_TIT.GTD_NUM WITH tmpVal, INC_TIT.COUNTRY WITH ""
  ELSE
    REPLACE INC_TIT.GTD_NUM WITH LEFT(tmpVal, point-1), INC_TIT.COUNTRY WITH SUBSTR(tmpVal, point+1)
  ENDIF
  SHOW GET INC_TIT.GTD_NUM
  SHOW GET INC_TIT.COUNTRY
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� G_Inv       ��о�｀砒┴ ��むォ ��瓱�                       �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �襦��/�腮痰�� �����き��                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 23.11.2010 陳�
PROCEDURE G_Inv

IF sw_doc
  tmp_D_Code = Get_Inv()
  IF tmp_D_Code = 0
    sw_doc = .F.
    SHOW GET sw_doc
  ELSE
    USE base_path+"CMP_DOCS" SHARED AGAIN ORDER TAG DOC_CODE IN 0
    IF .NOT. SEEK(tmp_D_Code, "CMP_DOCS")
      USE IN CMP_DOCS
      PRIVATE mss
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"�����き� �竅��!"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      sw_doc = .F.
      SHOW GET sw_doc
    ELSE
      m.prc_type = IIF(CMP_DOCS.PRICE_TYPE = "$", 3, 2)
      USE base_path+"CMP_INVS" SHARED AGAIN ORDER TAG DOC_CODE IN CMP_DOCS
      = SEEK(tmp_D_Code, "CMP_INVS")
      REPLACE INC_TIT.INC_DOC  WITH ALLTRIM(CMP_INVS.DOC_NUM), ;
              INC_TIT.INC_DATE WITH CMP_INVS.DOC_DATE,         ;
              INC_TIT.INC_CODE WITH CMP_INVS.DOC_CODE
      USE IN CMP_INVS
    ENDIF
  ENDIF
ELSE
  DO Clr_Inv
  REPLACE INC_TIT.INC_DOC  WITH "",       ;
          INC_TIT.INC_DATE WITH CTOD(""), ;
          INC_TIT.INC_CODE WITH 0,        ;
          INC_TIT.IS_PRICE WITH ""
ENDIF
SHOW GET INC_TIT.INC_DOC
SHOW GET INC_TIT.INC_DATE
SHOW GET prc_type
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                           �����き�� �������.                           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 23.11.2011 夕
PROCEDURE Get_InvN

PRIVATE pth, mss, g_0, c_0, l_0, mon_type, pr_code, s_sav

m.s_sav = SELECT()
SELECT TMP_INC
ZAP
SELECT 0
USE (base_path+"CMP_DOCS") ORDER TAG DOC_CODE ALIAS RYN_M AGAIN

SEEK INC_TIT.INC_CODE
SCAN REST WHILE DOC_CODE = INC_TIT.INC_CODE
  
  IF .NOT. SEEK(CODE, "STOCK")   
    LOOP
  ENDIF
  IF .NOT. EMPTY(STOCK.DATE_OFF)
    REPLACE STOCK.DATE_OFF WITH {}
  ENDIF
  SELECT TMP_INC
  APPEND BLANK
  REPLACE CODE      WITH STOCK.CODE,      ;
          PREFIX    WITH STOCK.PREFIX,    ;
          NAME      WITH STOCK.NAME,      ;
          PRODUCER  WITH STOCK.PRODUCER,  ;
          QNT_INV   WITH RYN_M.QNT,       ;
          LIST_NO   WITH STR(tit_rec,5),  ;
          GTD_NUM   WITH RYN_M.GTD_NUM,   ;
          GTD_LINE  WITH RYN_M.GTD_LINE,  ;
          COUNTRY   WITH RYN_M.COUNTRY,   ;
          COUNTRYC  WITH RYN_M.COUNTRYC,  ;
          PRICE_0   WITH RYN_M.PRICE,     ;
          PRICE   WITH RYN_M.PRICE
  SELECT RYN_M
ENDSCAN
USE

SELECT (m.s_sav)

RETURN .T.
