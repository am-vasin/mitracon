*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� List_Ryn     ��о�｀砒┴ ��むォ ��瓱�           03.02.98 13:21:08 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     �ムイ��� 甎�瓷� �� 爛��├�罔�.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE List_Ryn
PRIVATE d_n, ord_n

WAIT WIND "ｯム�罔� �皙キキ�" NOWAIT
RETURN

ord_n = 0
d_n = 0
s_sav = SELECT()
IF LIST_TIT.CLI_CODE = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� ��°�� 礫���碎 ��┘���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� 礒� �牀����..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF .NOT. SEEK(LIST_TIT.CLI_CODE, "ACCOUNT")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰-皰 �� ��� � ��┘�皰�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF .NOT. Tst_Cus()
  RETURN
ENDIF

IF hard_sale
  SELECT 0
  USE (base_path+"ORD_T") ORDER TAG DOC_NUM
  SEEK STR(LIST_TIT.LST_NUM,6)
  SCAN REST WHILE STR(LIST_TIT.LST_NUM,6) = LIST_NUM
    IF EMPTY(STO_MARK) .OR. EMPTY(WHO)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"�� ≡� ��錢�� 竄▲爨ぅ��!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      USE
      SELECT (s_sav)
      RETURN
    ENDIF
  ENDSCAN
  USE
ENDIF

IF .NOT. Tst_List()
  SELECT (s_sav)
  RETURN
ENDIF
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� � ....:....!....:....!....:....!....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�                [ ] �ムギ罐��碎 ����腮�                 �
*� 敖 ���祠キ硅 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� �      [ ] �����き��      [ ] �イ���痰� ����腮�      � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*� 敖 ����� �ムイ�腮 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �      [ ] �����き��      [ ] �イ���痰� ����腮�      � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�             [ ] �ョ�碎 矗ァ������ �� 瓷���             �
*�            < OK Ctrl-W > < �皖���碎瘴 Esc >            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, new_pr, is_inv, is_list, send_i, send_l, p_req
ex = 1
new_pr = .F.
is_inv = .T.
is_list= .T.
send_i = .T.
send_l = .T.
p_req  = .T.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 15, 60, "�皴竅� �� 爛��├�罔�"
DO Sun_Bord WITH 2, 3, 4, 56, " ��┘�� "
DO Sun_Bord WITH 6, 3, 8, 56, " ���祠キ硅 "
DO Sun_Bord WITH 9, 3, 11, 56, " ����� か� �ムイ�腮 "
@ 3, 5 SAY ACCOUNT.CUS_NAME COLOR SCHEME 1
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

@  5, 18 GET new_pr PICTURE "@*C �ムギ罐��碎 ����腮�"
@  7, 10 GET is_inv PICTURE "@*C �����き��"
@  7, 29 GET is_list PICTURE "@*C �イ���痰� ����腮�"
@ 10, 10 GET send_i PICTURE "@*C �����き��"
@ 10, 29 GET send_l PICTURE "@*C �イ���痰� ����腮�"
@ 12, 15 GET p_req PICTURE "@*C �ョ�碎 矗ァ������ �� 瓷���"
@ 13, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

RELEASE WINDOW (win_name)
POP KEY
IF ex = 2
  SELECT (s_sav)
  RETURN
ENDIF
*--------------------------------------------------------------------------
DO Last_Ord
DO Inv_Prp
IF new_pr         && �ムギ罐���
  DO Fil_Pric WITH ACCOUNT.CUS_CODE
ENDIF

IF is_inv .AND. d_n # 0
  DO P_Inv_F WITH d_n, DATE()
ENDIF

IF is_list
  DO P_Sto_F WITH ACCOUNT.CUS_CODE
ENDIF

IF p_req .AND. d_n # 0
  DO P_Req WITH "", STR(d_n,10), DATE()
ENDIF

IF send_i .AND. d_n # 0
  send_i = S_Inv(d_n, DATE())
ENDIF
IF send_l
*  send_l = S_List(ACCOUNT.CUS_CODE)
   DO Sto_S WITH ACCOUNT.CUS_CODE
ENDIF

IF TYPE("send_i") = "C" .OR. TYPE("send_l") = "C"

  PRIVATE path_w
  path_w = ""
  DO Get_Path WITH path_w, "DOC_L_F"
  path_w = Int_Dir(path_w, "���Θ皀 �竄�", 0)

  IF TYPE("send_i") = "C"

    DELETE FILE (path_w+send_i+"_T.DBF")
    DELETE FILE (path_w+send_i+".DBF")
    DELETE FILE (path_w+send_i+".FPT")
    COPY FILE (tmpo_path+send_i+"_T.DBF") TO (path_w+"RYN_M_T.DBF")
    COPY FILE (tmpo_path+send_i+".DBF") TO (path_w+"RYN_M.DBF")
    COPY FILE (tmpo_path+send_i+".FPT") TO (path_w+"RYN_M.FPT")

    DELETE FILE (tmpo_path+send_i+"_T.DBF")
    DELETE FILE (tmpo_path+send_i+".DBF")
    DELETE FILE (tmpo_path+send_i+".FPT")

  ENDIF

  IF TYPE("send_l") = "C"

    DELETE FILE (path_w+send_l+"_T.DBF")
    DELETE FILE (path_w+send_l+".DBF")
    DELETE FILE (path_w+send_l+".FPT")

    DELETE FILE (path_w+"DIVISION.DBF")
    DELETE FILE (path_w+"DEVELOP.DBF")
    DELETE FILE (path_w+"DIVISION.CDX")
    DELETE FILE (path_w+"DEVELOP.CDX")

    COPY FILE (tmpo_path+send_l+"_T.DBF") TO (path_w+"RYN_L_T.DBF")
    COPY FILE (tmpo_path+send_l+".DBF") TO (path_w+"RYN_L.DBF")
    COPY FILE (tmpo_path+send_l+".FPT") TO (path_w+"RYN_L.FPT")
    DELETE FILE (tmpo_path+send_l+"_T.DBF")
    DELETE FILE (tmpo_path+send_l+".DBF")
    DELETE FILE (tmpo_path+send_l+".FPT")

    COPY FILE (base_path+"DEVELOP.DBF") TO (path_w+"DEVELOP.DBF")
    COPY FILE (base_path+"DEVELOP.CDX") TO (path_w+"DEVELOP.CDX")
    COPY FILE (base_path+"DIVISION.DBF") TO (path_w+"DIVISION.DBF")
    COPY FILE (base_path+"DIVISION.CDX") TO (path_w+"DIVISION.CDX")

  ENDIF
  
ENDIF

SELECT (s_sav)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                    蹍▲瑙ガ ��┘���: 筥���� �� ��?                    �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 03.02.98 15:41:27 夕
PROCEDURE Tst_Cus
PRIVATE s_sav, is_cus1, is_cus2, is_cus3, mss, rt

s_sav = SELECT()

SELECT 0
USE (base_path+"CUSTOMER") ORDER TAG CODE ALIAS CUS_000 AGAIN

is_cus1 = ACCOUNT.ACC_TYPE = 4

IF .NOT. SEEK(ACCOUNT.CUS_CODE)
  is_cus2 = .F.
  is_cus3 = .F.
ELSE
  is_cus2 = .T.
  is_cus3 = .NOT. EMPTY(RYN)
ENDIF

rt = 0
IF .NOT. is_cus1 .OR. .NOT. is_cus2 .OR. .NOT. is_cus3
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"��┘�� �� 筥����, ��� ��痰祚━?"
  mss(3) = CHR(0)+"  [ �ぅ��ガ ィ� 筥������ ] [ �皖�Ε�瘴 �� �ムイ�腮 �� 爛��├�罔� ]"
  mss(4) = ""
  rt = Out_Mess(7, "mss")
ENDIF
IF rt = 2
  USE
  SELECT (s_sav)
  RETURN .F.
ENDIF

IF rt = 1
  SELECT ACCOUNT
  REPLACE ACCOUNT.ACC_TYPE WITH 4
  SELECT CUS_000
  IF .NOT. is_cus2
    APPEND BLANK
    REPLACE CODE      WITH ACCOUNT.CUS_CODE, ;
            B_NAME    WITH ACCOUNT.CUS_NAME, ;
            L_NAME    WITH ACCOUNT.CUS_NAME, ;
            RYN       WITH "*",              ;
            DATE_ON   WITH ACCOUNT.DATE_ON,  ;
            WHO_ADD   WITH ACCOUNT.WHO,      ;
            WHERE_ADD WITH ACCOUNT.WHERE
  ENDIF
  IF .NOT. is_cus3
    REPLACE RYN WITH "*"
  ENDIF
ENDIF
USE
SELECT (s_sav)

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                     蹍������ �牀▲爲� 甎�瓷�.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Tst_List

PRIVATE k00, r00, mss, w, c1, c2, prv_r
DIMENSION mss(3)
*
*     ����皰→� ��錢��
*
SELECT 0
IF .NOT. FILE(tmpo_path+"REQ_TMP.DBF")
  USE (base_path+"ORD_D")
  COPY STRUCTURE TO (tmpo_path+"REQ_TMP")
ENDIF
USE (tmpo_path+"REQ_TMP") EXCLUSIVE
ZAP

*
*     ����皰→� �����き��
*
SELECT 0
IF .NOT. FILE(tmpo_path+"RYN_TMP.DBF")
  USE (base_path+"RYN_M")
  COPY STRUCTURE TO (tmpo_path+"RYN_TMP")
ENDIF
USE (tmpo_path+"RYN_TMP") EXCLUSIVE
ZAP

SELECT 0
USE (base_path+"ACT_S") ORDER TAG SALE ALIAS SER_000

SELECT 0
d_n = 0
k00 = STR(LIST_TIT.LST_NUM,6)
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO000 AGAIN
SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_NAME ALIAS LIST_TMP AGAIN

IF .NOT. SEEK(k00)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���甌� �竅�! �ョィ� �皴竅��碎!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT STO000
  USE
  SELECT RYN_TMP
  USE
  SELECT SER_000
  USE
  SELECT REQ_TMP
  USE
  RETURN .F.
ENDIF

r00 = .T.
c1 = 0
c2 = 0
*
*   ����茫� �� 甎�瓷�
*
prv_r = -1
SCAN REST WHILE LIST_NUM = k00
  IF CODE < 0 .OR. QNT = 0 .AND. QNT_REQ = 0
    LOOP
  ENDIF
  IF QNT_REZ # 0
    r00 = .F.
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"踳Δ� 腑� �皴竅��碎, ├���赳メ� 爛Д燿�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    EXIT
  ENDIF
  IF CODE = 0 .OR. .NOT. SEEK(CODE,"STO000")  && �礒��!
    r00 = .F.
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"踳Δ� 腑� �牀����碎, ├���赳メ� �� �メ���め��� ��Ж罔�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    EXIT
  ENDIF
  IF prv_r = RECNO()
    r00 = .F.
    mss(1) = ""
    mss(2) = CHR(0)+"��������! ��Ж罔� "+ALLTRIM(STO000.PREFIX)+" "+ ;
                                         ALLTRIM(STO000.NAME)+" "+ ;
                                         ALLTRIM(STO000.PRODUCER)+" 瓩〓┼�瘡!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    EXIT
  ENDIF
  prv_r = RECNO()
  IF QNT_DUE # 0
    r00 = .F.
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"踳Δ� 腑� �牀����碎, ├���赳メ� �� ぎ���!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    EXIT
  ENDIF
  w = QNT-QNT_REQ      && ��������碎!
  IF w # 0
    IF w > 0 .AND. STO000.QNT < w && .AND. EMPTY(IS_DEF) �┴���� �牀��Θ � �┃竅!
      r00 = .F.
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"踳Δ� 腑� �牀����碎, ├���赳メ� �� ぅ筥罔��!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      EXIT
    ENDIF
    IF hard_sale
      r00 = .F.
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"���甌� �� �����痰跪 �������!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      EXIT
    ENDIF
    IF ord_n = 0
      ord_n = LIST_TIT.ORD_NUM+1
    ENDIF
    SELECT REQ_TMP
    APPEND BLANK
    REPLACE LIST_NUM WITH k00,     ;
            ORD_NUM  WITH ord_n,   ;
            CODE     WITH LIST_TMP.CODE, ;
            QNT      WITH w
    c1 = c1+CODE
    c2 = c2+QNT
    SELECT LIST_TMP
  ENDIF
  IF LIST_TMP.QNT # 0
    IF .NOT. SEEK(STR(LIST_TMP.CODE,7)+LIST_TIT.FIRM, "SER_000")
      = SEEK(STR(LIST_TMP.CODE,7), "SER_000")
    ENDIF
    SELECT RYN_TMP
    APPEND BLANK
    REPLACE DOC_NUM   WITH d_n,             ;
            DOC_DATE  WITH DATE(),          ;
            CODE      WITH LIST_TMP.CODE,   ;
            GTD_NUM   WITH SER_000.GTD_NUM, ;
            COUNTRY   WITH SER_000.COUNTRY, ;
            COUNTRYC  WITH SER_000.COUNTRYC,;
            GTD_LINE  WITH SER_000.GTD_LINE,;
            QNT       WITH LIST_TMP.QNT
  ENDIF
  SELECT LIST_TMP
ENDSCAN

check_w = STR(c1 % 9, 1)+STR(c2 % 9, 1)+ " "

USE
SELECT STO000
USE
SELECT RYN_TMP
USE
SELECT REQ_TMP
USE
SELECT SER_000
USE

RETURN r00

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                            蹍������ ぎ������.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Last_Ord
PRIVATE sel_rem, k00, l_ord, cnt_pos, cnt_neg

sel_rem = SELECT()

IF .NOT. File_O(tmpo_path+"REQ_TMP.DBF")
  RETURN
ENDIF
k00 = STR(LIST_TIT.LST_NUM,6)
SELECT 0
USE (tmpo_path+"REQ_TMP")
l_ord = RECCOUNT()
IF l_ord = 0
  USE
  SELECT (sel_rem)
  RETURN
ENDIF
cnt_neg = 0
cnt_pos = 0
SCAN
  IF QNT < 0
    cnt_neg = cnt_neg+1
  ELSE
    cnt_pos = cnt_pos+1
  ENDIF
ENDSCAN
REPLACE LIST_TIT.ORD_NUM WITH ORD_NUM
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_ORD AGAIN
SELECT REQ_TMP
SCAN
  IF .NOT. SEEK(CODE,"STO_ORD")
    LOOP
  ENDIF
  SELECT STO_ORD
  REPLACE QNT_SAVE WITH QNT_SAVE+REQ_TMP.QNT,  ;
          QNT      WITH QNT-REQ_TMP.QNT
  SELECT REQ_TMP
ENDSCAN
USE
SELECT STO_ORD

USE (base_path+"ORD_T")
APPEND BLANK
REPLACE LIST_NUM  WITH k00,  ;
        LIST_NAME WITH LIST_TIT.LST_NAME, ;
        ORD_NUM   WITH LIST_TIT.ORD_NUM,  ;
        ORD_DATE  WITH DATE(),   ;
        ORD_TIME  WITH TIME(),   ;
        SIZE_POS  WITH cnt_pos,  ;
        SIZE_NEG  WITH cnt_neg,  ;
        QUEUE     WITH LIST_TIT.ORD_QUEUE, ;
        WHO       WITH user,     ;
        WHERE     WITH arm

USE (base_path+"ORD_D")
APPEND FROM (tmpo_path+"REQ_TMP")

USE (base_path+"LIST_DET") ORDER TAG L_NAME ALIAS LIST_000 AGAIN

SEEK k00
REPLACE REST WHILE LIST_NUM = k00 QNT_REQ WITH QNT
PRIVATE mss
DIMENSION mss(4)
mss(1) = ""
mss(2) = CHR(0)+"��������! �� �� �����痰跪 ��瘍�����瘡 甌 瓷��ぎ� �� 轤��� 甎�瓷�."
mss(3) = CHR(0)+"�筮爼�牀���� ��錢�� N "+ALLTRIM(STR(LIST_TIT.LST_NUM,10))+"/"+   ;
                                        +ALLTRIM(STR(LIST_TIT.ORD_NUM,10))
mss(4) = ""
DO Out_Mess WITH 7, "mss"
USE

SELECT (sel_rem)

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�          蹍������ 筮爼�牀����� �����き�� ( メ�� 矗ァ礇矚� ).         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Inv_Prp

PRIVATE sel_sav, arr_w, q_rep, mss, is_IDL, path_as, tmp_IDL

sel_sav = SELECT()
is_IDL = .F.
path_as = ALLTRIM(Get_Sys("PATH_AS"))
IF .NOT. EMPTY(path_as)
  IF File_O(path_as+"MATERS.DBF")
    USE (path_as+"MATERS")
    is_IDL = .T.
  ENDIF
ENDIF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_000

SELECT 0
USE (base_path+"RYN_M") ALIAS SAL_000

SELECT 0
USE (base_path+"RYN_L") ORDER TAG C_CODE AGAIN

SELECT 0
USE (tmpo_path+"RYN_TMP")
*
*   ��玻オ罔� ÷��� ( ━キ�� ÷���! ) � 筮爼�牀���┘ 痰牀� �����き��
*
PRIVATE q_sale, q_rest
m_tot = 0
SCAN FOR QNT # 0

  IF .NOT. SEEK(CODE, "STO_000")    && �矗����, 皰�讓� 艪� °��...
    LOOP
  ENDIF
  
  SCATTER TO arr_w
  SELECT STO_000
  REPLACE QNT_SAVE WITH QNT_SAVE-RYN_TMP.QNT, ;
          QNT_REAL WITH QNT_REAL+RYN_TMP.QNT
  SELECT RYN_L
  IF SEEK(STR(ACCOUNT.CUS_CODE, 5)+STR(RYN_TMP.CODE,7))
    REPLACE QNT WITH QNT+RYN_TMP.QNT
  ELSE
    APPEND BLANK
    REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE, ;
            CODE     WITH RYN_TMP.CODE, ;
            QNT      WITH RYN_TMP.QNT, ;
            PRICE_D  WITH STO_000.PRICE, ;
            PRICE_R  WITH STO_000.PRICE_R
  ENDIF
  IF EMPTY(d_n)
    d_n = Doc_Num("RYN_M", def_firm, arm, date())
  ENDIF
  SELECT SAL_000
  APPEND FROM ARRAY arr_w
  REPLACE DOC_NUM WITH d_n
  IF is_IDL
    SELECT MATERS
    tmp_IDL = IDL_MT
    REPLACE IDL_MT WITH IDL_MT+1
    SELECT SAL_000
    REPLACE PURSYS WITH MATERS.SUBSYS, ;
            IDL    WITH tmp_IDL
  ENDIF
  SELECT RYN_TMP

ENDSCAN

USE
SELECT SAL_000
USE
IF is_IDL
  SELECT MATERS
  USE
ENDIF
SELECT STO_000
USE (base_path+"RYN_M_T")
APPEND BLANK
REPLACE DOC_NUM   WITH d_n,              ;
        DOC_DATE  WITH DATE(),           ;
        DOC_TIME  WITH SECONDS(),        ;
        LST_NUM   WITH LIST_TIT.LST_NUM, ;
        TECHNTP   WITH LIST_TIT.TECHNTP, ;
        TECHNID   WITH LIST_TIT.TECHNID, ;
        CUS_CODE  WITH ACCOUNT.CUS_CODE, ;
        SUP_CODE  WITH sup_own,          ;
        ORD_NUM   WITH LIST_TIT.REQ_DOC, ;
        ORD_DATE  WITH LIST_TIT.REQ_DATE,;
        WHO       WITH user,             ;
        WHERE     WITH arm

USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE AGAIN
IF SEEK(ACCOUNT.CUS_CODE)
  REPLACE LAST_D_NUM WITH d_n, ;
          DATE_P     WITH DATE(), ;
          WHO        WITH user,   ;
          WHERE      WITH arm
ELSE
  APPEND BLANK
  REPLACE CUS_CODE   WITH ACCOUNT.CUS_CODE, ;
          CUS_NAME   WITH ACCOUNT.CUS_NAME, ;
          VALUTA     WITH "$",   ;
          LAST_D_NUM WITH d_n, ;
          DATE_P     WITH DATE(), ;
          WHO        WITH user,   ;
          WHERE      WITH arm
ENDIF
USE

SELECT LIST_TIT
REPLACE SAL_NUM  WITH STR(d_n,10), ;
        SAL_DATE WITH DATE(), ;
        WHO_SAL  WITH user
SELECT (sel_sav)

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                       �痰���→� 罐� か� 筥�����.                       �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 04.02.98 13:22:43 夕
PROCEDURE Set_Pr
PARAMETER c_code

PRIVATE s_sav, v

s_sav = SELECT()

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_WWW AGAIN

SELECT 0
USE (base_path+"RYN_L") ORDER TAG CUS_CODE ALIAS RYN_WWW AGAIN
SET RELATION TO CODE INTO STO_WWW

DO Wt_Mess WITH "�ぅ� �ムギ罐���..."
SEEK c_code
REPLACE REST WHILE c_code = CUS_CODE ;
    PRICE_R WITH STO_WWW.PRICE_R, ;
    PRICE_D WITH STO_WWW.PRICE
DO Wt_Mess

USE
SELECT STO_WWW
USE

SELECT (s_sav)

RETURN
