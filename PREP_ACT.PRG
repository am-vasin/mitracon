*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Prep_Act     ��о�｀砒┴ ��むォ ��瓱�           16.05.97 13:37:33 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 蹍������ 筮爼�牀����� �爬ガ���� ����.                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Prep_Act
PRIVATE key_v

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

*          ��爼�牀���┘ �爬ガ���� ����
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   敖 ��瘍モ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕   �
*�   �        ( ) ����膈覃   ( ) �/�        �   �
*�   青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰   �
*�           [ ] ��縲���碎 ����皰→�            �
*� 敖 ���祠キ硅 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � [ ] 踸ガ�. ���  [ ] 踸ガ�. ��� (甌��.) � �
*� � [ ] ��痳�Δキ��  [ ] ��錢�� ��痰�∫┴�   � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�       < OK Ctrl-W > < �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
PRIVATE ex, money_t, rest_l, doc_1, doc_2, doc_3, e_ord
ex   = 1             &&
money_t = 1
rest_l = .F.
doc_1 = .F.
doc_2 = .F.
doc_3 = .F.
e_ord = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 12, 50, "��爼�牀���┘ �爬ガ���� ����"
DO Sun_Bord WITH 2,  5, 4, 44, " ��瘍モ "
DO Sun_Bord WITH 6,  3, 9, 46, " ���祠キ硅 "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 14 GET money_t PICTURE "@*RH ����膈覃  ;�/�" WHEN INC_TIT.IS_PRICE = "�"
*@ 3, 32 GET is_buy PICTURE "@*RH ���祚�� ;����├�罔�"
@ 5,  13 GET rest_l PICTURE "@*C ��縲���碎 ����皰→�"
@ 7,  5 GET doc_1   PICTURE "@*C 踸ガ�. ���"
@ 7, 22 GET doc_2   PICTURE "@*C 踸ガ�. ��� (甌��.)"
@ 8,  5 GET doc_3   PICTURE "@*C ��痳�Δキ��"
@ 8, 22 GET e_ord   PICTURE "@*C ��錢�� ��痰�∫┴�"
@ 10, 9 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Do_Act()

*--------------------------------------------------------------------------
POP KEY
RELEASE WINDOW (win_name)
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                �ク�痼イ痰▲��� 甌Г��┘ �爬ガ���� ����.                �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 16.05.97 16:30:38 夕
PROCEDURE Do_Act

PRIVATE mss, sel_sav, f_d, n_d, d_d, pr_0, key_v
PRIVATE pr_r, pr_d, s_0, s_h, q_w, q_w1, q_r, is_emp
PRIVATE save_GTD
PRIVATE r_brends, t_gtd, t_cnt, t_line, bad_g, is_unk
PRIVATE prtList, prtNum, tmpParty, i

m.is_unk = .F.
IF READKEY() % 256 = 12 .OR. ex = 2
  RETURN .T.
ENDIF

IF INC_TIT.SUP_CODE = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰-皰 �� ��� � ��痰�∫┴��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

*
*   蹍▲爲� �� ����腮� �竅硅� ���
*
sel_sav = SELECT()
SELECT STOCK
SET ORDER TO TAG CODE
SELECT 0
r_brends = ""
USE base_path+"B_GROUPS" SHARED AGAIN ALIAS BBB_2412
SCAN
  r_brends = r_brends+DEV_LIST
ENDSCAN
USE
SELECT INC_LIST
key_v = STR(RECNO("INC_TIT"),5)
SEEK key_v
is_emp = .F.
IF .F. &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
SCAN REST WHILE LIST_NO = key_v   ;
          FOR (QNT_INV # 0 .OR. QNT # 0) .AND. CODE # 0
  IF EMPTY(GTD_NUM)
    = SEEK(CODE, "STOCK")
    IF " "+ALLTRIM(STR(STOCK.PRO_CODE))+" " $ r_brends
      LOOP
    ENDIF
    STORE "" TO t_gtd, t_cnt, t_cntC, t_line
    bad_g = .T.
    DO Set_Gtd WITH CODE, STOCK.PREFIX, STOCK.NAME, STOCK.PRODUCER, t_gtd, t_cnt, t_cntC, t_line, bad_g
    REPLACE GTD_NUM WITH t_gtd, COUNTRY WITH t_cnt, COUNTRYC WITH t_cntC, GTD_LINE WITH t_line, B_GTD WITH bad_g
    IF EMPTY(GTD_NUM)
      is_emp = .T.
    ENDIF
  ENDIF
ENDSCAN
ENDIF &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

IF is_emp .AND. .F. &&&&&&&&&&&&&&&
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��������! �� ▲Г� �牀痰�←キ� ���! 蹍ぎ���ガ �爬ガ? [ �モ ] [ �� ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 1
    SELECT (sel_sav)
    RETURN .T.
  ENDIF
ENDIF
save_GTD = .NOT. EMPTY(Get_Sys("SAVE_GTD")) && 踸Л�� �ギ≦�え��痰�  甌縲. ���

IF INC_TIT.FILE_TYPE = 4
  SELECT 0
  IF .NOT. rest_l
    USE (path_comm+"INVOICET") ORDER TAG EXP_NUM
    SEEK LEFT(DTOS(INC_TIT.INC_DATE),4)+INC_TIT.INC_DOC
    REPLACE DATE_INC WITH DATE()  && TERM WITH "*"
  ENDIF
  USE (path_comm+"INVOICED")
  SELECT (sel_sav)
ENDIF

PRIVATE s_d0, old_rev, tmpName
n_d = ""
d_d = DATE()
SELECT 0
USE (base_path+"PARMS")
s_d0 = DAT_START
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
IF .NOT. SEEK(INC_TIT.SUP_CODE)    && �薀� ��痰�∫┴� � ���↓�� 甎�瓷�
  SELECT 0
  USE (base_path+"CUSTOMER") ORDER TAG CODE
  SEEK INC_TIT.SUP_CODE
  SELECT ACCOUNT
  APPEND BLANK
  REPLACE SUBSYS   WITH sys_char,         ;
          CUS_CODE WITH CUSTOMER.CODE,    ;
          CUS_NAME WITH CUSTOMER.L_NAME,  ;
          DATE_ON  WITH CUSTOMER.DATE_ON, ;
          WHO      WITH CUSTOMER.WHO_ADD, ;
          WHERE    WITH CUSTOMER.WHERE_ADD
  SELECT CUSTOMER
  USE
ENDIF
SELECT 0
USE (tmp_path+"DL"+ALLTRIM(STR(RECNO("INC_TIT")))) ALIAS LOG_TMP
REPLACE ALL GOT WITH .T.
USE
IF File_O(path_comm+"GTD.DBF") .AND. File_O(path_comm+"GTD_TIT.DBF")
  USE (path_comm+"GTD") ORDER TAG NAME_GTD ALIAS GTD_0220 AGAIN
  SELECT 0
  USE (path_comm+"GTD_TIT") ORDER TAG GTD_NUM ALIAS GTDT_0220 AGAIN
ELSE
  save_GTD = .F.
ENDIF
STORE 0 TO s_0, s_h
USE (base_path+"UNK_LIST") ORDER TAG DOC_DATE
SELECT INC_LIST
SEEK key_v
*
*      ��爼�珮ガ 甌ぅ爨━�� �爬ガ���� ����
*
IF sale_mode = "Mitracon"
  REPLACE INC_TIT.BUY WITH 2
ENDIF

f_d = INC_TIT.FIRM
SCAN REST WHILE LIST_NO = key_v   ;
          FOR (QNT_INV # 0 .OR. QNT # 0) .AND. CODE # 0
  IF rest_l .AND. QNT = 0 && ��Ж罔� 腮甄�矚� �� ぎ�., �� �� �爬絎ゃモ瘴
    LOOP                  && �痰�※�, ��Ε� ラ� ���ぅ矚�...
  ENDIF
  IF EMPTY(n_d)     && �� °�� ���ム�? ����ガ!
    n_d = Doc_Num("ACT_NUM", INC_TIT.FIRM, arm, d_d)
    n_d = STR(n_d,8)+"  "
  ENDIF
  pr_r = 0
  pr_d = 0
  pr_0 = 0
  IF INC_TIT.IS_PRICE = "$"
    pr_d = INC_LIST.PRICE
    pr_r = ROUND(pr_d*kurs_b,2)
    pr_0 = pr_d
  ENDIF
  IF INC_TIT.IS_PRICE = "�"
    pr_r = Round_6(INC_LIST.PRICE)
    pr_0 = pr_r*100/(100+nds_flt)
    pr_d = ROUND(pr_r/kurs_b,2)
  ENDIF

  old_rev = 0
  = SEEK(CODE,"STOCK")
  IF INC_LIST.QNT >= 0
    IF .NOT. EMPTY(INC_LIST.GTD_NUM) .AND. save_GTD
      SELECT GTD_0220
      tmpName = PADR(ALLTRIM(STOCK.PREFIX)+ALLTRIM(STOCK.NAME),FSIZE("NAME", "GTD_0220"))
      IF .NOT. SEEK(tmpName + INC_LIST.GTD_NUM)
        APPEND BLANK
        REPLACE GTD_NUM  WITH INC_LIST.GTD_NUM, ;
                GTD_DATE WITH DATE(),           ;
                COUNTRY  WITH INC_LIST.COUNTRY, ;
                COUNTRYC WITH INC_LIST.COUNTRYC,;
                GTD_LINE WITH INC_LIST.GTD_LINE,;
                NAME     WITH tmpName
        IF .NOT. SEEK(GTD_NUM, "GTDT_0220")
          SELECT GTDT_0220
          APPEND BLANK
          REPLACE GTD_NUM  WITH INC_LIST.GTD_NUM, ;
                  GTD_DATE WITH DATE()
        ENDIF
      ENDIF
    ENDIF
    m.tmpParty = G_Party()
    SELECT ACT_S
    APPEND BLANK
    REPLACE FIRM     WITH INC_TIT.FIRM,     ;
            DOC_NUM  WITH n_d,              ;
            DOC_DATE WITH d_d,              ;
            INC_TYPE WITH " ",              ;
            FLG_BUY  WITH IIF(INC_TIT.BUY=1, "B", "R"),  ;
            GTD_NUM  WITH INC_LIST.GTD_NUM, ;
            COUNTRY  WITH INC_LIST.COUNTRY, ;
            COUNTRYC WITH INC_LIST.COUNTRYC,;
            GTD_LINE WITH INC_LIST.GTD_LINE,;
            IDP      WITH INC_LIST.IDP,     ;
            CODE     WITH INC_LIST.CODE,    ;
            PARTY    WITH m.tmpParty,		;
            QNT_EXP  WITH INC_LIST.QNT_INV, ;
            QNT_INC  WITH INC_LIST.QNT,     ;
            QNT_REP  WITH IIF(INC_TIT.BUY=1, 0, INC_LIST.QNT), ;
            REST_REP WITH IIF(INC_TIT.BUY=1, 0, INC_LIST.QNT), ;
            QNT_REST WITH INC_LIST.QNT,     ;
            PRICE_0  WITH pr_0,             ;
            PRICE_R  WITH pr_r,             ;
            PRICE_D  WITH pr_d
    q_r = QNT_REP
    IF INC_TIT.FILE_TYPE = 4
      SELECT INVOICED
      IF INC_LIST.INV_REF = 0
        APPEND BLANK
        REPLACE EXP_NUM  WITH INC_TIT.INC_DOC, ;
                EXP_DATE WITH INC_TIT.INC_DATE
      ELSE
        GO INC_LIST.INV_REF
      ENDIF
      REPLACE CODE     WITH INC_LIST.CODE, ;
              QNT      WITH QNT+INC_LIST.QNT,  ;
              AKT_NUM  WITH ALLTRIM(AKT_NUM)+INC_TIT.FIRM+STR(YEAR(d_d),4)+LTRIM(n_d)+","
    ENDIF
    IF INC_LIST.UNK .AND. ACT_S.QNT_INC > 0
      m.is_unk = .T.
      SELECT UNK_LIST
      GO TOP
      IF .NOT. EMPTY(DOC_DATE) .OR. BOF() .OR. EOF()
        APPEND BLANK
      ENDIF
      REPLACE	CODE		WITH ACT_S.CODE,	;
      			FIRM		WITH ACT_S.FIRM,	;
      			DOC_DATE	WITH ACT_S.DOC_DATE,;
      			DOC_NUM		WITH ACT_S.DOC_NUM,	;
      			QNT			WITH ACT_S.QNT_INC
    ENDIF
    SELECT ACT_S
  ELSE
    m.prtNum = 0
    q_w = -INC_LIST.QNT
    q_r = INC_LIST.QNT
    SELECT ACT_S
    SEEK INC_LIST.CODE
    q_w1 = 0
    SCAN REST WHILE CODE = INC_LIST.CODE FOR REST_REP > 0
      = SEEK(FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM, "ACT_S_T")
      IF ACT_S_T.SUP_CODE # INC_TIT.SUP_CODE
        LOOP
      ENDIF
      q_w1 = MIN(q_w, REST_REP)
      REPLACE REST_REP WITH REST_REP - q_w1, ;
              QNT_REP  WITH QNT_REP - q_w1,  ;
              QNT_REST WITH QNT_REST - q_w1
      m.prtNum = m.prtNum+1
      DIMENSION prtList(m.prtNum, 2)
      prtList(m.prtNum, 1) = LEFT(PARTY, 10)+"_"
      prtList(m.prtNum, 2) = m.q_w1
      IF DOC_DATE < s_d0
        old_rev = old_rev+q_w1
      ENDIF
      q_w = q_w-q_w1
      IF q_w = 0
        EXIT
      ENDIF
    ENDSCAN

    FOR m.i = 1 TO m.prtNum
      APPEND BLANK
      REPLACE FIRM     WITH INC_TIT.FIRM,     ;
              DOC_NUM  WITH n_d,              ;
              DOC_DATE WITH d_d,              ;
              FLG_BUY  WITH IIF(INC_TIT.BUY=1, "B", "R"),  ;
              INC_TYPE WITH IIF(money_t=1, "0", " "),  ;
              CODE     WITH INC_LIST.CODE,    ;
              PARTY    WITH prtList(m.i, 1),  ;
              QNT_EXP  WITH IIF(m.i=1, INC_LIST.QNT_INV, 0), ;
              QNT_INC  WITH -prtList(m.i, 2), ;
              PRICE_0  WITH pr_0,             ;
              PRICE_R  WITH pr_r,             ;
              PRICE_D  WITH pr_d

*            QNT_REP  WITH  -q_w1,           ;

    ENDFOR
  ENDIF
  s_0    = s_0 + QNT_INC*pr_0
  IF INC_TIT.IS_PRICE = "$"
    s_h = s_h + QNT_INC*pr_d
  ELSE
    s_h = s_h + ROUND(QNT_INC*pr_r, 2)
  ENDIF
  REPLACE STOCK.QNT     WITH STOCK.QNT+QNT_INC,  ;
          STOCK.QNT_REP WITH STOCK.QNT_REP+q_r,  ;
          STOCK.QNT_S_REP WITH STOCK.QNT_S_REP-old_rev
  IF .NOT. EMPTY(STOCK.DATE_OFF)
    REPLACE STOCK.DATE_OFF WITH CTOD(""), STOCK.WHO_DEL WITH 0
  ENDIF
  SELECT INC_LIST
  DELETE
ENDSCAN
USE IN UNK_LIST

SELECT ACT_S
IF EMPTY(n_d)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���祠キ� �� 痲�爼�牀���, �. �. 甎�甌� �竅�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT ACCOUNT
  USE
ELSE
  PRIVATE i_type
  i_type = " "
  DO CASE
  CASE INC_TIT.FILE_TYPE = 2
    i_type = " "
  CASE INC_TIT.FILE_TYPE = 4
    i_type = "2"
  ENDCASE
  SELECT ACT_S_T
  APPEND BLANK
  REPLACE FIRM      WITH INC_TIT.FIRM, ;
          DOC_NUM   WITH n_d,  ;
          DOC_DATE  WITH d_d,  ;
          DOC_TIME  WITH SECONDS(),    ;
          SUP_CODE  WITH INC_TIT.SUP_CODE,  ;
          FLG_BUY   WITH IIF(INC_TIT.BUY=1, "B", "R"),  ;
          INC_TYPE  WITH " ",  ;
          INV_TYPE  WITH i_type, ;
          EXP_NUM   WITH INC_TIT.INC_DOC,  ;
          EXP_CODE  WITH INC_TIT.INC_CODE,  ;
          EXP_DATE  WITH INC_TIT.INC_DATE,  ;
          VALUTA    WITH IIF(INC_TIT.IS_PRICE = "$", "$", "�"),    ;
          NDS_      WITH IIF(INC_TIT.IS_PRICE = "�", nds_flt, 0),  ;
          TOTAL_0   WITH s_0,   ;
          TOTAL_H   WITH s_h,   ;
          NOTE      WITH INC_TIT.NOTE, ;
          WHO_ACT   WITH user,  ;
          WHERE_ACT WITH arm
  SELECT ACCOUNT
  IF s_h # 0
    USE (base_path+"CUS_REST") ORDER TAG CUS_CODE
    IF .NOT. SEEK(STR(ACT_S_T.SUP_CODE)+ACT_S_T.FIRM)
      APPEND BLANK
      REPLACE FIRM     WITH ACT_S_T.FIRM, ;
              CUS_CODE WITH ACT_S_T.SUP_CODE
    ENDIF
    IF ACT_S_T.VALUTA = "$"
      REPLACE ACC_SUMD WITH ACC_SUMD+ACT_S_T.TOTAL_H
    ELSE
      REPLACE ACC_SUM WITH ACC_SUM+ACT_S_T.TOTAL_H
    ENDIF
  ENDIF
  USE
  IF m.is_unk
    PRIVATE unk_mss
    USE (base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS UNK_PERS IN 0
    = SEEK(m.user, "UNK_PERS")
    unk_mss = "踸 甌Г��┬ �爬ガ���� ���� "+ALLTRIM(n_d)+" �� "+DTOC(d_d)+CHR(13)+ ;
    "�´�珮Ε� �ム�甎�Л���覃 ぎ��."+CHR(13)+ ;
    "���ぎ∫┴ "+ALLTRIM(UNK_PERS.FAMILY)+" "+LEFT(UNK_PERS.NAME, 1)+" "+LEFT(UNK_PERS.S_NAME, 1)
    USE IN UNK_PERS
    DO SS_Send with 1, unk_mss
  ENDIF
ENDIF
IF INC_TIT.FILE_TYPE = 4
  SELECT INVOICED
  USE
ENDIF
PRIVATE n__1, n__2, st_n, i_0
st_n = Prep_Ord()          && ��爼�珮ガ ��錢�� �� ÷���牀���┘
DO WHILE .F.	&& .NOT. EMPTY(st_n) && �ョ�碎 ��錢��
  i_0 = AT("!", st_n)
  IF i_0 = 0
    st_n = ""
  ELSE
    n__2 = LEFT(st_n, i_0-1)
    n__1 = VAL(ALLTRIM(LEFT(n__2,6)))
    n__2 = VAL(ALLTRIM(RIGHT(n__2,3)))
    IF new_ord
      DO Prn_OrdN WITH n__1, n__2
    ELSE
      DO Prn_Ord WITH n__1, n__2
    ENDIF
    st_n = SUBSTR(st_n,i_0+1)
  ENDIF
ENDDO
IF USED("GTD_0220")
  SELECT GTD_0220
  USE
ENDIF
IF USED("GTDT_0220")
  SELECT GTDT_0220
  USE
ENDIF
SELECT INC_LIST
IF .NOT. rest_l
  DO Del_Tit
ENDIF

IF doc_1
  DO P_Act WITH f_d, n_d, d_d, "H"
ENDIF
IF doc_2
  DO P_Act WITH f_d, n_d, d_d, "B"
ENDIF
IF doc_3
  DO P_Act WITH f_d, n_d, d_d, "D"
ENDIF

IF e_ord
  DO El_Ord WITH f_d, n_d, d_d
ENDIF

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                  ��爼�牀���┘ ��錢�� �� ÷���牀���┘.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 19.05.97 13:47:48 夕
PROCEDURE Prep_Ord
PRIVATE mss, l_sav, n_o, s_ret, st_ret, q, l_ord, cnt_pos, cnt_neg
PRIVATE sendList, dueMode, n_ord, n_SOrd

m.dueMode = "R"
*m.dueMode = "B"

st_ret = ""
n_ord  = 0
SELECT INC_DUE
SET ORDER TO TAG L_LIST
IF .NOT. SEEK(key_v)        && �モ �┴����� ぎ��� � 轤�� �爬絎ぅ
  SET ORDER TO TAG L_CODE
  RETURN st_ret
ENDIF

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM

SELECT 0
USE (base_path+"ORD_D")

SELECT 0
USE (base_path+"ORD_T")

SELECT LIST_DET
SET ORDER TO TAG L_CODE

SELECT STOCK
SET ORDER TO TAG CODE

SELECT INC_DUE
l_ord = 0
cnt_neg = 0
cnt_pos = 0
n_o = 0
n_list = "      "
n_SOrd = SOrd_Num(STR(LIST_TIT.LST_NUM, 6))
n_SOrd = STR(n_SOrd, 3)
SCAN REST WHILE INC_LIST = key_v FOR QNT # 0
  IF EXP_LIST # n_list       && ��腮��ガ ���覃 甎�甌�
    IF .NOT. EMPTY(n_list)		&& �� �ム�覃:
      DO Scan_Ord WITH n_list	&&   �爬〓え� � ��瑙ぎ� ��錢��
    ENDIF
    n_list = EXP_LIST
    IF .NOT. SEEK(VAL(ALLTRIM(EXP_LIST)), "LIST_TIT")
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"���甌� N "+ALLTRIM(EXP_LIST)+" 礒� 磴��キ, ÷��� �� 甌Г�モ瘴!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      n_o = 0
    ELSE
      n_o = LIST_TIT.ORD_NUM+1
      REPLACE LIST_TIT.ORD_NUM  WITH n_o, ;
              LIST_TIT.NOTE_SAL WITH "*"

*      ��錢�� 礒� �＝�礒キ� (B) ┼� �� �� ��ぎ 甌；��碎 (R)! �� ��ぎ �� � 瘍モ腮�!!!
*      IF m.dueMode # "R"
*        REPLACE LIST_TIT.ORD_COUNT WITH LIST_TIT.ORD_COUNT+1
*      ENDIF
      st_ret = st_ret+INC_DUE.EXP_LIST+STR(n_o,3)+"!"
      SELECT ORD_T
      IF l_ord # 0
        REPLACE SIZE_NEG WITH cnt_neg, SIZE_POS WITH cnt_pos
      ENDIF
      cnt_neg = 0
      cnt_pos = 0
      l_ord = 0
      APPEND BLANK
      REPLACE LIST_NUM  WITH INC_DUE.EXP_LIST,   ;
              LIST_NAME WITH LIST_TIT.LST_NAME,  ;
              ORD_NUM   WITH n_o,                ;
              ORD_DATE  WITH DATE(),             ;
              ORD_TIME  WITH TIME(),             ;
              QUEUE     WITH LIST_TIT.ORD_QUEUE, ;
              WHO       WITH m.user, ;
              WHERE     WITH arm

      IF m.dueMode # "R"
        REPLACE STO_MARK WITH user,   ;
                CHECK_S  WITH n_SOrd, ;
                REQ_DATE WITH DATE(), ;
                REQ_TIME WITH TIME()
      ENDIF

*REQ_DATE  WITH LIST_TIT.DATE_REQ, REQ_TIME  WITH LIST_TIT.TIME_REQ

      m.n_ord = m.n_ord+1
      DIMENSION sendList[m.n_ord, 2]
      sendList[m.n_ord, 1] = LIST_TIT.LST_ARM
      sendList[m.n_ord, 2] = ALLTRIM(LIST_NUM)+"/"+ALLTRIM(STR(ORD_NUM))+CHR(13)+CHR(10)

    ENDIF
  ENDIF
  IF n_o = 0    && 蹍�竅��ガ ÷��� か� 轤��� 甎�瓷�, �. �. �� 磴��キ
    LOOP
  ENDIF
  = SEEK(INC_DUE.CODE,"STOCK")
  IF .NOT. SEEK(INC_DUE.EXP_LIST+STR(INC_DUE.CODE,7), "LIST_DET")
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"� 甎�瓷� N "+ALLTRIM(INC_DUE.EXP_LIST)+" �モ ��Ж罔� "
    mss(3) = CHR(0)+ALLTRIM(STOCK.PREFIX)+" "+ALLTRIM(STOCK.NAME)+" "+ALLTRIM(STOCK.PRODUCER)+", �牀�竅��ガ!"
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
    LOOP
  ENDIF
  IF LIST_DET.QNT_DUE <= 0
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"� 甎�瓷� N "+ALLTRIM(INC_DUE.EXP_LIST)+" �モ ぎ��� �� ��Ж罔� "
    mss(3) = CHR(0)+ALLTRIM(STOCK.PREFIX)+" "+ALLTRIM(STOCK.NAME)+" "+ALLTRIM(STOCK.PRODUCER)+", �牀�竅��ガ!"
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
    LOOP
  ENDIF
  q = MIN(INC_DUE.QNT, STOCK.QNT, LIST_DET.QNT_DUE)
  REPLACE STOCK.QNT        WITH STOCK.QNT-q, ;
          STOCK.QNT_SAVE   WITH STOCK.QNT_SAVE+q, ;
          LIST_DET.QNT_DUE WITH LIST_DET.QNT_DUE-q, ;
          LIST_DET.QNT     WITH LIST_DET.QNT+q,     ;
          LIST_DET.QNT_REQ WITH LIST_DET.QNT_REQ+q
  l_ord = l_ord+1
  SELECT ORD_D
  APPEND BLANK
  REPLACE LIST_NUM WITH INC_DUE.EXP_LIST, ;
          ORD_NUM  WITH n_o, ;
          CODE     WITH INC_DUE.CODE, ;
          QNT      WITH q
  IF QNT < 0
    cnt_neg = cnt_neg+1
  ELSE
    cnt_pos = cnt_pos+1
  ENDIF
  SELECT INC_DUE
ENDSCAN

IF .NOT. EMPTY(n_list)		&& ���瓷� ━カ� �メ皰:
  DO Scan_Ord WITH n_list	&&   �爬〓え� � ��瑙ぎ� ��錢�� か� ��甄イ�ィ�
ENDIF

IF SEEK(key_v)
  DELETE REST WHILE INC_LIST = key_v
ENDIF

SELECT LIST_DET
SET ORDER TO TAG DUE_CODE

SELECT LIST_TIT
USE

SELECT ORD_D
USE

SELECT ORD_T
IF l_ord # 0
  REPLACE SIZE_NEG WITH cnt_neg, SIZE_POS WITH cnt_pos
ENDIF
USE

DO SendMess		&& �皴��→� 甌�♂キ┤

RETURN st_ret

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� SendMess     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �皴��→� 甌�♂キ┤                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.11.2005 陳�
PROCEDURE SendMess
PRIVATE s_sav, i, tmp_ID, tmpArm, tmpText, tmpTit

IF m.n_ord = 0
  RETURN
ENDIF

m.tmpTit = "��錢�� �� "+IIF(m.dueMode#"R", "÷���牀���┘", "爛Д燿�牀���┘")

m.s_sav = SELECT()
SELECT 0
USE base_path+"LETT_OUT" SHARED AGAIN ALIAS OUT_5B13
SELECT 0
USE base_path+"LETT_IN" SHARED AGAIN ALIAS IN_5B13 ORDER TAG MESS_ID

DO WHILE .T.
  m.sw      = .F.
  m.tmpArm  = ""
  m.tmpText = ""

  FOR m.i = 1 TO m.n_ord
    IF EMPTY(sendList[m.i, 1])
      LOOP
    ENDIF
    IF EMPTY(m.tmpArm)
      m.tmpArm  = sendList[m.i, 1]
      m.tmpText = sendList[m.i, 2]
      sendList[m.i, 1] = ""
      LOOP
    ENDIF
    IF m.tmpArm  = sendList[m.i, 1]
      m.tmpText = m.tmpText+sendList[m.i, 2]
      sendList[m.i, 1] = ""
    ENDIF
  ENDFOR
  IF EMPTY(m.tmpArm)
    EXIT
  ENDIF

  m.tmp_ID = DOC_NUM("LETT_ID", "", m.arm, DATE())
  m.tmp_ID = SUBSTR(DTOS(DATE()), 3, 2)+STR(m.tmp_ID, 10)
  SELECT OUT_5B13
  APPEND BLANK
  REPLACE MESS_ID	WITH m.tmp_ID, ;
          TITLE		WITH m.tmpTit, ;
          MESS_TEXT	WITH m.tmpText,;
          CR_DATE	WITH DATE(),   ;
          CR_TIME	WITH TIME(),   ;
          WHO		WITH m.user
  SELECT IN_5B13
  GO TOP
  IF .NOT. EMPTY(MESS_ID)
    APPEND BLANK
  ENDIF
  REPLACE MESS_ID	WITH m.tmp_ID, ;
          ADDRESS_T	WITH "*",      ;
          ADDRESS	WITH m.tmpArm, ;
          SHOW_DATE	WITH DATE()
ENDDO

USE IN IN_5B13
USE IN OUT_5B13

SELECT (m.s_sav)

RETURN
