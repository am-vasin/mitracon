*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                    蹍������ □��� ���皀Κ�.                          �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Inp_Pay
PARAMETER ext_menu, is_bill

PRIVATE new_cli, cli_nm, cli_cd, pay_note, pay_dat, pay_sum, ex, p_doc
PRIVATE ln, wd, dep, hld, nt, p_type, to_hold, b_sum

IF FOUND("ACCOUNT")
  IF .NOT. EMPTY(ACCOUNT.DATE_OFF)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"��┘�� 礒� 磴��キ!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN
  ENDIF
ELSE
  IF BILLS.CUS_CODE # 0 .AND. is_bill
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�モ ��┘��� � ���━ ��ぎ�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN
  ENDIF
ENDIF

IF is_bill .AND. ALLTRIM(BILLS.STATUS) = "ｯ��腑��"
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  �皰� 瘍モ 礒� ����腑�. �ョ���ガ ぎ�祠キ�? [ �� ] [ �モ ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 1
    DO P_Inc_O WITH BILLS.INC_DOC, BILLS.INC_DATE, BILLS.INC_TYPE
  ENDIF
  RETURN
ENDIF

DO Entry_P WITH ext_menu, ""

ON KEY LABEL LeftMouse DO Ms_Lock

dep = 0
hld = 0
nt  = SPACE(32)
to_hold = 0
b_sum = 0
IF is_bill
  to_hold = IIF(BILLS.NUM_LIST = -1, BILLS.BILL_SUM, 0)
  b_sum   = BILLS.BILL_SUM
  new_cli = EMPTY(BILLS.CUS_CODE)
  cli_nm  = IIF(FOUND("ACCOUNT"), ACCOUNT.CUS_NAME, BILLS.CUS_NAME)
  cli_cd  = BILLS.CUS_CODE
  pay_note= SPACE(16)
  pay_dat = DATE()
  pay_sum = BILLS.BILL_SUM
  IF FOUND("ACCOUNT")
    dep = ACCOUNT.ACC_SUM
    hld = ACCOUNT.ACC_S_H
    nt  = ACCOUNT.ACC_NOTE
  ENDIF
ELSE
  new_cli = .T.
  cli_nm  = SPACE(50)
  cli_cd  = 0
  pay_note= SPACE(16)
  pay_dat = DATE()
  pay_sum = 0
ENDIF

p_doc = .T.
ex = 1
p_type = 1

*                 踸絎� ぅ�ィ �� �/� ��瘍モ�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�           ( ) �/���.   ( ) 踸�. �爐ム   ( ) ���.            �
*�  敖 ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕  �
*�  �      [ ] ���覃                                         �  �
*�  � ��� 旭旭碓旭芦旭旭碓旭芦旭旭碓旭芦旭旭碓旭芦旭旭碓旭芦 �  �
*�  青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰  �
*� ����. ��珮腑�┘ 旭旭碓旭芦旭旭碓   ���� ��痰. ぅ�ィ ��.��.�� �
*楽陳 �祠�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕�
*崖 �� 瘍モ� 旭旭碓旭芦  踸絎� 旭旭碓旭芦  ��ぅ爨. 旭旭碓旭芦 崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*楽陳 �ク�Ж� 陳 ��ぅ爨. 陳陳 踸�ョ��┘ 陳陳陳陳陳陳陳陳陳陳陳審
*崖 旭旭碓旭芦 旭旭碓旭芦 旭旭碓旭芦旭旭碓旭芦旭旭碓旭芦旭旭碓 崖
*垣陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳抒
*�                     [ ] �ョ�碎 ぎ�祠キ��                     �
*�        < ｯ爬絎ぎ��碎 Ctrl-W > < �皖���碎瘴 Esc >            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
ln = 17
wd = 66
DO D_W WITH "Inp_Pay", ln, wd, "踸絎� ぅ�ィ �� �/� ��瘍モ�", 1

@  3, 4 TO 6, WCOLS()-5
@  3, 6 SAY " ��┘�� "
@  8, 2 TO 10, WCOLS()-3
@  8, 5 SAY " �祠�� "
@ 11, 2 TO 13, 63
@ 11, 5 SAY " �ク�Ж� "
@ 11,16 SAY " ��ぅ爨. "
@ 11,29 SAY " 踸�ョ��┘ "

IF new_cli
  @  2,13 GET p_type PICTURE "@*RH �/���.  ;踸�. �爐ム  ;���."  ;
                     VALID Put_Dep()
  @  4,11 GET new_cli PICTURE "@*C ���覃" VALID Ch_Cust()
  @  5, 6 SAY "���" GET cli_nm WHEN new_cli
ENDIF

@  7, 3 SAY "����. ��珮腑�┘" GET pay_note
@  7,38 SAY "���� ��痰. ぅ�ィ" GET pay_dat PICTURE "@D"
@  9, 4 SAY "�� 瘍モ�" GET b_sum PICTURE "@Z 9999999.99" WHEN .F.
@  9,25 SAY "踸絎�" GET pay_sum PICTURE "@Z 9999999.99" VALID To_Hold()
@  9,44 SAY "��ぅ爨." GET to_hold PICTURE "@Z 9999999.99"
@ 12, 4 GET dep PICTURE "@Z 9999999.99" WHEN .F.
@ 12,15 GET hld PICTURE "@Z 9999999.99" WHEN .F.
@ 12,26 GET nt
@ 14,23 GET p_doc PICTURE "@*C �ョ�碎 ぎ�祠キ��"
@ 15,10 GET ex PICTURE "@*HT \ ｯ爬絎ぎ��碎 Ctrl-W ;\? �皖���碎瘴 Esc "

IF .NOT. new_cli
  @  2,13 GET p_type PICTURE "@*RH �/���.  ;踸�. �爐ム  ;���." ;
                     VALID Put_Dep()
  @  4,11 GET new_cli PICTURE "@*C ���覃" VALID Ch_Cust()
  @  5, 6 SAY "���" GET cli_nm WHEN new_cli
ENDIF

READ CYCLE VALID Tst_Blank()

RELEASE WINDOW Inp_Pay

DO Exit_P WITH ext_menu, ""

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ �襦��� ��痰�鎰���� ��┘���.                  �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Ch_Cust
PRIVATE sel_rem

IF new_cli
  dep = 0
  hld = 0
  nt  = SPACE(16)
  SHOW GET dep
  SHOW GET hld
  SHOW GET nt
  RETURN .T.
ENDIF
sel_rem = SELECT()
SELECT ACCOUNT
IF is_bill
  SELECT ACCOUNT
  SET FILTER TO SUBSYS = LEFT(BILLS.DOC_NUM,1)
ENDIF

IF Custs("", .F., .F., .F., pl_code)
  ACTIVATE WINDOW Inp_Pay
  cli_nm = ACCOUNT.CUS_NAME
  dep = IIF(p_type = 3, ACCOUNT.ACC_SUM0, ACCOUNT.ACC_SUM)
  hld = IIF(p_type = 3, ACCOUNT.ACC_S0_H, ACCOUNT.ACC_S_H)
  nt  = IIF(p_type = 3, ACCOUNT.ACC_NOTE0, ACCOUNT.ACC_NOTE)
  SHOW GET dep
  SHOW GET hld
  SHOW GET nt
  SHOW GET cli_nm
ELSE
  ACTIVATE WINDOW Inp_Pay
  new_cli = .T.
  dep = 0
  hld = 0
  nt  = SPACE(16)
  SHOW GET dep
  SHOW GET hld
  SHOW GET nt
  SHOW GET new_cli
ENDIF

IF is_bill
  SELECT ACCOUNT
  SET FILTER TO
ENDIF
SELECT (sel_rem)
RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                蹍������ �諷��� ├ 蹣������ 筮爼�.                    �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Tst_Blank

PRIVATE mss, w_s, d_n, p_stat, sums, doc_cnt, sum_w, rst, j000

IF ex = 2
  RETURN .T.
ENDIF

IF new_cli .AND. EMPTY(cli_nm)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��┘�� � �竅硅� ━キガ? �� ��Ε� °碎!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

*IF pay_sum <= 0
*  DIMENSION mss(3)
*  mss(1) = ""
*  mss(2) = CHR(0)+"�皰膈�皀, 瓷��讓� Ε �� ����皋�."
*  mss(3) = ""
*  DO Out_Mess WITH 7, "mss"
*  RETURN .F.
*ENDIF
PRIVATE sel_sv

sel_sv = SELECT()
IF new_cli
  SELECT 0
  USE PARMS
  cli_cd = LAST_CUS+1
  REPLACE LAST_CUS WITH cli_cd
  USE
  SELECT ACCOUNT
  APPEND BLANK
  REPLACE SUBSYS   WITH pl_code,;
          CUS_CODE WITH cli_cd, ;
          CUS_NAME WITH cli_nm, ;
          DATE_ON  WITH DATE(), ;
          WHO      WITH user,   ;
          WHERE    WITH arm
ENDIF  
SELECT ACCOUNT

IF p_type = 3
  w_s = ACC_SUM0-to_hold+pay_sum
  REPLACE ACC_SUM0  WITH w_s, ;
          ACC_S0_H  WITH ACC_S0_H+to_hold, ;
          ACC_NOTE0 WITH nt
ELSE
  w_s = ACC_SUM-to_hold+pay_sum
  REPLACE ACC_SUM  WITH w_s, ;
          ACC_S_H  WITH ACC_S_H+to_hold, ;
          ACC_NOTE WITH nt
ENDIF
cli_cd  = CUS_CODE
cli_nam = CUS_NAME

IF pay_sum # 0
  IF max_ord # 0 .AND. pay_sum > max_ord .AND. p_type # 1
    doc_cnt = 0
    rst = pay_sum
    DO WHILE rst # 0
      IF rst < max_ord
        sum_w = rst
      ELSE
        sum_w = max_ord-ROUND(0.1*RAND()*max_ord,0)
      ENDIF
      rst = rst-sum_w
      doc_cnt = doc_cnt+1
      DIMENSION sums(doc_cnt)
      sums(doc_cnt) = sum_w
    ENDDO
  ELSE
    DIMENSION sums(1)
    sums(1) = pay_sum
    doc_cnt = 1
  ENDIF
  SELECT 0
  USE PARMS
  DO CASE
  CASE p_type = 1     && �/�
    p_type = "1"
    ln = LAST_BN
    REPLACE LAST_BN WITH LAST_BN+doc_cnt
  CASE p_type = 2     && 踸�. �爐ム
    p_type = " "
    ln = LAST_ORDR
    REPLACE LAST_ORDR WITH LAST_ORDR+doc_cnt
  CASE p_type = 3     && ���.
    p_type = "2"
    ln = LAST_ORD0
    REPLACE LAST_ORD0 WITH LAST_ORD0+doc_cnt
  ENDCASE
  USE INCMONEY
  d_n = pl_code+STR(ln+doc_cnt,9)
  FOR j000 = 1 TO doc_cnt
    APPEND BLANK
    REPLACE DOC_TYPE  WITH p_type,      ;  
            DOC_NUM   WITH pl_code+STR(ln+j000,9),           ;
            DOC_DATE  WITH DATE(),      ;
            DOC_TIME  WITH SECONDS(),   ;
            BILL_NUM  WITH IIF(is_bill, BILLS.DOC_NUM, " "), ;
            BILL_DAT  WITH IIF(is_bill, BILLS.DOC_DATE, {}), ;
            ENTER_DOC WITH pay_note,  ;
            ENTER_DAT WITH IIF(p_type="1", pay_dat, DATE()-1+j000),  ;
            CUS_CODE  WITH ACCOUNT.CUS_CODE,    ;
            CUS_NAME  WITH ACCOUNT.CUS_NAME,    ;
            MONEY_R   WITH sums(j000),          ;
            MONEY_D   WITH ROUND(sums(j000)/kurs_b,4), ;
            WHO       WITH user,   ;
            WHERE     WITH arm
  ENDFOR
  IF p_type # "1"
    USE ARM_LST ORDER TAG ARM_C
    SEEK arm
    REPLACE KASSA WITH KASSA+pay_sum
  ENDIF
  USE
ENDIF

SELECT 0
IF .NOT. EMPTY(BILLS.NUM_LIST) .AND. is_bill .AND. LEFT(BILLS.DOC_NUM,1) = sys_char
  USE LIST_TIT ORDER TAG LST_NUM
  IF SEEK(BILLS.NUM_LIST)
    IF EMPTY(SAL_NUM)
      REPLACE CLI_CODE WITH cli_cd
    ENDIF
  ENDIF
*  USE
*  SELECT INCMONEY
ENDIF

USE

IF is_bill
  SELECT BILLS
  p_stat = "ｯ��腑��"
  IF pay_sum < BILL_SUM
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�皰 ������皀�讚�� ������? [ �� ] [ �モ ]"
    mss(3) = ""
    p_stat = IIF(Out_Mess(5, "mss") = 1, "ｯ��腑��", "踳ぎ��.")
  ENDIF
  REPLACE INC_TYPE WITH p_type,    ;
          INC_DOC  WITH d_n,       ;
          INC_DATE WITH DATE(),    ;
          CUS_CODE WITH cli_cd,    ;
          DOC_TIME WITH SECONDS(), ;
          STATUS   WITH p_stat
ENDIF
IF p_doc
  FOR j000 = 1 TO doc_cnt
    DO P_Inc_O WITH pl_code+STR(ln+j000,9), DATE(), p_type
  ENDFOR
ENDIF
SELECT (sel_sv)

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�             蹍������ 竅����→� ぅ��Ж�� �爬 甃キ� 皋��.              �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Put_Dep

IF new_cli
  dep = 0
  hld = 0
  nt  = SPACE(LEN(nt))
ELSE
  IF p_type = 3
    dep = ACCOUNT.ACC_SUM0
    hld = ACCOUNT.ACC_S0_H
    nt  = ACCOUNT.ACC_NOTE0
  ELSE
    dep = ACCOUNT.ACC_SUM
    hld = ACCOUNT.ACC_S_H
    nt  = ACCOUNT.ACC_NOTE
  ENDIF
ENDIF

SHOW GET dep
SHOW GET hld
SHOW GET nt

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                   ��ぅ爨│�ガ か� 瘍モ�� �. �.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE To_Hold

IF is_bill .AND. BILLS.NUM_LIST = -1
  to_hold = pay_sum
  SHOW GET to_hold
ENDIF

RETURN .T.
