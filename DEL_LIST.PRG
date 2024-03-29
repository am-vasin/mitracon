*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                     蹍������ 磴��キ�� 甎�瓷�.                        �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Del_List
PARAMETER pMode

PRIVATE sel_rem, k00, ord_n, c1, c2, mss, mode, pr_d, queue_t
PRIVATE ord_len, cnt, c_zone, tmp_zone, tmpArr, cnt_neg, cnt_pos
ord_len = 20      && ���瓱���讚�� か┃� ��錢��.

sel_rem = SELECT()
IF .NOT. Tst_Hard(LIST_TIT.LST_NUM)
  SELECT (sel_rem)
  RETURN
ENDIF

IF TYPE("pMode") = "N"
  mode = pMode
ELSE
  IF "�皖�щ" $ LST_NAME .OR. "ぅ筥罔�" $ LST_NAME
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"� 艪�, ぅ�痰※皀�讚�, 磴��錺�?"
    mss(3) = CHR(0)+"  [ ��, 磴��錺�! ] [ �ム�竄赱�! ]"
    mss(4) = ""
    mode = Out_Mess(7, "mss")+1
  ELSE    
    IF EMPTY(SAL_NUM)
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"� 艪�, ぅ�痰※皀�讚�, 磴��錺�, � ���?"
      mss(3) = CHR(0)+"   [ ��┘�� �皖����瘴... ] [ 蹍痰� 磴��錺� � ≡�! ] [ �ム�竄赱�! ]"
      mss(4) = ""
      mode = Out_Mess(7, "mss")
    ELSE
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"� 艪�, ぅ�痰※皀�讚�, 磴��錺�?"
      mss(3) = CHR(0)+"  [ ��, 磴��錺�! ] [ �ム�竄赱�! ]"
      mss(4) = ""
      mode = Out_Mess(7, "mss")+1
    ENDIF
  ENDIF
ENDIF

IF mode = 3
  RETURN
ENDIF
PUSH KEY CLEAR
DEFINE WINDOW Hide_M FROM 0, 0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW Hide_M

queue_t = LIST_TIT.ORD_QUEUE
ord_n = LIST_TIT.ORD_NUM
IF EMPTY(LIST_TIT.SAL_NUM)
  SELECT 0
  DELETE FILE (tmpo_path+"REQ_TMP.DBF")
  USE (base_path+"ORD_D")
  COPY STRUCTURE TO (tmpo_path+"REQ_TMP")
  USE (tmpo_path+"REQ_TMP") EXCLUSIVE
  ZAP
ENDIF

*IF File_O(base_path+"ZONES.DBF")
*  SELECT 0
*  USE (base_path+"ZONES") ORDER TAG ZONE
*ENDIF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_TMP AGAIN

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG LISTPURE ALIAS LIST_TMP AGAIN

k00 = STR(LIST_TIT.LST_NUM,6)

DO Emp_Date
*
*  �牀��?
PRIVATE tmpNote, is_bron
tmpNote = SPACE(40)
is_bron = .F.
IF EMPTY(LIST_TIT.SAL_NUM)
  k00 = STR(LIST_TIT.LST_NUM,6)
  SEEK k00
  SCAN REST WHILE LIST_NUM = k00
    IF QNT_REZ > 0
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"���甌� 甌ぅ爨�� 爛Д燿. ��� �カ讌� 磴��閧�!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      USE
      IF USED("REQ_TMP")
        SELECT REQ_TMP
        USE
        DELETE FILE (tmpo_path+"REQ_TMP.DBF")
      ENDIF
      IF USED("ZONES")
        SELECT ZONES
        USE
      ENDIF
      SELECT STO_TMP
      USE
      SELECT LIST_TMP
      USE
      SELECT (sel_rem)
      RETURN
    ENDIF
    IF QNT_REQ > 0
      is_bron = .T.
      EXIT
    ENDIF
  ENDSCAN
  IF is_bron
    IF .NOT. To_Del()
      IF USED("ZONES")
        SELECT ZONES
        USE
      ENDIF
      SELECT STO_TMP
      USE
      SELECT LIST_TMP
      USE
      SELECT REQ_TMP
      USE
      DELETE FILE (tmpo_path+"REQ_TMP.DBF")
      SELECT (sel_rem)
      RELEASE WINDOW Hide_M
      RETURN
    ENDIF
  ENDIF
ENDIF

SEEK k00

*
*   ����茫� �� 甎�瓷�
*
c1 = 0
c2 = 0
c_zone = CHR(0)+CHR(0)
cnt = ord_len
cnt_neg = 0
cnt_pos = 0
SCAN REST WHILE LIST_NUM = k00
  = SEEK(CODE, "STO_TMP")
  IF EMPTY(LIST_TIT.SAL_NUM)
    w = -QNT_REQ      && ��������碎!
    IF w # 0
      tmp_zone = WhatZone(STO_TMP.ADDRESS)
      IF c_zone # tmp_zone .OR. cnt >= ord_len
        ord_n = ord_n+1
        cnt = 0
        cnt_neg = 0
        cnt_pos = 0
        c_zone = tmp_zone
      ELSE
        cnt = cnt+1
      ENDIF

      SELECT REQ_TMP
      APPEND BLANK
      REPLACE LIST_NUM WITH k00,     ;
              ORD_NUM  WITH ord_n,   ;
              CODE     WITH LIST_TMP.CODE, ;
              QNT      WITH w
      SELECT LIST_TMP
    ENDIF
  ENDIF
  IF USED("REFUSAL") .AND. mode = 1
    pr_d = IIF(FOUND("STO_TMP"),   ;
                       STO_TMP.PRICE, LIST_TMP.SAL_PRICE/kurs)
    SELECT REFUSAL
    APPEND BLANK
    REPLACE STO_CODE  WITH LIST_TMP.CODE,      ;
            PREFIX    WITH LIST_TMP.PREFIX,    ;
            NAME      WITH LIST_TMP.NAME,      ;
            PRO_CODE  WITH LIST_TMP.PRO_CODE,  ;
            PRODUCER  WITH LIST_TMP.PRODUCER,  ;
            DATE      WITH DATE(),             ;
            QTY       WITH LIST_TMP.QNT,       ;
            PRICE     WITH LIST_TMP.SAL_PRICE, ;
            PRICE_D   WITH pr_d,               ;
            CUS_CODE  WITH LIST_TIT.CLI_CODE,  ;
            CLIENT    WITH LIST_TIT.LST_NAME,  ;
            FUNCTION  WITH LIST_TMP.FUNCTION,  ;
            AMODEL    WITH LIST_TMP.AMODEL,    ;
            WHO       WITH user,               ;
            WHERE     WITH arm
    SELECT LIST_TMP
  ENDIF
  DELETE
ENDSCAN
USE
SELECT STO_TMP
USE
SELECT LIST_TIT
IF EMPTY(SAL_NUM)                  && ��ぎ 痲�爼�牀��碎
  IF RECCOUNT("REQ_TMP") # 0       &&       ���矗��錢��
    REPLACE ORD_NUM WITH ord_n
    ord_n = 0
    SELECT 0
    USE (base_path+"ORD_D")                         && ��ぅ爨━�� ��錢��
    SELECT 0
    USE (base_path+"ORD_T") ORDER TAG DOC_NUM       && ������〓� ��錢��
    SELECT 0
    USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_ORD AGAIN
    SELECT REQ_TMP
    cnt = 0
    cnt_neg = 0
    cnt_pos = 0
    SCAN                           && ��玻オ罔� ÷��� �� 瓷��ぅ
      IF .NOT. SEEK(CODE,"STO_ORD")
        LOOP
      ENDIF
      SELECT STO_ORD
      REPLACE QNT_SAVE WITH QNT_SAVE+REQ_TMP.QNT,  ;
              QNT      WITH QNT-REQ_TMP.QNT
      SELECT REQ_TMP
      SCATTER TO tmpArr
      IF ord_n # ORD_NUM
        SELECT ORD_T
        IF ord_n # 0
          REPLACE SIZE_NEG WITH cnt_neg, SIZE_POS WITH cnt_pos
        ENDIF
        cnt = 0
        cnt_neg = 0
        cnt_pos = 0
        ord_n = REQ_TMP.ORD_NUM
        APPEND BLANK
        REPLACE LIST_NUM  WITH k00,  ;
                LIST_NAME WITH LIST_TIT.LST_NAME, ;
                ZONE      WITH WhatZone(STO_ORD.ADDRESS), ;
                ORD_NUM   WITH ord_n,      ;
                ORD_DATE  WITH DATE(),     ;
                ORD_TIME  WITH TIME(),     ;
                NOTE      WITH tmpNote,    ;
                QUEUE     WITH queue_t,    ;
                WHO       WITH user,       ;
                WHERE     WITH arm
      ENDIF
      cnt = cnt+1
      IF REQ_TMP.QNT < 0
        cnt_neg = cnt_neg+1
      ELSE
        cnt_neg = cnt_pos+1
      ENDIF
      SELECT ORD_D
      APPEND FROM ARRAY tmpArr
      SELECT REQ_TMP
    ENDSCAN
    SELECT ORD_T
    IF cnt # 0
      REPLACE SIZE_NEG WITH cnt_neg, SIZE_POS WITH cnt_pos
    ENDIF
    SEEK k00
    SCAN REST WHILE LIST_NUM = k00 FOR EMPTY(REQ_DATE)
      REPLACE REQ_DATE WITH DATE(), REQ_TIME WITH TIME()
    ENDSCAN
    USE
    SELECT ORD_D
    USE
    SELECT STO_ORD
    USE
    SELECT LIST_TIT
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"��������! �� �� �����痰跪 ��瘍�����瘡 甌 瓷��ぎ� �� 甎�瓷� "+ALLTRIM(STR(LIST_TIT.LST_NUM,10))+"."
    mss(3) = CHR(0)+"�筮爼�牀���� ��錢�� N "+ALLTRIM(STR(LIST_TIT.LST_NUM,10))+"/"+ ;
                                             ALLTRIM(STR(LIST_TIT.ORD_NUM,10))
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
ENDIF
DELETE
IF USED("ZONES")
  SELECT ZONES
  USE
ENDIF
IF USED("REQ_TMP")
  SELECT REQ_TMP
  USE
ENDIF
DELETE FILE (tmpo_path+"REQ_TMP.DBF")
POP KEY
RELEASE WINDOW Hide_M
IF USED("DEMAND")
  IF USED("DEM_LINK")
    SELECT DEM_LINK
    DO WHILE SEEK(LIST_TIT.LST_NUM, "DEM_LINK")
      REPLACE LST_NUM WITH 0
    ENDDO
  ENDIF
ENDIF
SELECT (sel_rem)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� To_Del       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�               ��も▲爨ぅ�┘ �� 磴��キ┬ 甎�瓷� � ÷��跪                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 17.01.2001 陳�
PROCEDURE To_Del

PRIVATE s_sav, tmpCusN

IF hard_sale
  RETURN .F.
ENDIF
s_sav = SELECT()
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS TMPC_1117 AGAIN
tmpCusN = SPACE(50)
IF .NOT. EMPTY(LIST_TIT.CLI_CODE)
  IF SEEK(LIST_TIT.CLI_CODE)
    tmpCusN = TMPC_1117.CUS_NAME
  ENDIF
ENDIF
USE
tmpNote = PADR(LIST_TIT.ADDRESS, 40)

SELECT (s_sav)

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
*���甌� � ÷��跪
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�       N 999999 xxxx!xxxxXxxxx!xxxxXxxxx!xxxxXxx        �
*� 敖 ��┘�� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� � xxxx!xxxxXxxxx!xxxxXxxxx!xxxxXxxxx!xxxxXxxxx!xxxxX � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�  踸�ョ��┘ xxxx!xxxxXxxxx!xxxxXxxxx!xxxxXxxxx!xxxxX   �
*�         < �����碎 Ctrl-W > < �皖���碎瘴 Esc >          �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 60, "�������! �� 磴��錺皀 甎�甌� � ÷��跪!"
DO Sun_Bord WITH  3,  3,  5, 56, " ��┘�� "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@  2,  9 SAY "N" GET LIST_TIT.LST_NUM PICTURE "999999" WHEN .F.
@  2, 18 GET LIST_TIT.LST_NAME WHEN .F.
@  4,  5 GET tmpCusN WHEN .F.
@  6,  4 SAY "踸�ョ��┘" GET tmpNote
@ WROWS()-2, 11 GET ex PICTURE "@*HT \! �����碎 Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Emp_Date     ��о�｀砒┴ ���┃� ���矗┘���                 �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       鉑��� ���� ����腮 ��錢��.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 13.03.2002 陳�
PROCEDURE Emp_Date
PRIVATE m_sav
m_sav = SELECT()

SELECT LIST_TIT
IF .NOT.EMPTY(ORD_NUM) .AND. EMPTY(DATE_REQ)
  REPLACE DATE_REQ WITH DATE(), ;
          TIME_REQ WITH TIME()
  SELECT 0
  USE(base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS OT_2313 AGAIN
  SEEK k00
  SCAN REST WHILE LIST_NUM = k00
    IF EMPTY(REQ_DATE)
      REPLACE REQ_DATE WITH LIST_TIT.DATE_REQ, ;
              REQ_TIME WITH LIST_TIT.TIME_REQ
    ENDIF  
  ENDSCAN
  USE
ENDIF

SELECT(m_sav)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_hard     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�   蹍▲爲� ぎ�竅皋��痰� 磴��キ�� 甎�瓷� �爬 Ε痰��� 爛Θ�� �牀��Θ.    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 06.11.2005 陳�
PROCEDURE Tst_hard
PARAMETERS prmList

PRIVATE s_sav, tmpN, mss
IF .NOT. hard_sale
  RETURN .T.
ENDIF

m.s_sav = SELECT()

SELECT 0
m.tmpN = STR(m.prmList, 6)
IF EMPTY(LIST_TIT.SAL_NUM)
  USE (m.base_path+"LIST_DET") SHARED AGAIN ORDER TAG LIST ALIAS TMP5B06
  SEEK m.tmpN
  SCAN REST WHILE LIST_NUM = m.tmpN
    IF QNT_REQ # 0
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"���甌� 甌ぅ爨�� ÷���!"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      USE
      SELECT (m.s_sav)
      RETURN .F.
    ENDIF
    IF QNT_DUE # 0
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"���甌� 甌ぅ爨�� ぎ��!"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      USE
      SELECT (m.s_sav)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF
USE (m.base_path+"ORD_T") SHARED AGAIN ORDER TAG DOC_NUM ALIAS TMP5B06
SEEK m.tmpN
SCAN REST WHILE LIST_NUM = m.tmpN
  IF EMPTY(STO_MARK)
    IF .NOT. EMPTY(REQ_DATE)
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"�� ≡� ��錢�� 竄▲爨ぅ�� �� 瓷��ぅ!"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      USE
      SELECT (m.s_sav)
      RETURN .F.
    ELSE	&& ���讓� 爛Д燿 - 竄▲爨��ガ ����
      REPLACE STO_MARK WITH m.user, ;
              REQ_DATE WITH DATE()
    ENDIF
  ENDIF
  IF EMPTY(WHO)
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"�� ≡� ��錢�� 竄▲爨ぅ�� �キイΕ牀�!"
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (m.s_sav)
    RETURN .F.
  ENDIF
ENDSCAN
USE
SELECT (m.s_sav)
RETURN .T.
