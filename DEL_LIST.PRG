*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                     �ணࠬ�� 㤠����� ᯨ᪠.                        �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Del_List
PARAMETER pMode

PRIVATE sel_rem, k00, ord_n, c1, c2, mss, mode, pr_d, queue_t
PRIVATE ord_len, cnt, c_zone, tmp_zone, tmpArr, cnt_neg, cnt_pos
ord_len = 20      && ���ᨬ��쭠� ����� ���.

sel_rem = SELECT()
IF .NOT. Tst_Hard(LIST_TIT.LST_NUM)
  SELECT (sel_rem)
  RETURN
ENDIF

IF TYPE("pMode") = "N"
  mode = pMode
ELSE
  IF "�⪠��" $ LST_NAME .OR. "�����" $ LST_NAME
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"� ��, ����⢨⥫쭮, 㤠�塞?"
    mss(3) = CHR(0)+"  [ ��, 㤠�塞! ] [ ��������! ]"
    mss(4) = ""
    mode = Out_Mess(7, "mss")+1
  ELSE    
    IF EMPTY(SAL_NUM)
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"� ��, ����⢨⥫쭮, 㤠�塞, � ���?"
      mss(3) = CHR(0)+"   [ ������ �⪠�����... ] [ ���� 㤠�塞 � ��! ] [ ��������! ]"
      mss(4) = ""
      mode = Out_Mess(7, "mss")
    ELSE
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"� ��, ����⢨⥫쭮, 㤠�塞?"
      mss(3) = CHR(0)+"  [ ��, 㤠�塞! ] [ ��������! ]"
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
*  �஭�?
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
      mss(2) = CHR(0)+"���᮪ ᮤ�ন� १��. ��� ����� 㤠����!"
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
*   ����諨 �� ᯨ��
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
    w = -QNT_REQ      && ����������!
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
IF EMPTY(SAL_NUM)                  && ���� ��ନ஢���
  IF RECCOUNT("REQ_TMP") # 0       &&       ����ৠ�
    REPLACE ORD_NUM WITH ord_n
    ord_n = 0
    SELECT 0
    USE (base_path+"ORD_D")                         && ����ন��� ���
    SELECT 0
    USE (base_path+"ORD_T") ORDER TAG DOC_NUM       && ��������� ���
    SELECT 0
    USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_ORD AGAIN
    SELECT REQ_TMP
    cnt = 0
    cnt_neg = 0
    cnt_pos = 0
    SCAN                           && ���४�� �஭� �� ᪫���
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
    mss(2) = CHR(0)+"��������! �� �� ��������� ���⠫��� � ᪫���� �� ᯨ�� "+ALLTRIM(STR(LIST_TIT.LST_NUM,10))+"."
    mss(3) = CHR(0)+"��ନ஢��� ��� N "+ALLTRIM(STR(LIST_TIT.LST_NUM,10))+"/"+ ;
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� To_Del       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�               ���⢥ত���� �� 㤠����� ᯨ᪠ � �஭��                �
*�                                                                        �
*����������������������������������������������������������� 17.01.2001 ���
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
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*
*���᮪ � �஭��
*��������������������������������������������������������Ŀ
*�       N 999999 xxxx!xxxxXxxxx!xxxxXxxxx!xxxxXxx        �
*� �� ������ ������������������������������������������Ŀ �
*� � xxxx!xxxxXxxxx!xxxxXxxxx!xxxxXxxxx!xxxxXxxxx!xxxxX � �
*� ������������������������������������������������������ �
*�  �ਬ�砭�� xxxx!xxxxXxxxx!xxxxXxxxx!xxxxXxxxx!xxxxX   �
*�         < ������� Ctrl-W > < �⪠������ Esc >          �
*����������������������������������������������������������

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 9, 60, "�������! �� 㤠��� ᯨ᮪ � �஭��!"
DO Sun_Bord WITH  3,  3,  5, 56, " ������ "

*------------------------------------------------------------------------
*      ���� ����� ������
*
@  2,  9 SAY "N" GET LIST_TIT.LST_NUM PICTURE "999999" WHEN .F.
@  2, 18 GET LIST_TIT.LST_NAME WHEN .F.
@  4,  5 GET tmpCusN WHEN .F.
@  6,  4 SAY "�ਬ�砭��" GET tmpNote
@ WROWS()-2, 11 GET ex PICTURE "@*HT \! ������� Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Emp_Date     ���ࠡ��稪 ������ ����ਥ���                 �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                       ����� ��� ����� ���.                       �
*�                                                                        �
*����������������������������������������������������������� 13.03.2002 ���
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

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Tst_hard     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�   �஢�ઠ �����⨬��� 㤠����� ᯨ᪠ �� ���⪮� ०��� �த���.    �
*�                                                                        �
*����������������������������������������������������������� 06.11.2005 ���
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
      mss[2] = CHR(0)+"���᮪ ᮤ�ন� �஭�!"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      USE
      SELECT (m.s_sav)
      RETURN .F.
    ENDIF
    IF QNT_DUE # 0
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"���᮪ ᮤ�ন� ����!"
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
      mss[2] = CHR(0)+"�� �� ��� �⢥ত��� �� ᪫���!"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      USE
      SELECT (m.s_sav)
      RETURN .F.
    ELSE	&& ���쪮 १�� - �⢥ত��� ᠬ�
      REPLACE STO_MARK WITH m.user, ;
              REQ_DATE WITH DATE()
    ENDIF
  ENDIF
  IF EMPTY(WHO)
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"�� �� ��� �⢥ত��� �������஬!"
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