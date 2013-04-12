*������������������������������������������������������������������������ͻ
*� ���� Rep_List     ���ࠡ��稪 ���३ ��ᨭ           10.10.97 17:02:10 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                    ��������� ���⮣� �� ॠ������.                    �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Rep_List

DO Prep_Lst   && �����⮢�� ������
DO View_Lst   && ��ᬮ�� ������

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                           �����⮢�� ������.                           �
*�                                                                        �
*����������������������������������������������������� 10.10.97 17:03:10 ͼ
PROCEDURE Prep_Lst
PRIVATE sup_rem, sr0, sd0, sr, sr0s, sd0s, srs, n_q, vlt
PRIVATE qd, qds

DELETE FILE (tmpo_path+"R_LIST_T.DBF")
DELETE FILE (tmpo_path+"R_LIST_T.CDX")
CREATE DBF (tmpo_path+"R_LIST_T.DBF") ;
    (  SUP_CODE  N( 8),  ;
       SUP_NAME  C(50),  ;
       VALUTA    C( 1),  ;
       IS_STO    L,      ;
       SUM_R0    N(15,2),;
       SUM_D0    N(15,4),;
       SUM_R0S   N(15,2),;
       SUM_D0S   N(15,4),;
       SUM_R     N(15,2),;
       SUM_RS    N(15,2) )
INDEX ON SUP_NAME TAG NAME

SELECT 0
DELETE FILE (tmpo_path+"R_LIST_D.DBF")
DELETE FILE (tmpo_path+"R_LIST_D.CDX")
CREATE DBF (tmpo_path+"R_LIST_D.DBF") ;
    (  SUP_CODE  N( 8),  ;
       CODE      N( 7),  ;
       PREFIX    C( 4),  ;
       NAME      C(35),  ;
       PRODUCER  C( 8),  ;
       QNT       N( 6),  ;
       QNT_SALED N( 6),  ;
       SUM_R0    N(12,2),;
       SUM_D0    N(12,4),;
       SUM_R0S   N(12,2),;
       SUM_D0S   N(12,4),;
       SUM_RS    N(12,2),;
       SUM_R     N(12,2) )
INDEX ON STR(SUP_CODE,8)+NAME+PREFIX+PRODUCER TAG NAME ADDITIVE
INDEX ON STR(SUP_CODE,8)+STR(CODE,7) TAG CODE ADDITIVE

SELECT 0
USE (base_path+"PARMS")
vlt = VALUTA

USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ACT_S") ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK

SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG SUP_CODE

STORE 0 TO sr0, sd0, sr0s, sd0s, sr, srs, n_q
sup_rem = 0
SCAN FOR FLG_BUY = "R"
  IF sup_rem # SUP_CODE
    IF n_q # 0
      = SEEK(sup_rem,"ACCOUNT")
      SELECT R_LIST_T
      APPEND BLANK
      REPLACE SUP_CODE WITH sup_rem,  ;
              SUP_NAME WITH ACCOUNT.CUS_NAME,  ;
              VALUTA   WITH vlt,  ;
              SUM_R0   WITH sr0,  ;
              SUM_D0   WITH sd0,  ;
              SUM_R0S  WITH sr0s, ;
              SUM_D0S  WITH sd0s, ;
              SUM_RS   WITH srs,  ;
              SUM_R    WITH sr
    ENDIF
    sup_rem = ACT_S_T.SUP_CODE
    STORE 0 TO sr0, sd0, sr0s, sd0s, sr, srs, n_q
  ENDIF
  SELECT ACT_S
  SEEK ACT_S_T.FIRM+LEFT(DTOS(ACT_S_T.DOC_DATE),4)+ACT_S_T.DOC_NUM
  SCAN REST WHILE ACT_S_T.FIRM=FIRM .AND. ACT_S_T.DOC_DATE=DOC_DATE .AND. ACT_S_T.DOC_NUM=DOC_NUM ;
            FOR QNT_REP # 0
    SELECT R_LIST_D
    IF .NOT. SEEK(STR(sup_rem,8)+STR(ACT_S.CODE,7))
      APPEND BLANK
      REPLACE SUP_CODE  WITH sup_rem,     ;
              CODE      WITH ACT_S.CODE,  ;
              PREFIX    WITH STOCK.PREFIX,;
              NAME      WITH STOCK.NAME,  ;
              PRODUCER  WITH STOCK.PRODUCER
    ENDIF
    n_q = n_q+1
    qd  = ACT_S.QNT_REP
    qds = ACT_S.QNT_REP-ACT_S.REST_REP
    REPLACE QNT       WITH QNT+qd, ;
            QNT_SALED WITH QNT_SALED+qds, ;
            SUM_R0    WITH SUM_R0+qd*STOCK.PRICE_B_R, ;
            SUM_D0    WITH SUM_D0+qd*STOCK.PRICE_B,   ;
            SUM_R0S   WITH SUM_R0S+qds*STOCK.PRICE_B_R, ;
            SUM_D0S   WITH SUM_D0S+qds*STOCK.PRICE_B, ;
            SUM_RS    WITH SUM_RS+qds*ACT_S.PRICE_R, ;
            SUM_R     WITH SUM_R+qd*ACT_S.PRICE_R
    sr0  = sr0+qd*STOCK.PRICE_B_R
    sd0  = sd0+qd*STOCK.PRICE_B
    sr0s = sr0s+qds*STOCK.PRICE_B_R
    sd0s = sd0s+qds*STOCK.PRICE_B
    sr   = sr+qd*ACT_S.PRICE_R
    srs  = srs+qds*ACT_S.PRICE_R
    SELECT ACT_S
  ENDSCAN
  SELECT ACT_S_T
ENDSCAN
IF n_q # 0
  = SEEK(sup_rem,"ACCOUNT")
  SELECT R_LIST_T
  APPEND BLANK
  REPLACE SUP_CODE WITH sup_rem,  ;
          SUP_NAME WITH ACCOUNT.CUS_NAME,  ;
          VALUTA   WITH vlt,  ;
          SUM_R0   WITH sr0,  ;
          SUM_D0   WITH sd0,  ;
          SUM_R0S  WITH sr0s, ;
          SUM_D0S  WITH sd0s, ;
          SUM_RS   WITH srs,  ;
          SUM_R    WITH sr
ENDIF

SELECT STOCK
USE

SELECT ACCOUNT
USE

SELECT ACT_S
USE

SELECT ACT_S_T
USE

SELECT R_LIST_T

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                       ��ᬮ�� ����� ᯨ᪮�.                        �
*�                                                                        �
*����������������������������������������������������� 13.10.97 10:43:19 ͼ
PROCEDURE View_Lst

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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD View OF (menu_name) PROMPT "��ᬮ��" KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Print OF (menu_name) PROMPT "�����" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Price OF (menu_name) PROMPT "��� 業�" KEY Ctrl-A, "F1"
ON SELECTION PAD Price OF (menu_name) DO Sw_Mode WITH "Price"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

DO Use_Dummy

*
*   ����� ࠧ��஢ ����
*
SELECT R_LIST_T

PRIVATE ln, wd, r_sav
ln = MIN(100, RECCOUNT())   && ������⢮ ������� ��ப BROWSE
wd = 52

DO D_Wins WITH ln, wd, "���⠢騪�", 0, 1
@ 1, 3 SAY "��� ���⠢騪�"
@ 1, WCOLS()-3 SAY "�"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    SELECT R_LIST_T
    ON KEY LABEL Enter  KEYBOARD CHR(23)
    ON KEY LABEL F1     DO Sw_Mode WITH "Price"
    ON KEY LABEL Ctrl+P DO Sw_Mode WITH "Print"

*
*    BROWSE - ����
*
    BROWSE FIELDS SUP_NAME:H="",    ;
           F_SW = IIF(IS_STO, "�", " "):H="":1, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Footer()  ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "exit", "View")
    ENDIF

  CASE what_do = "View"    && ��ᬮ�� ᯨ᪠

    DO View0
    what_do = "List"

  CASE what_do = "Print"    && ����� ᯨ᪠

    WAIT WIND "����� ����� ᯨ᪠"
    what_do = "List"

  CASE what_do = "Price"    && ����� ⨯� 業�

    IF IS_STO
      REPLACE IS_STO WITH .F.
    ELSE
      r_sav = RECNO()
      REPLACE ALL IS_STO WITH .F.
      GO r_sav
      REPLACE IS_STO WITH .T.
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN
*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                    �ணࠬ�� �뢮�� ������ ��ப�.                     �
*�                                                                        �
*����������������������������������������������������� 13.10.97 11:13:34 ͼ
PROCEDURE Footer
PRIVATE p1, p2, mon

ACTIVATE WINDOW (win_name+"_M") SAME

mon = "��."
IF IS_STO
  IF VALUTA = "$"
    mon = "USD "
    p1 = STR(SUM_D0,15,4)
    p2 = STR(SUM_D0S,15,4)
  ELSE
    p1 = STR(SUM_R0,15,2)
    p2 = STR(SUM_R0S,15,2)
  ENDIF
ELSE
  p1 = STR(SUM_R,15,2)
  p2 = STR(SUM_RS,15,2)
ENDIF

@ WROWS()-2, 2 SAY "�ᥣ�"
@ WROWS()-2, COL() SAY p1 COLOR SCHEME 1
@ WROWS()-2, COL() SAY mon+"  �த���"
@ WROWS()-2, COL() SAY p2 COLOR SCHEME 1
@ WROWS()-2, COL() SAY mon

ACTIVATE WINDOW (ALIAS()) SAME
RETURN .T.

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                            ��ᬮ�� ᯨ᪠.                            �
*�                                                                        �
*����������������������������������������������������� 13.10.97 11:45:52 ͼ
PROCEDURE View0
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
SELECT R_LIST_D
SET ORDER TO TAG NAME
*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, prc_sw
ln = 100   && ������⢮ ������� ��ப BROWSE
wd = 69
prc_sw = 1
IF R_LIST_T.IS_STO
  prc_sw = IIF(R_LIST_T.VALUTA = "$", 2, 3)
ENDIF

DO D_Wins WITH ln, wd, ALLTRIM(R_LIST_T.SUP_NAME), 0, 1
what_do = "List"
@ 1,  8 SAY " ������������ "
@ 1, 42 SAY "���-��"
@ 1, 54 SAY "�㬬�"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*
    BROWSE KEY STR(R_LIST_T.SUP_CODE,8)  ;
    FIELDS PREFIX:H="",   ;
           NAME:H="",     ;
           PRODUCER:H="", ;
           QNT:H="",      ;
           PRC = Prc_T(.F.):H="":12, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Footer1()  ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = "Exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
SELECT R_LIST_T

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                            ��ନ�㥬 業�.                             �
*�                                                                        �
*����������������������������������������������������� 13.10.97 11:56:19 ͼ
PROCEDURE Prc_T
PARAMETER is_sale

IF is_sale
  DO CASE
  CASE prc_sw = 1  && 
    RETURN STR(R_LIST_D.SUM_RS,12,2)
  CASE prc_sw = 2
    RETURN STR(R_LIST_D.SUM_D0S,12,4)
  CASE prc_sw = 3
    RETURN STR(R_LIST_D.SUM_R0S,12,2)
  ENDCASE
ELSE
  DO CASE
  CASE prc_sw = 1  && 
    RETURN STR(R_LIST_D.SUM_R,12,2)
  CASE prc_sw = 2
    RETURN STR(R_LIST_D.SUM_D0,12,4)
  CASE prc_sw = 3
    RETURN STR(R_LIST_D.SUM_R0,12,2)
  ENDCASE
ENDIF

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                    �ணࠬ�� �뢮�� ������ ��ப�.                     �
*�                                                                        �
*����������������������������������������������������� 13.10.97 11:13:34 ͼ
PROCEDURE Footer1

ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-2, 4 SAY "�த��� "
@ WROWS()-2, COL() SAY STR(QNT_SALED,6) COLOR SCHEME 1
@ WROWS()-2, COL() SAY "  �� �㬬� "
@ WROWS()-2, COL() SAY Prc_T(.T.) COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME
RETURN .T.