*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                       ����稥: �஭� � 䨫����.                        �
*�                                                                        �
*����������������������������������������������������� 16.06.97 09:47:10 ͼ
PROCEDURE Rep_Pos
PARAMETER mode
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
PRIVATE qnt_0, qnt_1, qnt_2
STORE 0 TO qnt_0, qnt_1, qnt_2

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

DO Wt_Mess WITH "�����ઠ."

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

PRIVATE w_where, w_who, w_cus, w_price, sum_qnt, sum_rez

DELETE FILE (tmpo_path+"BR_FIL0.DBF")

SELECT 0
CREATE DBF (tmpo_path+"BR_FIL0.DBF")  ;
  ( QNT      N( 6),  ;
    QNT_REZ  N( 6),  ;
    QNT_01   N( 6),  ;
    QNT_02   N( 6),  ;
    PRICE    C(10),  ;
    CUSTOMER C(50),  ;
    WHO      C(30),  ;
    WHO_1    C(30),  ;
    WHO_2    C(30),  ;
    L_NO     C( 6),  ;
    WHERE    C(16)   )

*
*    �� ॠ����樨
*
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_2712 AGAIN

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS ARM_2712 AGAIN

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_2712 AGAIN

*------------------------------------------------------------------------
sum_qnt = 0
sum_rez = 0
IF mode = 1
*
*  �������
*
  SELECT 0
  USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE ALIAS RT_2712 AGAIN

  SELECT 0
  USE (base_path+"RYN_L") ORDER TAG CODE ALIAS RD_2712 AGAIN

  SEEK ST_2712.CODE
  SCAN REST WHILE CODE = ST_2712.CODE
    = SEEK(CUS_CODE, "RT_2712")
    = SEEK(CUS_CODE, "ACC_2712")
    w_cus   = ACC_2712.CUS_NAME
    w_where = ""
    w_who   = ""
    w_price = TRANSFORM(RD_2712.PRICE_R, "@Z 999999.99")+"�"
    IF FOUND("RT_2712")        && �᫨ ��諨 ���������...
      IF SEEK(RT_2712.WHO, "P_2712")
        w_who = ALLTRIM(P_2712.FAMILY)+" "+  ;
                LEFT(P_2712.NAME,1)+". "+    ;
                LEFT(P_2712.S_NAME,1)+"."
      ENDIF
      IF SEEK(RT_2712.WHERE, "ARM_2712")
        w_where = ARM_2712.ARM_N
      ENDIF
      IF RT_2712.VALUTA = "$"
        w_price = TRANSFORM(RD_2712.PRICE_D, "@Z 99999.9999")+"$"
      ENDIF
    ENDIF
    SELECT BR_FIL0
    APPEND BLANK
    REPLACE QNT      WITH RD_2712.QNT, ;
            PRICE    WITH w_price,   ;
            CUSTOMER WITH w_cus,     ;
            WHO      WITH w_who,     ;
            WHERE    WITH w_where
    sum_qnt = sum_qnt+QNT
    SELECT RD_2712
  ENDSCAN
  SELECT RD_2712
  USE
  SELECT RT_2712
  USE
ENDIF

IF mode = 2
*
*  ����䨫����
*
  SELECT 0
  USE (base_path+"SPEC_SUM") ORDER TAG POS_CUS ALIAS SS_2712
  SET RELATION TO CUS_CODE INTO ACC_2712

  SEEK STR(ST_2712.CODE,7)
  SCAN REST WHILE CODE = ST_2712.CODE
    w_cus   = ACC_2712.CUS_NAME
    w_where = ""
    w_who   = ""
    w_price = ""
    SELECT BR_FIL0
    APPEND BLANK
    REPLACE QNT      WITH SS_2712.QNT, ;
            PRICE    WITH w_price,   ;
            CUSTOMER WITH w_cus,     ;
            WHO      WITH w_who,     ;
            WHERE    WITH w_where
    sum_qnt = sum_qnt+QNT
    SELECT SS_2712
  ENDSCAN
  USE
ENDIF

IF mode = 0
  *
  *   �஭�
  *
  SELECT 0
  USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_2712 AGAIN

  SELECT 0
  USE (base_path+"LIST_DET") ORDER TAG CODE ALIAS LD_2712 AGAIN
  SEEK ST_2712.CODE
  SCAN REST WHILE CODE = ST_2712.CODE FOR QNT_REQ # 0 .OR. QNT_REZ # 0
    IF .NOT. SEEK(VAL(ALLTRIM(LIST_NUM)), "LT_2712")
      LOOP
    ENDIF
    IF .NOT. EMPTY(LT_2712.SAL_NUM)
      LOOP
    ENDIF
    IF .NOT. EMPTY(LT_2712.CLI_CODE) .AND.SEEK(LT_2712.CLI_CODE,"ACC_2712")
      w_cus   = ACC_2712.CUS_NAME
    ELSE
      w_cus   = LT_2712.LST_NAME
    ENDIF
    w_where = ""
    w_who   = ""
    w_price = TRANSFORM(SAL_PRICE, "@Z 999999.99")+"�"
    IF SEEK(LT_2712.WHO_CR, "P_2712")
      w_who = ALLTRIM(P_2712.FAMILY)+" "+  ;
              LEFT(P_2712.NAME,1)+". "+    ;
              LEFT(P_2712.S_NAME,1)+"."
    ENDIF
    IF LT_2712.LST_ARM = "***"
      w_where = "���� �������"
    ELSE
      IF SEEK(LT_2712.LST_ARM, "ARM_2712")
        w_where = ARM_2712.ARM_N
      ENDIF
    ENDIF
    SELECT BR_FIL0
    APPEND BLANK
    REPLACE QNT      WITH LD_2712.QNT_REQ, ;
            QNT_REZ  WITH LD_2712.QNT_REZ, ;
            PRICE    WITH w_price,   ;
            CUSTOMER WITH w_cus,     ;
            L_NO     WITH LD_2712.LIST_NUM, ;
            WHO      WITH w_who,     ;
            WHERE    WITH w_where
    sum_qnt = sum_qnt+QNT
    sum_rez = sum_rez+QNT_REZ
    SELECT LD_2712
  ENDSCAN
  USE (base_path+"ORD_T") ORDER TAG DOC_NUM AGAIN ALIAS OT_2712
  SELECT LT_2712
  USE (base_path+"ORD_D") ORDER TAG DOC_NUM AGAIN ALIAS OD_2712
  SET RELATION TO LIST_NUM+STR(ORD_NUM,3) INTO OT_2712
  SELECT BR_FIL0
  SCAN
    SELECT OD_2712
    SEEK BR_FIL0.L_NO
    SCAN REST WHILE BR_FIL0.L_NO = LIST_NUM FOR CODE = ST_2712.CODE
      DO CASE
      CASE .NOT. EMPTY(OT_2712.STO_MARK)
        qnt_2 = qnt_2+QNT
        SELECT BR_FIL0
        REPLACE QNT_02 WITH QNT_02+OD_2712.QNT
        IF SEEK(OT_2712.STO_MARK, "P_2712")
          REPLACE WHO_2 WITH ALLTRIM(P_2712.FAMILY)+" "+  ;
                             LEFT(P_2712.NAME,1)+". "+    ;
                             LEFT(P_2712.S_NAME,1)+"."
        ENDIF
        SELECT OD_2712
      CASE .NOT. EMPTY(OT_2712.CHECK_S)
        qnt_1 = qnt_1+QNT
        SELECT BR_FIL0
        REPLACE QNT_01 WITH QNT_01+OD_2712.QNT
        IF SEEK(VAL(ALLTRIM(OT_2712.CHECK_S)), "P_2712")
          REPLACE WHO_1 WITH ALLTRIM(P_2712.FAMILY)+" "+  ;
                             LEFT(P_2712.NAME,1)+". "+    ;
                             LEFT(P_2712.S_NAME,1)+"."
        ENDIF
        SELECT BR_FIL0
      OTHERWISE
        qnt_0 = qnt_0+QNT
      ENDCASE
    ENDSCAN
    SELECT BR_FIL0
  ENDSCAN
  SELECT OT_2712
  USE
  SELECT OD_2712
  USE
ENDIF


SELECT P_2712
USE

SELECT ARM_2712
USE

SELECT ACC_2712
USE

DO Wt_Mess

SELECT BR_FIL0
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  DO CASE
  CASE mode = 0
    mss(2) = CHR(0)+"��祣� �� ���஭�஢���!"
  CASE mode = 1
    mss(2) = CHR(0)+"� 䨫����� ��祣� ���!"
  CASE mode = 2
    mss(2) = CHR(0)+"� ᯥ�䨫����� ��祣� ���!"
  ENDCASE
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"BR_FIL0.DBF")
  SELECT ST_2712
  RETURN
ENDIF

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ������⢮ ������� ��ப BROWSE
wd = 68+IIF(mode = 0, 7, 0)
w_cus = ALLTRIM(ST_2712.PREFIX+" "+ST_2712.NAME+" "+ST_2712.PRODUCER)

DO D_Wins WITH ln, wd, w_cus, IIF(mode = 0, 1, 0), IIF(mode = 0, 3, 1)

IF mode = 0
  @ 1, 2 SAY "����� "+ALLTRIM(STR(qnt_0))+ ;
             "    � ࠡ�� "+ALLTRIM(STR(qnt_1))+ ;
             "    ���࠭� "+ALLTRIM(STR(qnt_2))
  @ 2,  2 SAY "���-��"
  @ 2,  9 SAY "�����"
  @ 2, 16 SAY "� ࠡ."
  @ 2, 24 SAY "����."
  @ 2, 34 SAY "����"
  @ 2, 43 SAY "������"
  @ WROWS()-5,  0 CLEAR TO WROWS()-1,  WCOLS()-1

  DO Sun_Bord WITH WROWS()-5, 1, WROWS()-3, 10, "�ᥣ�"
  @ WROWS()-4,  3 SAY sum_qnt+sum_rez PICTURE "999999" COLOR SCHEME 1
  DO Sun_Bord WITH WROWS()-5, 11, WROWS()-3, WCOLS()-2
  @ WROWS()-5, 13 SAY "���. ����"
  @ WROWS()-5, 30 SAY "�த����"
  @ WROWS()-5, 61 SAY "���᮪"
  @ WROWS()-2,  2 SAY "����ࠥ�"
  @ WROWS()-1,  2 SAY "�⢥न�"

ELSE
  @ 1,  2 SAY "���-��"
  @ 1, 13 SAY "����"
  @ 1, 22 SAY "������"
  @ WROWS()-3,  3 SAY "�ᥣ�"
  @ WROWS()-2,  3 SAY sum_qnt PICTURE "999999" COLOR SCHEME 1
  @ WROWS()-3, 10 SAY "�" COLOR (SCHEME(14,2))
  @ WROWS()-2, 10 SAY "�" COLOR (SCHEME(14,2))
  @ WROWS()-1, 10 SAY "�" COLOR (SCHEME(14,2))
  @ WROWS()-3, 11 SAY "�" COLOR (SCHEME(13,1))
  @ WROWS()-2, 11 SAY "�" COLOR (SCHEME(13,1))
  @ WROWS()-1, 11 SAY "�" COLOR (SCHEME(13,1))

  @ WROWS()-3, 13 SAY "���. ����"
  @ WROWS()-3, 30 SAY "�த����"
  @ WROWS()-3, 61 SAY "���᮪"
ENDIF

what_do = "List"

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - ����
*

    IF mode = 0
      BROWSE FIELDS QNT:P="999999":H="",     ;
                    QNT_REZ:P="999999":H="", ;
                    QNT_01:P="999999":H="",  ;
                    QNT_02:P="999999":H="",  ;
                    PRICE:H="",    CUSTOMER:H="",        ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN ShowWho()              ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS QNT:P="999999":H="",  ;
                    PRICE:H="",    CUSTOMER:H="",        ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN ShowWho()              ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    what_do = "Exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
SELECT BR_FIL0
USE
DELETE FILE (tmpo_path+"BR_FIL0.DBF")
SELECT ST_2712
RELEASE MENU (menu_name) EXTENDED

RETURN
*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                        ���ଠ樮���� ��ப�.                          �
*�                                                                        �
*����������������������������������������������������� 16.06.97 12:35:10 ͼ
PROCEDURE ShowWho

ACTIVATE WINDOW (win_name+"_M") SAME

IF mode = 0
  @ WROWS()-4, 13 SAY WHERE COLOR SCHEME 1
  @ WROWS()-4, COL()+1 SAY WHO COLOR SCHEME 1
  @ WROWS()-4, COL()+1 SAY L_NO COLOR SCHEME 1
  @ WROWS()-2, 11 SAY WHO_1
  @ WROWS()-1, 11 SAY WHO_2
ELSE
  @ WROWS()-2, 13 SAY WHERE COLOR SCHEME 1
  @ WROWS()-2, COL()+1 SAY WHO COLOR SCHEME 1
  @ WROWS()-2, COL()+1 SAY L_NO COLOR SCHEME 1
ENDIF

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.