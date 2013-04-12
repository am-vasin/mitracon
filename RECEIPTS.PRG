*������������������������������������������������������������������������ͻ
*�   ��� 䠩�� Receipts     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�                       ����窠 �� ࠡ�祬 ����.                        �
*�                                                                        �
*����������������������������������������������������������� 18.04.2000 �ͼ
PROCEDURE Receipts
PARAMETER prmArm

PRIVATE dt

dt = Get_Date({})
IF EMPTY(dt)
  RETURN
ENDIF

DO Prep_Tmp

DO V_Arms WITH prmArm

SELECT TMP_ARM
USE
DELETE FILE (tmpo_path+"TMP_ARM.DBF")
DELETE FILE (tmpo_path+"TMP_ARM.CDX")

SELECT TMP_MOVE
USE
DELETE FILE (tmpo_path+"TMP_MOVE.DBF")
DELETE FILE (tmpo_path+"TMP_MOVE.CDX")
RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� Prep_Tmp     ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                      �����⮢�� �६������ 䠩��.                      �
*�                                                                        �
*����������������������������������������������������������� 18.04.2000 ���
PROCEDURE Prep_Tmp

DELETE FILE (tmpo_path+"TMP_ARM.DBF")
DELETE FILE (tmpo_path+"TMP_ARM.CDX")
CREATE DBF (tmpo_path+"TMP_ARM") ;
  ( ARM_C  N( 4),  ;
    ARM_N  C(16),  ;
    SUM_S1 N(12,2),;
    SUM_M1 N(12,2),;
    SUM_M2 N(12,2) )
INDEX ON SYS(15, lwr, ARM_N)TAG ARM_N
INDEX ON ARM_C TAG ARM_C ADDITIVE

SELECT 0

DELETE FILE (tmpo_path+"TMP_MOVE.DBF")
DELETE FILE (tmpo_path+"TMP_MOVE.CDX")
CREATE DBF (tmpo_path+"TMP_MOVE") ;
  ( REST      N(12,2),;
    ARM_C     N( 4),  ;
    FIRM      C( 3),  ;
    DOC_TIME  N( 5),  ;
    DOC_NUM   C(10),  ;
    DOC_TYPE  C( 1),  ;
    DOC_V     C(11),  ;
    SALE_NUM  C(10),  ;
    SALE_V    C(11),  ;
    SALE_DATE D,      ;
    SUM       N(12,2),;
    WHO       N( 5),  ;
    SAL_NAME  C(30)   )

SELECT 0
USE (base_path+"KASSA") ORDER TAG HRON DESCENDING ALIAS TMP_0418 AGAIN
SCAN WHILE DOC_DATE >= dt
  IF VALUTA = "$"
    LOOP
  ENDIF
  SELECT TMP_ARM
  IF .NOT. SEEK(VAL(TMP_0418.WHERE))
    APPEND BLANK
    REPLACE ARM_C  WITH VAL(TMP_0418.WHERE)
  ENDIF
  REPLACE SUM_S1 WITH SUM_S1+TMP_0418.TOTAL
  SELECT TMP_0418
ENDSCAN

USE (base_path+"INCMONEY") ORDER TAG HRON DESCENDING ALIAS TMP_0418 AGAIN
SCAN WHILE DOC_DATE >= dt
  IF .NOT. DOC_TYPE $ " �"
    LOOP
  ENDIF
  SELECT TMP_ARM
  IF .NOT. SEEK(VAL(TMP_0418.WHERE))
    APPEND BLANK
    REPLACE ARM_C WITH VAL(TMP_0418.WHERE)
  ENDIF
  REPLACE SUM_S1 WITH SUM_S1-TMP_0418.MONEY_R-TMP_0418.N_OF_SALE
  IF TMP_0418.DOC_DATE = dt
    IF TMP_0418.DOC_TYPE = " "
      REPLACE SUM_M1 WITH SUM_M1+TMP_0418.MONEY_R+TMP_0418.N_OF_SALE
    ELSE
      REPLACE SUM_M2 WITH SUM_M2+TMP_0418.MONEY_R+TMP_0418.N_OF_SALE
    ENDIF
    SELECT TMP_MOVE
    APPEND BLANK
    REPLACE FIRM      WITH TMP_0418.FIRM,      ;
            ARM_C     WITH VAL(TMP_0418.WHERE),;
            DOC_NUM   WITH TMP_0418.DOC_NUM,   ;
            DOC_TYPE  WITH TMP_0418.DOC_TYPE,  ;
            DOC_TIME  WITH TMP_0418.DOC_TIME,  ;
            DOC_V     WITH LEFT(TMP_0418.DOC_NUM,1)+ALLTRIM(SUBSTR(TMP_0418.DOC_NUM,2)), ;
            SALE_NUM  WITH TMP_0418.SALE_NUM,  ;
            SALE_DATE WITH TMP_0418.SALE_DATE, ;
            SALE_V    WITH IIF(EMPTY(TMP_0418.SALE_NUM), "",  ;
                       LEFT(TMP_0418.SALE_NUM,4)+"-"+ALLTRIM(SUBSTR(TMP_0418.SALE_NUM,5))), ;
            SUM       WITH TMP_0418.MONEY_R+TMP_0418.N_OF_SALE, ;
            WHO       WITH TMP_0418.WHO
  ENDIF
  SELECT TMP_0418
ENDSCAN

USE

USE (base_path+"ARM_LST") ORDER TAG ARM_CODE ALIAS TMP_0418 AGAIN
SCAN && FOR .NOT. EMPTY(KASSA)
  IF .NOT. SEEK(ARM_CODE, "TMP_ARM")
    IF EMPTY(KASSA)
      LOOP
    ENDIF
    SELECT TMP_ARM
    APPEND BLANK
    REPLACE ARM_C WITH TMP_0418.ARM_CODE
    SELECT TMP_0418
  ENDIF
  REPLACE TMP_ARM.ARM_N  WITH TMP_0418.ARM_N, ;
          TMP_ARM.SUM_S1 WITH TMP_ARM.SUM_S1+TMP_0418.KASSA
ENDSCAN
USE
SELECT TMP_MOVE
INDEX ON STR(ARM_C)+DOC_TYPE+STR(DOC_TIME) TAG ARM_C
PRIVATE keyVal, tmpRest, tmpArm
GO TOP
keyVal = STR(ARM_C)+DOC_TYPE
= SEEK(ARM_C, "TMP_ARM")
tmpRest = IIF(DOC_TYPE = " ", TMP_ARM.SUM_S1, 0)
SCAN
  IF keyVal # STR(ARM_C)+DOC_TYPE
    keyVal = STR(ARM_C)+DOC_TYPE
    = SEEK(ARM_C, "TMP_ARM")
    tmpRest = IIF(DOC_TYPE = " ", TMP_ARM.SUM_S1, 0)
  ENDIF
  tmpRest = tmpRest+SUM
  REPLACE REST WITH tmpRest
ENDSCAN

SELECT TMP_ARM
SET ORDER TO TAG ARM_N
GO TOP

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� V_Arms       ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                          ���᮪ ࠡ��� ����.                          �
*�                                                                        �
*����������������������������������������������������������� 20.04.2000 ���
PROCEDURE V_Arms
PARAMETER prmArm
PRIVATE tmpArm

*
*  ��६���� ���ﭨ� ��� ������樨
*
IF TYPE("prmArm") = "C"
  tmpArm = ASC(prmArm)
ELSE
  IF TYPE("prmArm") = "N"
    tmpArm = prmArm
  ENDIF
ENDIF

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT TMP_ARM
IF TYPE("tmpArm") = "N"
  SET ORDER TO TAG ARM_C
ENDIF

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Orders OF (menu_name) PROMPT "��室�� �थ�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Orders OF (menu_name) DO Sw_Mode WITH "Orders"

DEFINE PAD Checks OF (menu_name) PROMPT "����" KEY Ctrl-A, "Ctrl+Enter"
ON SELECTION PAD Checks OF (menu_name) DO Sw_Mode WITH "Checks"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = IIF(TYPE("tmpArm") = "N", 1, WROWS("")-16)  && ������⢮ ������� ��ப BROWSE
wd = 55

DO D_Wins WITH ln, wd, ;
     IIF(TYPE("tmpArm") = "N", "����ﭨ� ���� �� ", "����ﭨ� ����� �� ")+DTOC(dt), 0, 0
what_do = "List"
*�Đ��. �����������Ď��⮪���ď��.�थ������ė������Ŀ
*�....:....!....:. 999999999.99 999999999.99 999999999.99�
*���������������������������������������������������������
@ 1, 2 SAY "Đ��. �����������Ď��⮪���ď��.�थ������ė���"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter      KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "Checks"

*
*    BROWSE - ����
*
    IF TYPE("tmpArm") = "N"
      BROWSE KEY tmpArm ;
             FIELDS ARM_N:16:H="",     ;
                    SUM_S1:H="":P="999999999.99", ;
                    SUM_M1:H="":P="999999999.99", ;
                    SUM_M2:H="":P="999999999.99", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS ARM_N:16:H="",     ;
                    SUM_S1:H="":P="999999999.99", ;
                    SUM_M1:H="":P="999999999.99", ;
                    SUM_M2:H="":P="999999999.99", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Orders")
    ENDIF

  CASE what_do = "Checks"    && ��ᬮ�� 祪��

    DO V_Check
    what_do = "List"

  CASE what_do = "Orders"    && ��室�� �थ�

    DO V_Ord
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� V_Check      ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                            ��ᬮ�� 祪��.                             �
*�                                                                        �
*����������������������������������������������������������� 20.04.2000 ���
PROCEDURE V_Check

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
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
s_sav = SELECT()

SELECT TMP_MOVE
GO TOP

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD P_Check OF (menu_name) PROMPT "����� 祪�" KEY Ctrl-A, "Enter"
ON SELECTION PAD P_Check OF (menu_name) DO Sw_Mode WITH "P_Check"

DEFINE PAD P_Inv OF (menu_name) PROMPT "����� ⮢. 祪�" KEY Ctrl-A, "Ctrl_Enter"
ON SELECTION PAD P_Inv OF (menu_name) DO Sw_Mode WITH "P_Inv"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 60

DO D_Wins WITH ln, wd, ALLTRIM(TMP_ARM.ARM_N)+" (����)", 0, 0
*�ė�� N�����Ē��. 祪 N���Ą����������đ㬬������Ď��⮪��Ŀ
*�DDDDDDDDDDD SSSSSSSSSSS ��.��.���� 999999999.99 999999999.99�
*��������������������������������������������������������������
@ 1, 2 SAY "ė�� N�����Ē��. 祪 N���Ą����������đ㬬������Ď��⮪"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter      KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "P_Inv"

*
*    BROWSE - ����
*
    BROWSE KEY STR(TMP_ARM.ARM_C)+"�" ;
    FIELDS DOC_V    :H="",  ;
           SALE_V   :H="",  ;
           SALE_DATE:H="",  ;
           SUM      :H="":P="999999999.99", ;
           REST     :H="":P="999999999.99", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "P_Check")
    ENDIF

  CASE what_do = "P_Check"    && ����� 祪�

    DO P_Money WITH FIRM, DOC_TYPE, DOC_NUM, dt
    what_do = "List"

  CASE what_do = "P_Inv"      && ����� ⮢. 祪�

    DO Prn_Inv WITH FIRM, SALE_NUM, SALE_DATE
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
SELECT (s_sav)

RETURN

*������������������������������������������������������������������������Ŀ
*�   ��楤�� V_Ord        ���ࠡ��稪 ���३ ��ᨭ                      �
*������������������������������������������������������������������������Ĵ
*�                                                                        �
*�                     ��ᬮ�� ��室��� �थ஢.                        �
*�                                                                        �
*����������������������������������������������������������� 20.04.2000 ���
PROCEDURE V_Ord

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
PRIVATE s_sav         && ����� ࠡ�祩 ������ ��� ��࠭���� � ����⠭�������!

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*
s_sav = SELECT()

SELECT TMP_MOVE
GO TOP

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD P_Check OF (menu_name) PROMPT "����� ��� �थ�" KEY Ctrl-A, "Enter"
ON SELECTION PAD P_Check OF (menu_name) DO Sw_Mode WITH "P_Check"

DEFINE PAD P_Inv OF (menu_name) PROMPT "����� ���������" KEY Ctrl-A, "Ctrl_Enter"
ON SELECTION PAD P_Inv OF (menu_name) DO Sw_Mode WITH "P_Inv"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ������⢮ ������� ��ப BROWSE
wd = 60

DO D_Wins WITH ln, wd, ALLTRIM(TMP_ARM.ARM_N)+" (���. �थ�)", 0, 0
*ڏ��.�थ���č�����������Ą����������đ㬬������Ď��⮪��Ŀ
*�DDDDDDDDDDD SSSSSSSSSSS ��.��.���� 999999999.99 999999999.99�
*��������������������������������������������������������������
@ 1, 2 SAY "���.�थ���č�����������Ą����������đ㬬������Ď��⮪"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter      KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "P_Inv"

*
*    BROWSE - ����
*
    BROWSE KEY STR(TMP_ARM.ARM_C)+" " ;
    FIELDS DOC_V    :H="",  ;
           SALE_V   :H="",  ;
           SALE_DATE:H="",  ;
           SUM      :H="":P="999999999.99", ;
           REST     :H="":P="999999999.99", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "P_Check")
    ENDIF

  CASE what_do = "P_Check"    && ����� 祪�


    DO P_Money WITH FIRM, DOC_TYPE, DOC_NUM, dt
    what_do = "List"

  CASE what_do = "P_Inv"      && ����� ⮢. 祪�

    DO Prn_Inv WITH FIRM, SALE_NUM, SALE_DATE
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   �����⭥�쪮 (�����⭥�쪮!) ����뢠�� DBF-䠩��.
*
SELECT (s_sav)

RETURN