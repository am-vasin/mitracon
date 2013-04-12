*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�            �ணࠬ�� ����ண� ���� ����� �� ������.               �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Super_D
PARAMETERS dt_0, dt_1

PRIVATE stru_, dt_0w, dt_1w, win_name, cnt

*
*   ��।��塞 ���ࢠ� ��� ��� ���� ��������
*
USE PARMS
dt_0w = IIF(TYPE("dt_0") = "D", dt_0, DAT_START)
dt_1w = IIF(TYPE("dt_1") = "D", dt_1, DATE())

*
*  ��।��塞 ���誮 ��� ��⮪���
*
win_name = PROGRAM()
DO D_Win_N WITH 20, 56, "��⮪��"
DEFINE WINDOW Internal FROM WLROW()+2,         WLCOL()+2  ;
                       TO   WLROW()+WROWS()-2, WLCOL()+WCOLS()-3  ;
                       NONE COLOR SCHEME 13
ACTIVATE WINDOW Internal

*
*  ������� �६���� 䠩� �����
*
USE STOCK ORDER TAG CODE

DIMENSION stru_(7,4)

stru_( 1, 1) = "CODE"
stru_( 1, 2) = "N"
stru_( 1, 3) = FSIZE("CODE","STOCK")
stru_( 1, 4) = 0

stru_( 2, 1) = "DLT_Q"
stru_( 2, 2) = "N"
stru_( 2, 3) = 10
stru_( 2, 4) = 0

stru_( 3, 1) = "DLT_R"
stru_( 3, 2) = "N"
stru_( 3, 3) = 10
stru_( 3, 4) = 0

stru_( 4, 1) = "DLT_F"
stru_( 4, 2) = "N"
stru_( 4, 3) = 10
stru_( 4, 4) = 0

stru_( 5, 1) = "REST_Q"
stru_( 5, 2) = "N"
stru_( 5, 3) = 10
stru_( 5, 4) = 0

stru_( 6, 1) = "REST_R"
stru_( 6, 2) = "N"
stru_( 6, 3) = 10
stru_( 6, 4) = 0

stru_( 7, 1) = "REST_F"
stru_( 7, 2) = "N"
stru_( 7, 3) = 10
stru_( 7, 4) = 0

SELECT 0
SET EXCLU ON
DELETE FILE (tmpo_path+"TMP_STO.DBF")
CREATE DBF (tmpo_path+"TMP_STO") FROM ARRAY stru_
USE (tmpo_path+"TMP_STO")
SET EXCLUSIVE OFF

cnt = 0
?? "��砫� �ନ஢���� ������. �६� "+TIME()
? "������ "+STR(cnt,6)
SELECT STOCK
SCAN
  SELECT TMP_STO
  APPEND BLANK
  REPLACE CODE WITH STOCK.CODE
  SELECT STOCK
  cnt = cnt+1
  @ ROW(), COL()-6 SAY STR(cnt,6)
ENDSCAN
USE

DO Super_N

DO Super_F

DO Super_I

DO Clc_Rest

? "       ���!   �६� "+TIME()
= INKEY(15,"HM")

RELEASE WINDOW (win_name)
RELEASE WINDOW Internal
RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                     �ணࠬ�� ��� ���������.                        �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Super_N
PRIVATE q1, q2, q3, cd, rc_sv

*
*     ���뢠�� ���� ������
*
SELECT 0
USE SALE_TIT ORDER TAG DOC_NUM

SELECT 0
USE SALE ORDER TAG CODE
SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO SALE_TIT

SELECT TMP_STO
GO TOP

SELECT SALE

cnt = 0
? "��砫� ��ᬮ�� ���������. �६� "+TIME()
? "������ "+STR(cnt,6)

STORE 0 TO q1, q2, q3, cd
SCAN FOR BETWEEN(DOC_DATE, dt_0w, dt_1w)
  IF cd # CODE
    SELECT TMP_STO
    IF cd # 0
      REPLACE DLT_Q WITH q1,  ;
              DLT_R WITH q2,  ;
              DLT_F WITH q3
      STORE 0 TO q1, q2, q3
    ENDIF
    SCAN REST WHILE CODE < SALE.CODE
    ENDSCAN
    rc_sw = RECNO()
    cnt = cnt+1
    IF CODE # SALE.CODE
      ? "�訡��!"
      ? "������ "+STR(cnt,6)
      GO rc_sw
      SELECT SALE
      LOOP
    ENDIF
    @ ROW(), COL()-6 SAY STR(cnt,6)
    SELECT SALE
    cd = CODE
  ENDIF
  IF SALE_TIT.CUS_TYPE = 4 .AND. EMPTY(SALE_TIT.LIST_NUM)
    q3 = q3+QNT
  ELSE
    q1 = q1+QNT
  ENDIF
  q2 = q2+QNT_REP

ENDSCAN

USE
SELECT SALE_TIT
USE

RETURN      

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                 �ணࠬ�� ��।�� � 䨫����.                          �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Super_F
PRIVATE q1, cd, rc_sv

SELECT TMP_STO
GO TOP

SELECT 0
USE RYN_M ORDER TAG CODE

cnt = 0

STORE 0 TO q1, cd
? "��砫� ��ᬮ�� 䨫�����. �६� "+TIME()
? "������ "+STR(cnt,6)

SCAN FOR BETWEEN(DOC_DATE, dt_0w, dt_1w)
  IF cd # CODE
    SELECT TMP_STO
    IF cd # 0
      REPLACE DLT_Q WITH DLT_Q+q1,  ;
              DLT_F WITH DLT_F-q1
      STORE 0 TO q1
    ENDIF
    SCAN REST WHILE CODE < RYN_M.CODE
    ENDSCAN
    rc_sw = RECNO()
    cnt = cnt+1
    IF CODE # RYN_M.CODE
      ? "�訡��!"
      ? "������ "+STR(cnt,6)
      GO rc_sw
      SELECT RYN_M
      LOOP
    ENDIF
    @ ROW(), COL()-6 SAY STR(cnt,6)
    SELECT RYN_M
    cd = CODE
  ENDIF
  q1 = q1+QNT

ENDSCAN

USE

RETURN      

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                 �ணࠬ�� ��।�� � 䨫����.                          �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Super_I
PRIVATE q1, cd, rc_sv

SELECT TMP_STO
GO TOP

SELECT 0
USE ACT_S ORDER TAG CODE

cnt = 0

STORE 0 TO q1, cd
? "��砫� ��ᬮ�� ��室�. �६� "+TIME()
? "������ "+STR(cnt,6)

SCAN FOR BETWEEN(DOC_DATE, dt_0w, dt_1w)
  IF cd # CODE
    SELECT TMP_STO
    IF cd # 0
      REPLACE DLT_Q WITH DLT_Q-q1
      STORE 0 TO q1
    ENDIF
    SCAN REST WHILE CODE < ACT_S.CODE
    ENDSCAN
    rc_sw = RECNO()
    cnt = cnt+1
    IF CODE # ACT_S.CODE
      ? "�訡��!"
      ? "������ "+STR(cnt,6)
      GO rc_sw
      SELECT ACT_S
      LOOP
    ENDIF
    @ ROW(), COL()-6 SAY STR(cnt,6)
    SELECT ACT_S
    cd = CODE
  ENDIF
  q1 = q1+QNT_INC

ENDSCAN

USE

RETURN      

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                 �ணࠬ�� ���� ���⪮�.                           �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Clc_Rest

SELECT 0
USE STOCK ORDER TAG CODE

SELECT TMP_STO
SET RELATION TO CODE INTO STOCK

? "����� ���⪮�. �६� "+TIME()

REPLACE ALL REST_Q WITH STOCK.QNT_START-DLT_Q,  ;
            REST_R WITH STOCK.QNT_S_REP-DLT_R,  ;
            REST_F WITH STOCK.QNT_S_RYN-DLT_F

? "������ ��䥪��� ����権. �६� "+TIME()
COUNT FOR REST_Q # STOCK.QNT+STOCK.SAVE .OR. ;
          REST_R # STOCK.QNT_REP .OR. ;
          REST_F # STOCK.QNT_REAL     TO cnt

? "��䥪��� ����権 "+STR(cnt,6)+" �६� "+TIME()
SELECT STOCK 
USE
RETURN