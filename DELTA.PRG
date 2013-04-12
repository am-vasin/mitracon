*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�           �ணࠬ�� ���� ��������� ��砫��� ���祭��.             �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Delta
PARAMETERS s_dat, e_dat  && ���. � ����筠� ����
PRIVATE err_level, rc
err_level = 0

*
*   ������� �६���� 䠩�� � ��������ﬨ
*
DO Wt_Mess WITH "��ନ�㥬 ����⮢��..."

DELETE FILE (tmpo_path+"S_DELTA.DBF")
DELETE FILE (tmpo_path+"S_DELTA.CDX")

DELETE FILE (tmpo_path+"A_DELTA.DBF")
DELETE FILE (tmpo_path+"A_DELTA.CDX")

DELETE FILE (tmpo_path+"K_DELTA.DBF")

DELETE FILE (tmpo_path+"C_DELTA.DBF")
DELETE FILE (tmpo_path+"C_DELTA.CDX")


*
*  ����稥
*
CREATE DBF (tmpo_path+"S_DELTA") ;
  ( CODE     N ( 7),  ;
    QNT      N ( 6),  ;
    QNT_REP  N ( 6),  ;
    QNT_REAL N ( 6) )

USE (tmpo_path+"S_DELTA") EXCLUSIVE
INDEX ON CODE TAG CODE

SELECT 0
USE STOCK ORDER TAG CODE

SCAN
  SELECT S_DELTA
  APPEND BLANK
  REPLACE CODE WITH STOCK.CODE
  SELECT STOCK
ENDSCAN

*
*  ����� ࠡ��� ����
*
CREATE DBF (tmpo_path+"A_DELTA") ;
  ( ARM_C    C ( 3),  ;
    KASSA    N (15,2) )

USE (tmpo_path+"A_DELTA") EXCLUSIVE
INDEX ON ARM_C TAG ARM_C

SELECT 0
USE ARM_LST ORDER TAG ARM_C

SCAN
  SELECT A_DELTA
  APPEND BLANK
  REPLACE ARM_C WITH ARM_LST.ARM_C
  SELECT ARM_LST
ENDSCAN

*
*  ���� �।�����
*
CREATE DBF (tmpo_path+"K_DELTA") ;
  ( DELTA_R  N (15,2),  ;
    DELTA_D  N (15,3) )

APPEND BLANK

*
*  �������
*
SELECT 0
CREATE DBF (tmpo_path+"C_DELTA") ;
  ( CUS_CODE N ( 8),  ;
    CUS_TYPE N( 1),   ;
    DELTA_R  N (15,2),  ;
    DELTA_D  N (15,3) )

USE (tmpo_path+"C_DELTA") EXCLUSIVE
INDEX ON CUS_CODE TAG CUS_CODE

SELECT 0
USE ACCOUNT ORDER TAG CUS_CODE

SCAN
  SELECT C_DELTA
  APPEND BLANK
  REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE,  ;
          CUS_TYPE WITH ACCOUNT.ACC_TYPE
  SELECT ACCOUNT
ENDSCAN

DO Wt_Mess

DO Wt_Mess WITH "��� ��室�"

USE ACT_S
SET RELATION TO CODE INTO S_DELTA

SCAN FOR BETWEEN(DOC_DATE, s_dat, e_dat)
  IF .NOT. FOUND("S_DELTA")
    WAIT "�����㦥� ��室 �� ���������饩 ����樨." WINDOW NOWAIT
    err_level = MAX(err_level,1)
  ELSE
    SELECT S_DELTA
    REPLACE QNT WITH QNT+ACT_S.QNT_INC, QNT_REP WITH QNT_REP+ACT_S.QNT_REP
    SELECT ACT_S
  ENDIF
ENDSCAN

DO Wt_Mess

DO Wt_Mess WITH "��� ��������� �� ॠ������"

USE RYN_M
SET RELATION TO CODE INTO S_DELTA

SCAN FOR BETWEEN(DOC_DATE, s_dat, e_dat)
  IF .NOT. FOUND("S_DELTA")
    WAIT "�����㦥� ���� � 䨫��� �� ���������饩 ����樨." WINDOW NOWAIT
    err_level = MAX(err_level,1)
  ELSE
    SELECT S_DELTA
    REPLACE QNT WITH QNT-RYN_M.QNT, QNT_REAL WITH QNT_REAL+RYN_M.QNT
    SELECT RYN_M
  ENDIF
ENDSCAN

DO Wt_Mess

DO Wt_Mess WITH "��� ���⥦��"

USE INCMONEY ORDER TAG HRON
SET RELATION TO CUS_CODE INTO C_DELTA,  ;
                WHERE    INTO A_DELTA

IF .NOT. SEEK(DTOS(s_dat))
  rc = RECNO(0)
  IF BETWEEN(rc, 1, RECCOUNT())
    GO rc
  ENDIF
ENDIF

SCAN REST WHILE DOC_DATE <= e_dat

  *
  *  ���뢠�� ���죨 �� ������� ������
  *
  IF FOUND("C_DELTA")
    SELECT C_DELTA
    REPLACE DELTA_R WITH DELTA_R+INCMONEY.MONEY_R, ;
            DELTA_D WITH DELTA_D+INCMONEY.MONEY_D
  ELSE
    WAIT "�����㦥� ��室 ����� �� ���������騩 �������." WINDOW NOWAIT
    err_level = MAX(err_level,1)
  ENDIF

  *
  *  ���뢠�� ���죨 � ���� ࠡ�祣� ����
  *
  IF FOUND("A_DELTA")
    IF .NOT. INLIST(INCMONEY.DOC_TYPE, "1", "3")   && �/� � ����ન - ����!
      SELECT A_DELTA
      REPLACE KASSA WITH KASSA+INCMONEY.MONEY_R
    ENDIF
  ELSE
    WAIT "�����㦥� ��室 ����� �� ���������騩 ���." WINDOW NOWAIT
    err_level = MAX(err_level,1)
  ENDIF
  SELECT INCMONEY

ENDSCAN

DO Wt_Mess

DO Wt_Mess WITH "��� ���� ࠡ��� ����"

USE KASSA ORDER TAG HRON
SET RELATION TO WHERE INTO A_DELTA

IF .NOT. SEEK(DTOS(s_dat))
  rc = RECNO(0)
  IF BETWEEN(rc, 1, RECCOUNT())
    GO rc
  ENDIF
ENDIF

SCAN REST WHILE DOC_DATE <= e_dat

  IF WHO_GET = 0
    WAIT "�����㦥� ���⢥ত���� ����!" WINDOW NOWAIT
    err_level = MAX(err_level,2)
    LOOP
  ENDIF

  IF FOUND("A_DELTA")
    SELECT A_DELTA
    REPLACE KASSA WITH KASSA-KASSA.R_TOTAL
  ELSE
    WAIT "�����㦥� ���� �� ���������饣� ����." WINDOW NOWAIT
    err_level = MAX(err_level,1)
  ENDIF
  SELECT KASSA

ENDSCAN

DO Wt_Mess

DO Wt_Mess WITH "��� ����� �।�����"

USE KASSA_M ORDER TAG HRON

IF .NOT. SEEK(DTOS(s_dat))
  rc = RECNO(0)
  IF BETWEEN(rc, 1, RECCOUNT())
    GO rc
  ENDIF
ENDIF

SCAN REST WHILE DOC_DATE <= e_dat

  SELECT K_DELTA
  REPLACE DELTA_R WITH DELTA_R+KASSA_M.SUM_R, ;
          DELTA_D WITH DELTA_D+KASSA_M.SUM_D
  SELECT KASSA_M

ENDSCAN


DO Wt_Mess

DO Wt_Mess WITH " ��� ��������� "

USE SALE ORDER TAG DOC_NUM
SELECT 0
USE SALE_TIT ORDER TAG HRON

IF .NOT. SEEK(DTOS(s_dat))
  rc = RECNO(0)
  IF BETWEEN(rc, 1, RECCOUNT())
    GO rc
  ENDIF
ENDIF

SCAN REST WHILE DOC_DATE <= e_dat

  * ����ॡ��� ���죨 �� ���������
  rc = DTOC(DOC_DATE)
  @ WROWS()-1, 0 SAY PADC(rc,WCOLS())

  IF CUS_CODE # 0      && �����ࠥ��� � ������⮬
    IF .NOT. SEEK(CUS_CODE, "C_DELTA")
      WAIT "��� ��������� �� �����㦥� ������." WINDOW NOWAIT
      err_level = MAX(err_level,1)
    ELSE
      IF CUS_TYPE # C_DELTA.CUS_TYPE
        WAIT "�� �ࠢ���� ⨯ ������ � ���������." WINDOW NOWAIT
        err_level = MAX(err_level,1)
      ENDIF
      SELECT C_DELTA
      IF CUS_TYPE = 2
        REPLACE DELTA_D WITH DELTA_D-SALE_TIT.TOTAL_D
      ELSE
        REPLACE DELTA_R WITH DELTA_R-SALE_TIT.TOTAL_R+SALE_TIT.KASSA
      ENDIF
    ENDIF
  ENDIF
  IF .NOT. SEEK(SALE_TIT.WHERE, "A_DELTA")
    WAIT "��� ��������� �� �����㦥� ���." WINDOW NOWAIT
    err_level = MAX(err_level,1)
  ELSE
    IF SALE_TIT.KASSA # 0
      SELECT A_DELTA
      REPLACE KASSA WITH KASSA+SALE_TIT.KASSA
    ENDIF
  ENDIF
  
  SELECT SALE
  SEEK LEFT(DTOS(SALE_TIT.DOC_DATE),4)+SALE_TIT.DOC_NUM
  SCAN REST WHILE DOC_NUM = SALE_TIT.DOC_NUM .AND. DOC_DATE = SALE_TIT.DOC_DATE
    SELECT S_DELTA
    SEEK SALE.CODE
    REPLACE QNT_REP WITH QNT_REP-SALE.QNT_REP
    IF SALE_TIT.CUS_TYPE = 4 .AND. EMPTY(SALE_TIT.LIST_NUM)
      REPLACE QNT_REAL WITH QNT_REAL-SALE.QNT
    ELSE
      REPLACE QNT WITH QNT-SALE.QNT
    ENDIF
    SELECT SALE
  ENDSCAN
  SELECT SALE_TIT
ENDSCAN
DO Wt_Mess

CLOSE DATABASES

RETURN err_level