*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Del_Mon      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ����キ┘ ぅ�ウ���� ぎ�祠キ��.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 01/18/2000 様�
PROCEDURE Del_Mon
PARAMETERS f_code, d_type, d_num, d_dat, nt
*
*  f_code - ��� �爛く爬閧��;
*  d_type - 皋� ぎ�祠キ��;
*  d_num  - ���ム ぎ�祠キ�� ( ↓竄爛��┤ 筮爼�� );
*  d_dat  - �─� ���� ぎ�祠キ�� �─� ��� � 瓱�〓�讚�� ┼� 腮甄�〓� 筮爼�皀;
*  nt     - �ギ￥��皀�讚覃 �����モ�. 踸�ョ��┘.
*

PRIVATE s_sav, d_year
s_sav = SELECT()

IF TYPE("d_dat") = "D"
  d_year = LEFT(DTOS(d_dat),4)
ENDIF
IF TYPE("d_dat") = "N"
  d_year = STR(d_dat,4)
ENDIF
IF TYPE("d_dat") = "C"
  d_year = d_dat
ENDIF

SELECT 0
USE (base_path+"PAY_BILL") ORDER TAG PAY_DOC ALIAS PAY_0118 AGAIN
SET FILTER TO REV_TYPE # "S"
SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM ALIAS DOC_0118 AGAIN

IF .NOT. SEEK(f_code+d_type+d_year+d_num)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���祠キ� �� ���ぅ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE IN PAY_0118
  USE
  SELECT (s_sav)
  RETURN
ENDIF

IF SEEK(f_code+d_year+d_num, "PAY_0118")
  IF PAY_0118.BILL_SUM = PAY_0118.REST_SUM
    SELECT PAY_0118
    DELETE
    SELECT DOC_0118
  ELSE
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� 轤��� ���皀�� ━カ� �メ皰 ����甌��� 瘍モ-���矣�� � �癸珮И�!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE IN PAY_0118
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
ENDIF

PRIVATE c_code, c_name, sum_r, sum_d, sum_n, who_cr, tmpArm
c_code = CUS_CODE
sum_r  = MONEY_R
sum_d  = MONEY_D
sum_n  = N_OF_SALE
who_cr = WHO
tmpArm = WHERE
DELETE
USE IN PAY_0118

IF c_code # 0
  USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS CUS_0118 AGAIN
  IF .NOT. SEEK(STR(c_code)+f_code)
    APPEND BLANK
    REPLACE FIRM WITH f_code, CUS_CODE WITH c_code
  ENDIF
  IF d_type $ "34"
    REPLACE ACC_SUMD WITH ACC_SUMD-sum_d
  ELSE
    REPLACE ACC_SUM  WITH ACC_SUM-sum_r
  ENDIF
ENDIF

IF d_type $ " �3"
  USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS ARM_0118 AGAIN
  SEEK tmpArm
  IF d_type $ " �"
    REPLACE KASSA WITH KASSA-sum_r
  ELSE
    REPLACE KASSA_USD WITH KASSA_USD-sum_d
  ENDIF
ENDIF
USE
IF .NOT. File_O(log_path+"DEL_LOG.DBF")
  CREATE DBF (log_path+"DEL_LOG") ;
  (  DEL_DATE D,     ;
     DEL_TIME C( 8), ;
     WHO_DEL  N( 5), ;
     F_TYPE   C( 8), ;
     FIRM     C( 3), ;
     DOC_TYPE C( 1), ;
     DOC_NUM  C(10), ;
     DOC_YEAR C( 4), ;
     NOTE     C(80)  )
ENDIF
USE (log_path+"DEL_LOG") ALIAS LOG_0118 AGAIN
APPEND BLANK
REPLACE DEL_DATE WITH DATE(), ;
        DEL_TIME WITH TIME(), ;
        WHO_DEL  WITH user,   ;
        F_TYPE   WITH "MONEY",;
        FIRM     WITH f_code, ;
        DOC_TYPE WITH d_type, ;
        DOC_NUM  WITH d_num,  ;
        DOC_YEAR WITH d_year, ;
        NOTE     WITH IIF(TYPE("nt") = "C", nt, "")
USE

SELECT (s_sav)

RETURN
