*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Cus_Imp      ��о�｀砒┴ ��むォ ��瓱�           14.05.97 13:39:35 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ����珥 甎�瓷� ���諷 ��┘�皰�.                      �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Cus_Imp
PARAMETER ext_cus     && ������ (� 礫����┘� �竄�) ━� ↓ヨ�ィ� �����.
PRIVATE mss

IF .NOT. File_O(ext_cus)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 磴�モ瘴 ���皋 ↓ヨ�ィ� 甎�瓷� ��┘�皰�!"
  mss(3) = ""
  IF UPPER(PROGRAM()) == "CUS_IMP"
    DO Out_Mess WITH 7, "mss"
  ENDIF
  RETURN
ENDIF

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (ext_cus) ALIAS EXT_CUS
SET RELATION TO CUS_CODE INTO ACCOUNT
IF UPPER(PROGRAM()) == "CUS_IMP"
  DO Wt_Mess WITH "�▲爲� 甎�瓷� ��┘�皰�..."
ENDIF
SCAN FOR .NOT. FOUND("ACCOUNT")
  SELECT ACCOUNT
  APPEND BLANK
  REPLACE SUBSYS    WITH EXT_CUS.SUBSYS,     ;
          CUS_CODE  WITH EXT_CUS.CUS_CODE,   ;
          CUS_NAME  WITH EXT_CUS.CUS_NAME,   ;
          CITY      WITH EXT_CUS.CITY,       ;
          MARK1     WITH EXT_CUS.MARK1,      ;
          MARK2     WITH EXT_CUS.MARK2,      ;
          MARK3     WITH EXT_CUS.MARK3,      ;
          MARK4     WITH EXT_CUS.MARK4,      ;
          MARK5     WITH EXT_CUS.MARK5,      ;
          ACC_TYPE  WITH EXT_CUS.ACC_TYPE,   ;
          ACC_NOTE  WITH EXT_CUS.ACC_NOTE,   ;
          ACC_NOTED WITH EXT_CUS.ACC_NOTED,  ;
          DATE_ON   WITH EXT_CUS.DATE_ON,    ;
          WHO       WITH EXT_CUS.WHO,        ;
          DATE_OFF  WITH EXT_CUS.DATE_OFF
  SELECT EXT_CUS
ENDSCAN

IF UPPER(PROGRAM()) == "CUS_IMP"
  DO Wt_Mess
ENDIF
USE
SELECT ACCOUNT
USE
RETURN
