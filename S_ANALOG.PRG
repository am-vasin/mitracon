*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� s_analog     ��о�｀砒┴ ��むォ ��瓱�           05.05.97 13:10:48 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 蹍������ �´�←キ�� 甎�瓷� ��������.                  �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE S_Analog
PRIVATE f_s, mss, pth

pth = ""
DO Get_Path WITH pth, "S_ANALOG"
f_s = Int_File(pth, "DBF", "���Θ皀 ���� ��������")
IF EMPTY(f_s)
  RETURN
ENDIF
USE (f_s) ALIAS SOURCE
IF FSIZE("STO_CODE") = 0 .OR. ;
  FSIZE("NAME")     = 0 .OR. ;
  FSIZE("ANA_CODE") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�皰 �� ���� ��������!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"ANALOG")

DO Wt_Mess WITH "��瘁����→� ��������..."
SELECT SOURCE
SCAN FOR ANA_CODE # 0 .AND. SEEK(ANA_CODE, "STOCK")
  SELECT ANALOG
  IF EOF() .OR. BOF()
    APPEND BLANK
  ENDIF
  REPLACE CODE_1 WITH SOURCE.STO_CODE, ;
          NAME_1 WITH SOURCE.NAME,     ;
          CODE_2 WITH SOURCE.ANA_CODE
  SKIP
  SELECT SOURCE
ENDSCAN

SELECT ANALOG
DELETE REST
SET ORDER TO TAG CODE

SELECT STOCK
REPLACE ALL IS_CHANGE WITH IIF(SEEK(CODE,"ANALOG"), "�", " ")

DO Wt_Mess

CLOSE DATABASES

RETURN
