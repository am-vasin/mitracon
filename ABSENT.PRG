*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Absent       ��о�｀砒┴ ��むォ ��瓱�           02.02.98 09:34:56 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               ����←キ┘ ��Ж罔� � 甎�甌� �矚竄痰≪鉗��.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Absent
PARAMETER sto_c
PRIVATE s_sav

s_sav = SELECT()

SELECT 0
USE (base_path+"ABSENT") ORDER TAG CODE
IF .NOT. SEEK(sto_c)
  GO TOP
  IF CODE # 0
    APPEND BLANK
  ENDIF
ENDIF
REPLACE CODE WITH sto_c, D_DATE WITH DATE()

USE

SELECT (s_sav)

RETURN
