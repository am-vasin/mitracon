*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� SeekName     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                     ��爼�牀���┘ ━キ� か� ���瓷�.                     �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 02/11/2000 様�
PROCEDURE SeekName
PARAMETERS pVal

PRIVATE retVal

IF TYPE("upper_1") # "C"
  PRIVATE upper_1, upper_2
  STORE "" TO upper_1, upper_2
  DO SetUpper
ELSE
  IF LEN(upper_1) < 10
    DO SetUpper
  ENDIF
ENDIF

retVal = CHRTRAN(ALLTRIM(pVal), upper_1, upper_2)
retVal = STRTRAN(retVal, " ")

RETURN retVal
