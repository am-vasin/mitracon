*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�               蹍������ �襪�腮 甎��→� �� ��┘����.                   �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Cus_Rep
PARAMETER super
PRIVATE v, pl

PUSH KEY CLEAR
HIDE MENU Main_M

DO Use_Dummy
DO Use_Link
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
IF super
  pl = Choose_P()
ELSE
  pl = sys_char
ENDIF
DO WHILE .T.
  SELECT ACCOUNT
  IF Custs("", .F., .F., .F., pl)
    DO Acc_Hist WITH CUS_CODE
  ELSE
    EXIT
  ENDIF
ENDDO

CLOSE DATABASES
POP KEY

RETURN
