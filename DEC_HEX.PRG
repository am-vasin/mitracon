PROCEDURE Dec_Hex
PARAMETERS DecNumber

tmpRest=MOD(DecNumber,16)
tmpInt=INT(DecNumber/16)
HexNumber=''
DO WHILE .T.
DO CASE
CASE tmpRest=10
	HexNumber='A'+HexNumber
CASE tmpRest=11
	HexNumber='B'+HexNumber
CASE tmpRest=12
	HexNumber='C'+HexNumber
CASE tmpRest=13
	HexNumber='D'+HexNumber
CASE tmpRest=14
	HexNumber='E'+HexNumber
CASE tmpRest=15
	HexNumber='F'+HexNumber
OTHERWISE
	HexNumber=ALLTRIM(STR(tmpRest))+HexNumber
ENDCASE
tmpRest=MOD(tmpInt,16)
tmpInt1=tmpInt
tmpInt=INT(tmpInt/16)
IF tmpInt=0
	DO CASE
	CASE tmpInt1=10
		HexNumber='A'+HexNumber
	CASE tmpInt1=11
		HexNumber='B'+HexNumber
	CASE tmpInt1=12
		HexNumber='C'+HexNumber
	CASE tmpInt1=13
		HexNumber='D'+HexNumber
	CASE tmpInt1=14
		HexNumber='E'+HexNumber
	CASE tmpInt1=15
		HexNumber='F'+HexNumber
	OTHERWISE
		HexNumber=ALLTRIM(STR(tmpInt1))+HexNumber
  ENDCASE
	EXIT
ENDIF
ENDDO
RETURN HexNumber