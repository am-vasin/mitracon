*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�           蹍������ �牀▲爲� ��甎�珥� �爬�閧��� ���モ�.               �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE Get_ID
PARAMETERS pth  && 鉘� � ���モ�.

PRIVATE mss, must_be, own_c

IF .NOT. File_O(pth+"ID_SEND.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��瘠��� ．� ��甎�珥�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN .F.
ENDIF

SELECT 0
USE CUSTOMER
LOCATE FOR .NOT. EMPTY(SELF)
own_c = STR(CODE,8)

USE (pth+"ID_SEND")

IF own_c # ID_CORR
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��瘠��� �� か� ���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN .F.
ENDIF

SELECT 0
USE CUSTOMER
LOCATE FOR CODE = VAL(ALLTRIM(ID_SEND.ID_SELF))
IF EMPTY(RYN)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� 磴�モ瘴 ��爛ぅ��碎 ��玻メ���ぅ���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN .F.
ENDIF

c_code = CODE
c_name = B_NAME

USE TR_LOG ORDER TAG TR_NUM DESCENDING

IF .NOT. SEEK("RF"+ID_SEND.ID_SELF)
  must_be = 1
ELSE
  must_be = TR_NUM+1
ENDIF

DIMENSION mss(4)

mss(1) = ""
mss(2) = CHR(0)+"�┼��� "+ALLTRIM(c_name)+"."
mss(3) = CHR(0)+"���Ε� °碎 ���モ N "+ALLTRIM(STR(must_be))+  ;
                ", ���竍キ ���モ N "+ALLTRIM(STR(ID_SEND.TR_NUM))+  ;
                IIF(must_be = ID_SEND.TR_NUM, ". OK!", ".")
mss(4) = ""

IF must_be # ID_SEND.TR_NUM
  DIMENSION mss(5)
  mss(5) = ""
  mss(4) = CHR(0)+"  蹍ぎ���ガ �÷�｀皖� �砒モ�? [ �� ] [ �モ ]"
  IF Out_Mess(7, "mss") = 2
    CLOSE DATABASES
    RETURN .F.
  ENDIF
ELSE
  DO Out_Mess WITH 5, "mss"
ENDIF

RETURN .T.
