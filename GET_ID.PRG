*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�           �ணࠬ�� �஢�ન ��ᯮ�� �ਭ�⮣� �����.               �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Get_ID
PARAMETERS pth  && ���� � ������.

PRIVATE mss, must_be, own_c

IF .NOT. File_O(pth+"ID_SEND.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���뫪� ��� ��ᯮ��!"
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
  mss(2) = CHR(0)+"���뫪� �� ��� ���!"
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
  mss(2) = CHR(0)+"�� 㤠���� ��।����� ����ᯮ�����!"
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
mss(2) = CHR(0)+"������ "+ALLTRIM(c_name)+"."
mss(3) = CHR(0)+"������ ���� ����� N "+ALLTRIM(STR(must_be))+  ;
                ", ����祭 ����� N "+ALLTRIM(STR(ID_SEND.TR_NUM))+  ;
                IIF(must_be = ID_SEND.TR_NUM, ". OK!", ".")
mss(4) = ""

IF must_be # ID_SEND.TR_NUM
  DIMENSION mss(5)
  mss(5) = ""
  mss(4) = CHR(0)+"  �த������ ��ࠡ��� ����? [ �� ] [ ��� ]"
  IF Out_Mess(7, "mss") = 2
    CLOSE DATABASES
    RETURN .F.
  ENDIF
ELSE
  DO Out_Mess WITH 5, "mss"
ENDIF

RETURN .T.