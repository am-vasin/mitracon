*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�            �ணࠬ�� �ਥ�� ������ � �᭮����� ᪫���.                �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Receive1

PRIVATE c_name, c_code, pth_w, crs, s_sav, l, c

DO Prep_TD    && ������� ��� ��頥� �࠭����� ��४���
pth_w = tmpo_path+"TRANSMIT\"

IF .NOT. Get_File(pth_w)
  RETURN
ENDIF

c_code = 0
c_name = ""
crs    = SET("CURSOR") = "ON"
SET CURSOR OFF

ACTIVATE SCREEN
SAVE SCREEN TO s_sav

l = CEILING((WROWS()-4)/2)
c = FLOOR((WCOLS()-21)/2)
@ l+1, c+2 FILL TO l+4, c+22 COLOR (SCHEME(1,8))
@ l,   c SAY SPACE(21) COLOR SCHEME 5
@ l+1, c SAY PADC("��ᯠ�����!",21) COLOR SCHEME 5
@ l+2, c SAY PADC("����-�...",21) COLOR SCHEME 5
@ l+3, c SAY SPACE(21) COLOR SCHEME 5
c = "! ARJ E -V -Y "+pth_w+"F_STAT "+pth_w+" > nul"
&c
c = "!DEL "+pth_w+"F_STAT.A*"
&c
RESTORE SCREEN FROM s_sav

IF .NOT. Get_ID(pth_w)
  RETURN
ENDIF

SELECT 0

DO Get_Custs    && �������

DO Get_Arms     && ����稥 ����

DO Get_Users    && USER�

DO Get_Money    && ���⥦�

*DO Get_Bills    && ���  �� ࠡ����, �. �. �� ��।����� ��⠫�� ��ப� ��⮢

DO Get_Kassa    && ����� �த��殢

DO Get_Stock    && �����������

DO Put_ID WITH pth_w

CLEAR WINDOWS
CLOSE DATABASES

IF crs
  SET CURSOR ON
ENDIF
RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�               �ணࠬ�� ����� ����� �����⮢.                         �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Get_Custs
PRIVATE win_name, arr_w, skp, mss, n_in, n_out

win_name = PROGRAM()

*��������������������������������������������Ŀ
*�     ���㬥�� N 99999999999 �� 99.99.9999   �
*�                                            �
*����������������������������������������������

DO D_Win_N WITH 5, 56, "�������"

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

IF .NOT. File_O(pth_w+"ACCOUNT.DBF")
  @ 2, 3 SAY PADC("��� 䠩�� �����⮢!",   WCOLS()-6)
  @ 3, 3 SAY PADC("������ ��-�����...", WCOLS()-6)
  = INKEY(10,"MH")
  USE
  RELEASE WINDOW (win_name)
  RETURN
ENDIF
  
SELECT 0
USE (pth_w+"ACCOUNT") ALIAS SOURCE
SCAN
  @ 2, 3 SAY CUS_NAME
  IF SUBSYS # sys_char
    @ 3, 3 SAY PADC("�㦮�! ������ ��-�����...", WCOLS()-6)
    = INKEY(5, "HM")
    LOOP
  ENDIF
  IF SEEK(CUS_CODE, "ACCOUNT")
    IF .NOT. EMPTY(CUS_NAME) .AND. ACCOUNT.DATE_CORR < SOURCE.DATE_CORR
      @ 3, 3 SAY PADC("�������塞...", WCOLS()-6)
      REPLACE ACCOUNT.CUS_NAME   WITH CUS_NAME,     ;
              ACCOUNT.ACC_NOTE   WITH ACC_NOTE,     ;
              ACCOUNT.MARK1      WITH SOURCE.MARK1, ;
              ACCOUNT.MARK2      WITH SOURCE.MARK2, ;
              ACCOUNT.MARK3      WITH SOURCE.MARK3, ;
              ACCOUNT.MARK4      WITH SOURCE.MARK4, ;
              ACCOUNT.MARK5      WITH SOURCE.MARK5, ;
              ACCOUNT.DATE_CORR  WITH SOURCE.DATE_CORR, ;
              ACCOUNT.WHO_CORR   WITH SOURCE.WHO_CORR,  ;
              ACCOUNT.WHERE_CORR WITH SOURCE.WHERE_CORR

*      REPLACE ACCOUNT.ACC_NOTE WITH ACC_NOTE
    ENDIF
  ELSE
    @ 3, 3 SAY PADC("������塞...", WCOLS()-6)
    SELECT ACCOUNT
    APPEND BLANK
    REPLACE SUBSYS     WITH SOURCE.SUBSYS,    ;
            CUS_CODE   WITH SOURCE.CUS_CODE,  ;
            CUS_NAME   WITH SOURCE.CUS_NAME,  ;
            ID_CODE    WITH SOURCE.ID_CODE,   ;
            ACC_TYPE   WITH SOURCE.ACC_TYPE,  ;
            ACC_NOTE   WITH SOURCE.ACC_NOTE,  ;
            ACC_NOTED  WITH SOURCE.ACC_NOTED, ;
            MARK1      WITH SOURCE.MARK1,     ;
            MARK2      WITH SOURCE.MARK2,     ;
            MARK3      WITH SOURCE.MARK3,     ;
            MARK4      WITH SOURCE.MARK4,     ;
            MARK5      WITH SOURCE.MARK5,     ;
            DATE_CORR  WITH SOURCE.DATE_CORR, ;
            WHO_CORR   WITH SOURCE.WHO_CORR,  ;
            WHERE_CORR WITH SOURCE.WHERE_CORR,;
            DATE_ON    WITH SOURCE.DATE_ON,   ;
            WHO        WITH SOURCE.WHO,       ;
            WHERE      WITH SOURCE.WHERE
    SELECT SOURCE
  ENDIF
ENDSCAN

RELEASE WINDOW (win_name)

USE (base_path+"CUS_BIL") ORDER TAG CUS_CODE
SELECT ACCOUNT
SET ORDER TO TAG CUS_CODE
SELECT 0
USE (pth_w+"CUS_BIL") ALIAS SRS
SCAN
  IF .NOT. SEEK(CUS_CODE, "ACCOUNT")
    LOOP
  ENDIF
  IF SEEK(CUS_CODE, "CUS_BIL")
    IF CUS_BIL.DATE_CORR < SRS.DATE_CORR
      SCATTER TO arr_w
      SELECT CUS_BIL
      GATHER FROM arr_w
      SELECT SRS
    ENDIF
  ELSE
    SCATTER TO arr_w
    SELECT CUS_BIL
    APPEND FROM ARRAY arr_w
    SELECT SRS
  ENDIF
ENDSCAN
USE
SELECT CUS_BIL
USE
SELECT ACCOUNT
USE

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�               �ணࠬ�� ����� ����� ࠡ��� ����.                     �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Get_Arms
PRIVATE win_name, arr_w, skp, mss, n_in, n_out

win_name = PROGRAM()

*��������������������������������������������Ŀ
*�     ���㬥�� N 99999999999 �� 99.99.9999   �
*�                                            �
*����������������������������������������������

DO D_Win_N WITH 5, 56, "����稥 ����"

USE (base_path+"ARM_LST") ORDER TAG ARM_C
IF .NOT. File_O(pth_w+"ARM_LST.DBF")
  @ 2, 3 SAY PADC("��� 䠩�� ࠡ��� ����!",   WCOLS()-6)
  @ 3, 3 SAY PADC("������ ��-�����...", WCOLS()-6)
  = INKEY(10,"MH")
  USE
  RELEASE WINDOW (win_name)
ENDIF

SELECT 0
USE (pth_w+"ARM_LST") ALIAS SOURCE
SCAN
  @ 2, 3 SAY ARM_N
  IF SEEK(ARM_C, "ARM_LST")
    @ 3, 3 SAY PADC("�������塞...", WCOLS()-6)
    REPLACE ARM_LST.ARM_N WITH ARM_N
  ELSE
    @ 3, 3 SAY PADC("������塞...", WCOLS()-6)
    SELECT ARM_LST
    APPEND BLANK
    REPLACE ARM_C WITH SOURCE.ARM_C, ;
            ARM_N WITH SOURCE.ARM_N, ;
            ARM_S WITH SOURCE.ARM_S
    SELECT SOURCE
  ENDIF
ENDSCAN

RELEASE WINDOW (win_name)
USE
SELECT ARM_LST
USE
RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�               �ணࠬ�� ����� ����� USER��.                           �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Get_Users
PRIVATE win_name, arr_w, skp, mss, n_in, n_out

win_name = PROGRAM()

*��������������������������������������������Ŀ
*�     ���㬥�� N 99999999999 �� 99.99.9999   �
*�                                            �
*����������������������������������������������

DO D_Win_N WITH 5, 56, "USER�"

USE (base_path+"PERSONS") ORDER TAG CODE

IF .NOT. File_O(pth_w+"PERSONS.DBF")
  @ 2, 3 SAY PADC("��� 䠩�� ���㤭����!",   WCOLS()-6)
  @ 3, 3 SAY PADC("������ ��-�����...", WCOLS()-6)
  = INKEY(10,"MH")
  USE
  RELEASE WINDOW (win_name)
ENDIF

SELECT 0
USE (pth_w+"PERSONS") ALIAS SOURCE
SCAN
  @ 2, 3 SAY ALLTRIM(FAMILY)
  SCATTER TO arr_w
  IF SEEK(CODE, "PERSONS")
    @ 3, 3 SAY PADC("�������塞...", WCOLS()-6)
    SELECT PERSONS
    GATHER FROM arr_w
  ELSE
    @ 3, 3 SAY PADC("������塞...", WCOLS()-6)
    SELECT PERSONS
    APPEND FROM ARRAY arr_w
  ENDIF
  SELECT SOURCE
ENDSCAN

RELEASE WINDOW (win_name)
USE
SELECT PERSONS
USE
RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                    �ணࠬ�� ����� ��⮢.                            �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Get_Bills
PRIVATE win_name, arr_w, skp, mss, n_in, n_out

win_name = PROGRAM()

*��������������������������������������������Ŀ
*�     ���㬥�� N 99999999999 �� 99.99.9999   �
*�                                            �
*����������������������������������������������

DO D_Win_N WITH 5, 48, "���"

USE (base_path+"BILLS") ORDER TAG DOC_NUM

IF .NOT. File_O(pth_w+"BILLS.DBF")
  @ 2, 3 SAY PADC("��� 䠩�� ��⮢!",   WCOLS()-6)
  @ 3, 3 SAY PADC("������ ��-�����...", WCOLS()-6)
  = INKEY(10,"MH")
  USE
  RELEASE WINDOW (win_name)
ENDIF

SELECT 0
USE (pth_w+"BILLS") ALIAS SOURCE

STORE 0 TO n_in, n_out
SCAN
  n_in = n_in+1
  @ 2, 7 SAY "���㬥�� N "+LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5))+" �� "+TRANSFORM(DOC_DATE,"@D")
  skp = SEEK(FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM, "BILLS")
  @ 3, 2 SAY PADC(IIF(skp, "�ய�᪠��", "�����뢠��"), WCOLS()-4)
  IF skp
    LOOP
  ENDIF

  n_out = n_out+1
  SCATTER TO arr_w
  SELECT BILLS
  APPEND FROM ARRAY arr_w

  SELECT SOURCE
ENDSCAN

USE
SELECT BILLS
USE

@ 2, 2 CLEAR TO WROWS()-2, WCOLS()-3
@ 2, 2 SAY PADC("���⠭� "+ALLTRIM(STR(n_in))+" ���㬥�⮢, ����ᠭ� "+ ;
                ALLTRIM(STR(n_out))+".", WCOLS()-4)
@ 3, 2 SAY PADC("������ ��-�����...", WCOLS()-4)
= INKEY(15,"HM")
RELEASE WINDOW (win_name)

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                       �ணࠬ�� ���⥦��.                             �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Get_Money
PRIVATE win_name, arr_w, skp, mss, n_in, n_out

win_name = PROGRAM()

*��������������������������������������������Ŀ
*�     ���㬥�� N 99999999999 �� 99.99.9999   �
*�                                            �
*����������������������������������������������

DO D_Win_N WITH 5, 48, "���⥦�"

USE (base_path+"INCMONEY") ORDER TAG DOC_NUM

IF .NOT. File_O(pth_w+"INCMONEY.DBF")
  @ 2, 3 SAY PADC("��� 䠩�� ��室� �����!",   WCOLS()-6)
  @ 3, 3 SAY PADC("������ ��-�����...", WCOLS()-6)
  = INKEY(10,"MH")
  USE
  RELEASE WINDOW (win_name)
ENDIF

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"BILLS") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM

SELECT 0
USE (pth_w+"INCMONEY") ALIAS SOURCE

STORE 0 TO n_in, n_out
SCAN
  n_in = n_in+1
  @ 2, 7 SAY "���㬥�� N "+LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5))+" �� "+TRANSFORM(DOC_DATE,"@D")
  skp = SEEK(FIRM+DOC_TYPE+LEFT(DTOS(DOC_DATE),4)+DOC_NUM, "INCMONEY")  && ���㬥�� 㦥 ����!
  IF .NOT. skp 
    IF EMPTY(CUS_CODE)    && ��� ������!
      skp = .T.
    ELSE
      IF .NOT. SEEK(CUS_CODE,"ACCOUNT")  && �� ��� ������!
        skp = .T.
      ENDIF
    ENDIF
  ENDIF
  IF .NOT. skp
    skp = .NOT. DOC_TYPE $ " 31�"
  ENDIF
  @ 3, 2 SAY PADC(IIF(skp, "�ய�᪠��", "�����뢠��"), WCOLS()-4)
  IF skp
    LOOP
  ENDIF

  n_out = n_out+1
  SCATTER TO arr_w
  SELECT INCMONEY            && ������塞 ���㬥��
  APPEND FROM ARRAY arr_w

*   ������塞 ���죨, ��襤訥 �� �������
  SELECT CUS_REST
  IF .NOT. SEEK(STR(INCMONEY.CUS_CODE)+INCMONEY.FIRM)
    APPEND BLANK
    REPLACE FIRM     WITH INCMONEY.FIRM, ;
            CUS_CODE WITH INCMONEY.CUS_CODE
  ENDIF
  IF INCMONEY.DOC_TYPE $ "34"
    REPLACE ACC_SUMD WITH ACC_SUMD+INCMONEY.MONEY_D
  ELSE
    REPLACE ACC_SUM WITH ACC_SUM+INCMONEY.MONEY_R
  ENDIF

  IF .NOT. EMPTY(INCMONEY.BILL_NUM)   && �⬥砥� 䠪� ������ � ���!
    IF SEEK(INCMONEY.BILL_FIRM+LEFT(DTOS(INCMONEY.BILL_DAT),4)+INCMONEY.BILL_NUM, "BILLS")
      REPLACE BILLS.STATUS   WITH "����祭�",  ;
              BILLS.CUS_CODE WITH INCMONEY.CUS_CODE
      IF SEEK(BILLS.NUM_LIST,"LIST_TIT")    && ���४��㥬 ��� ������ � ᯨ᪥
        REPLACE LIST_TIT.CLI_CODE WITH INCMONEY.CUS_CODE, ;
                LIST_TIT.PAY_FIRM WITH INCMONEY.FIRM,     ;
                LIST_TIT.PAY_TYPE WITH INCMONEY.DOC_TYPE, ;
                LIST_TIT.PAY_NUM  WITH INCMONEY.DOC_NUM,  ;
                LIST_TIT.PAY_DATE WITH INCMONEY.DOC_DATE, ;
                LIST_TIT.WHO_PAY  WITH INCMONEY.WHO
      ENDIF
    ENDIF
  ENDIF

  SELECT SOURCE
ENDSCAN

USE
SELECT INCMONEY
USE
SELECT ACCOUNT
USE
SELECT LIST_TIT
USE
SELECT CUS_REST
USE
SELECT BILLS
USE

@ 2, 2 CLEAR TO WROWS()-2, WCOLS()-3
@ 2, 2 SAY PADC("���⠭� "+ALLTRIM(STR(n_in))+" ���㬥�⮢, ����ᠭ� "+ ;
                ALLTRIM(STR(n_out))+".", WCOLS()-4)
@ 3, 2 SAY PADC("������ ��-�����...", WCOLS()-4)
= INKEY(15,"HM")
RELEASE WINDOW (win_name)

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                �ணࠬ�� ����� ���� �த��殢.                      �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Get_Kassa
PRIVATE win_name, arr_w, skp, mss, n_in, n_out

win_name = PROGRAM()

*��������������������������������������������Ŀ
*�     ���㬥�� N 99999999999 �� 99.99.9999   �
*�                                            �
*����������������������������������������������

DO D_Win_N WITH 5, 48, "����� �த��殢"

USE (base_path+"ARM_LST") ORDER TAG ARM_C

IF .NOT. File_O(pth_w+"KASSA.DBF")
  @ 2, 3 SAY PADC("��� 䠩�� ���� ࠡ��� ����!",   WCOLS()-6)
  @ 3, 3 SAY PADC("������ ��-�����...", WCOLS()-6)
  = INKEY(10,"MH")
  USE
  RELEASE WINDOW (win_name)
ENDIF

SELECT 0
USE (base_path+"KASSA") ORDER TAG DOC_NUM

SELECT 0
USE (pth_w+"KASSA") ALIAS SOURCE

STORE 0 TO n_in, n_out
SCAN
  n_in = n_in+1
  @ 2, 7 SAY "���㬥�� N "+LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5))+" �� "+TRANSFORM(DOC_DATE,"@D")
  skp = .NOT. SEEK(LEFT(DTOS(DOC_DATE),4)+DOC_NUM, "KASSA")
  IF .NOT. skp
    skp = .NOT. EMPTY(KASSA.WHO_GET) .OR. EMPTY(WHO_GET)
  ENDIF
  @ 3, 2 SAY PADC(IIF(skp, "�ய�᪠��", "���४��㥬"), WCOLS()-4)
  IF skp
    LOOP
  ENDIF

  n_out = n_out+1
  REPLACE KASSA.WHO_GET WITH WHO_GET
  IF SEEK(KASSA.WHERE,"ARM_LST")
    IF KASSA.VALUTA = "$"
      REPLACE ARM_LST.KASSA_USD WITH ARM_LST.KASSA_USD-KASSA.TOTAL
    ELSE
      REPLACE ARM_LST.KASSA WITH ARM_LST.KASSA-KASSA.TOTAL
    ENDIF
  ENDIF
ENDSCAN

USE
SELECT KASSA
USE
SELECT ARM_LST
USE

@ 2, 2 CLEAR TO WROWS()-2, WCOLS()-3
@ 2, 2 SAY PADC("���⠭� "+ALLTRIM(STR(n_in))+" ���㬥�⮢, ����ᠭ� "+ ;
                ALLTRIM(STR(n_out))+".", WCOLS()-4)
@ 3, 2 SAY PADC("������ ��-�����...", WCOLS()-4)
= INKEY(15,"HM")
RELEASE WINDOW (win_name)

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�              �ணࠬ�� ����� � �ᯠ����� ARJ 䠩��.                  �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Get_File
PARAMETER pth

PRIVATE get_pth, i, mss, mss1, nm_w, w, is_fdd

get_pth = FromDisk(pth+"F_STAT.ARJ")

IF EMPTY(get_pth)
  RETURN .F.
ENDIF

is_fdd = .F.
IF LEN(get_pth) = 3
  IF INLIST(UPPER(LEFT(get_pth,2)), "A:", "B:")
    is_fdd = .T.
  ENDIF
ENDIF
IF is_fdd
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �㤥� ��᪥�? [ �� ] [ ��� ]"
  mss(3) = ""
  i = 1
  DO WHILE .T.
    nm_w = "F_STAT.A"+RIGHT(STR(100+i,3),2)
    IF Out_Mess(5, "mss") = 2
      EXIT
    ENDIF
    w = get_pth
    CALL IsDisk WITH w
    IF w # get_pth
      DIMENSION mss1(3)
      mss1(1) = ""
      mss1(2) = CHR(0)+"�� �� ��⠭����� ��᪥��!"
      mss1(3) = ""
      DO Out_Mess WITH 7,"mss1"
      LOOP
    ENDIF
    IF .NOT. File_O(get_pth+nm_w)
      DIMENSION mss1(3)
      mss1(1) = ""
      mss1(2) = CHR(0)+"�� ��⠭����� �� �� ��᪥��!"
      mss1(3) = ""
      DO Out_Mess WITH 7,"mss1"
      LOOP
    ENDIF
    COPY FILE (get_pth+nm_w) TO (pth+nm_w)
    i = i+1
  ENDDO
ELSE
  i = 1
  DO WHILE .T.
    nm_w = "F_STAT.A"+RIGHT(STR(100+i,3),2)
    w = get_pth
    IF .NOT. File_O(get_pth+nm_w)
      EXIT
    ENDIF
    DO Wt_Mess WITH "��襬 "+nm_w
    COPY FILE (get_pth+nm_w) TO (pth+nm_w)
    i = i+1
  ENDDO
ENDIF

DIMENSION mss(4)
mss(1) = ""
mss(2) = CHR(0)+"�� ��⠭� �ࠢ��쭮? ��稭��� ��ࠡ���?"
mss(3) = CHR(0)+"  [ �� ] [ ���, �⪠�����! ]"
mss(4) = ""
RETURN Out_Mess(5, "mss") = 1

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�               �ணࠬ�� ����� �ਥ�� � ��⮪��.                     �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Put_ID
PARAMETERS pth  && ���� � ������.

PRIVATE mss, i_code, i_corr, i_self, arr_w, n, i, w

SELECT ID_SEND
n = FCOUNT()
STORE 0 TO i_code, i_corr, i_self
FOR i = 1 TO n
  w = FIELD(i)
  IF w == "TR_CODE"
    i_code = i
  ENDIF
  IF w == "ID_CORR"
    i_corr = i
  ENDIF
  IF w == "ID_SELF"
    i_self = i
  ENDIF
ENDFOR

SCATTER TO arr_w

arr_w(i_code) = "RM"
w = arr_w(i_corr)
arr_w(i_corr) = arr_w(i_self)
arr_w(i_self) = w

SELECT TR_LOG
APPEND FROM ARRAY arr_w

RETURN

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�           �ணࠬ�� �஢�ન ��ᯮ�� �ਭ�⮣� �����.               �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Get_ID
PARAMETERS pth  && ���� � ������.

PRIVATE mss, must_be, o_cod, m_cod

IF .NOT. File_O(pth+"ID_SEND.DBF")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���뫪� ��� ��ᯮ��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN .F.
ENDIF

USE (base_path+"CUSTOMER")
LOCATE FOR .NOT. EMPTY(SELF)
o_cod = STR(CODE, 8)
LOCATE FOR .NOT. EMPTY(SUP)
m_cod = STR(CODE, 8)

USE (pth+"ID_SEND")

IF o_cod # ID_CORR
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"���뫪� �� ��� ���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN .F.
ENDIF

IF .F. &&ID_SELF # m_cod
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"����ᯮ����� - �� �᭮���� ᪫��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN .F.
ENDIF

SELECT 0
USE (base_path+"TR_LOG") ORDER TAG TR_NUM DESCENDING
IF .NOT. SEEK("RM"+ID_SEND.ID_SELF)
  must_be = 1
ELSE
  must_be = TR_NUM+1
ENDIF

DIMENSION mss(3)

mss(1) = ""
mss(2) = CHR(0)+"������ ���� ����� N "+ALLTRIM(STR(must_be))+  ;
                ", ����祭 ����� N "+ALLTRIM(STR(ID_SEND.TR_NUM))+  ;
                IIF(must_be = ID_SEND.TR_NUM, ". OK!", ".")
mss(3) = ""

IF must_be # ID_SEND.TR_NUM
  DIMENSION mss(5)
  mss(5) = ""
  mss(4) = CHR(0)+"  �த������ ��ࠡ��� ���뫪�? [ �� ] [ ��� ]"
  IF Out_Mess(7, "mss") = 2
    CLOSE DATABASES
    RETURN .F.
  ENDIF
ELSE
  DO Out_Mess WITH 5, "mss"
ENDIF

RETURN .T.

*�����������������������������������������������������������������������ͻ
*�                                                                       �
*�                  �ணࠬ�� �ਥ�� ������������.                       �
*�                                                                       �
*�����������������������������������������������������������������������ͼ
PROCEDURE Get_Stock
PARAMETERS pth  && ���� � ������.

PRIVATE win_name, arr_w, skp, mss, n_in, n_out, arr_w, cnt, cnt_ren, tmpQnt

win_name = PROGRAM()

*��������������������������������������������Ŀ
*�     ���㬥�� N 99999999999 �� 99.99.9999   �
*�                                            �
*����������������������������������������������

DO D_Win_N WITH 5, 56, "�����������"

IF .NOT. File_O(pth_w+"DIVISION.DBF")
  @ 2, 3 SAY PADC("��� 䠩�� ࠧ�����!",   WCOLS()-6)
  @ 3, 3 SAY PADC("������ ��-�����...", WCOLS()-6)
  = INKEY(10,"MH")
ELSE
  @ 2, 3 SAY PADC("������� ������������!",   WCOLS()-6)
  @ 3, 3 SAY SPACE(WCOLS()-6)
  USE (base_path+"DIVISION")
  SELECT 0
  USE (pth_w+"DIVISION") ALIAS SOURCE
  SCAN
    SCATTER TO arr_w
    SELECT DIVISION
    IF EOF()
      APPEND BLANK
    ENDIF
    GATHER FROM arr_w
    SKIP
    SELECT SOURCE
  ENDSCAN
  USE
  SELECT DIVISION
  DELETE REST
  USE
ENDIF
  
IF .NOT. File_O(pth_w+"CLASSES.DBF")
  @ 2, 3 SAY PADC("��� 䠩�� ����ᮢ!",   WCOLS()-6)
  @ 3, 3 SAY PADC("������ ��-�����...", WCOLS()-6)
  = INKEY(10,"MH")
ELSE
  @ 2, 3 SAY PADC("������ ������������!",   WCOLS()-6)
  @ 3, 3 SAY SPACE(WCOLS()-6)
  USE (base_path+"CLASSES")
  SELECT 0
  USE (pth_w+"CLASSES") ALIAS SOURCE
  SCAN
    SCATTER TO arr_w
    SELECT CLASSES
    IF EOF()
      APPEND BLANK
    ENDIF
    GATHER FROM arr_w
    SKIP
    SELECT SOURCE
  ENDSCAN
  USE
  SELECT CLASSES
  DELETE REST
  USE
ENDIF

IF .NOT. File_O(pth_w+"DEVELOP.DBF")
  @ 2, 3 SAY PADC("��� 䠩�� ��-�ந�����⥫��!",   WCOLS()-6)
  @ 3, 3 SAY PADC("������ ��-�����...", WCOLS()-6)
  = INKEY(10,"MH")
ELSE
  @ 2, 3 SAY PADC("����-�ந�����⥫�!",   WCOLS()-6)
  @ 3, 3 SAY SPACE(WCOLS()-6)
  USE (base_path+"DEVELOP")
  SELECT 0
  USE (pth_w+"DEVELOP") ALIAS SOURCE
  SCAN
    SCATTER TO arr_w
    SELECT DEVELOP
    IF EOF()
      APPEND BLANK
    ENDIF
    GATHER FROM arr_w
    SKIP
    SELECT SOURCE
  ENDSCAN
  USE
  SELECT DEVELOP
  DELETE REST
  USE
ENDIF

IF .NOT. File_O(pth_w+"ANALOG.DBF")
  @ 2, 3 SAY PADC("��� 䠩�� ��������!",   WCOLS()-6)
  @ 3, 3 SAY PADC("������ ��-�����...", WCOLS()-6)
  = INKEY(10,"MH")
ELSE
  @ 2, 3 SAY PADC("�������!",   WCOLS()-6)
  @ 3, 3 SAY SPACE(WCOLS()-6)
  USE (base_path+"ANALOG")
  SELECT 0
  USE (pth_w+"ANALOG") ALIAS SOURCE
  SCAN
    SCATTER TO arr_w
    SELECT ANALOG
    IF EOF()
      APPEND BLANK
    ENDIF
    GATHER FROM arr_w
    SKIP
    SELECT SOURCE
  ENDSCAN
  USE
  SELECT ANALOG
  DELETE REST
  USE
ENDIF

@ 2, 3 SAY PADC("�����������!",   WCOLS()-6)
@ 3, 3 SAY SPACE(WCOLS()-6)
IF .NOT. File_O(pth_w+"STOCK.DBF")
  @ 2, 3 SAY PADC("��� 䠩�� � ����������ன!",   WCOLS()-6)
  @ 3, 3 SAY PADC("������ ��-�����...", WCOLS()-6)
  = INKEY(10,"MH")
ELSE
  USE (base_path+"STOCK") ORDER TAG CODE
  SELECT 0
  USE (base_path+"STO_LOG.DBF")
  SELECT 0
  USE (pth_w+"STOCK") ALIAS SOURCE
  cnt = 0
  cnt_ren = 0
  SCAN    && FOR SEEK(CODE, "STOCK")
    IF .NOT. SEEK(CODE, "STOCK")
      SELECT STOCK
      APPEND BLANK
      REPLACE CODE WITH SOURCE.CODE
      SELECT SOURCE
    ENDIF
    skp = .F.
    IF SOURCE.PREFIX   # STOCK.PREFIX .OR. ;
       SOURCE.NAME     # STOCK.NAME .OR.   ;
       SOURCE.PRO_CODE # STOCK.PRO_CODE
       skp = .T.
       cnt_ren = cnt_ren+1
       SELECT STO_LOG
       APPEND BLANK
       REPLACE CODE     WITH STOCK.CODE, ;
               OLD_PRF  WITH STOCK.PREFIX, ;
               OLD_NAM  WITH STOCK.NAME, ;
               OLD_PRO  WITH STOCK.PRO_CODE, ;
               NEW_PRF  WITH SOURCE.PREFIX, ;
               NEW_NAM  WITH SOURCE.NAME,   ;
               NEW_PRO  WITH SOURCE.PRO_CODE, ;
               DAT_CORR WITH DATE(),        ;
               WHERE_C  WITH "***" 
    ENDIF
    SELECT STOCK
    IF SOURCE.QNT_STORE > 0
      tmpQnt = MIN(SOURCE.QNT_STORE, 999999)
    ELSE
      tmpQnt = MAX(SOURCE.QNT_STORE, -99999)
    ENDIF
    IF skp = .T.  .OR. DOW(DATE()) = 2
      REPLACE PREFIX    WITH SOURCE.PREFIX,   ;
              NAME      WITH SOURCE.NAME,     ;
              PRODUCER  WITH SOURCE.PRODUCER, ;
              PREFIX1   WITH SeekName(SOURCE.PREFIX),   ;
              NAME1     WITH SeekName(SOURCE.NAME),     ;
              PRODUCER1 WITH SeekName(SOURCE.PRODUCER), ;
              PRO_CODE  WITH SOURCE.PRO_CODE, ;
              NUM_PART  WITH SOURCE.NUM_PART, ;
              DIV_CODE  WITH SOURCE.DIV_CODE, ;
              CLASS     WITH SOURCE.CLASS,    ;
              QNT_STORE WITH tmpQnt,          ;
              STOP_PUR  WITH IIF(FSIZE("STOP_PUR","SOURCE") = 0,STOP_PUR,SOURCE.STOP_PUR),;
              DATE_OFF  WITH IIF(QNT = 0      .AND. ;
                                 QNT_SAVE = 0 .AND. ;
                                 QNT_REZ  = 0 .AND. ;
                                 QNT_SPEC = 0 .AND. ;
                                 QNT_REAL = 0, SOURCE.DATE_OFF, {})
    ENDIF  
    REPLACE PRICE_O   WITH SOURCE.PRICE,    ;
            PRICE_B   WITH SOURCE.PRICE_B,  ;
            PRICE_B_R WITH SOURCE.PRICE_B_R,;
            PRICE     WITH SOURCE.PRICE_B,  ;
            PRICE_R   WITH Clc_Rub(SOURCE.PRICE_B), ;
            PRICE_O1  WITH ROUND((SOURCE.PRICE_B+SOURCE.PRICE)/2,4),;
            QNT_ROUND WITH SOURCE.QNT_ROUND
    REPLACE PRICE_O1_R WITH Clc_Rub(PRICE_O1), ;
            PRICE_O_R  WITH Clc_Rub(PRICE_O)
    SELECT SOURCE
    cnt = cnt+1
    @ 3, 3 SAY PADC(ALLTRIM(STR(cnt)),WCOLS()-6)
  ENDSCAN

  USE (base_path+"DEVELOP") ORDER TAG CODE
  SELECT 0
  USE (base_path+"ANALOG") ORDER TAG CODE
  SELECT STOCK
  SET RELATION TO CODE INTO ANALOG, PRO_CODE INTO DEVELOP
  REPLACE ALL  ;
    IS_CHANGE WITH IIF(FOUND("ANALOG"), "�", " "),   ;
    PRODUCER  WITH DEVELOP.B_NAME
  WAIT WINDOW ALLTRIM(STR(cnt_ren))+" ����権 ����稫� ���� �����, �. ��ୠ�!"
  RELEASE WINDOW (win_name)
ENDIF
USE
IF USED("DEVELOP")
  SELECT DEVELOP
  USE
ENDIF
IF USED("STO_LOG")
  SELECT STO_LOG
  USE
ENDIF
IF USED("ANALOG")
  SELECT ANALOG
  USE
ENDIF

RETURN