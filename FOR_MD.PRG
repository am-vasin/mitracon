*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� For_MD       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               �砒モ �� 〓Б����� か� ����┼� �磧┃瓷���.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 10/18/1999 様�
PROCEDURE For_MD

PRIVATE d_0, d_1, dep_c, w

d_0 = {}
d_1 = DATE()
dep_c = 0
IF File_O(prmo_path+"FOR_MD.MEM")
  RESTORE FROM (prmo_path+"FOR_MD.MEM") ADDITIVE
  IF TYPE("___d") = "N"
    dep_c = ___d
  ENDIF
ENDIF

IF .NOT. G_Parms()
  RETURN
ENDIF

DELETE FILE (tmpo_path+"FOR_MD.DBF")
CREATE DBF (tmpo_path+"FOR_MD.DBF") ;
  ( FIRM      C( 3), ;
    REV_DOC   C(10), ;
    REV_DATE  D,     ;
    SALE_DOC  C(10), ;
    SALE_DATE D,     ;
    CUS_CODE  N( 8), ;
    CUS_NAME  C(50), ;
    WHO_REV_C N( 5), ;
    WHO_REV   C(24), ;
    WHO_SAL_C N( 5), ;
    WHO_SAL   C(24), ;
    STO_CODE  N( 7), ;
    PREFIX    C( 4), ;
    NAME      C(35), ;
    PRODUCER  C( 8), ;
    QNT       N( 6), ;
    PRICE_D   N(10,4) )

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_REV

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_SAL AGAIN

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS SALED AGAIN
SET RELATION TO WHO INTO P_SAL
SET RELATION TO CUS_CODE INTO ACCOUNT ADDITIVE

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON ALIAS REV AGAIN
SET RELATION TO WHO INTO P_REV

SEEK DTOS(d_0)
IF .NOT. FOUND()
  w = RECNO(0)
  IF BETWEEN(w, 1, RECCOUNT())
    GO w
  ENDIF
ENDIF

DO Wt_Mess WITH "��ぁ�爲� ����諷!"

SCAN REST FOR TOTAL_R < 0 .AND. .NOT. EMPTY(REQ_DOC) WHILE DOC_DATE <= d_1
  @ WROWS()-1, 0 SAY PADC(DTOC(DOC_DATE), WCOLS())
  IF .NOT. SEEK(FIRM+LEFT(DTOS(REQ_DATE),4)+REQ_DOC, "SALED")
    LOOP
  ENDIF
  IF P_SAL.DEP_NO # dep_c
    LOOP
  ENDIF
  SELECT SALE
  SEEK REV.FIRM+LEFT(DTOS(REV.DOC_DATE),4)+REV.DOC_NUM
  SCAN REST WHILE REV.FIRM = FIRM .AND. ;
                  REV.DOC_DATE = DOC_DATE .AND. ;
                  REV.DOC_NUM = DOC_NUM
    SELECT FOR_MD
    APPEND BLANK
    REPLACE FIRM      WITH SALED.FIRM, ;
            REV_DOC   WITH REV.DOC_NUM,    ;
            REV_DATE  WITH REV.DOC_DATE,   ;
            SALE_DOC  WITH SALED.DOC_NUM,  ;
            SALE_DATE WITH SALED.DOC_DATE, ;
            CUS_CODE  WITH SALED.CUS_CODE, ;
            CUS_NAME  WITH ACCOUNT.CUS_NAME, ;
            WHO_REV_C WITH REV.WHO, ;
            WHO_REV   WITH P_REV.FAMILY, ;
            WHO_SAL_C WITH SALED.WHO, ;
            WHO_SAL   WITH P_SAL.FAMILY, ;
            STO_CODE  WITH SALE.CODE, ;
            PREFIX    WITH STOCK.PREFIX, ;
            NAME      WITH STOCK.NAME, ;
            PRODUCER  WITH STOCK.PRODUCER, ;
            QNT       WITH SALE.QNT,     ;
            PRICE_D   WITH SALE.PRICE_D
    SELECT SALE
  ENDSCAN
  SELECT REV
ENDSCAN
DO Wt_Mess

CLOSE DATABASES

PRIVATE f_name
IF TYPE("___f") = "C"
  f_name = ___f
ELSE
  f_name = ""
ENDIF

f_name = New_File(f_name, "dbf", "��� ����� � 爛с�赳�皰�")
IF .NOT. EMPTY(f_name)
  COPY FILE (tmpo_path+"FOR_MD.DBF") TO (f_name)
  w = RAT("\", f_name)
  ___f = LEFT(f_name,w-1)
ENDIF

SAVE ALL LIKE ___* TO (prmo_path+"FOR_MD.MEM")

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� G_Parms      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                  ��皀���皋↓�� ������キ┘ �����モ牀�.                  �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10/18/1999 陳�
PROCEDURE G_Parms
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※��罔�
*

PRIVATE stat_type     && �┓ ��※��罔�: 0 - �メ�����珥�覃;
                                        1 - ゛���;
                                        2 - BROWSE - ��゛���;
                                        3 - BROWSE - 甎�甌�.
PRIVATE what_do       && ��� 爛Θ��.
PRIVATE menu_name     && ��� �瓱�縲������ �キ�.
PRIVATE last_mouse    && �爛�� ��甄イ�ィ� ����皋� Бム┃�� ������.
PRIVATE win_name      && ��� ���� ( ���� か� BROWSE ).

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ��ぅ爨�皀�讚�� ��痰� �牀������:
*

USE (base_path+"DEPS") ORDER TAG B_NAME

PRIVATE ex, d_names, d_codes, p
ex = 0
SCAN
  ex = ex+1
  DIMENSION d_names(ex), d_codes(ex)
  d_names(ex) = B_NAME
  d_codes(ex) = CODE
ENDSCAN
USE
IF ex < 1
  DIMENSION d_names(3)
  d_names(1) = ""
  d_names(2) = CHR(0)+"���甌� �發カ�� �竅�!"
  d_names(3) = ""
  DO Out_Mess WITH 7, "d_names"
  RETURN .F.
ENDIF

ex = 1
p = ASCAN(d_codes, dep_c)
p = MAX(p,1)

*------------------------------------------------------------------------
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                敖陳陳陳陳陳陳陳朕           �
*�          �發カ �....:....!....:.�           �
*�                青陳陳陳陳陳陳陳潰           �
*� ��Б��硅 � ��.��.����(F1) �� ��.��.����(F1) �
*�      < OK Ctrl-W > < �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 49, "�������皀 ��゛�聶"
@ 3, 12 SAY "�發カ"
@ 5, 3 SAY "��Б��硅 �           (F1) ��           (F1)"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 5, 14 GET d_0 PICTURE "@D" WHEN Set_F1(1) VALID Clr_F1()
@ 5, 32 GET d_1 PICTURE "@D" WHEN Set_F1(2) VALID Clr_F1()
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
@ 2, 18 GET p PICTURE "@^" FROM d_names SIZE 1, 18

READ CYCLE
dep_c = d_codes(p)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Set_F1       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �痰���→� �÷�｀砒┴� F1.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10/18/1999 陳�
PROCEDURE Set_F1
PARAMETERS t

IF t = 1
  ON KEY LABEL F1 DO S_Date WITH 1
ELSE
  ON KEY LABEL F1 DO S_Date WITH 2
ENDIF

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� S_Date       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �襦�� ��硅 �� ���キ��瑟.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10/18/1999 陳�
PROCEDURE S_Date
PARAMETERS t

IF t = 1
  d_0 = Get_Date(d_0)
  SHOW GET d_0
ELSE
  d_1 = Get_Date(d_1)
  SHOW GET d_1
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Clr_F1       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          �腮痰�� �÷�｀砒┴�.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 10/18/1999 陳�
PROCEDURE Clr_F1

ON KEY LABEL F1

RETURN .T.
