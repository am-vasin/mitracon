*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Check_L      ��о�｀砒┴ ��むォ ��瓱�           28.05.97 14:15:25 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�             蹍������ 筮爼�牀����� 甎�瓷� �牀��� � 腑����.             �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Check_L
PARAMETER all_arms

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

PRIVATE ex, arm_names, arm_codes, arm_point, dat0, dat1, n
*
*    ��爼�珮ガ 甎�甌� ��｀腮� �メ�.
*
USE ARM_LST
n = 0
arm_codes = ""
SCAN
  n = n+1
  DIMENSION arm_names(n)
  arm_names(n) = ARM_N
  arm_codes = arm_codes+ARM_C+"!"
ENDSCAN

arm_point = AT(arm+"!", arm_codes)
arm_point = (arm_point+3)/4
dat0 = DATE()
dat1 = DATE()
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                  敖陳陳陳陳陳陳陳陳跳   �
*�   ��｀腑� �メ皰  � 1234567890123456 �   �
*�                  塒様様様様様様様様夕   �
*� 敖 ��皀燿�� ��� 陳陳陳陳陳陳陳陳陳陳陳� �
*� �    �  ��.��.����  ��  ��.��.����    � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*�    < OK Ctrl-W > < �皖���碎瘴 Esc >     �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
ex   = 1

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 10, 45, "���甌� 腑��� �� �ム┏�"
@  3, 5 SAY "��｀腑� �メ皰"
DO Sun_Bord WITH 5, 3, 7, 41, " ��皀燿�� ��� "
@ 2, 20 GET arm_point PICTURE "@^" FROM arm_names SIZE 1, 20 WHEN all_arms
@ 6,  8 SAY "� "  GET dat0 PICTURE "@D"
@ 6, 23 SAY "�� " GET dat1 PICTURE "@D"
@ 8,  6 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF ex = 2
  RETURN
ENDIF
*--------------------------------------------------------------------------
*   ��爼�珮ガ 甎�甌� か� �牀甃�矗�
*
DO Wt_Mess WITH "��ぁ�爲�..."
DELETE FILE (tmpo_path+"CHECK_L.DBF")

CREATE DBF (tmpo_path+"CHECK_L.DBF")   ;
    ( C_DATE    D,     ;
      FIRM      C( 3), ;
      LIST_N    C( 6), ;
      INV_N     C(11), ;
      MON_N     C(11), ;
      SUM0      N(12,2), ;
      NEW_DOC   C(10), ;
      NEW_TYPE  C( 1), ;
      SALESMAN  C(29), ;
      CUSTOMER  C(50)   )
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG HRON

PRIVATE s000, arm_w, d01, d02, d03, c01, c02
arm_w = SUBSTR(arm_codes, arm_point*4-3, 3)

IF .NOT. SEEK(DTOS(dat0))
  s000 = RECNO(0)
  IF s000 > 0 .AND. s000 < RECCOUNT()
    GO s000
  ENDIF
ENDIF

s000 = 0

SCAN REST WHILE DOC_DATE <= dat1 FOR WHERE = arm_w .AND. DOC_TYPE = "�"
  * 蹍��▲�
  IF .NOT. SEEK(WHO, "PERSONS")
    c01 = IIF(WHO = 0, "���!", REPLICATE("*",29))
  ELSE
    c01 = ALLTRIM(PERSONS.FAMILY)+" "+  ;
          LEFT(PERSONS.NAME,1)+"."+     ;
          LEFT(PERSONS.S_NAME,1)+"."
  ENDIF
  IF .NOT. SEEK(FIRM+LEFT(DTOS(SALE_DATE),4)+SALE_NUM, "SALE_TIT")
    d01 = ""
    d02 = ""
    c02 = ""
  ELSE
    d01 = SALE_TIT.LIST_NUM
    d02 = LEFT(SALE_TIT.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(SALE_TIT.DOC_NUM,5))
    c02 = SALE_TIT.CUS_NAME
  ENDIF
  d03 = LEFT(DOC_NUM,1)+"-"+ALLTRIM(SUBSTR(DOC_NUM,2))
  s000 = s000+MONEY_R+N_OF_SALE
  SELECT CHECK_L
  APPEND BLANK
  REPLACE C_DATE   WITH INCMONEY.DOC_DATE, ;
          FIRM     WITH INCMONEY.FIRM,     ;
          LIST_N   WITH d01, ;
          INV_N    WITH d02, ;
          MON_N    WITH d03, ;
          SUM0     WITH INCMONEY.MONEY_R+INCMONEY.N_OF_SALE, ;
          SALESMAN WITH c01, ;
          CUSTOMER WITH c02
  SELECT INCMONEY
ENDSCAN

DO Wt_Mess

USE
SELECT PERSONS
USE
SELECT SALE_TIT
USE
SELECT CHECK_L
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �´�珮Ε�� �� �き��� 腑��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  DO V_Checks
ENDIF

CLOSE DATABASES
DELETE FILE (tmpo_path+"CHECK_L.DBF")

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                         蹍甃�矗 甎�瓷� 腑���.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 28.05.97 15:45:20 夕
PROCEDURE V_Checks

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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Docs OF (menu_name) PROMPT "�ョ�碎 ぎ�祠キ皰�"
ON PAD Docs OF (menu_name) ACTIVATE POPUP Docs

  DEFINE POPUP Docs MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Docs PROMPT "���. 腑�" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Docs DO Sw_Mode WITH "Invoice"

  DEFINE BAR  2 OF Docs PROMPT "�オ" KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF Docs DO Sw_Mode WITH "Money"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*

DO Use_Dummy
SELECT CHECK_L

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, tit
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = IIF(SET("CENTURY")="ON", 58, 56)
tit = ALLTRIM(arm_names(arm_point))+",  � "+DTOC(dat0)+" �� "+DTOC(dat1)

DO D_Wins WITH ln, wd, tit, 0, 2
@ 1, 2 SAY PADC("����",IIF(SET("CENTURY")="ON", 10, 8), "�")+"�"+ ;
       "���甌���爼������き.N陳��オ N陳陳陳陳陳陳�祠��"
@ WROWS()-3, WCOLS()-17 SAY STR(s000,15,2)
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F1 DO Sw_Mode WITH "Invoice"
    ON KEY LABEL F2 DO Sw_Mode WITH "Money"
    ON KEY LABEL F3 DO Sw_Mode WITH "Bill"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS C_DATE:H="", ;
                  LIST_N:H="", ;
                  FIRM:H="",   ;
                  INV_N:H="",  ;
                  MON_N:H="",  ;
                  SUM0:H="":P="@Z 999999999.99", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_C()       ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = IIF(what_do = "List", "Exit", what_do)

  CASE what_do = "Invoice"    && �����き��
  
    tit = LEFT(INV_N,4)+PADL(ALLTRIM(SUBSTR(INV_N,6)),6)
    DO Prn_Inv WITH FIRM, tit, C_DATE
    what_do = "List"
  
  CASE what_do = "Money"      && �オ
  
    tit = LEFT(MON_N,1)+PADL(ALLTRIM(SUBSTR(MON_N,3)),9)
    DO P_Money WITH FIRM, "�", tit, C_DATE
    what_do = "List"

  CASE what_do = "Bill"       && �腑�-���矣��

    DO P_Bill_F WITH FIRM, NEW_DOC, C_DATE, NEW_TYPE
    what_do = "List"
  
  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                        ��筮爼�罔����� �゛�痰�.                         �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 28.05.97 16:09:24 夕
PROCEDURE Show_C

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-3, 3 SAY "蹍��▲� "+ SALESMAN
@ WROWS()-2, 3 SAY CUSTOMER

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
