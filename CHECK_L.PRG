*������������������������������������������������������������������������ͻ
*� ���� Check_L      ���ࠡ��稪 ���३ ��ᨭ           28.05.97 14:15:25 �
*������������������������������������������������������������������������Ķ
*�                                                                        �
*�             �ணࠬ�� �ନ஢���� ᯨ᪠ �த�� � 祪���.             �
*�                                                                        �
*������������������������������������������������������������������������ͼ
PROCEDURE Check_L
PARAMETER all_arms

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    ����ঠ⥫쭠� ���� �ணࠬ��:
*

PRIVATE ex, arm_names, arm_codes, arm_point, dat0, dat1, n
*
*    ��ନ�㥬 ᯨ᮪ ࠡ��� ����.
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
*�����������������������������������������Ŀ
*�                  ������������������ķ   �
*�   ����祥 ����  � 1234567890123456 �   �
*�                  ������������������ͼ   �
*� �� ���ࢠ� ��� ���������������������Ŀ �
*� �    �  ��.��.����  ��  ��.��.����    � �
*� ��������������������������������������� �
*�    < OK Ctrl-W > < �⪠������ Esc >     �
*�������������������������������������������
ex   = 1

*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ��直� ������ ��砩!
DO Prp_Nav_2
DO D_Win_N WITH 10, 45, "���᮪ 祪�� �� ��ਮ�"
@  3, 5 SAY "����祥 ����"
DO Sun_Bord WITH 5, 3, 7, 41, " ���ࢠ� ��� "
@ 2, 20 GET arm_point PICTURE "@^" FROM arm_names SIZE 1, 20 WHEN all_arms
@ 6,  8 SAY "� "  GET dat0 PICTURE "@D"
@ 6, 23 SAY "�� " GET dat1 PICTURE "@D"
@ 8,  6 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �⪠������ Esc "

READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF ex = 2
  RETURN
ENDIF
*--------------------------------------------------------------------------
*   ��ନ�㥬 ᯨ᮪ ��� ��ᬮ��
*
DO Wt_Mess WITH "�����ઠ..."
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
  * �த����
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
  mss(2) = CHR(0)+"�� �����㦥�� �� ������ 祪�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  DO V_Checks
ENDIF

CLOSE DATABASES
DELETE FILE (tmpo_path+"CHECK_L.DBF")

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                         ��ᬮ�� ᯨ᪠ 祪��.                         �
*�                                                                        �
*����������������������������������������������������� 28.05.97 15:45:20 ͼ
PROCEDURE V_Checks

*
*  ��६���� ���ﭨ� ��� ������樨
*

PRIVATE stat_type     && ��� ������樨: 0 - ���⠭�����;
                                        1 - �����;
                                        2 - BROWSE - ⠡���;
                                        3 - BROWSE - ᯨ᮪.
PRIVATE what_do       && ��� ०���.
PRIVATE menu_name     && ��� �ᨭ�஭���� ����.
PRIVATE last_mouse    && �६� ��᫥����� ������ ���ਭ�� ������.
PRIVATE win_name      && ��� ���� ( ���� ��� BROWSE ).

*
*   ������塞 ���祭�ﬨ ��६���� ���ﭨ�...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ��।��塞 �ᨭ�஭��� ����
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Docs OF (menu_name) PROMPT "����� ���㬥�⮢"
ON PAD Docs OF (menu_name) ACTIVATE POPUP Docs

  DEFINE POPUP Docs MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Docs PROMPT "���. 祪" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Docs DO Sw_Mode WITH "Invoice"

  DEFINE BAR  2 OF Docs PROMPT "���" KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF Docs DO Sw_Mode WITH "Money"

DEFINE PAD Exit OF (menu_name) PROMPT "��室" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ���뢠�� 䠩�� �.�., ��⠭�������� �裡 � �. �.
*

DO Use_Dummy
SELECT CHECK_L

*
*   ����� ࠧ��஢ ����
*
PRIVATE ln, wd, tit
ln = MIN(100, RECCOUNT())   && ������⢮ ������� ��ப BROWSE
wd = IIF(SET("CENTURY")="ON", 58, 56)
tit = ALLTRIM(arm_names(arm_point))+",  � "+DTOC(dat0)+" �� "+DTOC(dat1)

DO D_Wins WITH ln, wd, tit, 0, 2
@ 1, 2 SAY PADC("���",IIF(SET("CENTURY")="ON", 10, 8), "�")+"�"+ ;
       "���᮪Ĕ�č������.N��ė�� N�����������đ㬬�"
@ WROWS()-3, WCOLS()-17 SAY STR(s000,15,2)
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && ��ᬮ�� ᯨ᪠

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F1 DO Sw_Mode WITH "Invoice"
    ON KEY LABEL F2 DO Sw_Mode WITH "Money"
    ON KEY LABEL F3 DO Sw_Mode WITH "Bill"

*
*    BROWSE - ����
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

  CASE what_do = "Invoice"    && ���������
  
    tit = LEFT(INV_N,4)+PADL(ALLTRIM(SUBSTR(INV_N,6)),6)
    DO Prn_Inv WITH FIRM, tit, C_DATE
    what_do = "List"
  
  CASE what_do = "Money"      && ���
  
    tit = LEFT(MON_N,1)+PADL(ALLTRIM(SUBSTR(MON_N,3)),9)
    DO P_Money WITH FIRM, "�", tit, C_DATE
    what_do = "List"

  CASE what_do = "Bill"       && ���-䠪���

    DO P_Bill_F WITH FIRM, NEW_DOC, C_DATE, NEW_TYPE
    what_do = "List"
  
  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
RELEASE MENU (menu_name) EXTENDED

RETURN

*������������������������������������������������������������������������ͻ
*�                                                                        �
*�                        ���ଠ樮���� �������.                         �
*�                                                                        �
*����������������������������������������������������� 28.05.97 16:09:24 ͼ
PROCEDURE Show_C

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-3, 3 SAY "�த���� "+ SALESMAN
@ WROWS()-2, 3 SAY CUSTOMER

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.