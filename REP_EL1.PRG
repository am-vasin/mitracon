*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Rep_El1      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               蹍����襯 ��Ж罔� �� き鍖 か� �爛く爬閧��.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 04/05/1999 様�
PROCEDURE Rep_El1

PRIVATE f_c000, f_n000, b_sw000, f_c_old, d0, d1
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyz�、�ぅ颱Ж�������珀矣粤肄蓍裨跋鈿"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZ���������������������������������"
f_c000 = def_firm
f_c_old = def_firm
f_n000 = SPACE(16)
d1 = DATE()
d0 = d1-6

IF .NOT. F_Form()
  RETURN
ENDIF

DO Wt_Mess WITH "��ぃ�皰→� ����諷..."
DO Prep_Dat  && ��ぃ�皰→� ����諷
DO Wt_Mess

d0 = New_File("", "DBF", "���Θ皀 え爛�皰爬� � ━� �����", 0)
IF .NOT. EMPTY(d0)
  COPY FILE (tmpo_path+"REP_EL1.DBF") TO (d0)
ENDIF

DELETE FILE (tmpo_path+"REP_EL1.DBF")

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� F_Form       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ������キ┘ 蹣������ 筮爼�.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04/05/1999 陳�
PROCEDURE F_Form
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�  踳く爬閧┘ 999 (F1) 9999999999999999   �
*� 敖 �ム┏� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� � � ��.��.���� (F1) �� ��.��.���� (F1) � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�     < OK Ctrl-W > < �皖���碎瘴 Esc >     �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 46, "�������皀 ゛���"
DO Sun_Bord WITH  3,  3,  5, 42, " �ム┏� "
@ 4, 5 SAY "�            (F1) ��            (F1)"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
DO Get_Firm WITH 2, 4, enab_firm
@ 4,  7 GET d0 WHEN Set_F1(1) VALID Clr_F1()
@ 4, 26 GET d1 WHEN Set_F1(2) VALID Clr_F1() 
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()

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
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04/05/1999 陳�
PROCEDURE Set_F1
PARAMETERS mode

IF mode = 1
  ON KEY LABEL F1 DO Ch_Date WITH 1
ELSE
  ON KEY LABEL F1 DO Ch_Date WITH 2
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Clr_F1       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �腮痰�� �÷�｀砒┴� F1.                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04/05/1999 陳�
PROCEDURE Clr_F1

ON KEY LABEL F1

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Ch_Date      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�               ��皀���皋↓覃 �襦�� ��硅 �� ���キ���.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04/05/1999 陳�
PROCEDURE Ch_Date
PARAMETERS mode

IF mode = 1
  d0 = Get_Date(d0)
  SHOW GET d0
ELSE
  d1 = Get_Date(d1)
  SHOW GET d1
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �諷�� ├ ゛����.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04/05/1999 陳�
PROCEDURE Tst_Blank

PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF d0 > d1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�����讚�� ���� ┃皀燿��� ｀�跏� ���ョ���? �ぅ瘡 艪�-皰 �� ���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 礫����� �爛く爬閧┘..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Dat     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��ぃ�皰→� ����諷.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.04.1999 陳�
PROCEDURE Prep_Dat

DELETE FILE (tmpo_path+"REP_EL0.DBF")
DELETE FILE (tmpo_path+"REP_EL0.CDX")
DELETE FILE (tmpo_path+"REP_EL1.DBF")

CREATE DBF (tmpo_path+"REP_EL0") ;
  ( SALE_DATE D,     ;
    CODE      N( 7), ;
    PREFIX    C( 4), ;
    NAME      C(35), ;
    PRODUCER  C( 8), ;
    GTD_NUM   C(26), ;
    COUNTRY   C(16), ;
    GTD_LINE  C( 4), ;
    QNT       N( 6), ;
    SALE_SUM  N(12,2) )
INDEX ON DTOS(SALE_DATE)+STR(CODE)+GTD_NUM+GTD_LINE+COUNTRY TAG CODE

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM

SEEK f_c000
SCAN REST WHILE FIRM = f_c000
  IF .NOT. BETWEEN(DOC_DATE, d0, d1)
    LOOP
  ENDIF
  SELECT REP_EL0
  IF .NOT. SEEK(DTOS(SALE.DOC_DATE)+STR(SALE.CODE)+SALE.GTD_NUM++SALE.GTD_LINE+SALE.COUNTRY)
    APPEND BLANK
    REPLACE CODE      WITH SALE.CODE,     ;
            SALE_DATE WITH SALE.DOC_DATE, ;
            GTD_NUM   WITH SALE.GTD_NUM,  ;
            GTD_LINE  WITH SALE.GTD_LINE, ;
            COUNTRY   WITH SALE.COUNTRY
  ENDIF
  REPLACE QNT      WITH QNT+SALE.QNT, ;
          SALE_SUM WITH SALE_SUM+SALE.PRICE_0*SALE.QNT
  SELECT SALE
ENDSCAN

USE (base_path+"STOCK") ORDER TAG CODE
SELECT REP_EL0
SET RELATION TO CODE INTO STOCK
REPLACE ALL PREFIX   WITH STOCK.PREFIX, ;
            NAME     WITH STOCK.NAME,   ;
            PRODUCER WITH STOCK.PRODUCER
INDEX ON DTOS(SALE_DATE)+NAME+PREFIX+PRODUCER TAG NAME
COPY TO (tmpo_path+"REP_EL1")
USE
DELETE FILE (tmpo_path+"REP_EL0.DBF")
DELETE FILE (tmpo_path+"REP_EL0.CDX")
SELECT STOCK
USE
RETURN
