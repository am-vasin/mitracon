*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                        �腮痰�� 甎�瓷� ��┘�皰�.                       �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE CusClear
PRIVATE c_type, mss, pl, sw

pl = Choose_P()

DO Use_Dummy

SELECT 0
USE (base_path+"CUSTOMER") ORDER TAG CODE

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE

SELECT 0
IF EMPTY(pl)
  USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
ELSE
  USE (base_path+"ACCOUNT") ORDER TAG PL_NAME
ENDIF
SEEK pl

SELECT 0
DELETE FILE (tmpo_path+"CUS_TMP.DBF")
DELETE FILE (tmpo_path+"CUS_TMP.CDX")
CREATE DBF (tmpo_path+"CUS_TMP")  ;
    (   CUS_CODE  N( 8),  ;
        CUS_NAME  C(50) )

INDEX ON CUS_NAME TAG NAME
SET ORDER TO TAG NAME

SELECT 0
DELETE FILE (tmpo_path+"DOCS_TMP.DBF")
DELETE FILE (tmpo_path+"DOCS_TMP.CDX")
CREATE DBF (tmpo_path+"DOCS_TMP")  ;
    (   CUS_CODE  N( 8),  ;
        FIRM      C( 3),  ;
        DOC_NUM   C(10),  ;
        DOC_DATE  D    ,  ;
        DOC_TIME  N( 5),  ;
        DOC_TYPE  C( 1),  ;
        DOC_SUM   N(15,2), ;
        DOC_SUMD  N(15,4) )

INDEX ON STR(CUS_CODE,8)+DTOS(DOC_DATE)+STR(DOC_TIME,5) TAG CUS_CODE
SET ORDER TO TAG CUS_CODE

SELECT ACCOUNT

DO Wt_Mess WITH "��ぃ�皰→� 甎�瓷� ��┘�皰�..."

*
*  ������錺� ∇ガキ�襯 �����
*
SCAN REST WHILE SUBSYS = pl
  IF SEEK(STR(CUS_CODE), "CUS_REST")
    SELECT CUS_REST
    sw = .F.
    SCAN REST WHILE CUS_CODE = ACCOUNT.CUS_CODE
      IF ACC_SUM # 0 .OR. ACC_S_H # 0 .OR. ACC_SUMD # 0 .OR. ACC_SD_H # 0
        sw = .T.
        EXIT
      ENDIF
    ENDSCAN
    IF sw
      SELECT ACCOUNT
      LOOP
    ENDIF
  ENDIF
  SELECT CUS_TMP
  APPEND BLANK
  REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE,  ;
          CUS_NAME WITH ACCOUNT.CUS_NAME

  IF SEEK(CUS_CODE,"INCMONEY")
    SELECT INCMONEY
    SCAN REST WHILE CUS_CODE = CUS_TMP.CUS_CODE
      SELECT DOCS_TMP
      APPEND BLANK
      REPLACE CUS_CODE WITH INCMONEY.CUS_CODE,  ;
              FIRM     WITH INCMONEY.FIRM,      ;
              DOC_NUM  WITH INCMONEY.DOC_NUM,   ;
              DOC_DATE WITH INCMONEY.DOC_DATE,  ;
              DOC_TIME WITH INCMONEY.DOC_TIME,  ;
              DOC_TYPE WITH INCMONEY.DOC_TYPE,  ;
              DOC_SUM  WITH INCMONEY.MONEY_R,   ;
              DOC_SUMD WITH INCMONEY.MONEY_D
      SELECT INCMONEY
    ENDSCAN
  ENDIF
  IF SEEK(CUS_TMP.CUS_CODE,"SALE_TIT")
    SELECT SALE_TIT
    SCAN REST WHILE CUS_CODE = CUS_TMP.CUS_CODE
      SELECT DOCS_TMP
      APPEND BLANK
      REPLACE CUS_CODE WITH SALE_TIT.CUS_CODE,  ;
              FIRM     WITH SALE_TIT.FIRM,      ;
              DOC_NUM  WITH SALE_TIT.DOC_NUM,   ;
              DOC_DATE WITH SALE_TIT.DOC_DATE,  ;
              DOC_TIME WITH SALE_TIT.DOC_TIME,  ;
              DOC_TYPE WITH "*",                ;
              DOC_SUM  WITH -SALE_TIT.TOTAL_R,  ;
              DOC_SUMD WITH -SALE_TIT.TOTAL_D
      SELECT SALE_TIT
    ENDSCAN
  ENDIF
  SELECT ACCOUNT

ENDSCAN

DO Wt_Mess

IF RECCOUNT("CUS_TMP") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ �� �き��� ��┘��� � �竅硅� ぅ��Ж皰�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"CUS_TMP.DBF")
  DELETE FILE (tmpo_path+"CUS_TMP.CDX")
  DELETE FILE (tmpo_path+"DOCS_TMP.DBF")
  DELETE FILE (tmpo_path+"DOCS_TMP.CDX")
  RETURN
ENDIF
  
*
*  �ムガキ�襯 甌痰�鎰�� か� ��※�∽┬
*

PRIVATE stat_type   && �┓ ��※��罔�: 0 - �メ�����珥�覃;
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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Docs OF (menu_name) PROMPT "���祠キ硅" KEY Ctrl-A, "Enter"
ON SELECTION PAD Docs OF (menu_name) Do Sw_Mode WITH "docs"

DEFINE PAD Del OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) Do Sw_Mode WITH "del"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ��瘍モ ��Кム�� ����
*
SELECT ACCOUNT
SET ORDER TO TAG CUS_CODE
SELECT CUS_TMP
GO TOP
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = FSIZE("CUS_NAME")

DO D_Wins WITH ln, wd, "��┘�硅 � �竅硅� ぅ��Ж皰�", 0, 0
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�
    
    SELECT CUS_TMP
    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Del   DO Sw_Mode WITH "del"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS CUS_NAME,  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "docs")
    ENDIF

  CASE what_do = "docs"    && 蹍甃�矗 ぎ�祠キ皰�

    DO View_Doc
    what_do = "list"

  CASE what_do = "del"     && ����キ┘ ��┘���

    SELECT ACCOUNT
    SEEK CUS_TMP.CUS_CODE
    REPLACE DATE_OFF WITH DATE()
    SELECT CUSTOMER
    IF SEEK(ACCOUNT.CUS_CODE)
      REPLACE DATE_OFF WITH DATE(), WHO_DEL WITH USER
    ENDIF
    SELECT CUS_TMP
    DELETE
    SKIP
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED
DELETE FILE (tmpo_path+"CUS_TMP.DBF")
DELETE FILE (tmpo_path+"CUS_TMP.CDX")
DELETE FILE (tmpo_path+"DOCS_TMP.DBF")
DELETE FILE (tmpo_path+"DOCS_TMP.CDX")

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�              蹍甃�矗 ぎ�祠キ皰� か� ��┘���.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE View_Doc
PRIVATE tit

SELECT DOCS_TMP
IF .NOT. SEEK(STR(CUS_TMP.CUS_CODE,8))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��� ��┘��� �モ �� �き��� ぎ�祠キ��!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT CUS_TMP
  RETURN
ENDIF
tit = ALLTRIM(CUS_TMP.CUS_NAME)

*
*  �ムガキ�襯 甌痰�鎰�� か� ��※�∽┬
*

PRIVATE stat_type   && �┓ ��※��罔�: 0 - �メ�����珥�覃;
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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Prnt OF (menu_name) PROMPT "�ョ�碎 ぎ�-��" KEY Ctrl-A, "Enter"
ON SELECTION PAD Prnt OF (menu_name) Do Sw_Mode WITH "Prnt"

DEFINE PAD Exit OF (menu_name) PROMPT "��Б���" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
*ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
ln = WROWS("")-7
wd = 60

DO D_Wins WITH ln, wd, tit, 0, 0

*��爼陳�����陳陳�N ぎ�-��陳陳陳�┓ ぎ�-��陳陳陳陳��祠��陳陳陳朕
*���� ��.��.���� NNNNNNNNNNN AAAAAAAAAAAAAAA 9 999 999 999.99$�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

@ 1, 2 SAY "�爼陳�����陳陳�N ぎ�-��陳陳陳�┓ ぎ�-��陳陳陳陳��祠��陳陳陳�"
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS FIRM:H="",         ;
                  DOC_DATE:H="",     ;
                  F000 = IIF(DOC_TYPE = "*",  ;
                     LEFT(DOC_NUM,4)+"-"+ALLTRIM(RIGHT(DOC_NUM,5)),  ;
                     LEFT(DOC_NUM,1)+"-"+ALLTRIM(RIGHT(DOC_NUM,2))):H="":11, ;
                  F001 = D_Type():H="":15,   ;
                  F002 = D_Sum():H="":17,    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           KEY STR(CUS_TMP.CUS_CODE,8) ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "prnt")
    ENDIF

  CASE what_do = "prnt"    && 蹍甃�矗 甎�瓷�

    DO P_D
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_I")
RELEASE MENU (menu_name) EXTENDED

RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                   ��瘉�籥�→� 皋�� ぎ�祠キ��.                         �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE D_Type

DO CASE
CASE DOC_TYPE = " "
  RETURN "踸�/��痳 �爐ム"
CASE DOC_TYPE = "1"
  RETURN "���皀Κ�     "
CASE DOC_TYPE = "3"
  RETURN "��ぅ��.      "
CASE DOC_TYPE = "*"
  RETURN "�����き��    "
ENDCASE
RETURN "             "

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                       �祠�� �� ぎ�祠キ矣.                             �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE D_Sum

DO CASE
CASE DOC_TYPE = "3"
  RETURN TRANSFORM(DOC_SUMD, "@Z 999 999 999.9999")+"$"
OTHERWISE
  RETURN TRANSFORM(DOC_SUM,  "@Z 9 999 999 999.99")+"�"
ENDCASE

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
*�                                                                       �
*�                     蹍������ �ョ�皋 ぎ�祠キ皰�.                      �
*�                                                                       �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様�
PROCEDURE P_D

DEFINE WINDOW H_W_876 FROM 0,0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW H_W_876

DO CASE
CASE INLIST(DOC_TYPE, " ", "1", "2", "3")  && ���皀�
  DO P_Money WITH FIRM, DOC_TYPE, DOC_NUM, DOC_DATE
CASE DOC_TYPE = "*"                  && �����き��
  DO Prn_Inv WITH FIRM, DOC_NUM, DOC_DATE
ENDCASE
RELEASE WINDOW H_W_876

RETURN
