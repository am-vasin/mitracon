*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� SpcMove      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                   �※Ε�┘ �� 甎ユ筥����� �� �ム┏�.                   �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 03.01.2002 様�
PROCEDURE SpcMove
PARAMETERS prmCode

PRIVATE spcCode, letSpc, spcName, d1, d2
PRIVATE sum01, sum02, sum00
STORE 0 TO sum01, sum02, sum00
IF TYPE("prmCode") = "N"
  spcCode = prmCode
ELSE
  spcCode = 0
ENDIF
d1 = DATE()-6
d2 = DATE()

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE SHARED AGAIN ALIAS SPC_LIST
spcName = SPACE(50)

letSpc = .T.
IF .NOT. EMPTY(spcCode)
  IF SEEK(spcCode)
    spcName = SPC_LIST.CUS_NAME
    letSpc = .F.
  ELSE
    spcCode = 0
  ENDIF
ENDIF

IF .NOT. FilBlank()
  USE
  RETURN
ENDIF
SELECT SPC_LIST
USE

DO Wt_Mess WITH "��ぃ�皰→� ����諷"
DO PrepTmp
DO Wt_Mess

DO View_Res

USE
DELETE FILE (tmpo_path+"SPEC_TMP.DBF")
DELETE FILE (tmpo_path+"SPEC_TMP.CDX")
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� FilBlank     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            ������錺� ゛���                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.01.2002 陳�
PROCEDURE FilBlank
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 [ ] ��ユ筥���� 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕 �
*� �....:....!....:....!....:....!....:....!....:....!� �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�  �����讚�� ���� ��.��.����  ���ョ��� ���� ��.��.���� �
*�               < OK Ctrl-W > < �皖���碎瘴 Esc >       �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, tmpSw
tmpSw = .F.
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 58, "�※Ε�┘ �� 甎ユ筥����� �� �ム┏�"
DO Sun_Bord WITH  2,  3,  4, 54, "                "

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 2,  6 GET tmpSw PICTURE "@*C ��ユ筥����" WHEN letSpc VALID Get_Spec()
@ 3,  4 GET spcName WHEN .F.
@ 5,  3 SAY "�����讚�� ����" GET d1
@ 5, 30 SAY "���ョ��� ����" GET d2
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID TstBlank()

IF ex = 1
  *
  * �矗���硅��ガ ゛���
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_Scpec    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �襦�� 甎ユ筥�����                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.01.2002 陳�
PROCEDURE Get_Spec

IF .NOT. tmpSw
  RETURN .T.
ENDIF

tmpSw = .F.

DO Spec_Fil WITH spcCode, spcName

SHOW GET spcName
SHOW GET tmpSw

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� TstBlank     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      蹍▲爲� ��玻オ皚�痰� ゛����                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.01.2002 陳�
PROCEDURE TstBlank

PRIVATE mss

IF ex # 1
  RETURN .T.
ENDIF

IF EMPTY(spcCode)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 礫����� 甎ユ筥����"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� PrepTmp      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.01.2002 陳�
PROCEDURE PrepTmp

SELECT 0
DELETE FILE (tmpo_path+"SPEC_TMP.DBF")
DELETE FILE (tmpo_path+"SPEC_TMP.CDX")
CREATE DBF (tmpo_path+"SPEC_TMP") ;
  ( CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    DOC_DATE D,     ;
    FIRM     C( 3), ;
    DOC_NUMV C(15), ;
    DOC_NUM  C(10), ;
    DOC_TYPE C( 6), ;
    QNT      N( 6), ;
    QNT_SPEC N( 6), ;
    PRICE_R  N(10,2),;
    PRICE_D  N(10,4),;
    WHO      C(28)  )

SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (base_path+"SALE") SHARED AGAIN ORDER TAG DOC_NUM ALIAS DETAIL
SET RELATION TO CODE INTO STOCK
SELECT 0
USE (base_path+"SALE_TIT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS TITLE
SET RELATION TO WHO INTO PERSONS
SEEK spcCode
SCAN REST WHILE CUS_CODE = spcCode FOR BETWEEN(DOC_DATE, d1, d2)
  SELECT DETAIL
  SEEK TITLE.FIRM+LEFT(DTOS(TITLE.DOC_DATE),4)+TITLE.DOC_NUM
  SCAN REST WHILE FIRM = TITLE.FIRM .AND. DOC_DATE = TITLE.DOC_DATE .AND. DOC_NUM = TITLE.DOC_NUM FOR QNT # 0
    SELECT SPEC_TMP
    APPEND BLANK
    REPLACE CODE     WITH DETAIL.CODE,    ;
            PREFIX   WITH STOCK.PREFIX,   ;
            NAME     WITH STOCK.NAME,     ;
            PRODUCER WITH STOCK.PRODUCER, ;
            DOC_DATE WITH TITLE.DOC_DATE, ;
            FIRM     WITH TITLE.FIRM,     ;
            DOC_NUMV WITH LEFT(TITLE.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(TITLE.DOC_NUM,5))+"/"+TITLE.FIRM, ;
            DOC_NUM  WITH TITLE.DOC_NUM,  ;
            DOC_TYPE WITH "����.",        ;
            QNT      WITH DETAIL.QNT,     ;
            PRICE_R  WITH STOCK.PRICE_R,  ;
            PRICE_D  WITH STOCK.PRICE,    ;
            WHO      WITH ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)
            sum01 = sum01 + IIF(QNT>0,PRICE_D*QNT,0)
            sum02 = sum02 + IIF(QNT<0,PRICE_D*QNT,0)
    SELECT DETAIL
  ENDSCAN
  SELECT TITLE
ENDSCAN

SELECT DETAIL
USE (base_path+"ACT_S") SHARED AGAIN ORDER TAG DOC_NUM ALIAS DETAIL
SET RELATION TO CODE INTO STOCK
SELECT TITLE
USE (base_path+"ACT_S_T") SHARED AGAIN ORDER TAG SUP_CODE ALIAS TITLE
SET RELATION TO WHO_ACT INTO PERSONS
SEEK spcCode
SCAN REST WHILE SUP_CODE = spcCode FOR BETWEEN(DOC_DATE, d1, d2)
  SELECT DETAIL
  SEEK TITLE.FIRM+LEFT(DTOS(TITLE.DOC_DATE),4)+TITLE.DOC_NUM
  SCAN REST WHILE FIRM = TITLE.FIRM .AND. DOC_DATE = TITLE.DOC_DATE .AND. DOC_NUM = TITLE.DOC_NUM FOR QNT_INC # 0
    SELECT SPEC_TMP
    APPEND BLANK
    REPLACE CODE     WITH DETAIL.CODE,    ;
            PREFIX   WITH STOCK.PREFIX,   ;
            NAME     WITH STOCK.NAME,     ;
            PRODUCER WITH STOCK.PRODUCER, ;
            DOC_DATE WITH TITLE.DOC_DATE, ;
            FIRM     WITH TITLE.FIRM,     ;
            DOC_NUMV WITH ALLTRIM(TITLE.DOC_NUM)+"/"+TITLE.FIRM, ;
            DOC_NUM  WITH TITLE.DOC_NUM,  ;
            DOC_TYPE WITH "��.���.",      ;
            QNT      WITH -DETAIL.QNT_INC,;
            PRICE_R  WITH STOCK.PRICE_R,  ;
            PRICE_D  WITH STOCK.PRICE,    ;
            WHO      WITH ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)
            sum01 = sum01 + IIF(QNT>0,PRICE_D*QNT,0)
            sum02 = sum02 + IIF(QNT<0,PRICE_D*QNT,0)
    SELECT DETAIL
  ENDSCAN
  SELECT TITLE
ENDSCAN

sum00 = sum01 + sum02

SELECT 0
USE (base_path+"SPEC_SUM") SHARED AGAIN ORDER TAG POS_CUS

SELECT SPEC_TMP
SET RELATION TO STR(CODE,7)+STR(spcCode,8) INTO SPEC_SUM
REPLACE ALL QNT_SPEC WITH SPEC_SUM.QNT

SELECT SPEC_SUM
USE
SELECT DETAIL
USE
SELECT TITLE
USE
SELECT STOCK
USE
SELECT PERSONS
USE
SELECT SPEC_TMP
INDEX ON NAME+PREFIX+PRODUCER+DTOS(DOC_DATE) TAG NAME
GO TOP

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_Res     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          ��甃�矗 爛с�赳���.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 03.01.2002 陳�
PROCEDURE View_Res


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
PRIVATE s_sav         && ���ム ��｀腑� �゛�痰� か� 甌縲��キ�� � 〓瘁����←キ��!

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT SPEC_TMP

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Info OF (menu_name) PROMPT "��筮爼�罔�"
ON PAD Info OF (menu_name) ACTIVATE POPUP Info

  DEFINE POPUP Info MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Info PROMPT "�※Ε�┘" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Info DO Sw_Mode WITH "Move"

DEFINE PAD Print OF (menu_name) PROMPT "������碎" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*

*敖陳陳��Ж罔鐵陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����陳陳���-〓陳陳陳�キ�陳����腮タ
*�PPPP nnnnnnnnnNnnnnnnnnnNnnnnn ppppPppp ��.��.���� 999999 9999999.99  999999�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�           ���-�  � 999999999999999 �� ��.��.����                   �
*�         �≒��                                                      �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 75

DO D_Wins WITH ln, wd, ALLTRIM(spcName), 1, 2
@ 1, 3 SAY "�ム┏� � "+DTOC(d1)+" �� "+DTOC(d2)
@ 2, 2 SAY "陳陳���Ж罔鐵陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳����陳陳���-〓陳陳陳�キ������腮�"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1 DO Sw_Mode WITH "Move"
    ON KEY LABEL Ctrl+P DO Sw_Mode WITH "Print"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:H="",   ;
                  Brw_Name=Nm_Part(NAME):25:H="", ;
                  PRODUCER:H="", ;
                  DOC_DATE:H="", ;
                  QNT:H="",      ;
                  PRICE_D:H="",  ;
                  QNT_SPEC:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  CASE what_do = "Move"    && �※Ε�┘

    DO Move_S WITH CODE
    what_do = "List"

  CASE what_do = "Print"   && �ョ�碎

    DO Spec_File
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   ���窶�皚キ讓� (���窶�皚キ讓�!) ���琺��ガ DBF-�����.
*
SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Draw_Foot    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��痺ガ ぎ�����皀�讚竡 ┃筮爼�罔�.                    �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.10.1998 陳�
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�         ���-�  � 999999999999999 �� ��.��.����                   �
*�         �≒��                                                    �
*�踸絎� 99999999.9999   ��痳�� 99999999.9999   �皰�� 99999999.9999 �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

@ WROWS()-3,  2 SAY DOC_TYPE+" � "+DOC_NUMV+" �� "+DTOC(DOC_DATE)
@ WROWS()-3, 42 SAY "�≒�� "+WHO
@ WROWS()-2,  2 SAY "踸絎� "+STR(sum01,12,4)
@ WROWS()-2, 25 SAY "��痳�� "+STR(sum02,12,4)
@ WROWS()-2, 48 SAY "�皰�� "+STR(sum00,12,4)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                         ���� � DBF-筮爼�皀.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様 09.01.2002 夕
PROCEDURE Spec_file

PRIVATE new_f
new_f = New_File("","DBF", "�磴� ��茱� ぎ�祠キ�?", 0)

IF .NOT. EMPTY(new_f)
  DO Wt_Mess WITH "����瘡..."
  new_f = ALLTRIM(new_f)
  DO Wt_Mess
  SELECT SPEC_TMP
  COPY TO (new_f)
  
ENDIF
RETURN