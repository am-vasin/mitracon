*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� NotSaled     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                 ���甌� ��Ж罔� �� �牀����∵�縺� N きォ                 �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 20.09.2000 様�
PROCEDURE NotSaled

PRIVATE n_days, a_days, incBuy

n_days = 30
a_days = 10
incBuy = .T.

IF .NOT. GetDays()
  RETURN
ENDIF

DO Wt_Mess WITH "��ぁ�爲� ����諷"
DO PrepData
DO Wt_Mess

DO ViewData

PRIVATE tmpName
tmpName = ""
tmpName = New_File(tmpName, "DBF", "�ぅ 甌縲���碎 爛с�赳��?")
IF .NOT. EMPTY(tmpName)
  COPY TO (tmpName)
ENDIF

SELECT NOTSALED
USE
DELETE FILE (tmpo_path+"NOTSALED.DBF")
DELETE FILE (tmpo_path+"NOTSALED.CDX")

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� GetDays      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                     ��皀���皋↓覃 □�� ┃皀燿���.                      �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.09.2000 陳�
PROCEDURE GetDays
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

* ���Θ皀 ����腑痰〓 きォ
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                  �
*�           ��皀燿�� 999           �
*�                                  �
*� �牀�� �爬絎�� �� ��甄イ�┘ 999   �
*�                                  �
*�  [ ] ���鈑�� �祚�キ�襯 ��Ж罔�   �
*�                                  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE  ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 11, 38, "���Θ皀 ����腑痰〓 きォ"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3, 13 SAY "��皀燿��" GET n_days PICTURE "999"
@ 5,  3 SAY "�牀�� �爬絎�� �� ��甄イ�┘ " GET a_days PICTURE "999"
@ 7,  4 GET incBuy PICTURE "@*C ���鈑�� �祚�キ�襯 ��Ж罔�"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� PrepData     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           ��ぃ�皰→� ����諷.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.09.2000 陳�
PROCEDURE PrepData

PRIVATE tmpRec, qnt000

SELECT 0
DELETE FILE (tmpo_path+"TMP000.DBF")
DELETE FILE (tmpo_path+"TMP000.CDX")
CREATE DBF (tmpo_path+"TMP000")  ;
  ( CODE     N( 7) )
INDEX ON CODE TAG CODE

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM
IF .NOT. incBuy
  USE (base_path+"ACT_S_T") ORDER TAG DOC_NUM IN 0
  SET RELATION TO ACT_FIRM+LEFT(DTOS(ACT_DATE), 4)+ACT_NUM INTO ACT_S_T
  SET FILTER TO ACT_S_T.FLG_BUY = "R"
ENDIF
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON
SEEK DTOS(DATE()-n_days)
IF .NOT. FOUND()
  tmpRec = RECNO(0)
  IF BETWEEN(tmpRec,1,RECCOUNT())
    GO tmpRec
  ENDIF
ENDIF
SCAN REST FOR CUS_CODE >= 0
  WAIT WINDOW DTOC(DOC_DATE) NOWAIT
  SELECT SALE
  SEEK SALE_TIT.FIRM+LEFT(DTOS(SALE_TIT.DOC_DATE),4)+SALE_TIT.DOC_NUM
  SCAN REST WHILE SALE_TIT.FIRM=FIRM .AND. SALE_TIT.DOC_DATE=DOC_DATE .AND. SALE_TIT.DOC_NUM = DOC_NUM
    SELECT TMP000
    IF .NOT. SEEK(SALE.CODE)
      APPEND BLANK
      REPLACE CODE WITH SALE.CODE
    ENDIF
    SELECT SALE
  ENDSCAN
  SELECT SALE_TIT
ENDSCAN

USE

IF USED("ACT_S_T")
  USE IN ACT_S_T
ENDIF
SELECT SALE
USE

USE (base_path+"ACT_S") ORDER TAG CODE
IF .NOT. incBuy
  SET FILTER TO FLG_BUY = "R"
ENDIF

SELECT 0
USE (base_path+"DIVISION") ORDER TAG CODE

SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME_P
SET RELATION TO CODE INTO TMP000, CODE INTO ACT_S, DIV_CODE INTO DIVISION

SELECT 0
DELETE FILE (tmpo_path+"NOTSALED.DBF")
DELETE FILE (tmpo_path+"NOTSALED.CDX")
CREATE DBF (tmpo_path+"NOTSALED")  ;
  ( CODE     N(  7), ;
    PREFIX   C(  4), ;
    NAME     C( 35), ;
    PRODUCER C(  8), ;
    QNT      N(  6), ;
    DIV      C( 16), ;
    ADDR     C(  8), ;
    PRICE_D  N(8,2)  )
SET RELATION TO CODE INTO ACT_S    

SELECT STOCK
nnn = 0
SCAN FOR .NOT. FOUND("TMP000")
  nnn = nnn+1
  IF nnn%1000 = 0
    WAIT WIND STR(nnn) NOWAIT
  ENDIF
  IF incBuy
    IF QNT+QNT_REAL+QNT_SAVE <= 0
      LOOP
    ENDIF
  ELSE
    IF QNT_REP <= 0
      LOOP
    ENDIF
  ENDIF
  SELECT ACT_S
  qnt000 = 0
  SCAN REST WHILE CODE = STOCK.CODE FOR QNT_REST # 0
    IF DOC_DATE <= DATE() - a_days
      qnt000 = qnt000 + QNT_INC
    ENDIF
  ENDSCAN
  IF qnt000 = 0
*  IF STOCK.QNT+STOCK.QNT_REAL+STOCK.QNT_SAVE+STOCK.QNT_REZ # qnt000
    SELECT NOTSALED
    APPEND BLANK
    REPLACE CODE     WITH STOCK.CODE,      ;
            PREFIX   WITH STOCK.PREFIX,    ;
            NAME     WITH STOCK.NAME,      ;
            PRODUCER WITH STOCK.PRODUCER,  ;
            DIV      WITH DIVISION.B_NAME, ;
            ADDR     WITH STOCK.ADDRESS,   ;
            PRICE_D  WITH STOCK.PRICE
  IF .NOT. incBuy
    REPLACE QNT WITH STOCK.QNT_REP
  ELSE
    REPLACE QNT WITH STOCK.QNT+STOCK.QNT_SAVE+STOCK.QNT_REZ
  ENDIF

  ENDIF
  SELECT STOCK
ENDSCAN

SELECT ACT_S
USE
SELECT DIVISION
USE
SELECT STOCK
USE

SELECT TMP000
USE
DELETE FILE (tmpo_path+"TMP000.DBF")
DELETE FILE (tmpo_path+"TMP000.CDX")

SELECT NOTSALED
SET ORDER TO
INDEX ON NAME TAG NAME

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� ViewData     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            蹍甃�矗 ����諷                             �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 20.09.2000 陳�
PROCEDURE ViewData

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
SELECT NOTSALED
SET ORDER TO

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Info OF (menu_name) PROMPT "��筮爼�罔�" KEY Ctrl-A, "F1"
ON SELECTION PAD Info OF (menu_name) DO Sw_Mode WITH "Info"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd, r_code
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 71

*DO D_Wins WITH ln, wd, '"�����痰"', 2, 2
DO D_Wins WITH ln, wd, "��Ж罔�, �爬�閧襯 ぎ "+DTOC(DATE()-a_days);
     +" � �� �牀����∵┘瘴 � 皀腑�┘ "+ALLTRIM(STR(n_days))+" きォ" , 0, 2 
*@ 1, 11 SAY "��Ж罔�, �爬�閧襯 ぎ "+DTOC(DATE()-a_days)
*@ 2,  9 SAY "� �� �牀����∵┘瘴 � 皀腑�┘ "+ALLTRIM(STR(n_days))+" きォ"
@ 1,  2 SAY "陳陳��Ж罔鐵陳陳陳陳陳陳陳陳陳陳陳陳陳陳���-〓陳陳�キ�陳陳陳�祠��"
@ WROWS()-2,  2 SAY "��Гカ"
@ WROWS()-2, 38 SAY "�むメ"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    SELECT NOTSALED
    SET ORDER TO TAG NAME 
    DO Prp_Nav_1
    ON KEY LABEL F1 DO Sw_Mode WITH "Info"
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:H="", ;
           Brw_Name=Nm_Part(NAME):25:H="", ;
           PRODUCER:H="", ;
           QNT:H="":P="999999", ;
           PRICE_D:H="":P=99999.99,;
           s000=PRICE_D*QNT:P=999999.99,; 
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           WHEN Draw_F()   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Info")
    ENDIF

  CASE what_do = "Info"
    r_code = CODE
    DO Move_S WITH r_code
    what_do = "List"

  CASE what_do = "Info"
    r_code = CODE
    DO Move_S WITH r_code
    what_do = "List"

  CASE what_do = "List"

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
PROCEDURE Draw_F

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   ��� Гメ�, 爬痺ガ...
*
@ WROWS()-3,  3 SAY PREFIX COLOR SCHEME 1
@ WROWS()-3, COL()+1 SAY NAME COLOR SCHEME 1
@ WROWS()-3, COL()+1 SAY PRODUCER COLOR SCHEME 1
@ WROWS()-2,  9 SAY DIV COLOR (SCHEME(14,2))
@ WROWS()-2, 45 SAY ADDR COLOR (SCHEME(14,2))

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
