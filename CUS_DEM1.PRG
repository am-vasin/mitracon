*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� Cus_Dem1     ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                           ��牀� �� ��┘�矣.                            �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 25.12.2000 様�
PROCEDURE Cus_Dem1
PARAMETERS c_code, c_name

PRIVATE s_sav, tmpFile, mss, s_date
s_date = {}
tmpFile = SYS(3)
CREATE DBF (tmpo_path+tmpFile) ;
  ( CODE     N( 7), ;
    DEM_DATE D,     ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT      N( 6), ;
    QNT_STO  N( 6), ;
    PRICE    N(8,2),;
    WHO      C(28)  )
USE (tmpo_path+tmpFile) EXCLUSIVE ALIAS TMP_DEM

IF Prep_Dem()
  
  SELECT TMP_DEM
  IF RECCOUNT() = 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"�� 礫�����覃 �ム┏� 甎牀�� �� °��!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    DO View_Dem
  ENDIF
ENDIF

SELECT TMP_DEM
tmpFile = DBF()
USE
DELETE FILE (tmpFile)
DELETE FILE (LEFT(tmpFile, LEN(tmpFile)-3), "cdx")
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Dem     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ��爼�牀���┘ ����� 甎牀��.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 25.12.2000 陳�
PROCEDURE Prep_Dem

IF .NOT. S_Date()
  RETURN .F.
ENDIF

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE
SELECT 0
USE (base_path+"DEMAND") ORDER TAG CLI_CODE

SEEK c_code
SCAN REST WHILE CLI_CODE = c_code FOR DATE >= s_date
  IF .NOT. EMPTY(DEMAND.STO_CODE)
    = SEEK(DEMAND.STO_CODE, "STOCK")
    SELECT TMP_DEM
    APPEND BLANK
    REPLACE CODE     WITH DEMAND.STO_CODE, ;
            DEM_DATE WITH DEMAND.DATE,     ;
            PREFIX   WITH STOCK.PREFIX,    ;
            NAME     WITH STOCK.NAME,      ;
            PRODUCER WITH STOCK.PRODUCER,  ;
            QNT      WITH DEMAND.QTY,      ;
            PRICE    WITH STOCK.PRICE_R,   ;
            QNT_STO  WITH STOCK.QNT
  ELSE
    SELECT TMP_DEM
    APPEND BLANK
    REPLACE DEM_DATE WITH DEMAND.DATE,     ;
            PREFIX   WITH DEMAND.PREFIX,   ;
            NAME     WITH DEMAND.NAME,     ;
            PRODUCER WITH DEMAND.PRODUCER, ;
            QNT      WITH DEMAND.QTY
  ENDIF
  IF SEEK(DEMAND.WHO, "PERSONS")
    REPLACE WHO WITH ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)
  ENDIF
  SELECT DEMAND
ENDSCAN
USE
SELECT STOCK
USE
SELECT PERSONS
USE
SELECT TMP_DEM
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
GO TOP

RETURN .T.

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� S_Date       ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�                               �〓� ��硅.                               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 25.12.2000 様�
PROCEDURE S_Date
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

* �爛�キ��� ┃皀燿��
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�  �����讚�� ���� ��.��.���� (F1)  �
*�                                  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 6, 38, "�爛�キ��� ┃皀燿��"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*

ON KEY LABEL F1 DO Date_Help
@ 2,  4 SAY "�����讚�� ���� ��.��.���� (F1)"
@ 2, 19 GET s_date PICTURE "@D"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE
ON KEY LABEL F1
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Date_Help    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       �〓� ��硅 腑爛� ���キ��瑕.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 25.12.2000 陳�
PROCEDURE Date_Help

s_date = Get_Date(s_date)
SHOW GET s_date

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_Dem     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            蹍甃�矗 甎牀��.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 25.12.2000 陳�
PROCEDURE View_Dem
PRIVATE f_value, mss
f_value = "�瓮 ��Ж罔�"

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
SELECT TMP_DEM

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Filter OF (menu_name) PROMPT "�┼赳�"
ON PAD Filter OF (menu_name) ACTIVATE POPUP Filter

  DEFINE POPUP Filter MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Filter PROMPT "�瓮 ��Ж罔�" KEY Ctrl-A, ""
  ON SELECTION BAR 1 OF Filter DO Sw_Mode WITH "Filter_1"

  DEFINE BAR  2 OF Filter PROMPT "���讓� 瓷��め�┘" KEY Ctrl-A, ""
  ON SELECTION BAR 2 OF Filter DO Sw_Mode WITH "Filter_2"

  DEFINE BAR  3 OF Filter PROMPT "���讓� ━ヮ薑メ� � ����腮�" KEY Ctrl-A, ""
  ON SELECTION BAR 3 OF Filter DO Sw_Mode WITH "Filter_3"

DEFINE PAD Seek OF (menu_name) PROMPT "���瓷" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Report OF (menu_name) PROMPT "����→�" KEY Ctrl-A, "Enter"
ON SELECTION PAD Report OF (menu_name) DO Sw_Mode WITH "Report"

DEFINE PAD List OF (menu_name) PROMPT "���甌�" KEY Ctrl-A, "F2"
ON SELECTION PAD List OF (menu_name) DO Sw_Mode WITH "Order"

DEFINE PAD Print OF (menu_name) PROMPT "�ョ�碎" KEY Ctrl-A, "F3"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Delete OF (menu_name) PROMPT "�����碎" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 75
*��陳陳����陳陳陳陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳��牀當����-ツ陳�キ�陳�
*�X DD.DD.DDDD PPPP NNNNNNNNNnNNNNNNNNNnNNNNN PPPPPPPP 999999 999999 99999.99�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*� �皰 甌÷�� ���������������������������� � �                               �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

DO D_Wins WITH ln, wd, ALLTRIM(c_name)+;
               ": 甎牀� � "+DTOC(s_date), 1, 2
@ 2, 2 SAY ;
"�陳陳����陳陳陳陳���━キ����┘陳陳陳陳陳陳陳陳陳陳陳陳��牀當����-ツ陳�キ�"
@ WROWS()-2, 3 SAY "�皰 甌÷��"  
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 2 SAY "�┼赳�: "+PADR(f_value, 30)
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"
    ON KEY LABEL F2    DO Sw_Mode WITH "Order"
    ON KEY LABEL F3    DO Sw_Mode WITH "Print"
    ON KEY LABEL Del   DO Sw_Mode WITH "Delete"
*
*    BROWSE - �キ�
*
    BROWSE FIELDS IS_STO = IIF(EMPTY(CODE), "X", " "):1:H="", ;
                  DEM_DATE:H="", ;
                  PREFIX:H="",   ;
                  Brw_Name=Nm_Part(NAME):25:H="",  ;
                  PRODUCER:H="", ;
                  QNT:P="@Z 999999":H="",     ;
                  QNT_STO:P="@Z 999999":H="", ;
                  PRICE:P="@Z 99999.99":H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Report")
    ENDIF

  CASE what_do = "Report"    && ����→�

    IF .NOT. BOF() .AND. .NOT. EOF()
      IF CODE = 0
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"��Ж罔� �メ���め��� - あ└キ�� �モ!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
      ELSE
       DO Move_s WITH CODE
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Filter_1"  && �┼赳�: �瓮 ��Ж罔�

    SET FILTER TO
    f_value = "�瓮 ��Ж罔�"

    what_do = "List"

  CASE what_do = "Filter_2"  && �┼赳�: ���讓� 瓷��め�┘

    SET FILTER TO CODE # 0
    f_value = "���讓� 瓷��め�┘"
    what_do = "List"

  CASE what_do = "Filter_3"  && �┼赳�: ���讓� ━ヮ薑メ� � ����腮�

    SET FILTER TO CODE # 0 .AND. QNT_STO > 0
    f_value = "���讓� ━ヮ薑メ� � ����腮�"
    what_do = "List"

  CASE what_do = "Seek"      && ���瓷

    DO Seek_Pos WITH Get_Shab("", "���皀�痰�覃 ���瓷")
    what_do = "List"

  CASE what_do = "Order"     && ���甌�

    DO Cr_List
    what_do = "List"

  CASE what_do = "Print"     && �ョ�碎

    DO P_C_Dem
    what_do = "List"

  CASE what_do = "Delete"    && ����キ┘

    IF .NOT. EOF() .AND. .NOT. BOF()
      DELETE
    ENDIF
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
@ WROWS()-3, 14 SAY WHO
@ WROWS()-2, 3 SAY PREFIX+" "+NAME+" "+PRODUCER

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Seek_Pos     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ���皀�痰�覃 ���瓷 ��Ж罔�.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.12.2000 陳�
PROCEDURE Seek_Pos
PARAMETERS patt

IF EMPTY(patt)
  RETURN
ENDIF

DO Prp_List WITH patt
DO Choose_P

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prp_List     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      �癲�� ��Ж罔� �� ���皀�痰�.                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.12.2000 陳�
PROCEDURE Prp_List
PARAMETERS patt

PRIVATE s_sav, tmpName, r_sav

s_sav = SELECT()
SELECT 0
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ;
  ( LINK     N(10), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8)  )
USE (tmpo_path+tmpName) ALIAS FOUND

SELECT TMP_DEM
r_sav = RECNO()
SCAN
  IF patt $ PREFIX+NAME+PRODUCER
    SELECT FOUND
    APPEND BLANK
    REPLACE LINK     WITH RECNO("TMP_DEM"), ;
            PREFIX   WITH TMP_DEM.PREFIX,   ;
            NAME     WITH TMP_DEM.NAME,     ;
            PRODUCER WITH TMP_DEM.PRODUCER
    SELECT TMP_DEM
  ENDIF
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Choose_P     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�               �襦�� ��Ж罔� ├ �癲������� �� ���皀�痰�.                �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.12.2000 陳�
PROCEDURE Choose_P

PRIVATE tmpName, mss

SELECT FOUND
tmpName = DBF()
*
*    � °� �� ���趺┴?
IF RECCOUNT("FOUND") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �� ���ぅ��..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE(tmpName)        && 踸｀爲�...
  SELECT TMP_DEM
  RETURN
ENDIF
GO TOP
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
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "�襦��碎" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "�皖���碎瘴" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(WROWS("")-11, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 49
DO D_Wins WITH ln, wd, "�ェ祀赳�� ���瓷�", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:H="",   ;
                  NAME:H="",     ;
                  PRODUCER:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"	&& �Д� �襦���...

    GO FOUND.LINK IN TMP_DEM
    EXIT

  OTHERWISE					&& �Д� �皖����瘴

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
USE
DELETE FILE(tmpName)        && 踸｀爲�...
SELECT TMP_DEM

RETURN


*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Get_P        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �����モ琺 甎�瓷�.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.12.2000 陳�
PROCEDURE Get_P
PARAMETERS c_code, firm_c, excl_def, tmpUSD

PRIVATE f_point, s_sav, f_list, f_numb

f_point = 0
f_numb  = 0

*
*   ��皰※� 甎�甌� ぎ痰祚�諷 �爛く爬閧┤ � 竅����←│�ガ 礫���皀��
s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") ORDER TAG B_NAME AGAIN ALIAS F_0C26
SCAN
  IF Leg_Firm(c_code, FIRM_CODE)
    f_numb = f_numb+1
    DIMENSION f_list(f_numb)
    f_list(f_numb) = FIRM_CODE+" "+B_NAME
    IF FIRM_CODE = firm_c			&& �� ����� �爛く爬閧┘!!!
      f_point = f_numb
    ENDIF
  ENDIF
ENDSCAN

USE
SELECT (s_sav)
IF f_numb = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�イ�痰祚�� �� �き� �爛く爬閧┘!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  firm_c = "   "
  RETURN
ENDIF

f_point = MAX(f_point,1)

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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�       [ ] �瓷�鈑�碎 ぅ筥罔�        �
*�         [ ] ���甌� � USD           �
*�             敖陳陳陳陳陳陳陳陳陳朕 �
*� 踳く爬閧┘ �FFF NNNNNNNNNnNNNNNN� �
*�             青陳陳陳陳陳陳陳陳陳潰 �
*�  < OK Ctrl-W > < �皖���碎瘴 Esc >  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 9, 40, "��Г��┘ 甎�瓷�"
@ 5, 3 SAY "踳く爬閧┘"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 4, 15 GET f_point PICTURE "@^" FROM f_list SIZE 1, 22 WHEN f_numb > 1
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "
@ 2,  9 GET excl_def PICTURE "@*C �瓷�鈑�碎 ぅ筥罔�"
@ 3, 11 GET tmpUSD PICTURE "@*C ���甌� � USD"
READ CYCLE

IF ex # 1  && �Д� �皖����瘴...
  firm_c = "   "
ELSE
  firm_c = LEFT(f_list(f_point),3)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Cr_List      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            ��Г��┘ 甎�瓷�.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 26.12.2000 陳�
PROCEDURE Cr_List

PRIVATE del_def, f_c, s_sav, tmpNum, sum_0
PRIVATE tmpName, newRec, tmpQnt, mss, r_sav, USD_list

USD_list = .T.
del_def = .T.
f_c     = def_firm
DO Get_P WITH c_code, f_c, del_def, USD_list
IF EMPTY(f_c)
  RETURN
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE AGAIN
SELECT 0
USE (base_path+"LIST_DET")
tmpName = SYS(3)
COPY STRUCTURE TO (tmpo_path+tmpName)
SELECT 0
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_LIST
INDEX ON CODE TAG CODE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME ADDITIVE

SELECT TMP_DEM
r_sav = RECNO()
tmpNum = 0

SCAN FOR QNT > 0

  SELECT TMP_LIST 
  IF TMP_DEM.CODE = 0
    SET ORDER TO TAG NAME
    newRec = .NOT. SEEK(TMP_DEM.NAME+TMP_DEM.PREFIX+TMP_DEM.PRODUCER)
  ELSE
    = SEEK(TMP_DEM.CODE, "STOCK")
    SET ORDER TO TAG CODE
    newRec = .NOT. SEEK(TMP_DEM.CODE)
  ENDIF

  IF newRec
    APPEND BLANK
    REPLACE CODE      WITH TMP_DEM.CODE, ;
            PREFIX    WITH IIF(TMP_DEM.CODE=0, TMP_DEM.PREFIX, STOCK.PREFIX),     ;
            NAME      WITH IIF(TMP_DEM.CODE=0, TMP_DEM.NAME, STOCK.NAME),         ;
            PRODUCER  WITH IIF(TMP_DEM.CODE=0, TMP_DEM.PRODUCER, STOCK.PRODUCER), ;
            ADDRESS   WITH IIF(TMP_DEM.CODE=0, "", STOCK.ADDRESS),                ;
            USD_DEF   WITH IIF(TMP_DEM.CODE=0, 0, STOCK.PRICE),                   ;
            DEF_PRICE WITH IIF(TMP_DEM.CODE=0, 0, STOCK.PRICE_R),                 ;
            SAL_PRICE WITH IIF(TMP_DEM.CODE=0.OR.USD_list, 0, STOCK.PRICE_R),     ;
            USD_PRICE WITH IIF(TMP_DEM.CODE=0.OR..NOT.USD_list, 0, STOCK.PRICE)
  ENDIF
  REPLACE QNT WITH QNT+TMP_DEM.QNT
  SELECT TMP_DEM

ENDSCAN

SELECT TMP_LIST
SET RELATION TO CODE INTO STOCK

sum_0 = 0
SCAN
  tmpQnt = QNT
  IF del_def
    IF CODE = 0
      tmpQnt = 0
    ELSE
      tmpQnt = MIN(QNT, STOCK.QNT)
    ENDIF
  ENDIF

  IF CODE # 0
    tmpQnt = CEILING(tmpQnt/MAX(STOCK.QNT_PACK,1))*MAX(STOCK.QNT_PACK,1)
  ENDIF
  tmpQnt = MAX(tmpQnt, 0)
  IF tmpQnt <= 0
    LOOP
  ENDIF
  IF tmpNum = 0
    SELECT 0
    USE (base_path+"LIST_TIT")
    tmpNum = Doc_Num("LISTS", f_c, arm, DATE())

    APPEND BLANK
    REPLACE LST_ARM   WITH arm,     ;
            LST_NUM   WITH tmpNum,  ;
            CR_DAT    WITH DATE(),  ;
            IS_USD    WITH USD_list,;
            LST_NAME  WITH LEFT(c_name, FSIZE("LST_NAME")-4)+"��牀�", ;
            CLI_CODE  WITH c_code,  ;
            FIRM      WITH f_c,     ;
            ORD_QUEUE WITH queuetype, ;
            WHO_CR    WITH user,    ;
            WHERE_CR  WITH arm
  ENDIF
  SELECT LIST_DET
  APPEND BLANK
  REPLACE LIST_NUM  WITH STR(tmpNum,6),		;
          CODE      WITH TMP_LIST.CODE,		;
          PREFIX    WITH TMP_LIST.PREFIX,	;
          NAME      WITH TMP_LIST.NAME,		;
          PRODUCER  WITH TMP_LIST.PRODUCER,	;
          ADDRESS   WITH TMP_LIST.ADDRESS,	;
          QNT       WITH tmpQnt,			;
          USD_DEF   WITH TMP_LIST.USD_DEF,	;
          DEF_PRICE WITH TMP_LIST.DEF_PRICE,;
          USD_PRICE WITH TMP_LIST.USD_PRICE,;
          SAL_PRICE WITH TMP_LIST.SAL_PRICE
  IF USD_list
    sum_0 = sum_0+USD_PRICE*QNT
  ELSE
    sum_0 = sum_0+ROUND(SAL_PRICE*QNT,2)
  ENDIF
  SELECT TMP_LIST
ENDSCAN
tmpName = DBF()
USE
DELETE FILE (tmpName)
DELETE FILE (LEFT(tmpName, LEN(tmpName)-3)+"cdx")
IF USED("LIST_TIT")
  SELECT LIST_TIT
  REPLACE R_TOTAL WITH sum_0
  USE
ENDIF
SELECT LIST_DET
USE
SELECT STOCK
USE
SELECT TMP_DEM
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

SELECT (s_sav)
IF EMPTY(tmpNum)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"��腑�� �モ �� 瓷��ぅ, 甎�甌� �� 痲�爼�牀���!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�筮爼�牀��� 甎�甌� � "+ALLTRIM(STR(tmpNum))
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
ENDIF
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� P_C_Dem        ��о�｀砒┴ ��むォ ��瓱�                    �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �ョ�碎 ぎ�祠キ��.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 12/27/2000 陳�
PROCEDURE P_C_Dem

PRIVATE p_ln, fnt, n_cp, lft, ffeed, sb, n, t_d, t_p, f_d, f_p, p_drctry
PRIVATE s_sav, r_sav

DIMENSION sb(4,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Customer}"
sb( 2,2) = c_name

sb( 3,1) = "{Pg_Sum  }"
sb( 3,2) = ""

sb( 4,1) = "{Tot_Sum }"
sb( 4,2) = ""

n = ALEN(sb,1)
FOR i = 1 TO n
  sb(i,2) =PADR(sb(i,2), LEN(sb(i,1)))
ENDFOR
s_sav = SELECT()
SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "CUS_DEM" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR


n = MEMLINES(DOC_H)
DIMENSION t_d(n)
FOR i = 1 TO n
  t_d(i) = MLINE(DOC_H,i)
ENDFOR

n = MEMLINES(PAGE_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR

n = MEMLINES(PAGE_F)
DIMENSION f_p(n)
FOR i = 1 TO n
  f_p(i) = MLINE(PAGE_F,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR
USE

SELECT TMP_DEM
r_sav = RECNO()
DELETE FILE (tmpo_path+"P_C_DEM.DBF")
COPY TO (tmpo_path+"P_C_DEM.DBF")
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
SELECT 0
USE (tmpo_path+"P_C_DEM.DBF")
nn = RECCOUNT()
DO Ini_Prn WITH "���甌� 甎牀�� か� ��┘���", p_ln, lft, n_cp, fnt, ffeed, p_drctry

PRIVATE n_pg, n_p_det, nn, sum_d, sum_p, f_pg, str_w
n_p_det  = p_ln-MAX(ALEN(t_d),ALEN(t_p))-MAX(ALEN(f_p),ALEN(f_d))
n_pg = 1

IF n_p_det <= 0  && ��；�� ≡� 痰����聶 ��� ������→�... �� ����� 痰�����!
  n_p_det = nn
ELSE
  nn = MAX(nn-n_p_det, 0)
  n_pg = 1+CEILING(nn/n_p_det)
ENDIF

nn = 0

STORE 0 TO sum_d, sum_p
FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1   && ������〓� �� �ム〓� 痰����肓
    EJECT
    FOR i = 1 TO ALEN(t_p)
      str_w = t_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ELSE        && ������〓� �ム〓� 痰����肓
    FOR i = 1 TO ALEN(t_d)
      str_w = t_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ENDIF
  STORE 0 TO sum_p
*
*   �モ��讚�� ��痰� 痰����肓
* 
  FOR i = 1 TO n_p_det
    nn = nn+1
    @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+STR(QNT,7)+STR(QNT_STO,7)+  ;
                     STR(PRICE,9,2)+STR(QNT*PRICE,11,2)
    sum_p = sum_p+QNT*PRICE
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
*
*    ｪ�����┘ 痰����肓
*
  sum_d = sum_d+sum_p

  sb( 3,2) = PADR(STR(sum_p,10,2), LEN(sb(3,1)))
  IF f_pg = n_pg
    sb(4,2) =PADR(STR(sum_d,10,2), LEN(sb(4,1)))
    FOR i = 1 TO ALEN(f_d)
      str_w = f_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR

USE
DO Term_Prn WITH "", tmpo_path+"P_C_DEM.DBF", c_code
DELETE FILE (tmpo_path+"P_C_DEM.DBF")

SELECT (s_sav)

RETURN
