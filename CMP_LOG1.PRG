*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*� ���� Cmp_Log      ��о�｀砒┴ ��むォ ��瓱�           17.03.98 12:50:12 �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�               ���↓キ┘ �璢│� か� �� � ����覓� �牀��Θ.               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様夕
PROCEDURE Cmp_Log1

PRIVATE f_name, n, f, d, dt, s, dt_w, dt_e, n0

dt_w = DATE()
dt_e = DATE()

IF TYPE("vk_arch") # "C"
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�ウ━ 瓠��� ┃筮爼�罔� �� 竅����←キ."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

*n = FOPEN(base_path+"FOR_VK.TXT")
*f_name = FGETS(n, 120)
*f_name = ALLTRIM(f_name)
*= FCLOSE(n)

f_name = vk_arch
IF .NOT. "." $ f_name
  f_name = f_name+".dbf"
ENDIF

IF .NOT. File_O(f_name)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�モ ����� �璢│�."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

IF .NOT. Get_Date()
  RETURN
ENDIF

DO Wt_Mess WITH "��ぃ�皰→� ����諷..."

DELETE FILE (tmpo_path+"ARCH_S.DBF")
DELETE FILE (tmpo_path+"ARCH_S.CDX")
DELETE FILE (tmpo_path+"ARCH_T.DBF")
DELETE FILE (tmpo_path+"ARCH_T.CDX")

CREATE DBF (tmpo_path+"ARCH_S") ;
  ( FIRM     C( 3), ;
    DOC_NUM  C(10), ;
    DOC_DATE D,     ;
    CODE     N( 7), ;
    QNT      N( 6), ;
    PRICE_R  N(12,5)  )
INDEX ON FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM TAG DOC_NUM

SELECT 0
CREATE DBF (tmpo_path+"ARCH_T") ;
  ( FIRM     C( 3), ;
    DOC_NUM  C(10), ;
    DOC_DATE D,     ;
    TOTAL_A  C(12), ;
    TOTAL_S  C(12)   )
INDEX ON FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM TAG DOC_NUM
    
SELECT 0
USE (f_name) ORDER TAG DATE_CODE ALIAS ARCH
SEEK DTOS(dt_w)
IF .NOT. FOUND()
  n0 = RECNO(0)
  IF BETWEEN(n0, 1, RECCOUNT())
    GO n0
  ENDIF
ENDIF
SCAN REST WHILE DATE <= dt_e
  SELECT ARCH_S
  APPEND BLANK
  REPLACE FIRM     WITH ARCH.FIRM,     ;
          DOC_NUM  WITH ARCH.DOC_NUM,  ;
          DOC_DATE WITH ARCH.DATE,     ;
          CODE     WITH ARCH.STO_CODE, ;
          QNT      WITH ARCH.QTY,      ;
          PRICE_R  WITH ARCH.PRICE
  SELECT ARCH
ENDSCAN

USE
SELECT ARCH_S
s = 0
n = "          "
f = "   "
d = {}
SCAN
  IF DOC_NUM # n .OR. FIRM # f .OR. DOC_DATE # d
    IF .NOT. EMPTY(n)
      SELECT ARCH_T
      APPEND BLANK
      REPLACE FIRM     WITH f,   ;
              DOC_NUM  WITH n,   ;
              DOC_DATE WITH d,   ;
              TOTAL_A  WITH STR(s,12,2), ;
              TOTAL_S  WITH "臼臼臼臼臼臼"
      SELECT ARCH_S
    ENDIF
    n = DOC_NUM
    d = DOC_DATE
    f = FIRM
    s = 0
  ENDIF
  s = s+ROUND(QNT*PRICE_R,2)
ENDSCAN

IF .NOT. EMPTY(n)
  SELECT ARCH_T
  APPEND BLANK
  REPLACE FIRM     WITH f,  ;
          DOC_NUM  WITH n,  ;
          DOC_DATE WITH d,  ;
          TOTAL_A  WITH STR(s,12,2), ;
          TOTAL_S  WITH "臼臼臼臼臼臼"
ENDIF

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON

SEEK DTOS(dt_w)
IF .NOT. FOUND()
  n0 = RECNO(0)
  IF BETWEEN(n0, 1, RECCOUNT())
    GO n0
  ENDIF
ENDIF
SCAN REST WHILE DOC_DATE <= dt_e FOR CUS_CODE >= 0
  SELECT ARCH_T
  IF .NOT. SEEK(SALE_TIT.FIRM+LEFT(DTOS(SALE_TIT.DOC_DATE),4)+SALE_TIT.DOC_NUM)
    APPEND BLANK
    REPLACE FIRM     WITH SALE_TIT.FIRM,     ;
            DOC_NUM  WITH SALE_TIT.DOC_NUM,  ;
            DOC_DATE WITH SALE_TIT.DOC_DATE, ;
            TOTAL_A  WITH "臼臼臼臼臼臼",    ;
            TOTAL_S  WITH STR(SALE_TIT.TOTAL_R,12,2)
  ELSE
    REPLACE TOTAL_S WITH STR(SALE_TIT.TOTAL_R,12,2)
  ENDIF
  SELECT SALE_TIT
ENDSCAN

DO Wt_Mess

USE
SELECT ARCH_T

IF RECCOUNT() # 0
  DO View_Res
ELSE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"蹍��� �� �ム┏� � "+DTOC(dt_w)+" �� "+DTOC(dt_e)+" �� �´ム礒キ�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

CLOSE DATABASES

DELETE FILE (tmpo_path+"ARCH_S.DBF")
DELETE FILE (tmpo_path+"ARCH_S.CDX")
DELETE FILE (tmpo_path+"ARCH_T.DBF")
DELETE FILE (tmpo_path+"ARCH_T.CDX")
RETURN

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                               �〓� ��硅.                               �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 17.03.98 15:53:14 夕
PROCEDURE Get_Date

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

*          蹍▲爲� �璢│�
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�                                  �
*�    C DD.MM.YYYY �� DD.MM.YYYY    �
*�                                  �
*� < OK Ctrl-W > < �皖���碎瘴 Esc > �
*�                                  �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 38, "蹍▲爲� �璢│�"

*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
@ 3,  6 SAY "�" GET dt_w
@ 3, 19 SAY "��" GET dt_e
@ 5,  3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�                                                                        �
*�                          蹍甃�矗 爛с�赳���.                          �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様� 17.03.98 16:00:42 夕
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

*
*   ������錺� Л�腑��鍖� �ムガキ�襯 甌痰�鎰��...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()
PRIVATE flt, rc_sav
flt = .F.

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD View OF (menu_name) PROMPT "蹍甃�矗" KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Print OF (menu_name) PROMPT "�ョ�碎 ぎ�-��" KEY Ctrl-A, "Tab"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Filter OF (menu_name) PROMPT "���讓� ��痳." KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   �皖琺��ガ ����� �.�., 竅����←│�ガ 瓣錻� � �. �.
*
DO Use_Dummy
SELECT ARCH_T
*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = IIF(SET("CENTURY") = "ON", 54, 52)

DO D_Wins WITH ln, wd, "�����き襯 � "+DTOC(dt_w)+" �� "+DTOC(dt_e), 0, 0
what_do = "List"
@ 1, 2 SAY IIF(SET("CENTURY") = "ON",  ;
         "�爼陳N ぎ�-��陳陳����陳陳陳陳陳陳�璢│陳陳�����き�鐵", ;
         "�爼陳N ぎ�-��陳�����陳陳陳陳陳�璢│陳陳�����き�鐵" )

GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F6    DO Sw_Mode WITH "Filter"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Print"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS FIRM:H="", ;
                  F_Doc = LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5)):11:H="", ;
                  DOC_DATE:P="@D", ;
                  TOTAL_A:H="":12, ;
                  TOTAL_S:H="":12, ;
                  Is_Eq = IIF(TOTAL_A = TOTAL_S, " ", "�"):1:H="" , ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() = 12, "Exit", "View")
    ENDIF

  CASE what_do = "Print"    && �ョ�碎 ぎ�祠キ��

    DO Prn_Inv WITH FIRM, DOC_NUM, DOC_DATE
    what_do = "List"
    
  CASE what_do = "View"      && 蹍甃�矗 ぎ�祠キ皰�

    DO View
    what_do = "List"
    
  CASE what_do = "Filter"    && �痰���→�/��閧┘ 筥��矗�

    IF .NOT. flt
      rc_sav = RECNO()
      SET FILTER TO TOTAL_A # TOTAL_S
      GO TOP
      IF EOF() .OR. BOF()
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"�モ ��痳�Δキ┤!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
        SET FILTER TO
        IF BETWEEN(rc_sav, 1, RECCOUNT("ARCH_T"))
          GO rc_sav IN ARCH_T
        ENDIF
      ELSE
        IF BETWEEN(rc_sav, 1, RECCOUNT("ARCH_T"))
          GO rc_sav IN ARCH_T
        ENDIF
        DEFINE PAD Filter OF (menu_name) PROMPT "�瓮." BEFORE Exit KEY Ctrl-A, "F6"
        flt = .T.
      ENDIF
    ELSE
      SET FILTER TO
      DEFINE PAD Filter OF (menu_name) PROMPT "���讓� ��痳." KEY Ctrl-A, "F6"
      flt = .F.
    ENDIF
    what_do = "List"
    
  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View         ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          蹍甃�矗 �����き諷.                           �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 18.02.2000 陳�
PROCEDURE View

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
DELETE FILE (tmpo_path+"TMP_VIEW.DBF")
DELETE FILE (tmpo_path+"TMP_VIEW.CDX")
CREATE DBF (tmpo_path+"TMP_VIEW") ;
  ( CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT_S    N( 6), ;
    QNT_A    N( 6), ;
    PRICE_S  N(12,5),;
    PRICE_A  N(12,5)  )
INDEX ON CODE TAG CODE
    
SELECT 0
DO Use_Dummy
SELECT ARCH_S
SET ORDER TO TAG DOC_NUM
SEEK ARCH_T.FIRM+LEFT(DTOS(ARCH_T.DOC_DATE),4)+ARCH_T.DOC_NUM
SCAN REST WHILE FIRM = ARCH_T.FIRM .AND. DOC_DATE = ARCH_T.DOC_DATE .AND. DOC_NUM = ARCH_T.DOC_NUM
  SELECT TMP_VIEW
  IF .NOT. SEEK(ARCH_S.CODE)
    APPEND BLANK
  ENDIF
  REPLACE CODE    WITH ARCH_S.CODE, ;
          QNT_A   WITH QNT_A+ARCH_S.QNT, ;
          PRICE_A WITH ARCH_S.PRICE_R
  SELECT ARCH_S
ENDSCAN
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS SAL_0218 AGAIN
SEEK ARCH_T.FIRM+LEFT(DTOS(ARCH_T.DOC_DATE),4)+ARCH_T.DOC_NUM
SCAN REST WHILE FIRM = ARCH_T.FIRM .AND. DOC_DATE = ARCH_T.DOC_DATE .AND. DOC_NUM = ARCH_T.DOC_NUM
  SELECT TMP_VIEW
  IF .NOT. SEEK(SAL_0218.CODE)
    APPEND BLANK
  ENDIF
  REPLACE CODE    WITH SAL_0218.CODE, ;
          QNT_S   WITH QNT_S+SAL_0218.QNT, ;
          PRICE_S WITH SAL_0218.PRICE_D
  SELECT SAL_0218
ENDSCAN
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_0218 AGAIN
SELECT TMP_VIEW
SET RELATION TO CODE INTO ST_0218
REPLACE ALL PREFIX   WITH ST_0218.PREFIX, ;
            NAME     WITH ST_0218.NAME,   ;
            PRODUCER WITH ST_0218.PRODUCER
SET RELATION TO
SELECT ST_0218
USE
SELECT TMP_VIEW
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
GO TOP
*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 74
*敖��Ж罔鐵陳陳陳陳陳陳陳陳陳陳陳�� �璢陳�� ��陳陳陳�キ� �璢陳陳��キ� ��陳朕
*�NNNN NNNNNNNNNNNNNNNNNNNNNNNNN 999999 999999 V 999999999999 99999999999 V�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

DO D_Wins WITH ln, wd, "��ぅ爨━�� �����き��", 0, 1
@ 1, 2 SAY "���Ж罔鐵陳陳陳陳陳陳陳陳陳陳陳�� �璢陳�� ��陳陳陳�キ� �璢陳陳��キ� ���"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:H="", Brw_Name=Nm_Part(NAME):25:H="",   ;
                  QNT_A:H="":P="@Z 999999", ;
                  QNT_S:H="":P="@Z 999999", ;
                  EQ_QNT=IIF(QNT_A=QNT_S, " ", "�"):1:H="", ;
                  PRICE_A:H="":P="@Z 999999.99999", ;
                  PRICE_S:H="":P="@Z 999999.99999", ;
                  EQ_PRICE=IIF(PRICE_A=PRICE_S, " ", "�"):1:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

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
USE
DELETE FILE (tmpo_path+"TMP_VIEW.DBF")
DELETE FILE (tmpo_path+"TMP_VIEW.CDX")
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
@ WROWS()-2, 10 SAY "��爼� "+PRODUCER

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
