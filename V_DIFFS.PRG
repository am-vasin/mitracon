*浜様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様様融
*�   ��� ����� V_Diffs      ��о�｀砒┴ ��むォ ��瓱�                      �
*把陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳超
*�                                                                        �
*�           蹍甃�矗 ぎ�祠キ皰� �� �爛がモ ���瓷� ��痳�Δキ┤.           �
*�                                                                        �
*藩様様様様様様様様様様様様様様様様様様様様様様様様様様様様� 04.06.2000 様�
PROCEDURE V_Diffs

PRIVATE d_0, d_1, s_code, s_name
d_1 = DATE()
d_0 = DATE()-30
IF sale_mode = "Mitracon"
  s_code = 10
  s_name = "�甅�↓�� 瓷���                                    "
ELSE
  s_code = 0
  s_name = SPACE(50)
ENDIF
IF .NOT. Fill_P()
  RETURN
ENDIF

DO Wt_Mess WITH "��ぃ�皰→� ����諷"
DO Prep_Tmp WITH d_0, d_1, s_code
DO Wt_Mess
DO V_Docs
SELECT V_DIFFS
USE
SELECT TMP0
USE
SELECT TMP00
USE
SELECT TMP1
USE
SELECT TMP11
USE
DELETE FILE (tmpo_path+"TMP0.DBF")
DELETE FILE (tmpo_path+"TMP0.CDX")
DELETE FILE (tmpo_path+"TMP1.DBF")
DELETE FILE (tmpo_path+"TMP1.CDX")
DELETE FILE (tmpo_path+"TMP00.DBF")
DELETE FILE (tmpo_path+"TMP00.CDX")
DELETE FILE (tmpo_path+"TMP11.DBF")
DELETE FILE (tmpo_path+"TMP11.CDX")
DELETE FILE (tmpo_path+"V_DIFFS.DBF")
DELETE FILE (tmpo_path+"V_DIFFS.CDX)
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Fill_P       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                   ��皀���皋↓�� 竅����→� �����モ牀�                   �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04.06.2000 陳�
PROCEDURE Fill_P
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

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*� 敖 [ ] ��痰�∫┴ 陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� �
*� � ....:....!....:....!....:....!....:....!....:....! � �
*� 青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰 �
*�      ��皀燿�� � ��.��.����(F1) �� ��.��.����(F1)       �
*�            < OK Ctrl-W > < �皖���碎瘴 Esc >            �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳潰

PRIVATE ex, sw
sw = .F.
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && �� ≡鍼┤ ����爿覃 甄竍��!
DO Prp_Nav_2
DO D_Win_N WITH 8, 60, "�������皀 ゛���"
DO Sun_Bord WITH  2,  3,  4, 56, " [ ] ��痰�∫┴ "
@ 5, 8 SAY "��皀燿�� � ��.��.����(F1) �� ��.��.����(F1)"
*------------------------------------------------------------------------
*      �〓� ���ォ ゛����
*
ON KEY LABEL F1 DO Int_D
@  2,  6 GET sw PICTURE "@*C ��痰�∫┴" WHEN sale_mode # "Mitracon" VALID G_Sup()
@  3,  5 GET s_name WHEN .F.
@  5, 19 GET d_0
@  5, 37 GET d_1
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? �皖���碎瘴 Esc "

READ CYCLE VALID Tst_Blank()
ON KEY LABEL F1

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Int_D        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �襷�� ���キ��瑙.                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04.06.2000 陳�
PROCEDURE Int_D

IF VARREAD() = "D_0"
  d_0 = Get_Date(d_0)
  SHOW GET d_0
ENDIF
IF VARREAD() = "D_1"
  d_1 = Get_Date(d_1)
  SHOW GET d_1
ENDIF

RETURN
*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� G_Sup        ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                            �襦�� ��痰�∫┴�                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04.06.2000 陳�
PROCEDURE G_Sup

IF .NOT. sw
  RETURN .T.
ENDIF

PRIVATE tmpName
tmpName = ""

DO Get_Sup WITH s_code, tmpName, s_name
s_name = LEFT(s_name, 50)
sw = .F.
SHOW GET sw
SHOW GET s_name

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Prep_Tmp     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                      ��ぃ�皰→� ∇ガキ���� �����                       �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04.06.2000 陳�
PROCEDURE Prep_Tmp
PARAMETERS dt_0, dt_1, sup_c
PRIVATE g_count

*
*   ��皰※� ��琺 �砒モ - �����き��
*

DELETE FILE (tmpo_path+"TMP0.DBF")
DELETE FILE (tmpo_path+"TMP0.CDX")
CREATE DBF (tmpo_path+"TMP0") ;
  ( DOC_NUM  C(10), ;
    DOC_DATE D )

SELECT 0
IF sale_mode # "Mitracon"
  USE (base_path+"REP_TIT")
  SCAN FOR BETWEEN(D_REP, dt_0, dt_1) .AND. SUP_CODE = sup_c
    SELECT TMP0
    APPEND BLANK
    REPLACE DOC_NUM WITH STR(REP_TIT.N_REP,10), DOC_DATE WITH REP_TIT.D_REP
    SELECT REP_TIT
  ENDSCAN
  USE
ENDIF
DELETE FILE (tmpo_path+"TMP1.DBF")
DELETE FILE (tmpo_path+"TMP1.CDX")
CREATE DBF (tmpo_path+"TMP1") ;
  ( DOC_NUM  C(10), ;
    DOC_DATE D,     ;
    DOC_FIRM C( 3), ; 
    REF_NUM  C(10), ;
    REF_DATE D )
SELECT 0
USE (base_path+"S_SALE_T") ORDER TAG HRON
IF sale_mode # "Mitracon"
  IF .NOT. SEEK(DTOS(dt_0))
    IF BETWEEN(RECNO(0), 1, RECCOUNT())
      GO RECNO(0)
    ENDIF
  ENDIF
  SCAN REST WHILE DOC_DATE <= dt_1 FOR SUP_CODE = sup_c
    SELECT TMP1
    APPEND BLANK
    REPLACE DOC_NUM  WITH S_SALE_T.DOC_NUM, ;
            DOC_DATE WITH S_SALE_T.DOC_DATE,;
            DOC_FIRM WITH S_SALE_T.DOC_FIRM,;
            REF_NUM  WITH S_SALE_T.REF_NUM, ;
            REF_DATE WITH S_SALE_T.REF_DATE
    SELECT S_SALE_T
  ENDSCAN
ENDIF
USE
DELETE FILE (tmpo_path+"V_DIFFS.DBF")
DELETE FILE (tmpo_path+"V_DIFFS.CDX)
CREATE DBF (tmpo_path+"V_DIFFS") ;
  ( ORDER  C(16), ;
    GROUP  N( 8), ;
    DOC_N0 C(10), ;
    DOC_D0 C(10), ;
    DOC_F0 C( 3), ;
    DOC_V0 C(14), ;
    DOC_T0 C( 6), ;
    LINK0  N(10), ;
    DOC_M0 C( 1), ;
    DOC_N1 C(10), ;
    DOC_D1 C(10), ;
    DOC_F1 C( 3), ;
    DOC_V1 C(14), ;
    DOC_T1 C( 6), ;
    LINK1  N(10), ;
    DOC_M1 C( 1)   )
SELECT TMP1
INDEX ON DTOS(REF_DATE)+REF_NUM TAG LINK
SELECT TMP0
SET RELATION TO DTOS(DOC_DATE)+DOC_NUM INTO TMP1
g_count = 0
SCAN
  g_count = g_count+1   && ���ム 珮��� ぎ�祠キ皰�
  SELECT V_DIFFS
  APPEND BLANK          && ≡��←錺� ��Гカ�皀�� �珮���
  REPLACE ORDER  WITH DTOS(TMP0.DOC_DATE)+STR(g_count,8), ;
          GROUP  WITH g_count, ;
          DOC_D0 WITH "----------", ;
          DOC_V0 WITH "--------------", ;
          DOC_T0 WITH "------", ;
          DOC_M0 WITH "-", ;
          DOC_D1 WITH "----------", ;
          DOC_V1 WITH "--------------", ;
          DOC_T1 WITH "------", ;
          DOC_M1 WITH "-"
  APPEND BLANK
  REPLACE ORDER  WITH DTOS(TMP0.DOC_DATE)+STR(g_count,8), ;
          GROUP  WITH g_count, ;
          DOC_N0 WITH TMP0.DOC_NUM, ;
          DOC_D0 WITH DTOC(TMP0.DOC_DATE), ;
          DOC_V0 WITH PADL(TMP0.DOC_NUM,14), ;
          DOC_T0 WITH "�砒モ",  ;
          LINK0  WITH RECNO("TMP0")
  IF FOUND("TMP1")
    REPLACE DOC_N1 WITH TMP1.DOC_NUM, ;
            DOC_F1 WITH TMP1.DOC_FIRM, ;
            DOC_D1 WITH DTOC(TMP1.DOC_DATE), ;
            DOC_V1 WITH LEFT(TMP1.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(TMP1.DOC_NUM,5))+TMP1.DOC_FIRM, ;
            DOC_T1 WITH "蹍�.",  ;
            LINK1  WITH RECNO("TMP1")
    SELECT TMP1
    DELETE
    SCAN REST WHILE REF_NUM = TMP0.DOC_NUM .AND. REF_DATE = TMP0.DOC_DATE FOR .NOT. DELETED()
      SELECT V_DIFFS
      APPEND BLANK
      REPLACE ORDER  WITH DTOS(TMP0.DOC_DATE)+STR(g_count,8), ;
              GROUP  WITH g_count, ;
              DOC_N1 WITH TMP1.DOC_NUM, ;
              DOC_F1 WITH TMP1.DOC_FIRM, ;
              DOC_D1 WITH DTOC(TMP1.DOC_DATE), ;
              DOC_V1 WITH LEFT(TMP1.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(TMP1.DOC_NUM,5))+TMP1.DOC_FIRM, ;
              DOC_T1 WITH "蹍�.",  ;
              LINK1  WITH RECNO("TMP1")
      SELECT TMP1
      DELETE
    ENDSCAN
  ENDIF
  SELECT TMP0
ENDSCAN
SET RELATION TO
SELECT TMP1
SCAN
  g_count = g_count+1   && ���ム 珮��� ぎ�祠キ皰�
  SELECT V_DIFFS
  APPEND BLANK          && ≡��←錺� ��Гカ�皀�� �珮���
  REPLACE ORDER  WITH DTOS(TMP1.DOC_DATE)+STR(g_count,8), ;
          GROUP  WITH g_count, ;
          DOC_D0 WITH "----------", ;
          DOC_V0 WITH "--------------", ;
          DOC_T0 WITH "------", ;
          DOC_M0 WITH "-", ;
          DOC_D1 WITH "----------", ;
          DOC_V1 WITH "--------------", ;
          DOC_T1 WITH "------", ;
          DOC_M1 WITH "-"
  APPEND BLANK
  REPLACE ORDER  WITH DTOS(TMP1.DOC_DATE)+STR(g_count,8), ;
          GROUP  WITH g_count, ;
          DOC_N1 WITH TMP1.DOC_NUM, ;
          DOC_F1 WITH TMP1.DOC_FIRM, ;
          DOC_D1 WITH DTOC(TMP1.DOC_DATE), ;
          DOC_V1 WITH LEFT(TMP1.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(TMP1.DOC_NUM,5))+TMP1.DOC_FIRM, ;
          DOC_T1 WITH "蹍�.",  ;
          LINK1  WITH RECNO("TMP1")
  SELECT TMP1
ENDSCAN
SET DELETED OFF
RECALL ALL
SET DELETED ON

*
*   ��皰※� ��琺 �����き�� - �爬ガ�覃 ���
*

DELETE FILE (tmpo_path+"TMP00.DBF")
DELETE FILE (tmpo_path+"TMP00.CDX")
CREATE DBF (tmpo_path+"TMP00") ;
  ( DOC_NUM  C(10), ;
    DOC_DATE D )

SELECT 0
USE (base_path+"S_RYN_T") ORDER TAG HRON
IF .NOT. SEEK(DTOS(dt_0))
  IF BETWEEN(RECNO(0), 1, RECCOUNT())
    GO RECNO(0)
  ENDIF
ENDIF
SCAN REST WHILE DOC_DATE <= dt_1 FOR SUP_CODE = sup_c
  SELECT TMP00
  APPEND BLANK
  REPLACE DOC_NUM WITH S_RYN_T.DOC_NUM, DOC_DATE WITH S_RYN_T.DOC_DATE
  SELECT S_RYN_T
ENDSCAN
USE
DELETE FILE (tmpo_path+"TMP11.DBF")
DELETE FILE (tmpo_path+"TMP11.CDX")
CREATE DBF (tmpo_path+"TMP11") ;
  ( DOC_NUM  C(10), ;
    DOC_DATE D,     ;
    DOC_FIRM C( 3), ; 
    REF_NUM  C(10), ;
    REF_DATE D )
SELECT 0
USE (base_path+"ACT_S_T")
SCAN FOR BETWEEN(DOC_DATE,dt_0,dt_1) .AND. SUP_CODE = sup_c .AND. ;
                    FLG_BUY = "R" .AND. INV_TYPE # "9"
  SELECT TMP11
  APPEND BLANK
  REPLACE DOC_NUM  WITH ACT_S_T.DOC_NUM, ;
          DOC_DATE WITH ACT_S_T.DOC_DATE,;
          DOC_FIRM WITH ACT_S_T.FIRM,;
          REF_NUM  WITH ACT_S_T.EXP_NUM, ;
          REF_DATE WITH ACT_S_T.EXP_DATE
  SELECT ACT_S_T
ENDSCAN
USE

SELECT TMP11
INDEX ON DTOS(REF_DATE)+REF_NUM TAG LINK
SELECT TMP00
SET RELATION TO DTOS(DOC_DATE)+DOC_NUM INTO TMP11
SCAN
  g_count = g_count+1   && ���ム 珮��� ぎ�祠キ皰�
  SELECT V_DIFFS
  APPEND BLANK          && ≡��←錺� ��Гカ�皀�� �珮���
  REPLACE ORDER  WITH DTOS(TMP00.DOC_DATE)+STR(g_count,8), ;
          GROUP  WITH g_count, ;
          DOC_D0 WITH "----------", ;
          DOC_V0 WITH "--------------", ;
          DOC_T0 WITH "------", ;
          DOC_M0 WITH "-", ;
          DOC_D1 WITH "----------", ;
          DOC_V1 WITH "--------------", ;
          DOC_T1 WITH "------", ;
          DOC_M1 WITH "-"
  APPEND BLANK
  REPLACE ORDER  WITH DTOS(TMP00.DOC_DATE)+STR(g_count,8), ;
          GROUP  WITH g_count, ;
          DOC_N1 WITH TMP00.DOC_NUM, ;
          DOC_D1 WITH DTOC(TMP00.DOC_DATE), ;
          DOC_V1 WITH PADL(TMP00.DOC_NUM,14), ;
          DOC_T1 WITH "����.",  ;
          LINK1  WITH RECNO("TMP00")
  IF FOUND("TMP11")
    REPLACE DOC_N0 WITH TMP11.DOC_NUM, ;
            DOC_F0 WITH TMP11.DOC_FIRM, ;
            DOC_D0 WITH DTOC(TMP11.DOC_DATE), ;
            DOC_V0 WITH ALLTRIM(TMP11.DOC_NUM)+"/"+TMP11.DOC_FIRM, ;
            DOC_T0 WITH "��.���",  ;
            LINK0  WITH RECNO("TMP11")
    SELECT TMP11
    DELETE
    SCAN REST WHILE REF_NUM = TMP00.DOC_NUM .AND. REF_DATE = TMP00.DOC_DATE FOR .NOT. DELETED()
      SELECT V_DIFFS
      APPEND BLANK
      REPLACE ORDER  WITH DTOS(TMP00.DOC_DATE)+STR(g_count,8), ;
              GROUP  WITH g_count, ;
              DOC_N0 WITH TMP11.DOC_NUM, ;
              DOC_F0 WITH TMP11.DOC_FIRM, ;
              DOC_D0 WITH DTOC(TMP11.DOC_DATE), ;
              DOC_V0 WITH ALLTRIM(TMP11.DOC_NUM)+"/"+TMP11.DOC_FIRM, ;
              DOC_T0 WITH "��.���",  ;
              LINK0  WITH RECNO("TMP11")
      SELECT TMP11
      DELETE
    ENDSCAN
  ENDIF
  SELECT TMP00
ENDSCAN
SET RELATION TO
SELECT TMP11
SCAN
  g_count = g_count+1   && ���ム 珮��� ぎ�祠キ皰�
  SELECT V_DIFFS
  APPEND BLANK          && ≡��←錺� ��Гカ�皀�� �珮���
  REPLACE ORDER  WITH DTOS(TMP11.DOC_DATE)+STR(g_count,8), ;
          GROUP  WITH g_count, ;
          DOC_D0 WITH "----------", ;
          DOC_V0 WITH "--------------", ;
          DOC_T0 WITH "------", ;
          DOC_M0 WITH "-", ;
          DOC_D1 WITH "----------", ;
          DOC_V1 WITH "--------------", ;
          DOC_T1 WITH "------", ;
          DOC_M1 WITH "-"
  APPEND BLANK
  REPLACE ORDER  WITH DTOS(TMP11.DOC_DATE)+STR(g_count,8), ;
          GROUP  WITH g_count, ;
          DOC_N0 WITH TMP11.DOC_NUM, ;
          DOC_F0 WITH TMP11.DOC_FIRM, ;
          DOC_D0 WITH DTOC(TMP11.DOC_DATE), ;
          DOC_V0 WITH ALLTRIM(TMP11.DOC_NUM)+"/"+TMP11.DOC_FIRM, ;
          DOC_T0 WITH "��.���",  ;
          LINK0  WITH RECNO("TMP11")
  SELECT TMP11
ENDSCAN
SET DELETED OFF
RECALL ALL
SET DELETED ON
SELECT V_DIFFS
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� V_Docs       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          蹍甃�矗 ぎ�祠キ皰�.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 04.06.2000 陳�
PROCEDURE V_Docs

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
SELECT V_DIFFS
GO TOP

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Mark OF (menu_name) PROMPT "�皙モ�碎"
ON PAD Mark OF (menu_name) ACTIVATE POPUP Mark

  DEFINE POPUP Mark MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Mark PROMPT "�〓� ぎ�-�" KEY Ctrl-A, "F2"
  ON SELECTION BAR 1 OF Mark DO Sw_Mode WITH "Mark_L"

  DEFINE BAR  2 OF Mark PROMPT "���-� ��痰�∫┴�" KEY Ctrl-A, "F3"
  ON SELECTION BAR 2 OF Mark DO Sw_Mode WITH "Mark_R"
  
  DEFINE BAR  3 OF Mark PROMPT "�皙モ�碎 ≡� 瓣�� ぎ�-硅" KEY Ctrl-A, "Ctrl-F2"
  ON SELECTION BAR 3 OF Mark DO Sw_Mode WITH "All_Mark_L"

  DEFINE BAR  4 OF Mark PROMPT "�皙モ�碎 ≡� ぎ�-硅 ��痰�∫┴�" KEY Ctrl-A, "Ctrl-F3"
  ON SELECTION BAR 4 OF Mark DO Sw_Mode WITH "All_Mark_R"

  DEFINE BAR  5 OF Mark PROMPT "�腮痰�碎 ≡� 瓣�� ぎ�-硅" KEY Ctrl-A, "Ctrl-F2"
  ON SELECTION BAR 5 OF Mark DO Sw_Mode WITH "All_Clear_L"

  DEFINE BAR  6 OF Mark PROMPT "�腮痰�碎 ≡� ぎ�-硅 ��痰�∫┴�" KEY Ctrl-A, "Ctrl-F3"
  ON SELECTION BAR 6 OF Mark DO Sw_Mode WITH "All_Clear_R"

DEFINE PAD Count OF (menu_name) PROMPT "��瘍モ" KEY Ctrl-A, "Tab"
ON SELECTION PAD Count OF (menu_name) DO Sw_Mode WITH "Count"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 69

*敖����-眥陳���ム ぎ�-��陳�����陳陳陳陳���-眥陳���ム ぎ�-��陳�����陳陳朕
*�M Type.. DOC_NUM....... DOC_DATE.. M Type.. DOC_NUM....... DOC_DATE..�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*敖���祠キ� �甅����┘陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
*�                                                                     �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

DO D_Wins WITH ln, wd, "�ムョキ� ぎ�祠キ皰�", 0, 1
@ 1, 4 SAY "���-眥陳���ム ぎ�-��陳�����陳陳陳陳���-眥陳���ム ぎ�-��陳�����"
@ WROWS()-3, 3 SAY "���祠キ� �甅����┘" 
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F2 DO Mark_D WITH "L"
    ON KEY LABEL F3 DO Mark_D WITH "R"
    ON KEY LABEL Ctrl+F2 DO Mark_All WITH "L"
    ON KEY LABEL Ctrl+F3 DO Mark_All WITH "R"
    ON KEY LABEL Shift+F2 DO Clear_All WITH "L"
    ON KEY LABEL Shift+F3 DO Clear_All WITH "R"
    ON KEY LABEL Tab DO Sw_Mode WITH "Count"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS DOC_M0:H="", ;
                  DOC_T0:H="", ;
                  DOC_V0:H="", ;
                  DOC_D0:H="", ;
                  DOC_M1:H="", ;
                  DOC_T1:H="", ;
                  DOC_V1:H="", ;
                  DOC_D1:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  CASE what_do = "Count"    && 蹍甃�矗 甎�瓷�

    DO TmpMove
    DO Tmp_Diff
    DO View_D
    what_do = "List"

  CASE what_do = "Mark_L"    && �皙モ�碎 瓣�� ぎ�祠キ�

    DO Mark_D WITH "L"
    what_do = "List"

  CASE what_do = "Mark_R"    && �皙モ�碎 ぎ�祠キ� ��痰�∫┴�

    DO Mark_D WITH "R"
    what_do = "List"

  CASE what_do = "All_Mark_L" && �皙モ�碎 ≡� 瓣�� ぎ�祠キ硅

    DO Mark_All WITH "L"
    what_do = "List"

  CASE what_do = "All_Mark_R" && �皙モ�碎 ≡� ぎ�祠キ硅 ��痰�∫┴�

    DO Mark_All WITH "R"
    what_do = "List"

  CASE what_do = "All_Clear_L" && �腮痰�碎 ≡� 瓣�� ぎ�祠キ硅

    DO Clear_All WITH "L"
    what_do = "List"

  CASE what_do = "All_Clear_R" && �腮痰�碎 ≡� ぎ�祠キ硅 ��痰�∫┴�

    DO Clear_All WITH "R"
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
@ WROWS()-2, 2 SAY SPACE(WCOLS()-4)
IF V_DIFFS.DOC_T0 = "��.���"
  IF BETWEEN(V_DIFFS.LINK0, 1, RECCOUNT("TMP11"))
    GO V_DIFFS.LINK0 IN TMP11
    IF .NOT. EMPTY(TMP11.REF_NUM)
      @ WROWS()-2, 3 SAY "�����き�� N "+ALLTRIM(TMP11.REF_NUM)+" �� "+DTOC(TMP11.REF_DATE)
    ENDIF
  ENDIF
ENDIF

IF V_DIFFS.DOC_T1 = "蹍�. "
  IF BETWEEN(V_DIFFS.LINK1, 1, RECCOUNT("TMP1"))
    GO V_DIFFS.LINK1 IN TMP1
    IF .NOT. EMPTY(TMP1.REF_NUM)
      @ WROWS()-2, 3 SAY "�砒モ N "+ALLTRIM(TMP1.REF_NUM)+" �� "+DTOC(TMP1.REF_DATE)
    ENDIF
  ENDIF
ENDIF

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tst_Blank    ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         �諷�� ├ ���� ゛����.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.06.2000 陳�
PROCEDURE Tst_Blank
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF
IF s_code = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"�� �� 礫������ ��痰�∫┴�!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Mark_D       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                           �皙モ�� ぎ�祠キ��                            �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.06.2000 陳�
PROCEDURE Mark_D
PARAMETERS mode

IF mode = "L"
  IF .NOT. DOC_M0 $ " �" .OR. EMPTY(DOC_N0)
    RETURN
  ENDIF
  REPLACE DOC_M0 WITH IIF(EMPTY(DOC_M0), "�", "")
ELSE
  IF .NOT. DOC_M1 $ " �" .OR. EMPTY(DOC_N1)
    RETURN
  ENDIF
  REPLACE DOC_M1 WITH IIF(EMPTY(DOC_M1), "�", "")
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Mark_All     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �皙モ�� ≡ュ ぎ�祠キ皰�                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.06.2000 陳�
PROCEDURE Mark_All
PARAMETERS mode
PRIVATE r_sav

r_sav = RECNO()
IF mode = "L"
  REPLACE ALL FOR EMPTY(DOC_M0) .AND. .NOT. EMPTY(DOC_N0) DOC_M0 WITH "�"
ELSE
  REPLACE ALL FOR EMPTY(DOC_M1) .AND. .NOT. EMPTY(DOC_N1) DOC_M1 WITH "�"
ENDIF

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Clear_All     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                        �皙モ�� ≡ュ ぎ�祠キ皰�                         �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.06.2000 陳�
PROCEDURE Clear_All
PARAMETERS mode
PRIVATE r_sav

r_sav = RECNO()
IF mode = "L"
  REPLACE ALL FOR DOC_M0 = "�" DOC_M0 WITH " "
ELSE
  REPLACE ALL FOR DOC_M1 = "�" DOC_M1 WITH " "
ENDIF

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� TmpMove      ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                       ��ぁ�爲� あ└キ�� 皰����.                        �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.06.2000 陳�
PROCEDURE TmpMove

PRIVATE s_sav, r_sav
s_sav = SELECT()
r_sav = RECNO()

*
*    �※Ε�┘ �� ぎ�祠キ��� ��痰�∫┴�
*
SELECT 0
DELETE FILE (tmpo_path+"TMP_M_S.DBF")
DELETE FILE (tmpo_path+"TMP_M_S.CDX")

CREATE DBF (tmpo_path+"TMP_M_S") ;
  ( CODE     N( 7), ;
    DOC_NUM  C(10), ;
    DOC_FIRM C( 3), ;
    DOC_DATE D,     ;
    DOC_TYPE C( 6), ;
    QNT      N( 6)  )

SELECT 0
USE (base_path+"S_SALE") ORDER TAG DOC_NUM
SELECT V_DIFFS
SCAN FOR DOC_M1 = "�" .AND. DOC_T1 = "蹍�. "
  SELECT S_SALE
  SEEK V_DIFFS.DOC_F1+RIGHT(V_DIFFS.DOC_D1,4)+V_DIFFS.DOC_N1+STR(s_code)
  SCAN REST WHILE V_DIFFS.DOC_F1 = DOC_FIRM .AND. ;
                  V_DIFFS.DOC_D1 = DTOC(DOC_DATE) .AND. ;
                  V_DIFFS.DOC_N1 = DOC_NUM  .AND.  ;
                  s_code  = SUP_CODE
    SELECT TMP_M_S
    APPEND BLANK
    REPLACE CODE     WITH S_SALE.CODE,    ;
            DOC_NUM  WITH S_SALE.DOC_NUM, ;
            DOC_FIRM WITH S_SALE.DOC_FIRM,;
            DOC_DATE WITH S_SALE.DOC_DATE,;
            DOC_TYPE WITH "蹍�.",        ;
            QNT      WITH -S_SALE.QNT
    SELECT S_SALE
  ENDSCAN
  SELECT V_DIFFS
ENDSCAN

SELECT S_SALE 
USE (base_path+"S_RYN") ORDER TAG DOC_NUM
SELECT V_DIFFS
SCAN FOR DOC_M1 = "�" .AND. DOC_T1 = "����. "
  SELECT S_RYN
  SEEK V_DIFFS.DOC_F1+RIGHT(V_DIFFS.DOC_D1,4)+V_DIFFS.DOC_N1+STR(s_code)
  SCAN REST WHILE V_DIFFS.DOC_F1 = DOC_FIRM .AND. ;
                  V_DIFFS.DOC_D1 = DTOC(DOC_DATE) .AND. ;
                  V_DIFFS.DOC_N1 = DOC_NUM  .AND.  ;
                  s_code  = SUP_CODE
    SELECT TMP_M_S
    APPEND BLANK
    REPLACE CODE     WITH S_RYN.CODE,    ;
            DOC_NUM  WITH S_RYN.DOC_NUM, ;
            DOC_FIRM WITH S_RYN.DOC_FIRM,;
            DOC_DATE WITH S_RYN.DOC_DATE,;
            DOC_TYPE WITH "����.",        ;
            QNT      WITH S_RYN.QNT
    SELECT S_RYN
  ENDSCAN
  SELECT V_DIFFS
ENDSCAN
SELECT S_RYN
USE

CREATE DBF (tmpo_path+"TMP_M_O") ;
  ( CODE     N( 7), ;
    DOC_NUM  C(10), ;
    DOC_FIRM C( 3), ;
    DOC_DATE D,     ;
    DOC_TYPE C( 6), ;
    QNT      N( 6)  )

SELECT 0
USE (base_path+"ACT_S") ORDER TAG DOC_NUM
SELECT V_DIFFS
SCAN FOR DOC_M0 = "�" .AND. DOC_T0 = "��.���"
  SELECT ACT_S
  SEEK V_DIFFS.DOC_F0+RIGHT(V_DIFFS.DOC_D0,4)+V_DIFFS.DOC_N0
  SCAN REST WHILE V_DIFFS.DOC_D0 = DTOC(DOC_DATE) .AND. ;
                  V_DIFFS.DOC_N0 = DOC_NUM .AND.  ;
                  V_DIFFS.DOC_F0 = FIRM
    SELECT TMP_M_O
    APPEND BLANK
    REPLACE CODE     WITH ACT_S.CODE,    ;
            DOC_NUM  WITH ACT_S.DOC_NUM, ;
            DOC_FIRM WITH ACT_S.FIRM,    ;
            DOC_DATE WITH ACT_S.DOC_DATE,;
            DOC_TYPE WITH "��.���",      ;
            QNT      WITH ACT_S.QNT_INC
    SELECT ACT_S
  ENDSCAN
  *
  *   � ラ� ��玻オ罔�...
  *
  SELECT 0
  USE (base_path+"ACT_S_T") ORDER TAG EXP_NUM
  SEEK RIGHT(V_DIFFS.DOC_D0,4)+V_DIFFS.DOC_N0+"9"+V_DIFFS.DOC_F0
  SCAN REST WHILE V_DIFFS.DOC_D0 = DTOC(EXP_DATE) .AND. ;
                  V_DIFFS.DOC_N0 = EXP_NUM .AND. ;
                  V_DIFFS.DOC_F0 = FIRM
    SELECT ACT_S
    SEEK ACT_S_T.FIRM+LEFT(DTOS(ACT_S_T.DOC_DATE),4)+ACT_S_T.DOC_NUM
    SCAN REST WHILE ACT_S_T.DOC_DATE = DOC_DATE .AND. ;
                    ACT_S_T.DOC_NUM = DOC_NUM .AND.  ;
                    ACT_S_T.FIRM = FIRM
      SELECT TMP_M_O
      APPEND BLANK
      REPLACE CODE     WITH ACT_S.CODE,    ;
              DOC_NUM  WITH ACT_S.DOC_NUM, ;
              DOC_FIRM WITH ACT_S.FIRM,    ;
              DOC_DATE WITH ACT_S.DOC_DATE,;
              DOC_TYPE WITH "��.���",      ;
              QNT      WITH ACT_S.QNT_INC
      SELECT ACT_S
    ENDSCAN
    SELECT ACT_S_T
  ENDSCAN
  USE
  SELECT V_DIFFS
ENDSCAN
SELECT ACT_S

USE (base_path+"REPORT") ORDER TAG N_REP
SELECT V_DIFFS
SCAN FOR DOC_M0 = "�" .AND. DOC_T0 = "�砒モ "
  SELECT REPORT
  SEEK RIGHT(V_DIFFS.DOC_D0,4)+RIGHT(V_DIFFS.DOC_N0,5)
  SCAN REST WHILE V_DIFFS.DOC_D0 = DTOC(D_REP) .AND. ;
                  VAL(ALLTRIM(V_DIFFS.DOC_N0)) = N_REP
    SELECT TMP_M_O
    APPEND BLANK
    REPLACE CODE     WITH REPORT.CODE,    ;
            DOC_NUM  WITH STR(REPORT.N_REP), ;
            DOC_DATE WITH REPORT.D_REP,   ;
            DOC_TYPE WITH "�砒モ",        ;
            QNT      WITH -REPORT.QNT
    SELECT REPORT
  ENDSCAN
  SELECT V_DIFFS
ENDSCAN
SELECT REPORT
USE
SELECT TMP_M_S
INDEX ON STR(CODE)+DTOS(DOC_DATE) TAG CODE
SELECT TMP_M_O
INDEX ON STR(CODE)+DTOS(DOC_DATE) TAG CODE
SELECT V_DIFFS
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Tmp_Diff     ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                    ��爼�牀���┘ ����� ��痳�Δキ┤.                     �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.06.2000 陳�
PROCEDURE Tmp_Diff

SELECT 0
DELETE FILE (tmpo_path+"TMP_DIFF.DBF")
DELETE FILE (tmpo_path+"TMP_DIFF.CDX")
CREATE DBF (tmpo_path+"TMP_DIFF")  ;
 ( CODE     N( 7), ;
   PREFIX   C( 4), ;
   NAME     C(35), ;
   PRODUCER C( 8), ;
   QNT_S    N( 8), ;
   QNT_O    N( 8)  )
INDEX ON CODE TAG CODE
SELECT TMP_M_S
SCAN
  SELECT TMP_DIFF
  IF .NOT. SEEK(TMP_M_S.CODE)
    APPEND BLANK
  ENDIF
  REPLACE CODE  WITH TMP_M_S.CODE, ;
          QNT_S WITH QNT_S+TMP_M_S.QNT
  SELECT TMP_M_S
ENDSCAN

SELECT TMP_M_O
SCAN
  SELECT TMP_DIFF
  IF .NOT. SEEK(TMP_M_O.CODE)
    APPEND BLANK
  ENDIF
  REPLACE CODE  WITH TMP_M_O.CODE, ;
          QNT_O WITH QNT_O+TMP_M_O.QNT
  SELECT TMP_M_O
ENDSCAN

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE
SELECT TMP_DIFF
SET RELATION TO CODE INTO STOCK
REPLACE ALL PREFIX   WITH STOCK.PREFIX, ;
            NAME     WITH STOCK.NAME,   ;
            PRODUCER WITH STOCK.PRODUCER
INDEX ON NAME+PREFIX+PRODUCER TAG DIFFS FOR QNT_O # QNT_S
INDEX ON NAME+PREFIX+PRODUCER TAG NAME ADDITIVE

SELECT STOCK
USE

SELECT V_DIFFS
RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� View_D       ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                         蹍甃�矗 ��痳�Δキ┤.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.06.2000 陳�
PROCEDURE View_D

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
SELECT Tmp_Diff

*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Switch OF (menu_name) PROMPT "���讓� ��痳." KEY Ctrl-A, "Tab"
ON SELECTION PAD Switch OF (menu_name) DO Sw_Mode WITH "Switch"

DEFINE PAD Position0 OF (menu_name) PROMPT "��＝癶. ぎ�." KEY Ctrl-A, "Enter"
ON SELECTION PAD Position0 OF (menu_name) DO Sw_Mode WITH "Pos_O"

DEFINE PAD Position1 OF (menu_name) PROMPT "���. ��痰." KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION PAD Position1 OF (menu_name) DO Sw_Mode WITH "Pos_S"

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 69
*敖陳���Ж罔鐵陳陳陳陳陳陳陳陳陳陳陳陳陳陳���＝癶.陳���痰.陳朕
*�.... ....:....!....:....!....: ....:... ....:... ....:... !�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�

DO D_Wins WITH ln, wd, "�※Ε�┘ ��Ж罔� ▼閧諷 �� 爛��├�罔�", 0, 0
@ 1, 6 SAY ;
"��Ж罔鐵陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳���＝癶.陳���痰."
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "Pos_S"
    ON KEY LABEL Tab DO Sw_Mode WITH "Switch"

*
*    BROWSE - �キ�
*
    BROWSE FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="",  ;
                  QNT_O:H="", QNT_S:H="",                 ;
                  DIFF = IIF(QNT_O = QNT_S, " ", "�"),    ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Pos_O")
    ENDIF

  CASE what_do = "Switch"    && ���讓� ��痳�Δキ��/≡�

    IF TAG() = "DIFFS"
      DEFINE PAD Switch OF (menu_name) PROMPT "���讓� ��痳." KEY Ctrl-A, "Tab" BEFORE Position0
      SET ORDER TO TAG NAME
    ELSE
      DEFINE PAD Switch OF (menu_name) PROMPT "�瓮 ��Ж罔�" KEY Ctrl-A, "Tab" BEFORE Position0
      SET ORDER TO TAG DIFFS
    ENDIF

    what_do = "List"

  CASE what_do = "Pos_O"    && �※Ε�┘ �� 瓣�━ ぎ�祠キ���

    DO Move WITH .T.
    what_do = "List"

  CASE what_do = "Pos_S"    && �※Ε�┘ �� ぎ�祠キ��� ��痰�∫┴�

    DO Move
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
SELECT TMP_M_S
USE
SELECT TMP_M_O
USE
SELECT TMP_DIFF
USE
DELETE FILE (tmpo_path+"TMP_M_S.DBF")
DELETE FILE (tmpo_path+"TMP_M_S.CDX")
DELETE FILE (tmpo_path+"TMP_M_O.DBF")
DELETE FILE (tmpo_path+"TMP_M_O.CDX")
DELETE FILE (tmpo_path+"TMP_DIFF.DBF")
DELETE FILE (tmpo_path+"TMP_DIFF.CDX")

SELECT (s_sav)

RETURN

*敖陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳朕
*�   蹍罐ゃ�� Move         ��о�｀砒┴ ��むォ ��瓱�                      �
*団陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳調
*�                                                                        �
*�                          �※ぅ�┘ �� ��Ж罔�.                          �
*�                                                                        �
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳� 05.06.2000 陳�
PROCEDURE Move
PARAMETERS is_own

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

IF is_own
  SELECT TMP_M_O
ELSE
  SELECT TMP_M_S
ENDIF
*
*   ｯ爛ぅ�錺� �瓱�縲����� �キ�
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "�諷��" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   ��瘍モ ��Кム�� ����
*
*����-眥陳����ム陳陳陳陳陳����陳陳���-〓朕
*�....:. ....:....! ... ....:....! ....:.�
*青陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳陳�
PRIVATE ln, wd
ln = WROWS("")-11   && ����腑痰〓 ※え�諷 痰牀� BROWSE
wd = 39

DO D_Wins WITH ln, wd, ALLTRIM(TMP_DIFF.PREFIX)+" "+  ;
                       ALLTRIM(TMP_DIFF.NAME)+" /"+   ;
                       ALLTRIM(TMP_DIFF.PRODUCER)+"/", 0, 0
@ 1, 2 SAY "���-眥陳����ム陳陳陳陳陳����陳陳���-〓"
what_do = "List"

GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && 蹍甃�矗 甎�瓷�

    statys_type = 3
    DO Prp_Nav_1

*
*    BROWSE - �キ�
*
    BROWSE KEY STR(TMP_DIFF.CODE) ;
           FIELDS DOC_TYPE:H="",  ;
                  DOC_NUM:H="",   ;
                  DOC_FIRM:H="",  ;
                  DOC_DATE:H="",  ;
                  QNT:H="",       ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
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
SELECT (s_sav)

RETURN
