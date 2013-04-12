*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Tmp     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Родготовка временного файла                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04.06.2000 ──┘
PROCEDURE Prep_Tmp
PARAMETERS dt_0, dt_1, sup_c
PRIVATE g_count

*
*   Готовим пары отчет - накладная
*

DELETE FILE (tmpo_path+"TMP0.DBF")
DELETE FILE (tmpo_path+"TMP0.CDX")
CREATE DBF (tmpo_path+"TMP0") ;
  ( DOC_NUM  C(10), ;
    DOC_DATE D )

SELECT 0
USE (base_path+"REP_TIT")
SCAN FOR BETWEEN(D_REP, dt_0, dt_1) .AND. SUP_CODE = sup_c
  SELECT TMP0
  APPEND BLANK
  REPLACE DOC_NUM WITH STR(REP_TIT.N_REP,10), DOC_DATE WITH REP_TIT.D_REP
  SELECT REP_TIT
ENDSCAN
USE
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
USE
DELETE FILE (tmpo_path+"V_DIFFS.DBF")
DELETE FILE (tmpo_path+"V_DIFFS.DCDX)
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
  g_count = g_count+1   && Номер руппы документов
  SELECT V_DIFFS
  APPEND BLANK          && вставляем разделитель группы
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
          DOC_T0 WITH "Отчет",  ;
          LINK0  WITH RECNO("TMP0")
  IF FOUND("TMP1")
    REPLACE DOC_N1 WITH TMP1.DOC_NUM, ;
            DOC_F1 WITH TMP1.DOC_FIRM, ;
            DOC_D1 WITH DTOC(TMP1.DOC_DATE), ;
            DOC_V1 WITH LEFT(TMP1.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(TMP1.DOC_NUM,5))+TMP1.DOC_FIRM, ;
            DOC_T1 WITH "Накл.",  ;
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
              DOC_T1 WITH "Накл.",  ;
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
  g_count = g_count+1   && Номер руппы документов
  SELECT V_DIFFS
  APPEND BLANK          && вставляем разделитель группы
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
          DOC_T1 WITH "Накл.",  ;
          LINK1  WITH RECNO("TMP1")
  SELECT TMP1
ENDSCAN
SET DELETED OFF
RECALL ALL
SET DELETED ON











*
*   Готовим пары накладная - приемный акт
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
  g_count = g_count+1   && Номер руппы документов
  SELECT V_DIFFS
  APPEND BLANK          && вставляем разделитель группы
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
          DOC_T1 WITH "Накл.",  ;
          LINK1  WITH RECNO("TMP00")
  IF FOUND("TMP11")
    REPLACE DOC_N0 WITH TMP11.DOC_NUM, ;
            DOC_F0 WITH TMP11.DOC_FIRM, ;
            DOC_D0 WITH DTOC(TMP11.DOC_DATE), ;
            DOC_V0 WITH ALLTRIM(TMP11.DOC_NUM)+"/"+TMP11.DOC_FIRM, ;
            DOC_T0 WITH "Пр.акт",  ;
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
              DOC_T0 WITH "Пр.акт",  ;
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
  g_count = g_count+1   && Номер руппы документов
  SELECT V_DIFFS
  APPEND BLANK          && вставляем разделитель группы
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
          DOC_T0 WITH "Пр.акт",  ;
          LINK0  WITH RECNO("TMP11")
  SELECT TMP11
ENDSCAN
SET DELETED OFF
RECALL ALL
SET DELETED ON

SELECT V_DIFFS
