*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Sup_Docs     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Ввод документов поставщика.                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 07/12/2000 ══╝
PROCEDURE Sup_Docs

PRIVATE pth_inc
pth_inc = "C:\"

pth_inc = Int_Dir(pth_inc, "Где файлы с документами?")
IF EMPTY(pth_inc)
  RETURN
ENDIF

DO Read_Docs

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Read_Docs    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Собственно ввод документов поставщика.                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.06.2000 ──┘
PROCEDURE Read_Docs
PRIVATE s_sav

s_sav = SELECT()
SELECT 0

PRIVATE w_ln, w_wd, w_row, w_col, cnt0, cnt1, cnt2
w_ln = 4
w_wd = 63
w_row = FLOOR((WROWS("")-w_ln)/2)
w_col = FLOOR((WCOLS("")-w_wd)/2)
DEFINE WINDOW Read_Docs FROM w_row, w_col TO w_row+w_ln-1, w_col+w_wd-1 ;
       NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW Read_Docs
@ 2, 3 SAY "Всего документов        Прочитано        Записано"

*
*┌───────────────────────────────────────────────────────────┐
*│ Всего документов 999999 Прочитано 999999 Записано 9999999 │
*└───────────────────────────────────────────────────────────┘

IF File_o(pth_inc+"s_sale.dbf") .AND. File_o(pth_inc+"s_sale_t.dbf")
  @ 0, 0 SAY PADC("Принятые отчеты", WCOLS())
  DELETE FILE (tmpo_path+"tmp_list.dbf")
  DELETE FILE (tmpo_path+"tmp_list.cdx")
  USE (pth_inc+"s_sale")
  cnt0 = RECCOUNT()
  @ 2, 20 SAY STR(cnt0,6)
  STORE 0 TO cnt1, cnt2
  COPY TO (tmpo_path+"TMP_LIST")
  USE (tmpo_path+"TMP_LIST") EXCLUSIVE
  INDEX ON DOC_FIRM+DTOS(DOC_DATE)+DOC_NUM TAG TMP
  SELECT 0
  USE (base_path+"S_SALE_T") ORDER TAG DOC_NUM
  SELECT 0
  USE (base_path+"S_SALE")
  SELECT 0
  USE (pth_inc+"s_sale_t") ALIAS SOURCE_T
  SCAN FOR CUS_CODE = sup_own
    cnt1 = cnt1+1
    @ 2, 37 SAY STR(cnt1,6)
    IF SEEK(DOC_FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM+STR(SUP_CODE), "S_SALE_T")
      LOOP
    ENDIF
    cnt2 = cnt2+1
    @ 2, 53 SAY STR(cnt2,6)
    SELECT S_SALE_T
    APPEND BLANK
    REPLACE DOC_NUM  WITH SOURCE_T.DOC_NUM,  ;
            DOC_DATE WITH SOURCE_T.DOC_DATE, ;
            DOC_FIRM WITH SOURCE_T.DOC_FIRM, ;
            REF_NUM  WITH SOURCE_T.REF_NUM,  ;
            REF_DATE WITH SOURCE_T.REF_DATE, ;
            SUP_CODE WITH SOURCE_T.SUP_CODE
    SELECT TMP_LIST
    SEEK SOURCE_T.DOC_FIRM+DTOS(SOURCE_T.DOC_DATE)+SOURCE_T.DOC_NUM
    SCAN REST WHILE SOURCE_T.DOC_FIRM = DOC_FIRM .AND. ;
                    SOURCE_T.DOC_DATE = DOC_DATE .AND. ;
                    SOURCE_T.DOC_NUM  = DOC_NUM
      SELECT S_SALE
      APPEND BLANK
      REPLACE DOC_NUM  WITH TMP_LIST.DOC_NUM,  ;
              DOC_DATE WITH TMP_LIST.DOC_DATE, ;
              DOC_FIRM WITH TMP_LIST.DOC_FIRM, ;
              SUP_CODE WITH SOURCE_T.SUP_CODE, ;
              CODE     WITH TMP_LIST.CODE,     ;
              QNT      WITH TMP_LIST.QNT,      ;
              PRICE_R  WITH TMP_LIST.PRICE_R,  ;
              PRICE_D  WITH TMP_LIST.PRICE_D
      SELECT TMP_LIST
    ENDSCAN
    SELECT SOURCE_T
  ENDSCAN
  USE
  SELECT TMP_LIST
  USE
  SELECT S_SALE_T
  USE
  SELECT S_SALE
  USE
  DELETE FILE (tmpo_path+"tmp_list.dbf")
  DELETE FILE (tmpo_path+"tmp_list.cdx")
ENDIF

IF File_o(pth_inc+"s_ryn.dbf") .AND. File_o(pth_inc+"s_ryn_t.dbf")
  @ 0, 0 SAY PADC("Накладные передачи на реализацию", WCOLS())
  DELETE FILE (tmpo_path+"tmp_list.dbf")
  DELETE FILE (tmpo_path+"tmp_list.cdx")
  USE (pth_inc+"s_ryn")
  cnt0 = RECCOUNT()
  @ 2, 20 SAY STR(cnt0,6)
  STORE 0 TO cnt1, cnt2
  COPY TO (tmpo_path+"TMP_LIST")
  USE (tmpo_path+"TMP_LIST") EXCLUSIVE
  INDEX ON DOC_FIRM+DTOS(DOC_DATE)+DOC_NUM TAG TMP
  SELECT 0
  USE (base_path+"S_RYN_T") ORDER TAG DOC_NUM ALIAS S_SALE_T
  SELECT 0
  USE (base_path+"S_RYN") ALIAS S_SALE
  SELECT 0
  USE (pth_inc+"S_RYN_T") ALIAS SOURCE_T
  SCAN FOR CUS_CODE = sup_own
    cnt1 = cnt1+1
    @ 2, 37 SAY STR(cnt1,6)
    IF SEEK(DOC_FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM+STR(SUP_CODE), "S_SALE_T")
      LOOP
    ENDIF
    cnt2 = cnt2+1
    @ 2, 53 SAY STR(cnt2,6)
    SELECT S_SALE_T
    APPEND BLANK
    REPLACE DOC_NUM  WITH SOURCE_T.DOC_NUM,  ;
            DOC_DATE WITH SOURCE_T.DOC_DATE, ;
            DOC_FIRM WITH SOURCE_T.DOC_FIRM, ;
            SUP_CODE WITH SOURCE_T.SUP_CODE
    SELECT TMP_LIST
    SEEK SOURCE_T.DOC_FIRM+DTOS(SOURCE_T.DOC_DATE)+SOURCE_T.DOC_NUM
    SCAN REST WHILE SOURCE_T.DOC_FIRM = DOC_FIRM .AND. ;
                    SOURCE_T.DOC_DATE = DOC_DATE .AND. ;
                    SOURCE_T.DOC_NUM  = DOC_NUM
      SELECT S_SALE
      APPEND BLANK
      REPLACE DOC_NUM  WITH TMP_LIST.DOC_NUM,  ;
              DOC_DATE WITH TMP_LIST.DOC_DATE, ;
              DOC_FIRM WITH TMP_LIST.DOC_FIRM, ;
              SUP_CODE WITH SOURCE_T.SUP_CODE, ;
              CODE     WITH TMP_LIST.CODE,     ;
              QNT      WITH TMP_LIST.QNT
      SELECT TMP_LIST
    ENDSCAN
    SELECT SOURCE_T
  ENDSCAN
  USE
  SELECT TMP_LIST
  USE
  SELECT S_SALE_T
  USE
  SELECT S_SALE
  USE
  DELETE FILE (tmpo_path+"tmp_list.dbf")
  DELETE FILE (tmpo_path+"tmp_list.cdx")
ENDIF

RELEASE WINDOW Read_Docs
SELECT (s_sav)

RETURN
