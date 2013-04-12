SET EXCLU OFF
SET DELE ON
USE DIFFS
pth_base = "Z:\STORE\NEW_STO\BASE\"
pth_shab = "Z:\STORE\SOURCE\NEW_BASE\"
SELECT 0
USE (pth_base+"DOC_FORM") ALIAS BASE
SELECT 0
USE (pth_shab+"DOC_FORM") ALIAS SHAB
SELECT DIFFS

SCAN
  SELECT SHAB
  LOCATE FOR DOC_NAME = DIFFS.DOC_NAME
  d0 = doc_h
  d1 = doc_f
  d2 = page_h
  d3 = page_f
  d4 = diction
  SCATTER TO df
  SELECT BASE
  IF DIFFS.DIFF_T = "A"
    APPEND BLANK
    GATHER FROM df
  ELSE
    LOCATE FOR DOC_NAME = SHAB.DOC_NAME
  ENDIF
  REPLACE doc_h WITH d0, ;
          doc_f WITH d1, ;
          page_h WITH d2,;
          page_f WITH d3,;
          diction WITH d4
  SELECT DIFFS
ENDSCAN

CLOSE DATA
