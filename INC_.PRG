*
*     Приход наличия
*
PROCEDURE Inc_
PARAMETER mt_
PRIVATE n_d, frm, tmpArr, tmpQ

USE DIV_STO ORDER TAG CODE
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE
SELECT 0
USE (base_path+"ACT_S")
SELECT 0
USE TMP_ACT
SET RELATION TO CODE INTO STOCK
SET RELATION TO CODE INTO DIV_STO ADDITIVE
frm = IIF(mt_, "MTK", "TEL")
n_d = Doc_Num("ACT_NUM", frm, arm, DATE())
n_d = STR(n_d, 8)
nn = 0
SCAN
  nn = nn+1
  IF nn % 100 = 0
    WAIT WIND STR(nn)+STR(reccount()) NOWAIT
  ENDIF
  tmpQ = IIF(mt_, DIV_STO.QNT_MT, DIV_STO.QNT_TE)
  IF tmpQ = 0
    LOOP
  ENDIF
  SCATTER TO tmpArr
  SELECT ACT_S
  APPEND FROM ARRAY tmpArr
  REPLACE FIRM     WITH frm,   ;
          DOC_NUM  WITH n_d,   ;
          DOC_DATE WITH DATE(),;
          QNT_INC  WITH tmpQ,  ;
          QNT_REP  WITH tmpQ,  ;
          QNT_REST WITH tmpQ,  ;
          REST_REP WITH tmpQ
  SELECT STOCK
  REPLACE QNT     WITH QNT+tmpQ, ;
          QNT_REP WITH QNT_REP+tmpQ
  SELECT TMP_ACT
ENDSCAN
USE (base_path+"ACT_S_T")
APPEND BLANK
REPLACE FIRM      WITH frm,      ;
        DOC_NUM   WITH n_d,      ;
        DOC_DATE  WITH DATE(),   ;
        DOC_TIME  WITH SECONDS(),;
        SUP_CODE  WITH 10,       ;
        FLG_BUY   WITH "R",      ;
        WHO_ACT   WITH user,     ;
        WHERE_ACT WITH arm

CLOSE DATABASES
