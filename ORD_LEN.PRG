PRIVATE cnt, cnt_pos, cnt_neg
USE (base_path+"ORD_D") ORDER TAG DOC_NUM
SELECT 0
USE (base_path+"ORD_T")
SCAN
  cnt = 0
  cnt_neg = 0
  cnt_pos = 0
  SELECT ORD_D
  SEEK ORD_T.LIST_NUM+STR(ORD_T.ORD_NUM,3)
  SCAN REST WHILE ORD_T.LIST_NUM = LIST_NUM .AND. ORD_T.ORD_NUM = ORD_NUM
    cnt = cnt+1
    IF QNT < 0
      cnt_neg = cnt_neg+1
    ELSE
      cnt_pos = cnt_pos+1
    ENDIF
  ENDSCAN
  SELECT ORD_T
  REPLACE SIZE_POS WITH cnt_pos, SIZE_NEG WITH cnt_neg
ENDSCAN

CLOSE DATABASES
RETURN
