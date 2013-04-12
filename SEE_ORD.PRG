*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                  Программа просмотра списка.                          ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE See_Ord
PARAMETERS lc, sum_list, sw_def, sw_due, sw_ord, sw_dis, sw_spec, ;
           sl_mark, st_mark
*
*  lc       - строковый номер списка;
*  sum_list - сумма;
*  sw_def   - признак наличия дефицита;
*  sw_due   - признак наличия долга;
*  sw_ord   - признак наличия брони;
*  sw_dis   - признак наличия скидки;
*  sw_spec  - признак наличия спеццен.
*
PRIVATE q_pres, cnt, s_sav
STORE .F. TO sw_def, sw_due, sw_ord, sw_dis, sw_spec
IF TYPE("alt_val") # "L"
  PRIVATE alt_val
  alt_val = .F.
ENDIF
s_sav = SELECT()
SELECT 0
USE (base_path+"LIST_TIT") AGAIN ALIAS LT_1914 ORDER TAG LST_NUM
SEEK VAL(ALLTRIM(lc))
SELECT 0
USE (base_path+"STOCK") AGAIN ALIAS ST_1914 ORDER TAG CODE
SELECT 0
USE (base_path+"LIST_DET") AGAIN ALIAS LD_1914 ORDER TAG LIST
SET RELATION TO CODE INTO ST_1914
sum_list = 0
cnt      = 0
SEEK lc
SCAN REST WHILE LIST_NUM = lc
  IF CODE < 0
    LOOP
  ENDIF
  cnt = cnt+1
  IF FOUND("ST_1914")
    IF ST_1914.NAME # LD_1914.NAME .OR.  ;
       ST_1914.PREFIX # LD_1914.PREFIX .OR. ;
       ST_1914.PRODUCER # LD_1914.PRODUCER .OR. ;
       ST_1914.ADDRESS # LD_1914.ADDRESS
      REPLACE LD_1914.NAME WITH ST_1914.NAME,  ;
              LD_1914.PREFIX WITH ST_1914.PREFIX,  ;
              LD_1914.PRODUCER WITH ST_1914.PRODUCER,  ;
              LD_1914.PRO_CODE WITH ST_1914.PRO_CODE,  ;
              LD_1914.ADDRESS WITH ST_1914.ADDRESS
    ENDIF
  ENDIF
  IF LT_1914.IS_USD     && Долларовый список
    IF alt_val           &&   ...показать в рублях
      sum_list = sum_list+ROUND(QNT*Clc_Rub(USD_PRICE),2)
    ELSE                 &&    ...показать в долларах
      sum_list = sum_list+QNT*USD_PRICE
    ENDIF
  ELSE                   && Рублевый список
    IF alt_val           &&    ...показать в долларах
      sum_list = sum_list+QNT*ROUND(SAL_PRICE/kurs,4)
    ELSE                 &&   ...показать в рублях
      sum_list = sum_list+ROUND(QNT*SAL_PRICE,2)
    ENDIF
  ENDIF
  q_pres = IIF(FOUND("ST_1914"), ST_1914.QNT, 0)
  q_pres = MAX(q_pres, 0)
  IF q_pres-QNT+QNT_REQ < 0 .AND. EMPTY(IS_DEF)
    sw_def = .T.
  ENDIF
  IF q_pres-QNT+QNT_REQ >= 0 .AND. .NOT. EMPTY(IS_DEF)
    REPLACE IS_DEF WITH " "
  ENDIF
  IF QNT_DUE # 0
    sw_due = .T.
  ENDIF
  IF QNT_REQ # 0
    sw_ord = .T.
  ENDIF
  IF .NOT. EMPTY(IS_SPEC)
    sw_spec = .T.
  ENDIF
  IF CODE # 0 .AND. EMPTY(IS_SPEC) .AND. IIF(LT_1914.IS_USD, USD_PRICE # USD_DEF, SAL_PRICE # Round_6(DEF_PRICE))
    sw_dis = .T.
  ENDIF
ENDSCAN

IF TYPE("cnt_list") = "N"
  cnt_list = cnt
ENDIF

SELECT 0
USE (base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS WWW000 AGAIN
sl_mark = .F.
st_mark = .F.
SEEK lc
SCAN REST WHILE LIST_NUM = lc
  IF EMPTY(WHO)
    sl_mark = .T.
  ENDIF
  IF EMPTY(STO_MARK)
    st_mark = .T.
  ENDIF
ENDSCAN
USE
SELECT LD_1914
USE
SELECT LT_1914
USE
SELECT ST_1914
USE
SELECT (s_sav)
RETURN
