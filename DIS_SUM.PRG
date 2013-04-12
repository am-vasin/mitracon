*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа расчета суммы для скидки.                    ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Dis_Sum
PARAMETERS ord_lst, sum_dis, sum_spec, flg
*
*  ord_lst  - строка со списком заказов ( если пусто - текущий );
*  sum_dis  - сумма, подлежащая скидке ( без спеццен );
*  sum_spec - сумма, не подлежащая скидке ( спеццены );
*  flg      - результат прорсмотра:
*                - пустое - пробел - OK;
*                         - 0      - есть дефицит;
*                         - 1      - скидка уже посчитана;
*                         - 2      - продано!
*                         - 3      - подарок!
*

PRIVATE i_p, lst_len, n_wk
SELECT LIST_DIS
i_p = 1
IF EMPTY(ord_lst)
  lst_len = 7
  n_wk = STR(LIST_TIT.LST_NUM,6)
ELSE
  lst_len = LEN(ord_lst)
  n_wk = LEFT(ord_lst,6)
ENDIF

flg = ""
sum_dis  = 0
sum_spec = 0

DO WHILE i_p < lst_len
  = SEEK(arm+n_wk, "LIST_TIT")
  IF .NOT. EMPTY(LIST_TIT.SAL_NUM)
    flg = "2"
    RETURN
  ENDIF
  IF LIST_TIT.CLI_TYPE = 3
    flg = "3"
    RETURN
  ENDIF
  IF .NOT. EMPTY(LIST_TIT.IS_DIS)
    flg = IIF("1" $ flg, flg, flg+"1")
  ENDIF
  SEEK n_wk
  SCAN REST WHILE n_wk = LIST_DIS.LIST_NUM FOR CODE >= 0
    IF IIF(LIST_TIT.IS_USD, USD_PRICE # USD_DEF, SAL_PRICE # DEF_PRICE) .OR. .NOT. EMPTY(IS_OPT) .OR. .NOT. EMPTY(SUB_DIS.NODISC)
      IF EMPTY(IS_SPEC) .AND. EMPTY(IS_OPT) .AND. EMPTY(SUB_DIS.NODISC)
        flg = IIF("1" $ flg, flg, flg+"1")
      ENDIF
      sum_spec = sum_spec+QNT*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)
    ELSE
      sum_dis = sum_dis+QNT*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)
    ENDIF
  ENDSCAN
  IF EMPTY(ord_lst)
    EXIT
  ELSE
    i_p = i_p+7
    n_wk = SUBSTR(ord_lst,i_p,6)
  ENDIF
ENDDO

RETURN
