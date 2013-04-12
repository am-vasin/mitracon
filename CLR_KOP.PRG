*************************************************************************
*    Файл Clr_Kop       Разработчик Андрей Васин          09.12.2011    *
*************************************************************************
*                                                                       *
*              Согласование денежных документов с товарным              *
*                                                                       *
*************************************************************************
PROCEDURE Clr_Kop
PRIVATE tot_pay, s_sav, delta, i
EXTERNAL ARRAY dates, numbs

IF .NOT. m.md_t $ " Ч"	&& Согласуем только наличные рубли
  RETURN
ENDIF
IF m.ord_cnt = 0		&& Деньги не приходовались
  RETURN
ENDIF

* PREVED!!!
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"INCMONEY") SHARED AGAIN ORDER TAG DOC_NUM ALIAS CP_Money
m.tot_pay = 0
FOR m.i = 1 TO m.ord_cnt
  SEEK LIST_TIT.FIRM+m.md_t+LEFT(DTOS(dates[m.i]),4)+numbs[m.i]
  m.tot_pay = m.tot_pay+CP_Money.MONEY_R
ENDFOR

USE (m.base_path+"SALE_TIT") SHARED AGAIN ORDER TAG DOC_NUM ALIAS CP_Sale IN 0
= SEEK(LIST_TIT.FIRM+LEFT(DTOS(m.d_d),4)+m.d_n, "CP_Sale")
m.delta = CP_Sale.TOTAL_R-m.tot_pay
IF .NOT. BETWEEN(ABS(m.delta), 0.01, 0.05)
  USE IN CP_Sale
  USE IN CP_Money
  SELECT (m.s_sav)
  RETURN
ENDIF

REPLACE MONEY_R WITH MONEY_R+m.delta
USE (m.base_path+"CUS_REST") SHARED AGAIN ORDER TAG CUS_CODE ALIAS CP_Rest
SEEK STR(CP_Sale.CUS_CODE)+CP_Sale.FIRM
REPLACE ACC_SUM WITH ACC_SUM+m.delta
USE IN CP_Sale
USE IN CP_Rest
SELECT (m.s_sav)
RETURN
