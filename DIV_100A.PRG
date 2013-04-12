*
*   Программа деноминации в базе данных
*
PROCEDURE Div_100A
SET EXCLU OFF
SET DELE ON
DO Set_Lwr
path_d = "d:\div_100\"
path_s = "z:\store\base\"

DIMENSION n_lst(27), f_lst(27, 3), rc_no(27)

rc_no( 1)    = 2590
n_lst( 1)    = "BILLS"
f_lst( 1, 2) = "LIST_SUM  "+ ;
               "BILL_SUM  "+ ;
               "PRC01     "+ ;
               "PRC02     "+ ;
               "PRC03     "+ ;
               "SUM01     "+ ;
               "SUM02     "+ ;
               "SUM03     "
f_lst( 1, 3) = "102"+ ;
               "102"+ ;
               "082"+ ;
               "082"+ ;
               "082"+ ;
               "102"+ ;
               "102"+ ;
               "102"

rc_no( 2)    = 11279
n_lst( 2)    = "KASSA_M"
f_lst( 2, 2) = "SUM_R     "+ ;
               "KURS_FL   "
f_lst( 2, 3) = "102"+ ;
               "063"

rc_no( 3)    = 0
n_lst( 3)    = "F_VIEW"
f_lst( 3, 2) = "SUM_R     "
f_lst( 3, 3) = "102"

rc_no( 4)    = 121
n_lst( 4)    = "CURS"
f_lst( 4, 2) = "CURS      "+ ;
               "CURS_B    "+ ;
               "CURS_N    "+ ;
               "CURS_MMVB "
f_lst( 4, 3) = "063"+ ;
               "063"+ ;
               "063"+ ;
               "063"

rc_no( 5)    = 35338
n_lst( 5)    = "INCMONEY"
f_lst( 5, 2) = "MONEY_R   "
f_lst( 5, 3) = "102"

rc_no( 6)    = 3881
n_lst( 6)    = "KASSA"
f_lst( 6, 2) = "R_TOTAL   "
f_lst( 6, 3) = "102"

rc_no( 7)    = 0
n_lst( 7)    = "KASSA_S"
f_lst( 7, 2) = "SUM_R_S   "+ ;
               "SUM_R     "
f_lst( 7, 3) = "132"+ ;
               "132"

rc_no( 8)    = 77540
n_lst( 8)    = "DEMAND"
f_lst( 8, 2) = "PRICE     "
f_lst( 8, 3) = "082"

rc_no( 9)    = 0
n_lst( 9)    = "RCV"
f_lst( 9, 2) = "PRICE_R   "
f_lst( 9, 3) = "082"

rc_no(10)    = 0
n_lst(10)    = "RCV_T"
f_lst(10, 2) = "TOTAL_R   "
f_lst(10, 3) = "102"

rc_no(11)    = 0
n_lst(11)    = "LIST_TIT"
f_lst(11, 2) = "R_TOTAL   "
f_lst(11, 3) = "102"

rc_no(12)    = 0
n_lst(12)    = "RYN_1_L"
f_lst(12, 2) = "PRICE_R   "
f_lst(12, 3) = "082"

rc_no(13)    = 35394
n_lst(13)    = "SALE_TIT"
f_lst(13, 2) = "TOTAL_R   "+ ;
               "KASSA     "
f_lst(13, 3) = "102"+ ;
               "102"

rc_no(14)    = 0
n_lst(14)    = "RYN_L"
f_lst(14, 2) = "PRICE_R   "
f_lst(14, 3) = "082"

rc_no(15)    = 0
n_lst(15)    = "ACCOUNT"
f_lst(15, 2) = "ACC_S_S   "+ ;
               "ACC_S0_S  "+ ;
               "ACC_SUM   "+ ;
               "ACC_SUM0  "+ ;
               "ACC_S_H   "+ ;
               "ACC_S0_H  "+ ;
               "CR_SUM    "+ ;
               "CR0_SUM   "+ ;
               "BUY_SUM   "
f_lst(15, 3) = "102"+ ;
               "102"+ ;
               "102"+ ;
               "102"+ ;
               "102"+ ;
               "102"+ ;
               "102"+ ;
               "102"+ ;
               "102"

rc_no(16)    = 0
n_lst(16)    = "ARM_LST"
f_lst(16, 2) = "S_KASSA   "+ ;
               "KASSA     "
f_lst(16, 3) = "102"+ ;
               "102"

rc_no(17)    = 0
n_lst(17)    = "MAN_REP"
f_lst(17, 2) = "RPRICE    "
f_lst(17, 3) = "102"

rc_no(18)    = 24893
n_lst(18)    = "ACT_S"
f_lst(18, 2) = "PRICE_R   "+ ;
               "NDS       "
f_lst(18, 3) = "082"+ ;
               "082"

rc_no(19)    = 2061
n_lst(19)    = "ACT_S_T"
f_lst(19, 2) = "TOTAL_0   "+ ;
               "TOTAL_NDS "+ ;
               "TOTAL_H   "
f_lst(19, 3) = "122"+ ;
               "122"+ ;
               "122"

rc_no(20)    = 602870
n_lst(20)    = "SALE"
f_lst(20, 2) = "PRICE_R_L "+ ;
               "PRICE_R   "+ ;
               "PRICE_ACT "
f_lst(20, 3) = "082"+ ;
               "082"+ ;
               "082"

rc_no(21)    = 0
n_lst(21)    = "WK_LIST"
f_lst(21, 2) = "PRICE     "
f_lst(21, 3) = "082"

rc_no(22)    = 0
n_lst(22)    = "CORR_LST"
f_lst(22, 2) = "PRICE     "
f_lst(22, 3) = "082"

rc_no(23)    = 0
n_lst(23)    = "STOCK"
f_lst(23, 2) = "PRICE_B_R "+ ;
               "PRICE_R   "
f_lst(23, 3) = "082"+ ;
               "082"

rc_no(24)    = 0
n_lst(24)    = "INC_LIST"
f_lst(24, 2) = "PRICE_0   "+ ;
               "NDS       "+ ;
               "PRICE     "
f_lst(24, 3) = "082"+ ;
               "082"+ ;
               "082"

rc_no(25)    = 4950
n_lst(25)    = "REFUSAL"
f_lst(25, 2) = "PRICE     "
f_lst(25, 3) = "082"

rc_no(26)    = 33505
n_lst(26)    = "BILL_F"
f_lst(26, 2) = "TOTAL_0   "+ ;
               "TOTAL_NDS "+ ;
               "TOTAL_R   "
f_lst(26, 3) = "102"+ ;
               "102"+ ;
               "102"

rc_no(27)    = 0
n_lst(27)    = "LIST_DET"
f_lst(27, 2) = "DEF_PRICE "+ ;
               "SAL_PRICE "
f_lst(27, 3) = "082"+ ;
               "082"

n_fil = ADIR(l_fil, path_s+"*.DBF")

FOR i = 1 TO n_fil
  CLEAR
  @ 1, 10 SAY "Файл "+l_fil(i,1)+ STR(i,20)+"/"+ALLTRIM(STR(n_fil))
  p = 0
  FOR j = 1 TO ALEN(n_lst)
    IF l_fil(i,1) == ALLTRIM(n_lst(j))+".DBF"
      p = j
      EXIT
    ENDIF
  ENDFOR
  USE (path_s+l_fil(i,1)) ALIAS SSS
*   ВНИМАНИЕ! ПРИ ОКОНЧАТЕЛЬНОЙ ОБРАБОТКЕ ВКЛЮЧИТЬ В ПРОГРАММУ!  
  IF p = 0
    @ 2, 10 SAY "Копируем без изменений!"
*    COPY TO (path_d+l_fil(i,1)) WITH CDX
    LOOP
  ENDIF

  @ 2, 10 SAY "Копируем с модификацией структуры!"
  = AFIELDS(stru_)
  nn = LEN(f_lst(p, 2))/10
  DIMENSION pp(nn)
  pp = 0
  FOR j = 1 TO nn   && перебор модифицируемых полей
    FOR j0 = 1 TO ALEN(stru_,1)
      IF stru_(j0,1) == ALLTRIM(SUBSTR(f_lst(p, 2), (j-1)*10+1, 10))
        stru_(j0, 3) = VAL(SUBSTR(f_lst(p, 3), (j-1)*3+1, 2))
        stru_(j0, 4) = VAL(SUBSTR(f_lst(p, 3), (j-1)*3+3, 1))
        pp(j) = j0
        EXIT
      ENDIF
    ENDFOR
    IF pp(j) = 0
      ? "Не найдено поле "+SUBSTR(f_lst(p, 2), (j-1)*10+1, 10)+"!"
      SUSPEND
    ENDIF
  ENDFOR
  SELECT 0
  CREATE TABLE (path_d+l_fil(i,1)) FROM ARRAY stru_
  USE (path_d+l_fil(i,1)) ALIAS DDD EXCLU
  SELECT SSS
  j = 1
  cdx_t = TAG(j)
  DO WHILE .NOT. EMPTY(cdx_t)
    cdx_f = SYS(2021,j)
    cdx_v = SYS(14,j)
    SELECT DDD
    st = "INDEX ON "+cdx_v+IIF(.NOT. EMPTY(cdx_f)," FOR "+cdx_f,"")+  ;
         " TAG "+cdx_t+" ADDITIVE"
    &st
    SELECT SSS
    j = j+1
    cdx_t = TAG(j)
  ENDDO
  cnt0 = 0
  cnt_h = RECCOUNT()
  lin0 = SPACE(50)
  lin1 = ""
  @ 24, 0 say lin0+"!"
  SCAN WHILE RECNO() <= rc_no(p)
    cnt0 = cnt0+1
    @ 20, 10 SAY STR(RECNO())+str(rc_no(p))
*    lin1 = REPLICATE("*", ROUND(cnt0/cnt_h*50,0))
    IF lin1 # lin0
      lin0 = lin1
      @ 24, 0 say lin0
    ENDIF
    SCATTER TO buff
    FOR k = 1 TO nn
      k00 = pp(k)
      buff(k00) = buff(k00)/1000
    ENDFOR
    SELECT DDD
    APPEND FROM ARRAY buff
    SELECT SSS
  ENDSCAN
  SELECT DDD
  USE
  SELECT SSS
  USE
ENDFOR  
