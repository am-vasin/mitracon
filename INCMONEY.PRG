*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура IncMoney     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Приход денег на депозит.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.01.1999 ──┘
PROCEDURE IncMoney
PARAMETERS bill_frm_p, bill_num_p, bill_dat_p, flags

PRIVATE d_year, typeList, tmpPic, tmpPay
PRIVATE pay_t        && Тип депозита
m.tmpPay = .F.
IF TYPE("m.pay_bill") = "L"
  m.tmpPay = m.pay_bill
ENDIF

pay_t = 1

IF "Req" $ flags
  typeList = "_1_3_4_"
ELSE
  IF "_" $ flags
    typeList = ""
    IF "_1_" $ flags
      typeList = typeList + "_1_"
    ENDIF
    IF "_2_" $ flags
      typeList = typeList + "_2_"
    ENDIF
    IF "_3_" $ flags
      typeList = typeList + "_3_"
    ENDIF
    IF "_4_" $ flags
      typeList = typeList + "_4_"
    ENDIF
    IF "_5_" $ flags
      typeList = typeList + "_5_"
    ENDIF
    IF "_6_" $ flags
      typeList = typeList + "_6_"
    ENDIF
  ELSE
    typeList = "_1_2_3_4_5_6_"
  ENDIF
ENDIF
tmpPic = "@*HR "
tmpPic = tmpPic + IIF("_1_" $ typeList, "Нал. ;", "\\Нал. ;") + ;
                  IIF("_2_" $ typeList, "Б/Н ;", "\\Б/Н ;") + ;
                  IIF("_3_" $ typeList, "Индекс. ;", "\\Индекс. ;") + ;
                  IIF("_4_" $ typeList, "Чек ;", "\\Чек ;") + ;
                  IIF("_5_" $ typeList, "Сбербанк ;", "\\Сбербанк ;") + ;
                  IIF("_6_" $ typeList, "Б/Н1 ;", "\\Б/Н1 ;")
tmpPic = LEFT(tmpPic, LEN(tmpPic)-1)
IF .NOT. "_1_" $ typeList .AND. pay_t = 1
  pay_t = 2
ENDIF
IF .NOT. "_2_" $ typeList .AND. pay_t = 2
  pay_t = 3
ENDIF
IF .NOT. "_3_" $ typeList .AND. pay_t = 3
  pay_t = 4
ENDIF
IF .NOT. "_4_" $ typeList .AND. pay_t = 4
  pay_t = 5
ENDIF
IF .NOT. "_5_" $ typeList .AND. pay_t = 5
  pay_t = 6
ENDIF

d_year = "    "
IF TYPE("bill_dat_p") = "D"
  d_year = LEFT(DTOS(bill_dat_p),4)
ENDIF
IF TYPE("bill_dat_p") = "N"
  d_year = STR(bill_dat_p,4)
ENDIF
IF TYPE("bill_dat_p") = "C"
  d_year = bill_dat_p
ENDIF

PRIVATE is_req
is_req = "Req" $ flags

IF user = 0
  PRIVATE mss_000
  DIMENSION mss_000(3)
  mss_000(1) = ""
  mss_000(2) = CHR(0)+"Андрюха! Не делай этого, тебе нельзя..."
  mss_000(3) = ""
  DO Out_Mess WITH 7, "mss_000"
  RETURN
ENDIF
*
*  Переменные состояния для навигации
*

PRIVATE stat_type     && Тип навигации: 0 - нестандартный;
                                        1 - бланк;
                                        2 - BROWSE - таблица;
                                        3 - BROWSE - список.
PRIVATE what_do       && Имя режима.
PRIVATE menu_name     && Имя асинхронного меню.
PRIVATE last_mouse    && Время последнего нажатия звериной кнопки.
PRIVATE win_name      && Имя окна ( окон для BROWSE ).

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌────────────────────────────────────────────────────────────────────┐
*│ ┌─ Тип оплаты ───────────────────────────────────────────────────┐ │
*│ │ ( ) Нал.  ( ) Б/Н  ( ) Индекс.  ( ) Чек  ( ) Сбербанк  ( ) Б/Н1│ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│ ┌─ Счет ─────────────────────────────────────────────────────────┐ │
*│ │       Предприятие ....:....!....:. Банк ....:....!....:.       │ │
*│ │  N ....:....! от ДД.ММ.ГГГГ Сумма по счету 999 999 999.99 Руб  │ │
*│ │  Плюс налог 99.9%  999 999 999.99    Итого 999 999 999.99 Руб  │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│ ┌────────────────────────────────────────────────────────────────┐ │
*│ │ Кому ... ................ (F1) Через [ ] Банк ....:....!....:. │ │
*│ │ Ост. 999999999.99(999999999.99)руб 999999999.99(999999999.99)$ │ │
*│ │ От кого [ ] ....:....!....:....!....:....!....:....!....:....! │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│     Платежное поручение N ....:....!....:. от ДД.MM.ГГГГ (F1)      │
*│ ┌─ Оплата ───────────────────────────────────────────────────────┐ │
*│ │ 999 999 999.99 руб (F1) по курсу 9999.99 (999 999 999.99 USD)  │ │
*│ │ 999 999 999.99 USD (F1) по курсу 9999.99 (999 999 999.99 руб)  │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│  Итого налог 99 999 999.9999 руб на депозит 99 999 999.9999 руб    │
*│                 Через кого ....:....!....:....!..                  │
*│            Примечание ....:....!....:....!....:....!..             │
*│                  < OK Ctrl-W > < Отказаться Esc >                  │
*└────────────────────────────────────────────────────────────────────┘

*┌────────────────────────────────────────────────────────────────────┐
*│ ┌─ Тип оплаты ───────────────────────────────────────────────────┐ │
*│ │ ( ) Нал.  ( ) Б/Н  ( ) Индекс.  ( ) Чек  ( ) Сбербанк  ( ) Б/Н1│ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│ ┌─ Счет ─────────────────────────────────────────────────────────┐ │
*│ │       Предприятие ....:....!....:. Банк ....:....!....:.       │ │
*│ │  N ....:....! от ДД.ММ.ГГГГ Сумма по счету 999 999 999.99 Руб  │ │
*│ │  Плюс налог 99.9%  999 999 999.99    Итого 999 999 999.99 Руб  │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│ ┌────────────────────────────────────────────────────────────────┐ │
*│ │ Кому ... ................ (F1) Через [ ] Банк ....:....!....:. │ │
*│ │ Ост. 999999999.99(999999999.99)руб 999999999.99(999999999.99)$ │ │
*│ │ От кого [ ] ....:....!....:....!....:....!....:....!....:....! │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│П/П ....:....!....:./ДД.MM.ГГГГ (F1) [ ] Банк ....:....!....:....!..│
*│ ┌─ Оплата ───────────────────────────────────────────────────────┐ │
*│ │ 999 999 999.99 руб (F1) по курсу 9999.99 (999 999 999.99 USD)  │ │
*│ │ 999 999 999.99 USD (F1) по курсу 9999.99 (999 999 999.99 руб)  │ │
*│ └────────────────────────────────────────────────────────────────┘ │
*│  Итого налог 99 999 999.9999 руб на депозит 99 999 999.9999 руб    │
*│                 Через кого ....:....!....:....!..                  │
*│            Примечание ....:....!....:....!....:....!..             │
*│                  < OK Ctrl-W > < Отказаться Esc >                  │
*└────────────────────────────────────────────────────────────────────┘

PRIVATE ex, s_sav, v_sav, q_usd, q_rub
*USE (m.base_path+"CUS_BIL") SHARED AGAIN ORDER TAG CUS_CODE ALIAS TMP_BANK IN 0
*IF SEEK()
ex = 1
v_sav = 0
DIMENSION q_usd(6), q_rub(9)
q_rub = 0
q_usd = 0

PRIVATE nt           && Примечание
PRIVATE b_firm       && Предприятие по счету
PRIVATE b_bank       && Банк по счету
PRIVATE b_num        && Номер счета
PRIVATE b_date       && Дата счета
PRIVATE bill_s       && Сумма по счету
PRIVATE n_pers       && Процент налога
PRIVATE bill_n       && Налог по счету
PRIVATE bill_h       && К оплате по счету
PRIVATE c_code       && Код клиента
PRIVATE c_old        && Флажок "старый клиент"
PRIVATE c_name       && Имя клиента
PRIVATE e_doc_n      && Номер платежного поручения
PRIVATE e_doc_d      && Дата прихода денег на расчетный счет
PRIVATE sum_r_       && Получено рублей
PRIVATE kurs_r_d     && Курс перевода в доллары
PRIVATE sum_r_d      && Долларовый эквивалент
PRIVATE sum_d_       && Получено долларов
PRIVATE kurs_d_r     && Курс перевода в рубли
PRIVATE sum_d_r      && Рублевый эквивалент
PRIVATE face         && Через кого
PRIVATE res_n        && Итого налог
PRIVATE res_dep      && Итого на депозит
PRIVATE tmpBank, swBank, bankName
m.tmpBank = 0
m.swBank  = .F.
m.bankName = SPACE(22)
PRIVATE f_c000, f_n000, b_c000, b_n000, b_sw000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
b_firm   = SPACE(16)
b_bank   = SPACE(16)
b_num    = SPACE(10)
b_date   = SPACE(10)
nt       = SPACE(32)
bill_s   = 0
c_code   = 0
n_pers   = G_Nalog(def_firm)
bill_n   = 0
bill_h   = 0
c_old    = .F.
c_name   = SPACE(50)
e_doc_n  = SPACE(20)
e_doc_d  = DATE()
sum_r_   = 0
kurs_r_d = kurs
sum_r_d  = 0
sum_d_   = 0
kurs_d_r = kurs
sum_d_r  = 0
res_n    = 0
res_dep  = 0
face     = SPACE(22)

f_c000 = def_firm
b_c000 = def_bank
b_sw000=.F.
f_c_old= def_firm
f_n000 = SPACE(16)

s_sav = SELECT()
SELECT 0

IF .NOT. EMPTY(bill_num_p)    && Оплата по счету
  DO Fill_0 WITH bill_frm_p, bill_num_p, bill_dat_p
  nt = LEFT(c_name, LEN(nt))
ENDIF

USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS _9121 AGAIN
IF .NOT. EMPTY(f_c000)
  IF SEEK(f_c000)
    f_n000 = B_NAME
    b_firm = B_NAME
  ENDIF
ENDIF
USE (base_path+"BANKS") ORDER TAG BANK ALIAS _9121 AGAIN
b_n000 = SPACE(16)
IF .NOT. EMPTY(b_c000)
  IF SEEK(f_c000+b_c000)
    b_n000 = B_NAME
    b_bank = B_NAME
  ENDIF
ENDIF
USE

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 25, 72, "Приход денег на депозит"
DO Sun_Bord WITH  2,  3,  4, 68, " Тип оплаты "
DO Sun_Bord WITH  5,  3,  9, 68, IIF(is_req, "Требование", " Счет ")
DO Sun_Bord WITH 10,  3, 14, 68
DO Sun_Bord WITH 16,  3, 19, 68, " Оплата "
DO Draw_V
@  7, 63 SAY IIF(INLIST(pay_t, 3, 6), "USD", "руб")
@  8, 63 SAY IIF(INLIST(pay_t, 3, 6), "USD", "руб")
@ 12,  5 SAY "Ост.             (            )руб             (            )$"
@ 15, 34 SAY "(F1)"
IF .NOT. m.pay_bill
  @ 19, 18 SAY "Авансовые счета-фактуры не создаются" COLOR SCHEME 7
ENDIF
DO ShowRest
*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 13,  5 SAY "От кого" GET c_old PICTURE "@*C " VALID Ch_Cus()
@ 13, 17 GET c_name WHEN .NOT. c_old .AND. .NOT. "Spec" $ flags
@ 15,  2 SAY "П/П" GET e_doc_n SIZE 1, 16
@ 15, 22 SAY "/" 
@ 15, 23 GET e_doc_d WHEN Set_F1(1) VALID V_E_Date()
@ 15, 39 GET m.swBank PICTURE "@*C Банк" VALID Ch_CBank()
@ 15, 48 GET m.bankName WHEN .F.
@ 17,  5 GET sum_r_ PICTURE "@Z 999 999 999.99"   ;
                                 WHEN Set_F1(2) VALID V_Sum_R()
@ 17, 20 SAY "руб (F1) по курсу" GET kurs_r_d PICTURE "@Z 999.999"  ;
                                 WHEN Set_F1(3) VALID V_Kurs1()
@ 17, 47 GET sum_r_d PICTURE "@Z 9 999 999.9999"   ;
                                 WHEN Set_F1(4) VALID V_Sum_RD()
@ 18,  5 GET sum_d_ PICTURE "@Z 9 999 999.9999"  ;
                                 WHEN Set_F1(5) VALID V_Sum_D()
@ 18, 20 SAY "USD (F1) по курсу" GET kurs_d_r PICTURE "@Z 999.999"  ;
                                 WHEN Set_F1(6) VALID V_Kurs2()
@ 18, 47 GET sum_d_r PICTURE "@Z 999 999 999.99"  ;
                                 WHEN Set_F1(7) VALID V_Sum_DR()
@ 17, 46 SAY "("
@ 17, 62 SAY "USD)"
@ 18, 46 SAY "("
@ 18, 62 SAY "руб)"
@ 20,  4 SAY "Итого налог" GET res_n PICTURE "@Z 99 999 999.9999" DISABLE
@ 20, 37 SAY "на депозит" GET res_dep PICTURE "@Z 99 999 999.9999" WHEN Sav_Dep() ;
                           VALID Clc_Nlg()
@ 21, 19 SAY "Через кого" GET face WHEN INLIST(pay_t, 1, 3)
@ WROWS()-3, 14 SAY "Примечание" GET nt WHEN (sys_char = "A" .OR. sale_mode = "Mitin")
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

@  3,  4 GET pay_t PICTURE tmpPic VALID Ch_Type()
*IF is_req
*  @  3,  9 GET pay_t PICTURE "@*HR Нал. ;\\Б/Н ;Индекс. ;Чек ;\\Сбербанк" VALID Ch_Type()
*ELSE
*  @  3,  9 GET pay_t PICTURE "@*HR Нал. ;Б/Н ;Индекс. ;Чек ;Сбербанк" VALID Ch_Type()
*ENDIF
@  6, 11 SAY "Предприятие" GET b_firm DISABLE
@  6, 40 SAY "Банк" GET b_bank DISABLE
@  7,  6 SAY "N" GET b_num DISABLE
@  7, 19 SAY "от" GET b_date DISABLE
@  7, 33 SAY IIF(is_req, "Сумма по треб.", "Сумма по счету")  ;
                    GET bill_s PICTURE "@Z 999 999 999.99" DISABLE
@  8,  6 SAY "Плюс налог" GET n_pers PICTURE "@Z 99.9" DISABLE
@  8, 21 SAY "% " GET bill_n PICTURE "@Z 999 999 999.99" DISABLE
@  8, 42 SAY "Итого" GET bill_h PICTURE "@Z 999 999 999.99" DISABLE
@ 11,  5 SAY "Кому"
@ 11, 36 SAY "Через"
DO Get_Firm WITH 11, 10, enab_firm, "Change_F"
DO Get_Bank WITH 11, 42, enab_bank

READ CYCLE VALID Tst_Blank()

RELEASE WINDOW (win_name)
IF ex = 1 .AND. res_n+res_dep # 0
  *
  * Отрабатываем бланк
  DO Corr_Base
ENDIF
*--------------------------------------------------------------------------

POP KEY
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_V       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Формирование надписей о валюте.                     │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.01.1999 ──┘
PROCEDURE Draw_V

PRIVATE w
w = IIF(INLIST(pay_t, 3, 6), "USD", "руб")
@ 20, 32 SAY w
@ 20, 64 SAY w
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Clc_Tot      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Пересчет итога.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE Clc_Tot
PRIVATE w
w        = IIF(INLIST(pay_t, 3, 6), sum_d_+sum_r_d, sum_r_+sum_d_r)
res_n    = ROUND(w/(100+n_pers)*n_pers,IIF(INLIST(pay_t, 3, 6), 4, 2))
res_dep  = w-res_n
SHOW GET res_n
SHOW GET res_dep
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Fill_0       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│            Начальное заполнение бланка на основании счета.             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE Fill_0
PARAMETERS bill_frm_p, bill_num_p, bill_dat_p, pay_tmp

PRIVATE d_year, w

d_year = "    "
IF TYPE("bill_dat_p") = "D"
  d_year = LEFT(DTOS(bill_dat_p),4)
ENDIF
IF TYPE("bill_dat_p") = "N"
  d_year = STR(bill_dat_p,4)
ENDIF
IF TYPE("bill_dat_p") = "C"
  d_year = bill_dat_p
ENDIF

IF is_req
  USE (base_path+"BUHG_REQ") ORDER TAG DOC_NUM ALIAS B_9122 AGAIN
  IF .NOT. SEEK(d_year+bill_num_p)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Требование не найдено!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    RETURN
  ENDIF
  IF "_"+ALLTRIM(STR(DEP_TYPE))+"_" $ typeList
    pay_t    = DEP_TYPE
  ENDIF
  b_num    = ALLTRIM(DOC_NUM)
  b_date   = DTOC(DOC_DATE)
  bill_s   = USD_INC_R+R_INC
  n_pers   = G_Nalog(FIRM)
  bill_n   = ROUND(bill_s*n_pers/100,2)
  bill_h   = bill_s+bill_n
  c_code   = CUS_CODE
  DO DefBank WITH c_code
  c_old    = CUS_CODE # 0
  IF c_old
    SELECT 0
    USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS C_9122 AGAIN
    SEEK c_code
    c_name = CUS_NAME
    USE
    SELECT B_9122
  ELSE
    c_name   = SPACE(50)
  ENDIF

  sum_r_   = bill_h-USD_INC_R
  sum_r_d  = ROUND(sum_r_/kurs_r_d,4)
  sum_d_   = USD_INC
  sum_d_r  = USD_INC_R
  IF sum_d_ > 0
    kurs_d_r = ROUND(sum_d_r/sum_d_,4)
  ENDIF
  w        = sum_d_+sum_r_d
  res_n    = bill_n
  res_dep  = bill_s

  f_c000 = FIRM
  b_c000 = "   "
  f_c_old= FIRM
  USE
ELSE
  USE (base_path+"BILLS") ORDER TAG DOC_NUM ALIAS B_9122 AGAIN
  IF .NOT. SEEK(bill_frm_p+d_year+bill_num_p)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Счет не найден!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    RETURN
  ENDIF

  pay_tmp  = IIF(VALUTA = "$", 3, 2)
  IF "_"+STR(m.pay_tmp, 1)+"_" $ m.typeList
    m.pay_t = m.pay_tmp
  ENDIF
  b_num    = PADR(LEFT(DOC_NUM,4)+"-"+ALLTRIM(RIGHT(DOC_NUM,6)),10)
  b_date   = DTOC(DOC_DATE)
  bill_s   = IIF(VALUTA = "$", BILL_SUM, ROUND(BILL_SUM,2))
  n_pers   = 0
  bill_n   = 0
  bill_h   = bill_s
  c_code   = CUS_CODE
  DO DefBank WITH c_code
  c_old    = CUS_CODE # 0
  IF c_old
    SELECT 0
    USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS C_9122 AGAIN
    SEEK c_code
    c_name = CUS_NAME
    USE
    SELECT B_9122
  ELSE
    c_name   = CUS_NAME
  ENDIF

  sum_r_   = IIF(.NOT. INLIST(pay_t, 3, 6), ROUND(BILL_SUM,2), 0)
  sum_r_d  = ROUND(sum_r_/kurs_r_d, 4)
  sum_d_   = IIF(INLIST(pay_t, 3, 6), BILL_SUM, 0)
  sum_d_r  = ROUND(sum_d_*kurs_r_d,2)
  w        = IIF(.NOT. INLIST(pay_t, 3, 6), sum_r_+sum_d_r, sum_d_+sum_r_d)
  res_n    = ROUND(w/(100+n_pers)*n_pers,IIF(INLIST(pay_t, 3, 6), 4, 2))
  res_dep  = w-res_n

  f_c000 = FIRM
  b_c000 = BANK
  b_sw000=.F.
  f_c_old= FIRM
  USE
ENDIF
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Type      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Смена типа оплаты.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE Ch_Type

IF INLIST(pay_t, 3, 6) .AND. (sys_char # "A" .AND. sale_mode # "Mitin") .AND. sum_r_ # 0 .AND. ex # 2
  IF "_1_" $ typeList
    pay_t    = 1
  ENDIF
  SHOW GET pay_t
ENDIF

IF .NOT. INLIST(pay_t, 3, 6) .AND. (sys_char # "A" .AND. sale_mode # "Mitin") .AND. sum_d_ # 0 .AND. ex # 2
  IF "_3_" $ typeList
    pay_t = 3
  ENDIF
  SHOW GET pay_t
ENDIF

n_pers = G_Nalog(f_c000)
SHOW GET n_pers
bill_n = ROUND(bill_s*n_pers/100, 2)
SHOW GET bill_n
bill_h = bill_s+bill_n
SHOW GET bill_h
DO Clc_Tot
DO Draw_V
IF INLIST(pay_t, 2, 4, 5, 6)
  face = SPACE(LEN(face))
  SHOW GET face
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Cus       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Выбор клиента из списка.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE Ch_Cus
PRIVATE sw, s_sav, ord_sav, is_open, r_sav, mss, win_name

IF ex = 2
  RETURN .T.
ENDIF

IF "Spec" $ flags
  win_name = PROGRAM()
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_9531 AGAIN
  PRIVATE cc_000, cn_000, cp_000, c_000
  c_000 = 0
  cp_000 = 1
  SCAN WHILE CUS_CODE < 0
    c_000 = c_000+1
    DIMENSION cc_000(c_000), cn_000(c_000)
    cc_000(c_000) = CUS_CODE
    cn_000(c_000) = CUS_NAME
    IF c_code = cc_000(c_000)
      cp_000 = c_000
    ENDIF
  ENDSCAN
  USE
  IF c_000 = 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Вы не создали ни одного спецклиента!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    DO D_Win_N WITH MIN(WROWS("")-15, ALEN(cn_000)+3), LEN(cn_000(1))+4, "Спецклиенты"
    DO WHILE .T.
      cp_000 = MAX(1, cp_000)
      cp_000 = MIN(c_000, cp_000)
      @ 1, 1 MENU cn_000, c_000, WROWS()-3
      READ MENU TO cp_000
      IF READKEY() = 15
        c_code = cc_000(cp_000)
        c_name = cn_000(cp_000)
        EXIT
      ENDIF
      IF READKEY() = 12
        EXIT
      ENDIF
    ENDDO
    RELEASE WINDOW (win_name)
  ENDIF
  c_old = .F.
  SHOW GET c_old
  SHOW GET c_name
ELSE
  IF .NOT. c_old
    c_code = 0
    DO DefBank WITH c_code
    SHOW GET m.bankName
    c_name   = SPACE(50)
    SHOW GET c_name
    ACTIVATE WINDOW INCMONEY
    n_pers = G_Nalog(f_c000)
    SHOW GET n_pers
    bill_n = ROUND(bill_s*n_pers/100, 2)
    SHOW GET bill_n
    bill_h = bill_s+bill_n
    SHOW GET bill_h
    DO Clc_Tot
    DO Draw_V
    DO ShowRest
    RETURN .T.
  ENDIF
  s_sav = SELECT()
  is_open = USED("ACCOUNT")
  IF is_open
    SELECT ACCOUNT
    r_sav = RECNO()
    ord_sav = TAG()
    SET ORDER TO TAG CUS_NAME
    GO TOP
  ELSE
    USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME AGAIN
  ENDIF

  sw = Custs ("", .T., .F., .T., pl_code)
  IF .NOT. sw
    c_old = .F.
    SHOW GET c_old
  ELSE
    IF c_code # CUS_CODE
      DO DefBank WITH CUS_CODE
      SHOW GET m.bankName
    ENDIF
    c_code = CUS_CODE
    c_name = CUS_NAME
    IF (sys_char = "A" .OR. sale_mode = "Mitin")
      nt     = PADR(ALLTRIM(c_name),32)
      SHOW GET nt
    ENDIF
    SHOW GET c_name
  ENDIF

  IF is_open
    SET ORDER TO TAG (ord_sav)
    IF BETWEEN(r_sav, 1, RECCOUNT())
      GO r_sav
    ENDIF
  ELSE
    USE
  ENDIF
ENDIF
SELECT (s_sav)
ACTIVATE WINDOW INCMONEY
n_pers = G_Nalog(f_c000)
SHOW GET n_pers
bill_n = ROUND(bill_s*n_pers/100, 2)
SHOW GET bill_n
bill_h = bill_s+bill_n
SHOW GET bill_h
DO Clc_Tot
DO Draw_V
IF INLIST(pay_t, 2, 4, 5, 6)
  face = SPACE(LEN(face))
  SHOW GET face
ENDIF
DO ShowRest

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_F1       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│      Установка обработчика F1 и запоминание начального значения.       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE Set_F1
PARAMETERS n_field

DO CASE
CASE n_field = 1   && Дата платежного поручения
  ON KEY LABEL F1 DO Ch_Date
CASE n_field = 2   && Прием рублей
  IF INLIST(pay_t, 3, 6) .AND. (sys_char # "A" .AND. sale_mode # "Mitin")
    RETURN .F.
  ENDIF
  ON KEY LABEL F1 DO C_Cup WITH 1
  v_sav = sum_r_
  q_rub = 0
CASE n_field = 3   && Курс конвертации рублей
  IF INLIST(pay_t, 3, 6) .AND. (sys_char # "A" .AND. sale_mode # "Mitin")
    RETURN .F.
  ENDIF
  v_sav = kurs_r_d
CASE n_field = 4   && Долларовый эквивалент принятых рублей
  IF INLIST(pay_t, 3, 6) .AND. (sys_char # "A" .AND. sale_mode # "Mitin")
    RETURN .F.
  ENDIF
  v_sav = sum_r_d
CASE n_field = 5   && Прием долларов
  IF .NOT. INLIST(pay_t, 3, 6) .AND. (sys_char # "A" .AND. sale_mode # "Mitin")
    RETURN .F.
  ENDIF
  ON KEY LABEL F1 DO C_Cup WITH 2
  v_sav = sum_d_
  q_usd = 0
CASE n_field = 6   && Курс конвертации долларов
  IF .NOT. INLIST(pay_t, 3, 6) .AND. (sys_char # "A" .AND. sale_mode # "Mitin")
    RETURN .F.
  ENDIF
  v_sav = kurs_d_r
CASE n_field = 7   && Рублевый эквивалент принятых долларов
  IF .NOT. INLIST(pay_t, 3, 6) .AND. (sys_char # "A" .AND. sale_mode # "Mitin")
    RETURN .F.
  ENDIF
  v_sav = sum_d_r
ENDCASE

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_E_Date     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Выход из поля даты платежного поручения.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE V_E_Date

ON KEY LABEL F1
RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Date      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                  Выбор даты для платежного поручения.                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE Ch_Date
PRIVATE s_dt

s_dt = e_doc_d
e_doc_d = Get_Date(e_doc_d)
IF e_doc_d # s_dt
  SHOW GET e_doc_d
  ON KEY LABEL F1
  KEYBOARD "{Tab}"
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Sum_R      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Выход из поля принятых рублей.                     │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE V_Sum_R

ON KEY LABEL F1
IF ex = 2
  RETURN .T.
ENDIF

IF sum_r_ # v_sav
  sum_r_d = ROUND(sum_r_/kurs_r_d,4)
  SHOW GET sum_r_d
ENDIF

DO Clc_Tot

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура C_Cup        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Подсчет купюр.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE C_Cup
PARAMETERS wh_m
PRIVATE sv, w

IF wh_m = 1   && Рубли
  sv = sum_r_
  w  = sum_r_
  w  = Cupury("Р", q_rub, 0, 0)
  IF w # 0
    sum_r_ = w
  ENDIF
  SHOW GET sum_r_
  IF sv # sum_r_
    sum_r_d = ROUND(sum_r_/kurs_r_d,4)
    SHOW GET sum_r_d
  ENDIF
ELSE
  sv = sum_d_
  w  = sum_d_
  w  = Cupury("$", q_usd, 0, 0)
  IF w # 0
    sum_d_ = w
  ENDIF
  SHOW GET sum_d_
  IF sv # sum_d_
    sum_d_r = ROUND(sum_d_*kurs_r_d,2)
    SHOW GET sum_d_r
  ENDIF
ENDIF

DO Clc_Tot

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Kurs1      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│           Выход из поля курса конвертации рублей в доллары.            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE V_Kurs1

IF ex = 2
  RETURN .T.
ENDIF
IF kurs_r_d <= 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Курс должен быть больше нуля!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
IF v_sav # kurs_r_d
  sum_r_d = ROUND(sum_r_/kurs_r_d,4)
  SHOW GET sum_r_d
ENDIF

DO Clc_Tot

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Sum_RD     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│         Выход из поля долларового эквивалента принятых рублей.         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE V_Sum_RD

IF ex = 2
  RETURN .T.
ENDIF

IF sum_r_d = 0 .AND. sum_r_ # 0
  PRIVATE mss
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Ненулевой рублевой массе долден соответствовать"
  mss(3) = CHR(0)+"ненулевой долларовый эквивалент!"
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF sum_r_d # v_sav
  IF sum_r_ = 0
    sum_r_ = ROUND(sum_r_d*kurs_r_d,2)
    SHOW GET sum_r_
  ELSE
    kurs_r_d = ROUND(sum_r_/sum_r_d,3)
    SHOW GET kurs_r_d
  ENDIF
ENDIF

DO Clc_Tot

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Sum_D      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Выход из поля принятых долларов.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE V_Sum_D

ON KEY LABEL F1
IF ex = 2
  RETURN .T.
ENDIF

IF sum_d_ # v_sav
  sum_d_r = ROUND(sum_d_*kurs_d_r,2)
  SHOW GET sum_d_r
ENDIF

DO Clc_Tot

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Kurs2      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│           Выход из поля курса конвертации долларов в рубли.            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE V_Kurs2

IF ex = 2
  RETURN .T.
ENDIF
IF kurs_d_r <= 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Курс должен быть больше нуля!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
IF v_sav # kurs_d_r
  sum_d_r = ROUND(sum_d_*kurs_d_r,2)
  SHOW GET sum_d_r
ENDIF

DO Clc_Tot

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Sum_DR     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│         Выход из поля рублевого эквивалента принятых долларов.         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE V_Sum_DR

IF ex = 2
  RETURN .T.
ENDIF

IF sum_d_r = 0 .AND. sum_d_ # 0
  PRIVATE mss
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Ненулевой долларовой массе долден соответствовать"
  mss(3) = CHR(0)+"ненулевой рублевый эквивалент!"
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF sum_d_r # v_sav
  IF sum_d_ = 0
    sum_d_ = ROUND(sum_d_r/kurs_d_r,4)
    SHOW GET sum_d_
  ELSE
    kurs_d_r = ROUND(sum_d_r/sum_d_,3)
    SHOW GET kurs_d_r
  ENDIF
ENDIF

DO Clc_Tot

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Blank    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Проверка корректности заполнения бланка.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE Tst_Blank

PRIVATE mss, s_sav, tmp_pay_t

IF ex = 2
  RETURN .T.
ENDIF

IF c_code = 0 .AND. EMPTY(c_name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Безымянный клиент? Не может быть!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 1
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали на какое предприятие приходовать деньги!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 22
ENDIF

DO CASE
CASE pay_t = 1
  tmp_pay_t = " "
CASE pay_t = 2
  tmp_pay_t = "1"
CASE pay_t = 3
  tmp_pay_t = "3"
CASE pay_t = 4
  tmp_pay_t = "Ч"
CASE pay_t = 5
  tmp_pay_t = "Б"
CASE pay_t = 6
  tmp_pay_t = "4"
ENDCASE
IF .NOT. LetMoney(f_c000, tmp_pay_t)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Тип платежа противопоказан предприятию!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 1
ENDIF

IF INLIST(pay_t, 2, 6) .AND. EMPTY(b_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Для безналичной оплаты ОБЯЗАТЕЛЬНО следует указать банк!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 22
ENDIF

IF e_doc_d > DATE()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  Дата поступления денег больше текущей даты! Это правильно? [ Нет ] [ Да ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 1
    RETURN 4
  ENDIF
ENDIF

IF .NOT. EMPTY(bill_num_p)
  IF bill_s # res_dep
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  Сумма в счете не соответствует приходу! Это правильно? [ Нет ] [ Да ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 1
      RETURN 5
    ENDIF
  ENDIF
ENDIF

RETURN TstForAS()

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Corr_Base    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Коррекция базы данных.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE Corr_Base

PRIVATE s_sav, max_ord, chk_num, chk_date, chk_time, chk_err, mss, tmpType
PRIVATE cusType, tmpNum, tmpMain, tmpRestR, tmpRestD, doc_sw_r, doc_sw_d
PRIVATE tmpINN, tmpKPP, i, tmpRec

m.tmpRec = 0
chk_num  = "          "
chk_date = DATE()
chk_time = SECONDS()
chk_err  = 0
m.tmpINN = ""
m.tmpKPP = ""

PRIVATE m0, mh, p0, ye0, d_type, doc_w, mh_r, mh_d, cntr

p0 = IIF(INLIST(pay_t, 3, 6), 0, nds_flt)  && Процент НДС для док-та
mh = res_dep                     && Общая сумма для документа
m0 = ROUND(mh/(100+p0)*100,IIF(INLIST(pay_t, 3, 6), 4, 2)) && Сумма без НДС для док-та
cntr = SPACE(16)

s_sav = SELECT()

SELECT 0
IF c_code = 0     && Клиент новый, надо добавить...
  IF (sys_char = "A" .OR. sale_mode = "Mitin")     && Основная площадка - просто добавляем...
    tmpType = WhatFace(1, 0)
    m.tmpINN = SUBSTR(tmpType, 3)
    m.i = AT(CHR(0), m.tmpINN)
    IF m.i # 0
      m.tmpKPP = SUBSTR(m.tmpINN, m.i+1)
      m.tmpINN = LEFT(m.tmpINN, m.i-1)
    ENDIF
    c_code = Doc_Num("CUS_CODE", "", "", "")
    USE (base_path+"ACCOUNT") ALIAS C_9122 AGAIN
    APPEND BLANK
    REPLACE SUBSYS     WITH pl_code, ;
            CUS_CODE   WITH c_code,  ;
            CUS_NAME   WITH c_name,  ;
            MARK1      WITH tmpType, ;
            DATE_CORR  WITH DATE(),  ;
            WHO_CORR   WITH user,    ;
            WHERE_CORR WITH arm,     ;
            DATE_ON    WITH DATE(),  ;
            WHO        WITH user,    ;
            WHERE      WITH arm
            
    USE(base_path+"CLI_INFO") ALIAS INF_3326 AGAIN
    APPEND BLANK
    REPLACE CUS_CODE WITH c_code,;
            NAME     WITH c_name,;
            COUNTRY  WITH cntr,  ;
            WHO      WITH user,  ;
            WHERE    WITH arm
    DO Put_INN WITH m.c_code, m.tmpINN, m.tmpKPP
  ELSE
    USE (base_path+"ACCOUNT") ORDER TAG EMP_NAME ALIAS C_9122 AGAIN
    IF .NOT. SEEK(sys_char)
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Вы исчерпали все заготовки!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      USE
      SELECT (s_sav)
      RETURN
    ELSE
      tmpType = WhatFace()
      REPLACE CUS_NAME WITH c_name,   ;
              DATE_CORR  WITH DATE(), ;
              WHO_CORR   WITH user,   ;
              MARK1      WITH tmpType,;
              WHERE_CORR WITH arm,    ;
              DATE_ON  WITH DATE(),   ;
              WHO      WITH user,     ;
              WHERE    WITH arm
      c_code = CUS_CODE
      
      USE(base_path+"CLI_INFO") ALIAS INF_3326 AGAIN
      APPEND BLANK
      REPLACE CUS_CODE WITH c_code,;
              NAME     WITH c_name,;
              COUNTRY  WITH cntr,  ;
              WHO      WITH user,  ;
              WHERE    WITH arm
      
    ENDIF
  ENDIF
ENDIF

USE (base_path+"ACCOUNT") ALIAS C_9122 AGAIN ORDER TAG CUS_CODE
SEEK c_code
cusType = MARK1   && Тип клиента

IF pay_t = 4
  PRIVATE is_kkm
  is_kkm = FirmKKM(f_c000)
  IF EMPTY(is_kkm)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"На данном предприятии нет кассового аппарата!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    IF USED("C_9122")
      USE IN C_9122
    ENDIF
    RETURN .F.
  ENDIF
  is_kkm = VAL(ALLTRIM(is_kkm))
  IF File_O(base_path+"TO_KKM.DBF") .AND. is_kkm # 0
    DO Do_Check WITH m0, mh-m0, res_n, chk_num, chk_date, chk_time, chk_err
    IF .NOT. EMPTY(chk_err)
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Ошибка "+ALLTRIM(STR(chk_err))+" при попытке пробить чек"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      IF USED("C_9122")
        USE IN C_9122
      ENDIF
      SELECT (s_sav)
      RETURN .F.
    ENDIF
  ENDIF
ENDIF
*
*  Кладем деньги на депозит
*

USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS C_9122 AGAIN
IF .NOT. SEEK(STR(c_code)+f_c000)
  APPEND BLANK
  REPLACE FIRM     WITH f_c000, ;
          CUS_CODE WITH c_code
ENDIF

IF INLIST(pay_t, 3, 6)
  REPLACE ACC_SUMD WITH ACC_SUMD+res_dep
ELSE
  REPLACE ACC_SUM WITH ACC_SUM+res_dep
ENDIF
*
*  Добавляем в поле налога...
*
IF res_n # 0
  SELECT 0
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9122 AGAIN
  SEEK f_c000
  REPLACE NALOG WITH NALOG+res_n
  USE
ENDIF

IF INLIST(pay_t, 3, 6)  && USD
  mh_d = mh
  mh_r = sum_r_+sum_d_r
ELSE
  mh_r = mh
  mh_d = ROUND((sum_d_+sum_r_d)/(sum_r_+sum_d_r)*mh_r, 2)
ENDIF

IF .NOT. EMPTY(bill_num_p) .AND. .NOT. is_req
  ye0 = "    "
  IF TYPE("bill_dat_p") = "D"
    ye0 = LEFT(DTOS(bill_dat_p),4)
  ENDIF
  IF TYPE("bill_dat_p") = "N"
    ye0 = STR(bill_dat_p,4)
  ENDIF
  IF TYPE("bill_dat_p") = "C"
    ye0 = bill_dat_p
  ENDIF
  SELECT 0
  USE (base_path+"BILLS") ORDER TAG DOC_NUM ALIAS B_9122 AGAIN
  IF SEEK(bill_frm_p+ye0+bill_num_p)
    IF NUM_LIST < 0
      SELECT C_9122
      IF INLIST(pay_t, 3, 6)
        REPLACE ACC_SUMD WITH ACC_SUMD-res_dep, ;
                ACC_SD_H WITH ACC_SD_H+res_dep
      ELSE
        REPLACE ACC_SUM WITH ACC_SUM-res_dep, ;
                ACC_S_H WITH ACC_S_H+res_dep
      ENDIF
    ENDIF
    SELECT B_9122
    IF pay_t = IIF(VALUTA = "$", 3, 2)
      p0 = NDS_      && Процент НДС берем из счета, если не меняли тип...
    ENDIF
    IF mh = BILL_SUM .AND. NUM_LIST >= 0   && Если сумма совпадает со счетом,
      m0 = SUM_0            &&        то сумму без НДС берем из счета...
    ELSE                    && ...иначе расчитываем
      m0 = ROUND(mh/(100+p0)*100,IIF(INLIST(pay_t, 3, 6), 4, 2))
    ENDIF
  ENDIF
ENDIF
SELECT C_9122
USE

DO CASE
CASE pay_t = 1
  d_t = " "
CASE pay_t = 2
  d_t = "1"
CASE pay_t = 3
  d_t = "3"
CASE pay_t = 4
  d_t = "Ч"
CASE pay_t = 5
  d_t = "Б"
CASE pay_t = 6
  d_t = "4"
ENDCASE
  
PRIVATE rst, rst_0, rst_n, rst_d, ord_cnt, numbs, sums, dates, k_w00, j00
PRIVATE mss_sum
mss_sum = 0

rst = SELECT()
SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9B23 AGAIN
SEEK f_c000
max_ord = IIF(FSIZE("M_SUM")=0,0,M_SUM)
USE 
SELECT (rst)

k_w00 = mh_r/mh_d
rst   = ABS(mh_r)
rst_0 = ABS(m0)
rst_n = ABS(res_n)
rst_d = ABS(mh_d)
ord_cnt  = 0

IF rst < max_ord .OR. max_ord = 0 .OR. d_t # " "
  DIMENSION numbs(1), sums(1,4), dates(1)
  ord_cnt  = 1
  numbs(1) = 0
  sums(1,1) = rst
  sums(1,2) = rst_0
  sums(1,3) = rst_n
  sums(1,4) = rst_d
  dates(1)  = DATE()
ELSE
  DO WHILE rst > 0
    IF rst < max_ord
      sum_w = rst
    ELSE
      sum_w = max_ord-ROUND(0.1*RAND()*max_ord, 0)
    ENDIF
    ord_cnt = ord_cnt+1
    rst = rst-sum_w
    DIMENSION numbs(ord_cnt), sums(ord_cnt,4), dates(ord_cnt)
    numbs(ord_cnt) = 0
    dates(ord_cnt) = DATE()+ord_cnt-1
    sums(ord_cnt,1)  = sum_w
    IF rst # 0
      sums(ord_cnt,2)  = ROUND(sum_w/(100+p0)*100, 2)
      rst_0 = rst_0-sums(ord_cnt,2)
      sums(ord_cnt,3)  = ROUND(sum_w*n_pers/100, 2)
      rst_n = rst_n-sums(ord_cnt,3)
      sums(ord_cnt,4)  = ROUND(sum_w/k_w00,4)
      rst_d = rst_d-sums(ord_cnt,4)
    ELSE
      sums(ord_cnt,2)  = rst_0
      sums(ord_cnt,3)  = rst_n
      sums(ord_cnt,4)  = rst_d
    ENDIF
  ENDDO
ENDIF

SELECT 0
USE (base_path+"INCMONEY") ALIAS M_9122 AGAIN

FOR j00 = 1 TO ord_cnt
  DO CASE
  CASE pay_t = 1
    d_t = " "
    IF mh > 0
      doc_w = Doc_Num("INC_ORDER", f_c000, arm, LEFT(DTOS(DATE()),4))
    ELSE
      doc_w = Doc_Num("EXP_ORDER", f_c000, arm, LEFT(DTOS(DATE()),4))
      doc_w = -doc_w
    ENDIF
  CASE pay_t = 2
    d_t = "1"
    doc_w = Doc_Num("MONEY_BN", f_c000, arm, LEFT(DTOS(DATE()),4))
  CASE pay_t = 3
    d_t = "3"
    doc_w = Doc_Num("MONEY_$", f_c000, arm, LEFT(DTOS(DATE()),4))
  CASE pay_t = 4
    d_t = "Ч"
    IF EMPTY(chk_num)
      doc_w = Doc_Num("CHECK", f_c000, arm, LEFT(DTOS(DATE()),4))
    ELSE
      doc_w = chk_num
    ENDIF
  CASE pay_t = 5
    d_t = "Б"
    doc_w = Doc_Num("SBERBANK", f_c000, arm, LEFT(DTOS(DATE()),4))
  CASE pay_t = 6
    d_t = "4"
    doc_w = Doc_Num("USD_BN", f_c000, arm, LEFT(DTOS(DATE()),4))
  ENDCASE
  IF TYPE("doc_w") = "N"
    doc_w = sys_char+STR(doc_w,9)
  ENDIF
  numbs(j00) = doc_w
  APPEND BLANK
  REPLACE FIRM       WITH f_c000,  ;
          DOC_TYPE   WITH d_t,     ;
          DOC_NUM    WITH doc_w,   ;
          DOC_DATE   WITH chk_date+j00-1, ;
          DOC_TIME   WITH chk_time,;
          BANK       WITH b_c000,  ;
          ENTER_DOC  WITH e_doc_n, ;
          ENTER_DAT  WITH IIF(d_t = " ", chk_date+j00-1, e_doc_d), ;
          CUS_CODE   WITH c_code,  ;
          CUS_NAME   WITH c_name,  ;
          CUS_BANK   WITH tmpBank, ;
          MONEY_0    WITH sums(j00,2)*SIGN(mh_r), ;
          NDS_       WITH p0,      ;
          MONEY_R    WITH sums(j00,1)*SIGN(mh_r), ;
          MONEY_D    WITH sums(j00,4)*SIGN(mh_r), ;
          NL_PRC     WITH n_pers,  ;
          N_OF_SALE  WITH sums(j00,3)*SIGN(mh_r), ;
          CONTACT    WITH face,    ;
          WHO        WITH user,    ;
          WHERE      WITH arm
  m.tmpRec = RECNO()
  IF DOC_TYPE = "Ч"
    mss_sum = MONEY_R
  ENDIF
  IF DOC_TYPE = "Ч" .AND. Is_Ur(CUS_CODE) .AND. YEAR(DOC_DATE) < 2006
    IF MONEY_R > 0 .OR. MONEY_D > 0
      tmpNum = Doc_Num("INC_ORDER", f_c000, arm, LEFT(DTOS(DATE()),4))
    ELSE
      tmpNum = Doc_Num("EXP_ORDER", f_c000, arm, LEFT(DTOS(DATE()),4))
      tmpNum = -tmpNum
    ENDIF
    REPLACE ENTER_DOC WITH CHR(0)+sys_char+STR(tmpNum,9)
  ENDIF
  IF .NOT. EMPTY(bill_num_p) .AND. .NOT. is_req   && Устанавливаем ссылку на счет
    REPLACE BILL_FIRM  WITH B_9122.FIRM,    ;
            BILL_NUM   WITH B_9122.DOC_NUM, ;
            BILL_DAT   WITH B_9122.DOC_DATE
  ENDIF
ENDFOR

*
*   Деньги в основную кассу, если они наличные...
*
PRIVATE doc_s_w0, doc_s_w1, doc_s_w2, kass_rest
doc_s_w0 = ""
IF .NOT. INLIST(pay_t, 2, 5, 6)  && Если деньги наличные
  USE (base_path+"ARM_LST") ALIAS K_9125 AGAIN
  LOCATE FOR ARM_C = arm
  m.tmpMain = sys_char = "A" .AND. sale_mode # "Mitin" .AND. Sale_Mode # "Mega"
*  m.tmpMain = MAIN_KASSA
  IF .NOT. m.tmpMain
    REPLACE KASSA     WITH KASSA+sum_r_, ;
            KASSA_USD WITH KASSA_USD+sum_d_
    m.tmpRestR = KASSA
    m.tmpRestD = KASSA_USD
    USE (base_path+"KASSA") ALIAS K_9125 AGAIN
    IF pay_t = 3         &&  Доллары, но...
      IF sum_r_ # 0      &&    часть денег получена в рублях...
        *
        *   Отправляем лишние доллары и получаем рубли
        *
        doc_s_w0 = Doc_Num("KASSA", f_c000, arm, LEFT(DTOS(DATE()),4))
        doc_sw_d = sys_char+CHRTRAN(arm," ","0")+STR(doc_s_w0,6)
        APPEND BLANK
        REPLACE DOC_NUM    WITH doc_sw_d, ;
                DOC_DATE   WITH DATE(),   ;
                DOC_TIME   WITH SECONDS(),;
                TOTAL      WITH sum_r_d,  ;
                MONEY_REST WITH 0,        ;
                KASSA_REST WITH m.tmpRestD,;
                VALUTA     WITH "$",      ;
                WHO_REP    WITH user,     ;
                WHO_GET    WITH user,     ;
                WHERE      WITH arm
        doc_s_w0 = Doc_Num("KASSA", f_c000, arm, LEFT(DTOS(DATE()),4))
        doc_sw_r = sys_char+CHRTRAN(arm," ","0")+STR(doc_s_w0,6)
        APPEND BLANK
        REPLACE DOC_NUM    WITH doc_sw_r, ;
                DOC_DATE   WITH DATE(),   ;
                DOC_TIME   WITH SECONDS(),;
                TOTAL      WITH -sum_r_,  ;
                MONEY_REST WITH 0,        ;
                KASSA_REST WITH m.tmpRestR,;
                VALUTA     WITH " ",      ;
                WHO_REP    WITH user,     ;
                WHO_GET    WITH user,     ;
                WHERE      WITH arm
        USE (base_path+"KASSA_M") ALIAS K_9125 AGAIN
        doc_s_w1 = Doc_Num("KASSA_M", f_c000, arm, LEFT(DTOS(DATE()),4))
        doc_s_w1 = sys_char+STR(doc_s_w1,6)
        APPEND BLANK
        REPLACE DOC_NUM    WITH doc_s_w1,  ;
                DOC_DATE   WITH DATE(),    ;
                DOC_TIME   WITH SECONDS(), ;
                CODE       WITH 1,         ;
                REF_NUM    WITH doc_sw_d,  ;
                REF_DATE   WITH DATE(),    ;
                SUM_D      WITH sum_r_d,   ;
                WHO        WITH user,      ;
                WHERE      WITH arm
        doc_s_w1 = Doc_Num("KASSA_M", f_c000, arm, LEFT(DTOS(DATE()),4))
        doc_s_w1 = sys_char+STR(doc_s_w1,6)
        APPEND BLANK
        REPLACE DOC_NUM    WITH doc_s_w1,  ;
                DOC_DATE   WITH DATE(),    ;
                DOC_TIME   WITH SECONDS(), ;
                CODE       WITH 1,         ;
                REF_NUM    WITH doc_sw_r,  ;
                REF_DATE   WITH DATE(),    ;
                SUM_R      WITH -sum_r_,   ;
                WHO        WITH user,      ;
                WHERE      WITH arm
      ENDIF
    ELSE                 &&  Рубли, но...
      IF sum_d_ # 0      &&    часть денег получена в долларах...
        *
        *   Отправляем лишние рубли и получаем доллары
        *
        doc_s_w0 = Doc_Num("KASSA", f_c000, arm, LEFT(DTOS(DATE()),4))
        doc_sw_r = sys_char+CHRTRAN(arm," ","0")+STR(doc_s_w0,6)
        APPEND BLANK
        REPLACE DOC_NUM    WITH doc_sw_r, ;
                DOC_DATE   WITH DATE(),   ;
                DOC_TIME   WITH SECONDS(),;
                TOTAL      WITH sum_d_r,  ;
                MONEY_REST WITH 0,        ;
                KASSA_REST WITH m.tmpRestR,;
                VALUTA     WITH " ",      ;
                WHO_REP    WITH user,     ;
                WHO_GET    WITH user,     ;
                WHERE      WITH arm
        doc_s_w0 = Doc_Num("KASSA", f_c000, arm, LEFT(DTOS(DATE()),4))
        doc_sw_d = sys_char+CHRTRAN(arm," ","0")+STR(doc_s_w0,6)
        APPEND BLANK
        REPLACE DOC_NUM    WITH doc_sw_d, ;
                DOC_DATE   WITH DATE(),   ;
                DOC_TIME   WITH SECONDS(),;
                TOTAL      WITH -sum_d_,  ;
                MONEY_REST WITH 0,        ;
                KASSA_REST WITH m.tmpRestD,;
                VALUTA     WITH "$",      ;
                WHO_REP    WITH user,     ;
                WHO_GET    WITH user,     ;
                WHERE      WITH arm
        USE (base_path+"KASSA_M") ALIAS K_9125 AGAIN
        doc_s_w1 = Doc_Num("KASSA_M", f_c000, arm, LEFT(DTOS(DATE()),4))
        doc_s_w1 = sys_char+STR(doc_s_w1,6)
        APPEND BLANK
        REPLACE DOC_NUM    WITH doc_s_w1,  ;
                DOC_DATE   WITH DATE(),    ;
                DOC_TIME   WITH SECONDS(), ;
                CODE       WITH 1,         ;
                REF_NUM    WITH doc_sw_d,  ;
                REF_DATE   WITH DATE(),    ;
                SUM_D      WITH -sum_d_,   ;
                WHO        WITH user,      ;
                WHERE      WITH arm
        doc_s_w1 = Doc_Num("KASSA_M", f_c000, arm, LEFT(DTOS(DATE()),4))
        doc_s_w1 = sys_char+STR(doc_s_w1,6)
        APPEND BLANK
        REPLACE DOC_NUM    WITH doc_s_w1,  ;
                DOC_DATE   WITH DATE(),    ;
                DOC_TIME   WITH SECONDS(), ;
                CODE       WITH 1,         ;
                REF_NUM    WITH doc_sw_r,  ;
                REF_DATE   WITH DATE(),    ;
                SUM_R      WITH sum_d_r,   ;
                WHO        WITH user,      ;
                WHERE      WITH arm
      ENDIF
    ENDIF
    USE (base_path+"KASSA_M") ALIAS K_9125 AGAIN
  ELSE
    *
    *   Посылаем и принимаем сумму в долларах или рублях соответственно...
    *
    kass_rest = IIF(pay_t = 3,KASSA_USD,KASSA)
    doc_s_w0 = Doc_Num("KASSA", f_c000, arm, LEFT(DTOS(DATE()),4))
    doc_s_w0 = sys_char+CHRTRAN(arm," ","0")+STR(doc_s_w0,6)
    USE (base_path+"KASSA") ALIAS K_9125 AGAIN
    APPEND BLANK
    REPLACE DOC_NUM    WITH doc_s_w0, ;
            DOC_DATE   WITH DATE(),   ;
            DOC_TIME   WITH SECONDS(),;
            TOTAL      WITH res_n+res_dep, ;
            MONEY_REST WITH 0,        ;
            KASSA_REST WITH kass_rest,;
            VALUTA     WITH IIF(INLIST(pay_t, 3, 6), "$", " "),;
            WHO_REP    WITH user,     ;
            WHO_GET    WITH user,     ;
            WHERE      WITH arm
    doc_s_w1 = Doc_Num("KASSA_M", f_c000, arm, LEFT(DTOS(DATE()),4))
    doc_s_w1 = sys_char+STR(doc_s_w1,6)
    USE (base_path+"KASSA_M") ALIAS K_9125 AGAIN
    APPEND BLANK
    REPLACE DOC_NUM    WITH doc_s_w1,  ;
            DOC_DATE   WITH DATE(),    ;
            DOC_TIME   WITH SECONDS(), ;
            CODE       WITH 1,         ;
            REF_NUM    WITH doc_s_w0,  ;
            REF_DATE   WITH DATE(),    ;
            SUM_R      WITH IIF(INLIST(pay_t, 3, 6), 0, res_n+res_dep), ;
            SUM_D      WITH IIF(.NOT. INLIST(pay_t, 3, 6), 0, res_n+res_dep), ;
            NOTE       WITH nt,        ;
            WHO        WITH user,      ;
            WHERE      WITH arm
    USE (base_path+"KASSA_S")
    REPLACE SUM_R WITH SUM_R+sum_r_, SUM_D WITH SUM_D+sum_d_
  ENDIF
  USE (base_path+"KASSA_M") ALIAS K_9125 AGAIN
  IF pay_t = 3         &&  Доллары, но...
    IF sum_r_ # 0      &&    часть денег получена в рублях...
      *
      *   Конвертируем часть принятых долларов в рубли
      *
      doc_s_w2 = Doc_Num("KASSA_M", f_c000, arm, LEFT(DTOS(DATE()),4))
      doc_s_w2 = sys_char+STR(doc_s_w2,6)
      APPEND BLANK
      REPLACE DOC_NUM    WITH doc_s_w2,  ;
              DOC_DATE   WITH DATE(),    ;
              DOC_TIME   WITH SECONDS(), ;
              CODE       WITH 2,         ;
              SUM_R      WITH sum_r_,    ;
              SUM_D      WITH -sum_r_d,  ;
              KURS_FL    WITH kurs_r_d,  ;
              WHO        WITH user,      ;
              WHERE      WITH arm
    ENDIF
  ELSE                 &&  Рубли, но...
    IF sum_d_ # 0      &&    часть денег получена в долларах...
      *
      *   Конвертируем часть принятых рублей в доллары
      *
      doc_s_w2 = Doc_Num("KASSA_M", f_c000, arm, LEFT(DTOS(DATE()),4))
      doc_s_w2 = sys_char+STR(doc_s_w2,6)
      APPEND BLANK
      REPLACE DOC_NUM    WITH doc_s_w2,  ;
              DOC_DATE   WITH DATE(),    ;
              DOC_TIME   WITH SECONDS(), ;
              CODE       WITH 2,         ;
              SUM_R      WITH -sum_d_r,  ;
              SUM_D      WITH sum_d_,    ;
              KURS_FL    WITH kurs_d_r,  ;
              WHO        WITH user,      ;
              WHERE      WITH arm
    ENDIF
  ENDIF
ENDIF

USE

*
*   Устанавливаем отметки в счете, если он существует
*
IF .NOT. EMPTY(bill_num_p)
  PRIVATE s_sav0
  s_sav0 = SELECT()
  IF is_req
    SELECT 0
    USE (base_path+"BUHG_REQ") ORDER TAG DOC_NUM ALIAS B_9122 AGAIN
    SEEK d_year+bill_num_p
    REPLACE IS_INC    WITH .F.,  ;
            WHERE_INP WITH arm,  ;
            WHO_INP   WITH user, ;
            DATE_INP  WITH DATE()
    SELECT 0
    USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS L_9122 AGAIN
    IF SEEK(B_9122.LST_NUM)
      REPLACE PAY_FIRM WITH f_c000,        ;
              PAY_TYPE WITH d_t,           ;
              PAY_NUM  WITH numbs(1),      ;
              PAY_DATE WITH dates(1),      ;
              WHO_PAY  WITH user
      IF pay_t = 4 .OR. pay_t = 1 .AND. .NOT. EMPTY(cusType)
        REPLACE KASSA WITH KASSA+res_dep
      ENDIF
    ENDIF
    USE
    SELECT B_9122
    USE
    SELECT (s_sav0)
  ELSE
    SELECT B_9122
    REPLACE INC_FIRM WITH f_c000, ;
            INC_TYPE WITH d_t,    ;
            INC_DOC  WITH doc_w,  ;
            INC_DATE WITH DATE(), ;
            CUS_CODE WITH c_code, ;
            STATUS   WITH "Оплачено"
    IF .NOT. EMPTY(NUM_LIST)
      PRIVATE n_list
      n_list = NUM_LIST
      USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT_9125 AGAIN
      IF SEEK(n_list)
        REPLACE CLI_CODE WITH c_code,        ;
                PAY_FIRM WITH f_c000,        ;
                PAY_TYPE WITH d_t,           ;
                PAY_NUM  WITH numbs(1),      ;
                PAY_DATE WITH dates(1),      ;
                WHO_PAY  WITH user
        IF pay_t = 4 .OR. pay_t = 1 .AND. .NOT. EMPTY(cusType)
          REPLACE KASSA WITH KASSA+res_dep
        ENDIF
      ENDIF
      USE
    ENDIF
    USE
  ENDIF
ENDIF

IF d_t $ " "
  FOR j00 = 1 TO ord_cnt
    DO BigOrd WITH f_c000, numbs(j00), dates(j00)
  ENDFOR
  DO ViewBig WITH max_ord
  SELECT BIGORD
  SCAN
    dates(RECNO()) = DOC_DATE
  ENDSCAN
  USE
  DELETE FILE (tmpo_path+"BIGORD.DBF")
  SELECT BIGDOC
  USE
  SELECT CUS_SUM
  USE
ENDIF

IF m.tmpRec # 0	.AND. m.tmpPay	&& Создаем счет-фактуру по платежу.
  DO Pay_Bill WITH m.tmpRec
ENDIF

DO CheckRem WITH mss_sum 
*FOR j00 = 1 TO ord_cnt
*  DO P_Money WITH f_c000, d_t, numbs(j00), dates(j00)
*ENDFOR

SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Change_F     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      VALID при смене предприятия.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE Change_F

n_pers = IIF(INLIST(pay_t, 1, 4, 5), G_Nalog(f_c000), 0)
SHOW GET n_pers
bill_n = ROUND(bill_s*n_pers/100, 2)
SHOW GET bill_n
bill_h = bill_s+bill_n
SHOW GET bill_h
DO Clc_Tot
DO Draw_V
IF INLIST(pay_t, 2, 4, 5, 6)
  face = SPACE(LEN(face))
  SHOW GET face
ENDIF
DO ShowRest

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Sav_Dep      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      WHEN для суммы на депозит.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE Sav_Dep
IF n_pers = 0
  RETURN .F.
ENDIF
v_sav = res_dep
RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Clc_Nlg      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     VALID для суммы на депозит.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.01.1999 ──┘
PROCEDURE Clc_Nlg
IF res_dep = v_sav
  RETURN .T.
ENDIF

res_n = ROUND(res_dep/100*n_pers,IIF(INLIST(pay_t, 3, 6), 4, 2))
IF INLIST(pay_t, 3, 6)  && USD
 sum_d_  = res_dep+res_n-sum_r_d
 sum_d_r = sum_d_*kurs_d_r
ELSE
 sum_r_  = res_dep+res_n-sum_d_r
 sum_r_d = sum_r_/kurs_r_d
ENDIF 
SHOW GETS

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура ShowRest     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Остатки на депозите                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 16.01.2001 ──┘
PROCEDURE ShowRest

PRIVATE s_sav, sr, srh, sd, sdh
STORE 0 TO sr, srh, sd, sdh
IF c_code # 0 .AND. .NOT. EMPTY(f_c000)
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"CUS_REST") AGAIN ALIAS RST_1116 ORDER TAG CUS_CODE
  IF SEEK(STR(c_code)+f_c000)
    sr  = ACC_SUM
    srh = ACC_S_H
    sd  = ACC_SUMD
    sdh = ACC_SD_H
  ENDIF
  USE
  SELECT (s_sav)
ENDIF

@ 12, 10 SAY sr  PICTURE "@Z 999999999.99"
@ 12, 23 SAY srh PICTURE "@Z 999999999.99"
@ 12, 40 SAY sd  PICTURE "@Z 9999999.9999"
@ 12, 53 SAY sdh PICTURE "@Z 9999999.9999"

RETURN
*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура TstForAS     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Проверка для АС.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/20/2001 ──┘
PROCEDURE TstForAS

PRIVATE s_sav, tmpSum
IF c_code <= 0 .OR. EMPTY(bill_num_p) .OR. is_req .OR. res_dep > 0
  RETURN .T.
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"BILLS") ORDER TAG DOC_NUM ALIAS B_1320 AGAIN
IF .NOT. SEEK(bill_frm_p+d_year+bill_num_p)
  USE
  SELECT (s_sav)
  RETURN .T.
ENDIF

IF NUM_LIST >= 0
  USE
  SELECT (s_sav)
  RETURN .T.
ENDIF

tmpSum = 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS C_1320 AGAIN
IF SEEK(STR(c_code)+f_c000)
  tmpSum = IIF(INLIST(pay_t, 3, 6), ACC_SD_H, ACC_S_H)
ENDIF
USE
SELECT (s_sav)

IF tmpSum+res_dep < 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  Задержаной суммы недостаточно! Продолжаем прием денег [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Nalog      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Вычисление налога с продаж.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.01.2002 ──┘
PROCEDURE G_Nalog
PARAMETERS tmpFirm

PRIVATE s_sav, tmpUr, tmpPers

tmpUr = .T.
IF c_code # 0
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"ACCOUNT") SHARED AGAIN ALIAS CUS_2121 ORDER TAG CUS_CODE
  SEEK c_code
  tmpUr = EMPTY(MARK1)
  USE
  SELECT (s_sav)
ENDIF

*IF pay_t = 1 .AND. .NOT. tmpUr .OR. pay_t = 4
IF INLIST(pay_t, 1, 4) .AND. .NOT. tmpUr
  tmpPers = GetNalog(tmpFirm)
ELSE
  tmpPers = 0
ENDIF

RETURN tmpPers

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура DefBank      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Установить банк по умолчанию для клиента                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.10.2006 ──┘
PROCEDURE DefBank
PARAMETERS prmCode

m.tmpBank = 0
m.bankName = SPACE(22)
IF m.prmCode = 0
  RETURN
ENDIF

USE (m.base_path+"CUS_BIL") SHARED AGAIN ORDER TAG CUS_CODE ALIAS TMP_BANK IN 0
IF SEEK(m.prmCode, "TMP_BANK")
  m.bankName = LEFT(TMP_BANK.BANK, 22)
ELSE
  m.bankName = SPACE(22)
ENDIF

USE IN TMP_BANK

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_CBank     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Выбор банка клиента                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.10.2006 ──┘
PROCEDURE Ch_CBank

PRIVATE newCode

m.newCode = Cus_Bank(m.c_code, "**", m.tmpBank)
IF m.newCode < 0
  RETURN .T.
ENDIF

m.swBank = .F.
SHOW GET m.swBank
m.tmpBank = m.newCode

IF m.tmpBank = 0
  USE (m.base_path+"CUS_BIL") SHARED AGAIN ORDER TAG CUS_CODE ALIAS TMP_BANK IN 0
  IF SEEK(m.c_code, "TMP_BANK")
    m.bankName = LEFT(TMP_BANK.BANK, 22)
  ELSE
    m.bankName = SPACE(22)
  ENDIF
ELSE
  USE (m.base_path+"CUS_BANK") SHARED AGAIN ORDER TAG BANK_CODE ALIAS TMP_BANK IN 0
  IF SEEK(STR(m.c_code)+STR(m.tmpBank), "TMP_BANK")
    m.bankName = LEFT(TMP_BANK.BANK, 22)
  ENDIF
ENDIF

USE IN TMP_BANK

SHOW GET m.bankName

RETURN .T.
