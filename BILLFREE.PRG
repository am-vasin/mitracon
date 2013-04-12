*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл BillFree     Разработчик Андрей Васин           20.03.97 22:25:05 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                        Свободная счет-фактура.                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE BillFree
PARAMETERS cnt_name

PRIVATE f_c000, f_n000, f_c_old, enable_f
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
*
*   Флажки, описывающие режим работы программы
*

IF TYPE("cnt_name")  = "C"
  IF cnt_name = "Modify" .AND. .NOT. EMPTY(BILL_F.HOLD_DATE)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Этот документ уже вошел в книгу продаж..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN
  ENDIF
ENDIF

IF TYPE("cnt_name")  = "C"
  IF cnt_name = "Modify" .AND. .NOT. EMPTY(BILL_F.SEND_DATE) .AND. (sys_char # "A" .AND. sale_mode # "Mitin")
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Этот документ уже передан на основную площадку..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN
  ENDIF
ENDIF

PRIVATE type_flg   && допустимые типы документов:
                   && 1 - оба типа,
                   && 2 - только Нал,
                   && 3 - только Б/Н.
PRIVATE base_flg   && наличие базы клиентов
PRIVATE addc_flg   && разрешение добавлять клиентов в базу (base_flg = .T.)
PRIVATE attr_flg   && наличие файла атрибутов клиента
PRIVATE gds_flg    && наличие списка товаров
PRIVATE addg_flg   && разрешение ввода товара без кода
PRIVATE g_codes, g_names, n

type_flg = 1
base_flg = File_O(base_path+"ACCOUNT.DBF")
addc_flg = .F.
attr_flg = File_O(base_path+"CUS_BIL.DBF")
gds_flg  = .F.
addg_flg = .T.

PRIVATE sel
sel = SELECT()
SELECT 0
n = 0
IF File_O(base_path+"GOODS.DBF")
  USE (base_path+"GOODS.DBF") ORDER TAG NAME
  IF addg_flg
    n = 1
    DIMENSION g_codes(n), g_names(n)
    g_codes(n) = 0
    g_names(n) = SPACE(24)
  ENDIF
  SCAN
    n = n+1
    DIMENSION g_codes(n), g_names(n)
    g_codes(n) = CODE
    g_names(n) = PADR(NAME, 24)
  ENDSCAN
  USE
ENDIF  
IF n = 0
  n = 1
  DIMENSION g_codes(n), g_names(n)
  g_codes(n) = 0
  g_names(n) = SPACE(24)
ENDIF

IF attr_flg
  USE (base_path+"CUS_BIL") ORDER TAG CUS_CODE
ENDIF

IF base_flg
  SELECT 0
  USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
  DO Use_Dummy
  DO Use_Link
ENDIF

PRIVATE i_zero, i_real, j, nnn, mode_w
IF cnt_name = "Modify"
  mode_w = "modify"
ELSE
  mode_w = "add"
ENDIF
DO Get_Form WITH mode_w
IF USED("CUS_BIL")
  SELECT CUS_BIL
  USE
ENDIF
IF USED("ACCOUNT")
  SELECT ACCOUNT
  USE
ENDIF

SELECT (sel)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                Интерактивная часть ввода счета-фактуры.                ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 20.03.97 22:39:12 ═╝
PROCEDURE Get_Form
PARAMETER mode_w
PRIVATE ln, wd, c_type, c_code, c_name
PRIVATE p01, p02, p03, p04, p05, p06, p07, p08, p09, p10, p11, dt_000
PRIVATE g_code, g_name, inv, money0, money_nds, money, prc_nds, sav_val
PRIVATE n_f, buff

*┌──────────────────────────────────────────────────────────────────────────┐
*│                           Клиент: [ ] Постоянный                         │
*││        Имя ....:....!....:....!....:....!....:....!....:....!          ││
*││                                                                        ││
*││  Адрес ....:....!....:....!....:....!....:....!....:....!....:....!    ││
*││                                                                        ││
*││ Город ....:....!....:. Тел. ....:....!....:.  Р/С ....:....!....:....! ││
*││                                                                        ││
*││ в ....:....!....:....!....:....!....:....!   К/С ....:....!....:....!  ││
*││                                                                        ││
*││  БИК ....:....!....:....! ИНН ....:....!.. ОКОНХ ....: ОКПО ....:...   ││
*││                                                                        ││
*││....:....!....:....!....:....!....:....!....:....!....:....!....:....!..││
*│└────────────────────────────────────────────────────────────────────────┘│
*│                  Предприятие 999 9999999999999999 (F1)                   │
*│┌─ Отпущено ─────────────────────────────────────────────────────────────┐│
*││ Товар ....:....!....:....!....:. Документ N ....:....! Дата 99.99.9999 ││
*││ Сумма без НДС 9999999.99  %НДС 999.9  НДС 9999999.99  Итого 9999999.99 ││
*│└────────────────────────────────────────────────────────────────────────┘│
*│                  < OK Ctrl-W > < Отказаться Esc >                        │
*└──────────────────────────────────────────────────────────────────────────┘
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

PRIVATE ex
&&   Заполняем поля бланка
ex   = 1             &&
SELECT BILL_F
IF mode_w = "add"
  IF File_O(prmo_path+"BILLFREE.SAV")
    n_f = FOPEN(prmo_path+"BILLFREE.SAV")
    buff = FGETS(n_f)
    c_type = .NOT. EMPTY(buff)
*    c_type = .F.                 && Тип клиента: неизв., постоянный
    buff = FGETS(n_f)
    c_code = VAL(ALLTRIM(buff))
*    c_code = 0                   && Неизв.
    c_name = FGETS(n_f)
*    c_name = SPACE(50)           && Имя клиента
    p01 = FGETS(n_f)   && Адрес
    p02 = FGETS(n_f)   && Телефон
    p03 = FGETS(n_f)   && Расчетный счет
    p04 = FGETS(n_f)   && Банк
    p05 = FGETS(n_f)   && Корр. счет
    p06 = FGETS(n_f)   && БИК
    p07 = FGETS(n_f)   && Город
    p08 = FGETS(n_f)   && ИНН
    p09 = FGETS(n_f)   && ОКОНХ
    p10 = FGETS(n_f)   && ОКПО
    p11 = FGETS(n_f)   && Примечание
    dt_000 = CTOD(FGETS(n_f))
    buff = FGETS(n_f)
    g_code = VAL(ALLTRIM(buff))
*    g_code = g_codes(1)  && Код товара
    g_name = FGETS(n_f)
*    g_name = g_names(1)  && Наименование товара
    inv    = FGETS(n_f)
*    inv    = SPACE(10)   && Накладная
    money0 = VAL(ALLTRIM(FGETS(n_f)))
*    money0 = 0
    prc_nds = VAL(ALLTRIM(FGETS(n_f)))
*    prc_nds= nds_flt
    money_nds = VAL(ALLTRIM(FGETS(n_f)))
*    money_nds = 0
    money = VAL(ALLTRIM(FGETS(n_f)))
*    money  = 0           && Сумма
    f_c000 = FGETS(n_f)
    f_n000 = FGETS(n_f)
    f_c_old= f_c000
    = FCLOSE(n_f)
  ELSE
    c_type = .F.                 && Тип клиента: неизв., постоянный
    c_code = 0                   && Неизв.
    c_name = SPACE(50)           && Имя клиента
    p01 = SPACE(60)   && Адрес
    p02 = SPACE(16)   && Телефон
    p03 = SPACE(20)   && Расчетный счет
    p04 = SPACE(40)   && Банк
    p05 = SPACE(20)   && Корр. счет
    p06 = SPACE(20)   && БИК
    p07 = SPACE(16)   && Город
    p08 = SPACE(12)   && ИНН
    p09 = SPACE(5)    && ОКОНХ
    p10 = SPACE(8)    && ОКПО
    p11 = SPACE(72)   && Примечание
    dt_000 = DATE()
    g_code = g_codes(1)  && Код товара
    g_name = g_names(1)  && Наименование товара
    inv    = SPACE(10)   && Накладная
    money0 = 0
    prc_nds= nds_flt
    money_nds = 0
    money  = 0           && Сумма
    f_c000 = def_firm
    f_n000 = ""
    f_c_old= def_firm
  ENDIF
  enable_f = enab_firm
  ln = 21
ELSE
  c_type = BILL_F.CUS_CODE # 0
  c_code = BILL_F.CUS_CODE
  c_name = BILL_F.CUS_NAME
  p01    = BILL_F.ADDRESS
  p02    = BILL_F.PHONE
  p03    = BILL_F.ACC_BILL
  p04    = BILL_F.BANK
  p05    = BILL_F.CORR_BILL
  p06    = BILL_F.BIK
  p07    = BILL_F.CITY
  p08    = BILL_F.INN
  p09    = BILL_F.OKOHX
  p10    = BILL_F.OKPO
  p11    = BILL_F.NOTE
  inv    = BILL_F.INV_NUM
  dt_000 = BILL_F.DOC_DATE
  g_code = BILL_F.GOODS_COD
  g_name = BILL_F.GOODS
  money0 = BILL_F.TOTAL_0
  prc_nds= BILL_F.NDS_
  money_nds = BILL_F.TOTAL_NDS
  money  = BILL_F.TOTAL_R
  f_c000 = BILL_F.FIRM
  f_n000 = ""
  f_c_old= BILL_F.FIRM
  enable_f = .F.
  ln = 21
ENDIF
wd = 78
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, IIF(mode_w = "add", "Ввод счета-фактуры",  ;
                            "Коррекция счета-фактуры N "+LEFT(DOC_NUM,1)+  ;
                            ALLTRIM(SUBSTR(DOC_NUM,2))+" от "+TRANSFORM(DOC_DATE,"@D"))
ln = 2
DO Sun_Bord WITH ln+13, 2, ln+16, 75
@ ln+13, 4 SAY " Отпущено "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
sav_val = 0
@ ln, 29 SAY "Клиент:"
IF base_flg
  @ ln, 37 GET c_type PICTURE "@*C Постоянный " VALID Get_Cust()
ENDIF
@ ln+1, 11 SAY "Имя" GET c_name WHEN c_code = 0
@ ln+3,  5 SAY "Адрес" GET p01
@ ln+5,  4 SAY "Город" GET p07
@ ln+5, 27 SAY "Тел." GET p02
@ ln+5, 50 SAY "Р/С" GET p03
@ ln+7,  4 SAY "в" GET p04
@ ln+7, 49 SAY "К/С" GET p05
@ ln+9,  5 SAY "БИК" GET p06
@ ln+9, 30 SAY "ИНН" GET p08
@ ln+9, 47 SAY "ОКОНХ" GET p09
@ ln+9, 59 SAY "ОКПО" GET p10
@ ln+11,  3 GET p11
DO Get_Firm WITH ln+12, 20, enable_f
@ ln+14,  4 SAY "Товар" GET g_name WHEN G_Goods()
@ ln+14, 37 SAY "Документ" GET inv
@ ln+14, 59 SAY "Дата" GET dt_000
@ ln+15,  4 SAY "Сумма без НДС" GET money0 PICTURE "@Z 9999999.99"  ;
       WHEN Sav_Ent(money0) VALID Recalc(money0 = sav_val,1)
@ ln+15, 30 SAY "%НДС" GET prc_nds PICTURE "@Z 999.9"  ;
       WHEN Sav_Ent(prc_nds) VALID Recalc(prc_nds = sav_val,1)
@ ln+15, 42 SAY "НДС" GET money_nds PICTURE "@Z 9999999.99" WHEN .F.
@ ln+15, 58 SAY "Итого" GET money PICTURE "@Z 9999999.99"  ;
       WHEN Sav_Ent(money) VALID Recalc(money = sav_val,2)
@ ln+17, 20 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Term_Bl()

IF ex # 2
  DELETE FILE (prmo_path+"BILLFREE.SAV")
  n_f = FCREATE(prmo_path+"BILLFREE.SAV")
  = FPUTS(n_f, IIF(c_type, "*", " "))
*    c_type = .F.                 && Тип клиента: неизв., постоянный
  = FPUTS(n_f, STR(c_code))
*    c_code = 0                   && Неизв.
  = FPUTS(n_f, c_name)
*    c_name = SPACE(50)           && Имя клиента
  = FPUTS(n_f, p01)
*    p01 = FGETS(n_f)   && Адрес
  = FPUTS(n_f, p02)
*    p02 = FGETS(n_f)   && Телефон
  = FPUTS(n_f, p03)
*    p03 = FGETS(n_f)   && Расчетный счет
  = FPUTS(n_f, p04)
*    p04 = FGETS(n_f)   && Банк
  = FPUTS(n_f, p05)
*    p05 = FGETS(n_f)   && Корр. счет
  = FPUTS(n_f, p06)
*    p06 = FGETS(n_f)   && БИК
  = FPUTS(n_f, p07)
*    p07 = FGETS(n_f)   && Город
  = FPUTS(n_f, p08)
*    p08 = FGETS(n_f)   && ИНН
  = FPUTS(n_f, p09)
*    p09 = FGETS(n_f)   && ОКОНХ
  = FPUTS(n_f, p10)
*    p10 = FGETS(n_f)   && ОКПО
  = FPUTS(n_f, p11)
*    p11 = FGETS(n_f)   && Примечание

  = FPUTS(n_f, DTOC(dt_000))
*    dt_000 = CTOD(FGETS(n_f))
  = FPUTS(n_f, STR(g_code))
*    g_code = g_codes(1)  && Код товара
  = FPUTS(n_f, g_name)
*    g_name = g_names(1)  && Наименование товара
  = FPUTS(n_f, inv)
*    inv    = SPACE(10)   && Накладная
  = FPUTS(n_f, STR(money0,15,3))
*    money0 = 0
  = FPUTS(n_f, STR(prc_nds,15,3))
*    prc_nds= nds_flt
  = FPUTS(n_f, STR(money_nds,15,3))
*    money_nds = 0
  = FPUTS(n_f, STR(money,15,3))
*    money  = 0           && Сумма
  = FPUTS(n_f, f_c000)
  = FPUTS(n_f, f_n000)
    = FCLOSE(n_f)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
SELECT TMP_LINK
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Сохранение входящего значения.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 23.06.97 21:34:37 ═╝
PROCEDURE Sav_Ent
PARAMETER v
sav_val = v
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Пересчет значений сумм.                        ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 23.06.97 21:40:37 ═╝
PROCEDURE Recalc
PARAMETER pass, mode
IF pass
  RETURN .T.
ENDIF
IF mode = 1
  money_nds = ROUND(money0*prc_nds/100,2)
  money     = money0+money_nds
ELSE
  money_nds = ROUND(money*prc_nds/(100+prc_nds),2)
  money0    = money-money_nds
ENDIF
SHOW GET money0
SHOW GET money_nds
SHOW GET money

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Выбор клиента из базы.                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 20.03.97 23:34:37 ═╝
PROCEDURE Get_Cust

IF c_type
  c_type = Custs("", addc_flg, .F., .T.)
ENDIF

SHOW GET c_type
IF c_type
  c_code = ACCOUNT.CUS_CODE
  c_name = ACCOUNT.CUS_NAME
  IF attr_flg
    IF SEEK(c_code, "CUS_BIL")
      c_name = CUS_BIL.CUS_NAME
      p01 = CUS_BIL.ADDRESS
      p02 = CUS_BIL.PHONE
      p03 = CUS_BIL.ACC_BILL
      p04 = CUS_BIL.BANK
      p05 = CUS_BIL.CORR_BILL
      p06 = CUS_BIL.BIK
      p07 = CUS_BIL.CITY
      p08 = CUS_BIL.INN
      p09 = CUS_BIL.OKOHX
      p10 = CUS_BIL.OKPO
      SHOW GET p01
      SHOW GET p02
      SHOW GET p03
      SHOW GET p04
      SHOW GET p05
      SHOW GET p06
      SHOW GET p07
      SHOW GET p08
      SHOW GET p09
      SHOW GET p10
    ENDIF
  ENDIF
  SHOW GET c_name
ELSE
  c_code = 0
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                        Выбор товара из списка.                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.03.97 00:05:27 ═╝
PROCEDURE G_Goods
PRIVATE new_cod, i

IF n > 1
  new_cod = ReadMenu(g_code, g_codes, g_names, "Перечень товаров")
  IF new_cod # g_code
    i = ASCAN(g_codes, new_cod)
    g_name = g_names(i)
    g_code = new_cod
    SHOW GET g_name
  ENDIF
ENDIF

RETURN g_code = 0

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Выход из бланка.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.03.97 10:45:53 ═╝
PROCEDURE Term_Bl

PRIVATE mss, arr_w, doc_n, doc_d, doc_t, rc_w

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF EMPTY(c_name)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Поподробнее, пожалуйста, о клиенте, ну, хотя бы, его имя..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(g_name)
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Остается непонятным, за что же, собственно,"
  mss(3) = CHR(0)+ALLTRIM(c_name)+" нам заплатил..."
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF money = 0
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Остается непонятным, сколько, собственно,"
  mss(3) = CHR(0)+ALLTRIM(c_name)+" нам заплатил..."
  mss(4) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF c_type
  SELECT CUS_BIL
  IF CUS_CODE # ACCOUNT.CUS_CODE
    APPEND BLANK
  ENDIF
  REPLACE CUS_CODE  WITH c_code,    ;
          CUS_NAME  WITH c_name,    ;
          ADDRESS   WITH p01,       ;
          PHONE     WITH p02,       ;
          ACC_BILL  WITH p03,       ;
          BANK      WITH p04,       ;
          CORR_BILL WITH p05,       ;
          BIK       WITH p06,       ;
          CITY      WITH p07,       ;
          INN       WITH p08,       ;
          OKOHX     WITH p09,       ;
          OKPO      WITH p10
ENDIF

SELECT BILL_F
IF mode_w = "add"
  doc_n = Doc_Num("BILL_F    ", f_c000, arm, DATE())
  doc_n = sys_char+STR(doc_n, FSIZE("DOC_NUM", "BILL_F")-1)
  doc_d = DATE()
  doc_t = TIME()
  APPEND BLANK
  REPLACE SUB_SYS   WITH sys_char,    ;
          FIRM      WITH f_c000,      ;
          DOC_NUM   WITH doc_n,       ;
          DOC_TIME  WITH doc_t,       ;
          WHO       WITH user,        ;
          WHERE     WITH arm
  rc_w = RECNO()
  SELECT TMP_LINK
  APPEND BLANK
  REPLACE RC_NO WITH rc_w
  GO RECNO()
ENDIF
SELECT BILL_F
REPLACE CUS_CODE  WITH c_code,    ;
        CUS_NAME  WITH c_name,    ;
        ADDRESS   WITH p01,       ;
        PHONE     WITH p02,       ;
        ACC_BILL  WITH p03,       ;
        DOC_DATE  WITH dt_000,    ;
        BANK      WITH p04,       ;
        CORR_BILL WITH p05,       ;
        BIK       WITH p06,       ;
        CITY      WITH p07,       ;
        INN       WITH p08,       ;
        OKOHX     WITH p09,       ;
        OKPO      WITH p10,       ;
        NOTE      WITH p11,       ;
        INV_NUM   WITH inv,       ;
        GOODS_COD WITH g_code,    ;
        GOODS     WITH g_name,    ;
        TOTAL_0   WITH money0,    ;
        NDS_      WITH prc_nds,   ;
        TOTAL_NDS WITH money_nds, ;
        TOTAL_R   WITH money

RETURN .T.
