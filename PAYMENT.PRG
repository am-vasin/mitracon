*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Payment      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                             Ввод оплаты.                               ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 20.01.1999 ══╝
PROCEDURE Payment
PARAMETER t_list, allDocs

PRIVATE max_ord, pl_code, show_p, show_a, show_f, show_b, tpList, pay_bill
PRIVATE ll

pay_bill = .T.
max_ord = 0
tpList = ""
IF TYPE("t_list") = "C"
  tpList = t_list
ENDIF
STORE .T. TO show_p, show_a
IF enab_firm
  show_f = ""
ELSE
  show_f = def_firm
ENDIF
IF enab_bank
  show_b = ""
ELSE
  show_b = def_bank
ENDIF
IF File_O(base_path+"MAX_ORD.MEM")
  RESTORE FROM (base_path+"MAX_ORD.MEM") ADDITIVE
ENDIF

*pl_code = G_Pl()
pl_code = "B"
IF EMPTY(pl_code)
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
PRIVATE s_sav         && Номер рабочей области для сохранения и восстановления!

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

s_sav = SELECT()
SELECT 0
DO Use_Dummy
SELECT 0
DO Use_Link
*ZAP

SELECT 0
USE (base_path+"CUS_NAME") ORDER TAG CUS_CODE ALIAS LNM_9120 AGAIN

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_9120 AGAIN
SET RELATION TO CUS_CODE  INTO LNM_9120

SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9120 AGAIN

SELECT 0
USE (base_path+"BANKS") ORDER TAG BANK ALIAS B_9120 AGAIN

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS WHE_9120 AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WHO_9120 AGAIN

SELECT 0
USE (base_path+"BILLS") ORDER TAG HRON ALIAS BLS_9120 AGAIN
IF .NOT. m.allDocs
  SET FILTER TO WHERE = m.arm
ENDIF
SET RELATION TO CUS_CODE  INTO CUS_9120, ;
                FIRM      INTO F_9120,   ;
                FIRM+BANK INTO B_9120,   ;
                WHO       INTO WHO_9120, ;
                WHERE     INTO WHE_9120
                

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4
DEFINE PAD Pay OF (menu_name) PROMPT "Оплата"
ON PAD Pay OF (menu_name) ACTIVATE POPUP Pay
  DEFINE POPUP Pay MARGIN RELATIVE SHADOW COLOR SCHEME 4
  DEFINE BAR  1 OF Pay PROMPT "Предварительные данные " KEY Ctrl-A, "F3"
  ON SELECTION BAR 1 OF Pay DO Sw_Mode WITH "prv_data"
  DEFINE BAR  2 OF Pay PROMPT "По счету " KEY Ctrl-A, "Enter"
  ON SELECTION BAR 2 OF Pay DO Sw_Mode WITH "int_pay"
  DEFINE BAR  3 OF Pay PROMPT "Без счета " KEY Ctrl-A, "Ins"
  ON SELECTION BAR 3 OF Pay DO Sw_Mode WITH "new_pay"
  DEFINE BAR  4 OF Pay PROMPT "Авансовые счета-фактуры "
  ON SELECTION BAR 4 OF Pay DO Sw_Mode WITH "stopDoc"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek_B
  DEFINE POPUP Seek_B MARGIN RELATIVE SHADOW COLOR SCHEME 4
  DEFINE BAR  1 OF Seek_B PROMPT "По клиенту " KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek_B DO Sw_Mode WITH "seek_c"
  DEFINE BAR  2 OF Seek_B PROMPT "По сумме " KEY Ctrl-A, "F8"
  ON SELECTION BAR 2 OF Seek_B DO Sw_Mode WITH "seek_s"
  DEFINE BAR  3 OF Seek_B PROMPT "Последние найденные" KEY Ctrl-A, "F9"
  ON SELECTION BAR 3 OF Seek_B DO Sw_Mode WITH "seek_l"
  DEFINE BAR  4 OF Seek_B PROMPT "Поиск по счету" && KEY Ctrl-A, "F9"
  ON SELECTION BAR 4 OF Seek_B DO Sw_Mode WITH "seek_b"

DEFINE PAD Cust OF (menu_name) PROMPT "Клиент"
ON PAD Cust OF (menu_name) ACTIVATE POPUP Cust
  DEFINE POPUP Cust MARGIN RELATIVE SHADOW COLOR SCHEME 4
  DEFINE BAR  4 OF Cust PROMPT "Длинное имя "
  ON SELECTION BAR 4 OF Cust DO Sw_Mode WITH "LName"
  DEFINE BAR  1 OF Cust PROMPT "История клиента " KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1OF Cust DO Sw_Mode WITH "CHist"
  DEFINE BAR  2 OF Cust PROMPT "Атрибуты клиента" KEY Ctrl-A, "Home"
  ON SELECTION BAR 2 OF Cust DO Sw_Mode WITH "CusBil"
  DEFINE BAR  3 OF Cust PROMPT "Маркетингоавая информ." KEY Ctrl-A, "Ctrl-Home"
  ON SELECTION BAR 3 OF Cust DO Sw_Mode WITH "CusInfo"

DEFINE PAD Print OF (menu_name) PROMPT "Счет" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "print"

DEFINE PAD Filter OF (menu_name) PROMPT "Фильтр" KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "filter"

DEFINE PAD Delete OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "delete"

DEFINE PAD Exit   OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit   OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 72
*┌Фрм──N док-та────Дата─────────────Сумма───Клиент────────────────────────┐
*│ФФФ ДДДДДДДДДДД ДД.ММ.ГГГГ 999_999_999.99 КККККККККККККККККККККККККККККК│
*└────────────────────────────────────────────────────────────────────────┘
*┌────────────────────────────────────────────────────────────────────────┐
*│       Предприятие 9999999999999999         Банк 9999999999999999       │
*│ Единицы           Состояние                  Аннулирован!              │
*│ Кто выписал счет 999999999999999999999999999999   АРМ 9999999999999999 │
*└────────────────────────────────────────────────────────────────────────┘
DO D_Wins WITH ln, wd, "Счета", 0, 3
@ 1, 2 SAY "Фрм──N док-та────Дата─────────────Сумма───Клиент"
@ WROWS()-4, 9 SAY "Предприятие                          Банк"
@ WROWS()-3, 3 SAY "Единицы           Состояние"
@ WROWS()-2, 3 SAY "Кто выписал счет                                  АРМ"
what_do = "List"

GO BOTTOM

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F3     DO Sw_Mode WITH "prv_data"
    ON KEY LABEL Ins    DO Sw_Mode WITH "new_pay"
    ON KEY LABEL F7     DO Sw_Mode WITH "seek_c"
    ON KEY LABEL F8     DO Sw_Mode WITH "seek_s"
    ON KEY LABEL F9     DO Sw_Mode WITH "seek_l"
    ON KEY LABEL Ctrl+P DO Sw_Mode WITH "print"
    ON KEY LABEL F6     DO Sw_Mode WITH "filter"
    ON KEY LABEL Del    DO Sw_Mode WITH "delete"
    ON KEY LABEL Tab    DO Sw_Mode WITH "CHist"
    ON KEY LABEL Home   DO Sw_Mode WITH "CusBil"
    ON KEY LABEL Ctrl-Home DO Sw_Mode WITH "CusInfo"

*
*    BROWSE - меню
*
    BROWSE FOR Flt_Val() REST ;
           FIELDS FIRM:3:H="",  ;
                  F000 = V_Doc(DOC_NUM):11:H="", ;
                  DOC_DATE:H="",                 ;
                  BILL_SUM:H="":P="999 999 999.99",;
                  F001 = Left_Cus(30):30:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "int_pay")
    ENDIF

  CASE what_do = "int_pay"    && Платеж по счету

    IF .NOT. EOF() .AND. .NOT. BOF()
      *
      *    Проверки
      *
      IF ALLTRIM(STATUS) = "Закрыт"
        PRIVATE mss_000
        DIMENSION mss_000(3)
        mss_000(1) = ""
        mss_000(2) = CHR(0)+"Этот счет недоступен для ввода платежа!"
        mss_000(3) = ""
        DO Out_Mess WITH 7, "mss_000"
        what_do = "List"
        LOOP
      ENDIF
      IF ALLTRIM(STATUS) = "Оплачено"
        PRIVATE mss_000
        DIMENSION mss_000(3)
        mss_000(1) = ""
        mss_000(2) = CHR(0)+"Этот счет уже оплачен. Будем вводить платеж? [ Нет ] [ Да ]"
        mss_000(3) = ""
        IF Out_Mess(7, "mss_000") = 1
          what_do = "List"
          LOOP
        ENDIF
      ENDIF
      IF OLD
        PRIVATE mss_000
        DIMENSION mss_000(3)
        mss_000(1) = ""
        mss_000(2) = CHR(0)+"Этот счет аннулирован. Будем вводить платеж? [ Нет ] [ Да ]"
        mss_000(3) = ""
        IF Out_Mess(7, "mss_000") = 1
          what_do = "List"
          LOOP
        ENDIF
      ENDIF
      IF Tst1_INN(CUS_CODE)
        DO IncMoney WITH FIRM, DOC_NUM, DOC_DATE, tpList
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "new_pay"    && Платеж без счета

    DO IncMoney WITH "", "", "", tpList
    what_do = "List"

  CASE what_do = "stopDoc"    && Авансовые счета-фактуры

    m.pay_bill = Set_PB(m.pay_bill)
    ACTIVATE WINDOW (win_name+"_M") SAME
    ll = FLOOR(WCOLS()-36)/2
    IF m.pay_bill
      @ WROWS()-5, ll SAY REPLICATE("─", 36) && COLOR (SCHEME(13,6))
    ELSE
      @ WROWS()-5, ll SAY "Авансовые счета фактуры не создаются" COLOR SCHEME 7
    ENDIF
    what_do = "List"

  CASE what_do = "CHist"      && История клиента

    IF .NOT. EMPTY(CUS_CODE)
      DO Acc_Hist WITH CUS_CODE
    ENDIF
    what_do = "List"

  CASE what_do = "LName"      && Длинное имя

    IF .NOT. EMPTY(CUS_CODE)
      DO Cus_Name WITH CUS_CODE
    ENDIF
    what_do = "List"

  CASE what_do = "CusBil"     && Атрибуты клиента

    IF .NOT. EMPTY(CUS_CODE)
      DO Cus_Bil WITH CUS_CODE
    ENDIF
    what_do = "List"

  CASE what_do = "CusInfo"    && Карточка маркетинга

    IF .NOT. EMPTY(CUS_CODE)
      DO Cli_Info WITH "", .F., CUS_CODE
    ENDIF
    what_do = "List"

  CASE what_do = "seek_c"     && Поиск по клиенту

    IF Seek_Cus()
      DO Ch_Last
    ENDIF
    what_do = "List"

  CASE what_do = "seek_b"     && Поиск по счету

    m.rec = Seek_Bl()
    IF m.rec # 0
      GO m.rec IN BLS_9120
    ENDIF
    what_do = "List"

  CASE what_do = "prv_data"   && Предварительные данные об оплате

    IF BLS_9120.STATUS = "Оплачено  "
      PRIVATE mss000
      DIMENSION mss000(3)
      mss000(1) = ""
      mss000(2) = CHR(0)+"С предварительными данными Вы опоздали: счет оплачен!"
      mss000(3) = ""
      DO Out_Mess WITH 7, "mss000"
    ELSE
      REPLACE BLS_9120.STATUS  WITH "Предв.", ;
              BLS_9120.WHO_INC WITH user,     ;
              BLS_9120.DAT_INC WITH DATE()
    ENDIF
    what_do = "List"

  CASE what_do = "seek_s"     && Поиск по сумме

    IF Seek_Sum()
      DO Ch_Last
    ENDIF
    what_do = "List"

  CASE what_do = "seek_l"     && Последнее найденое

    DO Ch_Last
    what_do = "List"

  CASE what_do = "print"      && Печать счета

    DO P_Bill WITH FIRM, DOC_NUM, DOC_DATE
    what_do = "List"

  CASE what_do = "filter"     && Установка фильтра

    DO Set_Flt
    what_do = "List"

  CASE what_do = "delete"     && Удаление счета

    DO Del_Bill
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
SELECT BLS_9120
USE

SELECT CUS_9120
USE

SELECT F_9120
USE

SELECT B_9120
USE

SELECT LNM_9120
USE

SELECT WHE_9120
USE

SELECT WHO_9120
USE

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot
PRIVATE w1, w2

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-4, 21 SAY F_9120.B_NAME COLOR (SCHEME(13,6))
@ WROWS()-4, 51 SAY B_9120.B_NAME COLOR (SCHEME(13,6))
@ WROWS()-3, 11 SAY IIF(BLS_9120.VALUTA = "$", "У Е", "Руб") COLOR (SCHEME(13,6))
@ WROWS()-3, 31 SAY BLS_9120.STATUS COLOR (SCHEME(13,6))
@ WROWS()-3, 48 SAY IIF(BLS_9120.OLD, "Аннулирован!", "            ") COLOR (SCHEME(13,6))
IF FOUND("LNM_9120")
  @ WROWS()-3, COL()+1 SAY "Дл. имя есть"
ELSE
  @ WROWS()-3, COL()+1 SAY "            "
ENDIF
@ WROWS()-3, 48 SAY IIF(BLS_9120.OLD, "Аннулирован!", "            ") COLOR (SCHEME(13,6))
DO CASE
CASE BLS_9120.WHO = 0
  w1 = "Сам!"
CASE .NOT. FOUND("WHO_9120")
  w1 = "Неизвестный!"
OTHERWISE
  w1 = ALLTRIM(WHO_9120.FAMILY)+" "+LEFT(WHO_9120.NAME,1)+". "+LEFT(WHO_9120.S_NAME,1)+"."
ENDCASE
w1 = PADR(w1,30)

DO CASE
CASE .NOT. FOUND("WHE_9120")
  w2 = "               "
OTHERWISE
  w2 = WHE_9120.ARM_N
ENDCASE

@ WROWS()-2, 20 SAY w1 COLOR (SCHEME(13,6))
@ WROWS()-2, 57 SAY w2 COLOR (SCHEME(13,6))
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Flt_Val      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Значение фильтра.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.01.1999 ──┘
PROCEDURE Flt_Val
IF pl_code # LEFT(BLS_9120.DOC_NUM,1)
  RETURN .F.
ENDIF
IF .NOT. show_p .AND. BLS_9120.STATUS = "Оплачено  "
  RETURN .F.
ENDIF
IF .NOT. show_a .AND. BLS_9120.OLD
  RETURN .F.
ENDIF
IF .NOT. BLS_9120.FIRM = show_f
  RETURN .F.
ENDIF
IF .NOT. BLS_9120.BANK = show_b
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Doc        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Визуальный номер документа.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.01.1999 ──┘
PROCEDURE V_Doc
PARAMETER doc_n

RETURN LEFT(doc_n,4)+"-"+ALLTRIM(RIGHT(doc_n,6))

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Left_Cus     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Обрезанное имя клиента.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.01.1999 ──┘
PROCEDURE Left_Cus
PARAMETERS len_nam

PRIVATE nm_w
IF EMPTY(BLS_9120.CUS_CODE) .OR. .NOT. FOUND("CUS_9120")
  nm_w = ALLTRIM(BLS_9120.CUS_NAME)
ELSE
  nm_w = ALLTRIM(CUS_9120.CUS_NAME)
ENDIF
IF LEN(nm_w) > len_nam
  nm_w = LEFT(nm_w, len_nam-1)+""
ELSE
  nm_w = PADR(nm_w, len_nam)
ENDIF
RETURN nm_w

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                      Программа выбора площадки.                       ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE G_Pl

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

PRIVATE ex, pl_w, ln, wd, c1, c2
                       &&   Объявляем и заполняем поля бланка
ex   = 1               &&
pl_w = 1               &&
USE (base_path+"PLACES")
ln = RECCOUNT()+5
wd = MAX(38, FSIZE("P_NAME")+12)
c1 = FLOOR((wd-FSIZE("P_NAME")-4)/2)
c2 = FLOOR((wd-32)/2)

*    Выберите площадку
*┌──────────────────────────────────┐
*│  ( ) Площадка 1                  │
*│  ( ) Площадка 2                  │
*│  ..............                  │
*│  ( ) Площадка N                  │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH ln, wd, "Выберите площадку"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO G_Place WITH 2, c1, pl_w
@ WROWS()-2, c2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  GO pl_w
  pl_w = P_CHAR
ELSE
  pl_w = ""
ENDIF
USE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN pl_w

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_Flt      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Установка фильтра.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.01.1999 ──┘
PROCEDURE Set_Flt
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

*┌───────────────────────────────────────┐
*│  [ ] Оплаченные   [ ] Аннулированные  │
*│                                       │
*│ Предприятие 999 9999999999999999 (F1) │
*│                                       │
*│       [ ] Банк 9999999999999999       │
*│                                       │
*│   < OK Ctrl-W > < Отказаться Esc >    │
*└───────────────────────────────────────┘

PRIVATE ex, w_p, w_a
PRIVATE f_c000, f_n000, b_c000, b_n000, b_sw000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 43, "Клиент"
w_p = show_p
w_a = show_a
f_c000 = PADR(show_f,3)
f_n000 = "                "
b_c000 = PADR(show_b,3)
b_n000 = "                "
b_sw000= .F.
f_c_old= f_c000
*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2,  4 GET w_p PICTURE "@*C Оплаченные"
@ 2, 21 GET w_a PICTURE "@*C Аннулированные"
DO Get_Firm WITH 4, 3, enab_firm
DO Get_Bank WITH 6, 9, enab_bank
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  show_p = w_p
  show_a = w_a
  show_f = IIF(EMPTY(f_c000), "", f_c000)
  show_b = IIF(EMPTY(b_c000), "", b_c000)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Del_Bill     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Удаление счета.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.01.1999 ──┘
PROCEDURE Del_Bill

PRIVATE k00, mss, s_sav, arr_w

IF EMPTY(INC_DOC) .AND. .NOT. OLD
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+" Этот счет не оплачен и не аннулирован! Удаляем? [ Нет ] [ Да ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 1
    RETURN
  ENDIF
ENDIF

k00 = FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM
SCATTER TO arr_w BLANK
GATHER FROM arr_w

s_sav = SELECT()
SELECT 0
USE (base_path+"BILLS_D") ORDER TAG DOC_NUM ALIAS B_D_9120 AGAIN
SCATTER TO arr_w BLANK
DO WHILE SEEK(k00)
  GATHER FROM arr_w
ENDDO
USE
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_Cus     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Поиск по клиенту.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.01.1999 ──┘
PROCEDURE Seek_Cus
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

*┌──────────────────────────────────────────────┐
*│ ┌─ Шаблон для поиска ──────────────────────┐ │
*│ │ ....:....!....:....!....:....!....:....! │ │
*│ └──────────────────────────────────────────┘ │
*│       < OK Ctrl-W > < Отказаться Esc >       │
*└──────────────────────────────────────────────┘

PRIVATE ex, seek_sh, nm_w, ret
seek_sh = SPACE(40)
ex = 1
ret = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 50, "Поиск счета по клиенту"
DO Sun_Bord WITH  2,  3,  4, 46, " Шаблон для поиска "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3, 5 GET seek_sh
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1 .AND. .NOT. EMPTY(seek_sh)
  *
  * Отрабатываем бланк
  seek_sh = ALLTRIM(SYS(15,lwr,seek_sh))
  PRIVATE r_sav
  r_sav = RECNO()
  DO Wt_Mess WITH "Ищем..."
  SELECT LINK
  ZAP
  SELECT BLS_9120
  SCAN
    IF EMPTY(BLS_9120.CUS_CODE) .OR. .NOT. FOUND("CUS_9120")
      nm_w = ALLTRIM(BLS_9120.CUS_NAME)
    ELSE
      nm_w = ALLTRIM(CUS_9120.CUS_NAME)
    ENDIF
    IF seek_sh $ SYS(15, lwr, nm_w)
      SELECT LINK
      APPEND BLANK
      REPLACE LNK WITH RECNO("BLS_9120")
      SELECT BLS_9120
    ENDIF
  ENDSCAN
  ret = RECCOUNT("LINK") # 0
  IF BETWEEN(r_sav,1,RECCOUNT())
    GO r_sav
  ENDIF
  DO Wt_Mess
  IF .NOT. ret
    PRIVATE mss33
    DIMENSION mss33(3)
    mss33(1) = ""
    mss33(2) = CHR(0)+"Ничего не найдено..."
    mss33(3) = ""
    DO Out_Mess WITH 7, "mss33"
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ret

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Last      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Просмотр найденного и выбор.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.01.1999 ──┘
PROCEDURE Ch_Last
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
PRIVATE s_sav         && Номер рабочей области для сохранения и восстановления!

IF RECCOUNT("LINK") = 0
  PRIVATE mss22
  DIMENSION mss22(3)
  mss22(1) = ""
  mss22(2) = CHR(0)+"Ничего не было найдено"
  mss22(3) = ""
  DO Out_Mess WITH 7, "mss22"
  RETURN
ENDIF
  
*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit   OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit   OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, r_sav
ln = MIN(WROWS("")-11, RECCOUNT("LINK"))   && Количество видимых строк BROWSE
wd = 72
*┌Фрм──N док-та────Дата───────────Сумма───Клиент──────────────────────────┐
*│ФФФ ДДДДДДДДДДД ДД.ММ.ГГГГ 9_999_999.99 КККККККККККККККККККККККККККККККК│
*└────────────────────────────────────────────────────────────────────────┘
*┌────────────────────────────────────────────────────────────────────────┐
*│       Предприятие 9999999999999999         Банк 9999999999999999       │
*│       Единицы           Состояние                  Аннулирован!        │
*│ Кто выписал счет 999999999999999999999999999999   АРМ 9999999999999999 │
*└────────────────────────────────────────────────────────────────────────┘
DO D_Wins WITH ln, wd, "Результат поиска...", 0, 3
@ 1, 2 SAY "Фрм──N док-та────Дата───────────Сумма───Клиент"
@ WROWS()-4, 9 SAY "Предприятие                          Банк"
@ WROWS()-3, 9 SAY "Единицы           Состояние"
@ WROWS()-2, 3 SAY "Кто выписал счет                                  АРМ"

what_do = "List"
r_sav = RECNO()
SET ORDER TO
SELECT LINK
SET RELATION TO LNK INTO BLS_9120
GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS BLS_9120.FIRM:3:H="",  ;
                  F000 = V_Doc(BLS_9120.DOC_NUM):11:H="", ;
                  BLS_9120.DOC_DATE:H="",                 ;
                  BLS_9120.BILL_SUM:H="":P="9 999 999.99",;
                  F001 = Left_Cus(32):32:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    IF .NOT. Flt_Val()
      PRIVATE mss22
      DIMENSION mss22(4)
      mss22(1) = ""
      mss22(2) = CHR(0)+"Выбраный Вами счет лежит за пределами фильтра"
      mss22(3) = CHR(0)+"или создан на другой площадке!"
      mss22(4) = ""
      DO Out_Mess WITH 7, "mss22"
      what_do = "List"
    ELSE
      SET RELATION TO
      EXIT
    ENDIF

  OTHERWISE

    SET RELATION TO
    IF BETWEEN(r_sav, 1, RECCOUNT("BLS_9120"))
      GO r_sav IN BLS_9120
    ENDIF
    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
SELECT BLS_9120
SET ORDER TO HRON

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_Sum     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Поиск по сумме.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.01.1999 ──┘
PROCEDURE Seek_Sum
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

*┌───────────────────────────────────┐
*│   Сумма для поиска 9 999 999.99   │
*│                                   │
*│ < OK Ctrl-W > < Отказаться Esc >  │
*└───────────────────────────────────┘

PRIVATE ex, seek_sum, ret
seek_sum = 0
ex = 1
ret = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 39, "Поиск счета по сумме"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 5 SAY "Сумма для поиска" GET seek_sum PICTURE "@Z 9 999 999.99"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1 .AND. .NOT. EMPTY(seek_sum)
  *
  * Отрабатываем бланк
  PRIVATE r_sav
  r_sav = RECNO()
  DO Wt_Mess WITH "Ищем..."
  SELECT LINK
  ZAP
  SELECT BLS_9120
  SCAN
    IF seek_sum = BILL_SUM
      SELECT LINK
      APPEND BLANK
      REPLACE LNK WITH RECNO("BLS_9120")
      SELECT BLS_9120
    ENDIF
  ENDSCAN
  ret = RECCOUNT("LINK") # 0
  IF BETWEEN(r_sav,1,RECCOUNT())
    GO r_sav
  ENDIF
  DO Wt_Mess
  IF .NOT. ret
    PRIVATE mss33
    DIMENSION mss33(3)
    mss33(1) = ""
    mss33(2) = CHR(0)+"Ничего не найдено..."
    mss33(3) = ""
    DO Out_Mess WITH 7, "mss33"
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ret
