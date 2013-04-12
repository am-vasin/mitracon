*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Order1       Разработчик Андрей Васин           24.04.98 17:20:08 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                    Формирование заявки на поставку.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Order1

*
*  Переменные состояния для навигации
*
PRIVATE old_sale
old_sale = ALLTRIM(GET_SYS("OLD_SALE"))

PRIVATE stat_type     && Тип навигации: 0 - нестандартный;
                                        1 - бланк;
                                        2 - BROWSE - таблица;
                                        3 - BROWSE - список.
PRIVATE what_do       && Имя режима.
PRIVATE menu_name     && Имя асинхронного меню.
PRIVATE last_mouse    && Время последнего нажатия звериной кнопки.
PRIVATE win_name      && Имя окна ( окон для BROWSE ).
PRIVATE sys_sav       && Состояние системного меню

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()
sys_sav = SET("SYSMENU")
SET SYSMENU OFF

***********************************************************************
*
*     Параметры заказа
*
**************  Общие  **************************************
PRIVATE __srok, __valuta, __over, __stop_ord
__srok    = 60        && Количество дней для анализа спроса и продажи
__valuta  = "$"       && валюта
__over = .T.          && Предварительно очистить заказ
__stop_ord = .T.      && Учесть флажки "не заказывать"

**************  Продажа  ************************************
PRIVATE __min_p, __min_p_v, __c_scale
__min_p   = 100       && Минимальная сумма для позиции
DIMENSION __c_scale(10, 3)    && Таблица градаций для MIN и MAX запаса
__c_scale = 0             && свыше...
__c_scale(1, 2) = 20      && MIN запас в днях
__c_scale(1, 3) = 70      && MAX запас в днях

**************  Спрос  *************************************
PRIVATE __koeff_q, __koeff_n, __porog
__koeff_q = 0       && Коэффициент для количества деталей
__koeff_n = 2       && Коэффициент для количества запросов
__porog   = 5       && Порог для включения спроса
__min_s   = 0       && Минимальная сумма заказа
__max_s   = 10000   && Максимальная сумма заказа

**************  Давно отсутствующие  ***********************
PRIVATE __abs_q, __abs_age
__abs_q = 1   && Количество
__abs_age = __srok  && Срок
**************  Новые  *************************************
PRIVATE __new_q, __new_d
__new_q = 1   && Количество
__new_d = 30  && "Возраст" в днях

**************  Фирма-производитель ************************
PRIVATE __fir_q, fir_code
__fir_q = 1   && Количество
* Код фирмы всегда надо указать!
fir_code = 0

**************  Из файла  **********************************
PRIVATE __f_name
__f_name = ""     && Нет умолчания!
IF File_O(tmp_path+"MEM00.MEM")
  RESTORE FROM (tmp_path+"MEM00") ADDITIVE
ENDIF
*
*
************************************************************

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
PRIVATE sup_cd, sup_nm, sup_cnt, r_pic
DO Use_Dummy

SELECT 0
USE (base_path+"CUSTOMER")
sup_cnt = 0
SCAN FOR SUP # " "
  sup_cnt = sup_cnt+1
  DIMENSION sup_cd(sup_cnt), sup_nm(sup_cnt)
  sup_cd(sup_cnt) = CODE
  sup_nm(sup_cnt) = B_NAME
ENDSCAN
IF sup_cnt = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не завели ни одного поставщика!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

IF File_O (base_path+"ACCOUNT.DBF")
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
ELSE
  USE (base_path+"CUSTOMER") ORDER TAG CODE
ENDIF

IF File_O(base_path+"OFFER.DBF")
  SELECT 0
  USE (base_path+"OFFER.DBF") ORDER TAG SUP_CODE
ENDIF
SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME ALIAS STO2748 AGAIN
r_pic = IIF(Mnt_Size("PRICE_R") = 0, "@Z 99999999", "@Z 99999.99")

SELECT 0
USE (base_path+"ORD_T_L") ORDER TAG EMPTY
IF USED("ACCOUNT")
  SET RELATION TO SUP_CODE INTO ACCOUNT
ELSE
  SET RELATION TO SUP_CODE INTO CUSTOMER
ENDIF
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr_L OF (menu_name) PROMPT "Заявка" KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr_L OF (menu_name) DO Sw_Mode WITH "Corr_L"

DEFINE PAD Title OF (menu_name) PROMPT "Заголовок"
ON PAD Title OF (menu_name) ACTIVATE POPUP Title

  DEFINE POPUP Title MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Title PROMPT "Создать" KEY Ctrl-A, "Ins"
  ON SELECTION BAR 1 OF Title DO Sw_Mode WITH "Create"

  DEFINE BAR  2 OF Title PROMPT "Корректировать" KEY Ctrl-A, "F1"
  ON SELECTION BAR 2 OF Title DO Sw_Mode WITH "Corr_T"

  DEFINE BAR  3 OF Title PROMPT "Удалить" KEY Ctrl-A, "Del"
  ON SELECTION BAR 3 OF Title DO Sw_Mode WITH "Delete"

DEFINE PAD Document OF (menu_name) PROMPT "Документ" KEY Ctrl-A, "Tab"
ON SELECTION PAD Document OF (menu_name) DO Sw_Mode WITH "Document"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = 10      && Количество видимых строк BROWSE
wd = 73

DO D_Wins WITH ln, wd, "Заготовки заявок", 0, 0
@ 1, 2 SAY ;
"──Фрм─Поставщик──────────────────────────────────────────Примечание"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1  DO Sw_Mode WITH "Corr_T"
    ON KEY LABEL Ins DO Sw_Mode WITH "Create"
    ON KEY LABEL Del DO Sw_Mode WITH "Delete"
    ON KEY LABEL Tab DO Sw_Mode WITH "Document"
    SELECT OFFER
    SET ORDER TO TAG SUP_CODE
    SELECT ORD_T_L

*
*    BROWSE - меню
*
    BROWSE KEY "*"   ;
           FIELDS F001=IIF(EMPTY(WHO), " ", "√"):H="",  ;
                  FIRM:H="",    ;
                  F002=IIF(USED("ACCOUNT"), ACCOUNT.CUS_NAME, CUSTOMER.L_NAME):50:H="", ;
                  NOTE:H="",  ;
                  DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Corr_L")
    ENDIF

  CASE what_do = "Corr_L"    && Коррекция списка

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Set_Qnt WITH "ORD"+ALLTRIM(STR(RECNO("ORD_T_L"))), "MEM"+ALLTRIM(STR(RECNO("ORD_T_L")))
    ENDIF
    what_do = "List"

  CASE what_do = "Delete"    && Удаление списка

    IF .NOT. BOF() .AND. .NOT. EOF()
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Удаляем? Точно?! [ Да ] [ Нет ]"
      mss(3) = ""
      IF Out_Mess(5,"mss") = 1
        REPLACE SUP_CODE WITH 0
        DELETE FILE (tmp_path+"ORD"+ALLTRIM(STR(RECNO()))+".DBF")
        DELETE FILE (tmp_path+"ORD"+ALLTRIM(STR(RECNO()))+".CDX")
        DELETE FILE (tmp_path+"MEM"+ALLTRIM(STR(RECNO()))+".MEM")
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Document"  && Создать документ

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Mk_Ord
    ENDIF
    what_do = "List"

  CASE what_do = "Corr_T"    && Коррекция заголовка

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO C_Title
    ENDIF
    what_do = "List"

  CASE what_do = "Create"    && Добавление заголовка

    DO C_Title WITH .T.
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED
SET SYSMENU &sys_sav

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                  Создание/коррекция заголовка списка.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 24.04.98 17:58:27 ═╝
PROCEDURE C_Title
PARAMETER new
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
*IF .NOT. new
*  IF .NOT. Tst_Title()
*    RETURN
*  ENDIF
*ENDIF
PRIVATE o_num, o_dat, w_usd, s0001, s_sav
PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌───────────────────────────────────────┐
*│ Предприятие 999 9999999999999999 (F1) │
*│               ┌────────────────┐      │
*│     Поставщик │....:....!....:.│      │
*│               └────────────────┘      │
*│     Примечание ....:....!....:.       │
*│                                       │
*│   < OK Ctrl-W > < Отказаться Esc >    │
*└───────────────────────────────────────┘

PRIVATE ex, nt, sup_point
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
IF new
  f_c000 = def_firm
  f_n000 = ""
  f_c_old = def_firm
  sup_point = 1
  nt        = SPACE(16)
ELSE
  f_c000 = FIRM
  f_n000 = ""
  f_c_old = FIRM
  sup_point = ASCAN(sup_cd, ORD_T_L.SUP_CODE)
  nt        = ORD_T_L.NOTE
ENDIF

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 43, IIF(new, "Создание заголовка", "Коррекция заголовка")
@ 4, 7 SAY "Поставщик"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 17 GET sup_point PICTURE "@^ " FROM sup_nm SIZE 1,18 WHEN new
@ 6,  7 SAY "Примечание" GET nt
@ 8,  5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
DO Get_Firm WITH 2, 3, enab_firm
READ CYCLE

IF ex = 1
  SELECT ORD_T_L
  IF new
    IF EOF() .OR. BOF()
      APPEND BLANK
      = LOCK()
    ELSE
      GO TOP
      DO WHILE EMPTY(SUP_CODE)
        IF LOCK()
          EXIT
        ENDIF
        GO TOP
      ENDDO
      IF .NOT. EMPTY(SUP_CODE)
        APPEND BLANK
        = LOCK()
      ENDIF
    ENDIF
  ENDIF
  REPLACE FIRM     WITH f_c000,            ;
          SUP_CODE WITH sup_cd(sup_point), ;
          NOTE     WITH nt
  GO RECNO()
  IF new
    f_name = "ORD"+ALLTRIM(STR(RECNO()))
    DO Prep_Tmp WITH f_name
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                  Подготовка номенклатуры для заказа.                   ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 27.04.98 13:23:09 ═╝
PROCEDURE Prep_Tmp
PARAMETER f_name
PRIVATE s_sav, cnt, n, flg_sj

DELETE FILE (tmp_path+f_name+".DBF")
DELETE FILE (tmp_path+f_name+".CDX")

s_sav = SELECT()
flg_sj = .F.

SELECT 0
CREATE DBF (tmp_path+f_name) ;
  ( CODE      N( 7),  ;
    OFFER_REC N(15),  ;
    PREFIX    C( 4),  ;
    NAME      C(35),  ;
    PRODUCER  C( 8),  ;
    QNT       N( 6),  ;
    QNT_DUE   N( 6),  ;
    QNT_SALE  N( 6),  ;
    QNT_DEM_N N( 6),  ;
    QNT_DEM_Q N( 6),  ;
    QNT_STORE N(10),  ;
    LAST_SALE D,      ;
    OWN       L,      ;
    SEGMENT   C( 6),  ;
    OWN_STO   C( 1)  )

USE (tmp_path+f_name) EXCLUSIVE ALIAS ORD_TMP

IF File_O(path_comm+"SUBJECT.DBF")
  SELECT 0
  USE (path_comm+"SUBJECT.DBF") ORDER STO_CODE ALIAS SJ_2802 AGAIN
ENDIF

SELECT STO2748
IF USED("SJ_2802")
  SET RELATION TO CODE INTO SJ_2802
  flg_sj = .T.
ENDIF

DO Wt_Mess WITH "Номенклатура..."
cnt = 0
SCAN
  cnt = cnt+1
  IF cnt % 100 = 0
    @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)),WCOLS())
  ENDIF
  SELECT ORD_TMP
  APPEND BLANK
  REPLACE CODE      WITH STO2748.CODE,     ;
          PREFIX    WITH STO2748.PREFIX,   ;
          NAME      WITH STO2748.NAME,     ;
          PRODUCER  WITH STO2748.PRODUCER, ;
          QNT_STORE WITH STO2748.QNT_STORE,;
          OWN       WITH .T.,              ;
          SEGMENT   WITH IIF(flg_sj,SJ_2802.SEGMENT,""),;
          OWN_STO   WITH IIF(flg_sj,SJ_2802.DEP_MAN,"")
  SELECT STO2748
ENDSCAN

SELECT ORD_TMP
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
INDEX ON NAME+PREFIX+PRODUCER FOR QNT # 0 TAG NAME1 ADDITIVE
INDEX ON IIF(CODE#0, CODE, -OFFER_REC) TAG CODE ADDITIVE

SELECT STO2748
SET ORDER TO TAG CODE

SELECT 0
USE (tmp_path+f_name) ORDER TAG NAME ALIAS TMP1 AGAIN

DO Wt_Mess

SELECT 0
IF USED("OFFER")
  DO Wt_Mess WITH "Предложение..."
  cnt = 0
  SELECT OFFER
  SEEK ORD_T_L.SUP_CODE
  SCAN REST WHILE ORD_T_L.SUP_CODE = SUP_CODE
    cnt = cnt+1
    IF cnt % 100 = 0
      @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)),WCOLS())
    ENDIF
    IF CODE # 0
      IF SEEK(CODE,"ORD_TMP")
        REPLACE PREFIX   WITH ORD_TMP.PREFIX,   ;
                NAME     WITH ORD_TMP.NAME,     ;
                PRODUCER WITH ORD_TMP.PRODUCER
        SELECT ORD_TMP
        REPLACE OFFER_REC WITH RECNO("OFFER")
        SELECT OFFER
      ELSE
        SELECT ORD_TMP
        APPEND BLANK
        REPLACE CODE      WITH OFFER.CODE,     ;
                PREFIX    WITH OFFER.PREFIX,   ;
                NAME      WITH OFFER.NAME,     ;
                PRODUCER  WITH OFFER.PRODUCER, ;
                OFFER_REC WITH RECNO("OFFER")
        SELECT OFFER
      ENDIF
    ELSE
      IF SEEK(NAME+PREFIX+PRODUCER,"TMP1")
        REPLACE CODE     WITH TMP1.CODE
        SELECT TMP1
        REPLACE OFFER_REC WITH RECNO("OFFER")
        SELECT OFFER
      ELSE
        SELECT TMP1
        APPEND BLANK
        REPLACE PREFIX   WITH OFFER.PREFIX,   ;
                NAME     WITH OFFER.NAME,     ;
                PRODUCER WITH OFFER.PRODUCER, ;
                OFFER_REC WITH RECNO("OFFER")
        SELECT OFFER
      ENDIF
    ENDIF
  ENDSCAN
  SET ORDER TO
  DO Wt_Mess
ENDIF

IF USED("SJ_2802")
  SELECT SJ_2802
  USE
ENDIF

SELECT TMP1
USE

SELECT ORD_TMP
USE

SELECT (s_sav)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Коррекция списка.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 28.04.98 14:14:24 ═╝
PROCEDURE Set_Qnt
PARAMETER f_name, m_name
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

PRIVATE d1, d2, d_s, is_all, e_sav, e_num, l_sav, fsw, flt_0, flt_r, flt_s
*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
STORE {} TO d1, d2
is_all = .T.
fsw = 1
flt_0 = .F.
flt_r = ""
flt_s = ""

SELECT 0
USE (base_path+"PARMS") ALIAS P2948 AGAIN
d_s = DAT_START

e_num = 0
e_sav = ON("ERROR")
ON ERROR e_num = ERROR()
USE (tmp_path+f_name) ORDER TAG NAME ALIAS ORD_TMP EXCLUSIVE
ON ERROR &e_sav
IF e_num # 0
  IF INLIST(e_num, 108, 1705)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"С этим заказом уже работают!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Ошибка "+ALLTRIM(STR(e_num))+" при попытке открыть файл с заявкой!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
  USE
  SELECT ORD_T_L
  RETURN
ENDIF
SELECT STO2748
SET ORDER TO TAG CODE
SELECT ORD_TMP
SET RELATION TO CODE INTO STO2748

IF File_O(tmp_path+m_name+".MEM")
  RESTORE FROM (tmp_path+m_name) ADDITIVE
ENDIF
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Hole OF (menu_name) PROMPT "Заказ" KEY Ctrl-A, "F2"
ON SELECTION PAD Hole OF (menu_name) DO Sw_Mode WITH "Hole"

DEFINE PAD Calc OF (menu_name) PROMPT "Расчет"
ON PAD Calc OF (menu_name) ACTIVATE POPUP Calc

  DEFINE POPUP Calc MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR 1 OF Calc PROMPT "Параметры" KEY Ctrl-A, "Ctrl-Enter"
  ON SELECTION BAR 1 OF Calc DO Sw_Mode WITH "Parms"

  DEFINE BAR 2 OF Calc PROMPT "Рекомендуемый заказ" KEY Ctrl-A, ""
  ON SELECTION BAR 2 OF Calc DO Sw_Mode WITH "Order"

  DEFINE BAR 3 OF Calc PROMPT "Сумма по заявке" KEY Ctrl-A, "F3"
  ON SELECTION BAR 3 OF Calc DO Sw_Mode WITH "Ord_Sum"

  DEFINE BAR 4 OF Calc PROMPT "Округление до нормы упаковки" KEY Ctrl-A, "F5"
  ON SELECTION BAR 4 OF Calc DO Sw_Mode WITH "Round_U"

DEFINE PAD Fltr OF (menu_name) PROMPT "Фильтр"
ON PAD Fltr OF (menu_name) ACTIVATE POPUP Fltr

  DEFINE POPUP Fltr MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR 1 OF Fltr PROMPT "По сегменту рынка" KEY Ctrl-A, "F6"
  ON SELECTION BAR 1 OF Fltr DO Sw_Mode WITH "Fltr"

  DEFINE BAR 2 OF Fltr PROMPT "По наличию у поставщика" KEY Ctrl-A, "F4"
  ON SELECTION BAR 2 OF Fltr DO Sw_Mode WITH "Qnt_0"

DEFINE PAD Info OF (menu_name) PROMPT "Информация"
ON PAD Info OF (menu_name) ACTIVATE POPUP Info

  DEFINE POPUP Info MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR 1 OF Info PROMPT "Продажа" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Info DO Sw_Mode WITH "Sale"

  DEFINE BAR 2 OF Info PROMPT "Спрос" KEY Ctrl-A, "Ctrl-F1"
  ON SELECTION BAR 2 OF Info DO Sw_Mode WITH "Demand"

  DEFINE BAR 3 OF Info PROMPT "Предложение" KEY Ctrl-A, "Shift-F1"
  ON SELECTION BAR 3 OF Info DO Sw_Mode WITH "Offer"

  DEFINE BAR 4 OF Info PROMPT "Бронь" KEY Ctrl-A, "F7"
  ON SELECTION BAR 4 OF Info DO Sw_Mode WITH "Bron"

  DEFINE BAR 5 OF Info PROMPT "Сегмент, фукц. назн...." KEY Ctrl-A, "Ctrl-F7"
  ON SELECTION BAR 5 OF Info DO Sw_Mode WITH "Segment"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, tit
ln = 12   && Количество видимых строк BROWSE
wd = 66
DO D_Wins WITH ln, wd, "Заявка: "+  ;
        ALLTRIM(IIF(USED("ACCOUNT"), ACCOUNT.CUS_NAME, CUSTOMER.L_NAME)),  ;
        1, IIF(EMPTY(d1), 4, 5)
@ 2, 12 SAY "Позиция"
@ 2, 50 SAY "Цена"
@ 2, 55 SAY "Налич."
@ 2, 63 SAY "Заказ"
@ WROWS()-4, 2 SAY PADC("Поставщик", WCOLS()-4)

@ 1, 2 SAY "Поиск Tab"
what_do = "List"
GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F2  DO Sw_Mode WITH "Hole"
    ON KEY LABEL Tab DO Seek_1
    ON KEY LABEL Ctrl+Enter  DO Sw_Mode WITH "Parms"
    ON KEY LABEL Ctrl+F1     DO Sw_Mode WITH "Demand"
    ON KEY LABEL Shift+F1    DO Sw_Mode WITH "Offer"
    ON KEY LABEL F1          DO Sw_Mode WITH "Sale"
    ON KEY LABEL F3          DO Sw_Mode WITH "Ord_Sum"
    ON KEY LABEL F4          DO Sw_Mode WITH "Qnt_0"
    ON KEY LABEL F5          DO Sw_Mode WITH "Round_U"
    ON KEY LABEL F6          DO Sw_Mode WITH "Fltr"
    ON KEY LABEL F7          DO Sw_Mode WITH "Bron"
    ON KEY LABEL Ctrl+F7     DO Sw_Mode WITH "Segment"

*
*    BROWSE - меню
*
    BROWSE FIELDS STO2748.WK_SW1:H="", F000 = IIF(OWN," ", "-"):H="", ;
           PREFIX:H="",  ;
           Brw_Name=Nm_Part(NAME):25:H="",;
           PRODUCER:H="",  ;
           STO2748.PRICE_R:H="":P=r_pic, ;
           STO2748.QNT:H="":P="@Z 999999", ;
           QNT:H="":P="@Z 999999" FREEZE QNT ;
           WHEN Show_F() ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "Hole"    && Переключение список/номенклатура

    IF is_all
      DEFINE PAD Hole OF (menu_name) PROMPT "Номенклатура" KEY Ctrl-A, "F2"
      SET ORDER TO TAG NAME1
    ELSE
      DEFINE PAD Hole OF (menu_name) PROMPT "Заказ" KEY Ctrl-A, "F2"
      SET ORDER TO TAG NAME
    ENDIF
    is_all = .NOT. is_all
    what_do = "List"

  CASE what_do = "Ord_Sum" && Стоимость заказа

    DO Ord_Sum
    what_do = "List"

  CASE what_do = "Sale"    && Информация о продажах

    IF .NOT. EOF() .AND. .NOT. BOF()
      DO Sale_Inf WITH ORD_TMP.CODE
      SELECT ORD_TMP
    ENDIF
    what_do = "List"

  CASE what_do = "Bron"    && Информация о брони

    IF .NOT. EOF() .AND. .NOT. BOF()
      DO View_Br WITH STO2748.CODE
    ENDIF
    what_do = "List"

  CASE what_do = "Order"    && Рекомендуемый заказ

    DO Clc_Ord
    what_do = "List"

  CASE what_do = "Parms"    && Расчет параметров
    PRIVATE flt_sav
    flt_sav = FILTER()
    SET FILTER TO
    l_sav = EMPTY(d1)
    DO Clc_Prms
    IF l_sav .AND. .NOT. EMPTY(d1)
      ZOOM WINDOW (win_name+"_E") NORM SIZE WROWS(win_name+"_E")-1, WCOLS(win_name+"_E")
      ZOOM WINDOW (win_name+"_I") NORM SIZE WROWS(win_name+"_I")-1, WCOLS(win_name+"_I")
      ACTIVATE WINDOW (win_name+"_M")
      SCROLL WROWS()-8, 0, WROWS()-5, WCOLS()-1, 1
      @ WROW()-5, 1 SAY "│"
      @ WROW()-5, WCOLS()-2 SAY "│" COLOR (SCHEME(14,2))
      DEFINE BAR 4 OF Info PROMPT "Файл для анализа заказа"
      ON SELECTION BAR 4 OF Info DO Sw_Mode WITH "Sav_File"
    ENDIF
    SET FILTER TO &flt_sav 
    what_do = "List"

  CASE what_do = "Sav_File"   && Файл для анализа

    PRIVATE sw_all
    sw_all = 1
    IF P_File()
      sw_all = sw_all=1
      DO P_File1
    ENDIF
    what_do = "List"

  CASE what_do = "Segment"   && Информация о сегменте, функц. назн. и т. д.

    IF .NOT. EOF() .AND. .NOT. BOF() .AND. ORD_TMP.CODE # 0 
      DO Sto_Add WITH ORD_TMP.CODE, .T.
    ENDIF
    what_do = "List"

  CASE what_do = "Demand"   && Спрос

    IF .NOT. EOF() .AND. .NOT. BOF()
      DO Dem_Inf WITH ORD_TMP.CODE
    ENDIF
    what_do = "List"

  CASE what_do = "Offer"    && Предложение

    DO Off_Inf WITH CODE, NAME+PREFIX
    what_do = "List"
    
  CASE what_do = "Round_U"      && Округление до нормы упаковки

    DO Round_U
    what_do = "List"

  CASE what_do = "Fltr"         && Фильтр
    DO CASE 
      CASE fsw = 1
        IF .NOT. flt_0
          SET FILTER TO .NOT. SEGMENT$"произв#разраб#      "
          flt_r = FILTER()
          @ 1, 35 SAY "                "
          @ 1, 54 SAY '"Ремонтные"   ' COLOR (SCHEME(14,2))
        ELSE
          SET FILTER TO .NOT. SEGMENT$"произв#разраб#      " .AND. QNT_STORE # 0
          @ 1, 35 SAY "Налич.у пост.# 0"
          @ 1, 54 SAY '"Ремонтные"   ' COLOR (SCHEME(14,2))
        ENDIF
        fsw =  2
        what_do = "List"
      CASE fsw = 2  
        IF .NOT. flt_0
          SET FILTER TO SEGMENT$"произв#разраб#      "
          flt_r = FILTER()
          @ 1, 35 SAY "                "
          @ 1, 54 SAY '"Не ремонтные"' COLOR (SCHEME(14,2))
        ELSE
          SET FILTER TO SEGMENT$"произв#разраб#      " .AND. QNT_STORE # 0
          flt_r = FILTER()
          @ 1, 35 SAY "Налич.у пост.# 0"
          @ 1, 54 SAY '"Не ремонтные"' COLOR (SCHEME(14,2))
        ENDIF
        fsw =  3
        what_do = "List"
      CASE fsw = 3
        IF .NOT. flt_0
          SET FILTER TO
          flt_r = ""
          @ 1, 35 SAY "                "
          @ 1, 54 SAY '"ВСЕ"          ' COLOR (SCHEME(14,2))
        ELSE
          @ 1, 35 SAY "Налич.у пост.# 0"
          SET FILTER TO QNT_STORE # 0
          @ 1, 54 SAY '"ВСЕ"          ' COLOR (SCHEME(14,2))
        ENDIF
        fsw =  1
        what_do = "List"

    OTHERWISE

      what_do = "List"

    ENDCASE

  CASE what_do = "Qnt_0"      && Фильтр по нулевому наличию у поставщика
    
    IF .NOT. flt_0
      flt_0 = .T.
      @ 1, 35 SAY "Налич.у пост.# 0"
      IF .NOT. EMPTY(flt_r)
*        flt_s = "QNT_STORE # 0"
*        SET FILTER TO &flt_s .AND. &flt_r
        SET FILTER TO QNT_STORE # 0 .AND. &flt_r
      ELSE
        SET FILTER TO QNT_STORE # 0
      ENDIF
    ELSE
      flt_0 = .F.
      @ 1, 35 SAY "                "
      IF .NOT. EMPTY(flt_r)
        SET FILTER TO &flt_r
      ELSE
        SET FILTER TO
      ENDIF
    ENDIF
    what_do = "List"

  OTHERWISE

    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  В самом деле выходим? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(5, "mss") = 2
      what_do = "List"
      LOOP
    ENDIF
    SAVE ALL LIKE __* TO (tmp_path+m_name)
    SAVE ALL LIKE __* TO (tmp_path+"MEM00")
    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
SELECT ORD_TMP
USE
SELECT ORD_T_L

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Поиск по начальным символам!                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.04.98 11:51:11 ═╝
PROCEDURE Seek_1
PRIVATE flt_pos, max_len, str_flt, s_n, s_c, ins_flt
PRIVATE chr_shab, i, pos_new, str_new, to_seek, rc_sav

DEFINE WINDOW Seek_Hide FROM 0, 0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW Seek_Hide
flt_pos = 0
max_len = FSIZE("NAME")-1
str_flt = ""
s_n = 0
s_c = ""
ins_flt = INSMODE()
chr_shab = ""
FOR i = 32 TO 126
  chr_shab = chr_shab+CHR(i)
ENDFOR
FOR i = 128 TO 175
  chr_shab = chr_shab+CHR(i)
ENDFOR
FOR i = 224 TO 241
  chr_shab = chr_shab+CHR(i)
ENDFOR

ACTIVATE WINDOW (win_name+"_M") SAME

DO WHILE .T.
  to_seek = .F.
  @ 1, 8 SAY PADR(str_flt, max_len+1, "▒") COLOR SCHEME 1
  @ 1, 8+flt_pos SAY ""
  s_n = INKEY(0)
  IF s_n < 0       && Функциональная клавиша - мимо тазика!
    LOOP
  ENDIF
  s_c = CHR(s_n)

  IF s_c $ chr_shab    && Печатный символ!
    str_new = LEFT(str_flt,flt_pos)+s_c+SUBSTR(str_flt,flt_pos+IIF(ins_flt,1,2))
    str_new = LEFT(str_new, max_len+1)
    pos_new = MIN(flt_pos+1, max_len)
    to_seek = .T.
  ENDIF

  IF s_n = 19          && Стрелка влево
    flt_pos = MAX(flt_pos-1,0)
  ENDIF

  IF s_n = 4           && Стрелка вправо
    flt_pos = MIN(flt_pos+1, LEN(str_flt), max_len)
  ENDIF

  IF s_n = 7           && Del
    str_new = LEFT(str_flt,flt_pos)+SUBSTR(str_flt,flt_pos+2)
    pos_new = flt_pos
    to_seek = .T.
  ENDIF

  IF s_n = 127         && BackSpace
    str_new = LEFT(str_flt,flt_pos-1)+SUBSTR(str_flt,flt_pos+1)
    pos_new = MAX(flt_pos-1,0)
    to_seek = .T.
  ENDIF

  IF s_n = 22          && Ins
    ins_flt = .NOT. ins_flt
    = INSMODE(ins_flt)
  ENDIF

  IF s_n = 1           && Home
    flt_pos = 0
  ENDIF

  IF s_n = 6           && End
    flt_pos = MIN(LEN(str_flt), max_len)
  ENDIF

  IF s_n = 13          && Enter
    EXIT
  ENDIF

  IF INLIST(s_n, 24, 5, 3, 18)
    DO CASE
    CASE s_n = 24  && Стрелка вниз
      i = 1
    CASE s_n = 5   && Стрелка вверх
      i = -1
    CASE s_n = 3   && PgDn
      i = WROWS(win_name+"_E")-1
    OTHERWISE    && PgUp
      i = -WROWS(win_name+"_E")+1
    ENDCASE
    SKIP i
    IF BOF()
      GO TOP
    ENDIF
    IF EOF()
      GO BOTTOM
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH SAME
  ENDIF

  IF to_seek
    rc_sav = RECNO()
    IF SEEK(str_new)
      str_flt = str_new
      flt_pos = pos_new
      SHOW WINDOW (ALIAS()) REFRESH SAME
    ELSE
      IF BETWEEN(rc_sav, 1, RECCOUNT())
        GO rc_sav
      ENDIF
    ENDIF
  ENDIF

ENDDO

RELEASE WINDOW Seek_Hide

@ 1, 8 SAY PADR("Tab", max_len+1)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                                Footer!                                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 28.04.98 17:32:55 ═╝
PROCEDURE Show_F

PRIVATE str_w, w, w_sav, nt

IF .NOT. WEXIST("Seek_Hide")   && Признак перемещения при поиске
  ACTIVATE WINDOW (win_name+"_M")
ENDIF

IF ORD_TMP.OWN
  str_w = "На реализации "+TRANSFORM(STO2748.QNT_REAL,"@Z 999999")+ ;
          "    Бронь "+TRANSFORM(STO2748.QNT_SAVE+STO2748.QNT_REZ,"@Z 999999")+ ;
          "    Упаковка "+TRANSFORM(STO2748.QNT_ROUND,"@Z 999999")
ELSE
  str_w = "На реализации           Бронь     Упаковка       "
ENDIF

IF EMPTY(d1) .OR. EMPTY(d2)
  @ WROWS()-7, 4 SAY NAME COLOR SCHEME 1
  @ WROWS()-5, 4 SAY str_w
ELSE
  @ WROWS()-8, 4 SAY NAME COLOR SCHEME 1
  @ WROWS()-6, 4 SAY str_w
  str_w = "Продано "+TRANSFORM(ORD_TMP.QNT_SALE,"@Z 999999")+ ;
          "  Спрошено "+TRANSFORM(ORD_TMP.QNT_DEM_N,"@Z 999999")+ ;
          " раз "+TRANSFORM(ORD_TMP.QNT_DEM_Q,"@Z 999999")+ ;
          " шт.  Долг "+TRANSFORM(ORD_TMP.QNT_DUE,"@Z 999999")
  @ WROWS()-5, 3 SAY str_w
ENDIF
IF ORD_TMP.OFFER_REC # 0
  GO ORD_TMP.OFFER_REC IN OFFER
  IF OFFER.SUP_QNT = -99999
    w = "Есть  "
  ELSE
    w = TRANSFORM(OFFER.SUP_QNT,"@Z 999999")
  ENDIF
  GO ORD_TMP.OFFER_REC IN OFFER
  str_w = "Наличие "+w+ ;
          "         Цена "+TRANSFORM(OFFER.PRICE_R,r_pic)+ ;
          "р/"+TRANSFORM(OFFER.PRICE_D,"@Z 99999.99")+"$"
  @ WROWS()-3, 9 SAY str_w
  str_w = "Позиция появилась "+TRANSFORM(OFFER.DATE_ON, "@D")+ ;
          "      Данные от "+TRANSFORM(OFFER.DATE_INF, "@D")
  @ WROWS()-2, 5 SAY str_w
ELSE
  @ WROWS()-3, 2 SAY PADC("Нет данных!", WCOLS()-4)
  @ WROWS()-2, 2 SAY SPACE(WCOLS()-4)
ENDIF

IF .NOT. WEXIST("Seek_Hide")   && Признак перемещения при поиске
  ACTIVATE WINDOW (ALIAS())
ENDIF

IF .NOT. EMPTY(STO2748.NOTE)
  m.nt = STO2748.NOTE
  WAIT WINDOW m.nt NOWAIT
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Информация о продажах.                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.04.98 13:59:27 ═╝
PROCEDURE Sale_Inf
PARAMETERS p_code

PRIVATE s_sav, spc_list, dt1, dt2, p_name, s_qnt, s_mon, dn1, dn2
PRIVATE sale_count

sale_count = IIF(EMPTY(old_sale), 2, 1)

s_sav = SELECT()
p_name = ""
IF p_code # 0
  SELECT 0
  USE (base_path+"STOCK") ALIAS ST2948 AGAIN ORDER TAG CODE
  IF SEEK(p_code)
    p_name = ALLTRIM(PREFIX)+" "+ALLTRIM(NAME)+" /"+ALLTRIM(PRODUCER)+"/"
  ENDIF
  USE
  SELECT (s_sav)
ENDIF
IF EMPTY(p_name)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Позиции нет в номенклатуре!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

dt1 = d1
dt2 = d2

IF .NOT. Period(dt1, dt2)
  SELECT (s_sav)
  RETURN
ENDIF

SELECT 0
USE (base_path+"CUSTOMER") ALIAS CUS2948 AGAIN
spc_list = "!"
SCAN FOR .NOT. RYN $ " *"   && Перечень особенных филиалов (Киевская)
  spc_list = spc_list+ALLTRIM(STR(CODE))+"!"
ENDSCAN
USE

DELETE FILE (tmpo_path+"V2948.DBF")
DELETE FILE (tmpo_path+"V2948.CDX")

CREATE DBF (tmpo_path+"V2948") ;
  ( DOC_DATE D,     ;
    FIRM     C( 3), ;
    DOC_NUMV C(11), ;
    DOC_NUM  C(10), ;
    QNT      N( 6), ;
    PRICE    N(8,IIF("." $ r_pic, 2, 0)),;
    CUST_N   C(50)  )
INDEX ON DTOS(DOC_DATE) TAG HRON
SELECT 0
IF File_O(base_path+"ACCOUNT.DBF")
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC2948 AGAIN
ELSE
  USE (base_path+"CUSTOMER") ORDER TAG CODE ALIAS CUS2948 AGAIN
ENDIF

SELECT 0
s_qnt = 0
s_mon = 0
DO WHILE sale_count <= 2
  IF sale_count = 2
    USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS ST2948 AGAIN
    IF USED("ACC2948")
      SET RELATION TO CUS_CODE INTO ACC2948
    ELSE
      SET RELATION TO CUS_CODE INTO CUS2948
    ENDIF
    SELECT 0
    USE (base_path+"SALE") ORDER TAG CODE DESCENDING ALIAS SD2948 AGAIN
    IF TYPE("DOC_NUM") = "C"
      SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO ST2948
    ELSE
      SET RELATION TO LEFT(DTOS(DOC_DATE),4)+WHERE+STR(DOC_NUM,6) INTO ST2948
    ENDIF
  ELSE
    USE (old_sale+"SALE_TIT") ORDER TAG DOC_NUM ALIAS ST2948 AGAIN
    IF USED("ACC2948")
      SET RELATION TO CUS_CODE INTO ACC2948
    ELSE
      SET RELATION TO CUS_CODE INTO CUS2948
    ENDIF
    SELECT 0
    USE (old_sale+"SALE") ORDER TAG CODE DESCENDING ALIAS SD2948 AGAIN
    IF TYPE("DOC_NUM") = "C"
      SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO ST2948
    ELSE
      SET RELATION TO LEFT(DTOS(DOC_DATE),4)+WHERE+STR(DOC_NUM,6) INTO ST2948
    ENDIF

  ENDIF
  SEEK p_code
  SCAN REST WHILE p_code = CODE FOR DOC_DATE > dt1
    IF ST2948.CUS_CODE < 0 .OR. "!"+ALLTRIM(STR(ST2948.CUS_CODE))+"!" $ spc_list
      LOOP
    ENDIF
    IF TYPE("DOC_NUM") = "C"
      dn1 = LEFT(SD2948.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(SD2948.DOC_NUM,5))
      dn2 = DOC_NUM
    ELSE
      dn1 = CHRTRAN(WHERE," ","0")+"-"+ALLTRIM(STR(DOC_NUM))
      dn2 = "   "+WHERE+STR(DOC_NUM,6)
    ENDIF
    SELECT V2948
    APPEND BLANK
    REPLACE DOC_DATE WITH SD2948.DOC_DATE,  ;
            FIRM     WITH SD2948.FIRM,      ;
            DOC_NUMV WITH dn1, ;
            DOC_NUM  WITH dn2, ;
            QNT      WITH SD2948.QNT,       ;
            PRICE    WITH SD2948.PRICE_R,   ;
            CUST_N   WITH IIF(ST2948.CUS_CODE # 0,   ;
      IIF(USED("ACC2948"), ACC2948.CUS_NAME, CUS2948.L_NAME),ST2948.CUS_NAME)
    s_qnt = s_qnt+SD2948.QNT
    s_mon = s_mon+SD2948.QNT*SD2948.PRICE_R
    SELECT SD2948
  ENDSCAN

  USE
  SELECT ST2948
  USE
  sale_count = sale_count+1
ENDDO
IF USED("ACC2948")
  SELECT ACC2948
ELSE
  SELECT CUS2948
ENDIF
USE
SELECT V2948
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Продаж за указанный период не было!!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"V2948.DBF")
  DELETE FILE (tmpo_path+"V2948.CDX")
  SELECT (s_sav)
  RETURN
ENDIF

DO V_Sal WITH dt1, dt2, p_name, s_qnt, s_mon

SELECT V2948
USE
DELETE FILE (tmpo_path+"V2948.DBF")
DELETE FILE (tmpo_path+"V2948.CDX")
SELECT (s_sav)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                        Установка интервала дат.                        ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.04.98 15:18:52 ═╝
PROCEDURE Period
PARAMETERS d001, d002
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

*┌──────────────────────────────────┐
*│                                  │
*│       Количество дней 9999       │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, n_days
ex = 1
IF EMPTY(d001)
  n_days = MIN(__srok, DATE()-d_s+1)
ELSE
  n_days = d002-d001+1
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, "Установите срок"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 9 SAY "Количество дней" GET n_days PICTURE "@Z 9999"
@ 5, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Date()
IF ex = 1
  d001 = DATE()-n_days+1
  d002 = DATE()
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Проверка корректности дат.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.04.98 15:42:22 ═╝
PROCEDURE Tst_Date
PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF n_days < 1
  DIMENSION mss(3)  mss(1) = ""
  mss(2) = CHR(0)+"Сколько? Сколько?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF DATE()-n_days+1 < d_s
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Да у нас данные только за "+ALLTRIM(STR(DATE()-d_s+1))+" дней!"
  mss(3) = CHR(0)+"  Работаем? [ Да ] [ Нет ]"
  mss(4) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Просмотр продаж.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.04.98 16:09:18 ═╝
PROCEDURE V_Sal
PARAMETERS f_date, l_date, position, s_qnt, s_mon
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()
*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
DO Use_Dummy
SELECT V2948
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Документ" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(15, RECCOUNT())   && Количество видимых строк BROWSE
wd = 54

DO D_Wins WITH ln, wd, position, 1, 2
@ 1, 0 SAY PADC("Продажи c "+DTOC(f_date)+" по "+DTOC(l_date),WCOLS())
@ 2, 2 SAY ;
"Фрм────Дата────Документ────Кол-во────Цена──────Сумма"
@ WROWS()-3, 12 SAY "Итого    "+STR(s_qnt,10)+STR(s_mon,21,IIF("." $ r_pic, 2, 0))
what_do = "List"

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
    BROWSE FIELDS FIRM:3:H="",      ;
                  DOC_DATE:10:H="", ;
                  DOC_NUMV:11:H="", ;
                  QNT:6:H="",       ;
                  PRICE:8:H="",     ;
                  F001 = QNT*PRICE:11:H="",   ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Foot1() ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && Печать накладной

    DO Prn_Inv WITH FIRM, DOC_NUM, DOC_DATE
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                                Footer!                                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.04.98 17:50:46 ═╝
PROCEDURE Foot1

ACTIVATE WINDOW (win_name+"_M")
@ WROWS()-2, 2 SAY CUST_N COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS())

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Информация о спросе.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.04.98 13:59:27 ═╝
PROCEDURE Dem_Inf
PARAMETERS p_code

PRIVATE s_sav, dt1, dt2, p_name, s_qnt

s_sav = SELECT()
p_name = ""
IF p_code # 0
  SELECT 0
  USE (base_path+"STOCK") ALIAS ST2948 AGAIN ORDER TAG CODE
  IF SEEK(p_code)
    p_name = ALLTRIM(PREFIX)+" "+ALLTRIM(NAME)+" /"+ALLTRIM(PRODUCER)+"/"
  ENDIF
  USE
  SELECT (s_sav)
ENDIF
IF EMPTY(p_name)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Позиции нет в номенклатуре!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

dt1 = d1
dt2 = d2

IF .NOT. Period(dt1, dt2)
  SELECT (s_sav)
  RETURN
ENDIF

DELETE FILE (tmpo_path+"V2948.DBF")
DELETE FILE (tmpo_path+"V2948.CDX")

CREATE DBF (tmpo_path+"V2948") ;
  ( DEM_DATE D,     ;
    QNT      N( 6), ;
    CUST_N   C(50)  )
INDEX ON DTOS(DEM_DATE) TAG HRON
SELECT 0
IF File_O(base_path+"ACCOUNT.DBF")
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC2948 AGAIN
ELSE
  USE (base_path+"CUSTOMER") ORDER TAG CODE ALIAS CUS2948 AGAIN
ENDIF
SELECT 0
USE (base_path+"DEMAND") ORDER TAG STO_CODE ALIAS DM2948 AGAIN
IF USED("ACC2948")
  SET RELATION TO CLI_CODE INTO ACC2948
ELSE
  SET RELATION TO CLI_CODE INTO CUS2948
ENDIF
SEEK p_code
s_qnt = 0
SCAN REST WHILE p_code = STO_CODE FOR DATE > dt1
  SELECT V2948
  APPEND BLANK
  REPLACE DEM_DATE WITH DM2948.DATE, ;
          QNT      WITH DM2948.QTY,  ;
          CUST_N   WITH IIF(DM2948.CLI_CODE # 0,   ;
  IIF(USED("ACC2948"), ACC2948.CUS_NAME, CUS2948.L_NAME), DM2948.CLIENT)
  s_qnt = s_qnt+DM2948.QTY
  SELECT DM2948
ENDSCAN

USE
IF USED("ACC2948")
  SELECT ACC2948
ELSE
  SELECT CUS2948
ENDIF
USE
SELECT V2948
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Никто ее и не спрашивал..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"V2948.DBF")
  DELETE FILE (tmpo_path+"V2948.CDX")
  SELECT (s_sav)
  RETURN
ENDIF

DO V_Dem WITH dt1, dt2, p_name, s_qnt

SELECT V2948
USE
DELETE FILE (tmpo_path+"V2948.DBF")
DELETE FILE (tmpo_path+"V2948.CDX")
SELECT (s_sav)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Просмотр спроса.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.04.98 16:09:18 ═╝
PROCEDURE V_Dem
PARAMETERS f_date, l_date, position, s_qnt
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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()
*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
DO Use_Dummy
SELECT V2948
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(15, RECCOUNT())   && Количество видимых строк BROWSE
wd = 68

DO D_Wins WITH ln, wd, position, 1, 1
@ 1, 0 SAY PADC("Спрос c "+DTOC(f_date)+" по "+DTOC(l_date),WCOLS())
@ 2, 2 SAY ;
"───Дата────Кол-во────Кто спросил"
@ WROWS()-2, 5 SAY "Итого "+STR(s_qnt,8)
what_do = "List"

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
    BROWSE FIELDS DEM_DATE:10:H="", ;
                  QNT:6:H="",       ;
                  CUST_N:H="",      ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Информация о предложении.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 29.04.98 13:59:27 ═╝
PROCEDURE Off_Inf
PARAMETERS p_code, p_name

PRIVATE s_sav, dt1, dt2, s_qnt, ord_sav
s_sav = SELECT()

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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()
*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
DO Use_Dummy
SELECT 0
IF File_O(base_path+"ACCOUNT.DBF")
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC5598 AGAIN
ELSE
  USE (base_path+"CUSTOMER") ORDER TAG CODE ALIAS CUS5598 AGAIN
ENDIF
SELECT OFFER
IF USED("ACC5598")
  SET RELATION TO SUP_CODE INTO ACC5598
ELSE
  SET RELATION TO SUP_CODE INTO CUS5598
ENDIF
ord_sav = TAG()
IF p_code = 0
  SET ORDER TO TAG NAME
  SEEK p_name
ELSE
  SET ORDER TO TAG CODE
  SEEK p_code
ENDIF
IF .NOT. FOUND()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Для позиции нет данных о предложении!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SET ORDER TO TAG (ord_sav)
  SET RELATION TO
  IF USED("ACC5598")
    SELECT ACC5598
  ELSE
    SELECT CUS5598
  ENDIF
  USE
  SELECT (s_sav)
  RETURN
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = 15   && Количество видимых строк BROWSE
wd = 68

DO D_Wins WITH ln, wd, "Предложение", 0, 0
@ 1, 2 SAY ;
"───Поставщик───────────────────Кол-во───Цена USD─Цена руб─Данные от"
what_do = "List"

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
    IF p_code # 0
      BROWSE KEY p_code FIELDS  ;
                    F000 = LEFT(IIF(USED("ACC5598"),ACC5598.CUS_NAME,CUS5598.L_NAME),30):H="", ;
                    F001 = IIF(SUP_QNT=-99999, "  Есть", TRANSFORM(SUP_QNT,"@Z 999999")):6:H="",  ;
                    PRICE_D:H="":P="@Z 99999.9999",  ;
                    PRICE_R:H="":P=r_pic,  ;
                    DATE_INF:10:H="",  ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE KEY p_name FIELDS  ;
                    F000 = LEFT(IIF(USED("ACC5598"),ACC5598.CUS_NAME,CUS5598.L_NAME),30):H="", ;
                    F001 = IIF(SUP_QNT=-99999, "  Есть", TRANSFORM(SUP_QNT,"@Z 999999")):6:H="",  ;
                    PRICE_D:H="":P="@Z 99999.9999",  ;
                    PRICE_R:H="":P=r_pic,  ;
                    DATE_INF:10:H="",  ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

IF USED("ACC5598")
  SELECT ACC5598
ELSE
  SELECT CUS5598
ENDIF
USE
SELECT OFFER
SET RELATION TO
SET ORDER TO TAG (ord_sav)
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Round_U      Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Округление до нормы упаковки.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.02.2000 ──┘
PROCEDURE Round_U
PRIVATE s_sav, q, r_sav, rel_sav, let_prc

r_sav = RECNO()
s_sav = SELECT()

let_prc = Let_Prc()
IF let_prc < 0
  RETURN
ENDIF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST0204 AGAIN

SELECT ORD_TMP
rel_sav = SET("RELATION")
SET RELATION TO

SET RELATION TO CODE INTO ST0204
SCAN FOR ST0204.QNT_ROUND > 1
  q = ROUND(QNT/ST0204.QNT_ROUND,0)*ST0204.QNT_ROUND
  IF let_prc > 0
    IF ABS(q-QNT)/QNT < let_prc
      REPLACE QNT WITH q
    ENDIF
  ENDIF
ENDSCAN  

SELECT ST0204
USE
SELECT (s_sav)
IF ALIAS() = "TMP_ORD"
  SET RELATION TO &rel_sav
ENDIF
SET RELATION TO &rel_sav
IF BETWEEN(r_sav,1,RECCOUNT())
  GO r_sav
ENDIF
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Расчет параметров.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.05.98 10:47:51 ═╝
PROCEDURE Clc_Prms
PRIVATE s_sav, spc_list, cnt, qd, nd, qdue, qs, rc_sav, o_sav, ls
PRIVATE flg_s, k_spl, n_spl, qn, m_qnt, qs_s

s_sav = SELECT()

flg_s = .F.
k_spl = 0
n_spl = 0

IF .NOT. Period(d1, d2)
  SELECT (s_sav)
  RETURN
ENDIF
__srok = d2-d1+1

DO Splash WITH k_spl, n_spl
IF k_spl = 0
  flg_s = .F.
ELSE
  flg_s = .T.
ENDIF

SELECT 0
USE (base_path+"CUSTOMER") ALIAS CUS2948 AGAIN
spc_list = "!"
SCAN FOR .NOT. RYN $ " *"   && Перечень особенных филиалов (Киевская)
  spc_list = spc_list+ALLTRIM(STR(CODE))+"!"
ENDSCAN

USE (base_path+"DEMAND") ORDER TAG STO_CODE DESCENDING ALIAS DMC6598 AGAIN
SELECT 0
USE (base_path+"DEMAND") ORDER TAG NAME DESCENDING ALIAS DMN6598 AGAIN
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS ST6598 AGAIN
SELECT 0
USE (base_path+"SALE") ORDER TAG CODE DESCENDING ALIAS SD6598 AGAIN
IF TYPE("DOC_NUM") = "C"
  SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO ST6598
ELSE
  SET RELATION TO LEFT(DTOS(DOC_DATE),4)+WHERE+STR(DOC_NUM,6) INTO ST6598
ENDIF

IF .NOT. EMPTY(old_sale)
  SELECT 0
  USE (old_sale+"SALE_TIT") ORDER TAG DOC_NUM ALIAS ST_6598 AGAIN
  SELECT 0
  USE (old_sale+"SALE") ORDER TAG CODE DESCENDING ALIAS SD_6598 AGAIN
  IF TYPE("DOC_NUM") = "C"
    SET RELATION TO LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO ST_6598
  ELSE
    SET RELATION TO LEFT(DTOS(DOC_DATE),4)+WHERE+STR(DOC_NUM,6) INTO ST_6598
  ENDIF
ENDIF

IF File_O(base_path+"LIST_DET.DBF")
  SELECT 0
  USE (base_path+"LIST_DET") ORDER TAG DUE_CODE ALIAS DC6598 AGAIN
  SELECT 0
  USE (base_path+"LIST_DET") ORDER TAG DUE_NAM ALIAS DN6598 AGAIN
ENDIF
SELECT ORD_TMP
STORE 0 TO cnt
rc_sav = RECNO()
o_sav = TAG()
SET ORDER TO
DO Wt_Mess WITH "Расчет..."
SCAN
  cnt = cnt+1
  IF cnt % 100 = 0
    @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)),WCOLS())
  ENDIF
  STORE 0 TO qd, nd, qdue, qs, qn, m_qnt, qs_s
  *
  *   Продажи...
  *
  SELECT SD6598
  = SEEK(ORD_TMP.CODE)
  ls = IIF(FOUND() , DOC_DATE, {})
  SCAN REST WHILE ORD_TMP.CODE = CODE .AND. DOC_DATE > d1
    IF ST6598.CUS_CODE < 0     && Спец. движение
      LOOP
    ENDIF 
    IF "!"+ALLTRIM(STR(ST6598.CUS_CODE))+"!" $ spc_list && Собств. филиал (Киевская)
      LOOP
    ENDIF
    qs = qs+QNT
    qn = qn + 1
  ENDSCAN 
  IF .NOT. EMPTY(old_sale)
    SELECT SD_6598
    SEEK ORD_TMP.CODE
    ls = IIF(FOUND() , DOC_DATE, {})
    SCAN REST WHILE ORD_TMP.CODE = CODE .AND. DOC_DATE > d1
      IF ST_6598.CUS_CODE < 0     && Спец. движение
        LOOP
      ENDIF 
      IF "!"+ALLTRIM(STR(ST_6598.CUS_CODE))+"!" $ spc_list && Собств. филиал (Киевская)
        LOOP
      ENDIF
      qs = qs+QNT
      qn = qn + 1
    ENDSCAN
  ENDIF

  *
  *   Всплеск продаж...
  *
  IF flg_s
    m_qnt = CEILING(qs/qn*(1+k_spl/100))
    SELECT SD6598
    SEEK ORD_TMP.CODE
    SCAN REST WHILE ORD_TMP.CODE = CODE .AND. DOC_DATE > d1
      IF ST6598.CUS_CODE < 0     && Спец. движение
        LOOP
      ENDIF 
      IF "!"+ALLTRIM(STR(ST6598.CUS_CODE))+"!" $ spc_list && Собств. филиал (Киевская)
        LOOP
      ENDIF
      IF QNT < m_qnt .AND. qn >= n_spl
        qs_s = qs_s + QNT
      ENDIF
    ENDSCAN
    IF .NOT. EMPTY(old_sale)
      SELECT SD_6598
      SEEK ORD_TMP.CODE
      ls = IIF(FOUND() , DOC_DATE, {})
      SCAN REST WHILE ORD_TMP.CODE = CODE .AND. DOC_DATE > d1
        IF ST_6598.CUS_CODE < 0     && Спец. движение
          LOOP
        ENDIF 
        IF "!"+ALLTRIM(STR(ST_6598.CUS_CODE))+"!" $ spc_list && Собств. филиал (Киевская)
          LOOP
        ENDIF
        IF QNT < m_qnt .AND. qn >= n_spl
          qs_s = qs_s + QNT
        ENDIF    
      ENDSCAN
    ENDIF
  ENDIF
  IF flg_s
    qs = qs_s
  ENDIF
  *
  *   Спрос...
  *
  IF ORD_TMP.OWN
    SELECT DMC6598
    SEEK ORD_TMP.CODE
    SCAN REST WHILE ORD_TMP.CODE = STO_CODE .AND. DATE > d1
      qd = qd+MAX(QTY,1)
      nd = nd+1
    ENDSCAN
  ELSE
    SELECT DMN6598
    SEEK ORD_TMP.NAME+ORD_TMP.PREFIX+ORD_TMP.PRODUCER
    SCAN REST WHILE ORD_TMP.NAME = NAME .AND. ;
                    ORD_TMP.PREFIX = PREFIX .AND. ;
                    ORD_TMP.PRODUCER = PRODUCER .AND. ;
                    DATE > d1
      qd = qd+MAX(QTY,1)
      nd = nd+1
    ENDSCAN
  ENDIF
  *
  *   Долг...
  *
  IF USED("DC6598")
    IF ORD_TMP.OWN
      SELECT DC6598
      SEEK ORD_TMP.CODE
      SCAN REST WHILE ORD_TMP.CODE = CODE
        qdue = qdue+MAX(QNT_DUE,1)
      ENDSCAN
    ELSE
      SELECT DN6598
      SEEK ORD_TMP.NAME+ORD_TMP.PREFIX+ORD_TMP.PRODUCER
      SCAN REST WHILE ORD_TMP.NAME = NAME .AND. ;
                      ORD_TMP.PREFIX = PREFIX .AND. ;
                      ORD_TMP.PRODUCER = PRODUCER
        qdue = qdue+MAX(QNT_DUE,1)
      ENDSCAN
    ENDIF
  ENDIF
  SELECT ORD_TMP
  REPLACE QNT_DUE   WITH qdue, ;
          QNT_SALE  WITH qs,   ;
          QNT_DEM_N WITH nd,   ;
          LAST_SALE WITH ls,   ;
          QNT_DEM_Q WITH qd
ENDSCAN

DO Wt_Mess
SET ORDER TO TAG (o_sav)
IF BETWEEN(rc_sav, 1, RECCOUNT())
  GO rc_sav
ENDIF

SELECT DMC6598
USE
SELECT DMN6598
USE
SELECT ST6598
USE
SELECT SD6598
USE
IF USED("DC6598")
  SELECT DC6598
  USE
  SELECT DN6598
  USE
ENDIF
SELECT (s_sav)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Изготовление заявки.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 18.05.98 16:28:16 ═╝
PROCEDURE Mk_Ord

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

*┌──────────────────────────────────────┐
*│               КОМПЭЛ                 │
*│ ┌─ Примечание ─────────────────────┐ │
*│ │ ....:....!....:....!....:....!.. │ │
*│ └──────────────────────────────────┘ │
*│ [ ] Печать документа [ ] Файл заказа │
*│                                      │
*│           Сторонний поставщик        │
*│ ┌─ Примечание ─────────────────────┐ │
*│ │ ....:....!....:....!....:....!.. │ │
*│ └──────────────────────────────────┘ │
*│ [ ] Печать документа [ ] Файл заказа │
*│                                      │
*│   < OK Ctrl-W > < Отказаться Esc >   │
*└──────────────────────────────────────┘

PRIVATE ex, nt, nt1, p_doc, s_file, p_doc1, s_file1, s_sav, frm000
ex      = 1
nt      = PADR(ORD_T_L.NOTE,32)
nt1     = PADR(ORD_T_L.NOTE,32)
p_doc   = .F.
p_doc1  = .F.
s_file  = .F.
s_file1 = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 16, 42, "Формирование заказа"
DO Sun_Bord WITH 3, 3,  5, 38, " Примечание "
DO Sun_Bord WITH 9, 3, 11, 38, " Примечание "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2, 18 SAY "КОМПЭЛ"
@  4,  5 GET nt1
@  6,  3 GET p_doc1 PICTURE "@*C Печать документа"
@  6, 24 GET s_file1 PICTURE "@*C Файл заказа"
@  8, 11 SAY "Сторонний поставщик"
@ 10,  5 GET nt
@ 12,  3 GET p_doc PICTURE "@*C Печать документа"
@ 12, 24 GET s_file PICTURE "@*C Файл заказа"
@ 14,  5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  @ 2, 2 CLEAR TO WROWS()-2, WCOLS()-3
  @ 4, 2 SAY PADC("Подготовка", WCOLS()-4)
  @ 5, 2 SAY PADC("заказа...", WCOLS()-4)
  s_sav = SELECT()
  frm000 = ORD_T_L.FIRM
  SELECT 0
  USE (tmp_path+"ORD"+ALLTRIM(STR(RECNO("ORD_T_L"))) ) ORDER TAG NAME1 ALIAS ORD_TMP
  GO TOP
  IF BOF() .OR. EOF()
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Заготовка пуста..."
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    n_ord = ""
    n_ord1 = ""
    SELECT 0
    USE (base_path+"ORDER")
    SELECT ORD_TMP
    SCAN
      IF OWN_STO = "P" .AND. sale_mode = "Mitracon"
        IF EMPTY(n_ord)
          n_ord = Doc_Num("ORDER", def_firm, arm, DATE())
          n_ord = STR(n_ord,10)
        ENDIF
        SELECT ORDER
        APPEND BLANK
        REPLACE FIRM     WITH ORD_T_L.FIRM, ;
                ORD_NUM  WITH n_ord,  ;
                ORD_DATE WITH DATE(), ;
                CODE     WITH ORD_TMP.CODE, ;
                PREFIX   WITH ORD_TMP.PREFIX, ;
                NAME     WITH ORD_TMP.NAME, ;
                PRODUCER WITH ORD_TMP.PRODUCER, ;
                QNT      WITH ORD_TMP.QNT
        SELECT ORD_TMP
      ELSE
        IF EMPTY(n_ord1)
          n_ord1 = Doc_Num("ORDER", def_firm, arm, DATE())
          n_ord1 = STR(n_ord1,10)
        ENDIF
        SELECT ORDER
        APPEND BLANK
        REPLACE FIRM     WITH ORD_T_L.FIRM, ;
                ORD_NUM  WITH n_ord1, ;
                ORD_DATE WITH DATE(), ;
                CODE     WITH ORD_TMP.CODE, ;
                PREFIX   WITH ORD_TMP.PREFIX, ;
                NAME     WITH ORD_TMP.NAME, ;
                PRODUCER WITH ORD_TMP.PRODUCER, ;
                QNT      WITH ORD_TMP.QNT
        SELECT ORD_TMP
      ENDIF
    ENDSCAN
    SELECT ORDER
    USE (base_path+"ORD_TIT")
    IF .NOT.EMPTY(n_ord)
      APPEND BLANK
      REPLACE FIRM     WITH ORD_T_L.FIRM, ;
              ORD_NUM  WITH n_ord,  ;
              ORD_DATE WITH DATE(), ;
              CUS_CODE WITH sup_own,;
              SUP_CODE WITH ORD_T_L.SUP_CODE, ;
              NOTE     WITH nt,     ;
              WHO_ORD  WITH user,   ;
              WHERE_ORD WITH arm
    ENDIF              
    IF .NOT.EMPTY(n_ord1)
      APPEND BLANK
      REPLACE FIRM     WITH ORD_T_L.FIRM, ;
              ORD_NUM  WITH n_ord1, ;
              ORD_DATE WITH DATE(), ;
              CUS_CODE WITH sup_own,;
              SUP_CODE WITH ORD_T_L.SUP_CODE, ;
              NOTE     WITH nt1,    ;
              WHO_ORD  WITH user,   ;
              WHERE_ORD WITH arm
    ENDIF              
    USE
    SELECT ORD_TMP
    USE
    SELECT ORD_T_L
    DELETE FILE (tmp_path+"ORD"+ALLTRIM(STR(RECNO("ORD_T_L")))+".DBF")
    DELETE FILE (tmp_path+"ORD"+ALLTRIM(STR(RECNO("ORD_T_L")))+".CDX")
    REPLACE SUP_CODE WITH 0
    @ 2, 2 CLEAR TO WROWS()-2, WCOLS()-3
    IF .NOT. EMPTY(n_ord1)
      @ 4, 2 SAY PADC("Заказ КОМПЭЛу N "+ALLTRIM(n_ord1)+".", WCOLS()-5)
    ENDIF
    IF .NOT. EMPTY(n_ord)
      @ 7, 2 SAY PADC("Заказ СТОРОННЕМУ ПОСТАВЩИКУ N "+ALLTRIM(n_ord)+".", WCOLS()-5)
    ENDIF
    @ WROWS()-2, 2 SAY PADC("Нажмите любую клавишу...", WCOLS()-5)
    = INKEY(0,"MH")
    IF p_doc1
      DO P_Ord WITH frm000, n_ord1, DATE()
    ENDIF
    IF s_file1
      DO S_Ord WITH frm000, n_ord1, DATE(), "КОМПЭЛу"
    ENDIF
    IF p_doc
      DO P_Ord WITH frm000, n_ord, DATE()
    ENDIF
    IF s_file
      DO S_Ord WITH frm000, n_ord, DATE(), "СТОРОННЕМУ ПОСТАВЩИКУ"
    ENDIF
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                             Сумма по заявке.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 24.04.98 17:58:27 ═╝
PROCEDURE Ord_Sum
PRIVATE rc_sav, s_od, s_sd, s_or, s_sr, l0, c0, ln, wd

rc_sav = RECNO()

SET ORDER TO TAG NAME1
s_or = 0
s_sr = 0
s_od = 0
s_sd = 0

DO Wt_Mess WITH "Подсчет..."
SCAN
  s_or = s_or+QNT*STO2748.PRICE_R
  s_od = s_od+QNT*STO2748.PRICE
  IF ORD_TMP.OFFER_REC # 0
    GO ORD_TMP.OFFER_REC IN OFFER
    s_sr = s_sr+QNT*OFFER.PRICE_R
    s_sd = s_sd+QNT*OFFER.PRICE_D
  ENDIF
ENDSCAN
DO Wt_Mess

IF is_all
  SET ORDER TO TAG NAME
ENDIF

IF BETWEEN(rc_sav, 1, RECCOUNT())
  GO rc_sav
ENDIF
*┌────────────────────────────────────────────┐
*│                                            │
*│ Цены продажи     99999999 руб   99999999 $ │
*│                                            │
*│ Цены поставщика  99999999 руб   99999999 $ │
*│                                            │
*│                                            │
*│            Нажмите что-нибудь...           │
*│                                            │
*└────────────────────────────────────────────┘
ln = 10
wd = 48
l0 = FLOOR((WROWS("")-ln)/2)
c0 = FLOOR((WCOLS("")-wd)/2)

DEFINE WINDOW Tmp_Sum FROM l0, c0 TO l0+ln-1, c0+wd-1 NONE SHADOW COLOR SCHEME 13
ACTIVATE WINDOW Tmp_Sum
DO Sun_Bord WITH 0,1,WROWS()-1, WCOLS()-2
@ 2, 3 SAY "Цены продажи     "+TRANSFORM(s_or, r_pic)+" руб   "+ ;
                               TRANSFORM(s_od, "@Z 99999.99")+" $"
@ 4, 3 SAY "Цены поставщика  "+TRANSFORM(s_sr, r_pic)+" руб   "+ ;
                               TRANSFORM(s_sd, "@Z 99999.99")+" $"

@ 6, 2 SAY PADC("Нажмите что-нибудь...",WCOLS()-4)

= INKEY(0, "MH")

RELEASE WINDOW Tmp_Sum

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура P_File       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Параметры тестового файла.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10.03.2000 ──┘
PROCEDURE P_File

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

*┌───────────────────────────────────────────┐
*│   ( ) Все позиции   ( ) Только непустые   │
*│                                           │
*│     < OK Ctrl-W > < Отказаться Esc >      │
*└───────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 47, "Состав файла"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 5 GET sw_all PICTURE "@*RH Все позиции ;Только непустые"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура P_File1      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Формирование тестового файла.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10.03.2000 ──┘
PROCEDURE P_File1

PRIVATE s_sav, own_v, p_own, r_sav, q_n, q_p, tmpQnt
s_sav = SELECT()

SELECT 0
USE (base_path+"PARMS") ALIAS P_0310 AGAIN
own_v = VALUTA
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_0310 AGAIN
SELECT 0
DELETE FILE(tmpo_path+"ORD_DEB.DBF")
DELETE FILE(tmpo_path+"ORD_DEB.CDX")
CREATE DBF (tmpo_path+"ORD_DEB.DBF") ;
  ( CODE      N( 7),  ;
    PREFIX    C( 4),  ;
    NAME      C(35),  ;
    PRODUCER  C( 8),  ;
    REST      N( 6),  ;
    QNT_DUE   N( 6),  ;
    PRICE     N(10,4),;
    L_BOUND   N( 3),  ;
    R_BOUND   N( 3),  ;
    QNT_SALE  N( 6),  ;
    QNT_DEM_N N( 6),  ;
    QNT_DEM_Q N( 6),  ;
    LAST_SALE D,      ;
    ORDER     N( 6) )

SELECT ORD_TMP
r_sav = RECNO()
SCAN
  IF .NOT. sw_all .AND. QNT = 0
    LOOP
  ENDIF

  p_own = 0
  IF ORD_TMP.OFFER_REC # 0
    GO ORD_TMP.OFFER_REC IN OFFER
    p_own = IIF(own_v = "$", OFFER.PRICE_D, OFFER.PRICE_R)
  ENDIF
  tmpQnt = 0
  IF CODE # 0
    IF SEEK(CODE, "ST_0310")
      tmpQnt = ST_0310.QNT
*      p_own = IIF(own_v = "$", ST_0310.PRICE, ST_0310.PRICE_R)
    ENDIF
  ENDIF
  q_n = __c_scale(1,2)
  q_p = __c_scale(1,3)
  FOR i = 2 TO 10
    IF __c_scale(i,1) = 0 .OR. p_own <= __c_scale(i,1)
      EXIT
    ENDIF
    q_n = __c_scale(i,2)
    q_p = __c_scale(i,3)
  ENDFOR
  SELECT ORD_DEB
  APPEND BLANK
  REPLACE CODE      WITH ORD_TMP.CODE,     ;
          PREFIX    WITH ORD_TMP.PREFIX,   ;
          NAME      WITH ORD_TMP.NAME,     ;
          PRODUCER  WITH ORD_TMP.PRODUCER, ;
          REST      WITH tmpQnt,           ;
          QNT_DUE   WITH ORD_TMP.QNT_DUE,  ;
          PRICE     WITH p_own,            ;
          L_BOUND   WITH q_n,              ;
          R_BOUND   WITH q_p,              ;
          QNT_SALE  WITH ORD_TMP.QNT_SALE, ;
          QNT_DEM_N WITH ORD_TMP.QNT_DEM_N,;
          QNT_DEM_Q WITH ORD_TMP.QNT_DEM_Q,;
          LAST_SALE WITH ORD_TMP.LAST_SALE,;
          ORDER     WITH ORD_TMP.QNT
  SELECT ORD_TMP
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

SELECT ST_0310
USE

SELECT ORD_DEB
PRIVATE tmpName
tmpName = ""
tmpName = New_File(tmpName, "DBF", "Где сохранить результат?")
IF .NOT. EMPTY(tmpName)
  COPY TO (tmpName)
ENDIF
USE

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Let_Prc      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│               Допустимый процент округления до упаковки.               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.03.2000 ──┘
PROCEDURE Let_Prc

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
* Округление до упаковки
*┌──────────────────────────────────┐
*│      Процент округления 999      │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, tmpVal
tmpVal = Get_Sys("ORD_ROUND")
tmpVal = VAL(ALLTRIM(tmpVal))
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 38, "Округление до упаковки"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 8 SAY "Процент округления" GET tmpVal PICTURE "999"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  DO Put_Sys WITH "ORD_ROUND", STR(MAX(0, tmpVal))
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN IIF(ex = 1, tmpVal/100, -1)

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                Установка коэффициента всплеска продаж.                 ║
*║                                                                        ║
*╚═══════════════════════════════════════════════════════════ 02.08.2002 ═╝
PROCEDURE Splash
PARAMETERS spl_k, spl_s
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
*┌─────────────────────────────────┐
*│                                 │
*│ Порог по количеству продаж 9999 │
*│                                 │
*│        Всплеск  9999.99 %       │
*│                                 │
*│ < OK Ctrl-W > < Отказаться Esc >│
*└─────────────────────────────────┘

PRIVATE ex, sale_n, sale_k
ex = 1

IF File_O(base_path+"SPLASH.MEM")
  RESTORE FROM(base_path+"SPLASH.MEM") ADDITIVE
  sale_n = n_001
  sale_k = k_001
ELSE
  sale_n = 0
  sale_k = 0
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 37, "Всплеск продаж"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 3 SAY "Порог по количеству продаж" GET sale_n PICTURE "@Z 9999"  VALID Tst_Spl()
@ 5,10 SAY "Всплеск" GET sale_k PICTURE "@Z 99999"
@ 5,24 SAY "%"
@ 7, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE
IF ex = 1
  spl_k = sale_k
  spl_s = sale_n
  n_001 = spl_s
  k_001 = spl_k
  SAVE ALL LIKE ?_001 TO (base_path+"SPLASH.MEM")
ELSE
  spl_k = 0
  spl_s = 0
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                   Проверка корректности порога продаж.                 ║
*║                                                                        ║
*╚═══════════════════════════════════════════════════════════ 02.08.2002 ═╝
PROCEDURE Tst_Spl

IF sale_n < 1
  PRIVATE mss
  DIMENSION mss(3)  
  mss(1) = ""
  mss(2) = CHR(0)+"Сколько? Сколько?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
