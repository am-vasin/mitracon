*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Inc_List     Разработчик Андрей Васин           06.05.97 19:25:14 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║         Программа формирования списка позиций приемного акта.          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Inc_List
PARAMETER stop_new

*
*  Переменные состояния для навигации
*
PRIVATE sum_line, sum_doc, tmpArr
IF .NOT. Get_L()  && Захватить список!
  RETURN
ENDIF

m.sum_line = 0
m.sum_doc  = 0
IF .NOT. EMPTY(INC_TIT.IS_PRICE)
  SELECT INC_LIST
  SEEK STR(RECNO("INC_TIT"), 5)
  SCAN REST WHILE LIST_NO = STR(RECNO("INC_TIT"), 5)
    IF INC_TIT.IS_PRICE = "$"
      m.sum_doc = m.sum_doc+QNT*PRICE
    ELSE
      m.sum_doc = m.sum_doc+QNT*PRICE_0+ROUND(QNT*PRICE_0*m.nds_flt/100,2)
    ENDIF
  ENDSCAN
ENDIF

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
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Sw_Mode OF (menu_name) PROMPT "Сп/Ном." KEY Ctrl-A, "F2"
ON SELECTION PAD Sw_Mode OF (menu_name) Do Sw_Mode WITH "Sw_Mode"

DEFINE PAD Pos OF (menu_name) PROMPT "Позиция"

DEFINE PAD Gtd OF (menu_name) PROMPT "ГТД" KEY Ctrl-A, "Ctrl-Tab"
ON SELECTION PAD Gtd OF (menu_name) Do Sw_Mode WITH "Gtd"

DEFINE PAD Calc OF (menu_name) PROMPT "Расчет"

DEFINE PAD Due OF (menu_name) PROMPT "Долг" KEY Ctrl-A, "F4"
ON SELECTION PAD Due OF (menu_name) Do Sw_Mode WITH "Due"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

ON PAD Pos  OF (menu_name) ACTIVATE POPUP Pos

DEFINE POPUP Pos  MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR 1 OF Pos PROMPT "Поиск" KEY Ctrl-A, "F7"
ON SELECTION BAR 1 OF Pos DO Sw_Mode WITH "Seek1"

DEFINE BAR 2 OF Pos PROMPT "Адрес" KEY Ctrl-A, "F5"
ON SELECTION BAR 2 OF Pos DO Sw_Mode WITH "Addr"

DEFINE BAR 3 OF Pos PROMPT "Доп. строка" KEY Ctrl-A, "Ctrl-Ins"
ON SELECTION BAR 3 OF Pos DO Sw_Mode WITH "Dbl"

IF (sys_char = "A" .OR. sale_mode = "Mitin") .AND. .NOT. stop_new
  DEFINE BAR 4 OF Pos PROMPT "Новая позиция" KEY Ctrl-A, "F3"
  ON SELECTION BAR 4 OF Pos Do Sw_Mode WITH "New_pos"
ENDIF

DEFINE BAR 5 OF Pos PROMPT "Заказы менеджеров" KEY Ctrl-A, "F6"
ON SELECTION BAR 5 OF Pos DO Sw_Mode WITH "Man_Ord"

DEFINE BAR 6 OF Pos PROMPT "Бронь" KEY Ctrl-A, "Ctrl-B"
ON SELECTION BAR 6 OF Pos DO Sw_Mode WITH "Bron"

DEFINE BAR 7 OF Pos PROMPT "Hерасп. долг" KEY Ctrl-A, "Ins"
ON SELECTION BAR 7 OF Pos DO Sw_Mode WITH "Unk"

ON PAD Calc OF (menu_name) ACTIVATE POPUP Calc

DEFINE POPUP Calc MARGIN RELATIVE SHADOW COLOR SCHEME 4

DEFINE BAR 1 OF Calc PROMPT "Документы" KEY Ctrl-A, "F9"
ON SELECTION BAR 1 OF Calc DO Sw_Mode WITH "Doc"

*DEFINE BAR 2 OF Calc PROMPT "Округление до нормы упаковки" KEY Ctrl-A, "F1"
*ON SELECTION BAR 2 OF Calc DO Sw_Mode WITH "Round_U"

DEFINE BAR 2 OF Calc PROMPT "Сумма" KEY Ctrl-A, "F8"
ON SELECTION BAR 2 OF Calc Do Sw_Mode WITH "Sum"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, key_v, h_list, chr_shab, int_1, int_2, rc_sv, mss
DIMENSION int_1(12), int_2(12)   && Описание пунктов меню для программы поиска
int_1(1) = 27      && Esc
int_2(1) = "{Esc}"
int_1(2) = -6      && F7
int_2(2) = "{F7}"
int_1(3) = -1      && F2
int_2(3) = "{F2}"
int_1(4) = -2      && F3
int_2(4) = IIF((sys_char = "A" .OR. sale_mode = "Mitin") .AND. .NOT. stop_new, "{F3}", "")
int_1(5) = -3      && F4
int_2(5) = "{F4}"
int_1(6) = -4      && F5
int_2(6) = "{F5}"
int_1(6) = -5      && F6
int_2(6) = "{F6}"
int_1(7) = -7      && F8
int_2(7) = "{F8}"
int_1(8) = -8      && F9
int_2(8) = "{F9}"
int_1(9) = 28      && F1
int_2(9) = "{F1}"
int_1(10) = 148    && Ctrl-Tab
int_2(10) = "{Ctrl+Tab}"
int_1(11) = 2    && Ctrl-B
int_2(11) = "{Ctrl+B}"
int_1(12) = 22    && Ins
int_2(12) = "{Ins}"

chr_shab = ' !"'+    ;
           "#$%&'()*+,-./0123456789:;<=>?@"+  ;
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"+         ;
           "[\]^_`"+                             ;
           "abcdefghijklmnopqrstuvwxyz"+         ;
           "{|}~"+                               ;
           "АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"+   ;
           "абвгдежзийклмнопрстуфхцчшщъыьэюя"
ln = 100   && Количество видимых строк BROWSE
wd = IIF(INC_TIT.IS_PRICE # " ", ;
            IIF(INC_TIT.IS_PRICE = "$", 77, 77), 64)
key_v  = STR(RECNO("INC_TIT"),5)
h_list = .T.

DO D_Wins WITH ln, wd, ;
   ALLTRIM(INC_TIT.SUP_NAME)+" Накл. N "+ALLTRIM(INC_TIT.INC_DOC)+   ;
   " от "+DTOC(INC_TIT.INC_DATE), 1, 3

@ 1, 3 SAY "Поиск:"
@ 1, 10 SAY PADR("Tab",WCOLS()-10) COLOR (SCHEME(14,2))

DO CASE
CASE INC_TIT.IS_PRICE = " "
  @ 2,  2 SAY ;
  "Д─────────Наименование────────────────────Налич.─По док─Приход─Н"
*  Д Прф_ Наименование_____________ Фирма___ 999999 999999 999999
CASE INC_TIT.IS_PRICE = "$"
  @ 2,  2 SAY ;
  "Д─────────Наименование────────────────По док─Приход───Цена────────Сумма─────Н"
*  Д Прф_ Наименование_________ Фирма___ 999999 999999 99999.9999 9999999.9999
OTHERWISE
  @ 2,  2 SAY ;
  "Д─────────Наименование────────────────По док─Приход─Цена б НДС────Сумма─────Н"
*  Д Прф_ Наименование_________ Фирма___ 999999 999999 9999999.99 999999999.99
ENDCASE

@ WROWS()-5, 3 SAY " Доп. информация "

what_do = "List"
SELECT 0
USE (base_path+"OKSM") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (tmp_path+"DL"+ALLTRIM(STR(RECNO("INC_TIT")))) ALIAS LOG_TMP
SELECT ACT_S_T
SET ORDER TO TAG DOC_NUM

SELECT ACT_S
SET ORDER TO TAG CODE
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO ACT_S_T

SELECT STOCK
SET ORDER TO TAG NAME
GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Tab DO Seek_ WITH IIF(h_list, "", key_v)
    ON KEY LABEL F2 DO Sw_Mode WITH "Sw_Mode"
    ON KEY LABEL F7 DO Sw_Mode WITH "Seek1"
    ON KEY LABEL F5 DO Sw_Mode WITH "Addr"
    ON KEY LABEL Ctrl+Tab Do Sw_Mode WITH "Gtd"
    ON KEY LABEL Ctrl+B Do Sw_Mode WITH "Bron"
    IF (sys_char = "A" .OR. sale_mode = "Mitin") .AND. .NOT. stop_new
      ON KEY LABEL F3 DO Sw_Mode WITH "New_pos"
    ENDIF
    ON KEY LABEL F4 DO Sw_Mode WITH "Due"
    ON KEY LABEL F6 DO Sw_Mode WITH "Man_Ord"
    ON KEY LABEL F8 Do Sw_Mode WITH "Sum"
    ON KEY LABEL F9 DO Sw_Mode WITH "Doc"
    ON KEY LABEL Ins DO Sw_Mode WITH "Unk"
*    ON KEY LABEL Ctrl+Ins DO Sw_Mode WITH "Dbl"
    ON KEY LABEL Ctrl+Ins DO Add_Line
*    ON KEY LABEL F1 DO Sw_Mode WITH "Round_U"

*
*    BROWSE - меню
*
    IF h_list           && Вся номенклатура
      SELECT INC_LIST
      SET ORDER TO TAG L_CODE
      SELECT STOCK
      SET ORDER TO TAG NAME
      SET RELATION TO IIF(SEEK(key_v+STR(CODE,7),"INC_LIST"),  ;
                       key_v+STR(CODE,7), key_v+"      0") INTO INC_LIST
      SET RELATION TO CODE INTO LIST_DET ADDITIVE
      SET RELATION TO NAME+PREFIX+PRODUCER INTO DUE_NAM ADDITIVE
      SET SKIP TO INC_LIST
      DO CASE

      CASE INC_TIT.IS_PRICE = " "
        BROWSE FIELDS ;
               F000 = IIF(FOUND("LIST_DET") .OR. FOUND("DUE_NAM"), "√", " "):H="":W=.F.,  ;
               PRF = STOCK.PREFIX:H="":4:W=.F.,    ;
               Brw_Name=Nm_Part(STOCK.NAME, 25):H="":25:W=.F.,     ;
               PRD = PRODUCER:H="":8:W=.F.,  ;
               RST = STOCK.QNT:H="":W=.F.:P="@Z 999999",  ;
               INC_LIST.QNT_INV:H="":W=.F.:P="@Z 999999",  ;
               INC_LIST.QNT:H="":P="@Z 999999":V=Add_Rec():F, ;
               F_UNK = IIF(INC_LIST.UNK, "√", " "):H="":W=.F. ;
               WHEN Show_Addr(.T.)     ;
               NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR ;
               WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")


      CASE INC_TIT.IS_PRICE = "$"
*  Д Прф_ Наименование_____________ Фирма___ 999999 99999.9999 9999999.9999
        BROWSE FIELDS ;
               F000 = IIF(FOUND("LIST_DET") .OR. FOUND("DUE_NAM"), "√", " "):H="":W=.F.,  ;
               PRF = STOCK.PREFIX:H="":4:W=.F.,    ;
               Brw_Name=Nm_Part(STOCK.NAME, 21):H="":21:W=.F.,     ;
               PRD = PRODUCER:H="":8:W=.F.,  ;
               INC_LIST.QNT_INV:H="":W=.F.:P="@Z 999999",  ;
               INC_LIST.QNT:H="":P="@Z 999999":V=Add_Rec():F,  ;
               INC_LIST.PRICE:H="":P="@Z 99999.9999":W=INC_LIST.QNT#0:V=ClcSum(),  ;
               L_SUM = INC_LIST.QNT*INC_LIST.PRICE:H="":P="@Z 9999999.9999":W=.F., ;
               F_UNK = IIF(INC_LIST.UNK, "√", " "):H="":W=.F. ;
               WHEN Show_Addr(.T.)     ;
               NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
               WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
      OTHERWISE
*  Д Прф_ Наименование_____________ Фирма___ 999999 9999999.99 999999999.99
        BROWSE FIELDS ;
               F000 = IIF(FOUND("LIST_DET") .OR. FOUND("DUE_NAM"), "√", " "):H="":W=.F.,  ;
               PRF = STOCK.PREFIX:H="":4:W=.F.,    ;
               Brw_Name=Nm_Part(STOCK.NAME, 21):H="":21:W=.F.,     ;
               PRD = PRODUCER:H="":8:W=.F.,  ;
               INC_LIST.QNT_INV:H="":W=.F.:P="@Z 999999",  ;
               INC_LIST.QNT:H="":P="@Z 999999":V=Add_Rec():F,  ;
               INC_LIST.PRICE_0:H="":P="@Z 9999999.99":W=INC_LIST.QNT#0.AND.INC_TIT.ENAB_PRICE:V=ClcSum(), ;
               L_SUM = ROUND(INC_LIST.QNT*INC_LIST.PRICE_0*(m.nds_flt+100)/100, 2):H="":P="@Z 999999999.99":W=.F., ;
               F_UNK = IIF(INC_LIST.UNK, "√", " "):H="":W=.F. ;
               WHEN Show_Addr(.T.)     ;
               NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
               WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

      ENDCASE
      SET SKIP TO
    ELSE
      SELECT STOCK
      SET ORDER TO TAG CODE
      SELECT INC_LIST
      SET ORDER TO TAG L_NAME
      SET RELATION TO CODE INTO STOCK
      SET RELATION TO CODE INTO LIST_DET ADDITIVE
      SET RELATION TO NAME+PREFIX+PRODUCER INTO DUE_NAM ADDITIVE

      DO CASE
      CASE INC_TIT.IS_PRICE = " "
        BROWSE KEY key_v FOR INC_LIST.QNT # 0 .OR. INC_LIST.QNT_INV # 0  ;
               FIELDS ;
               F000 = IIF(FOUND("LIST_DET") .OR. FOUND("DUE_NAM"), "√", " "):H="":W=.F.,  ;
               STOCK.PREFIX:H="":4:W=.F.,    ;
               Brw_Name=Nm_Part(STOCK.NAME):H="":25:W=.F.,     ;
               STOCK.PRODUCER:H="":8:W=.F.,  ;
               STOCK.QNT:H="":W=.F.:P="@Z 999999",  ;
               INC_LIST.QNT_INV:H="":W=.F.:P="@Z 999999",  ;
               INC_LIST.QNT:H="":P="@Z 999999":V=Add_Rec():F, ;
               F_UNK = IIF(INC_LIST.UNK, "√", " "):H="":W=.F. ;
               WHEN Show_Addr(.T.)     ;
               NOAPPEND NODELETE REST NOLGRID NOMENU NOCLEAR  ;
               WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

      CASE INC_TIT.IS_PRICE = "$"
        BROWSE KEY key_v FOR INC_LIST.QNT # 0 .OR. INC_LIST.QNT_INV # 0  ;
               FIELDS ;
               F000 = IIF(FOUND("LIST_DET") .OR. FOUND("DUE_NAM"), "√", " "):H="":W=.F.,  ;
               STOCK.PREFIX:H="":4:W=.F.,    ;
               Brw_Name=Nm_Part(STOCK.NAME, 21):H="":21:W=.F.,     ;
               STOCK.PRODUCER:H="":8:W=.F.,  ;
               INC_LIST.QNT_INV:H="":W=.F.:P="@Z 999999",  ;
               INC_LIST.QNT:H="":P="@Z 999999":V=Add_Rec():F,  ;
               INC_LIST.PRICE:H="":P="@Z 99999.9999":W=INC_LIST.QNT#0.AND.INC_TIT.ENAB_PRICE:V=ClcSum(),  ;
               L_SUM = INC_LIST.QNT*INC_LIST.PRICE:H="":P="@Z 9999999.9999":W=.F.,  ;
               F_UNK = IIF(INC_LIST.UNK, "√", " "):H="":W=.F. ;
               WHEN Show_Addr(.T.)     ;
               NOAPPEND NODELETE REST NOLGRID NOMENU NOCLEAR  ;
               WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

      OTHERWISE
        BROWSE KEY key_v FOR INC_LIST.QNT # 0 .OR. INC_LIST.QNT_INV # 0  ;
               FIELDS ;
               F000 = IIF(FOUND("LIST_DET") .OR. FOUND("DUE_NAM"), "√", " "):H="":W=.F.,  ;
               STOCK.PREFIX:H="":4:W=.F.,    ;
               Brw_Name=Nm_Part(STOCK.NAME, 21):H="":21:W=.F.,     ;
               STOCK.PRODUCER:H="":8:W=.F.,  ;
               INC_LIST.QNT_INV:H="":W=.F.:P="@Z 999999",  ;
               INC_LIST.QNT:H="":P="@Z 999999":V=Add_Rec():F,  ;
               INC_LIST.PRICE_0:H="":P="@Z 9999999.99":W=INC_LIST.QNT#0.AND.INC_TIT.ENAB_PRICE:V=ClcSum(), ;
               L_SUM = ROUND(INC_LIST.QNT*INC_LIST.PRICE_0*(m.nds_flt+100)/100, 2):H="":P="@Z 999999999.99":W=.F., ;
               F_UNK = IIF(INC_LIST.UNK, "√", " "):H="":W=.F. ;
               WHEN Show_Addr(.T.)     ;
               NOAPPEND NODELETE REST NOLGRID NOMENU NOCLEAR  ;
               WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
      ENDCASE
    ENDIF
    SET RELATION TO
    IF what_do = "List"
      what_do = "Exit"
    ENDIF
    ON KEY

  CASE what_do = "continue"   && Refresh

    what_do = "List"

  CASE what_do = "Dbl"        && Вторая запись для позиции

    IF INC_LIST.QNT # 0
      SELECT INC_LIST
      SCATTER TO tmpArr
      GO TOP
      IF .NOT. EMPTY(VAL(LIST_NO))
        APPEND FROM ARRAY tmpArr
      ELSE
        GATHER FROM tmpArr
      ENDIF
      &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&  
    ENDIF
    what_do = "List"

  CASE what_do = "Sw_Mode"    && Просмотр списка

    h_list = .NOT. h_list
    what_do = "List"

  CASE what_do = "Seek1"      && Контекстный поиск

    DO Seek1
    what_do = "List"

  CASE what_do = "Gtd"        && ГТД

    DO Choose_GTD
    what_do = "List"

  CASE what_do = "Unk"

    DO Unk_Due
    what_do = "List"

  CASE what_do = "Addr"       && Коррекция адресов

    DO Addr
    what_do = "List"

  CASE what_do = "Doc"        && Заказ по документу

    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Вы уверены, что хотите проставить количества"
    mss(3) = CHR(0)+"  в соответствии с документом? [ Да ] [ Нет ]"
    mss(4) = ""
    IF (Out_Mess(7, "mss") = 1) and (Sale_Mode # "MEGA") && NEW 05.04.2002
      SELECT INC_LIST
      rc_sv = RECNO()
      IF SEEK(key_v)
        REPLACE REST QNT WITH QNT_INV WHILE LIST_NO = key_v
        IF BETWEEN(rc_sv, 1, RECCOUNT())
          GO rc_sv
        ENDIF
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "New_pos"    && Создание новой позиции

    DO Add_Pos1
    what_do = "List"

  CASE what_do = "Man_Ord"    && Заказы менеджеров

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Pos_Ord2 WITH CODE
    ENDIF
    what_do = "List"

  CASE what_do = "Due"        && Обработка долга

    DO Inc_Due
    what_do = "List"

  CASE what_do = "Sum"        && Подсчет суммы по документу

    DO Calc_Sum
    what_do = "List"

  CASE what_do = "Bron"       && Бронь

    DO View_Br WITH STOCK.CODE
    what_do = "List"

  CASE what_do = "Round_U"      && Округление до нормы упаковки

*    DO Round_U
    what_do = "List"
  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

SELECT LOG_TMP
USE
USE IN OKSM

SELECT ACT_S
SET RELATION TO

SELECT INC_TIT
REPLACE LOCKED WITH "***"    && Освободить список
SELECT INC_LIST

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Round_U      Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Округление до нормы упаковки.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.02.2000 ──┘
PROCEDURE Round_U
PRIVATE s_sav, o_sav, r_sav, q, rel_sav
r_sav = RECNO()
s_sav = SELECT()
IF ALIAS() = "STOCK"
  rel_sav = SET("RELATION")
  SET RELATION TO
ENDIF
SELECT STOCK
o_sav = TAG()
SET ORDER TO TAG CODE
SELECT INC_LIST
SET RELATION TO CODE INTO STOCK
SCAN FOR STOCK.QNT_ROUND > 1
  q = CEILING(QNT/STOCK.QNT_ROUND)
  REPLACE QNT WITH q*STOCK.QNT_ROUND
ENDSCAN
SET RELATION TO
SELECT STOCK
SET ORDER TO (o_sav)
SELECT (s_sav)
IF ALIAS() = "STOCK"
  SET RELATION TO &rel_sav
ENDIF
IF BETWEEN(r_sav,1,RECCOUNT())
  GO r_sav
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Round_R      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│          Коррекция рублевых цен для правильного расчета НДС.           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.01.1999 ──┘
PROCEDURE Round_R
REPLACE INC_LIST.PRICE WITH Round_6(INC_LIST.PRICE)
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║        Добавление записи в файл списка позиций приемного акта.         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.05.97 20:41:21 ═╝
PROCEDURE Add_Rec
PRIVATE q_sav, mss, s_sav, q_w, g0, c0, l0

IF INC_LIST.QNT < 0
  IF INC_TIT.BUY = 1   && При покупке только приход!
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Приходуемое количество не может быть отрицательным!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    ACTIVATE WINDOW (IIF(h_list, "STOCK", "INC_LIST"))
    RETURN .F.
  ELSE
    IF INC_LIST.QNT+STOCK.QNT < 0
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Не можем столько вернуть: нет в наличии!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      ACTIVATE WINDOW (IIF(h_list, "STOCK", "INC_LIST"))
      RETURN .F.
    ENDIF
    s_sav = SELECT()
    SELECT ACT_S
    SEEK STOCK.CODE
    q_w = 0
    SCAN REST WHILE CODE = STOCK.CODE
      IF ACT_S_T.SUP_CODE = INC_TIT.SUP_CODE
        q_w = q_w+REST_REP
      ENDIF
    ENDSCAN
    SELECT (s_sav)
    IF q_w < -INC_LIST.QNT
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+IIF(q_w = 0, "На реализации ничего нет!", ;
                          "На реализации всего "+ALLTRIM(STR(q_w))+"!")
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      ACTIVATE WINDOW (IIF(h_list, "STOCK", "INC_LIST"))
      RETURN .F.
    ENDIF
  ENDIF
ENDIF

*IF INC_LIST.QNT+STOCK.QNT-INC_LIST.QNT_DUE < 0
*  DIMENSION mss(3)
*  mss(1) = ""
*  mss(2) = CHR(0)+"Этого количества не хватит для бронирования!"
*  mss(3) = ""
*  DO Out_Mess WITH 7, "mss"
*  RETURN .F.
*ENDIF

IF INC_LIST.QNT # 0
  DO To_Log
ENDIF

*IF QNT = 0
*  REPLACE PRICE WITH 0, PRICE_0 WITH 0
*ENDIF

IF INC_LIST.QNT <= 0 .AND. INC_LIST.UNK
  REPLACE INC_LIST.UNK WITH .F.
ENDIF

IF INC_LIST.QNT # 0 .AND. INC_LIST.CODE = 0
  SELECT INC_LIST
  q_sav = QNT
  REPLACE QNT WITH 0
  *
  *  Добавляем запись
  GO TOP
  IF BOF() .OR. EOF() .OR. .NOT. EMPTY(LIST_NO)
    APPEND BLANK
  ENDIF
  g0 = ""
  c0 = ""
  l0 = ""
  IF INC_TIT.GTD_TYPE = 2
    g0 = INC_TIT.GTD_NUM
    c0 = INC_TIT.COUNTRY
    l0 = INC_TIT.GTD_LINE
  ENDIF
  REPLACE LIST_NO  WITH key_v,    ;
          CODE     WITH STOCK.CODE,     ;
          PREFIX   WITH STOCK.PREFIX,   ;
          NAME     WITH STOCK.NAME,     ;
          PRODUCER WITH STOCK.PRODUCER, ;
          GTD_NUM  WITH g0,             ;
          GTD_LINE WITH l0,             ;
          COUNTRY  WITH c0,             ;
          QNT      WITH q_sav
  IF USED("LOG") .AND. (FOUND("LIST_DET") .OR. FOUND("DUE_NAM"))
    SELECT LOG
    APPEND BLANK
    REPLACE CODE     WITH STOCK.CODE, ;
            PREFIX   WITH STOCK.PREFIX, ;
            NAME     WITH STOCK.NAME, ;
            PRODUCER WITH STOCK.PRODUCER, ;
            DATE     WITH DATE(), ;
            WHO      WITH user,   ;
            WHERE    WITH arm
  ENDIF
  SELECT STOCK
*  GO RECNO()
*  SHOW WINDOW (ALIAS()) SAME REFRESH
*  = LOCK()
*  DO Show_Addr WITH .T.
  KEYBOARD CHR(27)
  m.what_do = "continue"
ENDIF

DO ClcSum

IF INC_LIST.QNT = 0 .AND. INC_LIST.QNT_INV = 0 .AND. INC_LIST.CODE # 0
  SELECT INC_LIST
  BLANK
  SELECT STOCK
  GO RECNO()
  m.what_do = "continue"
  KEYBOARD CHR(27)
ENDIF

ACTIVATE WINDOW (IIF(h_list, "STOCK", "INC_LIST"))
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Программа поиска по начальным буквам.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.05.97 22:05:53 ═╝
PROCEDURE Seek_
PARAMETER k_v

PRIVATE s, prf, ln, bg, rc_s, i
IF h_list           && Вся номенклатура
  SELECT STOCK
  SET ORDER TO TAG NAME
  SET RELATION TO key_v+STR(CODE,7) INTO INC_LIST
  SET RELATION TO CODE INTO LIST_DET ADDITIVE
  SET RELATION TO NAME+PREFIX+PRODUCER INTO DUE_NAM ADDITIVE
  SET SKIP TO INC_LIST
ENDIF

ln = FSIZE("NAME", "STOCK")
ln = 40
ACTIVATE WINDOW (win_name+"_M") SAME
prf = ""
@ 1, 3 SAY "Поиск:"
@ 1, 10 SAY PADR("Tab",ln) COLOR (SCHEME(14,2))
bg = REPLICATE("▒", ln)
DO WHILE .T.
  DO Show_Addr
  @ 1, 10 SAY bg
  @ 1, 10 SAY prf
  s = INKEY(0)
  i = ASCAN(int_1, s)
  IF i # 0
    KEYBOARD int_2(i)
    EXIT
  ENDIF
  IF s < 0     && Функциональная клавиша
    IF s = -9  && F10 - запускаем меню
      KEYBOARD "{F10}"
      EXIT
    ENDIF
    LOOP
  ENDIF
  s = CHR(s)
  IF s $ chr_shab .AND. LEN(prf) < ln   && Символ
    rc_s = RECNO()
    IF SEEK(k_v+prf+s)
      prf = prf+s
    ELSE
      IF rc_s < RECCOUNT() .AND. rc_s > 0
        GO rc_s
      ENDIF
    ENDIF
    SHOW WINDOW (ALIAS()) REFRESH
    LOOP
  ENDIF
  IF s = CHR(13)       && Enter
    EXIT
  ENDIF
  IF s = CHR(3)        && PgDn
    KEYBOARD "{PgDn}"
    EXIT
  ENDIF
  IF s = CHR(18)       && PgUp
    KEYBOARD "{PgUp}"
    EXIT
  ENDIF
  IF s = CHR(5)        && UpArrow
    KEYBOARD "{UpArrow}"
    EXIT
  ENDIF
  IF s = CHR(24)       && DnArrow
    KEYBOARD "{DnArrow}"
    EXIT
  ENDIF
  IF s = CHR(127)      && BackSpace
    IF LEN(prf) > 0
      prf = LEFT(prf, LEN(prf)-1)
      SEEK k_v+prf
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
    LOOP
  ENDIF
ENDDO

IF h_list           && Вся номенклатура
  SELECT STOCK
  SET ORDER TO TAG NAME
  SET RELATION TO IIF(SEEK(key_v+STR(CODE,7),"INC_LIST"),  ;
                  key_v+STR(CODE,7), key_v+"      0") INTO INC_LIST
  SET RELATION TO CODE INTO LIST_DET ADDITIVE
  SET RELATION TO NAME+PREFIX+PRODUCER INTO DUE_NAM ADDITIVE
  SET SKIP TO INC_LIST
ENDIF

@ 1, 10 SAY PADR("Tab",ln) COLOR (SCHEME(14,2))

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Коррекция адресов.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.97 10:48:15 ═╝
PROCEDURE Addr

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

PRIVATE ex, addr1, addr2, pk, pk_m  &&
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
addr1 = STOCK.ADDRESS
addr2 = STOCK.MEMO_ST
pk    = STOCK.QNT_ROUND
pk_m  = STOCK.QNT_PACK
*------------------------------------------------------------------------
*
*┌──────────────────────────────────────────────────────┐
*│                                                      │
*│Основной адрес ▒▒▒▒▓▒▒▒??  Доп. адрес ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒│
*│                                                      │
*│          Упаковка ▒▒▒▒▓▒   Фасовка ▒▒▒▒▓▒▒▒          │
*│                                                      │
*│           < OK Ctrl-W > < Отказаться Esc >           │
*│                                                      │
*└──────────────── F1 - история адресов ────────────────┘

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 58, ALLTRIM(STOCK.PREFIX)+" "+ALLTRIM(STOCK.NAME)+" "+  ;
                       ALLTRIM(STOCK.PRODUCER)
@ WROWS()-1, 18 SAY " F1 - история адресов "
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  2 SAY "Основной адрес" GET addr1 PICTURE Addr_Pic()
@ 3, 29 SAY "Доп. адрес" GET addr2
@ 5, 12 SAY "Упаковка" GET pk PICTURE "@Z 999999" VALID Tst_Pack(1)
@ 5, 30 SAY "Фасовка" GET pk_m PICTURE "@Z 99999999" VALID Tst_Pack(2)
@ 7, 13 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

ON KEY LABEL F1 DO Addr_Hst WITH addr1, addr2, STOCK.ADDR_0, STOCK.MEMO_ST_0, STOCK.ADDR_1, STOCK.MEMO_ST_1
READ CYCLE
ON KEY LABEL F1

IF ex = 1
  IF STOCK.ADDRESS # addr1
    REPLACE STOCK.ADDR_1  WITH STOCK.ADDR_0
    REPLACE STOCK.ADDR_0  WITH STOCK.ADDRESS
    REPLACE STOCK.ADDRESS WITH addr1
  ENDIF
  IF STOCK.MEMO_ST # addr2
     REPLACE STOCK.MEMO_ST_1 WITH STOCK.MEMO_ST_0
     REPLACE STOCK.MEMO_ST_0 WITH STOCK.MEMO_ST
     REPLACE STOCK.MEMO_ST   WITH addr2
  ENDIF
  REPLACE STOCK.QNT_ROUND WITH pk, ;
          STOCK.QNT_PACK  WITH pk_m
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Проверка нормы упаковки.                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.97 10:48:15 ═╝
PROCEDURE Tst_Pack
PARAMETER sw

IF ex = 2 .OR. MOD(READKEY(), 256) = 12
  RETURN .T.
ENDIF
IF sw = 1
  IF pk <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Таких упаковок не бывает!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ELSE
  IF pk_m <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Таких фасовок не бывает!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Вывод адресов в информационное окошко.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.97 11:02:58 ═╝
PROCEDURE Show_Addr
PARAMETER sw_wind

PRIVATE c1

IF sw_wind
  ACTIVATE WINDOW (win_name+"_M") SAME
ENDIF

@ WROWS()-4, 3 SAY "Адреса "
@ ROW(), COL() SAY STOCK.ADDRESS PICTURE Addr_Pic() COLOR SCHEME 1
@ ROW(), COL()+1 SAY STOCK.MEMO_ST COLOR SCHEME 1
IF INC_TIT.IS_PRICE $ "$Р"
  @ ROW(), COL() SAY "  Склад "
  @ ROW(), COL() SAY STOCK.QNT PICTURE "99999999" COLOR SCHEME 1
ENDIF
@ ROW(), COL() SAY "  Бронь "
@ ROW(), COL() SAY STOCK.QNT_SAVE PICTURE "99999999" COLOR SCHEME 1
@ WROWS()-3, 3 SAY "ГТД N "
@ ROW(), COL() SAY INC_LIST.GTD_NUM-"/"-INC_LIST.GTD_LINE COLOR SCHEME 1
@ ROW(), COL() SAY " Страна "
@ ROW(), COL() SAY INC_LIST.COUNTRYC COLOR SCHEME 1
IF EMPTY(INC_LIST.COUNTRYC)
  m.c1 = LEFT(INC_LIST.COUNTRY,11)
ELSE
  IF SEEK(INC_LIST.COUNTRYC, "OKSM")
    m.c1 = LEFT(OKSM.B_NAME,11)
  ELSE
    m.c1 = LEFT(INC_LIST.COUNTRY,11)
  ENDIF
ENDIF
@ ROW(), COL()+1 SAY m.c1 COLOR SCHEME 1
*@ ROW(), COL()+1 SAY LEFT(INC_LIST.COUNTRY,11) COLOR SCHEME 1

*@ WROWS()-2, 3 SAY "Позиция "
*@ ROW(), COL() SAY STOCK.PREFIX COLOR SCHEME 1
@ WROWS()-2, 3 SAY LEFT(STOCK.NAME, WCOLS()-6) COLOR SCHEME 1
*@ ROW(), COL()+1 SAY STOCK.PRODUCER COLOR SCHEME 1

IF FOUND("DUE_NAM")
  PRIVATE s_sav
  s_sav = SELECT()
  SELECT DUE_NAM
  DO WHILE SEEK(STOCK.NAME+STOCK.PREFIX+STOCK.PRODUCER)
    REPLACE CODE WITH STOCK.CODE
  ENDDO
  SELECT (s_sav)
ENDIF

DO CASE
CASE INC_TIT.IS_PRICE = "$"
  m.sum_line = INC_LIST.QNT*INC_LIST.PRICE
CASE INC_TIT.IS_PRICE = "Р"
  m.sum_line = ROUND(INC_LIST.QNT*INC_LIST.PRICE_0*(m.nds_flt+100)/100, 2)
ENDCASE

*DO ClcSum

IF sw_wind
  ACTIVATE WINDOW (ALIAS()) SAME
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Процедура контекстного поиска.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.97 11:59:42 ═╝
PROCEDURE Seek1

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

PRIVATE ex, nm_w     &&
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
nm_w = SPACE(25)     &&
*------------------------------------------------------------------------
*
*┌─────────────────────────────────────────────────────┐
*│                                                     │
*│ Задайте шаблон для поиска ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓ │
*│                                                     │
*│          < OK Ctrl-W > < Отказаться Esc >           │
*│                                                     │
*└─────────────────────────────────────────────────────┘
PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 57, "Поиск в "+IIF(h_list, "номенклатуре.", "списке.")

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  3 SAY "Задайте шаблон для поиска" GET nm_w
@ 5, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE
nm_w = ALLTRIM(nm_w)

IF ex = 1 .AND. .NOT. EMPTY(nm_w)
  DO Choose_P WITH nm_w, h_list
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Подборка по контексту и выбор позиции.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.97 12:17:29 ═╝
PROCEDURE Choose_P
PARAMETERS shab, is_stock
PRIVATE sel_sav

sel_sav = SELECT()

DO Wt_Mess WITH "Подборка..."
IF is_stock
  SELECT PREFIX, NAME, PRODUCER, RECNO() AS REC_N FROM STOCK  ;
         INTO CURSOR TMP_C   ;
         WHERE (shab $ NAME .OR. shab $ PREFIX .OR. shab $ PRODUCER)  ;
               .AND. EMPTY(DATE_OFF)
ELSE
  SELECT PREFIX, NAME, PRODUCER, RECNO() AS REC_N FROM INC_LIST  ;
         INTO CURSOR TMP_C   ;
         WHERE (shab $ NAME .OR. shab $ PREFIX .OR. shab $ PRODUCER)  ;
               .AND. LIST_NO = key_v .AND. (QNT # 0 .OR. QNT_INV # 0)
ENDIF
DO Wt_Mess
SELECT TMP_C
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не удалось найти..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_sav)
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
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = 49
DO D_Wins WITH ln, wd, "Выберите позицию", 0, 0
what_do = "list"

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS F000 = PREFIX+" "+NAME+" "+PRODUCER:H="":49,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Просмотр списка

    IF is_stock
      GO TMP_C.REC_N IN STOCK
    ELSE
      GO TMP_C.REC_N IN INC_LIST
    ENDIF
    what_do = "exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

SELECT TMP_C
USE
SELECT (sel_sav)
RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║           Предобработчик программы создания новой позиции.             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 08.05.97 12:59:01 ═╝
PROCEDURE Add_Pos1

PRIVATE rc, set_q, sel_sav, g0, c0, l0

set_q = .F.

IF SEEK(key_v, "INC_NEW")
  IF Ch_New()
    rc = Add_Pos(INC_NEW.PREFIX, INC_NEW.NAME, INC_NEW.PRODUCER, STOCK.DIV_CODE, STOCK.STOP_PUR)
    IF rc # 0
      GO rc IN STOCK
    ENDIF
    set_q = .T.
*    set_q = INC_NEW.PREFIX = STOCK.PREFIX .AND. ;
*            INC_NEW.NAME   = STOCK.NAME   .AND. ;
*            INC_NEW.PRODUCER = STOCK.PRODUCER
  ELSE
*    rc = Add_Pos(STOCK.PREFIX, STOCK.NAME, STOCK.PRODUCER, STOCK.DIV_CODE, STOCK.STOP_PUR)
    rc = Modi_Pos(0, STOCK.PREFIX, STOCK.NAME, STOCK.PRO_CODE, STOCK.DIV_CODE, STOCK.CLASS, STOCK.STOP_PUR)
  ENDIF
ELSE
*  rc = Add_Pos(STOCK.PREFIX, STOCK.NAME, STOCK.PRODUCER, STOCK.DIV_CODE, STOCK.STOP_PUR)
  rc = Modi_Pos(0, STOCK.PREFIX, STOCK.NAME, STOCK.PRO_CODE, STOCK.DIV_CODE, STOCK.CLASS, STOCK.STOP_PUR)
ENDIF
IF rc # 0
  GO rc IN STOCK
  IF set_q = .T.
    g0 = ""
    c0 = ""
    l0 = ""
    IF INC_TIT.GTD_TYPE = 2
      g0 = INC_TIT.GTD_NUM
      c0 = INC_TIT.COUNTRY
      l0 = INC_TIT.GTD_LINE
    ENDIF
    sel_sav = SELECT()
    SELECT INC_LIST
    APPEND BLANK
    REPLACE LIST_NO  WITH key_v,          ;
            CODE     WITH STOCK.CODE,     ;
            PREFIX   WITH STOCK.PREFIX,   ;
            NAME     WITH STOCK.NAME,     ;
            PRODUCER WITH STOCK.PRODUCER, ;
            QNT_INV  WITH INC_NEW.QNT,    ;
            IDP      WITH INC_NEW.IDP,    ;
            INV_REF  WITH INC_NEW.INV_REF,;
            GTD_NUM  WITH g0,             ;
            GTD_LINE WITH l0,             ;
            COUNTRY  WITH c0
    SELECT (sel_sav)
  ENDIF
  h_list = .T.
ENDIF

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                   Выбор новой позиции из заготовки.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.05.97 20:56:47 ═╝
PROCEDURE Ch_New

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
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, sel_sav
sel_sav = SELECT()
SELECT INC_NEW
ln = 100   && Количество видимых строк BROWSE
wd = FSIZE("PREFIX")+FSIZE("NAME")+FSIZE("PRODUCER")+2

DO D_Wins WITH ln, wd, "Новые позиции из накладной", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE KEY key_v     ;
           FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN what_do = "Choose"

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║               Программа подсчета суммы по заготовке.                  ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Calc_Sum

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
*│                                      │
*│  999999999.99 руб    999999999.99 $  │
*│                                      │
*│        Нажмите что-нибудь...         │
*└──────────────────────────────────────┘
PRIVATE s000r, s000d, s_sav, r_sav

s_sav = SELECT()
SELECT INC_LIST
r_sav = RECNO()

s000r = 0
s000d = 0
IF SEEK(STR(RECNO("INC_TIT"),5))
  DO CASE
  CASE INC_TIT.IS_PRICE = "Р"
    SCAN REST WHILE LIST_NO = STR(RECNO("INC_TIT"),5)
      s000r = s000r+QNT*PRICE
      s000d = s000d+QNT*PRICE/kurs_b
    ENDSCAN
  CASE INC_TIT.IS_PRICE = "$"
    SCAN REST WHILE LIST_NO = STR(RECNO("INC_TIT"),5)
      s000r = s000r+QNT*PRICE*kurs_b
      s000d = s000d+QNT*PRICE
    ENDSCAN
  ENDCASE
ENDIF

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
SELECT (s_sav)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 42, "Сумма"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  4 SAY TRANSFORM(s000r, "999999999.99")+" руб"
@ 3, 24 SAY TRANSFORM(s000d, "999999999.99")+" $"
@ 5,  2 SAY PADC("Нажмите что-нибудь...", WCOLS()-4)

= INKEY(0,"MH")

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура To_Log       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Запись в журнал при наличии долга!                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 30.12.1998 ──┘
PROCEDURE To_Log

PRIVATE s_sav
IF FOUND("LIST_DET") .OR. FOUND("DUE_NAM")
  s_sav = SELECT()
  SELECT LOG_TMP
  APPEND BLANK
  REPLACE DATE_DUE WITH DATE(),     ;
          TIME_DUE WITH TIME(),     ;
          STO_CODE WITH STOCK.CODE, ;
          WHO      WITH user,       ;
          WHERE    WITH arm
  WAIT WINDOW "ВНИМАНИЕ! Долг!" NOWAIT
  SELECT (s_sav)
ENDIF
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Choose_GTD   Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Выбор ГТД.                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 16.02.2000 ──┘
PROCEDURE Choose_GTD

PRIVATE tmpGTD, point1, point2, mss, s_sav

IF INC_LIST.CODE = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Номер ГТД для пустого количества!?"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
IF INC_LIST.QNT = 0 .AND. INC_LIST.QNT_INV = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  Номер ГТД для пустого количества! Это правильно? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN
  ENDIF
ENDIF

s_sav = SELECT()
*
*  Переменные состояния для навигации
*
SELECT INC_LIST

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

PRIVATE ex, tmpNum, tmpLine, tmpCountry, tmpCountryC, tmpPos, tmpSw  &&
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
tmpNum = GTD_NUM
tmpLine = GTD_LINE
tmpCountry = COUNTRY
tmpCntC = COUNTRYC
tmpSw   = .F.
*------------------------------------------------------------------------
*
*┌──────────────────────────────────────────────────────┐
*│ № ГТД  Строка ГТД  │
*│                                                      │
*│   Страна  │
*│           < OK Ctrl-W > < Отказаться Esc >           │
*└──────────────────────────────────────────────────────┘

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 24+LEN(tmpNum)+LEN(tmpLine), "ГТД"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2,  3 SAY "№ ГТД" GET tmpNum
@ 2, COL()+1 SAY "Строка ГТД" GET tmpLine
tmpPos = FLOOR((WCOLS()-12-LEN(tmpCountry)-LEN(tmpCntC))/2)
@ 4, tmpPos SAY "Страна" GET tmpSw PICTURE "@*C" VALID G_Ctry1()
@ 4, COL() GET tmpCntC VALID G_Ctry()
@ 4, COL()+1 GET tmpCountry WHEN .F.
tmpPos = FLOOR((WCOLS()-32)/2)
@ 6, tmpPos GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE

IF ex = 1
  REPLACE GTD_NUM  WITH tmpNum, ;
          GTD_LINE WITH tmpLine,;
          COUNTRY  WITH tmpCountry,;
          COUNTRYC WITH tmpCntC
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура ClcSum       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Измененние суммы по строке.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.09.2005 ──┘
PROCEDURE ClcSum
PRIVATE newPrice, pic

IF .NOT. INC_TIT.IS_PRICE $ "$Р"
  RETURN .T.
ENDIF

DO CASE
CASE INC_TIT.IS_PRICE = "$"
  m.newPrice = INC_LIST.QNT*INC_LIST.PRICE
  REPLACE INC_LIST.PRICE_0 WITH INC_LIST.PRICE
  m.pic = "9999999999.9999"
CASE INC_TIT.IS_PRICE = "Р"
  REPLACE INC_LIST.PRICE WITH INC_LIST.PRICE_0/100*(100+nds_flt)
  m.newPrice = ROUND(INC_LIST.QNT*INC_LIST.PRICE_0*(m.nds_flt+100)/100, 2)
  m.pic = "999999999999.99"
ENDCASE
m.sum_doc = m.sum_doc-m.sum_line+m.newPrice
m.sum_line = m.newPrice
ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-5, WCOLS()-17 SAY TRANSFORM(m.sum_doc, m.pic)
RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Unk_Due       Разработчик Андрей Васин                     │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Hерасп. долг                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.06.2011 ──┘
PROCEDURE Unk_Due

PRIVATE s_sav

IF EOF("INC_LIST") .OR. BOF("INC_LIST")
  RETURN
ENDIF

s_sav = SELECT()
SELECT INC_LIST
IF INC_LIST.QNT = 0
  REPLACE INC_LIST.UNK WITH .F.
  SELECT (s_sav)
  RETURN
ENDIF

REPLACE INC_LIST.UNK WITH .NOT. INC_LIST.UNK
  SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Ctry       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Поиск страны по коду                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.02.2012 ──┘
PROCEDURE G_Ctry
PRIVATE tmpVal

IF EMPTY(m.tmpCntC)
  m.tmpCountry = SPACE(LEN(m.tmpCountry))
  SHOW GET m.tmpCountry
  RETURN .T.
ENDIF
m.tmpVal = PADL(ALLTRIM(m.tmpCntC), 3, "0")
IF .NOT. SEEK(m.tmpVal, "OKSM")
  m.tmpCountry = SPACE(LEN(m.tmpCountry))
  m.tmpCntC = SPACE(LEN(m.tmpCntC))
  SHOW GET m.tmpCountry
  SHOW GET m.tmpCntC
  RETURN .F.
ENDIF
m.tmpCountry = LEFT(OKSM.B_NAME, LEN(m.tmpCountry))
m.tmpCntC = m.tmpVal
SHOW GET m.tmpCountry
SHOW GET m.tmpCntC
RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Ctry1       Разработчик Андрей Васин                     │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                                 Выбор страны.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.02.2012 ──┘
PROCEDURE G_Ctry1

PRIVATE tmpV

m.tmpSw = .F.
m.tmpV = Ctrl_Ch(m.tmpCntC)
IF .NOT. EMPTY(m.tmpV)
  m.tmpCntC = LEFT(m.tmpV, 3)
  m.tmpCountry = SUBSTR(m.tmpV, 4, LEN(m.tmpCountry))
  SHOW GET m.tmpCntC
  SHOW GET m.tmpCountry
ENDIF
SHOW GET m.tmpSw

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Add_Line      Разработчик Андрей Васин                     │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Добавить строку.                         │                                                                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.02.2012 ──┘
PROCEDURE Add_Line

IF INC_LIST.QNT # 0
  SELECT INC_LIST
  SCATTER TO tmpArr
  GO TOP
  IF .NOT. EMPTY(VAL(LIST_NO))
    APPEND FROM ARRAY tmpArr
  ELSE
    GATHER FROM tmpArr
  ENDIF
  REPLACE IDP		WITH "",	;
          QNT		WITH 0,		;
          QNT_INV	WITH 0,		;
          QNT_DUE	WITH 0,		;
          INV_REF	WITH 0,		;
          UNK		WITH .F.,	;
          PRICE		WITH 0,		;
          PRICE_0	WITH 0
          
ENDIF
