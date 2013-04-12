*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Inc_Tit1     Разработчик Андрей Васин           06.05.97 09:39:47 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Формирование заготовки приемного акта.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Inc_Tit1

*
*   Вне программы должны быть определены переменные:
*      - prod_c   - массив кодов фирм производителей;
*      - prod_n   - массив имен фирм производителей;
*
PRIVATE f_c000, f_n000, b_c000, b_n000, b_sw000, f_c_old
PRIVATE shab1, shab2, tmp_D_Code, sw_doc, prc_type
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
PRIVATE sw_0001, sw_0002
sw_0001 = .F.
sw_0002 = .F.
tmp_D_Code = 0
sw_doc = .F.
prc_type = 1

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
f_c000 = def_firm
f_n000 = ""
f_c_old= def_firm

*
*┌────────────────────────────────────────────┐
*│   Предприятие ░░░ ░░░░▓░░░░█░░░░▓░ (F1)    │
*│   ┌─ [ ] Поставщик ───────────────────┐    │
*│   │         ░░░░▓░░░░█░░░░▓░          │    │
*│   └───────────────────────────────────┘    │
*│┌─ [ ] Накладная ──────────────────────────┐│
*││ Номер ░░░░▓░░░░█░░░░▓░░░░█ от ░░░░▓░░░░█ ││
*│└──────────────────────────────────────────┘│
*│   ┌─ Цена ─────────────────────────────┐   │
*│   │ ( ) Нет    ( ) Рубли   ( ) Доллары │   │
*│   └────────────────────────────────────┘   │
*│┌─ Примечание ─────────────────────────────┐│
*││░░░░▓░░░░█░░░░▓░░░░█░░░░▓░░░░█░░░░▓░░░░█░░││
*│└──────────────────────────────────────────┘│
*│      < OK Ctrl-W > < Отказаться Esc >      │
*└────────────────────────────────────────────┘

*------------------------------------------------------------------------
*    Содержательная часть программы:
*
IF RECCOUNT("TMP_INC") # 0
  SELECT TMP_INC
  ZAP
  SELECT INC_TIT
ENDIF
IF RECCOUNT("TMP_NEW") # 0
  SELECT TMP_NEW
  ZAP
  SELECT INC_TIT
ENDIF

PRIVATE ex, arr_w, tit_rec, code_w, rc_sav, sup_sw, tmpGTD, buy_ryn, is_file
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
sup_sw = .F.
code_w = 0
tmpGTD = .T.
buy_ryn = 1
is_file = 1
*------------------------------------------------------------------------
*
*   Захватываем запись заголовка
*
rc_sav = RECNO()
SCATTER TO arr_w BLANK
LOCATE FOR EMPTY(SUP_CODE) .AND. EMPTY(LOCKED)
IF FOUND()
  GATHER FROM arr_w
ELSE
  APPEND BLANK
ENDIF
REPLACE SUP_NAME WITH "",        ;
        FILE_TYPE WITH 1,        ;
        LOCKED WITH arm,         ;
        BUY WITH IIF(sale_mode = "Mitracon", 2, 1),  ;
        GTD_NUM WITH "",         ;
        COUNTRY WITH "",         ;
        GTD_TYPE WITH 1
tit_rec = RECNO()

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 17, 48, "Заголовок заготовки приемного акта"
DO Sun_Bord WITH 3, 5, 5, 41, " [ ] Поставщик "
DO Sun_Bord WITH 6, 2, 8, 45, " [ ] Накладная "
DO Sun_Bord WITH 9, 6, 11, 42, " Цена "
DO Sun_Bord WITH 12, 2, 14, 45, " Примечание "
*------------------------------------------------------------------------
*      Ввод полей бланка
*

DO Get_Firm WITH 2, 5, enab_firm

@  3,  8 GET sup_sw PICTURE "@*C Поставщик" VALID Get_Supl()
@  4, 15 GET INC_TIT.SUP_NAME WHEN .F.
@  6,  5 GET sw_doc PICTURE "@*C Накладная" VALID G_Inv()
&&@  8, 15 GET is_file PICTURE "@*RH Нет  ;Есть" VALID Clr_Inv()
&&@ 10, 10 GET tmpGtd PICTURE "@*C ГТД доступна для ввода"
&&      VALID Clr_F() WHEN INC_TIT.FILE_TYPE = 1 .AND. .F. && Никогда!
@  7,  4 SAY "Номер" GET INC_TIT.INC_DOC WHEN .NOT. sw_doc
@  7, 31 SAY "от" GET INC_TIT.INC_DATE WHEN  .NOT. sw_doc
&&@ 11, 10 GET buy_ryn PICTURE "@*RH Покупка  ;Реализация" WHEN .NOT. sw_doc
@ 10, 7 GET m.prc_type PICTURE "@*RH Нет  ; Рубли ; Доллары" WHEN .NOT. sw_doc
@ 13, 3 GET INC_TIT.NOTE PICTURE "@S42"
@ 15,  8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE VALID Get_Inv0()

REPLACE INC_TIT.FIRM WITH f_c000

SELECT STOCK
SET ORDER TO TAG CODE

SELECT INC_TIT

IF ex = 1
  DO CASE
  CASE m.prc_type = 1
    m.prc_type = ""
  CASE m.prc_type = 2
    m.prc_type = "Р"
  CASE m.prc_type = 3
    m.prc_type = "$"
  ENDCASE
  PRIVATE sw
  REPLACE SUP_CODE   WITH code_w,                  ;
          FILE_TYPE  WITH is_file,                 ;
          GTD_TYPE   WITH IIF(tmpGTD, 1, 0),       ;
          IS_PRICE   WITH m.prc_type,              ;
          ENAB_PRICE WITH .NOT. EMPTY(m.prc_type), ;
          NDS_       WITH IIF(.NOT. m.prc_type $ " $", m.nds_flt, 0), ;
          BUY        WITH buy_ryn
*
*    Заполняем заготовку накладной
  SELECT TMP_INC
  SCAN
    SCATTER TO arr_w
    SELECT INC_LIST
    GO TOP
    IF .NOT. BOF() .AND. .NOT. EOF() .AND. EMPTY(VAL(LIST_NO))
      GATHER FROM arr_w
    ELSE
      APPEND FROM ARRAY arr_w
    ENDIF
    SELECT TMP_INC
  ENDSCAN
  SELECT INC_LIST
  SET ORDER TO TAG L_CODE

*
*    Заполняем заготовку нескладских позиций
  SELECT TMP_NEW
  SCAN FOR .NOT. DELETED()
    SCATTER TO arr_w
    SELECT INC_NEW
    GO TOP
    IF .NOT. EMPTY(VAL(LIST_NO)) .OR. BOF() .OR. EOF()
      APPEND BLANK            &&
    ENDIF
    GATHER FROM arr_w
    SELECT TMP_NEW
  ENDSCAN

  SELECT INC_LIST
  GO TOP
  IF EOF() .OR. BOF() .OR. .NOT. EMPTY(VAL(LIST_NO))
    APPEND BLANK
  ENDIF
  REPLACE LIST_NO WITH STR(tit_rec,5)
  SELECT 0
  USE (log_path+"DUE_LOG")
  COPY STRU TO (tmp_path+"DL"+ALLTRIM(STR(tit_rec)))
  USE
  SELECT INC_TIT
  REPLACE LOCKED WITH "***"
ELSE
  BLANK
  IF rc_sav > 0 .AND. rc_sav < RECCOUNT()
    GO rc_sav
  ENDIF
ENDIF

*--------------------------------------------------------------------------
POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                               Тип ГТД.                                 ║
*║                                                                        ║
*╚══════════════════════════════════════════════════ 14.02.2000 11:13:38 ═╝
PROCEDURE Clr_F

REPLACE INC_TIT.GTD_NUM WITH "", ;
        INC_TIT.COUNTRY WITH ""
SHOW GET INC_TIT.GTD_NUM
SHOW GET INC_TIT.COUNTRY

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Выбор поставщика.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.05.97 11:13:38 ═╝
PROCEDURE Get_Supl

IF .NOT. sup_sw
  RETURN .T.
ENDIF

sup_sw = .F.
PRIVATE tmpName1, tmpName2
tmpName1 = INC_TIT.SUP_NAME
tmpName2 = ""


DO Get_Sup WITH code_w, tmpName1, tmpName2

REPLACE SUP_NAME WITH tmpName1, SUP_CODE WITH code_w

SHOW GET sup_sw
SHOW GET INC_TIT.SUP_NAME

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Программа чтения накладной.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.05.97 15:57:27 ═╝
PROCEDURE Get_Inv0
PARAMETER md

*IF .NOT. md
  IF ex = 2
    RETURN .T.
  ENDIF

  IF EMPTY(code_w)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Вы не указали поставщика!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF

  IF EMPTY(f_c000)
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Вы не указали предприятие!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
*  RETURN .T.
*ENDIF

DO CASE
CASE INC_TIT.INC_CODE # 0 				   && CMP_DOCS!
  IF .NOT. Get_InvN()      && По какой-то причине накладная не прочитана
    ACTIVATE WINDOW (win_name)
    REPLACE INC_TIT.INC_DOC   WITH " ", ;
            INC_TIT.INC_CODE  WITH 0,   ;
            INC_TIT.INC_DATE  WITH {},  ;
            INC_TIT.FILE_TYPE WITH 1
    tmp_D_Code = 0
    sw_doc = .F.
    m.prc_type = 1
    SHOW GET INC_TIT.INC_DOC
    SHOW GET INC_TIT.INC_DATE
    SHOW GET INC_TIT.FILE_TYPE
    SHOW GET sw_doc
    SHOW GET m.prc_type
    SELECT TMP_INC
    ZAP
    SELECT INC_TIT
    ex = 2
    RETURN .T.
  ENDIF
  SHOW GET INC_TIT.INC_DOC
  SHOW GET INC_TIT.INC_DATE
  RETURN .T.
CASE is_file = 1    && Нет файла накладной!
*  REPLACE INC_TIT.INC_DOC WITH " ",  INC_TIT.INC_DATE WITH {}
*  SHOW GET INC_TIT.INC_DOC
*  SHOW GET INC_TIT.INC_DATE
  SELECT TMP_INC
  ZAP
  SELECT INC_TIT
  RETURN .T.
CASE is_file = 2    && Файл в формате обмена с филиалом!
  IF .NOT. Get_Inv1()     && По какой-то причине накладная не прочитана
    ACTIVATE WINDOW (win_name)
    REPLACE INC_TIT.INC_DOC   WITH " ", ;
            INC_TIT.INC_DATE  WITH {},  ;
            INC_TIT.FILE_TYPE WITH 1
    SHOW GET INC_TIT.INC_DOC
    SHOW GET INC_TIT.INC_DATE
    SHOW GET INC_TIT.FILE_TYPE
    SELECT TMP_INC
    ZAP
    SELECT INC_TIT
    ex = 2
    RETURN .T.
  ENDIF
  SHOW GET INC_TIT.INC_DOC
  SHOW GET INC_TIT.INC_DATE
  RETURN .T.
CASE INC_TIT.FILE_TYPE = 3    && Файл в формате Sing!
  IF .NOT. Get_Inv2()     && По какой-то причине накладная не прочитана
    ACTIVATE WINDOW (win_name)
    REPLACE INC_TIT.INC_DOC   WITH " ", ;
            INC_TIT.INC_DATE  WITH {},  ;
            INC_TIT.FILE_TYPE WITH 1
    SHOW GET INC_TIT.INC_DOC
    SHOW GET INC_TIT.INC_DATE
    SHOW GET INC_TIT.FILE_TYPE
    SELECT TMP_INC
    ZAP
    SELECT INC_TIT
    ex = 2
    RETURN .T.
  ENDIF
  SHOW GET INC_TIT.INC_DOC
  SHOW GET INC_TIT.INC_DATE
  RETURN .T.
CASE INC_TIT.FILE_TYPE = 4    && Файл в формате АС!
  IF .NOT. Get_Inv3()     && По какой-то причине накладная не прочитана
    ACTIVATE WINDOW (win_name)
    REPLACE INC_TIT.INC_DOC   WITH " ", ;
            INC_TIT.INC_DATE  WITH {},  ;
            INC_TIT.FILE_TYPE WITH 1
    SHOW GET INC_TIT.INC_DOC
    SHOW GET INC_TIT.INC_DATE
    SHOW GET INC_TIT.FILE_TYPE
    SELECT TMP_INC
    ZAP
    SELECT INC_TIT
    ex = 2
    RETURN .T.
  ENDIF
  SHOW GET INC_TIT.INC_DOC
  SHOW GET INC_TIT.INC_DATE
  RETURN .T.
ENDCASE

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║              Ввод накладной в формате обмена с филиалом.               ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.05.97 16:10:22 ═╝
PROCEDURE Get_Inv1

PRIVATE pth, mss, g_0, c_0, l_0, mon_type
l_0 = ""
mon_type = " "

pth = Int_Dir("", "Где ищем накладную")
IF EMPTY(pth)  && Отказались...
  RETURN .F.
ENDIF

IF .NOT. File_O(pth+"SAL_S_T.DBF") .OR. .NOT. File_O(pth+"SAL_S.DBF")
  IF .NOT. File_O(pth+"RYN_M_T.DBF") .OR. .NOT. File_O(pth+"RYN_M.DBF")  ;
     .OR. .NOT. File_O(pth+"RYN_M.FPT")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Указанная Вами директория не содержит накладных!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ELSE
    SELECT 0
    USE (pth+"RYN_M_T.DBF")
    buy_ryn = 2
    SHOW GET buy_ryn
  ENDIF
ELSE
  SELECT 0
  USE (pth+"SAL_S_T.DBF") ALIAS RYN_M_T
  buy_ryn = 1
  SHOW GET buy_ryn
  mon_type = "$"
  IF FSIZE("CURRENCY") # 0
    mon_type = IIF("USD" $ UPPER(CURRENCY), "$", "Р")
  ENDIF
ENDIF
PRIVATE tmp_pr, tmp_key, i, prdc, prdn
i = 0
SCAN FOR SUP_CODE = code_w
  i = i+1
  DIMENSION tmp_pr(i), tmp_key(i,5)
  tmp_pr(i)  = STR(DOC_NUM,10)+" "+DTOC(DOC_DATE)
  tmp_key(i,1) = DOC_NUM
  tmp_key(i,2) = DOC_DATE
  IF FSIZE("GTD_NUM") # 0
    tmp_key(i,3) = GTD_NUM
  ELSE
    tmp_key(i,3) = SPACE(20)
  ENDIF
  IF FSIZE("COUNTRY") # 0
    tmp_key(i,4) = COUNTRY
  ELSE
    tmp_key(i,4) = SPACE(16)
  ENDIF
  IF FSIZE("GTD_LINE") # 0
    tmp_key(i,5) = GTD_LINE
  ELSE
    tmp_key(i,5) = ""
  ENDIF
ENDSCAN

USE
SELECT INC_TIT
IF i = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"В файле нет ни одной накладной от указанного поставщика!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

*IF i > 1
  i = Ch_Inv()       && Выбираем накладную из списка
*ENDIF

IF i = 0
  is_file = 1
  SHOW GET is_file
  RETURN .F.
ENDIF

SELECT TMP_INC
ZAP
SELECT 0
USE (base_path+"DEVELOP") ORDER TAG CODE ALIAS DEV_0607 AGAIN
SELECT 0
IF File_O(pth+"SAL_S.DBF") 
  USE (pth+"SAL_S.DBF") ALIAS RYN_M
ELSE
  USE (pth+"RYN_M.DBF")
ENDIF

*REPLACE INC_TIT.GTD_TYPE WITH IIF(EMPTY(tmp_key(i,3)), 3, 2),  ;
*        INC_TIT.GTD_NUM  WITH tmp_key(i,3),  ;
*        INC_TIT.COUNTRY  WITH tmp_key(i,4)
SCAN FOR DOC_NUM = tmp_key(i,1) .AND. DOC_DATE = tmp_key(i,2)
  prdc = PRO_CODE
  = SEEK(prdc, "DEV_0607")
  prdn = LEFT(DEV_0607.B_NAME,8)
  = SEEK(CODE, "STOCK")
  IF .NOT. FOUND("STOCK")   && Новая позиция - добавляем!
    SELECT STOCK
    APPEND BLANK
    REPLACE CODE      WITH RYN_M.CODE,    ;
            PRO_CODE  WITH RYN_M.PRO_CODE,;
            PREFIX    WITH RYN_M.PREFIX,  ;
            NAME      WITH RYN_M.NAME,    ;
            PRODUCER  WITH prdn,          ;
            PREFIX1   WITH SeekName(RYN_M.PREFIX),;
            NAME1     WITH SeekName(RYN_M.NAME),  ;
            PRODUCER1 WITH SeekName(prdn),        ;
            NUM_PART  WITH RYN_M.NUM_PART,;
            DIV_CODE  WITH RYN_M.DIV_CODE,;
            QNT_ROUND WITH 1,             ;
            DATE_ON   WITH RYN_M.DATE_ON
  ENDIF
  IF EMPTY(DATE_ON)
    REPLACE DATE_ON WITH DATE()
  ENDIF
  IF .NOT. EMPTY(STOCK.DATE_OFF)
    REPLACE STOCK.DATE_OFF WITH {}
  ENDIF
  g_0 = RYN_M.GTD_NUM
  c_0 = RYN_M.COUNTRY
  IF FSIZE("GTD_LINE", "RYN_M") # 0
    l_0 = RYN_M.GTD_LINE
  ENDIF
  SELECT TMP_INC
  APPEND BLANK
  REPLACE CODE      WITH STOCK.CODE,    ;
          PREFIX    WITH STOCK.PREFIX,  ;
          NAME      WITH STOCK.NAME,    ;
          PRODUCER  WITH STOCK.PRODUCER,;
          QNT_INV   WITH RYN_M.QNT,     ;
          LIST_NO   WITH STR(tit_rec,5),;
          GTD_NUM   WITH g_0,           ;
          GTD_LINE  WITH l_0,           ;
          COUNTRY   WITH c_0
  IF TYPE("RYN_M.PRICE") = "N"
    REPLACE PRICE_0 WITH RYN_M.PRICE, ;
            PRICE   WITH RYN_M.PRICE
  ENDIF
  SELECT RYN_M
ENDSCAN
USE
SELECT DEV_0607
USE

SELECT INC_TIT
REPLACE INC_DOC  WITH STR(tmp_key(i,1),10),  ;
        INC_DATE WITH tmp_key(i,2),          ;
        IS_PRICE WITH mon_type

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                Ввод накладной из файла в формате Sing.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 13.05.97 17:56:00 ═╝
PROCEDURE Get_Inv2
PRIVATE pth, mss, sel_sav, sw, tmp_pr, wd, n, n_d

pth = Int_File("", "DBF", "Где ищем накладную")
IF EMPTY(pth)  && Отказались...
  RETURN .F.
ENDIF
*
*   Проверяем соответствие структуры
*
sel_sav = SELECT()
SELECT 0
USE (pth) ALIAS SING
sw = .T.
IF FSIZE("STO_CODE") < 5 .OR. TYPE("STO_CODE") # "N"
  sw = .F.
ENDIF
IF FSIZE("PREFIX") # 4 .OR. TYPE("PREFIX") # "C"
  sw = .F.
ENDIF
IF FSIZE("NAME") # 25 .OR. TYPE("NAME") # "C"
  sw = .F.
ENDIF
IF FSIZE("PRODUCER") # 8 .OR. TYPE("PRODUCER") # "C"
  sw = .F.
ENDIF
IF FSIZE("QTY") < 3 .OR. TYPE("QTY") # "N"
  sw = .F.
ENDIF
IF FSIZE("BOXNO") < 3 .OR. .NOT. TYPE("BOXNO") $ "NC"
  sw = .F.
ENDIF
IF .NOT. sw
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Это не файл с накладными!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_sav)
  RETURN .F.
ENDIF

*
*      Формируем список накладных и выбираем одну из них...
*
wd = FSIZE("BOXNO")

SELECT BOXNO FROM (pth) INTO CURSOR TMP_C GROUP BY BOXNO
SELECT TMP_C
n = RECCOUNT()
IF n = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Файл не содержит ни одной накладной!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT SING
  USE
  SELECT (sel_sav)
  RETURN .F.
ENDIF

DIMENSION tmp_pr(n)
n = 1
SCAN
  tmp_pr(n) = IIF(TYPE("BOXNO") = "N", STR(BOXNO,wd), BOXNO)
  n = n+1
ENDSCAN
IF n > 1
  n = Ch_Inv()
  IF n = 0  && Отказались...
    USE
    SELECT SING
    USE
    SELECT (sel_sav)
    RETURN .F.
  ENDIF
ENDIF
n_d = STR(n, FSIZE("INC_DOC", "INC_TIT"))
n   = IIF(TYPE("BOXNO") = "N", VAL(ALLTRIM(tmp_pr(n))), tmp_pr(n))
n_d = IIF(TYPE("N") = "N", STR(n, 10), n)
USE

*
*  Формируем списки в заготовках прихода и новых позиций.
*
SELECT SING
SCAN FOR BOXNO = n
  sw = .F.                && Признак складской позиции
  IF STO_CODE # 0         && Ищем по коду
    sw = SEEK(STO_CODE, "STOCK")
  ENDIF
  IF .NOT. sw             && По коду не нашли, пробуем искать по имени...
    SELECT STOCK
    SET ORDER TO TAG NAME_P
    sw = SEEK(SING.NAME+SING.PREFIX+SING.PRODUCER)
    SET ORDER TO TAG CODE
  ENDIF
  IF sw         && Позиция найдена в номенклатуре!
    SELECT TMP_INC
    APPEND BLANK
    REPLACE CODE      WITH STOCK.CODE,    ;
            PREFIX    WITH STOCK.PREFIX,  ;
            NAME      WITH STOCK.NAME,    ;
            PRODUCER  WITH STOCK.PRODUCER,;
            QNT_INV   WITH SING.QTY,      ;
            LIST_NO   WITH STR(tit_rec,5)
  ELSE
    SELECT TMP_NEW
    APPEND BLANK
    REPLACE PREFIX    WITH SING.PREFIX,  ;
            NAME      WITH SING.NAME,    ;
            PRODUCER  WITH SING.PRODUCER,;
            QNT       WITH SING.QTY,     ;
            LIST_NO   WITH STR(tit_rec,5)
  ENDIF
  SELECT SING
ENDSCAN
USE

SELECT INC_TIT
REPLACE INC_DOC WITH n_d

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Выбор накладной из списка.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.05.97 16:29:04 ═╝
PROCEDURE Ch_Inv
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

PRIVATE point, n_h, n_m, ex
*------------------------------------------------------------------------

n_h = ALEN(tmp_pr)
n_m = MIN(n_h, WROWS("")-12)
point = 1
PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH n_m+3, LEN(tmp_pr(1))+4, "Док-ты"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

DO WHILE .T.
  point = MAX(point,1)
  point = MIN(point,n_h)
  @ 1, 1 MENU tmp_pr, n_h, n_m
  READ MENU TO point
  ex = READKEY() % 256
  IF ex = 15
    EXIT
  ENDIF
  IF ex = 12
    point = 0
    EXIT
  ENDIF
ENDDO

POP KEY
RELEASE WINDOW (win_name)
RETURN point
*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                    Очистка номера и даты накладной.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 25.09.97 10:34:04 ═╝
PROCEDURE Clr_Inv

REPLACE INC_TIT.INC_DOC WITH "", INC_TIT.INC_DATE WITH {}
SHOW GET INC_TIT.INC_DOC
SHOW GET INC_TIT.INC_DATE
tmp_D_Code = 0
*IF is_file # 1
*  buy_ryn = 0
*ELSE
*  buy_ryn = 1
*ENDIF
*SHOW GET buy_ryn
*DO Get_Inv0 WITH .T.

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Ввод накладной в формате АС.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.05.97 16:10:22 ═╝
PROCEDURE Get_Inv3

PRIVATE mss, sel_sav, g_0, c_0

IF .NOT. File_O(path_comm+"INVOICET.DBF") .OR. .NOT. File_O(path_comm+"INVOICED.CDX") .OR. ;
   .NOT. File_O(path_comm+"INVOICET.DBF") .OR. .NOT. File_O(path_comm+"INVOICED.CDX")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не удается найти файлы с накладными!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
sel_sav = SELECT()
SELECT 0
USE (path_comm+"INVOICET")
SET FILTER TO EMPTY(DATE_INC)

IF .NOT. Ch_Inv3()
  USE
  SELECT (sel_sav)
  RETURN .F.
ENDIF

REPLACE INC_TIT.INC_DOC  WITH INVOICET.EXP_NUM, ;
        INC_TIT.INC_DATE WITH INVOICET.EXP_DATE
IF FSIZE("GTD_NUM", "INVOICET") = 0
  REPLACE INC_TIT.GTD_TYPE WITH 3
ELSE
  REPLACE INC_TIT.GTD_TYPE WITH IIF(EMPTY(INVOICET.GTD_NUM),3, 2), ;
          INC_TIT.GTD_NUM  WITH INVOICET.GTD_NUM, ;
          INC_TIT.COUNTRY  WITH INVOICET.COUNTRY
ENDIF  

USE (path_comm+"INVOICED") ORDER TAG EXP_NUM
SELECT TMP_INC
ZAP
SELECT INVOICED
SEEK LEFT(DTOS(INC_TIT.INC_DATE),4)+INC_TIT.INC_DOC

g_0 = SPACE(20)
c_0 = SPACE(16)
IF INC_TIT.GTD_TYPE = 2
  g_0 = INC_TIT.GTD_NUM
  c_0 = INC_TIT.COUNTRY
ENDIF

SCAN REST WHILE EXP_NUM = INC_TIT.INC_DOC .AND. EXP_DATE = INC_TIT.INC_DATE
    
  IF INC_TIT.GTD_TYPE = 3 .AND. FSIZE("GTD_NUM") # 0
    g_0 = GTD_NUM
    c_0 = COUNTRY
  ENDIF
  sw = .F.                && Признак складской позиции
  IF STO_CODE # 0         && Ищем по коду
    sw = SEEK(STO_CODE, "STOCK")
  ENDIF
  IF .NOT. sw             && По коду не нашли, пробуем искать по имени...
    SELECT STOCK
    SET ORDER TO TAG NAME_P
    sw = SEEK(INVOICED.NAME+INVOICED.PREFIX+INVOICED.PRODUCER)
    SET ORDER TO TAG CODE
  ENDIF
  IF sw         && Позиция найдена в номенклатуре!
    SELECT TMP_INC
    APPEND BLANK
    REPLACE CODE      WITH STOCK.CODE,    ;
            IDP       WITH INVOICED.PURSYS+STR(INVOICED.IDP), ;
            PREFIX    WITH STOCK.PREFIX,  ;
            NAME      WITH STOCK.NAME,    ;
            PRODUCER  WITH STOCK.PRODUCER,;
            QNT_INV   WITH INVOICED.QTY,  ;
            LIST_NO   WITH STR(tit_rec,5),;
            GTD_NUM   WITH g_0,           ;
            COUNTRY   WITH c_0,           ;
            INV_REF   WITH RECNO("INVOICED")
  ELSE
    SELECT TMP_NEW
    APPEND BLANK
    REPLACE PREFIX    WITH INVOICED.PREFIX,  ;
            NAME      WITH INVOICED.NAME,    ;
            PRODUCER  WITH INVOICED.PRODUCER,;
            IDP       WITH INVOICED.PURSYS+STR(INVOICED.IDP), ;
            QNT       WITH INVOICED.QTY,     ;
            LIST_NO   WITH STR(tit_rec,5),   ;
            GTD_NUM   WITH g_0,              ;
            COUNTRY   WITH c_0,              ;
            INV_REF   WITH RECNO("INVOICED")
  ENDIF
  SELECT INVOICED

ENDSCAN
USE

SELECT (sel_sav)

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Inv3      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│               Выбор накладной третьего типа (формат АС).               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.11.1998 ──┘
PROCEDURE Ch_Inv3

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
DO Use_Dummy
SELECT INVOICET
GO BOTTOM

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd, ret_w
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY") = "ON", 21,  19)

DO D_Wins WITH ln, wd, "Список накладных", 0, 0
what_do = "List"

ret_w = .F.
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS EXP_NUM:H="", EXP_DATE:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Просмотр списка

    ret_w = .T.
    EXIT

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

RETURN ret_w

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_GTD      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Выбор ГТД.                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 14.02.2000 ──┘
PROCEDURE Get_GTD

PRIVATE tmpVal, point
IF sw_0001
  sw_0001 = .F.
  SHOW GET sw_0001
  tmpVal = Ch_GTD(INC_TIT.GTD_NUM)
  point = AT(CHR(13), tmpVal)
  IF point = 0
    REPLACE INC_TIT.GTD_NUM WITH tmpVal, INC_TIT.COUNTRY WITH ""
  ELSE
    REPLACE INC_TIT.GTD_NUM WITH LEFT(tmpVal, point-1), INC_TIT.COUNTRY WITH SUBSTR(tmpVal, point+1)
  ENDIF
  SHOW GET INC_TIT.GTD_NUM
  SHOW GET INC_TIT.COUNTRY
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Inv       Разработчик Андрей Васин                       │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Выбор/очистка накладной                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 23.11.2010 ──┘
PROCEDURE G_Inv

IF sw_doc
  tmp_D_Code = Get_Inv()
  IF tmp_D_Code = 0
    sw_doc = .F.
    SHOW GET sw_doc
  ELSE
    USE base_path+"CMP_DOCS" SHARED AGAIN ORDER TAG DOC_CODE IN 0
    IF .NOT. SEEK(tmp_D_Code, "CMP_DOCS")
      USE IN CMP_DOCS
      PRIVATE mss
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Накладня пуста!"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      sw_doc = .F.
      SHOW GET sw_doc
    ELSE
      m.prc_type = IIF(CMP_DOCS.PRICE_TYPE = "$", 3, 2)
      USE base_path+"CMP_INVS" SHARED AGAIN ORDER TAG DOC_CODE IN CMP_DOCS
      = SEEK(tmp_D_Code, "CMP_INVS")
      REPLACE INC_TIT.INC_DOC  WITH ALLTRIM(CMP_INVS.DOC_NUM), ;
              INC_TIT.INC_DATE WITH CMP_INVS.DOC_DATE,         ;
              INC_TIT.INC_CODE WITH CMP_INVS.DOC_CODE
      USE IN CMP_INVS
    ENDIF
  ENDIF
ELSE
  DO Clr_Inv
  REPLACE INC_TIT.INC_DOC  WITH "",       ;
          INC_TIT.INC_DATE WITH CTOD(""), ;
          INC_TIT.INC_CODE WITH 0,        ;
          INC_TIT.IS_PRICE WITH ""
ENDIF
SHOW GET INC_TIT.INC_DOC
SHOW GET INC_TIT.INC_DATE
SHOW GET prc_type
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Накладная КОМПЭЛа.                           ║
*║                                                                        ║
*╚═══════════════════════════════════════════════════════════ 23.11.2011 ═╝
PROCEDURE Get_InvN

PRIVATE pth, mss, g_0, c_0, l_0, mon_type, pr_code, s_sav

m.s_sav = SELECT()
SELECT TMP_INC
ZAP
SELECT 0
USE (base_path+"CMP_DOCS") ORDER TAG DOC_CODE ALIAS RYN_M AGAIN

SEEK INC_TIT.INC_CODE
SCAN REST WHILE DOC_CODE = INC_TIT.INC_CODE
  
  IF .NOT. SEEK(CODE, "STOCK")   
    LOOP
  ENDIF
  IF .NOT. EMPTY(STOCK.DATE_OFF)
    REPLACE STOCK.DATE_OFF WITH {}
  ENDIF
  SELECT TMP_INC
  APPEND BLANK
  REPLACE CODE      WITH STOCK.CODE,      ;
          PREFIX    WITH STOCK.PREFIX,    ;
          NAME      WITH STOCK.NAME,      ;
          PRODUCER  WITH STOCK.PRODUCER,  ;
          QNT_INV   WITH RYN_M.QNT,       ;
          LIST_NO   WITH STR(tit_rec,5),  ;
          GTD_NUM   WITH RYN_M.GTD_NUM,   ;
          GTD_LINE  WITH RYN_M.GTD_LINE,  ;
          COUNTRY   WITH RYN_M.COUNTRY,   ;
          COUNTRYC  WITH RYN_M.COUNTRYC,  ;
          PRICE_0   WITH RYN_M.PRICE,     ;
          PRICE   WITH RYN_M.PRICE
  SELECT RYN_M
ENDSCAN
USE

SELECT (m.s_sav)

RETURN .T.
