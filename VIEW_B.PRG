*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл View_B       Разработчик Андрей Васин           21.03.97 12:56:16 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║               Просмотр, печать и создание счетов-фактур.               ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE View_B
PARAMETERS cnt_name
*
*  Переменные состояния для навигации
*
PRIVATE is_c

is_c = TYPE("cnt_name")  = "C"

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
DEFINE MENU (menu_name) IN SCREEN && COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Печатать"
ON PAD Print OF (menu_name) ACTIVATE POPUP Print

  DEFINE POPUP Print MARGIN RELATIVE SHADOW && COLOR SCHEME 4

  DEFINE BAR  1 OF Print PROMPT "Одного документа" KEY Ctrl-A, "Enter"
  ON SELECTION BAR 1 OF Print DO Sw_Mode WITH "Print1"

  DEFINE BAR  2 OF Print PROMPT "Всех документов" KEY Ctrl-A, "Ctrl+Enter"
  ON SELECTION BAR 2 OF Print DO Sw_Mode WITH "PrintA"

  IF sys_char = "A" .OR. sale_mode = "Mitin"
    DEFINE BAR  3 OF Print PROMPT "Книги продаж" KEY Ctrl-A, "Ctrl-P"
    ON SELECTION BAR 3 OF Print DO Sw_Mode WITH "PrintB"
  ENDIF

IF is_c
  DEFINE PAD Create OF (menu_name) PROMPT "Создать" KEY Ctrl-A, "Ins"
  ON SELECTION PAD Create OF (menu_name) Do Sw_Mode WITH "Create"
ENDIF

DEFINE PAD Filter OF (menu_name) PROMPT "Фильтр" KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) Do Sw_Mode WITH "Filter"

DEFINE PAD Modify OF (menu_name) PROMPT "Коррекция" KEY Ctrl-A, "Tab"
ON SELECTION PAD Modify OF (menu_name) Do Sw_Mode WITH "Modify"
IF sys_char = "A" .OR. sale_mode = "Mitin"
  DEFINE PAD Goods OF (menu_name) PROMPT "Товары" KEY Ctrl-A, "F2"
  ON SELECTION PAD Goods OF (menu_name) Do Sw_Mode WITH "Goods"
  DEFINE PAD Receive OF (menu_name) PROMPT "Импорт" KEY Ctrl-A, "F3"
  ON SELECTION PAD Receive OF (menu_name) Do Sw_Mode WITH "Receive"
ELSE
  DEFINE PAD Send OF (menu_name) PROMPT "Экспорт" KEY Ctrl-A, "F3"
  ON SELECTION PAD Send OF (menu_name) Do Sw_Mode WITH "Send"
ENDIF

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

PRIVATE p_line, p_numb
DO Use_Dummy
SELECT 0
DELETE FILE (tmpo_path+"TMP_LINK.DBF")
CREATE DBF (tmpo_path+"TMP_LINK")  ;
  ( ORD   C(18), ;
    RC_NO N(10) )
SELECT 0
USE (base_path+"PLACES")
p_numb = RECCOUNT()
p_line = LEFT("ABCDEFGHIJKLMNOPQRSTUVWXYZ", p_numb)
USE (base_path+"BILL_F")

PRIVATE ln, wd, rc_s

&& Значения фильтра
PRIVATE is_hr, c_chab, g_shab, dt1, dt2, nds_0, nds_1

PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
f_c000 = def_firm
f_n000 = SPACE(16)
f_c_old = def_firm
is_hr  = .F.
g_shab = SPACE(20)
c_shab = SPACE(20)
nds_0 = 0
nds_1 = 100
dt1 = {}
dt2 = DATE()
IF .NOT. Bill_Flt()
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"TMP_LINK.DBF")
  DELETE FILE (tmpo_path+"TMP_LINK.CDX")
  RETURN
ENDIF

ln = WROWS()-12
PRIVATE ln, wd
ln = 100      && Количество видимых строк BROWSE
wd = 68

DO D_Wins WITH ln, wd, "Список счетов-фактур", 0, 1
@ 1, 2 SAY "───N док-та─────Дата────Клиент───────────────────────────────Сумма──"
*┌───N док-та─────Дата────Клиент───────────────────────────────Сумма──┐
*│? NNNNNNNNNN ДД.ММ.ГГГГ ....:....!....:....!....:...> 999 999 999.99│
*└────────────────────────────────────────────────────────────────────┘
*┌────────────────────────────────────────────────────────────────────┐
*│     Накладная NNNNNNNNNNN      Товар ТТТТТТТТТТТТТТТТ              │
*└────────────────────────────────────────────────────────────────────┘
what_do = "list"

DO WHILE .T.
  
  SELECT TMP_LINK
  DO CASE

  CASE what_do = "list"    && Просмотр списка
    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter Do Sw_Mode WITH "PrintA"
    ON KEY LABEL F6    Do Sw_Mode WITH "Filter"
    IF is_c
      ON KEY LABEL Ins   Do Sw_Mode WITH "Create"
    ENDIF
    ON KEY LABEL Tab    Do Sw_Mode WITH "Modify"
    IF sys_char = "A" .OR. sale_mode = "Mitin"
      ON KEY LABEL F2     Do Sw_Mode WITH "Goods"
      ON KEY LABEL F3     Do Sw_Mode WITH "Receive"
      ON KEY LABEL Ctrl+P DO Sw_Mode WITH "PrintB"
    ELSE
      ON KEY LABEL F3    Do Sw_Mode WITH "Send"
    ENDIF
*
*    BROWSE - меню
*
    BROWSE FIELDS D000 = IIF(EMPTY(BILL_F.HOLD_DATE), " ", "√"):H="",  ;
               D001 = LEFT(BILL_F.DOC_NUM,1)+ALLTRIM(RIGHT(BILL_F.DOC_NUM,9)):10:H="", ;
               BILL_F.DOC_DATE:H="",     ;
               D002 = IIF(EMPTY(SUBSTR(BILL_F.CUS_NAME,29)), LEFT(BILL_F.CUS_NAME,28),  ;
                                         LEFT(BILL_F.CUS_NAME,28)+""):29:H="",  ;
               BILL_F.TOTAL_R:H="":P="@Z 999 999 999.99", ;
               DUMMY.F:H="" FREEZE DUMMY.F   ;
           WHEN P_Foot()                     ;
           NOAPPEND NODELETE NOMODIFY REST NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "Print1")
    ENDIF
  CASE what_do = "Filter"    && Установка фильтра
    DO Bill_Flt
    what_do = "list"
  CASE what_do = "Create"    && Ввод счета-фактуры
    DO BillFree WITH cnt_name
    what_do = "list"
  CASE what_do = "Print1"    && Печать счета-фактуры
    DO P_BF_Fre WITH BILL_F.FIRM, BILL_F.DOC_NUM, BILL_F.DOC_DATE, BILL_F.DOC_TYPE
    what_do = "list"
  CASE what_do = "PrintB"    && Печать книги продаж
    DO P_Book_S
    what_do = "list"
  CASE what_do = "Send"      && Передача данных
    DO Send_B_F
    what_do = "list"
  CASE what_do = "Modify"    && Корекция счета-фактуры
    DO BillFree WITH "Modify"
    what_do = "list"
  CASE what_do = "Receive"   && Прием данных
    DO Rec_B_F
    what_do = "list"
  CASE what_do = "Goods"     && Перечень товаров
    DO Goods
    what_do = "list"
  CASE what_do = "PrintA"    && Печать всех счетов-фактур
    PRIVATE mss, sw000, ans
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Распечатали очередной документ, и что дальше?"
    mss(3) = CHR(0)+"   [ Печатаем следующий! ][ Печатаем без глупых вопросов! ][ Все, хватит! ]"
    mss(4) = ""
    rc_s = RECNO()
    sw000 = .T.
    GO TOP
    SCAN
      DO P_BF_Fre WITH BILL_F.FIRM, BILL_F.DOC_NUM, BILL_F.DOC_DATE, BILL_F.DOC_TYPE
      IF sw000
        ans = Out_Mess(5, "mss")
        IF ans = 3
          EXIT
        ENDIF
        IF ans = 2
          sw000 = .F.
        ENDIF
      ENDIF
    ENDSCAN
    IF rc_s > 0 .AND. rc_s <= RECCOUNT()
      GO rc_s
    ENDIF
    what_do = "list"
  OTHERWISE
    EXIT
  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED
DELETE FILE (tmpo_path+"TMP_LINK.DBF")

RETURN

*╔══════════════════════════════════════════════════════════════════╗
*║                                                                  ║
*║               Печать номера накладной и типа товара.             ║
*║                                                                  ║
*╚══════════════════════════════════════════════════════════════════╝
PROCEDURE P_Foot

ACTIVATE WINDOW (win_name+"_M") BOTTOM
@ WROWS()-2, 3 SAY "Накладная "+BILL_F.INV_NUM+SPACE(10)+"Товар "+BILL_F.GOODS
ACTIVATE WINDOW(ALIAS())

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Прием счетов-фактур.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 25.03.97 16:55:23 ═╝
PROCEDURE Rec_B_F

PRIVATE t_rec, dt0, pth_w, ex, mss, strange

t_rec = Where_From()
strange = ""
*  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
*
*     Здесь не плохо бы уточнить список площадок.
*

DO CASE
CASE t_rec = 1
  dt0 = {}
  ex = 1
  DO G_Dat.spr
  IF ex = 1
    DO Rc_001 WITH dt0
    DO Prp_Tmp
  ENDIF
CASE t_rec = 2
  pth_w = "\"
  DO WHILE .T.
    pth_w = Int_Dir(pth_w, "Где ищем документы?", 0)
    IF .NOT. EMPTY(pth_w)
      IF .NOT. File_O(pth_w+"BILLS_T.DBF")
        DIMENSION mss(4)
        mss(1) = ""
        mss(2) = CHR(0)+"Ищу, но... не нахожу там документов. Что будем делать?"
        mss(3) = CHR(0)+" [ Поищем в другом месте ] [ Ну их, эти документы! ]"
        mss(4) = ""
        IF Out_Mess(7, "mss") = 1
          LOOP
        ENDIF
        pth_w = ""
      ENDIF
    ENDIF
    EXIT
  ENDDO
  IF .NOT. EMPTY(pth_w)
    DO Rc_002 WITH pth_w
    DO Prp_Tmp
  ENDIF
ENDCASE

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Источник документов.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 25.03.97 17:01:17 ═╝
PROCEDURE Where_From

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

PRIVATE ex, p, arr_w
                     &&   Объявляем и заполняем поля бланка
ex = 1
p  = 1
DIMENSION arr_w(2)
arr_w(1) = "Продажи             "
arr_w(2) = "Удаленная площадка  "
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 5, 24, "Укажите источник"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO WHILE .T.
  p = MAX(p, 1)
  p = MIN(p, 2)
  @ 1, 1 MENU arr_w, 2
  READ MENU TO p
  ex = READKEY() % 256
  IF ex = 12
    p = 0
    EXIT
  ENDIF
  IF ex = 15
    EXIT
  ENDIF
ENDDO

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN p

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Прием документов с удаленной площадки.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 25.03.97 18:32:38 ═╝
PROCEDURE Rc_002
PARAMETER pth

PRIVATE rc_sav, cnt_h, cnt_r, reason, no_skip, rec_im, win_name, mnt_s
PRIVATE i01, i02, i03, i, n, f_exist, fc_0, fc_1, fc_w

win_name = PROGRAM()
fc_0 = "   "
fc_1 = "   "
*
*┌─────────────────────────┐
*│    Документ N 999999    │
*│    Пропускаем: чужой    │
*│                повтор   │
*│          Пишем          │
*└─────────────────────────┘
cnt_h = 0
cnt_r = 0
SELECT BILL_F
rc_sav = RECNO()
SET ORDER TO TAG DOC_NUM

SELECT 0
USE (pth+"BILLS_T.DBF") ALIAS SOURCE
f_exist = FSIZE("FIRM") # 0
IF .NOT. f_exist
  IF .NOT. Fill_Firm(fc_0, fc_1)
    USE
    SELECT BILL_F
    SET ORDER TO
    SELECT TMP_LINK
    RETURN
  ENDIF
ENDIF
STORE 0 TO i01, i02, i03
n = AFIELDS(mnt_s)
FOR i = 1 TO n
  IF mnt_s(i,1) == "TOTAL_0"
    i01 = i
  ENDIF
  IF mnt_s(i,1) == "TOTAL_NDS"
    i02 = i
  ENDIF
  IF mnt_s(i,1) == "TOTAL_R"
    i03 = i
  ENDIF
ENDFOR
RELEASE nmt_s
mnt_s = Mnt_Size("TOTAL_R")
mnt_s = IIF(mnt_s = 0, 1000, 1)
SET FILTER TO .NOT. EMPTY(DOC_NUM)
DO D_Win_N WITH 5, 29, "Прием документов"
SCAN
  cnt_h = cnt_h+1
  no_skip = .T.
  reason = "Пишем!"
  IF f_exist
    fc_w = FIRM
  ELSE
    fc_w = IIF(DOC_TYPE = "0", fc_0, fc_1)
  ENDIF
  IF LEFT(DOC_NUM,1) $ strange    && Пропускаем чужой документ
    no_skip = .F.
    reason  = "Пропускаем: чужой!"
  ELSE  
    IF SEEK(fc_w+DOC_TYPE+LEFT(DTOS(DOC_DATE),4)+DOC_NUM, "BILL_F")
      no_skip = .F.
      reason  = "Пропускаем: повтор"
    ENDIF
  ENDIF
  @ 2, 6 SAY "Документ N "+LEFT(DOC_NUM,1)+ALLTRIM(SUBSTR(DOC_NUM,2))
  @ 3, 2 SAY PADC(reason, WCOLS()-4)
  IF no_skip
    cnt_r = cnt_r+1
    SCATTER TO rec_im
    IF .NOT. f_exist
      DIMENSION rec_im(ALEN(rec_im)+1)
      = AINS(rec_im,1)
      rec_im(1) = fc_w
    ENDIF
    IF mnt_s # 1
      IF i03 # 0
        rec_im(i03) = ROUND(rec_im(i03)/mnt_s,2)
      ENDIF
      IF i02 # 0
        rec_im(i02) = ROUND(rec_im(i02)/mnt_s,2)
      ENDIF
      IF i01 # 0
        rec_im(i01) = rec_im(i03)-rec_im(i02)
      ENDIF
    ENDIF
    SELECT BILL_F
    APPEND FROM ARRAY rec_im
    REPLACE HOLD_DATE WITH {}
    SELECT SOURCE
  ENDIF
ENDSCAN

USE

SELECT BILL_F
SET ORDER TO TAG HRON
IF cnt_r # 0
  IF rc_sav > 0 .AND. rc_sav <= RECCOUNT()
    GO rc_sav
  ENDIF
ENDIF

reason = "Всего "+ALLTRIM(STR(cnt_h))+", зап. "+ALLTRIM(STR(cnt_r))
@ 2, 2 SAY PADC(reason, WCOLS()-4)
@ 3, 2 SAY PADC("Нажмите что-нибудь...", WCOLS()-4)
= INKEY(0,"MH")

RELEASE WINDOW (win_name)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║               Выборка счетов-фактур из файла накладных.                ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 26.03.97 16:14:57 ═╝
PROCEDURE Rc_001
PARAMETER dt

PRIVATE rc_sav, cnt_h, cnt_r, reason, no_skip, win_name, n, c_nam
PRIVATE p1, p2, p3, p4, p5, p6, p7, p8, p9
PRIVATE p_num, p_type, p_doc, p_dat

win_name = PROGRAM()

*
*┌─────────────────────────┐
*│    Документ N 999999    │
*│    Пропускаем: чужой    │
*│                повтор   │
*│          Пишем          │
*└─────────────────────────┘
cnt_h = 0
cnt_r = 0
rc_sav = RECNO()
SELECT BILL_F
SET ORDER TO TAG DOC_NUM

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"CUS_BIL") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM ALIAS MON_1226

SELECT 0
USE (base_path+"SALE_TIT") ALIAS SOURCE ORDER TAG HRON
SET FILTER TO .NOT. EMPTY(NEW_DOC)
SET RELATION TO FIRM+PAY_TYPE+LEFT(DTOS(PAY_DATE),4)+(PAY_DOC) INTO MON_1226

IF .NOT. SEEK(DTOS(dt))
  n = RECNO(0)
  IF n > 0 .AND. n <= RECCOUNT()
    GOTO n
  ELSE
    GO BOTTOM
  ENDIF
ENDIF

DO D_Win_N WITH 5, 29, "Выборка документов"
SCAN REST
  cnt_h = cnt_h+1
  no_skip = .T.
  reason = "Пишем!"
  IF LEFT(NEW_DOC,1) $ strange    && Пропускаем чужой документ
    no_skip = .F.
    reason  = "Пропускаем: чужой!"
  ELSE  
    IF SEEK(FIRM+NEW_TYPE+LEFT(DTOS(DOC_DATE),4)+NEW_DOC, "BILL_F")
      no_skip = .F.
      reason  = "Пропускаем: повтор"
    ENDIF
  ENDIF
  @ 2, 6 SAY "Документ N "+LEFT(DOC_NUM,1)+ALLTRIM(SUBSTR(DOC_NUM,2))
  @ 3, 2 SAY PADC(reason, WCOLS()-4)
  IF no_skip
    STORE "" TO p1, p2, p3, p4, p5, p6, p7, p8, p9
    c_nam = CUS_NAME
    IF CUS_CODE # 0
      IF SEEK(CUS_CODE, "ACCOUNT")
        c_nam = ACCOUNT.CUS_NAME
      ENDIF
      IF SEEK(CUS_CODE, "CUS_BIL")
        c_nam = CUS_BIL.CUS_NAME
        p1 = CUS_BIL.ADDRESS
        p2 = CUS_BIL.PHONE
        p3 = CUS_BIL.ACC_BILL
        p4 = CUS_BIL.BIK
        p5 = CUS_BIL.BANK
        p6 = CUS_BIL.CITY
        p7 = CUS_BIL.INN
        p8 = CUS_BIL.OKOHX
        p9 = CUS_BIL.OKPO
      ENDIF
    ENDIF
*-----------------------------------------
    p_num = "" 
    p_type = ""
    p_doc = {}
    p_dat = {}
    IF .NOT. EMPTY(SOURCE.PAY_DOC)
      IF FOUND ("MON_1226")
        IF .NOT. EMPTY(MON_1226.DOC_NUM)
          p_num = MON_1226.DOC_NUM
          p_type = MON_1226.DOC_TYPE
          p_doc = MON_1226.DOC_DATE
          p_dat = IIF(EMPTY(MON_1226.ENTER_DAT),MON_1226.DOC_DATE,MON_1226.ENTER_DAT)
        ENDIF
      ENDIF
    ENDIF
*-----------------------------------------    
    cnt_r = cnt_r+1
    SELECT BILL_F
    APPEND BLANK
    REPLACE SUB_SYS   WITH LEFT(SOURCE.DOC_NUM,1),;
            FIRM      WITH SOURCE.FIRM,           ;
            DOC_TYPE  WITH SOURCE.NEW_TYPE,       ;
            DOC_NUM   WITH SOURCE.NEW_DOC,        ;
            DOC_DATE  WITH SOURCE.DOC_DATE,       ;
            CUS_CODE  WITH SOURCE.CUS_CODE,       ;
            CUS_NAME  WITH c_nam,                 ;
            ADDRESS   WITH p1,                    ;
            PHONE     WITH p2,                    ;
            ACC_BILL  WITH p3,                    ;
            BIK       WITH p4,                    ;
            BANK      WITH p5,                    ;
            CITY      WITH p6,                    ;
            INN       WITH p7,                    ;
            OKOHX     WITH p8,                    ;
            OKPO      WITH p9,                    ;
            INV_NUM   WITH LEFT(SOURCE.DOC_NUM,4) ;
                           +"-"+ALLTRIM(SUBSTR(SOURCE.DOC_NUM,5)), ;
            GOODS_COD   WITH 0,                     ;
            GOODS       WITH "Эл. компоненты",      ;
            PAY_DOC     WITH p_num,                 ;
            PAY_TYPE    WITH p_type,                ;
            PAY_DATE    WITH p_doc,                 ;
            ENTER_DATE  WITH p_dat,                 ;
            TOTAL_R     WITH SOURCE.TOTAL_R,        ;
            NDS_        WITH SOURCE.NDS_,           ;
            TOTAL_NDS   WITH SOURCE.TOTAL_R-SOURCE.TOTAL_0,;
            TOTAL_0     WITH SOURCE.TOTAL_0,        ;
            WHO         WITH SOURCE.WHO,            ;
            WHERE       WITH SOURCE.WHERE
    SELECT SOURCE
  ENDIF
ENDSCAN

USE

SELECT ACCOUNT
USE

SELECT MON_1226
USE

SELECT CUS_BIL
USE

SELECT BILL_F
SET ORDER TO

SELECT TMP_LINK

reason = "Всего "+ALLTRIM(STR(cnt_h))+", зап. "+ALLTRIM(STR(cnt_r))
@ 2, 2 SAY PADC(reason, WCOLS()-4)
@ 3, 2 SAY PADC("Нажмите что-нибудь...", WCOLS()-4)
= INKEY(0,"MH")

RELEASE WINDOW (win_name)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Передача счетов-фактур в бухгалтерию.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 26.03.97 17:08:41 ═╝
PROCEDURE Send_B_F

PRIVATE n_hole, n_old, n_ovl, win_name, l_send, p_cnt, pth
PRIVATE w_arr, sz

win_name = PROGRAM()
n_ovl = 4
n_hole = 0
n_old  = 0
*
*┌─────────────────────────┐
*│                         │
*│       Передаем...       │
*│ Всего NNNN, новых NNNN  │
*│                         │
*└─────────────────────────┘
p_cnt = 0
rc_sav = RECNO()

DO D_Win_N WITH 5, 29, "Передача документов"

@ 2, 2 SAY PADC("Передаем...", WCOLS()-4)

SELECT BILL_F
GO BOTTOM
l_send = {}
DO WHILE .NOT. BOF()
  IF l_send # SEND_DATE
    IF p_cnt >= n_ovl       && Набрали требуемое число перекрытий.
      SKIP
      EXIT
    ENDIF
    l_send = SEND_DATE       && Запоминаем новое текущее значение.
  ENDIF
  IF EMPTY(SEND_DATE)
    REPLACE SEND_DATE WITH DATE()
  ELSE
    n_old = n_old+1
  ENDIF
  n_hole = n_hole+1
  SKIP -1
ENDDO

IF BOF()
  GO TOP
ENDIF

DELETE FILE (tmpo_path+"TMP_BILL.DBF")
COPY REST TO (tmpo_path+"TMP_BILL.DBF")
= ADIR(w_arr, tmpo_path+"TMP_BILL.DBF")

pth = "\"
pth = Int_Dir(pth, "Куда поместим данные?", w_arr(1, 2))

IF .NOT. EMPTY(pth)
  COPY FILE (tmpo_path+"TMP_BILL.DBF") TO (pth+"BILLS_T.DBF")
  @ 2, 2 SAY PADC("Всего "+ALLTRIM(STR(n_hole))+", новых "+   ;
                  ALLTRIM(STR(n_hole-n_old)), WCOLS()-4)
  @ 2, 2 SAY PADC("Нажмите что-нибудь...", WCOLS()-4)
  = INKEY(0,"HM")
ENDIF

RELEASE WINDOW (win_name)
IF rc_sav > 0 .AND. rc_sav <= RECCOUNT()
  GO rc_sav
ENDIF

SELECT TMP_LINK
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Fill_Firm    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Установка фирм для счетов-фактур.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04/07/1999 ──┘
PROCEDURE Fill_Firm
PARAMETERS f0, f1

PRIVATE s_sav, fn0, fn1
fn0 = SPACE(16)
fn1 = SPACE(16)

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

*┌───────────────────────────────────────────────────┐
*│ < Предприятие для счетов-фактур нулевого типа   > │
*│           Код 999 Название 9999999999999999       │
*│                                                   │
*│ < Предприятие для счетов-фактур ненулевого типа > │
*│           Код 999 Название 9999999999999999       │
*│                                                   │
*│                  < OK Ctrl-W >                    │
*└───────────────────────────────────────────────────┘

PRIVATE ex, sw0, sw1
ex  = 1
sw0 = 1
sw1 = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 55, "Выбор предприятий"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 3 GET sw0 PICTURE "@*N \ Предприятие для счетов-фактур нулевого типа   " ;
     VALID Ch_F(0)
@ 3, 13 SAY "Код" GET f0 WHEN .F.
@ 3, 21 SAY "Название" GET fn0 WHEN .F.

@ 5, 3 GET sw1 PICTURE "@*N \ Предприятие для счетов-фактур ненулевого типа " ;
     VALID Ch_F(1)
@ 6, 13 SAY "Код" GET f1 WHEN .F.
@ 6, 21 SAY "Название" GET fn1 WHEN .F.

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_F         Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Выбор предприятия.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04/07/1999 ──┘
PROCEDURE Ch_F
PARAMETERS mode

PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2, w_sav
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
f_n000 = IIF(mode = 0, fn0, fn1)
f_c000 = IIF(mode = 0, f0, f1)
f_c_old = f_c000
w_sav = WOUTPUT()

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
*│ Предприятие 999 (F1) 9999999999999999 │
*│                                       │
*│     < OK Ctrl-W > < Отказаться Esc >  │
*└───────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 43, "Выбор предприятия"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO Get_Firm WITH 2, 3, .T.

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF ex = 1
  *
  * Отрабатываем бланк
  IF mode = 0
    f0  = f_c000
    fn0 = f_n000
    SHOW GET f0
    SHOW GET fn0
  ELSE
    f1  = f_c000
    fn1 = f_n000
    SHOW GET f1
    SHOW GET fn1
  ENDIF
ENDIF
*--------------------------------------------------------------------------

ACTIVATE WINDOW (w_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Blank    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Выход из бланка.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.04.1999 ──┘
PROCEDURE Tst_Blank

PRIVATE mss
IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(f0)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали предприятие для счетов-фактур нулевого типа."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(f1)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали предприятие для счетов-фактур ненулевого типа."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_Tmp      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временного файла.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 15.04.1999 ──┘
PROCEDURE Prp_Tmp
PRIVATE shb, shb_c

SELECT TMP_LINK
SET RELATION TO
USE
shb = SYS(15, lwr, ALLTRIM(g_shab))
shb_c = SYS(15, lwr, ALLTRIM(c_shab))
DO Wt_Mess WITH "Подборка..."
*SELECT BILL_F.DOC_NUM+DTOS(BILL_F.DOC_DATE) AS ORD, ;
*       RECNO("BILL_F") AS RC_NO ;
*       FROM BILL_F INTO DBF TMP_LNK ;
*       WHERE IIF(is_hr, BETWEEN(BILL_F.DOC_DATE, dt1, dt2), .T.) AND ;
*             BILL_F.FIRM = f_c000 AND ;
*             (shb $ SYS(15, lwr, GOODS) .OR. shb == "")  AND ;
*             (shb_c $ SYS(15, lwr, CUS_NAME) .OR. shb_c == "") AND ;
*             LEFT(DOC_NUM,1) $ p_line  ;
*       ORDER BY ORD
SELECT DTOS(BILL_F.DOC_DATE)+BILL_F.DOC_NUM AS ORD, ;
       RECNO() AS RC_NO ;
       FROM BILL_F INTO DBF (tmpo_path+"TMP_LINK") ;
       WHERE IIF(is_hr, BETWEEN(BILL_F.DOC_DATE, dt1, dt2), .T.) AND ;
             BILL_F.FIRM = f_c000 AND ;
             BETWEEN(BILL_F.NDS_, nds_0, nds_1) AND ;
             (shb $ SYS(15, lwr, GOODS) .OR. shb == "")  AND ;
             (shb_c $ SYS(15, lwr, CUS_NAME) .OR. shb_c == "") AND ;
             LEFT(DOC_NUM,1) $ p_line  ;
       ORDER BY ORD

USE (tmpo_path+"TMP_LINK")
SET RELATION TO RC_NO INTO BILL_F
GO BOTTOM
DO Wt_Mess
RETURN
