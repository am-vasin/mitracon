*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Corr_Nal     Разработчик Андрей Васин           02.09.97 17:04:20 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Программа коррекции наличия.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Corr_Nal

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
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Corr OF (menu_name) PROMPT "Корр." KEY Ctrl-A, "Enter"
ON SELECTION PAD Corr OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Add OF (menu_name) PROMPT "Доб." KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "Удал." KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exp OF (menu_name) PROMPT "Сформ. док-т" KEY Ctrl-A, "Tab"
ON SELECTION PAD Exp OF (menu_name) DO Sw_Mode WITH "Exp"

*DEFINE PAD <Pad name> OF (menu_name) PROMPT "<Prompt>"
*ON PAD <Pad name> OF (menu_name) ACTIVATE POPUP <Popup name>

*  DEFINE POPUP <Popup name> MARGIN RELATIVE SHADOW COLOR SCHEME 4

*  DEFINE BAR  1 OF <Popup name> PROMPT <Prompt> KEY Ctrl-A, "<Hot key>"
*  ON SELECTION BAR 1 OF <Popup name> DO Sw_Mode WITH "<Mode name>"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
PRIVATE reas_n, reas_c, r_cnt, let_pr
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
GO TOP
r_cnt = 0
let_pr = .T.
SCAN WHILE CUS_CODE < 0
  r_cnt = r_cnt+1
  DIMENSION reas_n(r_cnt), reas_c(r_cnt)
  reas_n(r_cnt) = LEFT(CUS_NAME,24)
  reas_c(r_cnt) = CUS_CODE
ENDSCAN

DELETE FILE (tmpo_path+"CORR_TMP.DBF")
DELETE FILE (tmpo_path+"CORR_TMP.CDX")
CREATE DBF (tmpo_path+"CORR_TMP.DBF")  ;
   ( CODE     N( 7), ;
     PREFIX   C( 4), ;
     NAME     C(35), ;
     PRODUCER C( 8), ;
     QNT_I    N( 6), ;
     QNT_E    N( 6), ;
     GTD_NUM  C(26), ;
     COUNTRY  C(16), ;
     COUNTRYC C( 3), ;
     GTD_LINE C( 4), ;
     PRICE    N( 8,2)  )
USE (tmpo_path+"CORR_TMP.DBF") EXCLU

INDEX ON CODE TAG CODE
INDEX ON NAME+PREFIX TAG NAME FOR CODE # 0

DO Use_Dummy

SELECT 0
USE (base_path+"STOCK")

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (base_path+"CORR_LST") ORDER TAG LST_NUM

SELECT 0
USE (base_path+"CORR_TIT") ORDER TAG NOT_EMP
SET RELATION TO REAS_CODE INTO ACCOUNT, ;
                WHO       INTO PERSONS, ;
                WHERE     INTO ARM_LST

SEEK "*"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = 12   && Количество видимых строк BROWSE
wd = 30+29

DO D_Wins WITH ln, wd, "Заготовки на коррекцию", 0, 1

@ 1, 2 SAY "Фрм"
@ 1, 7 SAY " Тип коррекции "
@ 1,31 SAY " Примечание "
@ WROWS()-3, 3 SAY " Раб. место "
@ WROWS()-3, 22 SAY " Кто создал "

what_do = IIF(BOF() .OR. EOF(), "Add", "List")

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ON KEY LABEL Ins DO Sw_Mode WITH "Add"
    ON KEY LABEL Del DO Sw_Mode WITH "Del"
    ON KEY LABEL Tab DO Sw_Mode WITH "Exp"
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE KEY "*" ; 
           FIELDS FIRM:H="", ;
                  F000 = LEFT(ACCOUNT.CUS_NAME,24):H="",  ;
                  NOTE:30:H="",                 ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Sh_Foot()  ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Modify"    && Коррекция списка

    DO Corr_Lst
    what_do = "List"

  CASE what_do = "Add"       && Добавление списка

    DO Add_List
    what_do = IIF(BOF() .OR. EOF(), "Exit", "List")

  CASE what_do = "Del"       && Удаление списка

    DO Del_List
    what_do = IIF(BOF() .OR. EOF(), "Add", "List")

  CASE what_do = "Exp"       && Отработать приход/расход

    DO Exp_List
    what_do = IIF(BOF() .OR. EOF(), "Add", "List")

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Формирование окна доп. полей.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 03.09.97 10:45:23 ═╝
PROCEDURE Sh_Foot
PRIVATE nn

ACTIVATE WINDOW (win_name+"_M") SAME
nn = IIF(FOUND("PERSONS"), PERSONS.FAMILY, PADR("Сам!",FSIZE("FAMILY","PERSONS")))
@ WROWS()-2, 3 SAY ARM_LST.ARM_N COLOR (SCHEME(1,1))
@ WROWS()-2, COL()+3 SAY nn COLOR (SCHEME(1,1))
ACTIVATE WINDOW (ALIAS()) SAME
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Добавление списка.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 02.09.97 17:26:40 ═╝
PROCEDURE Add_List

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

*
*┌─────────────────────────────────────┐
*│Предприятие 999 9999999999999999 (F1)│
*│  ┌─[ ]  Причина коррекции ───────┐  │
*│  │   ....:....!....:....!....    │  │
*│  └───────────────────────────────┘  │
*│  ┌─ Примечание ──────────────────┐  │
*│  │....:....!....:....!....:....!.│  │
*│  └───────────────────────────────┘  │
*│  < OK Ctrl-W > < Отказаться Esc >   │
*└─────────────────────────────────────┘

PRIVATE ex, nt, arr_w, c_f, c_n, c_flg
PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
f_c000 = def_firm
f_n000 = ""
f_c_old= def_firm

ex = 1
nt = SPACE(FSIZE("NOTE", "CORR_TIT"))
c_flg = .F.
c_f = 0
c_n = SPACE(28)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 11, 41, "Заполните бланк"

DO Sun_Bord WITH 3, 4, 5, 36, "    Причина коррекции"
DO Sun_Bord WITH WROWS()-5, 4, WROWS()-3, 36, " Примечание "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO Get_Firm WITH 2, 2, enab_firm
@ 3, 6 GET c_flg PICTURE "@*C " VALID S_Spec()
@ 4, 6 GET c_n WHEN .F.
@ WROWS()-4, 5 GET nt PICTURE "@S31"
@ WROWS()-2, 4 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE VALID Tst_F()

IF ex = 1
  SCATTER BLANK TO arr_w
  arr_w(1) = "*"
  IF SEEK(" ")
    GATHER FROM arr_w
  ELSE
    APPEND FROM ARRAY arr_w
  ENDIF
  REPLACE REAS_CODE WITH c_f,    ;
          FIRM      WITH f_c000, ;
          NOTE      WITH nt,     ;
          WHO       WITH user,   ;
          WHERE     WITH arm
ENDIF
*--------------------------------------------------------------------------
POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Выбор причины коррекции.                           ║
*║                                                                        ║
*╚═══════════════════════════════════════════════════════════ 16.11.2000 ═╝
PROCEDURE S_Spec
 
DO Spec_Fil WITH c_f, c_n
c_flg = .F.
SHOW GET c_n
SHOW GET c_flg

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Коррекция списка.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 03.09.97 12:35:51 ═╝
PROCEDURE Corr_Lst

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
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Switch OF (menu_name) PROMPT "Список" KEY Ctrl-A, "F2"
ON SELECTION PAD Switch OF (menu_name) DO Sw_Mode WITH "Switch"

DEFINE PAD GTD OF (menu_name) PROMPT "ГТД" KEY Ctrl-A, "Ctrl-Tab"
ON SELECTION PAD GTD OF (menu_name) DO Sw_Mode WITH "GTD"

DEFINE PAD Addr OF (menu_name) PROMPT "Адреса" KEY Ctrl-A, "F3"
ON SELECTION PAD Addr OF (menu_name) DO Sw_Mode WITH "Addr"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

SELECT CORR_TMP
ZAP
APPEND BLANK

SELECT STOCK
SET ORDER TO TAG CODE

SELECT CORR_LST
SET RELATION TO CODE INTO STOCK

SEEK RECNO("CORR_TIT")
SCAN REST WHILE LST_NUM = RECNO("CORR_TIT")
  SELECT CORR_TMP
  APPEND BLANK
  REPLACE CODE     WITH CORR_LST.CODE, ;
          PREFIX   WITH STOCK.PREFIX, ;
          NAME     WITH STOCK.NAME, ;
          PRODUCER WITH STOCK.PRODUCER, ;
          QNT_I    WITH IIF(CORR_LST.QNT > 0, CORR_LST.QNT, 0), ;
          QNT_E    WITH IIF(CORR_LST.QNT < 0, -CORR_LST.QNT, 0),;
          GTD_NUM  WITH CORR_LST.GTD_NUM, ;
          COUNTRY  WITH CORR_LST.COUNTRY, ;
          COUNTRYC WITH CORR_LST.COUNTRYC,;
          GTD_LINE WITH CORR_LST.GTD_LINE,;
          PRICE    WITH CORR_LST.PRICE
  SELECT CORR_LST
ENDSCAN
SET RELATION TO

SELECT STOCK
SET ORDER TO TAG NAME
GO TOP

SELECT CORR_TMP
SET ORDER TO TAG CODE

SELECT STOCK
SET ORDER TO TAG NAME
SET RELATION TO IIF(SEEK(CODE,"CORR_TMP"), CODE, 0) INTO CORR_TMP
*
*   Расчет размеров окна
*
PRIVATE ln, wd, all_st
ln = 15  && Количество видимых строк BROWSE
wd = 76
all_st = .T.

DO D_Wins WITH ln, wd, "Коррекция наличия: "+   ;
               ALLTRIM(LEFT(ACCOUNT.CUS_NAME,24)), 1, 2
@ 2, 10 SAY "Позиция"
@ 2, 43 SAY "Склад"
@ 2, 50 SAY "Дост."
@ 2, 56 SAY "Приход"
@ 2, 63 SAY "Расход"
@ 2, 72 SAY "Цена"
@ 1, 2 SAY "Поиск"

@ WROWS()-4, 3 SAY "Адрес"
@ WROWS()-4,16 SAY "Доп. адрес"
@ WROWS()-4,37 SAY "Филиалы"
@ WROWS()-4,48 SAY "Бронь"
@ WROWS()-4,59 SAY "Реал."

what_do = "List"

KEYBOARD IIF(let_pr, "{Enter}", "")+"{Enter}{Enter}{Tab}"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Номенклатура

    IF all_st
      SELECT CORR_TMP
      SET ORDER TO TAG CODE

      SELECT STOCK
      SET ORDER TO TAG NAME
      SET RELATION TO IIF(SEEK(CODE,"CORR_TMP"), CODE, 0) INTO CORR_TMP
    ELSE
      SELECT STOCK
      SET ORDER TO TAG CODE

      SELECT CORR_TMP
      SET ORDER TO TAG NAME
      SET RELATION TO CODE INTO STOCK
    ENDIF

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F2  DO Sw_Mode WITH "Switch"
    ON KEY LABEL Ctrl+Tab DO Sw_Mode WITH "GTD"
    ON KEY LABEL F3  DO Sw_Mode WITH "Addr"
    ON KEY LABEL Tab DO Seek_P
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM

*
*    BROWSE - меню
*
    BROWSE FIELDS PREFIX:H="":W=.F.,     ;
                  Brw_Name=Nm_Part(NAME):25:H="":W=.F.,     ;
                  PRODUCER:H="":W=.F.,     ;
                  STOCK.QNT:P="@Z 999999":H="":W=.F., ;
                  Q000=MIN(STOCK.QNT,STOCK.QNT+  ;
                       STOCK.QNT_REAL+STOCK.QNT_SAVE+STOCK.QNT_REZ-STOCK.QNT_REP)  ;
                       :P="@Z 999999":H="":W=.F.,    ;
                  CORR_TMP.QNT_I:P="@Z 999999":H="":V=Tst_Q(),  ;
                  CORR_TMP.QNT_E:P="@Z 999999":H="":V=Tst_Q(),  ;
                  CORR_TMP.PRICE:P="@Z 99999.99":W=W_Pr():H="" ;
           WHEN Pos_Foot() ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR   ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    SET RELATION TO
    IF what_do = "List"
      what_do = "Exit"
    ENDIF

  CASE what_do = "Switch"  && Переключение список/номенклатура

    all_st = .NOT. all_st
    DEFINE PAD Switch OF (menu_name) PROMPT IIF(all_st,"Список", "Склад") ;
                          KEY Ctrl-A, "F2"
    KEYBOARD IIF(let_pr, "{Enter}", "")+"{Enter}{Enter}{Tab}"
    what_do = "List"

  CASE what_do = "GTD"      && ГТД

    DO Get_GTD
    what_do = "List"

  CASE what_do = "Addr"     && Адреса

    DO Addr
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

PRIVATE arr_w
SELECT CORR_LST
DO WHILE SEEK(RECNO("CORR_TIT"))
  REPLACE LST_NUM WITH 0
ENDDO
SCATTER TO arr_w BLANK
arr_w(1) = RECNO("CORR_TIT")

SELECT CORR_TMP
SET ORDER TO TAG NAME
SCAN FOR CORR_TMP.QNT_I-CORR_TMP.QNT_E # 0
  SELECT CORR_LST
  GO TOP
  IF LST_NUM = 0
    APPEND FROM ARRAY arr_w
  ELSE
    GATHER FROM arr_w
  ENDIF
  REPLACE CODE     WITH CORR_TMP.CODE, ;
          QNT      WITH CORR_TMP.QNT_I-CORR_TMP.QNT_E, ;
          GTD_NUM  WITH CORR_TMP.GTD_NUM, ;
          COUNTRY  WITH CORR_TMP.COUNTRY, ;
          COUNTRYC WITH CORR_TMP.COUNTRYC,;
          GTD_LINE WITH CORR_TMP.GTD_LINE,;
          PRICE    WITH CORR_TMP.PRICE
  SELECT CORR_TMP
ENDSCAN

SELECT CORR_TIT

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Вход в поле цены.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 05.09.97 14:15:36 ═╝
PROCEDURE W_Pr

IF CORR_TMP.QNT_I-CORR_TMP.QNT_E = 0 
  IF CORR_TMP.PRICE # 0
    REPLACE PRICE WITH 0
  ENDIF
  RETURN .F.
ENDIF

RETURN let_pr

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║             Проверка количества и добавление новой записи.             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 03.09.97 14:39:30 ═╝
PROCEDURE Tst_Q
PRIVATE v1, v2, sel_sav, mss

IF MIN(STOCK.QNT,STOCK.QNT+STOCK.QNT_REAL+STOCK.QNT_SAVE+STOCK.QNT_REZ-STOCK.QNT_REP) ;
    < CORR_TMP.QNT_E-CORR_TMP.QNT_I
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"На складе столько нет!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  ACTIVATE WINDOW (IIF(all_st, "STOCK", "CORR_LST"))
  RETURN .F.
ENDIF

v1 = CORR_TMP.QNT_I-CORR_TMP.QNT_E
IF v1 = 0
  IF CORR_TMP.PRICE # 0
    REPLACE PRICE WITH 0
  ENDIF
ENDIF
v2 = 0
IF v1 < 0
  v2 = -v1
  v1 = 0
ENDIF
IF v1 # CORR_TMP.QNT_I .OR. v2 # CORR_TMP.QNT_E
  REPLACE CORR_TMP.QNT_I WITH v1, CORR_TMP.QNT_E WITH v2
  SHOW GET CORR_TMP.QNT_I
  SHOW GET CORR_TMP.QNT_E
ENDIF

IF (CORR_TMP.QNT_I # 0 .OR. CORR_TMP.QNT_E # 0) .AND. CORR_TMP.CODE = 0
  sel_sav = SELECT()
  SELECT CORR_TMP
  v1 = CORR_TMP.QNT_I
  v2 = CORR_TMP.QNT_E
  REPLACE CORR_TMP.QNT_I WITH 0, CORR_TMP.QNT_E WITH 0
  APPEND BLANK
  REPLACE CORR_TMP.CODE     WITH STOCK.CODE,    ;
          CORR_TMP.PREFIX   WITH STOCK.PREFIX,  ;
          CORR_TMP.NAME     WITH STOCK.NAME,    ;
          CORR_TMP.PRODUCER WITH STOCK.PRODUCER,;
          CORR_TMP.QNT_I    WITH v1,            ;
          CORR_TMP.QNT_E    WITH v2
  SELECT (sel_sav)
  SHOW WINDOW STOCK SAME REFRESH
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Доп. данные о позиции.                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 03.09.97 15:31:07 ═╝
PROCEDURE Pos_Foot

ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-3, 3 SAY TRANSFORM(STOCK.ADDRESS, Addr_Pic()) COLOR (SCHEME(1,1))
@ WROWS()-3, COL()+4 SAY STOCK.MEMO_ST COLOR (SCHEME(1,1))
@ WROWS()-3, COL()+4 SAY STOCK.QNT_REAL PICTURE "@Z 999999" COLOR (SCHEME(1,1))
@ WROWS()-3, COL()+4 SAY STOCK.QNT_SAVE+STOCK.QNT_REZ PICTURE "@Z 999999" COLOR (SCHEME(1,1))
@ WROWS()-3, COL()+4 SAY STOCK.QNT_REP PICTURE "@Z 999999" COLOR (SCHEME(1,1))
@ WROWS()-2, 2 SAY "ГТД N "
@ WROWS()-2, COL() SAY CORR_TMP.GTD_NUM+"/"+CORR_TMP.GTD_LINE COLOR (SCHEME(1,1))
@ WROWS()-2, COL() SAY " Страна "
@ WROWS()-2, COL() SAY CORR_TMP.COUNTRYC COLOR (SCHEME(1,1))
@ WROWS()-2, COL()+1 SAY CORR_TMP.COUNTRY COLOR (SCHEME(1,1))
ACTIVATE WINDOW (ALIAS()) SAME
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                   Режим поиска по начальным буквам.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 03.09.97 16:00:06 ═╝
PROCEDURE Seek_P
PRIVATE prf, s, ln, sh, rc

ACTIVATE WINDOW (win_name+"_M") SAME
ln = FSIZE("NAME","STOCK")
ln = 35
sh = REPLICATE("▒",ln)
prf = ""

DO WHILE .T.
  @ 1, 8 SAY sh
  @ 1, 8 SAY prf
  s = INKEY(0, "M")
  IF s = 151 .AND. LASTKEY() # 151   && Мышка
    IF MROW("") = 0
      s = -9
    ENDIF
  ENDIF
  IF s = 3     && PgDn
    KEYBOARD "{PgDn}"+IIF(let_pr, "{Enter}", "")+"{Enter}{Enter}{Tab}"
    RETURN
  ENDIF

*
*     Навигация
*
  IF s = 18    && PgUp
    KEYBOARD "{PgUp}"+IIF(let_pr, "{Enter}", "")+"{Enter}{Enter}{Tab}"
    RETURN
  ENDIF
  IF s = 1     && Home
    GO TOP
    prf = ""
    LOOP
  ENDIF
  IF s = 6     && End
    GO BOTTOM
    prf = ""
    LOOP
  ENDIF
  IF s = 24    && Стрелка вниз
    KEYBOARD "{DnArrow}"+IIF(let_pr, "{Enter}", "")+"{Enter}{Enter}{Tab}"
    RETURN
  ENDIF
  IF s = 5     && Стрелка ввех
    KEYBOARD "{UpArrow}"+IIF(let_pr, "{Enter}", "")+"{Enter}{Enter}{Tab}"
    RETURN
  ENDIF
  IF s = 27    && Esc
    KEYBOARD "{Esc}"
    EXIT
  ENDIF
  IF s = 13    && Enter
    EXIT
  ENDIF

*
*  Асинхронные обработчики
*
  IF s = -1
    KEYBOARD "{F2}"
    EXIT
  ENDIF
  IF s = -2
    KEYBOARD "{F3}"
    EXIT
  ENDIF
  IF s = -9
    KEYBOARD "{F10}"
    EXIT
  ENDIF
  IF s = 148
    KEYBOARD "{Ctrl+Tab}"
    EXIT
  ENDIF
  IF BETWEEN(s,32,175) .OR. BETWEEN(s,224,241)
    IF s = 127     && BackSpace
      IF LEN(prf) > 0
        prf = LEFT(prf,LEN(prf)-1)
        SEEK prf
      ENDIF
    ENDIF
    rc = RECNO()
    IF .NOT. SEEK(prf+CHR(s))
      IF BETWEEN(rc, 1, RECCOUNT())
        GO rc
      ENDIF
    ELSE
      prf = prf+CHR(s)
    ENDIF
    SHOW WINDOW (IIF(all_st, "STOCK", "CORR_TMP")) SAME REFRESH
  ENDIF
ENDDO

@ 1, 8 SAY SPACE(ln)
@ 1, 8 SAY "Tab" COLOR (SCHEME(4,7))

ACTIVATE WINDOW (IIF(all_st, "STOCK", "CORR_TMP"))

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Удаление заготовки.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 04.09.97 12:02:37 ═╝
PROCEDURE Del_List

PARAMETER mute

PRIVATE mss, rc_sv1, rc_sv2
IF .NOT. mute
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Действительно удалить? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN
  ENDIF
ENDIF

rc_sv1 = RECNO()
SKIP
IF EOF() .OR. BOF()
  GO BOTTOM
ENDIF
rc_sv2 = RECNO()
GO rc_sv1
REPLACE NOT_EMP WITH " "
SELECT CORR_LST
DO WHILE SEEK(rc_sv1)
  REPLACE LST_NUM WITH 0
ENDDO

SELECT CORR_TIT
GO rc_sv2
IF NOT_EMP = " "
  SEEK "*"
ENDIF

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Коррекция адресов.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 14.06.97 14:20:18 ═╝
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

*┌─────────────────────────────────────────────────────────┐
*│ ┌─ Адреса ────────────────────────────────────────────┐ │
*│ │ Основной ▓▓▓▓▓▓▓▓   Дополнительный ▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓ │ │
*│ └─────────────────────────────────────────────────────┘ │
*│           Упаковка ▓▓▓▓▓▓     Отпуск ▓▓▓▓▓▓▓▓           │
*│            < OK Ctrl-W > < Отказаться Esc >             │
*└───────────────── F1 - История адресов ──────────────────┘

PRIVATE a1, a2, pk, pk_m, ex, nm_w

nm_w = ALLTRIM(STOCK.PREFIX+" "+STOCK.NAME+" "+STOCK.PRODUCER)
a1   = STOCK.ADDRESS
a2   = STOCK.MEMO_ST
pk   = STOCK.QNT_ROUND
pk_m = STOCK.QNT_PACK
ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 61, nm_w
@ WROWS()-1, 19 SAY " F1 - История адресов "
*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO Sun_Bord WITH 2, 3, 4, 57, " Адреса "

@ 3,  5 SAY "Основной" GET a1 PICTURE Addr_Pic()
@ 3, 25 SAY "Дополнительный" GET a2
@ 5, 13 SAY "Упаковка" GET pk PICTURE "999999" VALID Tst_Pack(1)
@ 5, 32 SAY " Отпуск" GET pk_m PICTURE "99999999" VALID Tst_Pack(2)
@ 6, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

ON KEY LABEL F1 DO Addr_Hst WITH a1, a2, STOCK.ADDR_0, STOCK.MEMO_ST_0, STOCK.ADDR_1, STOCK.MEMO_ST_1
READ CYCLE
ON KEY LABEL F1

IF ex = 1
  IF STOCK.ADDRESS # a1   
    REPLACE STOCK.ADDR_1  WITH STOCK.ADDR_0
    REPLACE STOCK.ADDR_0  WITH STOCK.ADDRESS
    REPLACE STOCK.ADDRESS WITH a1
  ENDIF
  IF STOCK.MEMO_ST # a2
    REPLACE STOCK.MEMO_ST_1 WITH STOCK.MEMO_ST_0
    REPLACE STOCK.MEMO_ST_0 WITH STOCK.MEMO_ST
    REPLACE STOCK.MEMO_ST   WITH a2
  ENDIF
  REPLACE STOCK.QNT_PACK  WITH pk_m,;
          STOCK.QNT_ROUND WITH pk
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Продажа списка.                             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 04.09.97 14:36:39 ═╝
PROCEDURE Sale_L

PRIVATE doc_n, doc_s, doc_s_d, q_rest, q_exp, st_ord, delta, mss, q_pos

SELECT STOCK
st_ord = "TAG CODE" $ SET("ORDER")
SET ORDER TO TAG CODE

SELECT 0
USE (base_path+"ACT_S") ORDER TAG SALE

SELECT 0
USE (base_path+"SPEC_SUM") ORDER TAG POS_CUS

SELECT 0
USE (base_path+"SALE")

SELECT CORR_LST
SEEK RECNO("CORR_TIT")
doc_n = ""
doc_s = 0
doc_s_d = 0
SCAN REST WHILE LST_NUM = RECNO("CORR_TIT") FOR QNT < 0
  = SEEK(CODE,"STOCK")
  q_rest = MIN(-QNT, STOCK.QNT)
  delta = -QNT-q_rest     && Здесь собираем то, что недоизрасходовали!
  *
  *  Подбираем по сериям
  *
  SELECT ACT_S
  SEEK STR(CORR_LST.CODE,7)
  q_pos = 0
  SCAN REST WHILE CODE = CORR_LST.CODE FOR REST_REP < QNT_REST
    q_w = MIN(QNT_REST-REST_REP, q_rest)
    REPLACE LAST_SALE WITH DATE(), ;
            QNT_REST  WITH QNT_REST-q_w
    IF EMPTY(doc_n)   && Надо получить номер документа!
      doc_n = Doc_Num("SALE", CORR_TIT.FIRM, arm, DATE())
      doc_n = sys_char+RIGHT(STR(1000+VAL(ALLTRIM(arm))),3)+STR(doc_n,6)
    ENDIF
    SELECT SALE
    APPEND BLANK
    REPLACE FIRM      WITH CORR_TIT.FIRM, ;
            DOC_NUM   WITH doc_n,         ;
            DOC_DATE  WITH DATE(),        ;
            CODE      WITH CORR_LST.CODE, ;
            QNT       WITH q_w,           ;
            PRICE_D_L WITH STOCK.PRICE,   ;
            PRICE_R_L WITH STOCK.PRICE_R, ;
            PRICE_D   WITH CORR_LST.PRICE/kurs, ;
            PRICE_0   WITH CORR_LST.PRICE,;
            PRICE_R   WITH CORR_LST.PRICE,;
            ACT_FIRM  WITH ACT_S.FIRM,    ;
            ACT_NUM   WITH ACT_S.DOC_NUM, ;
            ACT_DATE  WITH ACT_S.DOC_DATE,;
            PARTY     WITH ACT_S.PARTY,   ;
            GTD_NUM   WITH ACT_S.GTD_NUM, ;
            COUNTRY   WITH ACT_S.COUNTRY, ;
            COUNTRYC  WITH ACT_S.COUNTRYC,;
            GTD_LINE  WITH ACT_S.GTD_LINE,;
            WHERE     WITH arm
    q_pos = q_pos+QNT
    doc_s_d = doc_s_d+QNT*PRICE_D

    SELECT STOCK
    REPLACE QNT WITH QNT-q_w, QNT_SPEC WITH QNT_SPEC+q_w

    SELECT SPEC_SUM
    IF .NOT. SEEK(STR(STOCK.CODE,7)+STR(CORR_TIT.REAS_CODE,8))
      DO New_Rec WITH STOCK.CODE, CORR_TIT.REAS_CODE
    ENDIF
    REPLACE QNT WITH QNT+q_w
    IF QNT = 0
      REPLACE CODE WITH 0, CUS_CODE WITH 0
    ENDIF

    SELECT ACT_S
    q_rest = q_rest-q_w
    IF q_rest < 1
      EXIT
    ENDIF
  ENDSCAN
  doc_s = doc_s+ROUND(q_pos*SALE.PRICE_R,2)
  delta = delta+q_rest
  IF delta # 0
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Позиция "+ALLTRIM(STOCK.PREFIX)+" "+  ;
                               ALLTRIM(STOCK.NAME)+" "+    ;
                               ALLTRIM(STOCK.PRODUCER)+":"
    mss(3) = CHR(0)+"не хватает "+ALLTRIM(STR(delta))+" штук."
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
  SELECT CORR_LST
ENDSCAN

SELECT ACT_S
USE

SELECT SPEC_SUM
USE

SELECT SALE
USE

IF .NOT. EMPTY(doc_n)
  USE (base_path+"SALE_TIT")
  APPEND BLANK
  REPLACE FIRM      WITH CORR_TIT.FIRM, ;
          DOC_NUM   WITH doc_n,         ;
          DOC_DATE  WITH DATE(),        ;
          DOC_TIME  WITH SECONDS(),     ;
          CUS_CODE  WITH CORR_TIT.REAS_CODE, ;
          CUS_NAME  WITH ACCOUNT.CUS_NAME, ;
          LIST_NUM  WITH "******",      ;
          NDS_      WITH 0,             ;
          TOTAL_D   WITH doc_s_d,       ;
          TOTAL_0   WITH doc_s,         ;
          TOTAL_R   WITH doc_s,         ;
          WHO       WITH user,          ;
          WHERE     WITH arm
  IF FSIZE("NOTE") # 0
    REPLACE NOTE WITH CORR_TIT.NOTE
  ENDIF
ENDIF
USE

IF .NOT. EMPTY(doc_s)
  USE (base_path+"CUS_REST") ORDER TAG CUS_CODE
  IF .NOT. SEEK(STR(CORR_TIT.REAS_CODE)+CORR_TIT.FIRM)
    APPEND BLANK
    REPLACE CUS_CODE WITH CORR_TIT.REAS_CODE, ;
            FIRM     WITH CORR_TIT.FIRM
  ENDIF
  REPLACE ACC_SUM WITH ACC_SUM-doc_s
  USE
ENDIF

SELECT STOCK
IF .NOT. st_ord
  SET ORDER TO TAG NAME
ENDIF

SELECT CORR_TIT

RETURN doc_n

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Приход позиций.                             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 04.09.97 18:33:05 ═╝
PROCEDURE Inc_L

PRIVATE doc_n, doc_s, doc_s_n, st_ord, delta, mss, is_emp
PRIVATE r_brends, t_gtd, t_cnt, t_cntc, t_line, bad_g
PRIVATE tmpParty

is_emp = .F.

SELECT 0
r_brends = ""
USE base_path+"B_GROUPS" SHARED AGAIN ALIAS BBB_2412
SCAN
  r_brends = r_brends+DEV_LIST
ENDSCAN
USE

SELECT STOCK
st_ord = "TAG CODE" $ SET("ORDER")
SET ORDER TO TAG CODE

SELECT CORR_LST
SEEK RECNO("CORR_TIT")
SCAN REST WHILE LST_NUM = RECNO("CORR_TIT") FOR QNT > 0

  IF .NOT. EMPTY(GTD_NUM)
    IF .NOT. B_GTD .OR. .NOT. EMPTY(LAST_SALE)
      LOOP
    ENDIF
  ENDIF
  = SEEK(CODE, "STOCK")
  IF " "+ALLTRIM(STR(STOCK.PRO_CODE))+" " $ r_brends
    LOOP
  ENDIF
  t_gtd  = GTD_NUM
  t_cnt  = COUNTRY
  t_cntc = COUNTRYC
  t_line = GTD_LINE
  bad_g  = B_GTD
  DO Set_Gtd WITH CODE, STOCK.PREFIX, STOCK.NAME, STOCK.PRODUCER, t_gtd, t_cnt, t_cntc, t_line, bad_g
  REPLACE GTD_NUM WITH t_gtd, COUNTRY WITH t_cnt, COUNTRYC WITH t_cntc, GTD_LINE WITH t_line, b_GTD WITH bad_g
  IF EMPTY(GTD_NUM)
    is_emp = .T.
    EXIT
  ENDIF
ENDSCAN
IF USED("SET_GTD")
  USE IN SET_GTD
ENDIF
IF is_emp
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  ВНИМАНИЕ! Не везде проставлена ГТД! Продолжаем прием? [ Нет ] [ Да ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 1
    IF .NOT. st_ord
      SET ORDER TO TAG NAME
    ENDIF
    SELECT CORR_TIT
    RETURN CHR(0)
  ENDIF
ENDIF

SELECT 0
USE (base_path+"ACT_S")

SELECT 0
USE (base_path+"SPEC_SUM") ORDER TAG POS_CUS

SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_NUM ALIAS GTD_0220 AGAIN

SELECT CORR_LST
SEEK RECNO("CORR_TIT")
doc_n = ""
doc_s = 0
SCAN REST WHILE LST_NUM = RECNO("CORR_TIT") FOR QNT > 0
  = SEEK(CODE,"STOCK")
  IF .NOT. EMPTY(CORR_LST.GTD_NUM) .AND. .NOT. SEEK(CORR_LST.GTD_NUM, "GTD_0220")
    SELECT GTD_0220
    APPEND BLANK
    REPLACE GTD_NUM  WITH CORR_LST.GTD_NUM, ;
            GTD_LINE WITH CORR_LST.GTD_LINE,;
            COUNTRY  WITH CORR_LST.COUNTRY, ;
            COUNTRYC WITH CORR_LST.COUNTRYC
  ENDIF
  SELECT ACT_S
  IF EMPTY(doc_n)   && Надо получить номер документа!
    doc_n = Doc_Num("ACT_NUM", CORR_TIT.FIRM, arm, DATE())
    doc_n = STR(doc_n,8)+"  "
  ENDIF
  m.tmpParty = G_Party()
  APPEND BLANK
  REPLACE DOC_NUM   WITH doc_n,         ;
          FIRM      WITH CORR_TIT.FIRM, ;
          DOC_DATE  WITH DATE(),        ;
          FLG_BUY   WITH "B",           ;
          CODE      WITH CORR_LST.CODE, ;
          PARTY     WITH m.tmpParty,	;
          QNT_INC   WITH CORR_LST.QNT,  ;
          QNT_REST  WITH CORR_LST.QNT,  ;
          GTD_NUM   WITH CORR_LST.GTD_NUM,;
          COUNTRY   WITH CORR_LST.COUNTRY,;
          COUNTRYC  WITH CORR_LST.COUNTRYC,;
          GTD_LINE  WITH CORR_LST.GTD_LINE,;
          PRICE_0   WITH CORR_LST.PRICE,;
          PRICE_R   WITH CORR_LST.PRICE,;
          PRICE_D   WITH CORR_LST.PRICE/kurs
  doc_s = doc_s+QNT_INC*PRICE_R
  SELECT STOCK
  REPLACE QNT WITH QNT+CORR_LST.QNT, QNT_SPEC WITH QNT_SPEC-CORR_LST.QNT

  SELECT SPEC_SUM
  IF .NOT. SEEK(STR(STOCK.CODE,7)+STR(CORR_TIT.REAS_CODE,8))
    DO New_Rec WITH STOCK.CODE, CORR_TIT.REAS_CODE
  ENDIF
  REPLACE QNT WITH QNT-CORR_LST.QNT
  IF QNT = 0
    REPLACE CODE WITH 0, CUS_CODE WITH 0
  ENDIF
  SELECT CORR_LST
ENDSCAN

SELECT GTD_0220
USE

SELECT ACT_S
USE

SELECT SPEC_SUM
USE

IF .NOT. EMPTY(doc_n)
  USE (base_path+"ACT_S_T")
  APPEND BLANK
  REPLACE DOC_NUM   WITH doc_n,         ;
          FIRM      WITH CORR_TIT.FIRM, ;
          DOC_DATE  WITH DATE(),        ;
          DOC_TIME  WITH SECONDS(),     ;
          FLG_BUY   WITH "B",           ;
          SUP_CODE  WITH CORR_TIT.REAS_CODE, ;
          VALUTA    WITH "Р",           ;
          TOTAL_0   WITH doc_s,         ;
          TOTAL_H   WITH doc_s,         ;
          NDS_      WITH 0,             ;
          WHO_ACT   WITH user,          ;
          WHERE_ACT WITH arm
  IF FSIZE("NOTE") # 0
    REPLACE NOTE WITH CORR_TIT.NOTE
  ENDIF
ENDIF
USE

IF .NOT. EMPTY(doc_s)
  SELECT 0
  USE (base_path+"CUS_REST") ORDER TAG CUS_CODE
  IF .NOT. SEEK(STR(CORR_TIT.REAS_CODE)+CORR_TIT.FIRM)
    APPEND BLANK
    REPLACE CUS_CODE WITH CORR_TIT.REAS_CODE, ;
            FIRM     WITH CORR_TIT.FIRM
  ENDIF
  REPLACE ACC_SUM WITH ACC_SUM+doc_s
  USE
ENDIF

SELECT STOCK
IF .NOT. st_ord
  SET ORDER TO TAG NAME
ENDIF

SELECT CORR_TIT

RETURN doc_n

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Приход/расход списка.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 04.09.97 19:01:06 ═╝
PROCEDURE Exp_List

PRIVATE d_d, doc_i, doc_e, prn_i, prn_e, mss, f_v
*DO Wt_Mess WITH "Формируем документы."

d_d = DATE()
doc_i = Inc_L()
IF doc_i = CHR(0)
  RETURN
ENDIF
doc_e = Sale_L()
IF .NOT. EMPTY(doc_e)
  DO SendToVK WITH CORR_TIT.FIRM, doc_e, DATE()
ENDIF
f_v = CORR_TIT.FIRM
DO Del_List WITH .T.
prn_i = .T.
prn_e = .T.

*DO Wt_Mess

IF EMPTY(doc_i+doc_e)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Документы не сформированы - список пуст!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
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

*┌───────────────────────────────────────┐
*│                                       │
*│  [ ] Приходный документ N 99999999    │
*│                                       │
*│  [ ] Расходный документ N A000-999999 │
*│                                       │
*│                < OK >                 │
*│                                       │
*└───────────────────────────────────────┘

PRIVATE ex, pic1, pic2
ex   = 1             &&
pic1 = ""
pic2 = ""
IF .NOT. EMPTY(doc_i)
  pic1 = "@*C Приходный документ N "+ALLTRIM(doc_i)
ENDIF
IF .NOT. EMPTY(doc_e)
  pic2 = "@*C Расходный документ N "+LEFT(doc_e,4)+"-"+ALLTRIM(SUBSTR(doc_e,5))
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH IIF(EMPTY(doc_i) .OR. EMPTY(doc_e), 7, 10), 43, "Печать документов"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 1, 0 SAY ""

IF .NOT. EMPTY(doc_i)
  @ ROW()+2, 4 GET prn_i PICTURE pic1
ENDIF
IF .NOT. EMPTY(doc_e)
  @ ROW()+2, 4 GET prn_e PICTURE pic2
ENDIF
@ ROW()+2,18 GET ex PICTURE "@*HT \ OK "

READ CYCLE

POP KEY
RELEASE WINDOW (win_name)

IF prn_i .AND. .NOT. EMPTY(doc_i)
  DO P_Act WITH f_v, doc_i, d_d
ENDIF

IF prn_e .AND. .NOT. EMPTY(doc_e)
  DO Prn_Inv WITH f_v, doc_e, d_d
ENDIF

*--------------------------------------------------------------------------

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║              Добавление записи в медленно растущий файл.               ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.04.98 13:59:08 ═╝
PROCEDURE New_Rec
PARAMETERS s_c, c_c

PRIVATE arr_w, to_add

SCATTER TO arr_w BLANK
arr_w(1) = s_c
arr_w(2) = c_c

to_add = .T.

DO WHILE SEEK("      0       0")
  IF LOCK()
    to_add = .F.
    EXIT
  ENDIF
ENDDO

IF to_add
  APPEND FROM ARRAY arr_w
ELSE
  GATHER FROM arr_w
ENDIF

UNLOCK

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_F        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Выход из бланка.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 09.03.1999 ──┘
PROCEDURE Tst_F

PRIVATE mss
IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали предприятие!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD "{Tab}{Tab}"
  RETURN .F.
ENDIF

IF EMPTY(c_n)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали причину коррекции!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  KEYBOARD "{Tab}{Tab}{Tab}"
  RETURN .F.
ENDIF

IF c_f = -2
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Пересортица осуществляется в программе WinSale!"
  mss(3) = CHR(0)+"  Будем производить коррекцию в этой программе? [ Нет ] [ Да ]"
  mss(4) = ""
  IF Out_Mess(7, "mss") = 1
    KEYBOARD "{Tab}{Tab}{Tab}"
    RETURN .F.
  ENDIF
ENDIF

IF c_f = -3
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Инвентаризация осуществляется в программе WinSale!"
  mss(3) = CHR(0)+"  Будем производить коррекцию в этой программе? [ Нет ] [ Да ]"
  mss(4) = ""
  IF Out_Mess(7, "mss") = 1
    KEYBOARD "{Tab}{Tab}{Tab}"
    RETURN .F.
  ENDIF
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Проверка нормы упаковки.                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.05.97 10:48:15 ═╝
PROCEDURE Tst_Pack
PARAMETER sw

IF ex = 2 .OR. READKEY() % 256 = 12
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

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_GTD      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Выбор ГТД.                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 16.02.2000 ──┘
PROCEDURE Get_GTD

PRIVATE tmpGTD, point1, point2, point3, mss, s_sav

IF CORR_TMP.CODE = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Номер ГТД для пустого количества!?"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
IF CORR_TMP.QNT_I = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"  Номер ГТД для пустого количества! Это правильно? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(7, "mss") = 2
    RETURN
  ENDIF
ENDIF

tmpGTD = Ch_GTD(CORR_TMP.GTD_NUM)
point1 = AT(CHR(13), tmpGTD)
point2 = AT(CHR(13), tmpGTD, 2)
point3 = AT(CHR(13), tmpGTD, 3)
s_sav = SELECT()
SELECT CORR_TMP
IF point1 # 0
  REPLACE GTD_NUM  WITH LEFT(tmpGTD,point1-1), ;
          COUNTRY  WITH SUBSTR(tmpGTD,point1+1, point2-point1-1), ;
          GTD_LINE WITH SUBSTR(tmpGTD,point2+1, point3-point2-1), ;
          COUNTRYC WITH SUBSTR(tmpGTD,point3+1)
ENDIF
SELECT (s_sav)

RETURN
