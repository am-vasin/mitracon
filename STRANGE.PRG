*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Strange      Разработчик Андрей Васин           21.05.97 12:11:25 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Обработка нескладских позиций.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Strange
PRIVATE upper_1, upper_2, cus_st
STORE "" TO upper_1, upper_2
cus_st = .F.
DO SetUpper
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
PRIVATE sel_sav, ld_rec, chr_shab
chr_shab = ' !"'+    ;
           "#$%&'()*+,-./0123456789:;<=>?@"+  ;
           "ABCDEFGHIJKLMNOPQRSTUVWXYZ"+         ;
           "[\]^_`"+                             ;
           "abcdefghijklmnopqrstuvwxyz"+         ;
           "{|}~"+                               ;
           "АБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"+   ;
           "абвгдежзийклмнопрстуфхцчшщъыьэюя"
sel_sav = SELECT()
SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME1 ALIAS ST_SEEK AGAIN
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_FOUND AGAIN
SELECT 0
DELETE FILE (tmpo_path+"ST_LINK.DBF")
DELETE FILE (tmpo_path+"ST_LINK.CDX")
CREATE DBF (tmpo_path+"ST_LINK") ;
  ( STRNG_REC N(10), WGT N(2), CODE N(7) )
INDEX ON STR(STRNG_REC)+STR(WGT) TAG ORD
SET RELATION TO CODE INTO ST_FOUND
SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_CODE ALIAS FOR_DUBL AGAIN
SELECT 0
USE (base_path+"TEACH") ORDER TAG NAME AGAIN
SELECT 0
DO Use_Dummy
SELECT 0
DELETE FILE (tmpo_path+"STRANGE.DBF")
DELETE FILE (tmpo_path+"STRANGE.CDX")
CREATE DBF (tmpo_path+"STRANGE")  ;
  (  RC_NO    N(10),  ;
     CODE     N( 7),  ;
     PREFIX   C( 4),  ;
     NAME     C(35),  ;
     PRO_CODE N( 5),  ;
     PRODUCER C( 8),  ;
     S_PREF   C( 4),  ;
     S_NAME   C(35),  ;
     S_PROD   C( 8),  ;
     AMODEL   C(24),  ;
     FUNCTION C(32),  ;
     QNT      N( 6),  ;
     PRICE_L  N(12,5),;
     PRICE_L_D N(10,4),;
     PRICE_R  N(12,5),;
     PRICE_D  N(10,4), ;
     OFF      L  )
USE (tmpo_path+"STRANGE") EXCLUSIVE
*INDEX ON NAME+PREFIX+PRODUCER TAG NAME
PRIVATE sto_ord, sto_rec, prf

SELECT STOCK
sto_ord = SET("ORDER")
IF .NOT. EMPTY(sto_ord)
  sto_ord = SUBSTR(sto_ord, 5)
  sto_rec = AT(" ", sto_ord)
  sto_ord = IIF(sto_rec > 0, LEFT(sto_ord, sto_rec-1), sto_ord)
ENDIF
sto_rec = RECNO()
SET ORDER TO TAG NAME_P

SELECT LD
ld_rec = RECNO()
SEEK lc
SCAN REST WHILE LIST_NUM = lc FOR EMPTY(CODE)
  SELECT STRANGE
  APPEND BLANK
  REPLACE  RC_NO    WITH RECNO("LD"), ;
           PREFIX   WITH LD.PREFIX,   ;
           NAME     WITH LD.NAME,     ;
           PRODUCER WITH LD.PRODUCER, ;
           S_PREF   WITH LD.PREFIX,   ;
           S_NAME   WITH LD.NAME,     ;
           S_PROD   WITH LD.PRODUCER, ;
           AMODEL   WITH LD.AMODEL,   ;
           FUNCTION WITH LD.FUNCTION, ;
           PRICE_L  WITH LD.DEF_PRICE,;
           PRICE_L_D WITH LD.USD_DEF, ;
           PRICE_R  WITH LD.SAL_PRICE,;
           PRICE_D  WITH LD.USD_PRICE,;
           QNT      WITH LD.QNT,      ;
           OFF      WITH .F.
  DO P_Offer
  SELECT LD
ENDSCAN
SELECT STRANGE
IF RECCOUNT() = 0
  USE
  SELECT STOCK
  SET ORDER TO (sto_ord)
  IF BETWEEN(sto_rec, 1, RECCOUNT())
    GO sto_rec
  ENDIF

  IF BETWEEN(ld_rec, 1, RECCOUNT("LD"))
    GO ld_rec IN LD
  ENDIF
  RELEASE MENU (menu_name) EXTENDED
  ACTIVATE WINDOW L_Det_M BOTTOM
  DELETE FILE (tmpo_path+"STRANGE.DBF")
  DELETE FILE (tmpo_path+"STRANGE.CDX")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одной нескладской позиции!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT FOR_DUBL
  USE
  SELECT ST_LINK
  USE
  SELECT ST_SEEK
  USE
  SELECT ST_FOUND
  USE
  SELECT (sel_sav)
  RETURN
ENDIF
SET RELATION TO STR(RECNO()) INTO ST_LINK
GO TOP

*
*   Расчет размеров окна
*

PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = IIF(LIST_TIT.IS_USD, 69, 70)

DO D_Wins WITH ln, wd, "Нескладские позиции", 0, 5

PRIVATE l_, ln_1, c_, wd_
l_ = WLROW(win_name+"_M")+WROWS(win_name+"_E")+4
ln_1 = 5
c_ = WLCOL(win_name+"_M")+2

DEFINE WINDOW (win_name+"_A") FROM l_, c_ TO l_+ln_1-1, c_+wd-1 NONE COLOR SCHEME 13
DEFINE WINDOW (win_name+"_B") FROM l_-3, c_-2 TO l_+ln_1, c_+wd+20   ;
                           NONE COLOR SCHEME 13


@ 1, 2 SAY "Ск─────── Наименование ──────────────────────────── Кол-во ──── Цена "

what_do = "List"

@ WROWS()-8, FLOOR((WCOLS()-30)/2) SAY " Ctrl+Enter - Вынести префикс "
@ WROWS()-7, 2 SAY PADC(IIF(mode_tch, " Режим обучения включен ", ;
                    " Режим обучения выключен "), WCOLS()-4, "─")

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

    DEFINE PAD Seek OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "Tab"
    ON SELECTION PAD Seek OF (menu_name) Do Sw_Mode WITH "Seek"

    DEFINE PAD C_Seek OF (menu_name) PROMPT "Конт.поиск" KEY Ctrl-A, "F7"
    ON SELECTION PAD C_Seek OF (menu_name) Do Sw_Mode WITH "C_Seek"

    DEFINE PAD F_List OF (menu_name) PROMPT "Сп." KEY Ctrl-A, "F1"
    ON SELECTION PAD F_List OF (menu_name) Do Sw_Mode WITH "Sw_Wind"

    DEFINE PAD Teach OF (menu_name) PROMPT "Ном.кл." KEY Ctrl-A, "F2"
    ON SELECTION PAD Teach OF (menu_name) Do Sw_Mode WITH "Teach"

    DEFINE PAD Change OF (menu_name) PROMPT "Зам." KEY Ctrl-A, "F6"
    ON SELECTION PAD Change OF (menu_name) Do Sw_Mode WITH "Change"

    DEFINE PAD Renames OF (menu_name) PROMPT "Переим." KEY Ctrl-A, "F8"
    ON SELECTION PAD Renames OF (menu_name) Do Sw_Mode WITH "Renames"

    DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
    ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "Exit"

    SELECT ST_LINK
    ACTIVATE WINDOW (win_name+"_A") TOP

    IF LIST_TIT.IS_USD
      BROWSE KEY STR(RECNO("STRANGE")) ;
             FIELDS DMY = " ":H="", ;
                    ST_FOUND.PREFIX:H="",     ;
                    ST_FOUND.NAME:H="",     ;
                    ST_FOUND.PRODUCER:H="",     ;
                    ST_FOUND.QNT:P="@Z 999999":H="", ;
                    ST_FOUND.PRICE:P="@Z 99999.9999":H="" ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             NOWAIT ;
             WINDOW (win_name+"_B") IN WINDOW (win_name+"_A")
    ELSE
      BROWSE KEY STR(RECNO("STRANGE")) ;
             FIELDS DMY = " ":H="", ;
                    ST_FOUND.PREFIX:H="",     ;
                    ST_FOUND.NAME:H="",     ;
                    ST_FOUND.PRODUCER:H="",     ;
                    ST_FOUND.QNT:P="@Z 999999":H="", ;
                    ST_FOUND.PRICE_R:P="@Z 99999.99999":H="" ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             NOWAIT ;
             WINDOW (win_name+"_B") IN WINDOW (win_name+"_A")
    ENDIF
    statys_type = 3
    SELECT STRANGE
    DO Prp_Nav_1
    ON KEY LABEL Ctrl+Enter DO Div_Pos
    ON KEY LABEL Tab    Do Sw_Mode WITH "Seek"
    ON KEY LABEL F6     Do Sw_Mode WITH "Change"
    ON KEY LABEL F7     Do Sw_Mode WITH "C_Seek"
    ON KEY LABEL F2     Do Sw_Mode WITH "Teach"
    ON KEY LABEL F8     Do Sw_Mode WITH "Renames"
    ON KEY LABEL F1     Do Sw_Mode WITH "Sw_Wind"

*
*    BROWSE - меню
*
    IF LIST_TIT.IS_USD
      BROWSE FOR .NOT. OFF REST  ;
             FIELDS F000 = IIF(EMPTY(CODE), " ", "√"):W=.F.:H="",     ;
             PREFIX:V=Is_Sto():H="",    ;
             NAME:W=S_Let():V=Is_Sto():H="",      ;
             PRODUCER:V=Is_Sto():H="",  ;
             QNT:W=.F.:P="@Z 999999":H="", PRICE_D:P="@Z 99999.9999":H="" ;
             WHEN Jmp_N() ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FOR .NOT. OFF REST  ;
             FIELDS F000 = IIF(EMPTY(CODE), " ", "√"):W=.F.:H="",     ;
             PREFIX:V=Is_Sto():H="",    ;
             NAME:W=S_Let():V=Is_Sto():H="",      ;
             PRODUCER:V=Is_Sto():H="",  ;
             QNT:W=.F.:P="@Z 999999":H="", PRICE_R:P="@Z 99999.99999":V=Corr_P():H="" ;
             WHEN Jmp_N() ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    RELEASE MENU (menu_name)
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Seek")
    ENDIF

*           DUMMY.F:H="" FREEZE DUMMY.F ;
*


  CASE what_do = "Sw_Wind"    && Коррекция позиции
  
    IF .NOT. FOUND("ST_LINK")
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Посмотрите внимательно, ничего не удалось подобрать!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      what_do = "List"
    ELSE
      DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

      DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
      ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

      DEFINE PAD List OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
      ON SELECTION PAD List OF (menu_name) Do Sw_Mode WITH "List"
    
      ON KEY LABEL Enter KEYBOARD CHR(23)

      SELECT ST_LINK
      ACTIVATE WINDOW (win_name+"_A") TOP

      IF LIST_TIT.IS_USD
        BROWSE KEY STR(RECNO("STRANGE")) ;
               FIELDS DMY = " ":H="", ;
                      ST_FOUND.PREFIX:H="",     ;
                      ST_FOUND.NAME:H="",     ;
                      ST_FOUND.PRODUCER:H="",     ;
                      ST_FOUND.QNT:P="@Z 999999":H="", ;
                      ST_FOUND.PRICE:P="@Z 99999.9999":H="", ;
                      DUMMY.F:H="" FREEZE DUMMY.F ;
               NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
               WINDOW (win_name+"_B") IN WINDOW (win_name+"_A")
      ELSE
        BROWSE KEY STR(RECNO("STRANGE")) ;
               FIELDS DMY = " ":H="", ;
                      ST_FOUND.PREFIX:H="",     ;
                      ST_FOUND.NAME:H="",     ;
                      ST_FOUND.PRODUCER:H="",     ;
                      ST_FOUND.QNT:P="@Z 999999":H="", ;
                      ST_FOUND.PRICE:P="@Z 99999.99999":H="", ;
                      DUMMY.F:H="" FREEZE DUMMY.F ;
               NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
               WINDOW (win_name+"_B") IN WINDOW (win_name+"_A")
      ENDIF
      what_do = IIF(READKEY() % 256 = 12, "List", "Choose")
    ENDIF
    ON KEY
    RELEASE MENU (menu_name)
    
  CASE what_do = "Corr"    && Коррекция позиции
  
    DO Corr_Pos
    what_do = "List"
    
  CASE what_do = "Seek"    && Поиск в номенклатуре
  
    DO Seek_Sto
    what_do = "List"
    
  CASE what_do = "Change"  && Контекстная замена
  
    DO Sh_Change
    what_do = "List"
    
  CASE what_do = "Teach"   && Поиск в номенклатуре клиента
  
    IF cus_st
      PRIVATE messg
      DIMENSION messg(3)
      messg(1) = ""
      messg(2) = CHR(0)+"Вы уже исследовали номенклатуру клиента"
      messg(3) = ""
      DO Out_Mess WITH 7, "messg"
    ELSE
      DO Cus_Sto
      cus_st = .T.
    ENDIF
    what_do = "List"
    
  CASE what_do = "Renames" && Переименования
  
    DO Old_Names
    what_do = "List"
    
  CASE what_do = "Choose"  && Выбор
  
    SELECT STRANGE
    REPLACE CODE      WITH ST_FOUND.CODE,     ;
            PREFIX    WITH ST_FOUND.PREFIX,   ;
            NAME      WITH ST_FOUND.NAME,     ;
            PRODUCER  WITH ST_FOUND.PRODUCER, ;
            PRICE_L   WITH ST_FOUND.PRICE_R,  ;
            PRICE_L_D WITH ST_FOUND.PRICE,    ;
            PRICE_R   WITH ST_FOUND.PRICE_R,  ;
            PRICE_D   WITH ST_FOUND.PRICE,    ;
            QNT       WITH Round_P(ALLTRIM(ST_FOUND.PREFIX)+" "+ ; 
                                   ALLTRIM(ST_FOUND.NAME),       ;
                                   QNT, ST_FOUND.QNT_ROUND,      ;
                                   ST_FOUND.QNT_PACK, ST_FOUND.PRICE)
    DO P_Offer
    IF .NOT. EMPTY(ST_FOUND.NOTE)
      DO View_Nt WITH "ST_FOUND"
    ENDIF
    what_do = "List"
    
  CASE what_do = "C_Seek"  && Контекстный поиск
  
    DO C_Seek
    what_do = "List"
    
  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_A")
RELEASE WINDOW (win_name+"_B")
SELECT STRANGE
SCAN
  SELECT LD
  GO STRANGE.RC_NO
  sum_list = sum_list+QNT*IIF(LIST_TIT.IS_USD, STRANGE.PRICE_D-LD.USD_PRICE, STRANGE.PRICE_R-LD.SAL_PRICE)
  REPLACE CODE      WITH STRANGE.CODE,     ;
          PREFIX    WITH STRANGE.PREFIX,   ;
          NAME      WITH STRANGE.NAME,     ;
          PRODUCER  WITH STRANGE.PRODUCER, ;
          AMODEL    WITH STRANGE.AMODEL,   ;
          FUNCTION  WITH STRANGE.FUNCTION, ;
          IS_SPEC   WITH "",               ;
          DEF_PRICE WITH STRANGE.PRICE_L,  ;
          USD_DEF   WITH STRANGE.PRICE_L_D,;
          QNT       WITH STRANGE.QNT,      ;
          SAL_PRICE WITH IIF(LIST_TIT.IS_USD, 0, STRANGE.PRICE_R),  ;
          USD_PRICE WITH IIF(LIST_TIT.IS_USD, STRANGE.PRICE_D, 0)
  SELECT STRANGE
  IF .NOT. EMPTY(CODE) .AND. mode_tch
    DO To_Teach
  ENDIF
ENDSCAN
SELECT STRANGE
USE
SELECT FOR_DUBL
USE
SELECT ST_SEEK
USE
SELECT TEACH
USE
SELECT ST_FOUND
USE
SELECT ST_LINK
USE
DELETE FILE (tmpo_path+"ST_LINK.DBF")
DELETE FILE (tmpo_path+"ST_LINK.CDX")
SELECT STOCK
SET ORDER TO (sto_ord)
IF BETWEEN(sto_rec, 1, RECCOUNT())
  GO sto_rec
ENDIF

IF BETWEEN(ld_rec, 1, RECCOUNT("LD"))
  GO ld_rec IN LD
ENDIF
SELECT (sel_sav)
RELEASE MENU (menu_name) EXTENDED
ACTIVATE WINDOW L_Det_M BOTTOM
@ WROWS()-2, 3 SAY STR(sum_list,12,2) COLOR SCHEME 1
DO U_Double WITH lc

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Коррекция рублевой цены.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 01.02.99 14:14:20 ═╝
PROCEDURE Corr_P
REPLACE PRICE_R WITH Round_6(PRICE_R)
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║               Поиск нескладской позиции в номенклатуре.                ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.05.97 14:14:20 ═╝
PROCEDURE Seek_Sto
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

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "Tab"
ON SELECTION PAD Choose OF (menu_name) Do Sw_Own WITH "{Tab}"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "Exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
PRIVATE sto_ord, sto_rec, prf, pic_w
pic_w = IIF(LIST_TIT.IS_USD, "@Z 99999.9999", "@Z 99999.99999")

SELECT STOCK
IF is_cor
  SET RELATION TO CODE INTO SUBJECT
ENDIF
sto_ord = SET("ORDER")
IF .NOT. EMPTY(sto_ord)
  sto_ord = SUBSTR(sto_ord, 5)
  sto_rec = AT(" ", sto_ord)
  sto_ord = IIF(sto_rec > 0, LEFT(sto_ord, sto_rec-1), sto_ord)
ENDIF
sto_rec = RECNO()
SET ORDER TO TAG NAME_P
prf = ""

DO WHILE LEN(prf) < FSIZE("NAME", "STRANGE")
  prf = LEFT(STRANGE.NAME,LEN(prf)+1)
  IF .NOT. SEEK(prf)
    prf = LEFT(prf,LEN(prf)-1)
    SEEK prf
    EXIT
  ENDIF
ENDDO

*
*   Расчет размеров окна
*
PRIVATE ln, wd, q_w
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = FSIZE("PREFIX")+25+FSIZE("PRODUCER")+IIF(LIST_TIT.IS_USD, 20, 21)+ ;
     IIF(is_cor, 15, 0)

DO D_Wins WITH ln, wd, ALLTRIM(STRANGE.PREFIX)+" "+  ;
               ALLTRIM(STRANGE.NAME)+" "+  ;
               ALLTRIM(STRANGE.PRODUCER),  ;
               1, 1
@ 2, 2 SAY PADC(" Наименование ───────────────",  ;
                FSIZE("PREFIX")+25+FSIZE("PRODUCER")+2,"─")+ ;
           IIF(is_cor, "───Корпус────", "")+" Кол-во ──── Цена "
@ WROWS()-3, 5 SAY "Полное наименование"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab DO Seek_

*
*    BROWSE - меню
*
    IF is_cor
      BROWSE FIELDS F001 = IIF(SUBJECT.DEP_COM = " ", " ", ;
                           IIF(SUBJECT.DEP_COM = "1", "▒", "█")):1:H="", ;
             PREFIX:H="",   ;
             Brw_Name=Nm_Part(NAME):25:H="",;
             PRODUCER:H="",  ;
             CORPUS.NAME:H="":12,       ;
             QNT:H="":P="@Z 999999",    ;
             F000 = IIF(LIST_TIT.IS_USD, PRICE, PRICE_R):H="":P=pic_w, ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Ft()    ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS PREFIX:H="", ;
             Brw_Name=Nm_Part(NAME):25:H="", ;
             PRODUCER:H="",  ;
             QNT:H="":P="@Z 999999",    ;
             F000 = IIF(LIST_TIT.IS_USD, PRICE, PRICE_R):H="":P=pic_w, ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Ft()    ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"
    PRIVATE rc_sav, nm_sav, prf_sav, prc_sav, rc_0
    rc_sav  = RECNO("STOCK")
    rc_0    = rc_sav
    nm_sav  = STOCK.NAME
    prf_sav = STOCK.PREFIX
    prc_sav = IIF(LIST_TIT.IS_USD, STOCK.PRICE, STOCK.PRICE_R)
    DO WHILE .T.
      SKIP IN STOCK
      IF EOF("STOCK")
        EXIT
      ENDIF
      IF STOCK.NAME # nm_sav .OR. STOCK.PREFIX # prf_sav
        EXIT
      ENDIF
      IF STOCK.QNT = 0 .OR. STOCK.PRICE_R = 0
        LOOP
      ENDIF
      IF IIF(LIST_TIT.IS_USD, STOCK.PRICE, STOCK.PRICE_R) < prc_sav .AND. STOCK.QNT # 0
        rc_sav  = RECNO("STOCK")
        prc_sav = IIF(LIST_TIT.IS_USD, STOCK.PRICE, STOCK.PRICE_R)
      ENDIF
    ENDDO
    IF rc_sav # rc_0
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"  Выберем лучшую? [ Да ] [ Нет ]"
      mss(3) = ""
      GO IIF(Out_Mess(7,"mss") = 1, rc_sav, rc_0) IN STOCK
    ELSE
      GO rc_0
    ENDIF
    IF STOCK.PRICE_R = 0
      WAIT WINDOW NOWAIT "ВНИМАНИЕ! Пустая цена!"
    ENDIF
    SELECT STRANGE
    IF SEEK(lc+STR(STOCK.CODE,7), "FOR_DUBL")
      PRIVATE mss0
      DIMENSION mss0(3)
      mss0(1) = ""
      mss0(2) = CHR(0)+"  Такая позиция уже есть в списке! Объединим? [ Да ] [ Нет ]"
      mss0(3) = ""
      IF Out_Mess(7, "mss0") = 2
        SELECT STOCK
        what_do = "List"
        LOOP
      ENDIF
    ENDIF
    REPLACE CODE     WITH STOCK.CODE,     ;
            PREFIX   WITH STOCK.PREFIX,   ;
            NAME     WITH STOCK.NAME,     ;
            PRO_CODE WITH STOCK.PRO_CODE, ;
            PRODUCER WITH STOCK.PRODUCER, ;
            PRICE_L  WITH STOCK.PRICE_R,  ;
            PRICE_L_D WITH STOCK.PRICE,   ;
            PRICE_R  WITH STOCK.PRICE_R,  ;
            PRICE_D  WITH STOCK.PRICE
    IF .NOT. EMPTY(STOCK.CODE)
      DO View_Nt WITH "STOCK"
    ENDIF
    q_w = QNT
*    IF q_w % MAX(1, STOCK.QNT_ROUND) # 0 .AND. q_w > 0
      q_w = Round_P(STOCK.PREFIX+" "+STOCK.NAME+" "+STOCK.PRODUCER, q_w, STOCK.QNT_ROUND, STOCK.QNT_PACK, STOCK.PRICE)
      REPLACE QNT WITH q_w
*    ENDIF
    SELECT STOCK
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO
IF is_cor
  SET RELATION TO
ENDIF

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")

SET ORDER TO (sto_ord)
IF BETWEEN(sto_rec, 1, RECCOUNT())
  GO sto_rec
ENDIF
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Программа поиска по начальным буквам.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.05.97 22:05:53 ═╝
PROCEDURE Seek_

PRIVATE s, ln, bg, rc_s, i, mss

ln = FSIZE("NAME", "STOCK")
ACTIVATE WINDOW (win_name+"_M") SAME
@ 1, 3 SAY "Поиск:"
bg = REPLICATE("▒", ln)
DO WHILE .T.
  @ 1, 10 SAY bg
  @ 1, 10 SAY prf
  s = INKEY(0)
*  i = ASCAN(int_1, s)
*  IF i # 0
*    KEYBOARD int_2(i)
*    EXIT
*  ENDIF
  IF s < 0     && Функциональная клавиша
    IF s = -9  && F10 - запускаем меню
      KEYBOARD "{F10}"
      RETURN
    ENDIF
    LOOP
  ENDIF
  s = CHR(s)
  IF s $ chr_shab .AND. LEN(prf) < ln   && Символ
    rc_s = RECNO()
    IF SEEK(prf+s)
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
    what_do = "Choose"
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
  IF s = CHR(27)       && Esc
    KEYBOARD CHR(27)
    what_do = "Exit"
    EXIT
  ENDIF
  IF s = CHR(127)      && BackSpace
    IF LEN(prf) > 0
      prf = LEFT(prf, LEN(prf)-1)
      SEEK prf
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
    LOOP
  ENDIF
ENDDO

@ 1, 3 SAY SPACE(ln+7)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Программа переключения в режим поиска.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 06.05.97 22:05:53 ═╝
PROCEDURE Sw_Own
PARAMETER k

KEYBOARD k

DEACTIVATE MENU (menu_name)

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Коррекция позиции.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.05.97 16:49:32 ═╝
PROCEDURE Corr_Pos
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

PRIVATE ex, prf_w, nm_w, pro_w, mod_w, func_w
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
prf_w   = PREFIX
nm_w    = NAME
pro_w   = PRODUCER
mod_w   = AMODEL
func_w  = FUNCTION
*------------------------------------------------------------------------
*
*┌─────────────────────────────────────────────┐
*│┌ Преф. ── Маркировка ──────────── Фирма ───┐│
*││  ▒▒▒▒ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓ ▒▒▒▒▓▒▒▒  ││
*│└───────────────────────────────────────────┘│
*│  Модель аппарата ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒   │
*│                                             │
*│Функц. назн. ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒│
*│                                             │
*│      < OK Ctrl-W > < Отказаться Esc >       │
*└─────────────────────────────────────────────┘
PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 11, 49, "Коррекция позиции"
DO Sun_Bord WITH 2, 2, 4, 46
@ 2, 3 SAY " Преф. "
@ 2,12 SAY " Маркировка "
@ 2,36 SAY " Фирма "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,10 GET nm_w
@ 3,36 GET pro_w
@ 5, 4 SAY "Модель аппарата" GET mod_w
@ 7, 2 SAY "Функц. назн." GET func_w
@ 9, 8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
@ 3, 5 GET prf_w PICTURE "@J"

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Выход из бланка и сохранение.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.05.97 17:10:40 ═╝
PROCEDURE Tst_Blank
PRIVATE mss, q_w

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF EMPTY(nm_w)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Маркировка не может быть пустой!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

PRIVATE sto_ord, sto_rec

SELECT STOCK
sto_ord = SET("ORDER")
IF .NOT. EMPTY(sto_ord)
  sto_ord = SUBSTR(sto_ord, 5)
  sto_rec = AT(" ", sto_ord)
  sto_ord = IIF(sto_rec > 0, LEFT(sto_ord, sto_rec-1), sto_ord)
ENDIF
sto_rec = RECNO()
SET ORDER TO TAG NAME_P

IF SEEK(nm_w+prf_w+pro_w)
  IF SEEK(lc+STR(STOCK.CODE,7), "FOR_DUBL")
    PRIVATE mss0
    DIMENSION mss0(3)
    mss0(1) = ""
    mss0(2) = CHR(0)+"  Такая позиция уже есть в списке! Объединим? [ Да ] [ Нет ]"
    mss0(3) = ""
    IF Out_Mess(7, "mss0") = 2
      SET ORDER TO (sto_ord)
      IF BETWEEN(sto_rec, 1, RECCOUNT())
        GO sto_rec
      ENDIF
      SELECT STRANGE
      RETURN .F.
    ENDIF
  ENDIF

  SELECT STRANGE
  REPLACE CODE     WITH STOCK.CODE,     ;
          PREFIX   WITH STOCK.PREFIX,   ;
          NAME     WITH STOCK.NAME,     ;
          PRO_CODE WITH STOCK.PRO_CODE, ;
          PRODUCER WITH STOCK.PRODUCER, ;
          PRICE_L  WITH STOCK.PRICE_R,  ;
          PRICE_L_D WITH STOCK.PRICE,   ;
          PRICE_R  WITH STOCK.PRICE_R,  ;
          PRICE_D  WITH STOCK.PRICE
  IF .NOT. EMPTY(STOCK.NOTE)
    DO View_Nt WITH "STOCK"
  ENDIF
  q_w = QNT
*  IF q_w % MAX(1, STOCK.QNT_ROUND) # 0 .AND. q_w > 0
    q_w = Round_P(STOCK.PREFIX+" "+STOCK.NAME+" "+STOCK.PRODUCER, q_w, STOCK.QNT_ROUND, STOCK.QNT_PACK, STOCK.PRICE)
    REPLACE QNT WITH q_w
*  ENDIF
  
ELSE
  SELECT STRANGE
  REPLACE CODE     WITH 0,     ;
          PREFIX   WITH prf_w, ;
          NAME     WITH nm_w,  ;
          PRODUCER WITH pro_w, ;
          AMODEL   WITH mod_w, ;
          FUNCTION WITH func_w

ENDIF
SELECT STOCK
SET ORDER TO (sto_ord)
IF BETWEEN(sto_rec, 1, RECCOUNT())
  GO sto_rec
ENDIF

SELECT STRANGE

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Не стала ли позиция складской.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.05.97 17:10:40 ═╝
PROCEDURE Is_Sto
PRIVATE rc_sav, rc_0, nm_sav, prc_sav, q_w

IF SEEK(NAME+PREFIX+PRODUCER, "STOCK")
  IF CODE # STOCK.CODE
    PRIVATE rc_sav, nm_sav, prf_sav, prc_sav, rc_0
    rc_sav  = RECNO("STOCK")
    rc_0    = rc_sav
    nm_sav  = STOCK.NAME
    prf_sav = STOCK.PREFIX
    prc_sav = IIF(LIST_TIT.IS_USD, STOCK.PRICE, STOCK.PRICE_R)
    DO WHILE .T.
      SKIP IN STOCK
      IF EOF("STOCK")
        EXIT
      ENDIF
      IF STOCK.NAME # nm_sav .OR. STOCK.PREFIX # prf_sav
        EXIT
      ENDIF
      IF STOCK.QNT = 0 .OR. STOCK.PRICE_R = 0
        LOOP
      ENDIF
      IF IIF(LIST_TIT.IS_USD, STOCK.PRICE, STOCK.PRICE_R) < prc_sav .AND. STOCK.QNT # 0
        rc_sav  = RECNO("STOCK")
        prc_sav = STOCK.PRICE_R
      ENDIF
    ENDDO
    IF rc_sav # rc_0
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"  Выберем лучшую? [ Да ] [ Нет ]"
      mss(3) = ""
      GO IIF(Out_Mess(7,"mss") = 1, rc_sav, rc_0) IN STOCK
    ELSE
      GO rc_0 IN STOCK
    ENDIF
    IF SEEK(lc+STR(STOCK.CODE,7), "FOR_DUBL")
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Внимание! Такая позия уже есть в списке строки будут объединены!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ENDIF
    REPLACE CODE     WITH STOCK.CODE,    ;
            PRODUCER WITH STOCK.PRODUCER,;
            PRICE_L  WITH STOCK.PRICE_R, ;
            PRICE_L_D WITH STOCK.PRICE,  ;
            PRICE_R  WITH STOCK.PRICE_R, ;
            PRICE_D  WITH STOCK.PRICE
  ENDIF
  q_w = QNT
*  IF q_w % MAX(1, STOCK.QNT_ROUND) # 0 .AND. q_w > 0
    q_w = Round_P(STOCK.PREFIX+" "+STOCK.NAME+" "+STOCK.PRODUCER, q_w, STOCK.QNT_ROUND, STOCK.QNT_PACK, STOCK.PRICE)
    REPLACE QNT WITH q_w
*  ENDIF
ELSE
  IF CODE # 0
    REPLACE CODE WITH 0
  ENDIF
ENDIF

DO P_Offer

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                             Делим позицию!                             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.05.97 17:10:40 ═╝
PROCEDURE Div_Pos
PRIVATE p, s1, s2

p = COL()-7
IF p < 1 .OR. p > 4
  RETURN
ENDIF

s1 = ALLTRIM(LEFT(NAME,p))
s2 = ALLTRIM(SUBSTR(NAME,p+1))
REPLACE PREFIX WITH PADL(s1, 4), NAME WITH s2
SHOW WINDOW (ALIAS()) REFRESH
DO Is_Sto

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                  Пропускаем символы до числовой группы!                ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.05.97 17:10:40 ═╝
PROCEDURE S_Let
PRIVATE s, i, st

st = ""
FOR i = 1 TO 4
  s = SUBSTR(NAME,i,1)
  IF ISDIGIT(s) .OR. EMPTY(s)
    EXIT
  ENDIF
  st = st+"{RightArrow}"
ENDFOR

KEYBOARD st

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                  Встаем в поле имени.                                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.05.97 17:10:40 ═╝
PROCEDURE Jmp_N
PRIVATE s, v

v = ""
s = UPPER(VARREAD())
DO CASE
CASE s == "PREFIX" .OR. s = "F000"
  v = CHR(13)
CASE s == "PRODUCER"
  v = CHR(13)+CHR(13)+CHR(13)
CASE s == "PRICE_R" .OR. s == "PRICE_D"
  v = CHR(13)+CHR(13)
ENDCASE
KEYBOARD v

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура C_Seek       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Контекстный поиск.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.01.2000 ──┘
PROCEDURE C_Seek
PARAMETERS nm

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
*│  ┌─ Шаблон для поиска ───────┐  │
*│  │ ....:....!....:....!....: │  │
*│  └───────────────────────────┘  │
*│< OK Ctrl-W > < Отказаться Esc > │
*└─────────────────────────────────┘

PRIVATE ex, nm_w, rc_w
ex = 1
nm_w = PADR(NAME,25)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 37, "Контекстный поиск"
DO Sun_Bord WITH  2,  4,  4, 32, " Шаблон для поиска "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3, 6 GET nm_w
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE
POP KEY
RELEASE WINDOW (win_name)

nm_w = ALLTRIM(nm_w)
IF ex = 1 .AND. .NOT. EMPTY(nm_w)
  *
  * Отрабатываем бланк
  DO Ch_Pos
ENDIF
*--------------------------------------------------------------------------

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Pos       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                 Формирование списка позиций и выбор.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.01.2000 ──┘
PROCEDURE Ch_Pos


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
ZAP
SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME ALIAS ST_0118 AGAIN
DO Wt_Mess WITH "Поиск..."
SCAN FOR nm_w $ NAME
  SELECT LINK
  APPEND BLANK
  REPLACE LNK WITH RECNO("ST_0118")
  SELECT ST_0118
ENDSCAN
DO Wt_Mess
SET ORDER TO
SELECT LINK
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего такого нет!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT ST_0118
  USE
  SELECT (s_sav)
  RETURN
ENDIF
SET RELATION TO LNK INTO ST_0118
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
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 49

DO D_Wins WITH ln, wd, "Результат поиска...", 0, 0
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
WAIT WIND "!!!!!"
    BROWSE FIELDS ST_0118.PREFIX:H="",   ;
           ST_0118.NAME:H="", ;
           ST_0118.PRODUCER:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    IF ST_0118.PRICE_R = 0
      WAIT WINDOW NOWAIT "ВНИМАНИЕ! Пустая цена!"
    ENDIF
    SELECT STRANGE
    IF SEEK(lc+STR(ST_0118.CODE,7), "FOR_DUBL")
      PRIVATE mss0
      DIMENSION mss0(3)
      mss0(1) = ""
      mss0(2) = CHR(0)+"  Такая позиция уже есть в списке!"
      mss0(3) = ""
      DO Out_Mess WITH 7, "mss0"
      SELECT LINK
      what_do = "List"
      LOOP
    ENDIF
    REPLACE CODE     WITH ST_0118.CODE,     ;
            PREFIX   WITH ST_0118.PREFIX,   ;
            NAME     WITH ST_0118.NAME,     ;
            PRO_CODE WITH ST_0118.PRO_CODE, ;
            PRODUCER WITH ST_0118.PRODUCER, ;
            PRICE_L  WITH ST_0118.PRICE_R,  ;
            PRICE_L_D WITH ST_0118.PRICE,   ;
            PRICE_R  WITH ST_0118.PRICE_R,  ;
            PRICE_D  WITH ST_0118.PRICE
    q_w = QNT
    q_w = Round_P(ST_0118.PREFIX+" "+ST_0118.NAME+" "+ST_0118.PRODUCER, q_w, STOCK.QNT_ROUND, STOCK.QNT_PACK, ST_0118.PRICE)
    REPLACE QNT WITH q_w
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
SELECT ST_0118
USE
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура P_Offer      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Распознавание...                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.02.2000 ──┘
PROCEDURE P_Offer

PRIVATE arrLink, s_sav, i
DIMENSION arrLink(1,2)

IF BOF("STRANGE") .OR. EOF("STRANGE")
  EXIT
ENDIF

s_sav = SELECT()

DO Link_Pos WITH STRANGE.PREFIX, STRANGE.NAME, STRANGE.PRODUCER, "ST_SEEK"

SELECT ST_LINK
DO WHILE SEEK(STR(RECNO("STRANGE")))
  REPLACE STRNG_REC WITH 0
ENDDO

FOR i = 2 TO ALEN(arrLink,1)
  GO TOP
  IF .NOT. EMPTY(STRNG_REC) .OR. RECCOUNT() = 0
    APPEND BLANK
  ENDIF
  REPLACE STRNG_REC WITH RECNO("STRANGE"), ;
          WGT       WITH arrLink(i,2),    ;
          CODE      WITH arrLink(i,1)
ENDFOR

SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Sw_Wind      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Переключение окон.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.02.2000 ──┘
PROCEDURE Sw_Wind

IF WOUTPUT() = UPPER(win_name+"_E")
  IF .NOT. FOUND("ST_LINK")
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Посмотрите внимательно, ничего не удалось подобрать!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN
  ENDIF
  SELECT ST_LINK
&&  ACTIVATE WINDOW (win_name+"_A") SAME
&&  ACTIVATE WINDOW ST_LINK SAME
ELSE
  SELECT STAGE
  ACTIVATE WINDOW STRANGE SAME
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Old_Names    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Переименования.                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.02.2000 ──┘
PROCEDURE Old_Names
PRIVATE rc, rc_sav, mss, sel_sav
rc = Renames(ALLTRIM(NAME))
sel_sav = SELECT()
IF rc = 0
  RETURN
ENDIF
rs_sav = RECNO("ST_FOUND")
= SEEK(rc, "ST_FOUND")
IF .NOT. FOUND("ST_FOUND") .OR. .NOT. EMPTY(ST_FOUND.DATE_OFF)
  IF BETWEEN(rc_sav, 1, RECCOUNT("ST_FOUND"))
    GO rc_sav IN ST_FOUND
  ENDIF
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы нашли что-то очень древнее..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
PRIVATE nm_sav, prf_sav, prc_sav, rc_0
IF ST_FOUND.PRICE_R = 0
  WAIT WINDOW NOWAIT "ВНИМАНИЕ! Пустая цена!"
ENDIF
SELECT STRANGE
IF SEEK(lc+STR(ST_FOUND.CODE,7), "FOR_DUBL")
  PRIVATE mss0
  DIMENSION mss0(3)
  mss0(1) = ""
  mss0(2) = CHR(0)+"  Такая позиция уже есть в списке! Объединим? [ Да ] [ Нет ]"
  mss0(3) = ""
  IF Out_Mess(7, "mss0") = 2
    SELECT (sel_sav)
    what_do = "List"
    LOOP
  ENDIF
ENDIF
REPLACE CODE     WITH ST_FOUND.CODE,     ;
        PREFIX   WITH ST_FOUND.PREFIX,   ;
        NAME     WITH ST_FOUND.NAME,     ;
        PRO_CODE WITH ST_FOUND.PRO_CODE, ;
        PRODUCER WITH ST_FOUND.PRODUCER, ;
        PRICE_L  WITH ST_FOUND.PRICE_R,  ;
        PRICE_L_D WITH ST_FOUND.PRICE,   ;
        PRICE_R  WITH ST_FOUND.PRICE_R,  ;
        PRICE_D  WITH ST_FOUND.PRICE
q_w = QNT
*    IF q_w % MAX(1, STOCK.QNT_ROUND) # 0 .AND. q_w > 0
q_w = Round_P(ST_FOUND.PREFIX+" "+ST_FOUND.NAME+" "+ST_FOUND.PRODUCER, q_w, ST_FOUND.QNT_ROUND, ST_FOUND.QNT_PACK, ST_FOUND.PRICE)
REPLACE QNT WITH q_w
*    ENDIF
SELECT (sel_sav)

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура To_Teach     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                              Обучение...                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 17.11.2000 ──┘
PROCEDURE To_Teach

IF SEEK(STR(LIST_TIT.CLI_CODE)+STRANGE.S_NAME+STRANGE.S_PREF+STRANGE.S_PROD, "TEACH")
  RETURN
ENDIF
PRIVATE s_sav
s_sav = SELECT()
SELECT TEACH
IF .NOT. SEEK("         0")
  APPEND BLANK
ENDIF
REPLACE CUS_CODE WITH LIST_TIT.CLI_CODE, ;
        PREFIX   WITH STRANGE.S_PREF, ;
        NAME     WITH STRANGE.S_NAME, ;
        PRODUCER WITH STRANGE.S_PROD, ;
        CODE     WITH STRANGE.CODE,   ;
        LAST_ORD WITH DATE(),      ;
        WHO      WITH user,        ;
        WHEN     WITH DATE()
SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Cus_Sto      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Поиск в номенклатуре клиента.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 17.11.2000 ──┘
PROCEDURE Cus_Sto

PRIVATE s_sav, r_sav

s_sav = SELECT()
SELECT 0
DELETE FILE (tmpo_path+"CUS_STO.DBF")
DELETE FILE (tmpo_path+"CUS_STO.CDX")
CREATE DBF (tmpo_path+"CUS_STO") ;
  ( REFUSE   C( 1),  ;
    LINK_S   N(10),  ;
    LINK_T   N(10),  ;
    CUS_PREF C( 4),  ;
    CUS_NAME C(35),  ;
    CUS_PROD C( 8),  ;
    QNT      N( 8),  ;
    CODE     N( 7),  ;
    PREFIX   C( 4),  ;
    NAME     C(35),  ;
    PRODUCER C( 8),  ;
    QNT_STO  N( 8),  ;
    WHEN     D,      ;
    WHO      C(28)   )
INDEX ON CUS_NAME+CUS_PREF+CUS_PROD TAG NAME

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_0B17 AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS WHO_0B17 AGAIN

SELECT STRANGE
r_sav = RECNO()
SCAN FOR SEEK(STR(LIST_TIT.CLI_CODE)+S_NAME+S_PREF+S_PROD, "TEACH")
  = SEEK(TEACH.CODE, "STO_0B17")
  = SEEK(TEACH.WHO,  "WHO_0B17")
  SELECT CUS_STO
  APPEND BLANK
  REPLACE CUS_PREF WITH TEACH.PREFIX,     ;
          LINK_S   WITH RECNO("STRANGE"), ;
          LINK_T   WITH RECNO("TEACH"),   ;
          CUS_NAME WITH TEACH.NAME,       ;
          CUS_PROD WITH TEACH.PRODUCER,   ;
          QNT      WITH STRANGE.QNT,      ;
          CODE     WITH TEACH.CODE,       ;
          PREFIX   WITH STO_0B17.PREFIX,  ;
          NAME     WITH STO_0B17.NAME,    ;
          PRODUCER WITH STO_0B17.PRODUCER,;
          QNT_STO  WITH STO_0B17.QNT,     ;
          WHEN     WITH TEACH.WHEN,       ;
          WHO      WITH ALLTRIM(WHO_0B17.FAMILY)+" "+ ;
                        LEFT(WHO_0B17.NAME,1)+" "+    ;
                        LEFT(WHO_0B17.S_NAME,1)
  SELECT STRANGE
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUN())
  GO r_sav
ENDIF
SELECT WHO_0B17
USE
IF RECCOUNT("CUS_STO") = 0
  PRIVATE messg
  DIMENSION messg(3)
  messg(1) = ""
  messg(2) = CHR(0)+"Ничего не удалось подобрать!"
  messg(3) = ""
  DO Out_Mess WITH 7, "messg"
  SELECT STO_0B17
  USE
  SELECT CUS_STO
  USE
  DELETE FILE (tmpo_path+"CUS_STO.DBF")
  DELETE FILE (tmpo_path+"CUS_STO.CDX")
  SELECT (s_sav)
  RETURN
ENDIF
  
IF View_Ch()
  SELECT CUS_STO
  SCAN
    IF EMPTY(REFUSE)
      SELECT STRANGE
      GO CUS_STO.LINK_S
      = SEEK(CUS_STO.CODE, "STO_0B17")
      REPLACE CODE      WITH CUS_STO.CODE,     ;
              PREFIX    WITH STO_0B17.PREFIX,  ;
              NAME      WITH STO_0B17.NAME,    ;
              PRO_CODE  WITH STO_0B17.PRO_CODE,;
              PRODUCER  WITH STO_0B17.PRODUCER,;
              PRICE_L   WITH STO_0B17.PRICE_R, ;
              PRICE_L_D WITH STO_0B17.PRICE,   ;
              PRICE_R   WITH STO_0B17.PRICE_R, ;
              PRICE_D   WITH STO_0B17.PRICE,   ;
              QNT       WITH Round_P(ALLTRIM(STO_0B17.PREFIX)+" "+ ; 
                                     ALLTRIM(STO_0B17.NAME),       ;
                                     QNT, STO_0B17.QNT_ROUND,      ;
                                     STO_0B17.QNT_PACK, STO_0B17.PRICE)
      IF .NOT. EMPTY(STO_0B17.NOTE)
        DO View_Nt WITH "STO_0B17"
      ENDIF
    ELSE
      SELECT TEACH
      GO CUS_STO.LINK_T
      REPLACE CUS_CODE WITH 0
      SELECT CUS_STO
    ENDIF
    SELECT CUS_STO
  ENDSCAN
ENDIF

SELECT STO_0B17
USE
SELECT CUS_STO
USE
DELETE FILE (tmpo_path+"CUS_STO.DBF")
DELETE FILE (tmpo_path+"CUS_STO.CDX")
SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_Ch      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│         Просмотр позиций, выбранных из номенклатуры клиента            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 17.11.2000 ──┘
PROCEDURE View_Ch

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
SELECT CUS_STO

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Исключить/Вставить" KEY Ctrl-A, "Tab"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Save OF (menu_name) PROMPT "Сохранить" KEY Ctrl-A, "Enter"
ON SELECTION PAD Save OF (menu_name) DO Sw_Mode WITH "Save"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 65
*┌────────Наименование клиента──────────Наименование───────────────┐
*│X .... ....:....!....:....!....: .... ....:....!....:....!....: V│
*└─────────────────────────────────────────────────────────────────┘
*┌─────────────Фирма───Кол-во─────────────────Фирма───Кол-во─────┐
*│            XXXXXXXX                       XXXXXXXX            │
*│ Позиция занесена ДД.ММ.ГГГГ ....:....!....:....!....:....:... │
*└───────────────────────────────────────────────────────────────┘

DO D_Wins WITH ln, wd, "Номенклатура клиента", 0, 2
@ 1,  2 SAY "────────Наименование клиента──────────Наименование"
@ WROWS()-4, 2 SAY "─────────────Фирма───Кол-во─────────────────Фирма───Кол-во"
@ WROWS()-2, 3 SAY "Позиция занесена"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Choose"

*
*    BROWSE - меню
*
    BROWSE FIELDS REFUSE:H="",   ;
                  CUS_PREF:H="", ;
                  CUS_NAME:H="", ;
                  PREFIX:H="",   ;
                  NAME:H="",     ;
                  DEFF=IIF(QNT > QNT_STO, "√", " "):1:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Save")
    ENDIF

  CASE what_do = "Choose"    && Инвертировать пометку

    IF EMPTY(REFUSE)
      REPLACE REFUSE WITH "X"
    ELSE
      REPLACE REFUSE WITH " "
    ENDIF
    what_do = "List"

  CASE what_do = "Save"      && Сохранить замены

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
SELECT (s_sav)

RETURN what_do = "Save"

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*

ACTIVATE WINDOW (ALIAS()) SAME

@ WROWS()-3, 14 SAY CUS_PROD+STR(QNT, 9)
@ WROWS()-3, 45 SAY PRODUCER+STR(QNT_STO, 9)
@ WROWS()-2, 20 SAY WHEN
@ WROWS()-2, COL()+1 SAY WHO

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Sh_Change    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Контекстная замена.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.11.2000 ──┘
PROCEDURE Sh_Change
PRIVATE shab1, shab2, r_sav, s_sav, to_ask, tmpSw

s_sav = SELECT()
to_ask = .T.

STORE "" TO shab1, shab2

DO G_Shabs

IF LEN(shab1) = 0
  RETURN
ENDIF

SELECT STRANGE
r_sav = RECNO()
SCAN
  IF .NOT. shab1 $ PREFIX .AND. ;
     .NOT. shab1 $ NAME   .AND. ;
     .NOT. shab1 $ PRODUCER
    LOOP
  ENDIF
  IF to_ask
    tmpSw = To_Change()
    DO CASE
    CASE tmpSw = 2
      LOOP
    CASE tmpSw = 3
      to_ask = .F.
    CASE tmpSw = 4
      EXIT
    ENDCASE
  ENDIF
  REPLACE PREFIX   WITH STRTRAN(PREFIX, shab1, shab2), ;
          NAME     WITH STRTRAN(NAME, shab1, shab2),   ;
          PRODUCER WITH STRTRAN(PRODUCER, shab1, shab2)
  IF SEEK(NAME+PREFIX+PRODUCER, "STOCK")
    REPLACE CODE WITH STOCK.CODE
    IF .NOT. EMPTY(STOCK.NOTE)
      DO View_Nt WITH "STOCK"
    ENDIF
  ELSE
    REPLACE CODE WITH 0
  ENDIF
ENDSCAN
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Shabs      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Шаблоны для контекстной замены.                     │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.11.2000 ──┘
PROCEDURE G_Shabs
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

*┌───────────────────────────────────────────────────────┐
*│ Заменить xxxxXxxxxXxxxxXxxxxX на xxxxXxxxxXxxxxXxxxxX │
*│           < OK Ctrl-W > < Отказаться Esc >            │
*└───────────────────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 5, 59, "Контекстная замена"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2,  3 SAY "Заменить"
@ 2, 33 SAY "на"
@ 2, 12 EDIT shab1 SIZE 1, 20 COLOR (SCHEME(1,1))
@ 2, 36 EDIT shab2 SIZE 1, 20 COLOR (SCHEME(1,1))
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 2
  shab1 = ""
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура To_Change    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                                Меняем?                                 │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.04.2001 ──┘
PROCEDURE To_Change
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

*┌───────────────────────────────────────────────────────────────────────────┐
*│         Исходное название XXXX xxxxXxxxxXxxxxXxxxxXxxxxX xxxxXxxx         │
*│                                                                           │
*│         Новое название    XXXX xxxxXxxxxXxxxxXxxxxXxxxxX xxxxXxxx         │
*│                                                                           │
*│ < Заменить > < Пропустить > < Заменить для всех > < Пропустить для всех > │
*└───────────────────────────────────────────────────────────────────────────┘

PRIVATE ex, new_prf, new_nam, new_prd
ex = 1
new_prf = STRTRAN(PREFIX, shab1, shab2)
new_nam = STRTRAN(NAME, shab1, shab2)
new_prd = STRTRAN(PRODUCER, shab1, shab2)

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 79, "Контекстная замена"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 11 SAY "Исходное название"
@ 4, 11 SAY "Новое название"
@ 2, 29 GET PREFIX WHEN .F.
@ 2, COL()+1 GET NAME WHEN .F.
@ 2, COL()+1 GET PRODUCER WHEN .F.
@ 4, 29 GET new_prf WHEN .F.
@ 4, COL()+1 GET new_nam WHEN .F.
@ 4, COL()+1 GET new_prd WHEN .F.

@ 6, 3 GET ex PICTURE "@*HT \ Заменить ;\ Пропустить ;\ Заменить для всех ;\ Пропустить для всех "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Ft      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Ft

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-2, 2 SAY PREFIX COLOR SCHEME 1
@ ROW(), COL()+1 SAY NAME COLOR SCHEME 1
@ ROW(), COL()+1 SAY PRODUCER COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
