*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа бронирования оплаченных списков.             ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE List_Ord
PRIVATE arm_w, ln, wd, i

WAIT WIND "Операция отменена" NOWAIT
RETURN

IF acc_level > 1
  arm_w = arm
ELSE
  USE (base_path+"ARM_LST")
  PRIVATE arm_nam, arm_cod, arm_ln
  arm_ln = 3
  DIMENSION arm_nam(3), arm_cod(3)
  arm_nam(1) = PADR("Свои списки", FSIZE("ARM_N"))
  arm_cod(1) = arm
  arm_nam(2) = PADR("Все списки", FSIZE("ARM_N"))
  arm_cod(2) = ""
  arm_nam(3) = PADR("Общая область", FSIZE("ARM_N"))
  arm_cod(3) = "***"
  SCAN ALL FOR ARM_C # arm
    arm_ln = arm_ln+1
    DIMENSION arm_nam(arm_ln), arm_cod(arm_ln)
    arm_nam(arm_ln) = ARM_N
    arm_cod(arm_ln) = ARM_C
  ENDSCAN
  USE
  ln = MIN(arm_ln+3, WROWS("")-5)
  wd = LEN(arm_nam(1))+4
  DO D_W WITH "Arms", ln, wd, "Что хотим видеть:", 0
  arm_w = 1
  DO WHILE .T.
    arm_w = MIN(arm_w, arm_ln)
    arm_w = MAX(arm_w, 1)
    @ 1,1 MENU arm_nam, arm_ln, WROWS()-3
    READ MENU TO arm_w
    i = READKEY() % 256
    IF i = 12
      RELEASE WINDOW Arms
      RETURN
    ENDIF
    IF i = 15
      RELEASE WINDOW Arms
      arm_w = arm_cod(arm_w)
      EXIT
    ENDIF
  ENDDO
ENDIF

IF File_O(tmpo_path+"LINK_O.DBF")
  DELETE FILE (tmpo_path+"LINK_O.DBF")
ENDIF

CREATE DBF (tmpo_path+"LINK_O")  ;
       ( L_NUM   C ( 6),         ;
         S_STAT  C ( 6),         ;
         P_STAT  C ( 6),         ;
         B_STAT  C ( 6),         ;
         CUS_NAME C(50)   )

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG L_NAME
SET RELATION TO CODE INTO STOCK

SELECT 0
USE (base_path+"BILLS") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"LIST_TIT")
IF LEN(arm_w) = 0
  SET ORDER TO TAG LST_NUM
ELSE
  SET ORDER TO TAG LST_ARM
ENDIF

SET RELATION TO STR(LST_NUM,6) INTO LIST_DET,  ;
                CLI_CODE       INTO ACCOUNT,   ;
                FIRM+BILL_YEAR+BILL_NUM INTO BILLS, ;
                STR(CLI_CODE)+FIRM INTO CUS_REST

IF LEN(arm_w) = 0
  GO TOP
ELSE
  SEEK arm_w
ENDIF
PRIVATE s_txt, p_txt, b_txt, sm0, sm_d, sm_cr, sm_d_USD, sm_cr_USD, is_df
PRIVATE b_exi, b_hole

DO Wt_Mess WITH "Готовим списки!"

SCAN REST WHILE LST_ARM = arm_w
  *
  *    Исключаем списки, которые нас не интересуют...
  *
  IF .NOT. EMPTY(SAL_NUM)  && Список уже продан
    LOOP
  ENDIF
  
  sm_d  = 0                && Определяем размер депозита и кредита
  sm_cr = 0
  sm_d_USD  = 0
  sm_cr_USD = 0

  IF FOUND("CUS_REST")
    sm_d  = CUS_REST.ACC_SUM
    sm_cr = CUS_REST.CR_SUM
    sm_d_USD  = CUS_REST.ACC_SUMD
    sm_cr_USD = CUS_REST.CRD_SUM
  ENDIF

  IF R_TOTAL > IIF(IS_USD,  ;
                sm_d_USD+sm_cr_USD, sm_d+sm_cr) && Нет или не хватает депозита и кредита
    IF .NOT. FOUND("BILLS")   && Счета тоже нет или...
      LOOP
    ENDIF
    IF EMPTY(BILLS.STATUS)    &&     никаких известий об оплате
      LOOP
    ENDIF
  ENDIF
  *
  *   Далее просмотрим список...
  *
  is_df = .F.    && Признак наличия дефицита ( в т. ч. нескладских прозиций )
  b_exi = .F.    && Признак брони
  b_hole= .T.    && Признак полноты брони
  sm0   = 0      && Заодно уточним сумму - не помешает...
  SELECT LIST_DET
  SCAN REST WHILE STR(LIST_TIT.LST_NUM,6) = LIST_NUM
    IF CODE = 0     && Нескладская
      is_df = .T.
      EXIT
    ENDIF
    IF STOCK.QNT < QNT-QNT_REQ .AND. QNT-QNT_REQ > 0  && Дефицит
      is_df = .T.
      EXIT
    ENDIF
    IF QNT_REQ # 0
      b_exi = .T.
    ENDIF
    IF QNT_REQ # QNT
      b_hole = .F.
    ENDIF
    sm0 = sm0+QNT*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE)
  ENDSCAN
  SELECT LIST_TIT
  IF is_df
    LOOP
  ENDIF
  IF R_TOTAL # sm0
    REPLACE R_TOTAL WITH sm0
  ENDIF
  IF b_hole    && Все уже забронировано...
    LOOP
  ENDIF
  s_txt = "      "    && Счет
  IF FOUND("BILLS")
    s_txt = LEFT(BILLS.STATUS,6)
    IF EMPTY(s_txt)
      s_txt = "Вып.  "
    ENDIF
  ENDIF
  p_txt = "      "    && Наличие денег на депозите
  IF LIST_TIT.IS_USD
    IF sm_d >= sm0
      p_txt = "Деп.  "
    ELSE
      IF sm_d+sm_cr >= sm0
        p_txt = "Деп+Кр"
      ENDIF
    ENDIF
  ELSE
    IF sm_d_USD >= sm0
      p_txt = "Деп.  "
    ELSE
      IF sm_d_USD+sm_cr_USD >= sm0
        p_txt = "Деп+Кр"
      ENDIF
    ENDIF
  ENDIF
  b_txt = IIF(b_exi, "Част. ", "      ")  && Бронь

  SELECT LINK_O
  APPEND BLANK
  REPLACE L_NUM  WITH STR(LIST_TIT.LST_NUM,6), ;
          S_STAT WITH s_txt, ;
          P_STAT WITH p_txt, ;
          B_STAT WITH b_txt, ;
          CUS_NAME WITH IIF(FOUND("ACCOUNT"), ACCOUNT.CUS_NAME, " ")
  SELECT LIST_TIT

ENDSCAN

DO Wt_Mess

SET ORDER TO TAG LST_NUM
DO Use_Dummy
SELECT BILLS
USE (base_path+"ARM_LST") ORDER TAG ARM_C
SELECT LIST_TIT
SET RELATION TO LST_ARM  INTO ARM_LST,  ;
                STR(CLI_CODE)+FIRM INTO CUS_REST, ;
                CLI_CODE INTO ACCOUNT
SELECT LINK_O
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет долгов по брони..."
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"LINK_O.DBF")
  RETURN
ENDIF

SET RELATION TO VAL(ALLTRIM(L_NUM)) INTO LIST_TIT

*
*    PAD - меню первого уровня
*
DEFINE MENU List_M IN SCREEN COLOR SCHEME 4
DEFINE PAD View OF List_M PROMPT "Просмотр" KEY Ctrl-A, "Enter"
DEFINE PAD Ord  OF List_M PROMPT "Заявка" KEY Ctrl-A, "F2"
DEFINE PAD Cash OF List_M PROMPT "Депозиты" KEY Ctrl-A, "F1"
DEFINE PAD Exit OF List_M PROMPT "Выход" KEY Ctrl-A, "Esc"

ON SELECTION PAD View OF List_M DO Ch_Mode WITH "List_M","View"
ON SELECTION PAD Ord  OF List_M DO Ch_Mode WITH "List_M","Order"
ON SELECTION PAD Cash OF List_M DO Ch_Mode WITH "List_M","Cash"
ON SELECTION PAD Exit OF List_M DO Ch_Mode WITH "List_M","Exit"

*
*    PAD - меню второго уровня
*
DEFINE MENU View_M IN SCREEN COLOR SCHEME 4
DEFINE PAD Exit OF View_M PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF View_M DO Ch_Mode WITH "View_M","List"

PRIVATE ln, wd, tt, ft, pic_0, pic_1, pic_2

DIMENSION tt(1)

tt(1) = PADC("Наименование───────", FSIZE("PREFIX","LIST_DET")+FSIZE("NAME","LIST_DET")+1,"─")+"─"+  ;
            +PADC("Кол-во", FSIZE("QNT","LIST_DET"),"─")+"─"+  ;
            +PADC("Цена", FSIZE("SAL_PRICE","LIST_DET"),"─")+"─"+  ;
            +PADC("──Сумма", FSIZE("SAL_PRICE","LIST_DET")+4,"──")

*            +PADC("Дата", IIF(SET("CENTURY") = "ON", 10, 8),"─")+"─"+  ;
*

pic_0 = REPLICATE("9",FSIZE("QNT","LIST_DET"))
pic_1 = "@Z "+REPLICATE("9",FSIZE("SAL_PRICE","LIST_DET")-3)+".99"
pic_2 = "@Z "+REPLICATE("9",FSIZE("SAL_PRICE","LIST_DET")+1)+".99"

ln = WROWS("") - 7
wd = LEN(tt(1))
DO Def_Win WITH "View_M", "View_E", "View_I", ln, wd, "", tt
HIDE WINDOW View_M

DIMENSION tt(1), ft(4)

tt(1) = PADC("N сп.", FSIZE("L_NUM"),"─")+"─"+  ;
        PADC("Имя списка────", FSIZE("LST_NAME","LIST_TIT"),"─")+"─"+  ;
        PADC("──Сумма", FSIZE("R_TOTAL","LIST_TIT"),"───")+"─"+  ;
        PADC("Счет",  FSIZE("S_STAT"),"─")+"─"+  ;
        PADC("Оплата", FSIZE("P_STAT"),"─")+"─"+  ;
        PADC("Бронь", FSIZE("B_STAT"),"─")+"─П"

*            +PADC("Дата", IIF(SET("CENTURY") = "ON", 10, 8),"─")+"─"+  ;
*

ft(1) = ""
ft(2) = PADR("────Покупатель",FSIZE("CUS_NAME","ACCOUNT"),"─")+"─"+ ;
        PADR("Раб. место",FSIZE("ARM_N","ARM_LST"),"─")
ft(2) = PADR(ft(2),LEN(tt(1)),"─")
ft(3) = ""
ft(4) = REPLICATE("─",LEN(tt(1)))


ln = WROWS("") - 10
wd = LEN(tt(1))
DO Def_Win WITH "List_M", "List_E", "List_I", ln, wd, ;
                "Списки для бронирования", tt, ft
ACTIVATE WINDOW List_M
@ WROWS()-3, 1 SAY "┌"
@ WROWS()-3, WCOLS()-2 SAY "┐"
@ WROWS()-2, 1 SAY "│"
@ WROWS()-2, WCOLS()-2 SAY "│"
@ WROWS()-1, 1 SAY "└"
@ WROWS()-1, WCOLS()-2 SAY "┘"

what_do = "List"
DO WHILE .T.
  DO CASE
  CASE what_do = "List"     && Списки
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F2    DO Ch_Mode WITH "View_M","Order"
    ON KEY LABEL F1    DO Ch_Mode WITH "View_M","Cash"
    ON KEY LABEL F10 ACTIVATE MENU List_M
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("Меню F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU List_M
    ACTIVATE WINDOW List_E
    BROWSE FIELDS L_NUM:H="",             ;
                  LIST_TIT.LST_NAME:H="", ;
                  LIST_TIT.R_TOTAL:H="",  ;
                  M_TYPE = IIF(LIST_TIT.IS_USD, "$", "Р"):H="":1, ;
                  S_STAT:H="",            ;
                  P_STAT:H="",            ;
                  B_STAT:H="",            ;
                  F001 = IIF(EMPTY(LIST_TIT.MAIL), " ", "√"):H="", ;
                  DUMMY.F:H=""            ;
           FREEZE DUMMY.F WHEN Show_C()   ;
           NOMENU NORGRID NOMODIFY NOAPPEND NODELETE NOCLEAR ;
           WINDOW List_I IN WINDOW List_E
    ON KEY
    HIDE MENU List_M
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    IF what_do = "List"
      what_do = IIF(READKEY()%256=12, "Exit", "View")
    ENDIF
  CASE what_do = "View"     && Содержимое списка
    ON KEY LABEL F10 ACTIVATE MENU View_M
    ON KEY LABEL Home GO TOP
    ON KEY LABEL End  GO BOTTOM
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("Меню F10 ",WCOLS()) COLOR SCHEME 13
    ACTIVATE WINDOW View_M TOP
    @ 0, 0 SAY PADC(ALLTRIM(L_NUM)+" "+ALLTRIM(LIST_TIT.LST_NAME), WCOLS()) COLOR SCHEME 5
    SHOW MENU View_M
    SELECT LIST_DET
    ACTIVATE WINDOW View_E
    BROWSE FIELDS PREFIX:H="",            ;
                  NAME:H="",              ;
                  QNT:H="":P=pic_0,       ;
                  PRICE0 = IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE):H="":P=pic_1, ;
                  SUM000 = QNT*IIF(LIST_TIT.IS_USD, USD_PRICE, SAL_PRICE):H="":P=pic_2, ;
                  DUMMY.F:H=""            ;
           FREEZE DUMMY.F                 ;
           KEY    LINK_O.L_NUM            ;
           NOMENU NORGRID NOAPPEND NOMODIFY NODELETE NOCLEAR ;
           WINDOW View_I IN WINDOW View_E
    SELECT LINK_O
    ON KEY LABEL F10
    ON KEY LABEL Home
    ON KEY LABEL End
    HIDE MENU View_M
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 13
    HIDE WINDOW View_E
    HIDE WINDOW View_M
    what_do = "List"
  CASE what_do = "Order"     && Заявка
    DO Ord_
    what_do = "List"
  CASE what_do = "Cash"      && Депозиты
    DO V_Note
    what_do = "List"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO
RELEASE MENU List_M EXTENDED
RELEASE MENU View_M EXTENDED
RELEASE WINDOW List_M
RELEASE WINDOW List_E
RELEASE WINDOW List_I

CLOSE DATABASES
DELETE FILE (tmpo_path+"LINK_O.DBF")

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                   Программа выдачи имени клиента.                     ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Show_C
PRIVATE arm_f

ACTIVATE WINDOW List_M BOTTOM
arm_f = IIF(LIST_TIT.LST_ARM = "***",   ;
            PADR("Общая область",FSIZE("ARM_N","ARM_LST")), ;
            ARM_LST.ARM_N )
IF .NOT. FOUND("ACCOUNT")
  @ WROWS()-2, 2 SAY SPACE(FSIZE("CUS_NAME", "ACCOUNT"))+" "+arm_f
ELSE
  @ WROWS()-2, 2 SAY ACCOUNT.CUS_NAME+" "+arm_f
ENDIF

ACTIVATE WINDOW (ALIAS())
RETURN .T.

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                 Программа формирования и печати заявки.               ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Ord_

PRIVATE sel_sav, rc_sav, nd1, q, cur_zone, l_cnt, p_lines, cnt_neg, cnt_pos
PRIVATE ordArr, ordCnt, i

p_lines = 20
sel_sav = SELECT()
rc_sav  = RECNO()

nd1 = STR(LIST_TIT.LST_NUM,6)
l_cnt = p_lines+1
cur_zone = CHR(0)+CHR(0)
ordCnt = 0
cnt_neg = 0
cnt_pos = 0

SELECT 0
USE (base_path+"ZONES") ORDER TAG ZONE

SELECT 0
USE (base_path+"ORD_D")

SELECT 0
USE (base_path+"ORD_T") ORDER TAG DOC_NUM

SELECT LIST_DET
SET ORDER TO L_ADDRESS
SEEK nd1
SCAN REST WHILE nd1 = LIST_NUM
  IF CODE <= 0 .OR. QNT = QNT_REQ
    LOOP
  ENDIF
  IF WhatZone(ADDRESS) # cur_zone .OR. l_cnt >= p_lines
    IF ordCnt > 0
      SELECT ORD_T
      APPEND BLANK
      REPLACE LIST_NUM  WITH nd1,     ;
              LIST_NAME WITH LIST_TIT.LST_NAME, ;
              ORD_NUM   WITH ordArr(ordCnt),    ;
              ZONE      WITH cur_zone,;
              ORD_DATE  WITH DATE(),  ;
              ORD_TIME  WITH TIME(),  ;
              SIZE_NEG  WITH cnt_neg, ;
              SIZE_POS  WITH cnt_pos, ;
              QUEUE     WITH LIST_TIT.ORD_QUEUE, ;
              WHO       WITH user,    ;
              WHERE     WITH arm
    ENDIF
    ordCnt = ordCnt+1
    DIMENSION ordArr(ordCnt)
    ordArr(ordCnt) = LIST_TIT.ORD_NUM+1
    REPLACE LIST_TIT.ORD_NUM WITH ordArr(ordCnt)
    cur_zone = WhatZone(ADDRESS)
    l_cnt = 0
    cnt_neg = 0
    cnt_pos = 0
  ENDIF
  l_cnt = l_cnt+1
  q = QNT-QNT_REQ
  IF q < 0
    cnt_neg = cnt_neg+1
  ELSE
    cnt_pos = cnt_pos+1
  ENDIF
  REPLACE QNT_REQ WITH QNT
  SELECT ORD_D
  APPEND BLANK
  REPLACE LIST_NUM WITH nd1,     ;
          ORD_NUM  WITH ordArr(ordCnt),;
          CODE     WITH LIST_DET.CODE, ;
          QNT      WITH q
  SELECT STOCK
  REPLACE QNT      WITH QNT-q,   ;
          QNT_SAVE WITH QNT_SAVE+q
  SELECT LIST_DET
ENDSCAN
SELECT ORD_T
IF ordCnt > 0
  APPEND BLANK
  REPLACE LIST_NUM  WITH nd1,     ;
          LIST_NAME WITH LIST_TIT.LST_NAME, ;
          ORD_NUM   WITH ordArr(ordCnt),    ;
          ZONE      WITH cur_zone,;
          ORD_DATE  WITH DATE(),  ;
          ORD_TIME  WITH TIME(),  ;
          SIZE_NEG  WITH cnt_neg, ;
          SIZE_POS  WITH cnt_pos, ;
          QUEUE     WITH LIST_TIT.ORD_QUEUE, ;
          WHO       WITH user,    ;
          WHERE     WITH arm
  REPLACE LIST_TIT.NOTE_STO WITH "*"
  PRIVATE t_ready, d_ready, tmpNote, to_print
  DO Get_Sto WITH LIST_TIT.ORD_QUEUE
  IF LIST_TIT.ORD_NUM = 0
    IF EMPTY(LIST_TIT.DATE_REQ)
      IF EMPTY(t_stock)
        dt_ready = {}
        tm_ready = ""
      ELSE
        dt_ready = DATE()
        tm_ready = TIME()
        DO Add_T WITH dt_ready, tm_ready, t_stock+2, dt_ready, tm_ready
      ENDIF
    ELSE
      dt_ready = LIST_TIT.DATE_REQ
      tm_ready = LIST_TIT.TIME_REQ
    ENDIF
  ELSE
    dt_ready = LIST_TIT.DATE_REQ
    tm_ready = LIST_TIT.TIME_REQ
  ENDIF
  tmpNote = SPACE(FSIZE("NOTE","ORD_T"))
  to_print = .NOT. hard_sale
  = Ord_Note("К какому времени собрать заявку", d_ready, t_ready, tmpNote, to_print, LIST_TIT.ORD_QUEUE)
  REPLACE LIST_TIT.TIME_REQ WITH t_ready, ;
          LIST_TIT.DATE_REQ WITH d_ready
  SEEK nd1
  REPLACE REST WHILE LIST_NUM = nd1  ;
          REQ_DATE WITH d_ready,     ;
          REQ_TIME WITH t_ready,     ;
          NOTE     WITH tmpNote
  IF to_print
    FOR i = 1 TO ordCnt
      IF new_ord
        DO Prn_OrdN WITH VAL(ALLTRIM(nd1)), ordArr(i)
      ELSE
        DO Prn_Ord WITH VAL(ALLTRIM(nd1)), ordArr(i)
      ENDIF
    ENDFOR
  ENDIF
ENDIF

SELECT ZONES
USE
SELECT ORD_T
USE
SELECT ORD_D
USE

SELECT LIST_DET
SET ORDER TO L_NAME
SELECT (sel_sav)
GO rc_sav
DELETE
SKIP

RETURN
