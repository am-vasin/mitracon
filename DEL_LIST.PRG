*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                     Программа удаления списка.                        ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Del_List
PARAMETER pMode

PRIVATE sel_rem, k00, ord_n, c1, c2, mss, mode, pr_d, queue_t
PRIVATE ord_len, cnt, c_zone, tmp_zone, tmpArr, cnt_neg, cnt_pos
ord_len = 20      && Максимальная длина заявки.

sel_rem = SELECT()
IF .NOT. Tst_Hard(LIST_TIT.LST_NUM)
  SELECT (sel_rem)
  RETURN
ENDIF

IF TYPE("pMode") = "N"
  mode = pMode
ELSE
  IF "отказы" $ LST_NAME .OR. "дефицит" $ LST_NAME
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"И что, действительно, удаляем?"
    mss(3) = CHR(0)+"  [ Да, удаляем! ] [ Вернуться! ]"
    mss(4) = ""
    mode = Out_Mess(7, "mss")+1
  ELSE    
    IF EMPTY(SAL_NUM)
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"И что, действительно, удаляем, и как?"
      mss(3) = CHR(0)+"   [ Клиент отказался... ] [ Просто удаляем и все! ] [ Вернуться! ]"
      mss(4) = ""
      mode = Out_Mess(7, "mss")
    ELSE
      DIMENSION mss(4)
      mss(1) = ""
      mss(2) = CHR(0)+"И что, действительно, удаляем?"
      mss(3) = CHR(0)+"  [ Да, удаляем! ] [ Вернуться! ]"
      mss(4) = ""
      mode = Out_Mess(7, "mss")+1
    ENDIF
  ENDIF
ENDIF

IF mode = 3
  RETURN
ENDIF
PUSH KEY CLEAR
DEFINE WINDOW Hide_M FROM 0, 0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW Hide_M

queue_t = LIST_TIT.ORD_QUEUE
ord_n = LIST_TIT.ORD_NUM
IF EMPTY(LIST_TIT.SAL_NUM)
  SELECT 0
  DELETE FILE (tmpo_path+"REQ_TMP.DBF")
  USE (base_path+"ORD_D")
  COPY STRUCTURE TO (tmpo_path+"REQ_TMP")
  USE (tmpo_path+"REQ_TMP") EXCLUSIVE
  ZAP
ENDIF

*IF File_O(base_path+"ZONES.DBF")
*  SELECT 0
*  USE (base_path+"ZONES") ORDER TAG ZONE
*ENDIF

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_TMP AGAIN

SELECT 0
USE (base_path+"LIST_DET") ORDER TAG LISTPURE ALIAS LIST_TMP AGAIN

k00 = STR(LIST_TIT.LST_NUM,6)

DO Emp_Date
*
*  Бронь?
PRIVATE tmpNote, is_bron
tmpNote = SPACE(40)
is_bron = .F.
IF EMPTY(LIST_TIT.SAL_NUM)
  k00 = STR(LIST_TIT.LST_NUM,6)
  SEEK k00
  SCAN REST WHILE LIST_NUM = k00
    IF QNT_REZ > 0
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Список содержит резерв. Его нельзя удалять!"
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
      USE
      IF USED("REQ_TMP")
        SELECT REQ_TMP
        USE
        DELETE FILE (tmpo_path+"REQ_TMP.DBF")
      ENDIF
      IF USED("ZONES")
        SELECT ZONES
        USE
      ENDIF
      SELECT STO_TMP
      USE
      SELECT LIST_TMP
      USE
      SELECT (sel_rem)
      RETURN
    ENDIF
    IF QNT_REQ > 0
      is_bron = .T.
      EXIT
    ENDIF
  ENDSCAN
  IF is_bron
    IF .NOT. To_Del()
      IF USED("ZONES")
        SELECT ZONES
        USE
      ENDIF
      SELECT STO_TMP
      USE
      SELECT LIST_TMP
      USE
      SELECT REQ_TMP
      USE
      DELETE FILE (tmpo_path+"REQ_TMP.DBF")
      SELECT (sel_rem)
      RELEASE WINDOW Hide_M
      RETURN
    ENDIF
  ENDIF
ENDIF

SEEK k00

*
*   Пааашли по списку
*
c1 = 0
c2 = 0
c_zone = CHR(0)+CHR(0)
cnt = ord_len
cnt_neg = 0
cnt_pos = 0
SCAN REST WHILE LIST_NUM = k00
  = SEEK(CODE, "STO_TMP")
  IF EMPTY(LIST_TIT.SAL_NUM)
    w = -QNT_REQ      && Дозаказать!
    IF w # 0
      tmp_zone = WhatZone(STO_TMP.ADDRESS)
      IF c_zone # tmp_zone .OR. cnt >= ord_len
        ord_n = ord_n+1
        cnt = 0
        cnt_neg = 0
        cnt_pos = 0
        c_zone = tmp_zone
      ELSE
        cnt = cnt+1
      ENDIF

      SELECT REQ_TMP
      APPEND BLANK
      REPLACE LIST_NUM WITH k00,     ;
              ORD_NUM  WITH ord_n,   ;
              CODE     WITH LIST_TMP.CODE, ;
              QNT      WITH w
      SELECT LIST_TMP
    ENDIF
  ENDIF
  IF USED("REFUSAL") .AND. mode = 1
    pr_d = IIF(FOUND("STO_TMP"),   ;
                       STO_TMP.PRICE, LIST_TMP.SAL_PRICE/kurs)
    SELECT REFUSAL
    APPEND BLANK
    REPLACE STO_CODE  WITH LIST_TMP.CODE,      ;
            PREFIX    WITH LIST_TMP.PREFIX,    ;
            NAME      WITH LIST_TMP.NAME,      ;
            PRO_CODE  WITH LIST_TMP.PRO_CODE,  ;
            PRODUCER  WITH LIST_TMP.PRODUCER,  ;
            DATE      WITH DATE(),             ;
            QTY       WITH LIST_TMP.QNT,       ;
            PRICE     WITH LIST_TMP.SAL_PRICE, ;
            PRICE_D   WITH pr_d,               ;
            CUS_CODE  WITH LIST_TIT.CLI_CODE,  ;
            CLIENT    WITH LIST_TIT.LST_NAME,  ;
            FUNCTION  WITH LIST_TMP.FUNCTION,  ;
            AMODEL    WITH LIST_TMP.AMODEL,    ;
            WHO       WITH user,               ;
            WHERE     WITH arm
    SELECT LIST_TMP
  ENDIF
  DELETE
ENDSCAN
USE
SELECT STO_TMP
USE
SELECT LIST_TIT
IF EMPTY(SAL_NUM)                  && Надо сформировать
  IF RECCOUNT("REQ_TMP") # 0       &&       контрзаявку
    REPLACE ORD_NUM WITH ord_n
    ord_n = 0
    SELECT 0
    USE (base_path+"ORD_D")                         && Содержимое заявки
    SELECT 0
    USE (base_path+"ORD_T") ORDER TAG DOC_NUM       && Заголовок заявки
    SELECT 0
    USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_ORD AGAIN
    SELECT REQ_TMP
    cnt = 0
    cnt_neg = 0
    cnt_pos = 0
    SCAN                           && коррекция брони на складе
      IF .NOT. SEEK(CODE,"STO_ORD")
        LOOP
      ENDIF
      SELECT STO_ORD
      REPLACE QNT_SAVE WITH QNT_SAVE+REQ_TMP.QNT,  ;
              QNT      WITH QNT-REQ_TMP.QNT
      SELECT REQ_TMP
      SCATTER TO tmpArr
      IF ord_n # ORD_NUM
        SELECT ORD_T
        IF ord_n # 0
          REPLACE SIZE_NEG WITH cnt_neg, SIZE_POS WITH cnt_pos
        ENDIF
        cnt = 0
        cnt_neg = 0
        cnt_pos = 0
        ord_n = REQ_TMP.ORD_NUM
        APPEND BLANK
        REPLACE LIST_NUM  WITH k00,  ;
                LIST_NAME WITH LIST_TIT.LST_NAME, ;
                ZONE      WITH WhatZone(STO_ORD.ADDRESS), ;
                ORD_NUM   WITH ord_n,      ;
                ORD_DATE  WITH DATE(),     ;
                ORD_TIME  WITH TIME(),     ;
                NOTE      WITH tmpNote,    ;
                QUEUE     WITH queue_t,    ;
                WHO       WITH user,       ;
                WHERE     WITH arm
      ENDIF
      cnt = cnt+1
      IF REQ_TMP.QNT < 0
        cnt_neg = cnt_neg+1
      ELSE
        cnt_neg = cnt_pos+1
      ENDIF
      SELECT ORD_D
      APPEND FROM ARRAY tmpArr
      SELECT REQ_TMP
    ENDSCAN
    SELECT ORD_T
    IF cnt # 0
      REPLACE SIZE_NEG WITH cnt_neg, SIZE_POS WITH cnt_pos
    ENDIF
    SEEK k00
    SCAN REST WHILE LIST_NUM = k00 FOR EMPTY(REQ_DATE)
      REPLACE REQ_DATE WITH DATE(), REQ_TIME WITH TIME()
    ENDSCAN
    USE
    SELECT ORD_D
    USE
    SELECT STO_ORD
    USE
    SELECT LIST_TIT
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"ВНИМАНИЕ! Вы не полностью расчитались со складом по списку "+ALLTRIM(STR(LIST_TIT.LST_NUM,10))+"."
    mss(3) = CHR(0)+"Сформирована заявка N "+ALLTRIM(STR(LIST_TIT.LST_NUM,10))+"/"+ ;
                                             ALLTRIM(STR(LIST_TIT.ORD_NUM,10))
    mss(4) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
ENDIF
DELETE
IF USED("ZONES")
  SELECT ZONES
  USE
ENDIF
IF USED("REQ_TMP")
  SELECT REQ_TMP
  USE
ENDIF
DELETE FILE (tmpo_path+"REQ_TMP.DBF")
POP KEY
RELEASE WINDOW Hide_M
IF USED("DEMAND")
  IF USED("DEM_LINK")
    SELECT DEM_LINK
    DO WHILE SEEK(LIST_TIT.LST_NUM, "DEM_LINK")
      REPLACE LST_NUM WITH 0
    ENDDO
  ENDIF
ENDIF
SELECT (sel_rem)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура To_Del       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│               Подтверждение об удалении списка с бронью                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 17.01.2001 ──┘
PROCEDURE To_Del

PRIVATE s_sav, tmpCusN

IF hard_sale
  RETURN .F.
ENDIF
s_sav = SELECT()
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS TMPC_1117 AGAIN
tmpCusN = SPACE(50)
IF .NOT. EMPTY(LIST_TIT.CLI_CODE)
  IF SEEK(LIST_TIT.CLI_CODE)
    tmpCusN = TMPC_1117.CUS_NAME
  ENDIF
ENDIF
USE
tmpNote = PADR(LIST_TIT.ADDRESS, 40)

SELECT (s_sav)

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
*Список с бронью
*┌────────────────────────────────────────────────────────┐
*│       N 999999 xxxx!xxxxXxxxx!xxxxXxxxx!xxxxXxx        │
*│ ┌─ Клиент ───────────────────────────────────────────┐ │
*│ │ xxxx!xxxxXxxxx!xxxxXxxxx!xxxxXxxxx!xxxxXxxxx!xxxxX │ │
*│ └────────────────────────────────────────────────────┘ │
*│  Примечание xxxx!xxxxXxxxx!xxxxXxxxx!xxxxXxxxx!xxxxX   │
*│         < Удалить Ctrl-W > < Отказаться Esc >          │
*└────────────────────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 60, "ВИНАНИЕ! Вы удаляете список с бронью!"
DO Sun_Bord WITH  3,  3,  5, 56, " Клиент "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2,  9 SAY "N" GET LIST_TIT.LST_NUM PICTURE "999999" WHEN .F.
@  2, 18 GET LIST_TIT.LST_NAME WHEN .F.
@  4,  5 GET tmpCusN WHEN .F.
@  6,  4 SAY "Примечание" GET tmpNote
@ WROWS()-2, 11 GET ex PICTURE "@*HT \! Удалить Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Emp_Date     Разработчик Галина Дмитриенко                 │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Пустая дата подачи заявки.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.03.2002 ──┘
PROCEDURE Emp_Date
PRIVATE m_sav
m_sav = SELECT()

SELECT LIST_TIT
IF .NOT.EMPTY(ORD_NUM) .AND. EMPTY(DATE_REQ)
  REPLACE DATE_REQ WITH DATE(), ;
          TIME_REQ WITH TIME()
  SELECT 0
  USE(base_path+"ORD_T") ORDER TAG DOC_NUM ALIAS OT_2313 AGAIN
  SEEK k00
  SCAN REST WHILE LIST_NUM = k00
    IF EMPTY(REQ_DATE)
      REPLACE REQ_DATE WITH LIST_TIT.DATE_REQ, ;
              REQ_TIME WITH LIST_TIT.TIME_REQ
    ENDIF  
  ENDSCAN
  USE
ENDIF

SELECT(m_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_hard     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│   Проверка допустимости удаления списка при жестком режиме продажи.    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.11.2005 ──┘
PROCEDURE Tst_hard
PARAMETERS prmList

PRIVATE s_sav, tmpN, mss
IF .NOT. hard_sale
  RETURN .T.
ENDIF

m.s_sav = SELECT()

SELECT 0
m.tmpN = STR(m.prmList, 6)
IF EMPTY(LIST_TIT.SAL_NUM)
  USE (m.base_path+"LIST_DET") SHARED AGAIN ORDER TAG LIST ALIAS TMP5B06
  SEEK m.tmpN
  SCAN REST WHILE LIST_NUM = m.tmpN
    IF QNT_REQ # 0
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Список содержит бронь!"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      USE
      SELECT (m.s_sav)
      RETURN .F.
    ENDIF
    IF QNT_DUE # 0
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Список содержит долг!"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      USE
      SELECT (m.s_sav)
      RETURN .F.
    ENDIF
  ENDSCAN
ENDIF
USE (m.base_path+"ORD_T") SHARED AGAIN ORDER TAG DOC_NUM ALIAS TMP5B06
SEEK m.tmpN
SCAN REST WHILE LIST_NUM = m.tmpN
  IF EMPTY(STO_MARK)
    IF .NOT. EMPTY(REQ_DATE)
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Не все заявки утверждены на складе!"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
      USE
      SELECT (m.s_sav)
      RETURN .F.
    ELSE	&& Только резерв - утверждаем сами
      REPLACE STO_MARK WITH m.user, ;
              REQ_DATE WITH DATE()
    ENDIF
  ENDIF
  IF EMPTY(WHO)
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"Не все заявки утверждены менеджером!"
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    USE
    SELECT (m.s_sav)
    RETURN .F.
  ENDIF
ENDSCAN
USE
SELECT (m.s_sav)
RETURN .T.
