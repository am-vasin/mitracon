*г========================================================================м
*ж Файл For_AS       Разработчик Андрей Васин           13.10.97 17:22:00 ж
*ж------------------------------------------------------------------------ж
*ж                                                                        ж
*ж                        Обработка заявки от АС.                         ж
*ж                                                                        ж
*L========================================================================-
PROCEDURE For_AS

PRIVATE n_ord, sel_sav, err_level,t_nds,sys_char, max_ord, pay_prc

n_ord = 0     && Номер заявки на склад
err_level = 0
sel_sav = SELECT()

IF FILE(base_store+"\MAX_ORD.MEM")
  RESTORE FROM (base_store+"\MAX_ORD.MEM") ADDITIVE
ELSE
  max_ord = 0
ENDIF
pay_prc = 0        && "пятипроцентный" налог

SELECT 0
USE (base_store+"\PARMS") ALIAS PRM_AS AGAIN
sys_char = PRM_AS.SYS_ID
USE IN PRM_AS

DO OP_MT WITH "MT_tNDS"
t_nds = MT_tNDS.NDS			&&Значение НДС
USE IN MT_tNDS

SELECT DECL_TIT
IF BOF() .OR. EOF()
  SELECT (sel_sav)
  RETURN
ENDIF

IF NOT USED("STO_AS")
SELECT 0
USE (base_store+"\STOCK") ORDER TAG CODE ALIAS STO_AS AGAIN
ENDIF

IF NOT USED("LT_AS")
SELECT 0
USE (base_store+"\LIST_TIT") ORDER TAG LST_NUM ALIAS LT_AS AGAIN
ENDIF

IF NOT USED("LD_AS")
SELECT 0
USE (base_store+"\LIST_DET") ORDER TAG L_CODE ALIAS LD_AS AGAIN
ENDIF

IF NOT USED("OT_AS")
SELECT 0
USE (base_store+"\ORD_T") ALIAS OT_AS AGAIN
ENDIF

IF NOT USED("OD_AS")
SELECT 0
USE (base_store+"\ORD_D") ALIAS OD_AS AGAIN
ENDIF

IF NOT USED("OTR_AS")
SELECT 0
USE (base_store+"\ORD_TR") ALIAS OTR_AS AGAIN
ENDIF

IF NOT USED("ODR_AS")
SELECT 0
USE (base_store+"\ORD_DR") ALIAS ODR_AS AGAIN
ENDIF

DO CASE
CASE DECL_TIT.CODE = "1"      && Модификация/создание списка долга
  IF EMPTY(DECL_TIT.LST_NUM)
    DO Create_L
  ELSE
    DO Modify_L
  ENDIF
CASE DECL_TIT.CODE = "2"      && Бронирование
  DO Bron_L
*!*	CASE DECL_TIT.CODE = "3" .OR. DECL_TIT.CODE = "5"     && Продажа или реализация
CASE DECL_TIT.CODE = "5"      && Реализация 
  DO Sale_L
CASE DECL_TIT.CODE = "6"     && Заявка на продажу
  DO S_Inv WITH DECL_TIT.Fill_CODE,"3"
CASE DECL_TIT.CODE = "7"      && Заявка на возврат из филиала
  DO S_Inv WITH DECL_TIT.Fill_CODE,"B"
CASE DECL_TIT.CODE = "4"      && Удаление
  DO Delete_L
CASE DECL_TIT.CODE = "3"      && Собственно продажа
  DO R_Sale
CASE DECL_TIT.CODE = "9"      && Резервирование
  DO Reserv
CASE DECL_TIT.CODE = "A"      && Бронь из резерва
  DO Bron_L WITH .T.
ENDCASE

SELECT STO_AS
USE
SELECT LT_AS
USE
SELECT LD_AS
USE
SELECT OT_AS
USE
SELECT OD_AS
USE
SELECT OTR_AS
USE
SELECT ODR_AS
USE
SELECT (sel_sav)

RETURN

*г========================================================================м
*ж                                                                        ж
*ж                            Создание списка.                            ж
*ж                                                                        ж
*L==================================================== 13.10.97 18:01:34 =-
PROCEDURE Create_L

PRIVATE l_num, s,nm,nam_cus
****моя вставка
USE (base_store+"\ACCOUNT")IN 0 ALIAS ACC_AS AGAIN
SET ORDER TO TAG CUS_CODE IN ACC_AS

IF SEEK(DECL_TIT.CUS_CODE,"ACC_AS")
  nam_cus = ACC_AS.CUS_NAME
ELSE
  nam_cus = ""
ENDIF
USE IN ACC_AS
*****

*l_num = Doc_Num("LISTS", "", "", DATE()) &&&&&&&&&&&&&&&&&&&&&&&&
l_num = NEWCOUNT("LISTS", "", "", STR(YEAR(DATE()),4)) 
*!*	nm = "N "+ALLTRIM(STR(DECL_TIT.SPEC_NUM))+" от "+DTOC(DATE())
nm = "N "+ALLTRIM(STR(DECL_TIT.SPEC_NUM))+ "   " + ALLTRIM(nam_cus)

SELECT LT_AS
APPEND BLANK
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
&&    Добавлено поле FIRM
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

REPLACE LST_ARM   WITH DECL_TIT.WHERE,;
        LST_NUM   WITH l_num,      ;
        CR_DAT    WITH DATE(),     ;
        LST_NAME  WITH PADR(nm,FSIZE("LST_NAME")-1)+CHR(0),;
        CLI_CODE  WITH DECL_TIT.CUS_CODE, ;
        MAIL      WITH DECL_TIT.MAIL, ;
        FIRM      WITH DECL_TIT.FIRM, ;
        ORD_QUEUE WITH glQueue,           ;
        WHO_CR    WITH DECL_TIT.WHO,  ;
        WHERE_CR  WITH DECL_TIT.WHERE
SELECT DECL_LST
s = 0
SEEK STR(YEAR(DECL_TIT.DOC_DATE),4)+STR(DECL_TIT.DOC_NUM, FSIZE("DOC_NUM"))
SCAN REST WHILE DECL_TIT.DOC_DATE = DOC_DATE .AND. DECL_TIT.DOC_NUM = DOC_NUM
  IF QTY <= 0 .OR. .NOT. SEEK(STO_CODE, "STO_AS") .AND. STO_CODE # 0
    err_level = MAX(err_level,1)
    LOOP
  ENDIF
  SELECT LD_AS
  APPEND BLANK
  REPLACE  ;
  LIST_NUM  WITH STR(LT_AS.LST_NUM,6),  ;
  CODE      WITH DECL_LST.STO_CODE,     ;
  PREFIX    WITH IIF(DECL_LST.STO_CODE=0, DECL_LST.PREFIX, STO_AS.PREFIX),;
  NAME      WITH IIF(DECL_LST.STO_CODE=0, DECL_LST.NAME, STO_AS.NAME), ;
  PRODUCER  WITH IIF(DECL_LST.STO_CODE=0, DECL_LST.PRODUCER, STO_AS.PRODUCER), ;
  ADDRESS   WITH IIF(DECL_LST.STO_CODE=0, "", STO_AS.ADDRESS),  ;
  QNT_DUE   WITH MAX(DECL_LST.QTY,0),   ;
  DEF_PRICE WITH STO_AS.PRICE_R,        ;
  USD_PRICE WITH DECL_LST.PRICE,		;
  SAL_PRICE WITH DECL_LST.PRICE_R * (1+t_nds/100),      ;
  IS_SPEC   WITH IIF(DECL_LST.PRICE_R = STO_AS.PRICE_R, " ", "S")
  s = s+MAX(DECL_LST.QTY,0)*SAL_PRICE
  SELECT DECL_LST
  REPLACE QTY_RPL WITH LD_AS.QNT_DUE
ENDSCAN

SELECT DECL_TIT
REPLACE STOC_NUM WITH STR(l_num,6), STOC_DATE WITH DATE(), FLAG WITH STR(err_level,1)

RETURN

*г========================================================================м
*ж                                                                        ж
*ж                          Модификация списка.                           ж
*ж                                                                        ж
*L==================================================== 13.10.97 18:01:34 =-
PROCEDURE Modify_L

PRIVATE q0 ,b_stock, e_stock, q_pos, q_neg, n_ord1

n_ord1 = 0
b_stock = "00:00"   && Время работы
e_stock = "23:59"   &&          склада

STORE 0 TO q_pos, q_neg

SELECT DECL_LST
IF .NOT. SEEK(VAL(ALLTRIM(DECL_TIT.LST_NUM)), "LT_AS")
  SELECT DECL_TIT
  REPLACE FLAG WITH "2"
  RETURN
ENDIF

SEEK STR(YEAR(DECL_TIT.DOC_DATE),4)+STR(DECL_TIT.DOC_NUM, FSIZE("DOC_NUM"))
SCAN REST WHILE DECL_TIT.DOC_DATE = DOC_DATE .AND. DECL_TIT.DOC_NUM = DOC_NUM
  IF .NOT. SEEK(STO_CODE, "STO_AS") .AND. STO_CODE # 0
    err_level = MAX(err_level,1)
    LOOP
  ENDIF
  SELECT LD_AS
  IF DECL_LST.STO_CODE = 0
    SET ORDER TO TAG L_CODE
    SEEK DECL_TIT.LST_NUM  && + DECL_LST.NAME+DECL_LST.PREFIX+DECL_LST.PRODUCER
    LOCATE REST WHILE LIST_NUM = DECL_TIT.LST_NUM .AND. CODE = 0 FOR ;
         NAME = DECL_LST.NAME .AND. PREFIX = DECL_LST.PREFIX .AND. PRODUCER = DECL_LST.PRODUCER
  ELSE
    SET ORDER TO TAG L_CODE
    SEEK DECL_TIT.LST_NUM + STR(DECL_LST.STO_CODE,7)
  ENDIF
  IF .NOT. FOUND()
    IF DECL_LST.QTY > 0
      APPEND BLANK
      REPLACE ;
      LIST_NUM  WITH STR(LT_AS.LST_NUM,6), ;
      CODE      WITH DECL_LST.STO_CODE,    ;
      PREFIX    WITH IIF(DECL_LST.STO_CODE=0,DECL_LST.PREFIX,  STO_AS.PREFIX), ;
      NAME      WITH IIF(DECL_LST.STO_CODE=0,DECL_LST.NAME,    STO_AS.NAME),   ;
      PRODUCER  WITH IIF(DECL_LST.STO_CODE=0,DECL_LST.PRODUCER,STO_AS.PRODUCER),;
      ADDRESS   WITH IIF(DECL_LST.STO_CODE=0, "", STO_AS.ADDRESS), ;
      QNT_DUE   WITH MAX(DECL_LST.QTY,0),  ;
      DEF_PRICE WITH IIF(DECL_LST.STO_CODE=0, 0, STO_AS.PRICE_R), ;
      USD_PRICE WITH DECL_LST.PRICE,		;
      SAL_PRICE WITH DECL_LST.PRICE_R * (1+t_nds/100),     ;
      IS_SPEC   WITH IIF(DECL_LST.PRICE_R = STO_AS.PRICE_R, " ", "S")
      SELECT DECL_LST
      REPLACE QTY_RPL WITH LD_AS.QNT_DUE
    ELSE
      err_level = MAX(err_level, 1)
    ENDIF
    SELECT DECL_LST
    LOOP
  ENDIF
  IF DECL_LST.QTY # 0
    q0 = IIF(DECL_LST.QTY+QNT_DUE < 0, -QNT_DUE, DECL_LST.QTY)
    IF q0 # DECL_LST.QTY
      err_level = MAX(err_level, 1)
    ENDIF
    REPLACE QNT_DUE WITH q0+QNT_DUE
    IF QNT_DUE = 0 .AND. QNT = 0 .AND. QNT_REQ = 0
      DELETE
    ENDIF
  ELSE
    q0 = -QNT_DUE-QNT_REQ-QNT_REZ
    * Удалить и разбронировать
    IF QNT_REQ # 0
      IF EMPTY(n_ord)
        n_ord = LT_AS.ORD_NUM+1
        REPLACE LT_AS.ORD_NUM WITH n_ord
        SELECT OT_AS
        APPEND BLANK
        REPLACE LIST_NUM  WITH STR(LT_AS.LST_NUM,6), ;
                LIST_NAME WITH LT_AS.LST_NAME,       ;
                ORD_NUM   WITH n_ord,     ;
                ORD_DATE  WITH DATE(),    ;
                ORD_TIME  WITH TIME(),    ;
                QUEUE     WITH LT_AS.ORD_QUEUE, ;
                WHO       WITH DECL_TIT.WHO, ;
                WHERE     WITH DECL_TIT.WHERE

        IF NOT EMPTY(FSIZE("REQ_DATE"))
          d_w = DATE()
          t_w = TIME()
	      DO Add_T WITH d_w, t_w, 1, d_w, t_w
          REPLACE REQ_DATE	WITH d_w,;
                  REQ_TIME	WITH t_w
        ENDIF
                
      ENDIF
      SELECT OD_AS
      APPEND BLANK
      REPLACE LIST_NUM  WITH OT_AS.LIST_NUM, ;
              ORD_NUM   WITH n_ord,     ;
              CODE      WITH LD_AS.CODE,;
              QNT       WITH -LD_AS.QNT_REQ
      IF QNT < 0
        q_neg = q_neg+1
      ELSE
        q_pos = q_pos+1
      ENDIF
      SELECT STO_AS
      REPLACE QNT WITH QNT+LD_AS.QNT_REQ, QNT_SAVE WITH QNT_SAVE-LD_AS.QNT_REQ
    ENDIF

    IF QNT_REZ # 0
      IF EMPTY(n_ord1)
        n_ord1 = LT_AS.REZ_NUM+1
        REPLACE LT_AS.REZ_NUM WITH n_ord1
        SELECT OTR_AS
        APPEND BLANK
        REPLACE LIST_NUM  WITH STR(LT_AS.LST_NUM,6), ;
                LIST_NAME WITH LT_AS.LST_NAME,       ;
                REZ_NUM   WITH n_ord1,    ;
                REZ_DATE  WITH DATE(),    ;
                REZ_TIME  WITH TIME(),    ;
                WHO       WITH DECL_TIT.WHO, ;
                WHERE     WITH DECL_TIT.WHERE
      ENDIF
      SELECT ODR_AS
      APPEND BLANK
      REPLACE LIST_NUM  WITH OT_AS.LIST_NUM, ;
              REZ_NUM   WITH n_ord1,    ;
              CODE      WITH LD_AS.CODE,;
              QNT       WITH -LD_AS.QNT_REZ
      SELECT STO_AS
      REPLACE QNT WITH QNT+LD_AS.QNT_REZ, QNT_REZ WITH QNT_REZ-LD_AS.QNT_REZ
    ENDIF

    SELECT LD_AS
    DELETE
  ENDIF
  SELECT DECL_LST
  REPLACE QTY_RPL WITH q0
ENDSCAN
IF n_ord # 0
  SELECT OT_AS
  REPLACE SIZE_NEG WITH q_neg, SIZE_POS WITH q_pos
ENDIF
SELECT DECL_TIT
REPLACE FLAG WITH STR(err_level,1)
IF n_ord # 0
  REPLACE RESULT WITH n_ord, RES_DATE WITH DATE()
ENDIF

RETURN

*г========================================================================м
*ж                                                                        ж
*ж                        Резервирование списка.                          ж
*ж                                                                        ж
*L==================================================== 13.10.97 18:01:34 =-
PROCEDURE Reserv

PRIVATE q0, t_w, d_w, b_stock, e_stock, q_pos, q_neg

STORE 0 TO q_pos, q_neg
SELECT DECL_LST
IF .NOT. SEEK(VAL(ALLTRIM(DECL_TIT.LST_NUM)), "LT_AS")
  SELECT DECL_TIT
  REPLACE FLAG WITH "2"
  RETURN
ENDIF

SEEK STR(YEAR(DECL_TIT.DOC_DATE),4)+STR(DECL_TIT.DOC_NUM, FSIZE("DOC_NUM"))
SCAN REST WHILE DECL_TIT.DOC_DATE = DOC_DATE .AND. DECL_TIT.DOC_NUM = DOC_NUM
  IF .NOT. SEEK(STO_CODE, "STO_AS")
    err_level = MAX(err_level,1)
    LOOP
  ENDIF
  SELECT LD_AS
  IF .NOT. SEEK(DECL_TIT.LST_NUM+STR(DECL_LST.STO_CODE,7))
    = SEEK(DECL_TIT.LST_NUM)
    LOCATE REST WHILE LIST_NUM = DECL_TIT.LST_NUM .AND. CODE = 0 FOR ;
         NAME = DECL_LST.NAME .AND. PREFIX = DECL_LST.PREFIX .AND. PRODUCER = DECL_LST.PRODUCER
    IF .NOT. FOUND()
      err_level = MAX(err_level,1)
      LOOP
    ENDIF
    REPLACE LD_AS.CODE WITH DECL_LST.STO_CODE
  ENDIF
  q0 = DECL_LST.QTY
  IF q0 < 0
    q0 = -MIN(-q0, QNT_REZ)
  ELSE
    q0 = MIN(STO_AS.QNT, q0, QNT_DUE)
  ENDIF
  IF q0 = 0
    err_level = MAX(err_level,1)
    LOOP
  ENDIF
  IF q0 # DECL_LST.QTY
    err_level = MAX(err_level,1)
  ENDIF
  REPLACE QNT WITH QNT+q0, QNT_REZ WITH QNT_REZ+q0, QNT_DUE WITH QNT_DUE-q0
  REPLACE STO_AS.QNT WITH STO_AS.QNT-q0, STO_AS.QNT_REZ WITH STO_AS.QNT_REZ+q0
  IF EMPTY(n_ord)
    n_ord = LT_AS.REZ_NUM+1
    REPLACE LT_AS.REZ_NUM WITH n_ord
    SELECT OTR_AS
    APPEND BLANK
    REPLACE LIST_NUM  WITH STR(LT_AS.LST_NUM,6), ;
            LIST_NAME WITH LT_AS.LST_NAME,       ;
            REZ_NUM   WITH n_ord,     ;
            REZ_DATE  WITH DATE(),    ;
            REZ_TIME  WITH TIME(),    ;
            WHO       WITH DECL_TIT.WHO, ;
            WHERE     WITH DECL_TIT.WHERE,;
            NOTE	  WITH DECL_TIT.NOTE

  ENDIF
  SELECT ODR_AS
  APPEND BLANK
  REPLACE LIST_NUM  WITH OTR_AS.LIST_NUM, ;
          REZ_NUM   WITH n_ord,          ;
          CODE      WITH LD_AS.CODE,     ;
          QNT       WITH q0
  IF QNT < 0
    q_neg = q_neg+1
  ELSE
    q_pos = q_pos+1
  ENDIF
  SELECT DECL_LST
  REPLACE QTY_RPL WITH q0
ENDSCAN

SELECT DECL_TIT
IF n_ord # 0
  REPLACE RESULT WITH n_ord, RES_DATE WITH DATE(), FLAG WITH STR(err_level,1)
ENDIF

RETURN

*г========================================================================м
*ж                                                                        ж
*ж                            Список на продажу.                          ж
*ж                                                                        ж
*L==================================================== 13.10.97 18:01:34 =-
PROCEDURE Sale_L

PRIVATE l_num, s, q0, arr_w,new_chr, ord_s, nam_s, rc_old, rc_new, q_neg, q_pos
DIMENSION arr_w(4)
STORE 0 TO rc_old, rc_new, q_neg, q_pos

*l_num = Doc_Num("LISTS", "", "", DATE()) &&&&&&&&&&&&&&&&&&&&&&&&
l_num = NEWCOUNT("LISTS", "", "", STR(YEAR(DATE()),4)) 

**Моя вставка для перекодировки

new_chr = "***"

SELECT LT_AS
SEEK VAL(ALLTRIM(DECL_TIT.LST_NUM))
nam_s = LST_NAME
ord_s = ORD_NUM+1
REPLACE ORD_NUM WITH ord_s
APPEND BLANK       && Заголовок нового списка
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
&&    Добавлено поле FIRM
&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
REPLACE LST_ARM   WITH new_chr, ;
        LST_NUM   WITH l_num,   ;
        CR_DAT    WITH DATE(),  ;
        LST_NAME  WITH "N "+ALLTRIM(STR(DECL_TIT.SPEC_NUM))+" от "+DTOC(DATE()),;
        CLI_CODE  WITH DECL_TIT.CUS_CODE, ;
        MAIL      WITH DECL_TIT.MAIL, ;
        FIRM      WITH DECL_TIT.FIRM, ;
        IS_USD	  WITH DECL_TIT.DOLLAR = "3",;
        ORD_QUEUE WITH glQueue,       ;
        ORD_NUM   WITH 1,             ;
        WHO_CR    WITH DECL_TIT.WHO,  ;
        WHERE_CR  WITH DECL_TIT.WHERE
*
*  Заголовки заявок!
SELECT OT_AS
APPEND BLANK   && Для исходного списка
REPLACE LIST_NUM  WITH DECL_TIT.LST_NUM, ;
        LIST_NAME WITH nam_s,            ;
        ORD_NUM   WITH ord_s,            ;
        ORD_DATE  WITH DATE(),           ;
        ORD_TIME  WITH TIME(),           ;
        STO_MARK  WITH DECL_TIT.WHO,     ;
        QUEUE     WITH LT_AS.ORD_QUEUE,  ;
        WHO       WITH DECL_TIT.WHO,     ;
        WHERE     WITH DECL_TIT.WHERE,   ;
        NOTE      WITH ""
rc_old = RECNO()
APPEND BLANK   && Для нового списка
REPLACE LIST_NUM  WITH STR(l_num,6),     ;
        LIST_NAME WITH "N "+ALLTRIM(STR(DECL_TIT.SPEC_NUM))+" от "+DTOC(DATE()),;
        ORD_NUM   WITH 1,                ;
        ORD_DATE  WITH DATE(),           ;
        ORD_TIME  WITH TIME(),           ;
        STO_MARK  WITH DECL_TIT.WHO,     ;
        QUEUE     WITH LT_AS.ORD_QUEUE,  ;
        WHO       WITH DECL_TIT.WHO,     ;
        WHERE     WITH DECL_TIT.WHERE,   ;
        NOTE      WITH ""
rc_new = RECNO()

SELECT DECL_LST
s = 0
SEEK STR(YEAR(DECL_TIT.DOC_DATE),4)+STR(DECL_TIT.DOC_NUM, FSIZE("DOC_NUM"))  && На начало заявки и...
SCAN REST WHILE DECL_TIT.DOC_DATE = DOC_DATE .AND. DECL_TIT.DOC_NUM = DOC_NUM &&       перебираем содержимое
  IF QTY <= 0 .OR. .NOT. SEEK(STO_CODE, "STO_AS")
    err_level = MAX(err_level,1)
  ENDIF
  SELECT LD_AS
  IF .NOT. SEEK(DECL_TIT.LST_NUM+STR(DECL_LST.STO_CODE,7))
    err_level = MAX(err_level,1)
    LOOP
  ENDIF
  arr_w( 1) = CODE
  arr_w( 2) = DEF_PRICE
  arr_w( 3) = SAL_PRICE
  arr_w( 4) = IS_SPEC
  q0 = MIN(DECL_LST.QTY, QNT_REQ)
  q0 = MAX(q0, 0)
  IF q0 # DECL_LST.QTY
    err_level = MAX(err_level,1)
  ENDIF
  REPLACE QNT WITH QNT - q0,  QNT_REQ WITH QNT_REQ - q0
  SELECT LD_AS
  APPEND BLANK
  REPLACE LIST_NUM  WITH STR(LT_AS.LST_NUM,6), ;
          CODE      WITH DECL_LST.STO_CODE,    ;
          PREFIX    WITH STO_AS.PREFIX,        ;
          NAME      WITH STO_AS.NAME,          ;
          PRODUCER  WITH STO_AS.PRODUCER,      ;
          ADDRESS   WITH STO_AS.ADDRESS,       ;
          QNT       WITH q0,                   ;
          QNT_REQ   WITH q0,                   ;
          DEF_PRICE WITH arr_w(2),             ;
          USD_PRICE WITH DECL_LST.PRICE,	   ;
          SAL_PRICE WITH DECL_LST.PRICE_R* (1+t_nds/100),     ;
          IS_SPEC   WITH arr_w(4)
  IF LT_AS.IS_USD 
  s = s + QNT*USD_PRICE
  ELSE
  s = s + ROUND(QNT*SAL_PRICE,2)
  ENDIF
  SELECT OD_AS
  APPEND BLANK    && Контрзаявка для старого списка
  REPLACE LIST_NUM WITH DECL_TIT.LST_NUM,  ;
          ORD_NUM  WITH ord_s,             ;
          CODE     WITH DECL_LST.STO_CODE, ;
          QNT      WITH -q0
  IF QNT < 0
    q_neg = q_neg+1
  ELSE
    q_pos = q_pos+1
  ENDIF
  APPEND BLANK    && Заявка для нового списка
  REPLACE LIST_NUM WITH STR(LT_AS.LST_NUM,6), ;
          ORD_NUM  WITH 1,                    ;
          CODE     WITH DECL_LST.STO_CODE,    ;
          QNT      WITH q0
  SELECT DECL_LST
  REPLACE QTY_RPL WITH q0
ENDSCAN

SELECT OT_AS
GO rc_old
REPLACE SIZE_NEG WITH q_neg, SIZE_POS WITH q_pos
GO rc_new
REPLACE SIZE_NEG WITH q_pos, SIZE_POS WITH q_neg
SELECT LT_AS
REPLACE R_TOTAL WITH s
SELECT DECL_TIT
REPLACE STOC_NUM WITH STR(l_num,6), STOC_DATE WITH DATE(), FLAG WITH STR(err_level,1)

RETURN

*г========================================================================м
*ж                                                                        ж
*ж                         Удаление списка.                               ж
*ж                                                                        ж
*L==================================================== 13.10.97 18:01:34 =-
PROCEDURE Delete_L

PRIVATE q0, b_stock, e_stock, q_pos, q_neg

b_stock = "00:00"   && Время работы
e_stock = "23:59"   &&          склада
STORE 0 TO q_pos, q_neg

IF .NOT. SEEK(VAL(ALLTRIM(DECL_TIT.LST_NUM)), "LT_AS")
  SELECT DECL_TIT
  REPLACE FLAG WITH "2"
  RETURN
ENDIF

SELECT LD_AS
IF .NOT. SEEK(DECL_TIT.LST_NUM)
  SELECT LT_AS
  DELETE
  SELECT DECL_TIT
  REPLACE FLAG WITH "1"
  RETURN
ENDIF
SCAN REST WHILE LIST_NUM = DECL_TIT.LST_NUM
  q0 = QNT_REQ
  IF q0 > 0
    IF EMPTY(n_ord)
      n_ord = LT_AS.ORD_NUM+1
      REPLACE LT_AS.ORD_NUM WITH n_ord
      SELECT OT_AS
      APPEND BLANK
      REPLACE LIST_NUM  WITH STR(LT_AS.LST_NUM,6), ;
              LIST_NAME WITH LT_AS.LST_NAME,       ;
              ORD_NUM   WITH n_ord,     ;
              ORD_DATE  WITH DATE(),    ;
              ORD_TIME  WITH TIME(),    ;
              QUEUE     WITH LT_AS.ORD_QUEUE, ;
              WHO       WITH DECL_TIT.WHO, ;
              WHERE     WITH DECL_TIT.WHERE
              
       IF NOT EMPTY(FSIZE("REQ_DATE"))
       d_w = DATE()
       t_w = TIME()
	   DO Add_T WITH d_w, t_w, 1, d_w, t_w
       REPLACE REQ_DATE	WITH d_w,;
               REQ_TIME	WITH t_w
       ENDIF
     
    ENDIF
    SELECT OD_AS
    APPEND BLANK
    REPLACE LIST_NUM  WITH OT_AS.LIST_NUM, ;
            ORD_NUM   WITH n_ord,     ;
            CODE      WITH LD_AS.CODE,;
            QNT       WITH -q0
    IF QNT < 0
      q_neg = q_neg+1
    ELSE
      q_pos = q_pos+1
    ENDIF
    SELECT STO_AS
    SEEK LD_AS.CODE
    REPLACE QNT WITH QNT+q0, QNT_SAVE WITH QNT_SAVE-q0
  ENDIF
  SELECT LD_AS 
  DELETE
ENDSCAN
SELECT LT_AS
DELETE
IF n_ord # 0
  SELECT OT_AS
  REPLACE SIZE_NEG WITH q_neg, SIZE_POS WITH q_pos
ENDIF
SELECT DECL_TIT
REPLACE FLAG WITH STR(err_level,1)
IF n_ord # 0
  REPLACE RESULT WITH n_ord, RES_DATE WITH DATE()
ENDIF

RETURN

*+------------------------------------------------------------------------+
*!   Имя файла Add_T        Разработчик Андрей Васин                      !
*+------------------------------------------------------------------------+
*!                                                                        !
*!                 Добавление минут к складскому времени.                 !
*!                                                                        !
*+---------------------------------------------------------- 12.01.2000 --+
PROCEDURE Add_T
PARAMETERS date_0, time_0, mnts, date_res, time_res

PRIVATE time_0w, b_stock_w, e_stock_w, d_len, m, h
b_stock_w = VAL(ALLTRIM(LEFT(b_stock,2)))*60+VAL(ALLTRIM(SUBSTR(b_stock,4,2)))
e_stock_w = VAL(ALLTRIM(LEFT(e_stock,2)))*60+VAL(ALLTRIM(SUBSTR(e_stock,4,2)))
d_len     = e_stock_w - b_stock_w
time_0w = VAL(ALLTRIM(LEFT(time_0,2)))*60+VAL(ALLTRIM(SUBSTR(time_0,4,2)))
IF mnts >= 0
  date_res = date_0+FLOOR(mnts/d_len)
  time_res = mnts%d_len
  time_res = time_0w+time_res
  IF time_res > e_stock_w
    date_res = date_res+1
    time_res = b_stock_w+time_res-e_stock_w
  ENDIF
ELSE
  date_res = date_0-FLOOR(ABS(mnts)/d_len)
  time_res = ABS(mnts)%d_len
  time_res = time_0w-time_res
  IF time_res < b_stock_w
    date_res = date_res-1
    time_res = e_stock_w+time_res-b_stock_w
  ENDIF
  
ENDIF

h = FLOOR(time_res/60)
m = time_res%60
time_res = RIGHT(STR(100+h),2)+":"+RIGHT(STR(100+m),2)+":00"
RETURN

*+------------------------------------------------------------------------+
*!                                                                        !
*!               Посылка файла накладной передачи в филиал.               !
*!                                                                        !
*+---------------------------------------------------- 15.04.98 11:31:54 -+
PROCEDURE S_Inv
PARAMETERS f_code,f_type
PRIVATE s_sav, n_doc, sup_own, f_code, tc_id, nam_cus, addr_cus
PRIVATE tmpMet,tPursys,tmType

tmType = f_type
IF NOT EMPTY(f_code)
s_sav = SELECT()
sup_own = 10

SELECT 0
USE (base_store+"\ORD_TIT")IN 0 ALIAS RYN_M_T0 AGAIN

SELECT 0
n_doc = STR(-NEWCOUNT("ORDER", "", "", STR(YEAR(DATE()),4)))
IF .NOT. EMPTY(DECL_TIT.TECHNID)
  tc_id = DECL_TIT.TECHNID
ELSE
  tc_id = NEWCOUNT("COUNT", "", "", STR(YEAR(DATE()),4))
  tc_id = STR(sup_own,8)+RIGHT(STR(YEAR(DATE()),4),2)+STR(tc_id)
ENDIF

USE (base_path+"\ACCOUNT1") IN 0 ALIAS ACC_AS AGAIN
SET ORDER TO TAG CUS_CODE IN ACC_AS

IF SEEK(DECL_TIT.CUS_CODE,"ACC_AS")
  nam_cus = ACC_AS.CUS_NAME
ELSE
  nam_cus = ""
ENDIF
USE IN ACC_AS

USE (base_store+"\CLI_INFO")IN 0 ALIAS ACC_INFO AGAIN
SET ORDER TO TAG CUS_CODE IN ACC_INFO

IF SEEK(DECL_TIT.CUS_CODE,"ACC_INFO")
  addr_cus = ACC_INFO.ADDRESS
ELSE
  addr_cus = ""
ENDIF
USE IN ACC_INFO

SELECT ryn_m_t0

APPEND BLANK
REPLACE ORD_NUM   WITH n_doc, ;
        ORD_DATE  WITH DATE(),     ;
        MOVE_TYPE WITH "A",    ;
        TECHNTP   WITH tmType,    ;
        TECHNID   WITH tc_id,  ;
        SUP_CODE  WITH f_code,;
        CUS_CODE  WITH sup_own, ;
        TERMS     WITH DECL_TIT.TERMS,    ;
        SHIP      WITH DECL_TIT.SHIP,     ;
        DELIVERY  WITH DECL_TIT.DELIVERY, ;
        NOTE      WITH DECL_TIT.NOTE,     ;
        FOR_CODE  WITH DECL_TIT.CUS_CODE, ;
        FOR_NAME  WITH nam_cus, ;
        FOR_ADDR  WITH addr_cus ,;
        WHO_ORD	  WITH DECL_TIT.WHO,;
        WHERE_ORD WITH DECL_TIT.WHERE

IF NOT EMPTY(FSIZE("SPEC_NUM"))        
REPLACE  HOLD	   WITH DECL_TIT.HOLD,;
       	 SPEC_NUM  WITH DECL_TIT.SPEC_NUM,;
         SPEC_DATE WITH DECL_TIT.SPEC_DATE,;
         MOD_DATE  WITH DATE()
ENDIF         

SELECT 0
DO OP_MT WITH "MATERS"
tPursys = MATERS.SUBSYS

SELECT 0
USE (base_store+"\ORDER")IN 0 ALIAS RYN_M0 AGAIN

SELECT DECL_TIT
REPLACE TECHNTP  WITH tmType,  ;
        TECHNID  WITH tc_id

SELECT DECL_LST
SEEK STR(YEAR(DECL_TIT.DOC_DATE),4)+STR(DECL_TIT.DOC_NUM, FSIZE("DOC_NUM"))
SCAN REST WHILE DECL_TIT.DOC_DATE = DOC_DATE .AND. DECL_TIT.DOC_NUM = DOC_NUM
  IF QTY <= 0 .OR. .NOT. SEEK(STO_CODE, "STO_AS") .AND. STO_CODE # 0
    err_level = MAX(err_level,1)
    LOOP
  ENDIF
  tmpMet = MT_ALL("IDL_MT","MATERS")
  SELECT RYN_M0
  APPEND BLANK
  REPLACE ORD_NUM   WITH n_doc,            ;
          ORD_DATE  WITH DATE(),           ;
          CODE      WITH DECL_LST.STO_CODE,;
          PREFIX    WITH STO_AS.PREFIX,    ;
          NAME      WITH STO_AS.NAME,      ;
          PRODUCER  WITH STO_AS.PRODUCER,  ;
          QNT       WITH DECL_LST.QTY,     ;
          PRICE     WITH DECL_LST.PRICE,   ;
          PURSYS    WITH tPursys,    ;
          IDL       WITH tmpMet
  SELECT DECL_LST
  REPLACE QTY_RPL WITH -RYN_M0.QNT
ENDSCAN

IF USED("MATERS")
SELECT MATERS
USE
ENDIF
SELECT RYN_M_T0
USE
SELECT RYN_M0
USE

SELECT (s_sav)

ELSE
=MESSAGEBOX("У вас не указан филиал, для которого готовим заявку.Повторите операцию!",0+16+0)
ENDIF

RETURN

*г========================================================================м
*ж                                                                        ж
*ж                         Бронирование списка.                           ж
*ж                                                                        ж
*L==================================================== 13.10.97 18:01:34 =-
PROCEDURE Bron_L
PARAMETER is_rez

PRIVATE q0, t_w, d_w, b_stock, e_stock, q_pos, q_neg, tmpName

b_stock = "00:00"   && Время работы
e_stock = "23:59"   &&          склада
SELECT DECL_LST
IF .NOT. SEEK(VAL(ALLTRIM(DECL_TIT.LST_NUM)), "LT_AS")
  SELECT DECL_TIT
  REPLACE FLAG WITH "2"
  RETURN
ENDIF

tmpName = SYS(3)
SELECT 0
CREATE DBF (SQLName+tmpName) (LINK N(10), ADDRESS C(16), ZONE C(2))
USE (SQLName+tmpName) EXCLUSIVE ALIAS TMP_LIST
SELECT 0
USE (base_store+"\ZONES") ORDER TAG ZONE ALIAS ZONE_AS AGAIN

SELECT DECL_LST
SEEK STR(YEAR(DECL_TIT.DOC_DATE),4)+STR(DECL_TIT.DOC_NUM, FSIZE("DOC_NUM"))
SCAN REST WHILE DECL_TIT.DOC_DATE = DOC_DATE .AND. DECL_TIT.DOC_NUM = DOC_NUM
  IF .NOT. SEEK(STO_CODE, "STO_AS")
    err_level = MAX(err_level,1)
    LOOP
  ENDIF
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE LINK    WITH RECNO("DECL_LST"), ;
          ADDRESS WITH IIF(EMPTY(STO_AS.MEMO_ST), STO_AS.ADDRESS, STO_AS.MEMO_ST)
  REPLACE ZONE WITH IIF(SEEK(LEFT(ADDRESS,2), "ZONE_AS"), LEFT(ADDRESS,2), "**" )
  SELECT DECL_LST
ENDSCAN

SELECT TMP_LIST
INDEX ON ZONE+ADDRESS TAG ZONE

PRIVATE curZone, to_ask
curZone = ""
to_ask  = .T.

SCAN
  GO TMP_LIST.LINK IN DECL_LST
  = SEEK(DECL_LST.STO_CODE, "STO_AS")
  SELECT LD_AS
  IF .NOT. SEEK(DECL_TIT.LST_NUM+STR(DECL_LST.STO_CODE,7))
    = SEEK(DECL_TIT.LST_NUM)
    LOCATE REST WHILE LIST_NUM = DECL_TIT.LST_NUM .AND. CODE = 0 FOR ;
         NAME = DECL_LST.NAME .AND. PREFIX = DECL_LST.PREFIX .AND. PRODUCER = DECL_LST.PRODUCER
    IF .NOT. FOUND()
      err_level = MAX(err_level,1)
      LOOP
    ENDIF
    REPLACE LD_AS.CODE WITH DECL_LST.STO_CODE
  ENDIF
  q0 = DECL_LST.QTY
  IF is_rez
    IF q0 < 0
      q0 = -MIN(-q0, QNT_REQ)
    ELSE
      q0 = MIN(STO_AS.QNT_REZ, q0, QNT_REZ)
    ENDIF
  ELSE
    IF q0 < 0
      q0 = -MIN(-q0, QNT_REQ)
    ELSE
      q0 = MIN(STO_AS.QNT, q0, QNT_DUE)
    ENDIF
  ENDIF
  IF q0 = 0
    err_level = MAX(err_level,1)
    LOOP
  ENDIF
  IF q0 # DECL_LST.QTY
    err_level = MAX(err_level,1)
  ENDIF
  IF is_rez
    REPLACE QNT_REQ WITH QNT_REQ+q0, QNT_REZ WITH QNT_REZ-q0
    REPLACE STO_AS.QNT_REZ WITH STO_AS.QNT_REZ-q0, STO_AS.QNT_SAVE WITH STO_AS.QNT_SAVE+q0
  ELSE
    REPLACE QNT WITH QNT+q0, QNT_REQ WITH QNT_REQ+q0, QNT_DUE WITH QNT_DUE-q0
    REPLACE STO_AS.QNT WITH STO_AS.QNT-q0, STO_AS.QNT_SAVE WITH STO_AS.QNT_SAVE+q0
  ENDIF
*  IF EMPTY(n_ord)
  IF .NOT. curZone == TMP_LIST.ZONE
    IF n_ord # 0
      REPLACE OT_AS.SIZE_NEG WITH q_neg, OT_AS.SIZE_POS WITH q_pos
    ENDIF
    STORE 0 TO q_pos, q_neg
    curZone = TMP_LIST.ZONE
    n_ord = LT_AS.ORD_NUM+1
    REPLACE LT_AS.ORD_NUM WITH n_ord
    SELECT OT_AS

    APPEND BLANK
    REPLACE LIST_NUM  WITH STR(LT_AS.LST_NUM,6), ;
            LIST_NAME WITH LT_AS.LST_NAME,       ;
            ORD_NUM   WITH n_ord,     ;
            ORD_DATE  WITH DATE(),    ;
            ORD_TIME  WITH TIME(),    ;
            SOURCE    WITH IIF(is_rez, "1", " "),;
            ZONE      WITH curZone,   ;
            QUEUE     WITH LT_AS.ORD_QUEUE, ;
            WHO       WITH DECL_TIT.WHO, ;
            WHERE     WITH DECL_TIT.WHERE,;
            NOTE	  WITH DECL_TIT.NOTE
     IF NOT EMPTY(FSIZE("REQ_DATE"))
*!*	       d_w = DATE()
*!*	       t_w = TIME()
       IF to_ask
         d_w = DECL_TIT.Date_ord
         t_w = DECL_TIT.Time_ord
	     DO Add_T WITH d_w, t_w, 5, d_w, t_w
	     to_ask = .F.
	   ENDIF
       REPLACE REQ_DATE	WITH d_w, ;
               REQ_TIME	WITH t_w
     ENDIF

  ENDIF
  SELECT OD_AS
  APPEND BLANK
  REPLACE LIST_NUM  WITH OT_AS.LIST_NUM, ;
          ORD_NUM   WITH n_ord,     ;
          CODE      WITH LD_AS.CODE,;
          QNT       WITH q0
  IF QNT < 0
    q_neg = q_neg+1
  ELSE
    q_pos = q_pos+1
  ENDIF
  SELECT DECL_LST
  REPLACE QTY_RPL WITH q0
  SELECT TMP_LIST
ENDSCAN

IF n_ord # 0
  SELECT OT_AS
  REPLACE SIZE_NEG WITH q_neg, SIZE_POS WITH q_pos
ENDIF
SELECT TMP_LIST
USE
DELETE FILE (SQLName+tmpName+".dbf")
DELETE FILE (SQLName+tmpName+".cdx")
SELECT ZONE_AS
USE
SELECT DECL_TIT
IF n_ord # 0
  REPLACE RESULT WITH n_ord, RES_DATE WITH DATE(), FLAG WITH STR(err_level,1)
ENDIF

RETURN
