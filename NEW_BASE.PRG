PROCEDURE New_Base
PRIVATE pth_old, pth_new, pth_shab
*pth_old  = "D:\VASIN\SALE\SALE\BASE\"
pth_old  = "Z:\STORE\BASE\"
pth_new  = "Z:\STORE\NEW_BASE\"
pth_shab = "BASE\"
SET EXCLU OFF
SET TALK OFF
set dele on
DO Set_Lwr
CLEAR

DO P_Copy WITH pth_old, pth_new, pth_shab

*ABSENT.DBF   Как есть   
DO C_Cont WITH "ABSENT"

*ACCOUNT.DBF  Убрать поля
*CUS_REST.DBF Заполнить: старт. значения = остаткам
DO Acc_F WITH pth_old, pth_new, pth_shab

*ACT_F.DBF    Очистить   
DO C_Stru WITH "ACT_F"

*ACT_F_T.DBF  Очистить   
DO C_Stru WITH "ACT_F_T"

*ACT_S.DBF    Подобрать остатки подставить предпр.
*ACT_S_T.DBF            +суммы, проценты налога...
DO Fill_Act WITH pth_old, pth_new, pth_shab

*ANALOG.DBF   Оставить   
DO C_Cont WITH "ANALOG.DBF"

*ARM_LST.DBF  Переставить остатки
DO Copy_1 WITH pth_old, pth_new, pth_shab

*BAL_LOG.DBF  Оставить   
DO C_Cont WITH "BAL_LOG.DBF"

*BANKS.DBF    Заполнить
DO Fill_B

*BAS_LIST.DBF Оставить   
DO C_Cont WITH "BAS_LIST.DBF"

*BILLS.DBF    Переписать неоплаченные
*BILLS_D.DBF  По одной строке для заголовка
DO Bills_F WITH pth_old, pth_new, pth_shab

*BILL_F.DBF   Очистить   
DO C_Stru WITH "BILL_F"

*BUHG_REQ.DBF Очистить   
DO C_Stru WITH "BUHG_REQ"

*CLI_INFO.DBF Оставить   
DO C_Cont WITH "CLI_INFO"

*CNTDESCR.DBF Заполнить
USE (pth_shab+"CNTDESCR")
COPY TO (pth_new+"CNTDESCR") WITH CDX
USE

*CORR_LST.DBF Очистить   
DO C_Stru WITH "CORR_LST"

*CORR_TIT.DBF Очистить   
DO C_Stru WITH "CORR_TIT"

*COURSE.DBF   Описать типы курсов
*CURS_SET.DBF Сформировать на основе CURS
DO Curs_M

*CUPURY.DBF   Оставить   
DO C_Cont WITH "CUPURY"

*CUSTOMER.DBF Оставить   
DO C_Cont WITH "CUSTOMER"

*CUS_BIL.DBF  Оставить   
DO C_Cont WITH "CUS_BIL"

*DAYS_T.DBF   Оставить   
DO C_Cont WITH "DAYS_T"

*DEF_PATH.DBF Оставить   
DO C_Cont WITH "DEF_PATH"

*DEMAND.DBF   Оставить
USE (pth_old+"DEMAND")
COPY STRU TO (pth_new+"DEMAND") WITH CDX
USE
*DO C_Stru WITH "DEMAND"

*DEPS.DBF     Оставить   
DO C_Cont WITH "DEPS"

*DEVELOP.DBF  Оставить   
DO C_Cont WITH "DEVELOP"

*DIS_SCH.DBF  Оставить   
DO C_Cont WITH "DIS_SCH"

*DIVISION.DBF Оставить   
DO C_Cont WITH "DIVISION"

*DOC_CNT.DBF  Пустой? Стартовые номера документов!
DO C_Stru WITH "DOC_CNT"

*DOC_FORM.DBF Править форматы документов, старая структура!     
DO C_Cont WITH "DOC_FORM"

*DOSSIER.DBF  Копировать 
DO C_Cont WITH "DOSSIER"

*ERR_LOG.DBF  Оставить   
DO C_Cont WITH "ERR_LOG"

*FIRMS.DBF    Заполнить
DO Fill_F

*F_VIEW.DBF   Оставить   
DO C_Cont WITH "F_VIEW"

*GOODS.DBF    Оставить   
DO C_Cont WITH "GOODS"

*HELP.DBF     Оставить   
DO C_Cont WITH "HELP"

*INCMONEY.DBF Очистить - новая структура           
DO C_Stru WITH "INCMONEY"

*INC_DUE.DBF  Очистить   
DO C_Stru WITH "INC_DUE"

*INC_LIST.DBF Очистить   
DO C_Stru WITH "INC_LIST"

*INC_LOG.DBF  Очистить   
DO C_Stru WITH "INC_LOG"

*INC_NEW.DBF  Очистить   
DO C_Stru WITH "INC_NEW"

*INC_TIT.DBF  Очистить   
DO C_Stru WITH "INC_TIT"

*KARJERA.DBF  Оставить   
DO C_Cont WITH "KARJERA"

*KASSA.DBF    Очистить - новая структура           
DO C_Stru WITH "KASSA"

*KASSA_M.DBF  Очистить   
DO C_Stru WITH "KASSA_M"

*KASSA_S.DBF  Расставить остатки
DO C_Cont WITH "KASSA_S"
USE (pth_new+"KASSA_S")
REPLACE SUM_R_S WITH SUM_R, SUM_D_S WITH SUM_D
USE

*LET_SPEC.DBF Очистить   
DO C_Stru WITH "LET_SPEC"

*LET_SP_T.DBF Очистить   
DO C_Stru WITH "LET_SP_T"

*LIST_DET.DBF Скорректировать в соответствии со структ.
*LIST_TIT.DBF Скорректировать в соответствии со структ.
DO Fill_L WITH pth_old, pth_new, pth_shab

*LS_TABLE.DBF Оставить   
DO C_Cont WITH "LS_TABLE"

*MAN_REP.DBF  Оставить   
DO C_Cont WITH "MAN_REP"

*MENU_LST.DBF Оставить   
DO C_Cont WITH "MENU_LST"

*MM_GROUP.DBF Оставить   
DO C_Cont WITH "MM_GROUP"

*MM_TYPES.DBF Оставить   
DO C_Cont WITH "MM_TYPES"

*MOULD.DBF    Оставить   
DO C_Cont WITH "MOULD"

*MSS_LIST.DBF Оставить   
&&     DO C_Cont WITH "MSS_LIST"

*NEWS.DBF     Оставить   
DO C_Cont WITH "NEWS"

*NIGHT.DBF    Оставить   
DO C_Cont WITH "NIGHT"

*OFFER.DBF    Оставить   
DO C_Cont WITH "OFFER"

*OPERSTRU.DBF Оставить   
DO C_Cont WITH "OPERSTRU"

*ORDER.DBF    Очистить   
DO C_Stru WITH "ORDER"

*ORD_D.DBF    Удалить утв. и без спсиков!
*ORD_T.DBF    Удалить утв. и без спсиков!
DO Fill_Ord WITH pth_old, pth_new, pth_shab

*ORD_L.DBF    Очистить   
DO C_Stru WITH "ORD_L"

*ORD_TIT.DBF  Очистить   
DO C_Stru WITH "ORD_TIT"

*ORD_T_L.DBF  Очистить   
DO C_Stru WITH "ORD_T_L"

*OWN_DIRS.DBF Не нужен!  

*PARMS.DBF    Исправить структуру и содержимое     

*PATTERN.DBF  Оставить   
DO C_Cont WITH "PATTERN"

*PERSONS.DBF  Оставить   
DO C_Cont WITH "PERSONS"

*PLACES.DBF   Оставить   
DO C_Cont WITH "PLACES"

*PRN_DIRS.DBF Оставить   
DO C_Cont WITH "PRN_DIRS"

*PS_STAT.DBF  Оставить   
DO C_Cont WITH "PS_STAT"

*RCV.DBF      Оставить   
DO C_Cont WITH "RCV"

*RCV_T.DBF    Оставить   
DO C_Cont WITH "RCV_T"

*REFUSAL.DBF  Оставить   
USE (pth_old+"REFUSAL")
COPY STRU TO (pth_new+"REFUSAL") WITH CDX
USE
*DO C_Cont WITH "REFUSAL"

*REPORT.DBF   Новая структура
DO C_Stru WITH "REPORT"

*REP_TIT.DBF  Очистить   
DO C_Stru WITH "REP_TIT"

*RIGHTS.DBF   Оставить   
DO C_Cont WITH "RIGHTS"

*RYN_1_L.DBF  Очистить
DO C_Stru WITH "RYN_1_L"

*RYN_1_T.DBF  Очистить
DO C_Stru WITH "RYN_1_T"

*RYN_L.DBF    Оставить   
DO C_Cont WITH "RYN_L"

*RYN_L_T.DBF  Оставить   
DO C_Cont WITH "RYN_L_T"

*RYN_M.DBF    Очистить   
DO C_Stru WITH "RYN_M"

*RYN_M_T.DBF  Очистить   
DO C_Stru WITH "RYN_M_T"

*SALE.DBF     Очистить   
DO C_Stru WITH "SALE"

*SALE_TIT.DBF Очистить   
DO C_Stru WITH "SALE_TIT"

*SLR_MOVE.DBF Оставить   
DO C_Cont WITH "SLR_MOVE"

*SLR_TYPE.DBF Оставить   
DO C_Cont WITH "SLR_TYPE"

*SPEC_SUM.DBF Оставить   
DO C_Cont WITH "SPEC_SUM"

*STOCK.DBF    Скорректировать структуру и переставить остатки   
DO Sto_Set WITH pth_old, pth_new, pth_shab

*STO_DEM.DBF  Оставить   
DO C_Cont WITH "STO_DEM"

*STO_LOG.DBF  Оставить   
DO C_Cont WITH "STO_LOG"

*S_RIGHTS.DBF Оставить   
DO C_Cont WITH "S_RIGHTS"

*TMP.DBF      Не нужен!  

*TMP_NAME.DBF Не нужен!  

*TR_LOG.DBF   Оставить   
DO C_Cont WITH "TR_LOG"

*TST.DBF      Не нужен   

*TXT_DOCS.DBF Оставить   
DO C_Cont WITH "TXT_DOCS"

*T_LIST.DBF   Не нужен!  

*USERS.DBF    Оставить   
DO C_Cont WITH "USERS"

*WK_LIST.DBF  Оставить   
DO C_Cont WITH "WK_LIST"

*WK_TIT.DBF   Оставить   
DO C_Cont WITH "WK_TIT"

*WORK_LOG.DBF Оставить
DO C_Cont WITH "WORK_LOG"

*********************************************************************
PROCEDURE C_Cont
PARAMETER f_name
? "Файл "+f_name+" копируем содержимое... "
USE (pth_old+f_name)
COPY TO (pth_new+f_name) WITH CDX
?? "Реиндексация..."
USE (pth_new+f_name) EXCLU
REINDEX
USE
?? " Все!"
RETURN

*********************************************************************
PROCEDURE C_Stru
PARAMETER f_name
? "Файл "+f_name+" копируем структуру... "
USE (pth_shab+f_name)
COPY STRUCTURE TO (pth_new+f_name) WITH CDX
?? "Реиндексация..."
USE (pth_new+f_name) EXCLU
REINDEX
USE
?? " Все!"
RETURN

*********************************************************************
*
*  Обновление курсов...
*
PROCEDURE Curs_M
c1 = 0
c2 = 0
c3 = 0
c4 = 0
CLOSE DATA
USE (pth_shab+"COURSE")
COPY TO (pth_new+"COURSE") WITH CDX

USE (pth_shab+"CURS_SET")
COPY STRUCTURE TO (pth_new+"CURS_SET") WITH CDX

USE (pth_new+"CURS_SET") ALIAS NEW
SELECT 0
USE (pth_old+"CURS") ORDER TAG HRON ALIAS OLD
SCAN
  IF c1 # CURS
    SELECT NEW
    APPEND BLANK
    REPLACE B_NAME   WITH "CURS",       ;
            DATE_SET WITH OLD.DATE_SET, ;
            TIME_SET WITH OLD.TIME_SET, ;
            CURS     WITH OLD.CURS,     ;
            WHO      WITH OLD.WHO_SET,  ;
            WHERE    WITH OLD.WHERE_SET
    SELECT OLD
    c1 = CURS
  ENDIF
  IF c2 # CURS_B
    SELECT NEW
    APPEND BLANK
    REPLACE B_NAME   WITH "CURS_B",     ;
            DATE_SET WITH OLD.DATE_SET, ;
            TIME_SET WITH OLD.TIME_SET, ;
            CURS     WITH OLD.CURS_B,   ;
            WHO      WITH OLD.WHO_SET,  ;
            WHERE    WITH OLD.WHERE_SET
    SELECT OLD
    c2 = CURS_B
  ENDIF
  IF c3 # CURS_N
    SELECT NEW
    APPEND BLANK
    REPLACE B_NAME   WITH "CURS_N",     ;
            DATE_SET WITH OLD.DATE_SET, ;
            TIME_SET WITH OLD.TIME_SET, ;
            CURS     WITH OLD.CURS_N,   ;
            WHO      WITH OLD.WHO_SET,  ;
            WHERE    WITH OLD.WHERE_SET
    SELECT OLD
    c3 = CURS_N
  ENDIF
  IF c4 # CURS_MMVB
    SELECT NEW
    APPEND BLANK
    REPLACE B_NAME   WITH "CURS_MMVB",  ;
            DATE_SET WITH OLD.DATE_SET, ;
            TIME_SET WITH OLD.TIME_SET, ;
            CURS     WITH OLD.CURS_MMVB,;
            WHO      WITH OLD.WHO_SET,  ;
            WHERE    WITH OLD.WHERE_SET
    SELECT OLD
    c4 = CURS_MMVB
  ENDIF
ENDSCAN

CLOSE DATA

RETURN



*********************************************************************
*
* Список рабочих мест
PROCEDURE Copy_1
PARAMETERS pth_old, pth_new, pth_shab
PRIVATE ALL
USE (pth_shab+"ARM_LST")
COPY STRUCTURE TO (pth_new+"ARM_LST") WITH CDX
USE (pth_new+"ARM_LST") EXCLU
REINDEX
SELECT 0
USE (pth_old+"ARM_LST") ALIAS OLD
SCAN
  SCATTER MEMVAR
  SELECT ARM_LST
  APPEND BLANK
  GATHER MEMVAR
  REPLACE S_KASSA WITH KASSA
  SELECT OLD
ENDSCAN

CLOSE DATABASES

RETURN

*********************************************************************
*
* Список предприятий
*
PROCEDURE Fill_F

USE (pth_shab+"FIRMS")
COPY STRUCTURE TO (pth_new+"FIRMS") WITH CDX
USE (pth_new+"FIRMS")
APPEND BLANK
REPLACE FIRM_CODE WITH "CMP",    ;
        B_NAME    WITH "Компэл", ;
        L_NAME    WITH "Компэл"
APPEND BLANK
REPLACE FIRM_CODE WITH "C00",    ;
        B_NAME    WITH "Компэл 0", ;
        L_NAME    WITH "Компэл 0"
APPEND BLANK
REPLACE FIRM_CODE WITH "MTK",    ;
        B_NAME    WITH "МИТРАКОН", ;
        L_NAME    WITH "МИТРАКОН"
APPEND BLANK
REPLACE FIRM_CODE WITH "M00",    ;
        B_NAME    WITH "МИТРАКОН 0", ;
        L_NAME    WITH "МИТРАКОН 0"
USE

*********************************************************************
*
* Список банков
*
PROCEDURE Fill_B

USE (pth_shab+"BANKS")
COPY STRUCTURE TO (pth_new+"BANKS") WITH CDX
USE
RETURN
USE (pth_new+"BANKS")
APPEND BLANK
REPLACE FIRM_CODE WITH "CMP", ;
        BANK_CODE WITH "B00", ;
        B_NAME    WITH "?",   ;
        L_NAME    WITH "??"

APPEND BLANK
REPLACE FIRM_CODE WITH "CMP", ;
        BANK_CODE WITH "B01", ;
        B_NAME    WITH "!",   ;
        L_NAME    WITH "!!"

USE

*********************************************************************
*
* Остатки по партиям
*
PROCEDURE Fill_Act
PARAMETERS pth_old, pth_new, pth_shab
PRIVATE all

USE (pth_shab+"ACT_S_T")
COPY STRUCTURE TO (pth_new+"ACT_S_T") WITH CDX

cnt = 0
? "ACT_S_T... "+STR(RECCOUNT())+" 0000000000"
USE (pth_new+"ACT_S_T") ALIAS NEW
SELECT 0
USE (pth_old+"ACT_S_T") ALIAS OLD
SCAN
  SCATTER MEMVAR
  SELECT NEW
  APPEND BLANK
  IF m.inc_type = "0"
    m.inc_type = " "
    m.firm = "C00"
  ELSE
    m.firm = "CMP"
  ENDIF
  m.nds_ = 20
  m.valuta = "Р"
  GATHER MEMVAR
  SELECT OLD
  cnt = cnt+1
  IF cnt % 100 = 0
    @ ROW(), COL()-10 SAY STR(cnt)
  ENDIF
ENDSCAN
USE (pth_old+"ACT_S") ALIAS OLD
SELECT NEW
USE (pth_shab+"ACT_S")
COPY STRUCTURE TO (pth_new+"ACT_S") WITH CDX
USE (pth_new+"ACT_S") ALIAS NEW
SELECT 0
USE (pth_old+"CURS") ORDER TAG HRON DESCENDING
SELECT OLD
? "ACT_S... "+STR(RECCOUNT())+" 0000000000"
cnt = 0
SCAN
  SCATTER MEMVAR
  SELECT CURS
  c_w = 0
  IF .NOT. SEEK(DTOS(m.doc_date))
    IF BETWEEN(RECNO(0), 1, RECCOUNT())
      GO RECNO(0)
      c_w = CURS
    ENDIF
  ELSE
    c_w = CURS
  ENDIF
  SELECT NEW
  APPEND BLANK
  IF m.inc_type = "0"
    m.inc_type = " "
    m.firm = "C00"
  ELSE
    m.firm = "CMP"
  ENDIF
  m.price_0 = 5*m.price_r/6
  m.price_d = IIF(c_w = 0, 0, m.price_r/c_w)
  GATHER MEMVAR
  SELECT OLD
  cnt = cnt+1
  IF cnt % 100 = 0
    @ ROW(), COL()-10 SAY STR(cnt)
  ENDIF
ENDSCAN

CLOSE DATABASES

*********************************************************************
*
* Заполнение счетов
*
PROCEDURE Bills_F

PARAMETERS pth_old, pth_new, pth_shab
PRIVATE all

USE (pth_shab+"BILLS")
COPY STRU TO (pth_new+"BILLS") WITH CDX
USE (pth_new+"BILLS") ALIAS NEW

SELECT 0
USE (pth_shab+"BILLS_D")
COPY STRU TO (pth_new+"BILLS_D") WITH CDX
USE (pth_new+"BILLS_D") ALIAS NEW_D

SELECT 0
USE (pth_old+"BILLS") ALIAS OLD
m.name = "Эл. кмпоненты в ассорт."
m.nds_ = 20
m.valuta = "Р"
m.bank = "B00"
SCAN
  IF ALLTRIM(STATUS) = "Оплачено"
    LOOP
  ENDIF
  SCATTER MEMVAR
  SELECT NEW
  APPEND BLANK
  m.firm = "CMP"
  m.inc_firm = IIF(m.inc_type = "0", "C00", "CMP")
  m.inc_type = IIF(m.inc_type = "0", " ", m.inc_type)
  m.sum_0 = 5*m.bill_sum/6
  m.sum_t = m.bill_sum
  GATHER MEMVAR
  SELECT NEW_D
  APPEND BLANK
  GATHER MEMVAR
  SELECT OLD
ENDSCAN

CLOSE DATA

*********************************************************************
*
* Заполнение депозитов
*
PROCEDURE Acc_F
PARAMETERS pth_old, pth_new, pth_shab
PRIVATE ALL EXCEPT lwr

USE (pth_shab+"ACCOUNT")
COPY STRU TO (pth_new+"ACCOUNT") WITH CDX
USE (pth_new+"ACCOUNT") ALIAS NEW

SELECT 0
USE (pth_shab+"CUS_REST")
COPY STRU TO (pth_new+"CUS_REST") WITH CDX
USE (pth_new+"CUS_REST") ALIAS NEW_D

SELECT 0
USE (pth_old+"ACCOUNT") ALIAS OLD

cnt = 0
? "ACCOUNT... "+STR(RECCOUNT())+" 0000000000"
SCAN
  SCATTER MEMVAR
  SELECT NEW
  APPEND BLANK
  GATHER MEMVAR
  IF ACC_SUM # 0 .OR. ACC_SUM0 # 0 .OR. ACC_SUMD # 0 .OR. ;
     ACC_S_H # 0 .OR. ACC_S0_H # 0 .OR. ACC_SD_H # 0 .OR. ;
     CR_SUM  # 0 .OR. CR0_SUM  # 0 .OR. CRD_SUM  # 0
    SELECT NEW_D
    IF m.ACC_SUM # 0 .OR. m.ACC_SUMD # 0 .OR. ;
       m.ACC_S_H # 0 .OR. m.ACC_SD_H # 0 .OR. ;
       m.CR_SUM  # 0 .OR. m.CRD_SUM  # 0
      m.acc_s_s  = m.acc_sum+m.acc_s_h
      m.acc_sd_s = m.acc_sumd+m.acc_sd_h
      m.firm = "CMP"
      APPEND BLANK
      GATHER MEMVAR
    ENDIF
    IF m.ACC_SUM0 # 0 .OR. m.ACC_S0_H # 0 .OR. m.CR0_SUM # 0
      m.acc_sum = m.acc_sum0
      m.acc_s_h = m.acc_s0_h
      m.cr_sum = m.cr0_sum
      m.acc_sumd = 0
      m.acc_sd_h = 0
      m.crd_sum  = 0
      m.acc_s_s = m.acc_sum+m.acc_s_h
      m.acc_sd_s = m.acc_sumd+m.acc_sd_h
      m.firm = "C00"
      APPEND BLANK
      GATHER MEMVAR
    ENDIF
  ENDIF
  SELECT OLD
  cnt = cnt+1
  IF cnt % 100 = 0
    @ ROW(), COL()-10 SAY STR(cnt)
  ENDIF
ENDSCAN

CLOSE DATA

RETURN

*********************************************************************
*
* Заполнение списков
*
PROCEDURE Fill_L
PARAMETERS pth_old, pth_new, pth_shab
PRIVATE ALL EXCEPT lwr

? "LIST_TIT... "
USE (pth_shab+"LIST_TIT")
COPY STRU TO (pth_new+"LIST_TIT") WITH CDX
USE (pth_new+"LIST_TIT") ALIAS NEW
APPEND FROM (pth_old+"LIST_TIT") FOR EMPTY(SAL_NUM)
REPLACE ALL BILL_FIRM WITH "CMP", FIRM WITH "CMP"
SET ORDER TO TAG LST_NUM

SELECT 0
USE (pth_shab+"LIST_DET")
COPY STRU TO (pth_new+"LIST_DET") WITH CDX
USE (pth_new+"LIST_DET") ALIAS NEW_D

SELECT 0
USE (pth_old+"LIST_DET") ALIAS OLD

cnt = 0
? "LIST_DET... "+STR(RECCOUNT())+" 0000000000"
SCAN
  cnt = cnt+1
  IF cnt % 100 = 0
    @ ROW(), COL()-10 SAY STR(cnt)
  ENDIF
  IF .NOT. SEEK(VAL(ALLTRIM(LIST_NUM)), "NEW")
    LOOP
  ENDIF
  SCATTER MEMVAR
  SELECT NEW_D
  APPEND BLANK
  GATHER MEMVAR
  SELECT OLD
ENDSCAN

CLOSE DATA

RETURN

*********************************************************************
*
* Заполнение заявок на склад
*
PROCEDURE Fill_Ord
PARAMETERS pth_old, pth_new, pth_shab
PRIVATE ALL EXCEPT lwr

USE (pth_shab+"ORD_T")
COPY STRU TO (pth_new+"ORD_T") WITH CDX
USE (pth_new+"LIST_TIT") ORDER TAG LST_NUM
SELECT 0
USE (pth_new+"ORD_T") ALIAS NEW
SELECT 0
USE (pth_old+"ORD_T") ALIAS OLD

cnt = 0
? "ORD_T... "+STR(RECCOUNT())+" 0000000000"
SCAN
  cnt = cnt+1
  IF cnt % 100 = 0
    @ ROW(), COL()-10 SAY STR(cnt)
  ENDIF
  IF .NOT. SEEK(VAL(ALLTRIM(LIST_NUM)), "LIST_TIT") .AND. .NOT. EMPTY(STO_MARK) .AND. .NOT. EMPTY(WHO)
    LOOP
  ENDIF
  SCATTER MEMVAR
  SELECT NEW
  APPEND BLANK
  GATHER MEMVAR
  SELECT OLD
ENDSCAN
USE
SELECT LIST_TIT
USE 
SELECT NEW
SET ORDER TO DOC_NUM
SELECT 0
USE (pth_old+"ORD_D") ALIAS OLD_D
COPY STRU TO (pth_new+"ORD_D") WITH CDX
SELECT 0
USE (pth_new+"ORD_D") ALIAS NEW_D
SELECT OLD_D

cnt = 0
? "ORD_D... "+STR(RECCOUNT())+" 0000000000"
SCAN
  cnt = cnt+1
  IF cnt % 100 = 0
    @ ROW(), COL()-10 SAY STR(cnt)
  ENDIF
  IF .NOT. SEEK(LIST_NUM, "NEW")
    LOOP
  ENDIF
  SCATTER MEMVAR
  SELECT NEW_D
  APPEND BLANK
  GATHER MEMVAR
  SELECT OLD_D
ENDSCAN

CLOSE DATA

RETURN

*********************************************************************
*
* Заполнение файла параметров (ЛАПАМИ!)
*
PROCEDURE P_Copy
PARAMETERS pth_old, pth_new, pth_shab
PRIVATE ALL EXCEPT lwr

USE (pth_shab+"PARMS")
COPY STRU TO (pth_new+"PARMS")
USE (pth_old+"PARMS")
SCATTER MEMVAR
USE (pth_new+"PARMS")
APPEND BLANK
GATHER MEMVAR

EDIT

USE

*********************************************************************
*
* Заполнение номенклатуры
*
PROCEDURE Sto_Set
PARAMETERS pth_old, pth_new, pth_shab
PRIVATE ALL EXCEPT lwr

USE (pth_shab+"STOCK")
COPY STRU TO (pth_new+"STOCK") WITH CDX
USE (pth_new+"STOCK") ALIAS NEW
SELECT 0
USE (pth_old+"STOCK") ALIAS OLD

cnt = 0
? "STOCK... "+STR(RECCOUNT())+" 0000000000"
SCAN
  cnt = cnt+1
  IF cnt % 100 = 0
    @ ROW(), COL()-10 SAY STR(cnt)
  ENDIF
  SCATTER MEMVAR
  SELECT NEW
  APPEND BLANK
  m.qnt_start = m.qnt+m.qnt_save
  m.qnt_s_rep = m.qnt_rep
  m.qnt_s_ryn = m.qnt_real
  m.qnt_s_spec = m.qnt_spec
  GATHER MEMVAR
  SELECT OLD
ENDSCAN
USE
SELECT NEW
USE 

RETURN
