*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Справка: история депозита.                         ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Acc_Hist
PARAMETERS c_code, f_out, dp_type

PRIVATE what_do, mss, sel_sav, c_name, c_type, d_s, pic, menu_name
PRIVATE f_names, f_status, f_point, i_w, e_w, i, tmpw_path
PRIVATE tAltName   && NEW 02.04.2002
tAltName = File_O(base_path + "AltName.DBF")

IF TYPE("f_out") = "C"
  i_w = FULLPATH(f_out)
  i = RAT("\", f_out)
  tmpw_path = LEFT(i_w, i)
ELSE
  tmpw_path = tmpo_path
ENDIF
*
*   Разбираемся с клиентом: кто таков, и есть ли вообще...
*
IF EMPTY(c_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы ошиблись, это не депозит!"
  mss(3) = ""
  IF TYPE("f_out") # "C"
    DO Out_Mess WITH 7, "mss"
  ENDIF
  RETURN
ENDIF

sel_sav = SELECT()
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS ACC_H0 AGAIN
IF .NOT. SEEK(c_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Клиент не найден!"
  mss(3) = ""
  IF TYPE("f_out") # "C"
    DO Out_Mess WITH 7, "mss"
  ENDIF
  USE
  SELECT (sel_sav)
  RETURN
ENDIF
c_name  = CUS_NAME   && Имя клиента
c_type  = "Р"        && Тип депозита
pic     = "@Z 9999999.99"
USE (base_path+"FIRMS") ORDER TAG NAME ALIAS F_9215 AGAIN
f_count = 1
DIMENSION f_status(f_count, 7), f_names(f_count)
f_status(f_count, 1) = "   "
f_status(f_count, 2) = 0
f_status(f_count, 3) = 0
f_status(f_count, 4) = 0
f_status(f_count, 5) = 0
f_status(f_count, 6) = 0
f_status(f_count, 7) = 0
f_names(f_count)     = "Все предприятия "
SCAN
  f_count = f_count+1
  DIMENSION f_status(f_count, 7), f_names(f_count)
  f_status(f_count, 1) = FIRM_CODE
  f_status(f_count, 2) = 0
  f_status(f_count, 3) = 0
  f_status(f_count, 4) = 0
  f_status(f_count, 5) = 0
  f_status(f_count, 6) = 0
  f_status(f_count, 7) = 0
  f_names(f_count)     = B_NAME
ENDSCAN
SET ORDER TO TAG FIRM_CODE
SELECT 0
IF f_count > 1
  USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS CR_9215 AGAIN
  FOR i = 2 TO f_count
    IF SEEK(STR(c_code)+f_status(i, 1))
      f_status(i, 2) = ACC_S_S
      f_status(i, 3) = ACC_SUM+ACC_S_H+REST_SUM
      f_status(i, 4) = ACC_SD_S
      f_status(i, 5) = ACC_SUMD+ACC_SD_H
      f_status(i, 6) = ACC_S_S
      f_status(i, 7) = ACC_SD_S
      f_status(1, 2) = f_status(1, 2)+ACC_S_S
      f_status(1, 3) = f_status(1, 3)+ACC_SUM+ACC_S_H+REST_SUM
      f_status(1, 4) = f_status(1, 4)+ACC_SD_S
      f_status(1, 5) = f_status(1, 5)+ACC_SUMD+ACC_SD_H
      f_status(1, 6) = f_status(1, 6)+ACC_S_S
      f_status(1, 7) = f_status(1, 7)+ACC_SD_S
    ENDIF
  ENDFOR
ENDIF
USE

*
*  Создаем и заполняем временный файл...
*
IF TYPE("f_out") # "C"
  DO Wt_Mess WITH "Подготовка данных..."
ENDIF

USE (base_path+"PARMS") AGAIN ALIAS ACC_H1

d_s = DAT_START
USE

*
*  Создаем...
*
DELETE FILE (tmpw_path+"ACC_HIST.DBF")
DELETE FILE (tmpw_path+"ACC_HIST.CDX")

CREATE DBF (tmpw_path+"ACC_HIST")   ;
     (  DOC_TYPE C( 1),    ;
        DOC_BASE C( 1),    ;
        FIRM     C( 3),    ;
        DOC_NUM  C(10),    ;
        NEW_DOC  C(10),    ;
        NEW_TYPE C( 1),    ;
        PAY_TYPE C( 1),    ;
        DOC_NUMV C(11),    ;
        DOC_DATE D,        ;
        DOC_TIME N( 5),    ;
        DOC_NAME C(16),    ;
        AUTOR    C(32),    ;
        ARM_NM   C(16),    ;
        INC_S    N(14,4),  ;
        EXP_S    N(14,4),  ;
        REST_RF  N(14,4),  ;
        REST_DF  N(14,4),  ;
        REST_RH  N(14,4),  ;
        REST_DH  N(14,4)   )

USE (tmpw_path+"acc_hist") EXCLUSIVE
INDEX ON NEW_TYPE+DTOS(DOC_DATE)+STR(DOC_TIME,5) TAG HRON
INDEX ON NEW_TYPE+FIRM+DTOS(DOC_DATE)+STR(DOC_TIME,5) TAG FIRM ADDITIVE

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS USER_A_H AGAIN

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C ALIAS ARM_A_H AGAIN
*
*  Заполняем поступлениями денег...
*
SELECT 0
USE (base_path+"INCMONEY") ORDER TAG CUS_CODE ALIAS INC_A_H AGAIN
SEEK c_code
SCAN REST WHILE CUS_CODE = c_code FOR DOC_DATE >= d_s
  IF .NOT. SEEK(FIRM, "F_9215")
    LOOP
  ENDIF
  IF .NOT. EMPTY(F_9215.DATE_OFF)
    LOOP
  ENDIF
  SELECT ACC_HIST
  i_w = IIF(INC_A_H.DOC_TYPE $ "34", INC_A_H.MONEY_D, INC_A_H.MONEY_R)
  IF i_w < 0
    e_w = -i_w
    i_w = 0
  ELSE
    e_w = 0
  ENDIF
  APPEND BLANK
  REPLACE NEW_TYPE WITH IIF(INC_A_H.DOC_TYPE $ "34", "3", " "),  ;
          PAY_TYPE WITH INC_A_H.DOC_TYPE,   ;
          DOC_BASE WITH "M",                ;
          FIRM     WITH INC_A_H.FIRM,       ;
          DOC_NUM  WITH INC_A_H.DOC_NUM,    ;
          DOC_NUMV WITH LEFT(INC_A_H.DOC_NUM,1)+"-"+ALLTRIM(SUBSTR(INC_A_H.DOC_NUM,2)),  ;
          DOC_DATE WITH INC_A_H.DOC_DATE,   ;
          DOC_TIME WITH INC_A_H.DOC_TIME,   ;
          DOC_NAME WITH D_Name(INC_A_H.DOC_TYPE, "M", i_w),  ;
          AUTOR    WITH U_Nam(INC_A_H.WHO), ;
          ARM_NM   WITH A_Nam(INC_A_H.WHERE), ;
          INC_S    WITH i_w,     ;
          EXP_S    WITH e_w
  SELECT INC_A_H
ENDSCAN

*
*  Заполняем продажами...
*
USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE ALIAS SAL_A_H AGAIN
SEEK c_code
SCAN REST WHILE CUS_CODE = c_code FOR DOC_DATE >= d_s
  IF .NOT. SEEK(FIRM, "F_9215")
    LOOP
  ENDIF
  IF .NOT. EMPTY(F_9215.DATE_OFF)
    LOOP
  ENDIF
  SELECT ACC_HIST
  e_w = IIF(SAL_A_H.NEW_TYPE # "3", SAL_A_H.TOTAL_R, SAL_A_H.TOTAL_D)
  IF e_w < 0
    i_w = -e_w
    e_w = 0
  ELSE
    i_w = 0
  ENDIF
  APPEND BLANK
  REPLACE DOC_TYPE WITH SAL_A_H.NEW_TYPE,   ;
          DOC_BASE WITH "S",                ;
          DOC_NUMV WITH LEFT(SAL_A_H.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(SAL_A_H.DOC_NUM,5)),  ;
          NEW_DOC  WITH SAL_A_H.NEW_DOC,    ;
          NEW_TYPE WITH SAL_A_H.NEW_TYPE,   ;
          FIRM     WITH SAL_A_H.FIRM,       ;
          DOC_NUM  WITH SAL_A_H.DOC_NUM,    ;
          DOC_DATE WITH SAL_A_H.DOC_DATE,   ;
          DOC_TIME WITH SAL_A_H.DOC_TIME,   ;
          DOC_NAME WITH D_Name(SAL_A_H.NEW_TYPE, "S", i_w),  ;
          AUTOR    WITH U_Nam(SAL_A_H.WHO), ;
          ARM_NM   WITH A_Nam(SAL_A_H.WHERE), ;
          INC_S    WITH i_w,     ;
          EXP_S    WITH e_w
  SELECT SAL_A_H
ENDSCAN
*
*  Заполняем приход...
*
USE (base_path+"ACT_S_T") ORDER TAG SUP_CODE ALIAS ACT_A_H AGAIN
SEEK c_code
SCAN REST WHILE SUP_CODE = c_code FOR DOC_DATE >= d_s
  IF .NOT. SEEK(FIRM, "F_9215")
    LOOP
  ENDIF
  IF .NOT. EMPTY(F_9215.DATE_OFF)
    LOOP
  ENDIF
  SELECT ACC_HIST
  i_w = ACT_A_H.TOTAL_H
  IF i_w < 0
    e_w = -i_w
    i_w = 0
  ELSE
    e_w = 0
  ENDIF
  APPEND BLANK
  REPLACE DOC_TYPE WITH ACT_A_H.INC_TYPE,   ;
          DOC_BASE WITH "A",                ;
          NEW_TYPE WITH IIF(ACT_A_H.VALUTA = "$", "3", " "),  ;
          FIRM     WITH ACT_A_H.FIRM,       ;
          DOC_NUM  WITH ACT_A_H.DOC_NUM,    ;
          DOC_NUMV WITH ALLTRIM(LEFT(ACT_A_H.DOC_NUM,8))+"."+ ;
                        ALLTRIM(RIGHT(ACT_A_H.DOC_NUM,2)),  ;
          DOC_DATE WITH ACT_A_H.DOC_DATE,   ;
          DOC_TIME WITH ACT_A_H.DOC_TIME,   ;
          DOC_NAME WITH D_Name(ACT_A_H.INC_TYPE, "A", i_w),  ;
          AUTOR    WITH U_Nam(ACT_A_H.WHO_ACT), ;
          ARM_NM   WITH A_Nam(ACT_A_H.WHERE_ACT), ;
          INC_S    WITH i_w,     ;
          EXP_S    WITH e_w
  SELECT ACT_A_H
ENDSCAN

*
*  Заполняем утверждения для псевдоклиентов...
*
USE (base_path+"LET_SP_T") ORDER TAG CUS_CODE ALIAS ACT_A_H AGAIN
SEEK c_code
SCAN REST WHILE CUS_CODE = c_code FOR DOC_DATE >= d_s
  tmpTime = VAL(ALLTRIM(LEFT(ACT_A_H.DOC_TIME,2)))*3600
  tmpTime = tmpTime+VAL(ALLTRIM(SUBSTR(ACT_A_H.DOC_TIME,4,2)))*60
  tmpTime = tmpTime+VAL(ALLTRIM(RIGHT(ACT_A_H.DOC_TIME,2)))
  SELECT ACC_HIST
  APPEND BLANK
  REPLACE DOC_BASE WITH "L",                ;
          DOC_NUM  WITH ACT_A_H.DOC_NUM,    ;
          DOC_NUMV WITH ACT_A_H.DOC_NUM,    ;
          DOC_DATE WITH ACT_A_H.DOC_DATE,   ;
          DOC_TIME WITH tmpTime,            ;
          DOC_NAME WITH "Утвержд.",         ;
          AUTOR    WITH U_Nam(ACT_A_H.WHO), ;
          ARM_NM   WITH A_Nam(ACT_A_H.WHERE)
  SELECT ACT_A_H
ENDSCAN
USE
SELECT USER_A_H
USE
SELECT ARM_A_H
USE
*
*   Расчет остатков по предприятиям
*
SELECT ACC_HIST
SET ORDER TO TAG FIRM
IF f_count > 1
  FOR i = 2 TO f_count
    IF SEEK(" "+f_status(i, 1))
      SCAN REST WHILE FIRM = f_status(i, 1) .AND. NEW_TYPE = " "
        f_status(i, 6) = f_status(i, 6)+INC_S-EXP_S
        REPLACE REST_RF WITH f_status(i, 6)
      ENDSCAN
    ENDIF
    IF SEEK("3"+f_status(i, 1))
      SCAN REST WHILE FIRM = f_status(i, 1) .AND. NEW_TYPE = "3"
        f_status(i, 7) = f_status(i, 7)+INC_S-EXP_S
        REPLACE REST_DF WITH f_status(i, 7)
      ENDSCAN
    ENDIF
  ENDFOR
ENDIF

*
*   Расчет общих остатков
*
SET ORDER TO TAG HRON
SCAN
  IF NEW_TYPE = "3"
    f_status(1, 7) = f_status(1, 7)+INC_S-EXP_S
    REPLACE REST_DH WITH f_status(1, 7)
  ELSE
    f_status(1, 6) = f_status(1, 6)+INC_S-EXP_S
    REPLACE REST_RH WITH f_status(1, 6)
  ENDIF
ENDSCAN

IF TYPE("f_out") # "C"
  DO Wt_Mess
  DO V_Docs
ELSE
  PRIVATE k000, f_p, f_t
  IF TYPE("dp_type") = "C"
    k000 = dp_type
  ELSE
    k000 = " "
  ENDIF
  f_p = 1
  f_t = " "
  DO P_H
ENDIF

SELECT F_9215
USE

SELECT ACC_HIST
USE
DELETE FILE (tmpw_path+"ACC_HIST.DBF")
DELETE FILE (tmpw_path+"ACC_HIST.CDX")

SELECT (sel_sav)

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                     Программа печати документов.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE P_D
PRIVATE tpDoc

DEFINE WINDOW H_W_876 FROM 0,0 TO 0, WCOLS("")-1 NONE COLOR SCHEME 13
SHOW WINDOW H_W_876

DO CASE
CASE DOC_BASE = "M"
  DO P_Money WITH FIRM, PAY_TYPE, DOC_NUM, DOC_DATE
CASE DOC_BASE = "S"
  IF SUBSTR(DOC_NUM, 2,3) = "***"
    DO Prn_Inv WITH FIRM, DOC_NUM, DOC_DATE
  ELSE
    tpDoc = What_P()
    DO CASE
    CASE tpDoc  = "N"
      DO Prn_Inv WITH FIRM, DOC_NUM, DOC_DATE	&& .f., tAltName && NEW 02.04.2002
    CASE tpDoc  = "B"
      DO P_Bill_F WITH FIRM, NEW_DOC, DOC_DATE, NEW_TYPE, tAltName && NEW 02.04.2002
    CASE tpDoc  = "C"
      DO P_Check WITH FIRM, DOC_NUM, DOC_DATE	&&, NEW_TYPE, tAltName && NEW 02.04.2002
    ENDCASE
  ENDIF
CASE DOC_BASE = "A"
  DO P_Act WITH FIRM, DOC_NUM, DOC_DATE
CASE DOC_BASE = "L"
  DO P_L_Spec WITH DOC_NUM, DOC_DATE
ENDCASE
RELEASE WINDOW H_W_876

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║               Программа определения имени документа.                  ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE D_Name
PARAMETERS t_d, t_b, s

DO CASE
CASE t_b = "M"
  DO CASE
  CASE t_d = " " .AND. s >= 0
    RETURN "Приходный ордер "
  CASE t_d = " " .AND. s < 0
    RETURN "Расходный ордер "
  CASE t_d = "4"
    RETURN "Б/Н USD         "
  CASE t_d = "1" .AND. s >= 0
    RETURN "Приход Б/Н      "
  CASE t_d = "1" .AND. s < 0
    RETURN "Платежное пор.  "
  CASE t_d = "2" .AND. s >= 0
    RETURN "Приход нал.     "
  CASE t_d = "2" .AND. s < 0
    RETURN "Возврат нал.    "
  CASE t_d = "3" .AND. s >= 0
    RETURN "Приход инд.     "
  CASE t_d = "3" .AND. s < 0
    RETURN "Расход инд.     "
  CASE t_d = "Ч"
    RETURN "Чек             "
  CASE t_d = "Б"
    RETURN "Через СБ        "
  ENDCASE
CASE t_b = "S"
  DO CASE
  CASE SUBSTR(SAL_A_H.DOC_NUM, 2, 3) = "***"
    RETURN "Товарный чек    "
  CASE s >= 0
    RETURN "Расх. накладная "
  CASE s < 0
    RETURN "Накладная возвр."
  ENDCASE
CASE t_b = "A"
  DO CASE
  CASE s >= 0
    RETURN "Приход          "
  CASE s <= 0
    RETURN "Возв. поставщику"
  ENDCASE
ENDCASE

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                 Программа определения имени USERa.                    ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE U_Nam
PARAMETER w

IF w = 0
  RETURN "Сам!"
ENDIF
IF SEEK(w, "USER_A_H")
  RETURN USER_A_H.FAMILY
ELSE
  RETURN REPLICATE("*", FSIZE("USER_NAME","USER_A_H"))
ENDIF

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║             Программа определения имени рабочего места.               ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE A_Nam
PARAMETER w

IF SEEK(w, "ARM_A_H")
  RETURN ARM_A_H.ARM_N
ELSE
  RETURN REPLICATE("*", FSIZE("ARM_N","ARM_A_H"))
ENDIF

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Уточняем тип печатаемого документа.                    ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE What_P
PRIVATE ex

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

*      Какой документ печатать?
*┌────────────────────────────────────────────────────┐
*│                                                    │
*│  < Накладную > < Счет-фактуру > < Товарный чек >   │
*│                                                    │
*└────────────────────────────────────────────────────┘
PRIVATE ex
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH IIF(Sale_Mode = "MEGA",8,6), 56, "Какой документ печатать?" && NEW 02.04.2002

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 4 GET ex PICTURE "@*HT \ Накладную ; Счет-фактуру ; Товарный чек"
if Sale_Mode = "MEGA" && NEW 02.04.2002
  @ 5, 10 GET tAltName  PICTURE "@*C Альт.названия"
endif

READ CYCLE

*--------------------------------------------------------------------------
DO CASE
CASE ex = 1
  ex = "N"
CASE ex = 2
  ex = "B"
OTHERWISE
  ex = "C"
ENDCASE
POP KEY
RELEASE WINDOW (win_name)
RETURN ex

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║              Программа печати истории движения.                       ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE P_H

PRIVATE s_sav, r_sav, d00, d01

s_sav = SELECT()

d01 = DATE()
d00 = DATE() - 30

IF .NOT. G_Period(d00, d01)
  RETURN
ENDIF


SELECT 0
IF USED("TMP_PRNT")
  SELECT TMP_PRNT
  ZAP
ELSE
  DELETE FILE (tmpw_path+"TMP_PRNT.DBF")
  CREATE DBF (tmpw_path+"TMP_PRNT") ;
     (  DOC_NUM  C(15),    ;
        DOC_DATE D,        ;
        DOC_NAME C(16),    ;
        INC_S    N(14,4),  ;
        EXP_S    N(14,4),  ;
        REST     N(14,4)   )
  USE (tmpw_path+"TMP_PRNT") EXCLUSIVE
ENDIF

SELECT ACC_HIST
r_sav = RECNO()
SEEK k000
SCAN REST WHILE k000 = NEW_TYPE+IIF(f_p = 1, "", FIRM)
  SELECT TMP_PRNT
  APPEND BLANK
  REPLACE DOC_NUM  WITH ACC_HIST.FIRM+"."+ACC_HIST.DOC_NUMV, ;
          DOC_DATE WITH ACC_HIST.DOC_DATE, ;
          DOC_NAME WITH ACC_HIST.DOC_NAME, ;
          INC_S    WITH ACC_HIST.INC_S,    ;
          EXP_S    WITH ACC_HIST.EXP_S,    ;
          REST     WITH IIF(EMPTY(f_status(f_p,1)),     ;
                      IIF(f_t = " ", ACC_HIST.REST_RH, ACC_HIST.REST_DH), ;
                      IIF(f_t = " ", ACC_HIST.REST_RF, ACC_HIST.REST_DF))
  SELECT ACC_HIST
ENDSCAN
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
SELECT TMP_PRNT
IF TYPE("f_out") # "C"
  DO P_Doc WITH f_t
ELSE
  COPY TO (f_out)
ENDIF

SELECT (s_sav)

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║            Программа собственно печати истории движения.              ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE P_Doc
PARAMETERS c_type

PRIVATE p_ln, fnt, n_cp, lft, n, i, t_p, f_p, t_d, p_drctry
PRIVATE sel_rem, f_name, ffeed, mntss

sel_rem = SELECT()

SELECT 0
USE (base_path+"DOC_FORM")
mntss = 2
DO CASE
CASE c_type = " "
  LOCATE FOR "CUS_HIST1" = ALLTRIM(UPPER(DOC_NAME))
CASE c_type = "3"
  LOCATE FOR "CUS_HIST$" = ALLTRIM(UPPER(DOC_NAME))
  mntss = 3
ENDCASE

p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
IF n = 0
  DIMENSION t_p(1)
  t_p(1) = ""
ELSE
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_H)
IF n = 0
  DIMENSION t_d(1)
  t_d(1) = ""
ELSE
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)
IF n = 0
  DIMENSION f_p(1)
  f_p(1) = ""
ELSE
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ENDIF

USE

SELECT TMP_PRNT
COPY TO tmpw_path+"TMP_P.DBF" FOR DOC_DATE => d00 .AND. DOC_DATE =< d01
USE

SELECT 0
USE tmpw_path+"TMP_P.DBF"
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"За указанный период движения не было!"
  mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

PRIVATE n_p_det, sum_1, sum_2, cur_p, hol_p, sub_

sum_d = 0
n_p_det = p_ln-ALEN(t_p)-ALEN(f_p)-ALEN(f_p)-1
hol_p = CEILING(RECCOUNT()/n_p_det)

DIMENSION sub_(8)
sub_(1) = ""
sub_(2) = ALLTRIM(c_name)
sub_(3) = TRANSFORM(d_s,"@D")
sub_(4) = TRANSFORM(DATE(),"@D")
sub_(7) = TRANSFORM(d00,"@D")
sub_(8) = TRANSFORM(d01,"@D")
GO BOTT
sub_(6) = STR(REST,12,mntss)
GO TOP
sub_(5) = STR(REST-INC_S+EXP_S,12,mntss)

FOR i = 1 TO ALEN(t_d)
  t_d(i) = STRTRAN(t_d(i),"{Customer}",sub_(2))
  t_d(i) = STRTRAN(t_d(i),"{Date_S}",sub_(3))
  t_d(i) = STRTRAN(t_d(i),"{Date_T}",sub_(4))
  t_d(i) = STRTRAN(t_d(i),"{Sum_S       }",sub_(5))
  t_d(i) = STRTRAN(t_d(i),"{Sum_T       }",sub_(6))
  t_d(i) = STRTRAN(t_d(i),"{Date_0}",sub_(7))
  t_d(i) = STRTRAN(t_d(i),"{Date_1}",sub_(8))
ENDFOR

FOR i = 1 TO ALEN(t_p)
  t_p(i) = STRTRAN(t_p(i),"{Customer}",sub_(2))
  t_p(i) = STRTRAN(t_p(i),"{Date_S}",sub_(3))
  t_p(i) = STRTRAN(t_p(i),"{Date_T}",sub_(4))
  t_p(i) = STRTRAN(t_p(i),"{Sum_S       }",sub_(5))
  t_p(i) = STRTRAN(t_p(i),"{Sum_T       }",sub_(6))
  t_p(i) = STRTRAN(t_d(i),"{Date_0}",sub_(7))
  t_p(i) = STRTRAN(t_d(i),"{Date_1}",sub_(8))
ENDFOR

FOR i = 1 TO ALEN(f_p)
  f_p(i) = STRTRAN(f_p(i),"{Customer}",sub_(2))
  f_p(i) = STRTRAN(f_p(i),"{Date_S}",sub_(3))
  f_p(i) = STRTRAN(f_p(i),"{Date_T}",sub_(4))
  f_p(i) = STRTRAN(f_p(i),"{Sum_S       }",sub_(5))
  f_p(i) = STRTRAN(f_p(i),"{Sum_T       }",sub_(6))
ENDFOR

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

FOR cur_p = 1 TO hol_p
  IF cur_p # 1
    EJECT
    FOR i = 1 TO ALEN(t_d)
      @ PROW()+1, 0 SAY STRTRAN(t_d(i),"{Pg/Pgs}",sub_(1))
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(t_p)
      @ PROW()+1, 0 SAY STRTRAN(t_p(i),"{Pg/Pgs}",sub_(1))
    ENDFOR
  ENDIF
  FOR i = 1 TO n_p_det
    @ PROW()+1,0 SAY TRANSFORM(DOC_DATE,"@D")+" "+DOC_NUM+" "+ ;
                     DOC_NAME+STR(INC_S,13,mntss)+ ;
                     STR(EXP_S,13,mntss)+STR(REST,13,mntss)
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR

  FOR i = 1 TO ALEN(f_p)
    @ PROW()+1, 0 SAY STRTRAN(f_p(i),"{Pg/Pgs}",sub_(1))
  ENDFOR

ENDFOR

USE
DO Term_Prn WITH "", tmpw_path+"TMP_P.DBF", c_code

DELETE FILE (tmpw_path+"TMP_P.DBF")

SELECT (sel_rem)
RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа вывода курса для накладной.                  ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Cource
PARAMETER m_name
PRIVATE mss

IF DOC_BASE # "S"
  ACTIVATE SCREEN
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Это не накладная!"
  mss(3) = ""
  IF TYPE("f_out") = "C"
    DO Out_Mess WITH 7, "mss"
  ENDIF
  IF WEXIST(ALIAS())
    ACTIVATE WINDOW (ALIAS()) SAME
  ENDIF
  RETURN
ENDIF

DO Crs_Inv WITH FIRM, DOC_NUM, DOC_DATE, m_name

IF WEXIST(ALIAS())
  ACTIVATE WINDOW (ALIAS()) SAME
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_Docs    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Просмотр документов.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 15.02.1999 ──┘
PROCEDURE V_Docs
PRIVATE k000, pic, f_p, f_t
k000 = " "
pic = "@Z 999 999 999.99"
f_p = 1
f_t = " "
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

SELECT ACC_HIST
SET ORDER TO TAG HRON

*
*   Определяем асинхронное меню
*
*
*  Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Acc_Type OF (menu_name) PROMPT "Тип депозита"
ON PAD Acc_Type OF (menu_name) ACTIVATE POPUP Acc_Type

  DEFINE POPUP Acc_Type MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Acc_Type PROMPT "Предприятие" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Acc_Type DO Sw_Mode WITH "Type 1"

  DEFINE BAR  2 OF Acc_Type PROMPT "Тип депозита" KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF Acc_Type DO Sw_Mode WITH "Type 2"

DEFINE PAD Print OF (menu_name) PROMPT "Печать" && KEY Ctrl-A, "Enter"
ON PAD Print OF (menu_name) ACTIVATE POPUP Print

  DEFINE POPUP Print MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Print PROMPT "Документа" KEY Ctrl-A, "Enter"
  ON SELECTION BAR 1 OF Print DO Sw_Mode WITH "p_doc"

  DEFINE BAR  2 OF Print PROMPT "Накладная для STORE"
  ON SELECTION BAR 2 OF Print DO Sw_Mode WITH "s_inv"

  DEFINE BAR  3 OF Print PROMPT "Истории" KEY Ctrl-A, "Ctrl+Enter"
  ON SELECTION BAR 3 OF Print DO Sw_Mode WITH "p_hist"

  DEFINE BAR  4 OF Print PROMPT "Идентификатор программы" KEY Ctrl-A, "F9"
  ON SELECTION BAR 4 OF Print DO Sw_Mode WITH "prg_id"

DEFINE PAD Dis_Inv  OF (menu_name) PROMPT "Скидка по накл." KEY Ctrl-A, "F3"
ON SELECTION PAD Dis_Inv  OF (menu_name) DO Sw_Mode WITH "Dis_Inv"

DEFINE PAD Cource  OF (menu_name) PROMPT "Курс" KEY Ctrl-A, "F4"
ON SELECTION PAD Cource  OF (menu_name) DO Sw_Mode WITH "Cource"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
*┌───Дата────Тип документа───────────Приход─────────Расход────────Остаток─┐
*│ДД.ММ.ГГГГ Имя_документа___ 999_999_999.99 999_999_999.99 999_999_999.99│
*└────────────────────────────────────────────────────────────────────────┘
*┌────────────────────── Документ N 99999999999/ФФФ ──────────────────────┐
*│     Кто ....:....!....:....!....:....!..      Где ....:....!....:.     │
*└────────────────────────────────────────────────────────────────────────┘
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 72

DO D_Wins WITH ln, wd, ALLTRIM(c_name), 1, 1
@ 2, 2 SAY "───Дата────Тип документа───────────Приход─────────Расход────────Остаток"
what_do = "List"
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M")
    IF f_t = " "
      @ WROWS()-4, WCOLS()-16 SAY f_status(f_p, 3) PICTURE SUBSTR(pic,4)
    ELSE
      @ WROWS()-4, WCOLS()-16 SAY f_status(f_p, 5) PICTURE SUBSTR(pic,4)
    ENDIF
    @ 1, 3 SAY "Депозит: "+IIF(EMPTY(f_t), "обычный      ", "индексируемый")+ ;
               "         Предприятие: "+f_names(f_p)
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "p_hist"
    ON KEY LABEL F1    DO Sw_Mode WITH "Type 1"
    ON KEY LABEL F2    DO Sw_Mode WITH "Type 2"
    ON KEY LABEL F3    DO Sw_Mode WITH "Dis_Inv"
    ON KEY LABEL F9    DO Sw_Mode WITH "prg_id"
    ON KEY LABEL F4    DO Cource WITH menu_name
*
*    BROWSE - меню
*
    BROWSE KEY k000  ;
           FIELDS DOC_DATE:H="",     ;
                  DOC_NAME:H="",     ;
                  INC_S:H="":P=pic,  ;
                  EXP_S:H="":P=pic,  ;
                  RST = IIF(EMPTY(f_status(f_p,1)),     ;
                      IIF(f_t = " ", REST_RH, REST_DH), ;
                      IIF(f_t = " ", REST_RF, REST_DF)):H="":P=pic, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "p_doc")
    ENDIF

  CASE what_do = "p_doc"     && Печать документа

    DO P_D
    what_do = "List"

  CASE what_do = "s_inv"     && Посылка накладной

    IF DOC_BASE # "S"
      PRIVATE mss000
      DIMENSION mss000(3)
      mss000(1) = ""
      mss000(2) = CHR(0)+"Это не накладная!"
      mss000(3) = ""
      DO Out_Mess WITH 7, "mss000"
    ELSE
      DO Send_Sto WITH FIRM, DOC_NUM, DOC_DATE
    ENDIF
    what_do = "List"

  CASE what_do = "p_hist"    && Печать истории

    DO P_H
    what_do = "List"

  CASE what_do = "prg_id"    && Идентификатор программы

    DO CalcId1 WITH c_code
    what_do = "List"

  CASE what_do = "Type 1"    && Смена предприятия

    DO Ch_Dep
    what_do = "List"

  CASE what_do = "Type 2"    && Смена типа депозита

    IF f_t = " "
      f_t = "3"
      pic = "@Z 9 999 999.9999"
    ELSE
      f_t = " "
      pic = "@Z 999 999 999.99"
    ENDIF
    k000 = f_t+SUBSTR(k000, 2)
    GO BOTTOM
    what_do = "List"

  CASE what_do = "Cource"
    DO Cource WITH menu_name
    what_do = "List"

  CASE what_do = "Dis_Inv"
    IF DOC_BASE # "S"
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Вы шиблись, это не накладная!"
      mss(3) = ""
    ELSE
      DO Dis_Inv WITH FIRM, DOC_NUM, DOC_DATE
    ENDIF
    what_do = "List"

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

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot
PRIVATE v

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
*┌────────────────────── Документ N 99999999999/ФФФ ──────────────────────┐
*│     Кто ....:....!....:....!....:....!..      Где ....:....!....:.     │
*└────────────────────────────────────────────────────────────────────────┘
w = " Документ N "+ALLTRIM(DOC_NUMV)+"/"+FIRM+" "
@ WROWS()-2, 7 SAY "Кто "+AUTOR+"      Где "+ARM_NM
@ WROWS()-3, 2 SAY PADC(w, WCOLS()-4, "─")

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Dep       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Выбор предприятия.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 16.02.1999 ──┘
PROCEDURE Ch_Dep
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

*┌────────────────────────────────┐
*│      ┌──────────────────┐      │
*│      │ ....:....!....:. │      │
*│      └──────────────────┘      │
*│< OK Ctrl-W > < Отказаться Esc >│
*└────────────────────────────────┘

PRIVATE ex, p_w, ln
ex = 1
ln = MIN(6+ALEN(f_names), WROWS("")-8)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH ln, 36, "Предприятия"
p_w = f_p
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 8 GET p_w FROM f_names SIZE ln-4, 20 PICTURE "@&"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  f_p = p_w
  IF f_p = 1
    SET ORDER TO TAG HRON
    k000 = LEFT(k000,1)
  ELSE
    SET ORDER TO TAG FIRM
    k000 = LEFT(k000,1)+f_status(f_p, 1)
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
