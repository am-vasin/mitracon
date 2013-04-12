*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Справка остатки на депозитах ( б/н ).                  ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Rest_B

PRIVATE dat_0, dat_1, dat_v0, dat_v1, ex, acc_t_c, sw, p_numb, p_line

PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
f_c000 = "   "
f_n000 = SPACE(16)
f_c_old = f_c000

PRIVATE mss

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
PRIVATE ln, ln1, l_ex, wk_list

USE (base_path+"PARMS")
dat_0  = DAT_START
dat_v0 = DAT_START
USE
dat_1  = DATE()
dat_v1 = DATE()
p_line = "   "
acc_t_c = 1
IF .NOT. Fill_Bl()
  RETURN
ENDIF

DO Wt_Mess WITH "Подборка данных..."

SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
SET RELATION TO STR(CUS_CODE)+f_c000 INTO CUS_REST

DELETE FILE (tmpo_path+"REST_BN.DBF")
DELETE FILE (tmpo_path+"REST_BN.CDX")
SELECT 0
SET EXCLUSIVE ON
CREATE DBF (tmpo_path+"REST_BN.DBF")  ;
   ( CUS_CODE N( 8),   ;
     CUS_NAME C(FSIZE("CUS_NAME","ACCOUNT")),  ;
     ST_MONEY N(12,2), ;
     INC_BN   N(12,2), ;
     INC      N(12,2), ;
     EXP      N(12,2), ;
     REST     N(12,2)   )

INDEX ON CUS_CODE TAG CUS_CODE
SET EXCLUSIVE OFF
PRIVATE v_inc_bn, v_inc, v_exp, v_nam, v_start
SELECT 0

USE (base_path+"INCMONEY") ORDER TAG HRON DESCENDING
SCAN REST WHILE DOC_DATE >= dat_0 FOR FIRM = f_c000
  WAIT WINDOW "INCMONEY  "+DTOC(DOC_DATE) NOWAIT &&&&&&&&&&&&&&&
  IF DOC_TYPE $ "1Б" .AND. ENTER_DAT < dat_0
    LOOP
  ENDIF
  SELECT REST_BN
  IF .NOT. SEEK(INCMONEY.CUS_CODE, "REST_BN")		&& Добавим клиента...
    = SEEK(INCMONEY.CUS_CODE, "ACCOUNT")
    APPEND BLANK
    REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE, ;
            CUS_NAME WITH ACCOUNT.CUS_NAME, ;
            ST_MONEY WITH CUS_REST.ACC_SUM+CUS_REST.ACC_S_H, ;
            REST     WITH CUS_REST.ACC_SUM+CUS_REST.ACC_S_H
  ENDIF
  IF INCMONEY.DOC_TYPE $ " Ч" .AND. INCMONEY.DOC_DATE <= dat_1 .OR. ;
     INCMONEY.DOC_TYPE $ "1Б" .AND. INCMONEY.ENTER_DAT <= dat_1
    IF INCMONEY.DOC_TYPE $ "1Б"
      REPLACE ST_MONEY WITH ST_MONEY-INCMONEY.MONEY_R-INCMONEY.N_OF_SALE, ;
              INC_BN   WITH INC_BN+INCMONEY.MONEY_R+INCMONEY.N_OF_SALE
    ELSE
      REPLACE ST_MONEY WITH ST_MONEY-INCMONEY.MONEY_R-INCMONEY.N_OF_SALE, ;
              INC      WITH INC+INCMONEY.MONEY_R+INCMONEY.N_OF_SALE
    ENDIF
  ELSE
    REPLACE ST_MONEY WITH ST_MONEY-INCMONEY.MONEY_R-INCMONEY.N_OF_SALE, ;
            REST     WITH REST-INCMONEY.MONEY_R-INCMONEY.N_OF_SALE
  ENDIF
  SELECT INCMONEY
ENDSCAN

USE (base_path+"SALE_TIT") ORDER TAG HRON DESCENDING
SCAN REST WHILE DOC_DATE >= dat_0 FOR FIRM = f_c000
  WAIT WINDOW "SALE_TIT  "+DTOC(DOC_DATE) NOWAIT &&&&&&&&&&&&&&&
  IF NEW_TYPE # " "
    LOOP
  ENDIF
  IF .NOT. SEEK(STR(CUS_CODE)+f_c000, "CUS_REST")
    LOOP
  ENDIF
  SELECT REST_BN
  IF .NOT. SEEK(SALE_TIT.CUS_CODE, "REST_BN")		&& Добавим клиента...
    = SEEK(SALE_TIT.CUS_CODE, "ACCOUNT")
    APPEND BLANK
    REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE, ;
            CUS_NAME WITH ACCOUNT.CUS_NAME, ;
            ST_MONEY WITH CUS_REST.ACC_SUM+CUS_REST.ACC_S_H, ;
            REST     WITH CUS_REST.ACC_SUM+CUS_REST.ACC_S_H
  ENDIF
  IF SALE_TIT.DOC_DATE <= dat_1
    REPLACE ST_MONEY WITH ST_MONEY+SALE_TIT.TOTAL_R+ROUND(SALE_TIT.KASSA*SALE_TIT.NSP_/100,2), ;
            EXP      WITH EXP+SALE_TIT.TOTAL_R+ROUND(SALE_TIT.KASSA*SALE_TIT.NSP_/100,2)
  ELSE
    REPLACE ST_MONEY WITH ST_MONEY+SALE_TIT.TOTAL_R+ROUND(SALE_TIT.KASSA*SALE_TIT.NSP_/100,2), ;
            REST     WITH REST+SALE_TIT.TOTAL_R+ROUND(SALE_TIT.KASSA*SALE_TIT.NSP_/100,2)
  ENDIF
  SELECT SALE_TIT
ENDSCAN

USE (base_path+"ACT_S_T")
SCAN REST FOR BETWEEN(DOC_DATE, dat_0, dat_1) .AND. FIRM = f_c000
  WAIT WINDOW "ACT_S_T  "+DTOC(DOC_DATE) NOWAIT &&&&&&&&&&&&&&&
  IF VALUTA = "$"
    LOOP
  ENDIF
  IF .NOT. SEEK(STR(CUS_CODE)+f_c000, "CUS_REST")
    LOOP
  ENDIF
  SELECT REST_BN
  IF .NOT. SEEK(ACT_S_T.SUP_CODE, "REST_BN")		&& Добавим клиента...
    = SEEK(ACT_S_T.SUP_CODE, "ACCOUNT")
    APPEND BLANK
    REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE, ;
            CUS_NAME WITH ACCOUNT.CUS_NAME, ;
            ST_MONEY WITH CUS_REST.ACC_SUM+CUS_REST.ACC_S_H, ;
            REST     WITH CUS_REST.ACC_SUM+CUS_REST.ACC_S_H
  ENDIF
  IF ACT_S_T.DOC_DATE <= dat_1
    REPLACE ST_MONEY WITH ST_MONEY-ACT_S_T.TOTAL_H, ;
            INC      WITH INC+ACT_S_T.TOTAL_H
  ELSE
    REPLACE ST_MONEY WITH ST_MONEY-ACT_S_T.TOTAL_H, ;
            REST     WITH REST-ACT_S_T.TOTAL_H
  ENDIF
  SELECT ACT_S_T
ENDSCAN

USE

SELECT REST_BN
INDEX ON SYS(15,lwr,CUS_NAME) TAG NAME
DELETE FILE (tmpo_path+"REST_BN0.DBF")
COPY TO (tmpo_path+"REST_BN0")

PRIVATE sb, p_ln, fnt, n_cp, lft, t_p, f_p, f_d, n, i, ffeed
DIMENSION sb(15,2)

*{Pgs}      - Страница/всего страниц
*{Type}     - Тип депозитов
*{Dat1  }   - Начальная дата
*{Dat2  }   - Конечная дата
*{Firm_Name}- Название предприятия
*{T001    } - Сумма остатков на начальную дату на странице
*{T002    } - Сумма б/н прихода за период на странице
*{T003    } - Сумма прихода за период на странице
*{T004    } - Сумма расхода за период на странице
*{T005    } - Сумма остатков на конечную дату на странице
*{T101    } - Сумма остатков на начальную дату по документу
*{T102    } - Сумма б/н прихода за период по документу
*{T103    } - Сумма прихода за период по документу
*{T104    } - Сумма расхода за период по документу
*{T105    } - Сумма остатков на конечную дату по документу

sb(1,1) = "{Pgs}"
sb(1,2) = ""

sb(2,1) = "{Type}"
sb(2,2) = ""

sb(3,1) = "{Dat1  }"
sb(3,2) = TRANSFORM(dat_0,"@D ")

sb(4,1) = "{Dat2  }"
sb(4,2) = TRANSFORM(dat_1,"@D ")

sb(5,1) = "{T001    }"
sb(5,2) = "          "

sb(6,1) = "{T002    }"
sb(6,2) = "          "

sb(7,1) = "{T003    }"
sb(7,2) = "          "

sb(8,1) = "{T004    }"
sb(8,2) = "          "

sb(9,1) = "{T005    }"
sb(9,2) = "          "

sb(10,1) = "{T101    }"
sb(10,2) = "          "

sb(11,1) = "{T102    }"
sb(11,2) = "          "

sb(12,1) = "{T103    }"
sb(12,2) = "          "

sb(13,1) = "{T104    }"
sb(13,2) = "          "

sb(14,1) = "{T105    }"
sb(14,2) = "          "

sb(15,1) = "{Firm_Name}"

SELECT CUS_REST
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE
SEEK f_c000
sb(15,2) = L_NAME

SELECT ACCOUNT
USE

USE (base_path+"DOC_FORM")
LOCATE FOR "REP_BN" == ALLTRIM(UPPER(DOC_NAME))

p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR

n = MEMLINES(PAGE_F)
DIMENSION f_p(n)
FOR i = 1 TO n
  f_p(i) = MLINE(PAGE_F,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR

USE
SELECT REST_BN
GO TOP
DO Wt_Mess

PRIVATE s001, s002, s003, s004, s005, s101, s102, s103, s104, s105
PRIVATE n_pg, f_pg, n_det, j, stw, p_drctry

n_det = p_ln-ALEN(t_p)-ALEN(f_p)-ALEN(f_d)
n_pg  = CEILING(RECCOUNT()/n_det)

STORE 0 TO s101, s102, s103, s104, s105
DO Ini_Prn WITH "Справка по остаткам на депозитах", p_ln, lft, n_cp, fnt, ffeed, p_drctry
FOR f_pg = 1 TO n_pg
  IF f_pg # 1
    EJECT
  ENDIF
  STORE 0 TO s001, s002, s003, s004, s005
  sb(1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  FOR i = 1 TO ALEN(t_p)
    stw = t_p(i)
    FOR j = 1 TO 15
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR
  FOR i = 1 TO n_det
    DO CASE
    CASE acc_t_c = 2 
      stw = CUS_NAME+STR(ST_MONEY,13,4)+STR(INC,13,4)+STR(EXP,13,4)+  ;
            STR(ST_MONEY+INC-EXP,13,4)
    CASE acc_t_c = 1
      stw = CUS_NAME+STR(ST_MONEY,13,2)+STR(INC_BN,13,2)+  ;
            STR(INC,13,2)+STR(EXP,13,2)+STR(ST_MONEY+INC_BN+INC-EXP,13,2)
    OTHERWISE
      stw = CUS_NAME+STR(ST_MONEY,13,2)+  ;
            STR(INC,13,2)+STR(EXP,13,2)+STR(ST_MONEY+INC-EXP,13,2)
    ENDCASE
    s001 = s001+ST_MONEY
    s002 = s002+INC_BN
    s003 = s003+INC
    s004 = s004+EXP
    s005 = s005+ST_MONEY+INC+INC_BN-EXP
    @ PROW()+1, 0 SAY stw
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR

  IF acc_t_c = 2
    sb(5,2) = STR(s001, 13, 4)
    sb(6,2) = STR(s002, 13, 4)
    sb(7,2) = STR(s003, 13, 4)
    sb(8,2) = STR(s004, 13, 4)
    sb(9,2) = STR(s005, 13, 4)
  ELSE
    sb(5,2) = STR(s001, 13, 2)
    sb(6,2) = STR(s002, 13, 2)
    sb(7,2) = STR(s003, 13, 2)
    sb(8,2) = STR(s004, 13, 2)
    sb(9,2) = STR(s005, 13, 2)
  ENDIF

  s101 = s101+s001
  s102 = s102+s002
  s103 = s103+s003
  s104 = s104+s004
  s105 = s105+s005

  FOR i = 1 TO ALEN(f_p)
    stw = f_p(i)
    FOR j = 1 TO 15
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR
  IF EOF()
    EXIT
  ENDIF
ENDFOR

IF acc_t_c = 2
  sb(10,2) = STR(s101, 13, 4)
  sb(11,2) = STR(s102, 13, 4)
  sb(12,2) = STR(s103, 13, 4)
  sb(13,2) = STR(s104, 13, 4)
  sb(14,2) = STR(s105, 13, 4)
ELSE
  sb(10,2) = STR(s101, 13, 2)
  sb(11,2) = STR(s102, 13, 2)
  sb(12,2) = STR(s103, 13, 2)
  sb(13,2) = STR(s104, 13, 2)
  sb(14,2) = STR(s105, 13, 2)
ENDIF

FOR i = 1 TO ALEN(f_d)
  stw = f_d(i)
  FOR j = 1 TO 15
    stw = STRTRAN(stw, sb(j,1), sb(j,2))
  ENDFOR
  @ PROW()+1, 0 SAY stw
ENDFOR

USE

DO Term_Prn WITH "", tmpo_path+"REST_BN0.DBF"
DELETE FILE (tmpo_path+"REST_BN0.DBF")

CLOSE DATABASES
DELETE FILE (tmpo_path+"REST_BN.DBF")
DELETE FILE (tmpo_path+"REST_BN.CDX")

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Fill_Bl      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Заполнение бланка.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/15/1999 ──┘
PROCEDURE Fill_Bl
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
*│   ┌─ Интервал дат ───────────────┐   │
*│   │  С ДД.ММ.ГГГГ по ДД.ММ.ГГГГ  │   │
*│   └──────────────────────────────┘   │
*│Предприятие .... ....:....!....:. (F1)│
*│   < OK Ctrl-W > < Отказаться Esc >   │
*└──────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 42, "Заполните бланк"
DO Sun_Bord WITH  2,  5,  4, 36, " Интервал дат "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  8 SAY "С" GET dat_0
@  3, 21 SAY "по" GET dat_1
DO Get_Firm WITH 5, 2, .T.
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID T_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура T_Blank      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Проверка корректности заполнения бланка.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/15/1999 ──┘
PROCEDURE T_Blank
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы забыли указать предприятие!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_0 < dat_v0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Имеем данные только с "+DTOC(dat_v0)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF .F. && dat_1 > dat_v1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Сегодня только "+DTOC(dat_v1)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF dat_1 < dat_0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы указали начальную дату больше конечной!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
