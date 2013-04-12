*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Справка остатки на депозитах.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Rest_Rep

PRIVATE dat_0, dat_1, dat_v0, dat_v1, ex, acc_t_n, acc_t_c, p_list, p_numb
PRIVATE mss

*
*  Переменные состояния для навигвции
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

USE PARMS
dat_0  = DAT_START
dat_v0 = DAT_START
USE PLACES
p_numb = RECCOUNT()
USE
PRIVATE ln, wk_arr

p_list = LEFT("ABCDEFGHIJKLMNOPQRSTUVWXYZ", p_numb)
dat_1  = DATE()
dat_v1 = DATE()
DIMENSION wk_arr(p_numb)
FOR ex = 1 TO p_numb
  wk_arr(ex) = .T.
ENDFOR

acc_t_c = 1
ex = 1
ln = MAX(17, p_numb+8)

*                          Период
*┌─────────────────────────────────────────────────────────────────┐
*│ ┌─ Тип депозита ────────────┐ ┌─ Площадки ────────────────────┐ │
*│ │  ( ) Б/Н                  │ │                               │ │
*│ │  ( ) Нал.                 │ │  [ ] ....:....!....:....!.... │ │
*│ │  ( ) Индекс.              │ │                               │ │
*│ └───────────────────────────┘ └───────────────────────────────┘ │
*│ ┌─ Период ──────────────────┐                                   │
*│ │                           │                                   │
*│ │  Начальная дата ДД.ММ.ГГ  │                                   │
*│ │                           │                                   │
*│ │  Конечная дата  ДД.ММ.ГГ  │                                   │
*│ │                           │                                   │
*│ └───────────────────────────┘                                   │
*│                 < OK Ctrl-W > < Отказаться Esc >                │
*└─────────────────────────────────────────────────────────────────┘
*
PUSH KEY CLEAR 
DO Prp_Nav_2
DO D_Win_N WITH ln, 69, "Период"
DO Sun_Bord WITH 2, 3, 6, 31
@ 2,  5 SAY " Тип депозита "
DO Sun_Bord WITH 7, 3, 13, 31
@ 7,  5 SAY " Период "
DO Sun_Bord WITH 2, 33, 5+p_numb, 65
@ 2, 35 SAY " Площадки "

@  3,  6 GET acc_t_c PICTURE "@*RV Б/Н  ;Нал.  ;Индекс."
@  9,  6 SAY "Начальная дата" GET dat_0 PICTURE "@D"
@ 11,  6 SAY "Конечная дата " GET dat_1 PICTURE "@D"

DO G_Place WITH 4, 36, wk_arr

@ WROWS()-2, 19 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE VALID V_Dat()

POP KEY
RELEASE WINDOW (win_name)
IF ex = 2
  RETURN
ENDIF

p_list = ""
FOR ex = 1 TO p_numb
  IF wk_arr(ex)
    p_list = p_list+CHR(ASC("A")+ex-1)
  ENDIF
ENDFOR
  
DO Wt_Mess WITH "Подборка данных..."

USE ACCOUNT ORDER TAG CUS_NAME

SELECT 0
USE INCMONEY ORDER TAG CUS_CODE

SELECT 0
USE SALE_TIT ORDER TAG CUS_CODE

SELECT 0
USE ACT_S_T ORDER TAG SUP_CODE

SELECT 0
SET EXCLUSIVE ON
DELETE FILE (tmpo_path+"REST_REP.DBF")
DELETE FILE (tmpo_path+"REST_REP.CDX")
CREATE DBF (tmpo_path+"REST_REP.DBF")  ;
   ( CUS_NAME C(FSIZE("CUS_NAME","ACCOUNT")),  ;
     ST_MONEY N(19,4), ;
     INC_BN   N(19,4), ;
     INC      N(19,4), ;
     EXP      N(19,4)   )
INDEX ON SYS(15,lwr,CUS_NAME) TAG NAME
PRIVATE v_inc_bn, v_inc, v_exp, v_nam, v_start, d_tp, d_tp_s
SET EXCLUSIVE OFF
SELECT ACCOUNT

*SEEK STR(acc_t_c,1)

SCAN FOR SUBSYS $ p_list
  DO CASE
  CASE acc_t_c = 1
    v_start = ACC_S_S
    d_tp    = " 1"
    d_tp_s = " "
    acc_t_n = "Б/нал."
  CASE acc_t_c = 2
    v_start = ACC_S0_S
    d_tp    = "2"
    d_tp_s = "0"
    acc_t_n = "Нал."
  CASE acc_t_c = 3
    v_start = ACC_SD_S
    d_tp    = "3"
    acc_t_n = "Индекс."
  ENDCASE
  v_namn  = CUS_NAME
  v_inc   = 0
  v_inc_bn= 0
  v_exp   = 0
  IF SEEK(ACCOUNT.CUS_CODE,"INCMONEY")
    SELECT INCMONEY
    SCAN REST WHILE CUS_CODE = ACCOUNT.CUS_CODE FOR DOC_TYPE $ d_tp
      DO CASE
      CASE DOC_DATE < dat_0
        IF d_tp = "3"
          v_start = v_start+MONEY_D
        ELSE
          v_start = v_start+MONEY_R
        ENDIF
      CASE DOC_DATE <= dat_1
        DO CASE
        CASE acc_t_c = 3
          IF MONEY_D >= 0
            v_inc = v_inc+MONEY_D
          ELSE
            v_exp = v_exp-MONEY_D
          ENDIF
        CASE acc_t_c = 1
          IF DOC_TYPE = "1"
            v_inc_bn = v_inc_bn+MONEY_R
          ELSE
            IF MONEY_R >= 0
              v_inc = v_inc+MONEY_R
            ELSE
              v_exp = v_exp-MONEY_R
            ENDIF
          ENDIF
        OTHERWISE
          IF MONEY_R >= 0
            v_inc = v_inc+MONEY_R
          ELSE
            v_exp = v_exp-MONEY_R
          ENDIF
        ENDCASE
      ENDCASE
    ENDSCAN
  ENDIF

  IF SEEK(ACCOUNT.CUS_CODE,"SALE_TIT") .AND. acc_t_c # 3
    SELECT SALE_TIT
    SCAN REST WHILE CUS_CODE = ACCOUNT.CUS_CODE FOR NEW_TYPE = d_tp_s
      DO CASE
      CASE DOC_DATE < dat_0
        v_start = v_start-TOTAL_R
      CASE DOC_DATE <= dat_1
        v_exp = v_exp+TOTAL_R
      ENDCASE
    ENDSCAN
  ENDIF

  IF SEEK(ACCOUNT.CUS_CODE,"ACT_S_T") .AND. acc_t_c # 3
    SELECT ACT_S_T
    SCAN REST WHILE SUP_CODE = ACCOUNT.CUS_CODE FOR INC_TYPE = d_tp_s
      DO CASE
      CASE DOC_DATE < dat_0
        v_start = v_start+TOTAL_H
      CASE DOC_DATE <= dat_1
        v_inc = v_inc+TOTAL_H
      ENDCASE
    ENDSCAN
  ENDIF

  IF v_start # 0 .OR. v_inc_bn # 0 .OR. v_inc # 0 .OR. v_exp # 0
    SELECT REST_REP
    APPEND BLANK
    REPLACE CUS_NAME WITH ACCOUNT.CUS_NAME, ;
            ST_MONEY WITH v_start, ;
            INC_BN   WITH v_inc_bn, ;
            INC      WITH v_inc, ;
            EXP      WITH v_exp
  ENDIF
  SELECT ACCOUNT
ENDSCAN

PRIVATE sb, p_ln, fnt, n_cp, lft, t_p, f_p, f_d, n, i, ffeed
DIMENSION sb(14,2)

*{Pgs}      - Страница/всего страниц
*{Type}     - Тип депозитов
*{Dat1  }   - Начальная дата
*{Dat2  }   - Конечная дата
*{T001         } - Сумма остатков на начальную дату на странице
*{T002         } - Сумма б/н прихода за период на странице
*{T003         } - Сумма прихода за период на странице
*{T004         } - Сумма расхода за период на странице
*{T005         } - Сумма остатков на конечную дату на странице
*{T101         } - Сумма остатков на начальную дату по документу
*{T102         } - Сумма б/н прихода за период по документу
*{T103         } - Сумма прихода за период по документу
*{T104         } - Сумма расхода за период по документу
*{T105         } - Сумма остатков на конечную дату по документу



sb(1,1) = "{Pgs}"
sb(1,2) = ""

sb(2,1) = "{Type}"
sb(2,2) = ALLTRIM(acc_t_n)

sb(3,1) = "{Dat1  }"
sb(3,2) = TRANSFORM(dat_0,"@D ")

sb(4,1) = "{Dat2  }"
sb(4,2) = TRANSFORM(dat_1,"@D ")

sb(5,1) = "{T001         }"
sb(5,2) = "               "

sb(6,1) = "{T002         }"
sb(6,2) = "               "

sb(7,1) = "{T003         }"
sb(7,2) = "               "

sb(8,1) = "{T004         }"
sb(8,2) = "               "

sb(9,1) = "{T005         }"
sb(9,2) = "               "

sb(10,1) = "{T101         }"
sb(10,2) = "               "

sb(11,1) = "{T102         }"
sb(11,2) = "               "

sb(12,1) = "{T103         }"
sb(12,2) = "               "

sb(13,1) = "{T104         }"
sb(13,2) = "               "

sb(14,1) = "{T105         }"
sb(14,2) = "               "

SELECT ACCOUNT
USE

SELECT INCMONEY
USE

SELECT ACT_S_T
USE

SELECT SALE_TIT

USE DOC_FORM
IF acc_t_c # 1
  LOCATE FOR "REP_REST" == ALLTRIM(UPPER(DOC_NAME))
ELSE
  LOCATE FOR "REP_REST1" == ALLTRIM(UPPER(DOC_NAME))
ENDIF

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
SELECT REST_REP
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
    FOR j = 1 TO 14
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR
  FOR i = 1 TO n_det
    DO CASE
    CASE acc_t_c = 3
      stw = CUS_NAME+STR(ST_MONEY,16,4)+STR(INC,16,4)+STR(EXP,16,4)+  ;
            STR(ST_MONEY+INC-EXP,16,4)
    CASE acc_t_c = 1
      stw = CUS_NAME+STR(ST_MONEY,16,2)+STR(INC_BN,16,2)+  ;
            STR(INC,16,2)+STR(EXP,16,2)+STR(ST_MONEY+INC_BN+INC-EXP,16,2)
    OTHERWISE
      stw = CUS_NAME+STR(ST_MONEY,16,2)+  ;
            STR(INC,16,2)+STR(EXP,16,2)+STR(ST_MONEY+INC-EXP,16,2)
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

  IF acc_t_c = 3
    sb(5,2) = STR(s001, 15, 4)
    sb(6,2) = STR(s002, 15, 4)
    sb(7,2) = STR(s003, 15, 4)
    sb(8,2) = STR(s004, 15, 4)
    sb(9,2) = STR(s005, 15, 4)
  ELSE
    sb(5,2) = STR(s001, 15, 2)
    sb(6,2) = STR(s002, 15, 2)
    sb(7,2) = STR(s003, 15, 2)
    sb(8,2) = STR(s004, 15, 2)
    sb(9,2) = STR(s005, 15, 2)
  ENDIF

  s101 = s101+s001
  s102 = s102+s002
  s103 = s103+s003
  s104 = s104+s004
  s105 = s105+s005

  FOR i = 1 TO ALEN(f_p)
    stw = f_p(i)
    FOR j = 1 TO 14
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR
  IF EOF()
    EXIT
  ENDIF
ENDFOR

IF acc_t_c = 3
  sb(10,2) = STR(s101, 15, 4)
  sb(11,2) = STR(s102, 15, 4)
  sb(12,2) = STR(s103, 15, 4)
  sb(13,2) = STR(s104, 15, 4)
  sb(14,2) = STR(s105, 15, 4)
ELSE
  sb(10,2) = STR(s101, 15, 2)
  sb(11,2) = STR(s102, 15, 2)
  sb(12,2) = STR(s103, 15, 2)
  sb(13,2) = STR(s104, 15, 2)
  sb(14,2) = STR(s105, 15, 2)
ENDIF

FOR i = 1 TO ALEN(f_d)
  stw = f_d(i)
  FOR j = 1 TO 14
    stw = STRTRAN(stw, sb(j,1), sb(j,2))
  ENDFOR
  @ PROW()+1, 0 SAY stw
ENDFOR

DO Term_Prn WITH ""

CLOSE DATABASES
DELETE FILE (tmpo_path+"REST_REP.DBF")
DELETE FILE (tmpo_path+"REST_REP.CDX")

RETURN

*********************************************************************
PROCEDURE V_Dat

IF ex = 2
  RETURN .T.
ENDIF

IF dat_0 < dat_v0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Имеем данные только с "+TRANSFORM(dat_v0,"@D")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  dat_0 = dat_v0
  SHOW GETS
  RETURN .F.
ENDIF

IF dat_1 > dat_v1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Сегодня только "+TRANSFORM(dat_v1,"@D")+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  dat_1 = dat_v1
  SHOW GETS
  RETURN .F.
ENDIF

IF ASCAN(wk_arr, .T.) = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Так о каких площадках идет речь?..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
