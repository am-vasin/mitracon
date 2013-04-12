*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║               Справка по кассовой операции за период.                 ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Oper_Rep

PRIVATE a_codes, a_names, a_dirs, a_val, dat_0, dat_1, dat_v0, dat_v1
PRIVATE ex, a_t_n, a_t_c, a_t_v, a_t_d
PRIVATE mss, i, pic_r, pic_d, pic_k, oper_c, oper_n, c_flt

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

USE (base_path+"PARMS")
dat_0  = DAT_START
dat_v0 = DAT_START
USE
dat_1  = DATE()
dat_v1 = DATE()
USE (base_path+"MM_TYPES") ORDER TAG NAME

i = 0
SCAN

  i = i+1
  DIMENSION a_codes(i), a_names(i), a_val(i), a_dirs(i)

  a_names(i) = NAME+IIF(M_RUB, " Руб. ", " USD  ")+IIF(M_DIR=0, "Прих.","Расх.")
*  IF HOT_KEY > 0 .AND. HOT_KEY <= LEN(NAME)
*    a_names(i) = LEFT(a_names(i), HOT_KEY-1)+"\<"+SUBSTR(a_names(i), HOT_KEY)
*  ENDIF
  a_codes(i) = CODE
  a_val(i)   = M_RUB
  a_dirs(i)  = M_DIR

ENDSCAN

a_t_c = 1

DO WHILE .T.
IF USED("MM_TYPES")
  SELECT MM_TYPES
ELSE
  USE (base_path+"MM_TYPES") ORDER TAG NAME
ENDIF
ex = 1
oper_c = 0
oper_n = SPACE(16)

*                          Период
*┌────────────────────────────────────────────────────────────┐
*│                                                            │
*│               Операция ....:....!....:....!                │
*│                                                            │
*│   Начальная дата ДД.ММ.ГГ       Конечная дата ДД.ММ.ГГ     │
*│                                                            │
*│              < OK Ctrl-W > < Отказаться Esc >              │
*│                                                            │
*└────────────────────────────────────────────────────────────┘
*
PUSH KEY CLEAR 
DO Prp_Nav_2
DO D_Win_N WITH 10, 64, "Период"

@ 3, 17 SAY "Операция" GET oper_n WHEN W_Oper()
@ 5,  5 SAY "Начальная дата" GET dat_0 PICTURE "@D"
@ 5, 35 SAY "Конечная дата"  GET dat_1 PICTURE "@D"
@ 7, 16 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
READ CYCLE VALID V_Dat()

POP KEY
RELEASE WINDOW (win_name)
IF ex = 2
  RETURN
ENDIF
SET ORDER TO TAG CODE
SEEK oper_c
a_t_d = M_DIR
a_t_v = M_RUB
a_t_n = oper_n
a_t_c = oper_c

DO Wt_Mess WITH "Подборка данных..."

USE (base_path+"KASSA_M") ORDER TAG HRON

DELETE FILE (tmpo_path+"OPER_REP.DBF")

SELECT 0
*IF a_t_v
  pic_r = REPLICATE("9", FSIZE("SUM_R","KASSA_M")+1)
  pic_d = REPLICATE("9", FSIZE("SUM_D","KASSA_M")+1)
  pic_r = STUFF(pic_r, LEN(pic_r)-2, 1, ".")
  pic_d = STUFF(pic_d, LEN(pic_d)-4, 1, ".")
  pic_k = "99.999"
  CREATE DBF (tmpo_path+"OPER_REP.DBF")  ;
     ( O_NUM    C(FSIZE("DOC_NUM","KASSA_M")+1),  ;
       O_DATE   D, ;
       KURS     N(6,3),   ;
       R_MONEY  N(FSIZE("SUM_R","KASSA_M"),2),    ;
       D_MONEY  N(FSIZE("SUM_D","KASSA_M"),4),    ;
       O_NOTE   C(FSIZE("NOTE","KASSA_M")) )
*ELSE
*  pic = REPLICATE("9", FSIZE("SUM_D","KASSA_M"))
*  pic = STUFF(pic, LEN(pic)-4, 1, ".")
*  CREATE DBF (tmpo_path+"OPER_REP.DBF")  ;
*     ( O_NUM    C(FSIZE("DOC_NUM","KASSA_M")+1),  ;
*       O_DATE   D, ;
*       O_MONEY  N(FSIZE("SUM_D","KASSA_M"),4),    ;
*       O_NOTE   C(FSIZE("NOTE","KASSA_M")) )
*ENDIF
USE (tmpo_path+"OPER_REP.DBF") EXCLUSIVE
SET NEAR ON
SELECT 0
USE (base_path+"CURS_SET") ORDER TAG HRON DESCENDING ALIAS CURS

SELECT KASSA_M
IF .NOT. SEEK(DTOS(dat_0))
  i = RECNO(0)
  IF BETWEEN(i,1,RECCOUNT())
    GO i
  ELSE
    GO BOTTOM
    SKIP
  ENDIF
ENDIF

SCAN REST WHILE DOC_DATE <= dat_1
  IF CODE # a_t_c
    LOOP
  ENDIF
  c_flt = Get_Curs("CURS_B","CURS",DTOS(DOC_DATE))
  SELECT OPER_REP
  APPEND BLANK
  REPLACE O_NUM   WITH LEFT(KASSA_M.DOC_NUM,1)+"-"+ALLTRIM(SUBSTR(KASSA_M.DOC_NUM,2)), ;
          O_DATE  WITH KASSA_M.DOC_DATE, ;
          KURS    WITH c_flt,            ;
          R_MONEY WITH IIF(a_t_d=0, 1, -1) * IIF(a_t_v, KASSA_M.SUM_R, KASSA_M.SUM_D*c_flt),  ;
          D_MONEY WITH IIF(a_t_d=0, 1, -1) * IIF(a_t_v, KASSA_M.SUM_R/c_flt, KASSA_M.SUM_D),  ;
          O_NOTE  WITH KASSA_M.NOTE
  SELECT KASSA_M
ENDSCAN
USE

SELECT OPER_REP
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не было ни одной такой кассовой операции за указанный Вами период!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"OPER_REP.DBF")
  DO Wt_Mess
  LOOP
ENDIF

PRIVATE sb, p_ln, fnt, n_cp, lft, t_p, f_p, f_d, n, i, ffeed, p_drctry
DIMENSION sb(8,2)

*{Pgs}      - Страница/всего страниц
*{Type}     - Тип депозитов
*{Dat1  }   - Начальная дата
*{Dat2  }   - Конечная дата
*{T001    } - Сумма остатков на начальную дату на странице
*{T101    } - Сумма остатков на начальную дату по документу


sb(1,1) = "{Pgs}"
sb(1,2) = ""

sb(2,1) = "{Type}"
sb(2,2) = ALLTRIM(a_t_n)

sb(3,1) = "{Dat1  }"
sb(3,2) = TRANSFORM(dat_0,"@D ")

sb(4,1) = "{Dat2  }"
sb(4,2) = TRANSFORM(dat_1,"@D ")

sb(5,1) = "{T001    }"
sb(5,2) = "          "

sb(6,1) = "{T101    }"
sb(6,2) = "          "

sb(7,1) = "{T002    }"
sb(7,2) = "          "

sb(8,1) = "{T102    }"
sb(8,2) = "          "

SELECT 0

USE (base_path+"DOC_FORM")

LOCATE FOR "OPER_REP" == ALLTRIM(UPPER(DOC_NAME))

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
SELECT OPER_REP
GO TOP
DO Wt_Mess

PRIVATE s001, s101, s002, s102
PRIVATE n_pg, f_pg, n_det, j, stw

n_det = p_ln-ALEN(t_p)-ALEN(f_p)-ALEN(f_d)
n_pg  = CEILING(RECCOUNT()/n_det)

s101 = 0
s102 = 0
DO Ini_Prn WITH "Справка по кассовой операции", p_ln, lft, n_cp, fnt, ffeed, p_drctry

FOR f_pg = 1 TO n_pg
  IF f_pg # 1
    EJECT
  ENDIF
  s001 = 0
  s002 = 0
  sb(1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  FOR i = 1 TO ALEN(t_p)
    stw = t_p(i)
    FOR j = 1 TO 8
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR
  FOR i = 1 TO n_det
    stw = PADR(TRANSFORM(O_DATE,"@D "), 11)+" "+ ;
          O_NUM+" "+ ;
          TRANSFORM(KURS, pic_k)+" "+ ;
          TRANSFORM(R_MONEY, pic_r)+" "+ ;
          TRANSFORM(D_MONEY, pic_d)+" "+ ;
          O_NOTE
    s001 = s001+R_MONEY
    s002 = s002+D_MONEY
    @ PROW()+1, 0 SAY stw
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR

  sb(5,2) = STR(s001, 10, 2)
  sb(7,2) = STR(s002, 10, 4)

  s101 = s101+s001
  s102 = s102+s002

  FOR i = 1 TO ALEN(f_p)
    stw = f_p(i)
    FOR j = 1 TO 8
      stw = STRTRAN(stw, sb(j,1), sb(j,2))
    ENDFOR
    @ PROW()+1, 0 SAY stw
  ENDFOR
  IF EOF()
    EXIT
  ENDIF
ENDFOR

sb(6,2) = STR(s101, 10, 2)
sb(8,2) = STR(s102, 10, 4)

FOR i = 1 TO ALEN(f_d)
  stw = f_d(i)
  FOR j = 1 TO 8
    stw = STRTRAN(stw, sb(j,1), sb(j,2))
  ENDFOR
  @ PROW()+1, 0 SAY stw
ENDFOR

DO Term_Prn WITH ""

CLOSE DATABASES
DELETE FILE (tmpo_path+"OPER_REP.DBF")

ENDDO

RETURN

*********************************************************************
PROCEDURE W_Oper

DO Ch_Oper WITH oper_c, oper_n
SHOW GET oper_n

RETURN .F.

*********************************************************************
PROCEDURE V_Dat

IF ex = 2
  RETURN .T.
ENDIF

IF oper_c = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы так и не указали операцию!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
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

RETURN .T.
