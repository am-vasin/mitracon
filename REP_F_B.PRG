*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                           Отчет для Б. Р.                             ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Rep_F_B
PRIVATE set_n, set_c, mss, m_nal, m_bn, is_fil, v_sw, c_flt
PRIVATE ln, wd, dat_0, dat_1, dat_v0, dat_v1, ex, dat_fl, p_numb, p_line

set_n = SET("NEAR") = "OFF"
SET NEAR ON
set_c = SET("CENTURY") = "ON"
*SET CENTURY OFF

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
PRIVATE ln, ln1, l_ex, wk_list, k_manual, k_manual_v
USE (base_path+"PLACES")
p_numb = RECCOUNT()
p_line = ""
FOR dat_1 = 1 TO p_numb
  p_line = p_line+CHR(ASC("A")+dat_1-1)
ENDFOR

USE (base_path+"PARMS")
dat_0  = DAT_START
dat_v0 = DAT_START
USE
dat_1  = DATE()
dat_v1 = DATE()

DIMENSION wk_list(p_numb)
FOR ex = 1 TO p_numb
  wk_list(ex) = CHR(ASC("A")+ex-1) $ p_line
ENDFOR

ex   = 1
k_manual = .F.
k_manual_v = 0
v_sw = 2
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
*
*┌────────────────────────────────────────────────────────────────┐
*│ ┌─ Интервал ────────────────┐ ┌─ Площадки ───────────────────┐ │
*│ │                           │ │                              │ │
*│ │ Начальная дата ДД.ММ.ГГГГ │ │ ( ) ....:....!....:....!.... │ │
*│ │                           │ │                              │ │
*│ │ Конечная дата  ДД.ММ.ГГГГ │ │                              │ │
*│ │                           │ │                              │ │
*│ └───────────────────────────┘ └──────────────────────────────┘ │
*│                    ( ) Рубли   ( ) Доллары                     │
*│               < OK Ctrl-W > < Отказаться Esc >                 │
*└────────────────────────────────────────────────────────────────┘

ln1  = p_numb+4
ln   = MAX(12, ln1+5)
l_ex = ln - 2

DO D_Win_N WITH ln, 68, "Фильтр"

DO Sun_Bord WITH 2, 3, 8, 31
@  2, 5 SAY " Интервал "

DO Sun_Bord WITH 2, 32, 2+ln1-1, 64
@  2, 34 SAY " Площадки "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 4,  5 SAY "Начальная дата" GET dat_0 PICTURE "@D"
@ 6,  5 SAY "Конечная дата " GET dat_1 PICTURE "@D"
DO G_Place WITH 4, 35, wk_list
@ WROWS()-3, 17 GET v_sw PICTURE "@*RH Рубли  ;Доллары"
*@ WROWS()-3, 17 GET k_manual PICTURE "@*C Установить курс равным" VALID S_Value()
*@ WROWS()-3, 44 GET k_manual_v PICTURE "@Z 999.999" WHEN k_manual VALID S_Value()

@ WROWS()-2, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID V_Dat()
v_sw = v_sw = 1

POP KEY
RELEASE WINDOW (win_name)

IF ex = 2
  IF set_n
    SET NEAR OFF
  ENDIF
  IF set_c
    SET CENTURY ON
  ENDIF
  RETURN
ENDIF

DELETE FILE(tmpo_path+"REP_F_B.DBF")
DELETE FILE(tmpo_path+"REP_F_B.CDX")

CREATE DBF (tmpo_path+"REP_F_B")  ;
    (    DAT     D,      ;
         CURS    N( 6,3),  ;
         DELTA   N(12,4),  ;
         SUM_N   N(12,4),  ;
         SUM_B   N(12,4),  ;
         DELTA_F N(12,4),  ;
         SUM_F_N N(12,4),  ;
         SUM_F_B N(12,4)  )
SELECT 0
USE (base_path+"CUSTOMER") ORDER TAG CODE

SELECT 0
USE (base_path+"CURS_SET") ORDER TAG HRON DESCENDING ALIAS CURS

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON

dat_fl = {}
SEEK DTOS(dat_0)
SCAN REST WHILE DOC_DATE <= dat_1 FOR LEFT(DOC_NUM,1) $ p_line .AND. CUS_CODE >= 0
  SELECT REP_F_B
  IF dat_fl # SALE_TIT.DOC_DATE
    dat_fl = SALE_TIT.DOC_DATE
    c_flt = Get_Curs("CURS", "CURS", dat_fl)
    SELECT REP_F_B
    APPEND BLANK
    REPLACE DAT  WITH dat_fl,;
            CURS WITH c_flt
  ENDIF
  is_fil = .F.
  IF SALE_TIT.CUS_CODE # 0
    IF SEEK(SALE_TIT.CUS_CODE, "CUSTOMER")
      is_fil = .NOT. EMPTY(CUSTOMER.RYN)
    ENDIF
  ENDIF
  DO CASE
  CASE is_fil
    IF SALE_TIT.NEW_TYPE = " "
      REPLACE SUM_F_B WITH SUM_F_B+IIF(v_sw, SALE_TIT.TOTAL_R, SALE_TIT.TOTAL_D)
    ELSE
      REPLACE SUM_F_N WITH SUM_F_N+IIF(v_sw, SALE_TIT.TOTAL_R, SALE_TIT.TOTAL_D)
    ENDIF
  OTHERWISE
    IF SALE_TIT.NEW_TYPE = " "
      REPLACE SUM_B WITH SUM_B+IIF(v_sw, SALE_TIT.TOTAL_R, SALE_TIT.TOTAL_D)
    ELSE
      REPLACE SUM_N WITH SUM_N+IIF(v_sw, SALE_TIT.TOTAL_R, SALE_TIT.TOTAL_D)
    ENDIF
  ENDCASE
  SELECT SALE_TIT
ENDSCAN

PRIVATE no, vl_sum
no = SET("NEAR") = "OFF"
SET NEAR ON
USE (base_path+"INCMONEY") ORDER TAG HRON
SELECT REP_F_B
SCAN
  SELECT INCMONEY
  SEEK DTOS(REP_F_B.DAT)
  SCAN REST WHILE DOC_DATE = REP_F_B.DAT   ;
                  FOR DOC_TYPE = " " .AND. ;
                  LEFT(DOC_NUM,1) $ p_line .AND. CUS_CODE >= 0
    is_fil = .F.
    vl_sum = INCMONEY.MONEY_R
    IF .NOT. v_sw
      c_flt = Get_Curs("CURS", "CURS", INCMONEY.DOC_DATE)
      vl_sum = vl_sum/c_flt
    ENDIF
    IF INCMONEY.CUS_CODE # 0
      IF SEEK(INCMONEY.CUS_CODE, "CUSTOMER")
        is_fil = .NOT. EMPTY(CUSTOMER.RYN)
      ENDIF
    ENDIF
    IF is_fil
      REPLACE REP_F_B.DELTA_F WITH REP_F_B.DELTA_F+vl_sum
    ELSE
      REPLACE REP_F_B.DELTA WITH REP_F_B.DELTA+vl_sum
    ENDIF
    SELECT INCMONEY
  ENDSCAN
  SELECT REP_F_B
ENDSCAN
SELECT INCMONEY
IF no
  SET NEAR OFF
ENDIF

USE
SELECT CURS
USE
SELECT REP_F_B
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"За указанный Вами период продаж не было!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  DO P_Doc
ENDIF
CLOSE DATABASES
DELETE FILE(tmpo_path+"REP_F_B.DBF")
DELETE FILE(tmpo_path+"REP_F_B.CDX")
IF set_n
  SET NEAR OFF
ENDIF
IF set_c
  SET CENTURY ON
ENDIF

RETURN

*********************************************************************
PROCEDURE S_Value    && Ручная установка курса
IF VARREAD == "K_MANUAL_V"
  IF k_manual_v <= 0
    k_manual_v = 0
    k_manual = .F.
    SHOW GET k_manual
    SHOW GET k_manual_v
  ENDIF
ELSE
  IF .NOT. k_manual
    k_manual_v = 0
    SHOW GET k_manual_v
  ENDIF
ENDIF

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
PRIVATE l_tmp, i

l_tmp = ""
FOR i = 1 TO p_numb
  IF wk_list(i)
    l_tmp = l_tmp+CHR(ASC("A")+i-1)
  ENDIF
ENDFOR

IF EMPTY(l_tmp)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Если Вас не интересует ни одна площадка?!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
IF k_manual_v <= 0
  k_manual_v = 0
  k_manual = .F.
ENDIF

p_line = l_tmp

RETURN .T.

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                    Программа печати отчета.                           ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE P_Doc

PRIVATE p_ln, fnt, n_cp, lft, sb, n, t_p, f_p, f_d, i, pic, p_drctry
PRIVATE n_p_det, n_pg, f_pg, sp1, sp2, sp3, sp4, sp5, sp6, sp7, ;
                             sd1, sd2, sd3, sd4, sd5, sd6, sd7
PRIVATE mss

DIMENSION sb(17,2)
sb( 1,1) = "{P/P      }"
sb( 1,2) = ""

sb( 2,1) = "{Dat1  }"
sb( 2,2) = TRANSFORM(dat_0,"@D")

sb( 3,1) = "{Dat2  }"
sb( 3,2) = TRANSFORM(dat_1,"@D")

sb( 4,1) = "{Sum_P1    }"
sb( 4,2) = ""

sb( 5,1) = "{Sum_P2    }"
sb( 5,2) = ""

sb( 6,1) = "{Sum_P3    }"
sb( 6,2) = ""

sb( 7,1) = "{Sum_P4    }"
sb( 7,2) = ""

sb( 8,1) = "{Sum_P5    }"
sb( 8,2) = ""

sb( 9,1) = "{Sum_P6    }"
sb( 9,2) = ""

sb(10,1) = "{Sum_P7    }"
sb(10,2) = ""

sb(11,1) = "{Sum_D1    }"
sb(11,2) = ""

sb(12,1) = "{Sum_D2    }"
sb(12,2)   = ""

sb(13,1) = "{Sum_D3    }"
sb(13,2)   = ""

sb(14,1) = "{Sum_D4    }"
sb(14,2)   = ""

sb(15,1) = "{Sum_D5    }"
sb(15,2)   = ""

sb(16,1) = "{Sum_D6    }"
sb(16,2)   = ""

sb(17,1) = "{Sum_D7    }"
sb(17,2)   = ""

n = ALEN(sb,1)
FOR i = 1 TO n
  sb(i,2) =PADR(sb(i,2), LEN(sb(i,1)))
ENDFOR

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "REP_F_B" = ALLTRIM(UPPER(DOC_NAME))
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

SELECT REP_F_B

DIMENSION mss(4)
mss(1) = ""
mss(2) = CHR(0)+"В чем хотим получить отчет?"
mss(3) = CHR(0)+"  [ В рублях ] [ В USD ]"
mss(4) = ""
*v_sw = Out_Mess(5,"mss") = 1
pic = IIF(v_sw, "@Z 999999999.99", "@Z 9999999.9999")

DO Ini_Prn WITH "Выручка за период", p_ln, lft, n_cp, fnt, ffeed, p_drctry

n_p_det = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = CEILING(RECCOUNT()/n_p_det)
ENDIF

STORE 0 TO sp1, sp2, sp3, sp4, sp5, sp6, sp7, sd1, sd2, sd3, sd4, sd5, sd6, sd7, curs_w

GO TOP
FOR f_pg = 1 TO n_pg
  IF f_pg = 1
    sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  ELSE
    sb( 1,2) = ""
  ENDIF
  sb( 1,2) =PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1
    EJECT
  ENDIF
  FOR i = 1 TO ALEN(t_p)
    str_w = t_p(i)
    FOR j = 1 TO ALEN(sb,1)
      str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
    ENDFOR
    IF i = 1
      @ PROW(), PCOL() SAY str_w
    ELSE
      @ PROW()+1, 0 SAY str_w
    ENDIF
  ENDFOR
  STORE 0 TO sp1, sp2, sp3, sp4

  FOR i = 1 TO n_p_det
    curs_w = IIF(k_manual .AND. k_manual_v > 0, k_manual_v, CURS)
    @ PROW()+1,0 SAY   ;
         "║"+DTOC(DAT)+"│"+STR(curs_w,6,3)+"║"+  ;
         TRANSFORM(SUM_N+DELTA,pic)+"│"+  ;
         TRANSFORM(SUM_B-DELTA,pic)+"│"+  ;
         TRANSFORM(SUM_N+SUM_B,pic)+"║"+  ;
         TRANSFORM(SUM_F_N+DELTA_F,pic)+"│"+  ;
         TRANSFORM(SUM_F_B-DELTA_F,pic)+"│"+  ;
         TRANSFORM(SUM_F_N+SUM_F_B,pic)+"║"+  ;
         TRANSFORM(SUM_F_N+SUM_F_B+SUM_N+SUM_B,pic)+"║"
    sp1 = sp1 + SUM_N+DELTA
    sp2 = sp2 + SUM_B-DELTA
    sp3 = sp3 + SUM_N+SUM_B
    sp4 = sp4 + SUM_F_N+DELTA_F
    sp5 = sp5 + SUM_F_B-DELTA_F
    sp6 = sp6 + SUM_F_N+SUM_F_B
    sp7 = sp7 + SUM_F_N+SUM_F_B+SUM_N+SUM_B
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
  sb( 4,2) = TRANSFORM(sp1,pic)
  sb( 5,2) = TRANSFORM(sp2,pic)
  sb( 6,2) = TRANSFORM(sp3,pic)
  sb( 7,2) = TRANSFORM(sp4,pic)
  sb( 8,2) = TRANSFORM(sp5,pic)
  sb( 9,2) = TRANSFORM(sp6,pic)
  sb(10,2) = TRANSFORM(sp7,pic)
  sd1 = sd1+sp1
  sd2 = sd2+sp2
  sd3 = sd3+sp3
  sd4 = sd4+sp4
  sd5 = sd5+sp5
  sd6 = sd6+sp6
  sd7 = sd7+sp7
  IF f_pg = n_pg
    FOR i = 1 TO ALEN(f_d)
      sb(11,2) = TRANSFORM(sd1,pic)
      sb(12,2) = TRANSFORM(sd2,pic)
      sb(13,2) = TRANSFORM(sd3,pic)
      sb(14,2) = TRANSFORM(sd4,pic)
      sb(15,2) = TRANSFORM(sd5,pic)
      sb(16,2) = TRANSFORM(sd6,pic)
      sb(17,2) = TRANSFORM(sd7,pic)
      str_w = f_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR
USE

DO Term_Prn WITH ""

RETURN
