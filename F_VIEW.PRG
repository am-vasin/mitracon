*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа просмотра денег филиалов.                    ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE F_View

PRIVATE s, d, sd, sr, pd, pr, nr

*
*   Если нет файла, то создать!
*

IF .NOT. File_O(base_path+"F_VIEW.DBF")
  CREATE DBF (base_path+"F_VIEW")  ;
       (  F_CODE  N( 8),   ;
          FIL_NAM C(16),   ;
          P_DATE  D,       ;
          USD_P   N( 6,3), ;
          SUM_D   N(10,4), ;
          SUM_R   N(10,2), ;
          PRC     N(7,3)     )
  USE (base_path+"F_VIEW") EXCLUSIVE
  INDEX ON F_CODE TAG CODE
ENDIF

*
*   Формируем/корректируем список филиалов
*
USE (base_path+"F_VIEW") ORDER TAG CODE

SELECT 0
USE (base_path+"CUSTOMER") ORDER TAG CODE
SET FILTER TO .NOT. EMPTY(RYN)

SELECT F_VIEW
SCAN
  IF .NOT. SEEK(F_CODE, "CUSTOMER")
    DELETE
  ENDIF
  REPLACE P_DATE WITH {},  ;
          USD_P  WITH 0,   ;
          SUM_D  WITH 0,   ;
          SUM_R  WITH 0
ENDSCAN
SET ORDER TO TAG CODE

SELECT CUSTOMER
GO TOP
SCAN
  IF .NOT. SEEK(CODE, "F_VIEW")
    SELECT F_VIEW
    APPEND BLANK
    REPLACE F_CODE  WITH CUSTOMER.CODE, ;
            FIL_NAM WITH CUSTOMER.B_NAME
  ELSE
    SELECT F_VIEW
    REPLACE FIL_NAM WITH CUSTOMER.B_NAME
  ENDIF
  SELECT CUSTOMER
ENDSCAN

*
*  Готовим данные по филиалам
*
USE (base_path+"CURS_SET") ORDER TAG HRON DESCENDING ALIAS CURS

SELECT 0
USE (base_path+"RYN_L") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"RYN_L_T")
DO Wt_Mess WITH "Подготовка данных..."

SCAN
  IF .NOT. SEEK(CUS_CODE, "F_VIEW")
    LOOP
  ENDIF
  IF .NOT. SEEK(CUS_CODE, "RYN_L")
    LOOP
  ENDIF
  *
  *   Определяем курс доллара в тот момент
  *
  s = Get_Curs("CURS", "CURS", RYN_L_T.DATE_P)
  d = RYN_L_T.DATE_P
  sr = 0
  sd = 0
  SELECT RYN_L
  SCAN REST WHILE CUS_CODE = F_VIEW.F_CODE
    IF RYN_L_T.VALUTA = "$"
      sd = sd+QNT*PRICE_D
    ELSE
      sr = sr+QNT*PRICE_R
    ENDIF
  ENDSCAN
  
  SELECT F_VIEW
  REPLACE P_DATE WITH d,  ;
          USD_P  WITH s,  ;
          SUM_D  WITH sd, ;
          SUM_R  WITH sr

  SELECT RYN_L_T

ENDSCAN

USE

SELECT CURS
USE

SELECT RYN_L
USE

SELECT F_VIEW
SET FILTER TO SUM_R # 0 .OR. SUM_D # 0

SUM SUM_R, SUM_D, SUM_R*PRC/100, SUM_D*PRC/100 TO sr, sd, pr, pd

DO Wt_Mess

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
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Печать документа" KEY Ctrl-A, "F1"
ON SELECTION PAD Print OF (menu_name) Do Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, pic_r, pic_d, pic_p

pic_r = "@Z 9999999.99"
pic_d = "@Z 999999.9999"
pic_p = "@Z 999.999"

ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE

wd = FSIZE("FIL_NAM")+53

DO D_Wins WITH ln, wd, "Филиалы", 1, 2

@ 1, 2 SAY PADR("Филиал",FSIZE("FIL_NAM"))+" "+ ;
           PADL("Сумма (р)", 10)+" "+  ;
           PADL("Сумма ($)", 10)+" "+  ;
           PADL("Процент", 7)+" "+  ;
           PADL("% (р)", 10)+" "+  ;
           PADL("% ($)", 10)
@ WROWS()-3, 3  SAY "Итого"
@ WROWS()-3, 19 SAY TRANSFORM(sr,pic_r)+" "+TRANSFORM(sd,pic_d)
@ WROWS()-3, 49 SAY TRANSFORM(pr,pic_r)+" "+TRANSFORM(pd,pic_d)
what_do = "list"

SET ORDER TO 0
GO TOP
DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F1 DO Sw_Mode WITH "Print"

*
*    BROWSE - меню
*
    BROWSE FIELDS FIL_NAM:H="",   ;
                  SUM_R:H="":P=pic_r, ;
                  SUM_D:H="":P=pic_d, ;
                  PRC:H="":P=pic_p:V=Show_Tot(),     ;
                  F00_R= PRC*SUM_R/100:H="":P=pic_r, ;
                  F00_D= PRC*SUM_D/100:H="":P=pic_d  ;
           FREEZE PRC      ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WHEN Show_D()   ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "list"
      what_do = IIF(READKEY() % 256 = 12, "exit", "edit")
    ENDIF

  CASE what_do = "Print"    && Печать документа

    DO P_View
    what_do = "list"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR WINDOWS
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа вывода даты и курса.                         ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Show_D

PRIVATE s

s = "Цены установлены "+DTOC(P_DATE)+" по курсу "+ALLTRIM(STR(USD_P,10,3))+"      "
ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-2, 2 SAY s
ACTIVATE WINDOW (ALIAS())

RETURN .T.

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа вывода итоговой суммы.                       ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Show_Tot

PRIVATE s

SUM SUM_R*PRC/100, SUM_D*PRC/100 TO pr, pd
ACTIVATE WINDOW (win_name+"_M") SAME
@ WROWS()-3, 49 SAY TRANSFORM(pr,pic_r)+" "+TRANSFORM(pd,pic_d)
ACTIVATE WINDOW (ALIAS())

RETURN .T.

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                   Программа печати документа.                         ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE P_View

PRIVATE p_ln, fnt, n_cp, lft, ffeed, p_drctry
PRIVATE sb, n, t_p, f_p, f_d, rc_sav

*{ДД.ММ.ГГ}    - Дата документа;
*{Pg/Pgs}      - Номер страницы/Кол-во страниц;
*{Tot_R_P_1  } - Сумма в рублях на странице;
*{Tot_D_P_1  } - Сумма в USD на странице;
*{Tot_R_P_2  } - Процент в рублях на странице;
*{Tot_D_P_2  } - Процент в USD на странице;
*{Tot_R_D_1  } - Сумма в рублях по документу;
*{Tot_D_D_1  } - Сумма в USD по документу;
*{Tot_R_D_2  } - Процент в рублях по документу;
*{Tot_D_D_2  } - Процент в USD по документу.

DIMENSION sb(10,2)
sb( 1,1) = "{ДД.ММ.ГГ}"
sb( 1,2) = PADL(DTOC(DATE()), 10)

sb( 2,1) = "{Pg/Pgs}"
sb( 2,2) = ""

sb( 3,1) = "{Tot_R_P_1  }"
sb( 3,2) = ""

sb( 4,1) = "{Tot_D_P_1  }"
sb( 4,2) = ""

sb( 5,1) = "{Tot_R_P_2  }"
sb( 5,2) = ""

sb( 6,1) = "{Tot_D_P_2  }"
sb( 6,2) = ""

sb( 7,1) = "{Tot_R_D_1  }"
sb( 7,2) = ""

sb( 8,1) = "{Tot_D_D_1  }"
sb( 8,2) = ""

sb( 9,1) = "{Tot_R_D_2  }"
sb( 9,2) = ""

sb(10,1) = "{Tot_D_D_2  }"
sb(10,2) = ""

rc_sav = RECNO()

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "F_VIEW" = ALLTRIM(UPPER(DOC_NAME))
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

SELECT F_VIEW
GO TOP

DO Ini_Prn WITH "Наличие в филиалах (деньги)", p_ln, lft, n_cp, fnt, ffeed, p_drctry

PRIVATE n_p_det, n_pg, f_pg, f_lines
COUNT TO f_lines
GO TOP

n_p_det = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))
IF n_p_det <= 0
  n_p_det = f_lines
  n_pg = 1
ELSE
  n_pg = CEILING(f_lines/n_p_det)
ENDIF

PRIVATE sp1, sp2, sp3, sp4, sd1, sd2, sd3, sd4, str_w, i, j

STORE 0 TO sd1, sd2, sd3, sd4
FOR f_pg = 1 TO n_pg
  sb( 2,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 2,2) = PADL(sb(2,2), LEN(sb(2,1)))
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

    @ PROW()+1,0 SAY   ;
       FIL_NAM+" "+PADR(DTOC(P_DATE),10)+STR(USD_P,6,3)+" "+ ;
       TRANSFORM(SUM_R,"@Z ## ### ###.##")+" "+ ;
       TRANSFORM(SUM_D,"@Z ### ###.####")+" "+ ;
       TRANSFORM(PRC,"@Z 999.999")+" "+ ;
       TRANSFORM(SUM_R*PRC/100,"@Z ## ### ###.##")+" "+ ;
       TRANSFORM(SUM_D*PRC/100,"@Z ### ###.####")
    sp1 = sp1 + SUM_R
    sp2 = sp2 + SUM_D
    sp3 = sp3 + SUM_R*PRC/100
    sp4 = sp4 + SUM_D*PRC/100
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
  IF f_pg = n_pg
    EXIT
  ENDIF
  sb( 3,2) = TRANSFORM(sp1,"@Z ## ### ###.##")
  sb( 4,2) = TRANSFORM(sp2,"@Z ### ###.####")
  sb( 5,2) = TRANSFORM(sp3,"@Z ## ### ###.##")
  sb( 6,2) = TRANSFORM(sp4,"@Z ### ###.####")
  FOR i = 1 TO ALEN(f_p)
    str_w = f_p(i)
    FOR j = 1 TO ALEN(sb,1)
      str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
    ENDFOR
    @ PROW()+1, 0 SAY str_w
  ENDFOR
  sd1 = sd1+sp1
  sd2 = sd2+sp2
  sd3 = sd3+sp3
  sd4 = sd4+sp4
ENDFOR

sd1 = sd1+sp1
sd2 = sd2+sp2
sd3 = sd3+sp3
sd4 = sd4+sp4
sb( 3,2) = TRANSFORM(sp1,"@Z ## ### ###.##")
sb( 4,2) = TRANSFORM(sp2,"@Z ### ###.####")
sb( 5,2) = TRANSFORM(sp3,"@Z ## ### ###.##")
sb( 6,2) = TRANSFORM(sp4,"@Z ### ###.####")
sb( 7,2) = TRANSFORM(sd1,"@Z ## ### ###.##")
sb( 8,2) = TRANSFORM(sd2,"@Z ### ###.####")
sb( 9,2) = TRANSFORM(sd3,"@Z ## ### ###.##")
sb(10,2) = TRANSFORM(sd4,"@Z ### ###.####")

FOR i = 1 TO ALEN(f_d)
  str_w = f_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR

DO Term_Prn WITH ""

GO rc_sav

RETURN
