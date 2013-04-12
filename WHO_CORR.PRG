*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Who_Corr     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Кол-во коррекций наличия за период.                    ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 30.04.1999 ══╝
PROCEDURE Who_Corr

PRIVATE dt0, dt1, c_code, c_name, l_code, l_name, l
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
l = 0
SCAN WHILE CUS_CODE < 0
  l = l+1
  DIMENSION l_code(l), l_name(l)
  l_code(l) = CUS_CODE
  l_name(l) = CUS_NAME
ENDSCAN
USE
IF l = 0
  PRIVATE mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одного спецклиента..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

dt0 = {}
dt1 = DATE()
c_code = 1
c_name = l_name(1)
IF .NOT. G_Period(dt0, dt1, c_code)
  RETURN
ENDIF
c_name = l_name(c_code)
c_code = l_code(c_code)
DO Wt_Mess WITH " Подготовка данных... "
DO Prep_Tmp
DO Wt_Mess
DO P_Doc
USE
DELETE FILE (tmpo_path+"WHO000.DBF")

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Tmp     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Подготовка файла.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 30.04.1999 ──┘
PROCEDURE Prep_Tmp
PRIVATE rc, w

DELETE FILE (tmpo_path+"WHO_CORR.DBF")
DELETE FILE (tmpo_path+"WHO_CORR.CDX")

CREATE DBF (tmpo_path+"WHO_CORR.DBF") ;
  ( CODE     N( 3), ;
    NAME     C(30), ;
    QNT_POS  N(10), ;
    QNT      N(10), ;
    QNT_PACK N(10) )
INDEX ON CODE TAG CODE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"ACT_S") ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK

SELECT 0
USE (base_path+"ACT_S_T") ORDER TAG SUP_CODE

SEEK c_code
SCAN REST WHILE SUP_CODE = c_code FOR BETWEEN(DOC_DATE, dt0, dt1)
  w = DTOC(DOC_DATE)+" "+DOC_NUM
  @ WROWS()-1, 0 SAY PADC(w, WCOLS())
  IF .NOT. SEEK(WHO_ACT, "WHO_CORR")
    SELECT WHO_CORR
    APPEND BLANK
    REPLACE CODE WITH ACT_S_T.WHO_ACT
  ENDIF
  SELECT ACT_S
  SEEK ACT_S_T.FIRM+LEFT(DTOS(ACT_S_T.DOC_DATE),4)+ACT_S_T.DOC_NUM
  SCAN REST WHILE FIRM = ACT_S_T.FIRM .AND. DOC_DATE = ACT_S_T.DOC_DATE .AND. DOC_NUM = ACT_S_T.DOC_NUM
    REPLACE WHO_CORR.QNT_POS  WITH WHO_CORR.QNT_POS+1, ;
            WHO_CORR.QNT      WITH WHO_CORR.QNT+ABS(QNT_INC),   ;
            WHO_CORR.QNT_PACK WITH WHO_CORR.QNT_PACK+ABS(FLOOR(QNT_INC/MAX(STOCK.QNT_ROUND,1)))
  ENDSCAN
  SELECT ACT_S_T
ENDSCAN

SELECT ACT_S
USE (base_path+"SALE") ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK

SELECT ACT_S_T
USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE

SEEK c_code
SCAN REST WHILE CUS_CODE = c_code FOR BETWEEN(DOC_DATE, dt0, dt1)
  w = DTOC(DOC_DATE)+" "+LEFT(DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(DOC_NUM,5))
  @ WROWS()-1, 0 SAY PADC(w, WCOLS())
  IF .NOT. SEEK(WHO, "WHO_CORR")
    SELECT WHO_CORR
    APPEND BLANK
    REPLACE CODE WITH SALE_TIT.WHO
  ENDIF
  SELECT SALE
  SEEK SALE_TIT.FIRM+LEFT(DTOS(SALE_TIT.DOC_DATE),4)+SALE_TIT.DOC_NUM
  SCAN REST WHILE FIRM = SALE_TIT.FIRM .AND. DOC_DATE = SALE_TIT.DOC_DATE .AND. DOC_NUM = SALE_TIT.DOC_NUM
    REPLACE WHO_CORR.QNT_POS  WITH WHO_CORR.QNT_POS+1, ;
            WHO_CORR.QNT      WITH WHO_CORR.QNT+ABS(QNT),   ;
            WHO_CORR.QNT_PACK WITH WHO_CORR.QNT_PACK+ABS(FLOOR(QNT/MAX(STOCK.QNT_ROUND,1)))
  ENDSCAN
  SELECT SALE_TIT
ENDSCAN
USE

SELECT STOCK
USE (base_path+"PERSONS") ORDER TAG CODE
SELECT WHO_CORR
SET RELATION TO CODE INTO PERSONS
REPLACE ALL NAME WITH ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)
SET RELATION TO
SELECT PERSONS
USE
SELECT WHO_CORR
INDEX ON NAME TAG NAME
COPY TO (tmpo_path+"WHO000")
USE (tmpo_path+"WHO000") ALIAS WHO_CORR
DELETE FILE (tmpo_path+"WHO_CORR.DBF")
DELETE FILE (tmpo_path+"WHO_CORR.CDX")
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла G_Period     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                        Программа ввода периода.                        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 04/30/1999 ══╝
PROCEDURE G_Period
PARAMETERS d0, d1, c_c

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

*┌───────────────────────────────────────────────────────────────┐
*│ Начальная дата ДД.ММ.ГГГГ (F1)  Конечная дата ДД.ММ.ГГГГ (F1) │
*│    ┌────────────────────────────────────────────────────┐     │
*│    │ ....:....!....:....!....:....!....:....!....:....! │     │
*│    └────────────────────────────────────────────────────┘     │
*│               < OK Ctrl-W > < Отказаться Esc >                │
*└───────────────────────────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 67, "Задайте интервал"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 3 SAY "Начальная дата            (F1)  Конечная дата            (F1)"
@ 2, 18 GET d0 PICTURE "@D" WHEN Set_F1(1) VALID Clr_F1()
@ 2, 49 GET d1 PICTURE "@D" WHEN Set_F1(2) VALID Clr_F1()
@ 3,  6 GET c_c PICTURE '@^' FROM l_name SIZE 3, 55
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_F1       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Установка обработчика F1.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04/30/1999 ──┘
PROCEDURE Set_F1
PARAMETERS v

IF v = 1
  ON KEY LABEL F1 DO Int_D WITH 1
ELSE
  ON KEY LABEL F1 DO Int_D WITH 2
ENDIF
RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Clr_F1       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Очистка обработчика F1.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04/30/1999 ──┘
PROCEDURE Clr_F1

ON KEY LABEL F1

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Int_D        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Вызов календаря.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 30.04.1999 ──┘
PROCEDURE Int_D
PARAMETERS v

IF v = 1
  d0 = Get_Date(d0)
  SHOW GET d0
ELSE
  d1 = Get_Date(d1)
  SHOW GET d1
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Blank    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Проверка корректности интервала.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 30.04.1999 ──┘
PROCEDURE Tst_Blank

PRIVATE mss

IF ex # 1
  RETURN .T.
ENDIF

IF d0 > d1
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Начальная дата больше конечной? Так не бывает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура P_Doc        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Печать документа.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 30.04.1999 ──┘
PROCEDURE P_Doc
PRIVATE p_ln, fnt, n_cp, lft, n, i, t_p, f_p, t_d, f_d, p_drctry
PRIVATE sel_rem, f_name, nn, ffeed, sb

DIMENSION sb(10,2)
sb( 1,1) = "{Ст./Ст-ц}"
sb( 1,2) = ""

sb( 2,1) = "{Дата Н  }"
sb( 2,2) = DTOC(dt0)

sb( 3,1) = "{Дата К  }"
sb( 3,2) = DTOC(dt1)

sb( 4,1) = "{Поз. Ст.}"
sb( 4,2) = ""

sb( 5,1) = "{Шт. Ст. }"
sb( 5,2) = ""

sb( 6,1) = "{Уп. Ст. }"
sb( 6,2) = ""

sb( 7,1) = "{Поз. Дк.}"
sb( 7,2) = ""

sb( 8,1) = "{Шт. Дк. }"
sb( 8,2) = ""

sb( 9,1) = "{Уп. Дк. }"
sb( 9,2) = ""

sb(10,1) = "{Тип коррекции                        }"
sb(10,2) = c_name

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "WHO_CORR" = ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(PAGE_H)
IF n = 0
  DIMENSION t_p(1)
ELSE
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ENDIF

n = MEMLINES(PAGE_F)
IF n = 0
  DIMENSION f_p(1)
ELSE
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_H)
IF n = 0
  DIMENSION t_d(1)
ELSE
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ENDIF

n = MEMLINES(DOC_F)
IF n = 0
  DIMENSION f_d(1)
ELSE
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ENDIF
USE

SELECT WHO_CORR
PRIVATE n_p_det, sum_00, sum_01, sum_02, cur_p, hol_p, str_w
PRIVATE sub_1, sub_2, sub_3, sub_4, sub_5, sum_10, sum_11, sum_12, j

STORE 0 TO sum_00, sum_01, sum_02
n_p_det = p_ln-MAX(ALEN(t_p),ALEN(t_d))-MAX(ALEN(f_p),ALEN(f_d))-1
hol_p = CEILING(RECCOUNT()/n_p_det)

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

FOR cur_p = 1 TO hol_p
  sb( 1,2) = ALLTRIM(STR(cur_p))+"/"+ALLTRIM(STR(hol_p))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF cur_p # 1
    EJECT
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
  ELSE
    FOR i = 1 TO ALEN(t_d)
      str_w = t_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ENDIF
  STORE 0 TO sum_10, sum_11, sum_12
  IF RECCOUNT() # 0
    FOR i = 1 TO n_p_det
      str_w = NAME+STR(QNT_POS, 11)+STR(QNT, 11)+STR(QNT_PACK,11)
      @ PROW()+1, 0 SAY str_w
      sum_10 = sum_10+QNT_POS
      sum_11 = sum_11+QNT
      sum_12 = sum_12+QNT_PACK
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  sum_00 = sum_00+sum_10
  sum_01 = sum_01+sum_11
  sum_02 = sum_02+sum_12
  sb(4, 2) = STR(sum_10)
  sb(5, 2) = STR(sum_11)
  sb(6, 2) = STR(sum_12)
  sb(7, 2) = STR(sum_00)
  sb(8, 2) = STR(sum_01)
  sb(9, 2) = STR(sum_02)
  IF cur_p = hol_p
    FOR i = 1 TO ALEN(f_d)
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
DO Term_Prn WITH "", tmpo_path+"WHO000.DBF"

RETURN
