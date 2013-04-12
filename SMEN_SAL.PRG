*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла SMen_Sal     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                  Объем продаж по продавцам за период.                  ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 01/19/2000 ══╝
PROCEDURE SMen_Sal

PRIVATE dt0, dt1, shab, s_sav
dt0 = {}
dt1 = DATE()
shab = SPACE(25)
IF .NOT. G_Period(dt0, dt1, shab)
  RETURN
ENDIF
shab = ALLTRIM(shab)
s_sav = SELECT()
SELECT 0
DO Wt_Mess WITH " Подготовка данных... "
DO Prep_Tmp
DO Wt_Mess
IF RECCOUNT() # 0
  DO P_Doc
ELSE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не найдено ни одной продажи!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF
USE
DELETE FILE (tmpo_path+"WS000.DBF")

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Tmp     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Подготовка файла.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 30.04.1999 ──┘
PROCEDURE Prep_Tmp
PRIVATE tmpVal

DELETE FILE (tmpo_path+"SMEN_SAL.DBF")
DELETE FILE (tmpo_path+"SMEN_SAL.CDX")

CREATE DBF (tmpo_path+"SMEN_SAL.DBF") ;
  ( CODE  N( 3), ;
    NAME  C(30), ;
    S_SUM N(12,4)  )
INDEX ON CODE TAG CODE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS ST_0119 AGAIN

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS DET_0119 AGAIN
SET RELATION TO CODE INTO ST_0119

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON ALIAS TIT_0119 AGAIN
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO DET_0119
SET SKIP TO DET_0119
tmpVal = SET("NEAR") = "OFF"
SET NEAR ON
SEEK DTOS(dt0)
IF tmpVal
  SET NEAR OFF
ENDIF
SCAN REST WHILE DOC_DATE <= dt1
  IF DET_0119.QNT < 0
    LOOP
  ENDIF
  IF .NOT. EMPTY(shab)
    IF .NOT. shab $ ST_0119.PREFIX+ST_0119.NAME+ST_0119.PRODUCER
      LOOP
    ENDIF
  ENDIF
  SELECT SMEN_SAL
  IF .NOT. SEEK(TIT_0119.WHO)
    APPEND BLANK
    REPLACE CODE WITH TIT_0119.WHO
  ENDIF
  REPLACE S_SUM WITH S_SUM+DET_0119.PRICE_D*DET_0119.QNT
  SELECT TIT_0119
ENDSCAN
USE
SELECT DET_0119
USE
SELECT ST_0119
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_0119 AGAIN
SELECT SMEN_SAL
SET RELATION TO CODE INTO P_0119
REPLACE ALL NAME WITH ALLTRIM(P_0119.FAMILY)+" "+LEFT(P_0119.NAME,1)+" "+LEFT(P_0119.S_NAME,1)
SET RELATION TO
SELECT P_0119
USE
SELECT SMEN_SAL
INDEX ON NAME TAG NAME
COPY TO (tmpo_path+"WS000")
USE (tmpo_path+"WS000") ALIAS SMEN_SAL
DELETE FILE (tmpo_path+"SMEN_SAL.DBF")
DELETE FILE (tmpo_path+"SMEN_SAL.CDX")
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
*│                                                               │
*│      Позиции, входящие в отчет ....:....!....:....!....:      │
*│                                                               │
*│               < OK Ctrl-W > < Отказаться Esc >                │
*└───────────────────────────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 67, "Заполните бланк"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 3 SAY "Начальная дата            (F1)  Конечная дата            (F1)"
@ 2, 18 GET d0 PICTURE "@D" WHEN Set_F1(1) VALID Clr_F1()
@ 2, 49 GET d1 PICTURE "@D" WHEN Set_F1(2) VALID Clr_F1()
@ 4,  8 SAY "Позиции, входящие в отчет" GET c_c
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

DIMENSION sb(6,2)

sb( 1,1) = "{Ст./Ст-ц}"
sb( 1,2) = ""

sb( 2,1) = "{Дата Н  }"
sb( 2,2) = DTOC(dt0)

sb( 3,1) = "{Дата К  }"
sb( 3,2) = DTOC(dt1)

sb( 4,1) = "{Шаблон                 }"
sb( 4,2) = shab

sb( 5,1) = "{Итого Стр   }"
sb( 5,2) = ""

sb( 6,1) = "{Итого Док   }"
sb( 6,2) = ""

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "SMEN_SAL" == ALLTRIM(UPPER(DOC_NAME))
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

SELECT SMEN_SAL
PRIVATE n_p_det, sum_00, sum_01, cur_p, hol_p, str_w, j

STORE 0 TO sum_00, sum_01
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
  STORE 0 TO sum_00
  IF RECCOUNT() # 0
    FOR i = 1 TO n_p_det
      str_w = NAME+STR(S_SUM, 13, 4)
      @ PROW()+1, 0 SAY str_w
      sum_00 = sum_00+S_SUM
      sum_01 = sum_01+S_SUM
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  sb(5, 2) = STR(sum_00, 14, 4)
  sb(6, 2) = STR(sum_01, 14, 4)
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

DO Term_Prn WITH "", tmpo_path+"WS000.DBF"

RETURN
