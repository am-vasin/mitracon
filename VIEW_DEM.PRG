*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл View_Dem     Разработчик Андрей Васин           20.07.97 12:13:10 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Таблица спроса по продавцам.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE View_Dem

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

*           Укажите период
*┌─────────────────────────────────────────────────┐
*│   Нач. дата ДД.ММ.ГГГГ    Кон. дата ДД.ММ.ГГГГ  │
*│                                                 │
*│        < OK Ctrl-W > < Отказаться Esc >         │
*└─────────────────────────────────────────────────┘

PRIVATE ex, d1, d2   &&
                     &&   Объявляем и заполняем поля бланка
ex = 1             &&
d1 = DATE()
d2 = DATE()
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 53, "Укажите период"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2,  5 SAY "Нач. дата" GET d1 PICTURE "@D"
@ 2, 29 SAY "Кон. дата" GET d2 PICTURE "@D"
@ 4, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Dat()

IF ex = 1
  DO Prep_Doc
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                  Проверка корректности интервала дат.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 20.07.97 12:19:37 ═╝
PROCEDURE Tst_Dat
PRIVATE mss

IF ex = 2 .OR. MOD(READKEY(),256) = 12
  RETURN .T.
ENDIF

IF d1 > DATE() .OR. d2 > DATE()
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Сегодня еще только "+TRANSFORM(DATE(),"@D")
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF d1 > d2
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Начальная дата больше конечной? Так не бывает!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                   Программа формирования документа.                    ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 20.07.97 12:23:58 ═╝
PROCEDURE Prep_Doc
PRIVATE n, mss, us_sav, del_sav, q_h, q_a, q_f

DELETE FILE (tmpo_path+"VIEW_DEM.DBF")
DELETE FILE (tmpo_path+"VIEW_DEM.CDX")

*
*  Создаем и заполняем временный файл - заготовку.
*
CREATE DBF (tmpo_path+"VIEW_DEM")  ;
   (  USER_C N( 4),  ;
      USER_N C(24),  ;
      Q_HOLE N( 5),  ;
      Q_MOD  N(15),  ;
      Q_FUNC N( 5)  )
USE (tmpo_path+"VIEW_DEM") EXCLUSIVE
del_sav = SET("DELETED") = "OFF"
SET DELETED ON
SELECT 0
USE (base_path+"DEMAND") ORDER TAG WHO
SET FILTER TO BETWEEN(DATE, d1, d2)
STORE 0 TO q_h, q_a, q_f
DO Wt_Mess WITH "Просматриваем спрос..."
us_sav = 10000000
SCAN
  IF us_sav # WHO
    IF us_sav # 10000000
      SELECT VIEW_DEM
      APPEND BLANK
      REPLACE USER_C WITH us_sav, ;
              Q_HOLE WITH q_h,    ;
              Q_MOD  WITH q_a,    ;
              Q_FUNC WITH q_f
      STORE 0 TO q_h, q_a, q_f
      SELECT DEMAND
    ENDIF
    us_sav = WHO
  ENDIF
  q_h = q_h+1
  IF .NOT. EMPTY(FUNCTION)
    q_f = q_f+1
  ENDIF
  IF .NOT. EMPTY(AMODEL)
    q_a = q_a+1
  ENDIF
ENDSCAN
IF us_sav # 10000000
  SELECT VIEW_DEM
  APPEND BLANK
  REPLACE USER_C WITH us_sav, ;
          Q_HOLE WITH q_h,    ;
          Q_MOD  WITH q_a,    ;
          Q_FUNC WITH q_f
  STORE 0 TO q_h, q_a, q_f
  SELECT DEMAND
ENDIF

IF del_sav
  SET DELETED OFF
ENDIF

SELECT DEMAND
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT VIEW_DEM
SET RELATION TO USER_C INTO PERSONS
REPLACE ALL FOR FOUND("PERSONS") USER_N WITH PERSONS.FAMILY

DO Wt_Mess

IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"За указанный период спрос не собирался!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  DELETE FILE (tmpo_path+"VIEW_DEM.DBF")
  DELETE FILE (tmpo_path+"VIEW_DEM.CDX")
  RETURN
ENDIF

INDEX ON USER_N TAG USER_N

SELECT 0

PRIVATE sb
DIMENSION sb(5,2)

sb( 1,1) = "{Date_1}"
sb( 1,2) = TRANSFORM(d1, "@D")

sb( 2,1) = "{Date_2}"
sb( 2,2) = TRANSFORM(d2, "@D")

sb( 3,1) = "{Q_H }"
sb( 3,2) = ""

sb( 4,1) = "{Q_A }"
sb( 4,2) = ""

sb( 5,1) = "{Q_F }"
sb( 5,2) = ""

n = ALEN(sb,1)

PRIVATE doc_tit, p_ln, fnt, n_cp, lft, ffeed, t_d, f_d, i, j, str_w, p_drctry
PRIVATE sqh, sqf, sqa

SELECT 0
USE DOC_FORM
doc_tit = "Таблица спроса"

LOCATE FOR "VIEW_DEM" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(DOC_H)
DIMENSION t_d(n)
FOR i = 1 TO n
  t_d(i) = MLINE(DOC_H,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR
USE

DO Ini_Prn WITH doc_tit, p_ln, lft, n_cp, fnt, ffeed, p_drctry

STORE 0 TO sqh, sqa, sqf

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

SELECT VIEW_DEM
SCAN
  str_w = USER_N+TRANSFORM(Q_HOLE,"@Z 9999999")+ ;
                 TRANSFORM(Q_MOD, "@Z 9999999")+ ;
                 TRANSFORM(Q_FUNC,"@Z 9999999")
  @ PROW()+1,0 SAY str_w
  sqh = sqh+Q_HOLE
  sqf = sqf+Q_MOD
  sqa = sqa+Q_FUNC
ENDSCAN
sb(3,2) = STR(sqh,6)
sb(4,2) = STR(sqa,6)
sb(5,2) = STR(sqf,6)
FOR i = 1 TO ALEN(f_d)
  str_w = f_d(i)
  FOR j = 1 TO ALEN(sb,1)
    str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
  ENDFOR
  @ PROW()+1, 0 SAY str_w
ENDFOR

SELECT VIEW_DEM
USE

DO Term_Prn WITH "", tmpo_path+"VIEW_DEM.DBF"
CLOSE DATABASES

DELETE FILE (tmpo_path+"VIEW_DEM.DBF")
DELETE FILE (tmpo_path+"VIEW_DEM.CDX")

RETURN
