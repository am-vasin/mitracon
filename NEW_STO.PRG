*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла New_Sto      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                             Новые позиции.                             ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 19.01.2000 ══╝
PROCEDURE New_Sto
PRIVATE dt0
dt0 = DATE()-10
IF .NOT. G_Period(dt0)
  RETURN
ENDIF
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
DELETE FILE (tmpo_path+"NEW_STO.DBF")

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

DELETE FILE (tmpo_path+"NEW_STO.DBF")
DELETE FILE (tmpo_path+"NEW_STO.CDX")

CREATE DBF (tmpo_path+"NEW_STO.DBF") ;
  ( CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    INC_DATE D      )
INDEX ON CODE TAG CODE

SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE ALIAS INC_0119 AGAIN

SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME ALIAS STO_0119 AGAIN
SET RELATION TO CODE INTO INC_0119

SCAN
  IF .NOT. FOUND("INC_0119")
    LOOP
  ENDIF
  IF INC_0119.DOC_DATE <= dt0
    LOOP
  ENDIF
  SELECT NEW_STO
  APPEND BLANK
  REPLACE CODE     WITH STO_0119.CODE,     ;
          PREFIX   WITH STO_0119.PREFIX,   ;
          NAME     WITH STO_0119.NAME,     ;
          PRODUCER WITH STO_0119.PRODUCER, ;
          INC_DATE WITH INC_0119.DOC_DATE
  SELECT STO_0119
ENDSCAN
USE
SELECT INC_0119
USE
SELECT NEW_STO
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

*┌────────────────────────────────────────┐
*│ Укажите начальную дату ДД.ММ.ГГГГ (F1) │
*│                                        │
*│     < OK Ctrl-W > < Отказаться Esc >   │
*└────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
ON KEY LABEL F1 DO Int_D
DO Prp_Nav_2
DO D_Win_N WITH 6, 44, "Заполните бланк"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2,  3 SAY "Укажите начальную дату            (F1)"
@ 2, 26 GET d0 PICTURE "@D"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------
ON KEY LABEL F1
POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Int_D        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Вызов календаря.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 30.04.1999 ──┘
PROCEDURE Int_D
PARAMETERS v

d0 = Get_Date(d0)
SHOW GET d0
RETURN

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

DIMENSION sb(2,2)

sb( 1,1) = "{Ст./Ст-ц}"
sb( 1,2) = ""

sb( 2,1) = "{Дата Н  }"
sb( 2,2) = DTOC(dt0)

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "NEW_STO" == ALLTRIM(UPPER(DOC_NAME))
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

SELECT NEW_STO
PRIVATE n_p_det, cur_p, hol_p, str_w, j, nn

n_p_det = p_ln-MAX(ALEN(t_p),ALEN(t_d))-MAX(ALEN(f_p),ALEN(f_d))-1
hol_p = CEILING(RECCOUNT()/n_p_det)

DO Ini_Prn WITH "", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP

nn = 0
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
  IF RECCOUNT() # 0
    FOR i = 1 TO n_p_det
      nn = nn+1
      str_w = STR(nn,6)+" "+PREFIX+" "+NAME+" "+PRODUCER+" "+DTOC(INC_DATE)
      @ PROW()+1, 0 SAY str_w
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
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

DO Term_Prn WITH "", tmpo_path+"NEW_STO.DBF"

RETURN
