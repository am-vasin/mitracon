*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл K_Rep_H      Разработчик Андрей Васин           07.09.98 16:59:19 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                  Полный отчет по кассовым операциям.                   ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE K_Rep_H

PRIVATE d0, d1, kurs_w, d0_, d1_

USE (base_path+"PARMS")
d0 = DAT_START
USE
d1 = DATE()
kurs_w = kurs_mmvb
d0_ = d0
d1_ = d1

IF .NOT. Set_P()
  RETURN
ENDIF

DO Wt_Mess WITH "Подборка данных..."
DO Prep_Tmp
DO Wt_Mess

PRIVATE p_hole, p_emp
p_hole = .T.
p_emp  = .T.
IF Del_Empty()
  DO P_Doc
ENDIF

CLOSE DATABASES
DELETE FILE (tmpo_path+"K_H_TMP2.DBF")
DELETE FILE (tmpo_path+"K_H_TMP2.CDX")
DELETE FILE (tmpo_path+"K_TMP.DBF")
DELETE FILE (tmpo_path+"K_TMP.CDX")

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Параметры отчета.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.09.98 17:02:17 ═╝
PROCEDURE Set_P

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
*│ ┌─ Интервал дат ─────────────┐ │
*│ │ С ДД.ММ.ГГГГ по ДД.ММ.ГГГГ │ │
*│ └────────────────────────────┘ │
*│    Расчет по курсу 999.999     │
*│                                │
*│< OK Ctrl-W > < Отказаться Esc >│
*└────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 36, "Полный отчет по кассе"
DO Sun_Bord WITH 2, 3, 4, 32, " Интервал дат "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  5 SAY "С" GET d0 PICTURE "@D"
@ 3, 18 SAY "по" GET d1 PICTURE "@D"
@ 5,  6 SAY "Расчет по курсу" GET kurs_w PICTURE "999.999"
@ 7,  2 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                    Корректность заполнения бланка.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.09.98 17:10:04 ═╝
PROCEDURE Tst_Blank

IF ex = 2
  RETURN .T.
ENDIF

IF d0 < d0_
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Начальная дата не может быть меньше "+DTOC(d0_)+"."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF d1 > d1_
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"А сегодня только "+DTOC(d1_)+"..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF d1 < d0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Начальная дата должна быть меньше конечной..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF kurs_w <= 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Курс доллара должен быть больше нуля!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                            Подборка данных.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.09.98 17:16:08 ═╝
PROCEDURE Prep_Tmp
PRIVATE n, n_c, str_w, pr

DELETE FILE (tmpo_path+"K_H_TMP2.DBF")
DELETE FILE (tmpo_path+"K_H_TMP2.CDX")
DELETE FILE (tmpo_path+"K_TMP.DBF")
DELETE FILE (tmpo_path+"K_TMP.CDX")

SELECT 0
CREATE DBF (tmpo_path+"K_H_TMP2")  ;
  ( G_CODE  N( 3), ;
    O_CODE  N( 3), ;
    L_TYPE  C( 1), ;
    DOC_N   C( 7), ;
    DOC_D   D,     ;
    SUM_P   N(12,4), ;
    SUM_N   N(12,4), ;
    NOTE    C(32)  )

SELECT 0
USE (base_path+"KASSA_M") ORDER TAG HRON
SEEK DTOS(d0)
IF .NOT. FOUND()
  n = RECNO(0)
  IF BETWEEN(n, 1, RECCOUNT())
    GO n
  ENDIF
ENDIF
COPY REST WHILE DOC_DATE <= d1 TO (tmpo_path+"K_TMP")
USE (tmpo_path+"K_TMP") EXCLUSIVE
INDEX ON STR(CODE,3)+DTOS(DOC_DATE) TAG K_TMP

SELECT 0
USE (base_path+"MM_TYPES") ORDER TAG CODE

SELECT 0
USE (base_path+"MM_GROUP")

PRIVATE sum_p0, sum_n0, sum_p1, sum_n1
SCAN     && По группам
  sum_p0 = 0
  sum_n0 = 0
  str_w = OPER_LST
  SELECT K_H_TMP2
  APPEND BLANK
  REPLACE G_CODE  WITH RECNO("MM_GROUP"), ;
          O_CODE  WITH 0, ;
          L_TYPE  WITH "0"

  DO WHILE .NOT. EMPTY(str_w)   && По типам операций в группе
    sum_p1 = 0
    sum_n1 = 0
    n_c = LEFT(str_w,3)
    n   = VAL(ALLTRIM(n_c))
    str_w = SUBSTR(str_w, 5)
    SELECT K_H_TMP2
    APPEND BLANK
    REPLACE G_CODE  WITH RECNO("MM_GROUP"), ;
            O_CODE  WITH n, ;
            L_TYPE  WITH "1"
    SELECT K_TMP
    SEEK n_c
    SCAN REST WHILE CODE = n   && По операциям соотв. типа
      SELECT K_H_TMP2
      APPEND BLANK
      pr = K_TMP.SUM_D+K_TMP.SUM_R/kurs_w
      REPLACE G_CODE  WITH RECNO("MM_GROUP"), ;
              O_CODE  WITH n, ;
              L_TYPE  WITH "2", ;
              DOC_N   WITH LEFT(K_TMP.DOC_NUM,1)+ALLTRIM(RIGHT(K_TMP.DOC_NUM,6)),  ;
              DOC_D   WITH K_TMP.DOC_DATE, ;
              SUM_P   WITH IIF(pr > 0, pr, 0), ;
              SUM_N   WITH IIF(pr < 0, -pr, 0),;
              NOTE    WITH K_TMP.NOTE
      sum_p1 = sum_p1+SUM_P
      sum_n1 = sum_n1+SUM_N
      SELECT K_TMP
    ENDSCAN
    SELECT K_H_TMP2
    APPEND BLANK
    REPLACE G_CODE  WITH RECNO("MM_GROUP"), ;
            O_CODE  WITH n, ;
            L_TYPE  WITH "3", ;
            SUM_P   WITH sum_p1, ;
            SUM_N   WITH sum_n1
    sum_p0 = sum_p0+sum_p1
    sum_n0 = sum_n0+sum_n1
  ENDDO
  SELECT K_H_TMP2
  APPEND BLANK
  REPLACE G_CODE  WITH RECNO("MM_GROUP"), ;
          O_CODE  WITH n, ;
          L_TYPE  WITH "4", ;
          SUM_P   WITH sum_p0, ;
          SUM_N   WITH sum_n0
ENDSCAN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                   Установка режима печати документа.                   ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 08.09.98 12:20:07 ═╝
PROCEDURE Del_Empty

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

*┌──────────────────────────────────┐
*│                                  │
*│    [ ] Печать пустых операций    │
*│                                  │
*│    [ ] Печать полного отчета     │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 38, "Вид отчета"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 6 GET p_emp PICTURE "@*C Печать пустых операций"
@ 5, 6 GET p_hole PICTURE "@*C Печать полного отчета"
@ 7, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE
PRIVATE n_sav
IF ex = 1
  SELECT K_H_TMP2
  IF .NOT. p_emp
    PRIVATE n_sav
    SET FILTER TO L_TYPE $ "13"
    SCAN
      IF L_TYPE = "1"
        n_sav = RECNO()
      ENDIF
      IF L_TYPE = "3"
        IF n_sav = RECNO()-1
          GO n_sav
          DELETE
          SKIP
          DELETE
        ENDIF
      ENDIF
    ENDSCAN
    SET FILTER TO
  ENDIF
  IF .NOT. p_hole
    DELETE ALL FOR L_TYPE = "2"
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Собственно печать документа.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 08.09.98 12:40:13 ═╝
PROCEDURE P_Doc
PRIVATE p_ln, fnt, n_cp, lft, ffeed, p_drctry
PRIVATE cnt

p_ln = 60
fnt  = " "
n_cp = 1
lft  = 0
ffeed = -1
p_drctry = 0

DO Ini_Prn WITH "Касса", p_ln, lft, n_cp, fnt, ffeed, p_drctry

cnt = 0

SELECT K_H_TMP2
SCAN
  cnt = cnt+1
  DO CASE
  CASE L_TYPE = "0"        && Имя группы операций
    GO K_H_TMP2.G_CODE IN MM_GROUP
    @ PROW()+1, 0 SAY PADC(" "+ALLTRIM(MM_GROUP.NAME)+" ", 60, "=")
  CASE L_TYPE = "1"        && Имя операции
    = SEEK(O_CODE, "MM_TYPES")
    @ PROW()+1, 0 SAY "----------- "+ALLTRIM(MM_TYPES.NAME)
  CASE L_TYPE = "2"        && Операция
    @ PROW()+1, 0 SAY "   "+ ;
      DTOC(DOC_D)+" "+DOC_N+STR(SUM_P,13,2)+STR(SUM_N,13,2)+" "+NOTE
  CASE L_TYPE = "3"        && Итого по операции
    @ PROW()+1, 0 SAY  ;
    PADR("Итого по операции",IIF(SET("DATE") = "ON", 21, 19))+ ;
      +STR(SUM_P,13,2)+STR(SUM_N,13,2)+STR(SUM_P-SUM_N,13,2)
  CASE L_TYPE = "4"        && Итого по группе операций
    @ PROW()+1, 0 SAY  ;
    PADR("Итого по группе",IIF(SET("DATE") = "ON", 21, 19))+ ;
      +STR(SUM_P,13,2)+STR(SUM_N,13,2)+STR(SUM_P-SUM_N,13,2)
  ENDCASE
  IF cnt >= p_ln-10
    EJECT
    cnt = 0
  ENDIF

ENDSCAN

DO Term_Prn WITH ""

RETURN
