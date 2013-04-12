*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Rep_Slr      Разработчик Андрей Васин           09.09.98 13:41:08 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Отчет по типам начислений.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Rep_Slr
PRIVATE d0, d0_, d1, t_names, t_codes, t_numb, p_hole, p_emp

p_hole = .T.
p_emp  = .T.

USE (base_path+"PARMS")
d0  = DAT_START
d0_ = DAT_START
d1  = DATE()
USE (base_path+"SLR_TYPE") ORDER TAG RANG
t_numb = 0
SCAN
  t_numb = t_numb+1
  DIMENSION t_names(t_numb), t_codes(t_numb)
  t_names(t_numb) = "√"+NAME
  t_codes(t_numb) = CODE
ENDSCAN
USE

IF .NOT. Set_P()
  RETURN
ENDIF

DO Prep_Tmp

IF Del_Empty()
  DO P_Doc
ENDIF

CLOSE DATABASES
*DELETE FILE (tmpo_path+"TMP002.DBF")


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
*│  [ ] Список типов начислений   │
*│                                │
*│< OK Ctrl-W > < Отказаться Esc >│
*└────────────────────────────────┘

PRIVATE ex, sw
ex = 1
sw = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 36, "Отчет по начислениям"
DO Sun_Bord WITH 2, 3, 4, 32, " Интервал дат "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  5 SAY "С" GET d0 PICTURE "@D"
@ 3, 18 SAY "по" GET d1 PICTURE "@D"
@ 5,  4 GET sw PICTURE "@*C Список типов начислений" VALID C_List()
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

IF d1 > DATE()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"А сегодня только "+DATE()+"..."
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

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                       Коррекция списка операций.                       ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 07.09.98 14:46:07 ═╝
PROCEDURE C_List
PRIVATE i
IF .NOT. sw
  RETURN
ENDIF

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

*  Типы начислений
*┌─────────────────┐
*│                 │
*│                 │
*│                 │
*│                 │
*│                 │
*│                 │
*│                 │
*└─────────────────┘
* Enter - отметить
*   Esc - выход

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 12, LEN(t_names(1))+4, "Типы начислений"
@ 1, 1 CLEAR TO WROWS()-1, WCOLS()-1
@ WROWS()-2, 0 SAY PADC("Enter - отметить", WCOLS())
@ WROWS()-1, 0 SAY PADC("Esc - выход", WCOLS())

*------------------------------------------------------------------------
*      Ввод полей бланка
*
i = 1
DO WHILE .T.
  i = MAX(i,1)
  i = MIN(i, t_numb)
  @ 1, 1 MENU t_names, t_numb, 7
  READ MENU TO i
  IF READKEY() % 256 = 12
    EXIT
  ENDIF
  IF LEFT(t_names(i), 1) = " "
    t_names(i) = "√"+SUBSTR(t_names(i),2)
  ELSE
    t_names(i) = " "+SUBSTR(t_names(i),2)
  ENDIF
  i = i+1
ENDDO
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
sw = .F.
SHOW GET sw
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Формирование временного файла.                     ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 09.09.98 16:11:53 ═╝
PROCEDURE Prep_Tmp
PRIVATE i, s0, s1

DO Wt_Mess WITH "Подборка данных..."

USE (base_path+"SLR_MOVE")
DELETE FILE (tmpo_path+"TMP001.DBF")
DELETE FILE (tmpo_path+"TMP001.CDX")

COPY FOR BETWEEN(DOC_DATE, d0, d1) TO (tmpo_path+"TMP001")

USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (tmpo_path+"TMP001") EXCLUSIVE
INDEX ON STR(CODE)+DTOS(DOC_DATE) TAG ORD
SET RELATION TO PS_CODE INTO PERSONS

SELECT 0
DELETE FILE (tmpo_path+"TMP002.DBF")
CREATE DBF (tmpo_path+"TMP002")  ;
  ( DOC_N  C( 7), ;
    DOC_D  D,     ;
    L_TYPE C( 1), ;
    MAN    C(30), ;
    SUM_P  N(10,2), ;
    NOTE   C(24)  )

SELECT TMP001
s0 = 0
FOR i = 1 TO t_numb
  IF LEFT(t_names(i),1) = " "
    LOOP
  ENDIF
  s1 = 0
  SEEK STR(t_codes(i))
  SELECT TMP002
  APPEND BLANK
  REPLACE MAN WITH SUBSTR(t_names(i),2), L_TYPE WITH "0"
  SELECT TMP001
  SCAN REST WHILE CODE = t_codes(i)
    SELECT TMP002
    APPEND BLANK
    REPLACE DOC_N  WITH LEFT(TMP001.DOC_NUM,1)+ALLTRIM(SUBSTR(TMP001.DOC_NUM,2)), ;
            DOC_D  WITH TMP001.DOC_DATE, ;
            L_TYPE WITH "1", ;
            MAN    WITH ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+". "+LEFT(PERSONS.S_NAME,1)+".", ;
            SUM_P  WITH Crpt_2(TMP001.TOTAL), ;
            NOTE   WITH TMP001.NOTE
    s1 = s1+SUM_P
    SELECT TMP001
  ENDSCAN
  SELECT TMP002
  APPEND BLANK
  REPLACE L_TYPE WITH "2", SUM_P WITH s1
  s0 = s0+s1
  SELECT TMP001
ENDFOR

SELECT TMP002
APPEND BLANK
REPLACE L_TYPE WITH "3", SUM_P WITH s0
SELECT PERSONS
USE
SELECT TMP001
USE
DELETE FILE (tmpo_path+"TMP001.DBF")
DELETE FILE (tmpo_path+"TMP001.CDX")
SELECT TMP002
DO Wt_Mess
RETURN

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
*│[ ] Печать пустых типов начислений│
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
@ 3, 2 GET p_emp PICTURE "@*C Печать пустых типов начислений"
@ 5, 6 GET p_hole PICTURE "@*C Печать полного отчета"
@ 7, 3 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE
PRIVATE n_sav
IF ex = 1
  SELECT TMP002
  IF .NOT. p_emp
    PRIVATE n_sav
    SET FILTER TO L_TYPE $ "02"
    SCAN
      IF L_TYPE = "0"
        n_sav = RECNO()
      ENDIF
      IF L_TYPE = "2"
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
    DELETE ALL FOR L_TYPE = "1"
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
PRIVATE cnt, s000

s000 = 0

p_ln = 60
fnt  = "1"
n_cp = 1
lft  = 0
ffeed = -1
p_drctry = 0

DO Ini_Prn WITH "Начисления", p_ln, lft, n_cp, fnt, ffeed, p_drctry

cnt = 0

SELECT TMP002
SCAN
  cnt = cnt+1
  DO CASE
  CASE L_TYPE = "0"
    @ PROW()+1, 0 SAY "----------- "+ALLTRIM(MAN)
  CASE L_TYPE = "1"
    @ PROW()+1, 0 SAY "   "+ ;
      DTOC(DOC_D)+" "+DOC_N+" "+MAN+STR(SUM_P,11,2)+" "+NOTE
  CASE L_TYPE = "2"
    @ PROW()+1, 0 SAY  ;
    PADR("Итого по типу начислений",IIF(SET("DATE") = "ON", 53, 51))+STR(SUM_P,10,2)
    s000 = s000+SUM_P
  CASE L_TYPE = "3"
    @ PROW()+1, 0 SAY  ;
    PADR("Итого по документу",IIF(SET("DATE") = "ON", 53, 51))+STR(s000,10,2)
  ENDCASE
  IF cnt >= p_ln-10
    EJECT
    cnt = 0
  ENDIF

ENDSCAN

DO Term_Prn WITH ""

RETURN
