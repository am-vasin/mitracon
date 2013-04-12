*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Rep_Flgs     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║     Отчет по породажам с использованием пользовательских флажков.      ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 08.02.2000 ══╝
PROCEDURE Rep_Flgs
PARAMETERS depParm

PRIVATE s_sav, dep_c, dep_n, dep_list, dep_codes, let_dep, n_deps, p_dep
PRIVATE mss, flg_str, d_0, d_1, n_flags

IF .NOT. File_O(base_path+"FS_FLAGS.MEM")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не описали ни одного флажка!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
RESTORE FROM (base_path+"FS_FLAGS.MEM") ADDITIVE
IF EMPTY(fs_names(1))
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не описали ни одного флажка!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
n_flags = 0
FOR n_deps = 1 TO ALEN(fs_names)
  IF EMPTY(fs_names(n_deps))
    EXIT
  ENDIF
  n_flags = n_flags+1
ENDFOR
flg_str = SPACE(ALEN(fs_names))

IF TYPE("depParm") = "N"
  dep_c = depParm
ELSE
  dep_c = 0
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"DEPS") ORDER TAG CODE ALIAS DEP_0208
IF dep_c > 0
  IF .NOT. SEEK(dep_c)
    dep_c = -1
  ELSE
    dep_n = B_NAME
  ENDIF
ENDIF

SET ORDER TO TAG B_NAME
n_deps = 0
SCAN
  n_deps = n_deps+1
  DIMENSION dep_list(n_deps), dep_codes(n_deps)
  dep_list(n_deps)  = B_NAME
  dep_codes(n_deps) = CODE
ENDSCAN

USE
IF n_deps = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не создали ни одного отдела предприятия!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (s_sav)
  RETURN
ENDIF

p_dep = 0
IF dep_c = 0      && Выбираем отдел по коду пользователя
  USE (base_path+"PERSONS") ORDER TAG CODE
  IF SEEK(user)
    dep_c = DEP_NO
  ENDIF
  USE
ENDIF
IF dep_c > 0
  p_dep = ASCAN(dep_codes, dep_c)
ENDIF
n_dep = ""
IF p_dep > 0
  let_dep = .F.
  n_dep = dep_list(p_dep)
ELSE
  p_dep = 1
  let_dep = .T.
ENDIF

STORE DATE() TO d_0, d_1

IF .NOT. G_Parms()
  RETURN
ENDIF

DO Prep_Tmp

DO View_Rep

SELECT REP_FLGS
USE
DELETE FILE (tmpo_path+"REP_FLGS.DBF")
DELETE FILE (tmpo_path+"REP_FLGS.CDX")

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Parms      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Заполнение параметров.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 08.02.2000 ──┘
PROCEDURE G_Parms
PRIVATE d0, d1
d0 = d_0
d1 = d_1
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
*│          ┌────────────────┐    │
*│    Отдел │....:....!....:.│    │
*│          └────────────────┘    │
*│ Начальная дата ДД.ММ.ГГГГ (F1) │
*│ Конечная дата  ДД.ММ.ГГГГ (F1) │
*│    ┌──────────────────────┐    │
*│    │ [ ] ....:....!....:. │    │
*│    └──────────────────────┘    │
*│< OK Ctrl-W > < Отказаться Esc >│
*└────────────────────────────────┘

PRIVATE ex, tmp_flg, i
DIMENSION tmp_flg(ALEN(fs_names))

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 11+n_flags, 36, "Заполниет бланк"
DO Sun_Bord WITH 7, 6, WROWS()-3, 29, " Флажки "
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3, 6 SAY "Отдел"
@ 5, 3 SAY "Начальная дата            (F1)"
@ 6, 3 SAY "Конечная дата             (F1)"

@ 2, 12 GET p_dep PICTURE "@^" FROM dep_list SIZE 1, 18 WHEN let_dep
@ 5, 18 GET d0 PICTURE "@D" WHEN Set_F1(1) VALID Clr_F1()
@ 6, 18 GET d1 PICTURE "@D" WHEN Set_F1(2) VALID Clr_F1()
FOR i = 1 TO n_flags
  @ 7+i, 8 GET tmp_flg(i) PICTURE "@*C "+fs_names(i)
ENDFOR

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Blank()

*--------------------------------------------------------------------------
FOR i = 1 TO n_flags
  IF tmp_flg(i)
    flg_str = STUFF(flg_str,i,1,"*")
  ENDIF
ENDFOR
d_0 = d0
d_1 = d1
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
  SHOW GETS && d0
ELSE
  d1 = Get_Date(d1)
  SHOW GETS && d1
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

PRIVATE mss, i, sw

IF ex # 1
  RETURN .T.
ENDIF

sw = .T.
FOR i = 1 TO n_flags
  IF tmp_flg(i)
    sw = .F.
    EXIT
  ENDIF
ENDFOR

IF sw
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не установили ни одного флажка!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
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
*│   Процедура Prep_Tmp     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временного файла.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 08.02.2000 ──┘
PROCEDURE Prep_Tmp

PRIVATE nr, q0, q1

DELETE FILE (tmpo_path+"REP_FLGS.DBF")
DELETE FILE (tmpo_path+"REP_FLGS.CDX")

CREATE DBF (tmpo_path+"REP_FLGS.DBF")  ;
  ( SAL_CODE N( 5), ;
    SAL_NAME C(30), ;
    QNT_INV  N( 6), ;
    QNT_POS  N( 6), ;
    QNT      N( 8), ;
    SUM_D    N(15,4), ;
    SUM_R    N(15,2)  )
INDEX ON SAL_CODE TAG SAL_CODE

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_0208 AGAIN

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS S_0208 AGAIN

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON ALIAS ST_0208 AGAIN
SET RELATION TO WHO INTO P_0208, ;
                FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO S_0208

nr = SET("NEAR") = "OFF"
SET NEAR ON
SEEK DTOS(d_0)
SCAN REST WHILE DOC_DATE <= d_1
  IF P_0208.DEP_NO # dep_c
    LOOP
  ENDIF
  IF .NOT. Flag_OK()
    LOOP
  ENDIF
  q0 = 0
  q1 = 0
  SELECT S_0208
  SCAN REST WHILE FIRM = ST_0208.FIRM .AND. ;
                  DOC_NUM = ST_0208.DOC_NUM .AND. ;
                  DOC_DATE = ST_0208.DOC_DATE
    q0 = q0+1
    q1 = q1+QNT
  ENDSCAN
  SELECT REP_FLGS
  IF .NOT. SEEK(P_0208.CODE)
    APPEND BLANK
    REPLACE SAL_CODE WITH P_0208.CODE, ;
            SAL_NAME WITH ALLTRIM(P_0208.FAMILY)+" "+ ;
                          LEFT(P_0208.NAME,1)+" "+    ;
                          LEFT(P_0208.S_NAME,1)
  ENDIF
  REPLACE QNT_INV WITH QNT_INV+1, ;
          QNT_POS WITH QNT_POS+q0,;
          QNT     WITH QNT+q1,    ;
          SUM_D   WITH SUM_D+ST_0208.TOTAL_D, ;
          SUM_R   WITH SUM_R+ST_0208.TOTAL_R
  SELECT ST_0208
ENDSCAN

USE

SELECT S_0208
USE

SELECT P_0208
USE

SELECT REP_FLGS
INDEX ON SAL_NAME TAG SAL_NAME
IF nr
  SET NEAR OFF
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Flg_OK       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Проверка флажков.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 08.02.2000 ──┘
PROCEDURE Flag_OK
PRIVATE retVal, i

retVal = .F.

FOR i = 1 TO n_flags
  IF .NOT. EMPTY(SUBSTR(flg_str, i, 1)) .AND. ;
     .NOT. EMPTY(SUBSTR(SAL_FLAGS, i, 1))
    retVal = .T.
    EXIT
  ENDIF
ENDFOR

RETURN retVal

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_Rep     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Просмотр отчета.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 09.02.2000 ──┘
PROCEDURE View_Rep


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
PRIVATE s_sav         && Номер рабочей области для сохранения и восстановления!

*
*   Заполняем значениями переменные состояния...
*
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT REP_FLGS

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Money OF (menu_name) PROMPT "Валюта" KEY Ctrl-A, "Tab"
ON SELECTION PAD Money OF (menu_name) DO Sw_Mode WITH "Money"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 71
*   Отдел XXXXXXXXXXXXXXXX
*   [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX
*   [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX
*   [ ] XXXXXXXXXXXXXXXX  [ ] XXXXXXXXXXXXXXXX
*┌─Продавец───────────────────────Накл.───Поз.───Кол-во────Сумма(У Е)──┐
*│XXXXXXXXXXXXXXXXXXXXXXXXXX X X 999999 999999 99999999 9999999999.9999│
*└─────────────────────────────────────────────────────────────────────┘
PRIVATE is_UE, tit, i, j, p
tit = "Продажи за период с "+DTOC(d_0)+" по "+DTOC(d_1)
is_UE = .T.
DO D_Wins WITH ln, wd, tit, 1+CEILING(n_flags/3), 0
p = 1
@ 1, 4 SAY "Отдел "+n_dep
FOR i = 1 TO 3
  FOR j = 1 TO 3
    IF p > n_flags
      EXIT
    ENDIF
    @ 1+i, 4+(j-1)*22 SAY IIF(EMPTY(SUBSTR(flg_str, p, 1)), "[ ] ", "[X] ")+ ;
                          fs_names(p)
    p = p+1
  ENDFOR
ENDFOR
@ CEILING(n_flags/3)+2, 2 SAY ;
"─Продавец───────────────────────Накл.───Поз.───Кол-во────Сумма(У Е)"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ CEILING(n_flags/3)+2, 64 SAY IIF(is_UE,"(У Е)","(Руб)")
    DO Prp_Nav_1
    ON KEY LABEL Tab DO Sw_Mode WITH "Money"

*
*    BROWSE - меню
*

    IF is_UE
      BROWSE FIELDS SAL_NAME:H="",      ;
                    QNT_INV:P="999999", ;
                    QNT_POS:P="999999", ;
                    QNT:P="99999999",   ;
                    SUM_D:P="9999999999.9999", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS SAL_NAME:H="",      ;
                    QNT_INV:P="999999", ;
                    QNT_POS:P="999999", ;
                    QNT:P="99999999",   ;
                    SUM_R:P="9999999999.9999", ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  CASE what_do = "Money"    && Смена валюты

    is_UE = .NOT. is_UE
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
SELECT (s_sav)

RETURN

