*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Sal_P_M      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║             Объем продаж по сотрудникам отдела за период.              ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 07/05/1999 ══╝
PROCEDURE Sal_P_M
PARAMETER own_dep
PRIVATE dep_c, d_0, d_1

dep_c = 0
IF own_dep
  USE (base_path+"PERSONS") SHARED AGAIN ALIAS TMP_2110 ORDER TAG CODE
  SEEK user
  dep_c = DEP_NO
  USE
ENDIF
d_0 = {}
d_1 = DATE()
IF .NOT. Fill_P()
  RETURN
ENDIF

DO Wt_Mess WITH "Подборка данных"
DO Prep_Tmp
DO Wt_Mess

DO View_D

USE
DELETE FILE (tmpo_path+"SAL_P_M.DBF")
DELETE FILE (tmpo_path+"SAL_P_M.CDX")

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Fill_P       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Выбор параметров.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.07.1999 ──┘
PROCEDURE Fill_P
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
*│              ┌────────────────┐        │
*│        Отдел │....:....!....:.│        │
*│              └────────────────┘        │
*│ ┌─ Интервал дат ─────────────────────┐ │
*│ │ С ДД.ММ.ГГГГ(F1) по ДД.ММ.ГГГГ(F1) │ │
*│ └────────────────────────────────────┘ │
*│    < OK Ctrl-W > < Отказаться Esc >    │
*└────────────────────────────────────────┘

PRIVATE d_names, d_codes, d_qnt, s_sav, let_dep, tmpDep
let_dep = dep_c = 0
tmpDep = dep_c
s_sav = SELECT()
SELECT 0
USE (base_path+"DEPS") ORDER TAG B_NAME ALIAS DP_9705 AGAIN
d_qnt = 0
SCAN
  d_qnt = d_qnt+1
  DIMENSION d_names(d_qnt), d_codes(d_qnt)
  d_names(d_qnt) = B_NAME
  d_codes(d_qnt) = CODE
  IF CODE = tmpDep
    dep_c = d_qnt
  ENDIF
ENDSCAN
IF let_dep
  dep_c = 1
ENDIF
USE
SELECT (s_sav)
IF d_qnt = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одного раздела!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 44, "Заполните бланк"
@ 3, 10 SAY "Отдел"
DO Sun_Bord WITH  5,  3,  7, 41, " Интервал дат "
@ 6, 5 SAY "С           (F1) по           (F1)"
*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 16 GET dep_c PICTURE "@^" FROM d_names SIZE 3,18 WHEN let_dep
@ 6,  7 GET d_0 WHEN Set_F1() VALID Clr_F1()
@ 6, 25 GET d_1 WHEN Set_F1() VALID Clr_F1()

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE
dep_c = d_codes(dep_c)
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
*└────────────────────────────────────────────────────────── 05.07.1999 ──┘
PROCEDURE Set_F1

ON KEY LABEL F1 DO Int_Date

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Clr_F1       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Очистка обработчика F1.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.07.1999 ──┘
PROCEDURE Clr_F1

ON KEY LABEL F1

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Int_Date     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Установка даты по календарю.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.07.1999 ──┘
PROCEDURE Int_Date

IF "D_0" $ OBJVAR()
  d_0 = Get_Date(d_0)
  SHOW GET d_0
ELSE
  d_1 = Get_Date(d_1)
  SHOW GET d_1
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Tmp     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временного файла.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.07.1999 ──┘
PROCEDURE Prep_Tmp

PRIVATE m_list, s_sav, n, q0, q1, s
s_sav = SELECT()
SELECT 0
USE (base_path+"PERSONS") ALIAS P_9705 AGAIN
m_list = ""
SCAN FOR DEP_NO = dep_c
  m_list = m_list+STR(CODE,5)+"!"
ENDSCAN
SET ORDER TO TAG CODE
SELECT 0
DELETE FILE (tmpo_path+"SAL_P_M.DBF")
DELETE FILE (tmpo_path+"SAL_P_M.CDX")
CREATE DBF (tmpo_path+"SAL_P_M") ;
 ( CODE  N( 5),   ;
   NAME  C(28),   ;
   POS   N( 5),   ;
   QNT   N( 8),   ;
   SUM_D N(15,4), ;
   PRC   N( 7,3) )
INDEX ON CODE TAG CODE
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM AGAIN ALIAS D_9705
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON AGAIN ALIAS T_9705
IF .NOT. SEEK(DTOS(d_0))
  n = RECNO(0)
  IF BETWEEN(n, 1, RECCOUNT())
    GO n
  ENDIF
ENDIF
s = 0
SCAN REST WHILE DOC_DATE <= d_1 FOR STR(WHO,5) $ m_list .AND. T_9705.TOTAL_D > 0
  q0 = 0
  q1 = 0
  SELECT D_9705
  SEEK T_9705.FIRM+LEFT(DTOS(T_9705.DOC_DATE),4)+T_9705.DOC_NUM
  SCAN REST WHILE FIRM=T_9705.FIRM .AND. DOC_DATE=T_9705.DOC_DATE .AND. DOC_NUM=T_9705.DOC_NUM
    q0 = q0+1
    q1 = q1+QNT
  ENDSCAN
  SELECT SAL_P_M
  IF .NOT. SEEK(T_9705.WHO)
    APPEND BLANK
    REPLACE CODE WITH T_9705.WHO
  ENDIF
  REPLACE POS   WITH POS+q0, ;
          QNT   WITH QNT+q1, ;
          SUM_D WITH SUM_D+T_9705.TOTAL_D
  SELECT T_9705
  s = s+T_9705.TOTAL_D
ENDSCAN
USE
SELECT D_9705
USE
SELECT SAL_P_M
SET RELATION TO CODE INTO P_9705

SCAN
  REPLACE NAME WITH P_9705.FAMILY-(" "+LEFT(P_9705.NAME,1))-(" "+LEFT(P_9705.S_NAME,1)), ;
          PRC  WITH SUM_D/s*100
ENDSCAN
SET RELATION TO
SELECT P_9705
USE
SELECT SAL_P_M
INDEX ON NAME TAG NAME
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_D       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Просмотр результатов.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 05.07.1999 ──┘
PROCEDURE View_D

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
SELECT Sal_P_M

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"
*
*   Расчет размеров окна
*
*┌Продавец──────────────────────Поз.────Штук──% Суммы──Сумма прод.┐
*│....:....!....:....!....:... NNNNN NNNNNNNN NNNNNNN NNNNNNN.NNNN│
*└────────────────────────────────────────────────────────────────┘
PRIVATE ln, wd
ln = MIN(RECCOUNT(), WROWS("")-11)   && Количество видимых строк BROWSE
ln = MAX(1, ln)
wd = 64

DO D_Wins WITH ln, wd, "Объем продаж по продавцам", 0, 0
@ 1, 2 SAY "Продавец──────────────────────Поз.────Штук──% Суммы──Сумма прод."
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:28:H="",   ;
                  POS:5:H="",     ;
                  QNT:8:H="",     ;
                  PRC:H="":P="999.999", ;
                  SUM_D:H="":P="9999999.9999",;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

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
