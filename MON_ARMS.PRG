*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                  Справка: деньги на рабочих местах.                   ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Mon_Arms

PRIVATE ln, wd, rc_sav, arm_sav, sw_flt

HIDE MENU Main_M
SELECT 0

DEFINE MENU List_A IN SCREEN COLOR SCHEME 4
DEFINE PAD List_C OF List_A PROMPT "Просмотр руб." KEY Ctrl-A, "F1"
DEFINE PAD List_D OF List_A PROMPT "Просмотр долл." KEY Ctrl-A, "F2"
DEFINE PAD Filter OF List_A PROMPT "Фильтр" KEY Ctrl-A, "F6"
DEFINE PAD Total OF List_A PROMPT "Сумма" KEY Ctrl-A, "F3"
DEFINE PAD Exit   OF List_A PROMPT "Выход" KEY Ctrl-A, "Esc"

ON SELECTION PAD List_C OF List_A  DO Ch_Mode WITH "List_A","show_mr"
ON SELECTION PAD List_D OF List_A  DO Ch_Mode WITH "List_A","show_md"
ON SELECTION PAD Filter OF List_A  DO Ch_Mode WITH "List_A","Filter"
ON SELECTION PAD Total OF List_A  DO Ch_Mode WITH "List_A","Total"
ON SELECTION PAD Exit   OF List_A  DO Ch_Mode WITH "List_A","ВСЕ!"

USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM

SELECT 0    
USE (base_path+"KASSA") ORDER TAG DOC_NUM

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG NAME
wd = FSIZE("ARM_N","ARM_LST")+FSIZE("KASSA","ARM_LST")+FSIZE("KASSA_USD","ARM_LST")+2
ln = WROWS("")-7
PRIVATE tt, ft, tt_m
tt_m = "Список раб. мест"
DIMENSION tt(1), ft(1)
tt(1) = ""
ft(1) = ""

DO Def_Win WITH "Arm_M", "Arm_E", "Arm_I", ln, wd, tt_m, tt, ft

@ 1, 2 SAY "─Раб. место───────Остаток (р)──Остаток ($)"

PRIVATE what_do

what_do = "arm_lst"

rc_sav = RECNO()
rst = .F.
sw_flt = .F.
DO WHILE .T.
  DO CASE
  CASE what_do = "arm_lst"  && Список рабочих мест
    DO Use_Dummy
    IF USED("ARM_LST")
      SELECT ARM_LST
    ELSE
      SELECT 0
      USE (base_path+"ARM_LST") ORDER TAG NAME
    ENDIF
    GO rc_sav
    IF sw_flt
      SET FILTER TO KASSA # 0 .OR. KASSA_USD # 0
    ENDIF
    ACTIVATE SCREEN
    @ 0, 0 SAY PADL("Меню F10 ",WCOLS()) COLOR SCHEME 13
    SHOW MENU List_A
*    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F10 ACTIVATE MENU List_A
    ON KEY LABEL F6  DO Ch_Mode WITH "List_A","Filter"
    ON KEY LABEL F1  DO Ch_Mode WITH "List_A","show_mr"
    ON KEY LABEL F2  DO Ch_Mode WITH "List_A","show_md"
    ON KEY LABEL F3  DO Ch_Mode WITH "List_A","Total"
    ON KEY LABEL LeftMouse DO M_Proc_1 WITH "List_A", "Arm_"
    ACTIVATE WINDOW Arm_E
    BROWSE FIELDS ARM_N:H="",   ;
                  KASSA:H="":P="@Z ", ;
                  KASSA_USD:H="":P="@Z ", ;
                  DUMMY.F:H=""  ;
           FREEZE DUMMY.F NOMODIFY NOCLEAR WINDOW Arm_I IN WINDOW Arm_E
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "arm_lst", "Filter")
    ENDIF

    HIDE MENU List_A
    arm_sav = ARM_C
    rc_sav = RECNO()
    CLOSE DATABASES
*    IF READKEY() % 256 # 12 .AND. what_do = "arm_lst"
*      what_do = "show_mr"
*    ENDIF
    IF what_do = "arm_lst"
      what_do = "ВСЕ!"
    ENDIF
  CASE what_do = "show_md"   && Доллары на рабочем месте
    DO Mon_Hist WITH arm_sav, "$"
    what_do = "arm_lst"
  CASE what_do = "show_mr"   && Рубли на рабочем месте
    DO Mon_Hist WITH arm_sav, " Р"
    what_do = "arm_lst"
  CASE what_do = "Filter"   && Фильтр
    sw_flt = .NOT. sw_flt
    what_do = "arm_lst"
  CASE what_do = "Total"    && Сумма наличности
    DO Sum_Tot
    what_do = "arm_lst"
  OTHERWISE
    EXIT
  ENDCASE
ENDDO

CLOSE DATA
RELEASE WINDOWS Arm_M, Arm_E, Arm_I
RELEASE MENU List_A

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║        Программа - обработчик мышки для списков.                      ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE M_Proc_1
PARAMETER menu_nm, wind_nm
PRIVATE mc, mr, cr

*
*  Асинхронное меню
*
IF MENU() == UPPER(menu_nm)
  RETURN
ENDIF

IF MROW("") = 0
  IF .NOT. MENU() == UPPER(menu_nm)
    ACTIVATE MENU (menu_nm)
  ENDIF
  RETURN
ENDIF

*
*  Окно BROWSE
*
mc = MCOL(wind_nm+"E")
mr = MROW(wind_nm+"E")
IF mc >= 0 .AND. mr >= 0
  KEYBOARD CHR(13)
  RETURN
ENDIF

= INKEY("M")

*
*  Основное окно
*
mc = MCOL(wind_nm+"M")
mr = MROW("")
IF mc >= 0
  IF mr = WLROW(wind_nm+"E")-1
    KEYBOARD "{PgUP}"
  ENDIF
  IF mr = WLROW(wind_nm+"E")+WROWS(wind_nm+"E")
    KEYBOARD "{PgDn}"
  ENDIF
ENDIF
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Sum_Tot      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Суммарная наличность.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04/28/2006 ──┘
PROCEDURE Sum_Tot
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
PRIVATE s_sav, s_RUR, s_USD

*
*   Заполняем значениями переменные состояния...
*
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()
m.s_sav = SELECT()
USE (m.base_path+"ARM_LST") SHARED AGAIN ALIAS ARM_6428
SUM KASSA, KASSA_USD TO m.s_RUR, m.s_USD
USE


*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌─────────────────────────┐
*│ Сумма руб. 999999999.99 │
*│                         │
*│ Сумма USD  9999999.9999 │
*│                         │
*│        < OK >           │
*└─────────────────────────┘

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 29, "Суммарная наличность"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 3 SAY "Сумма руб." GET m.s_RUR PICTURE "999999999.99"
@ 4, 3 SAY "Сумма USD " GET m.s_USD PICTURE "9999999.9999"
CLEAR GETS
@ WROWS()-2, FLOOR(WCOLS()/2-3) GET m.ex PICTURE "@*HT \ OK "

READ CYCLE

IF m.ex = 1
  *
  * Отрабатываем бланк
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
