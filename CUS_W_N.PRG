*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                Программа печати клиентов с примечаниями.              ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Cus_W_N

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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

PRIVATE ex, pl, dep_t, p_n, p_c, p_num, point
PRIVATE sw_note, sw_cr, sw_hold &&  Объявляем и заполняем поля бланка
ex    = 1             &&
dep_t = 1
sw_note = .T.
sw_cr   = .T.
sw_hold = .T.
pl = 1
*------------------------------------------------------------------------
point = SELECT()

SELECT 0
USE (base_path+"PLACES") ALIAS P_WWWWW AGAIN
p_num = 1
DIMENSION p_n(p_num), p_c(p_num)
p_n(1) = PADR("Все площадки", FSIZE("P_NAME"))
p_c(1) = ""
SCAN
  p_num = p_num+1
  DIMENSION p_n(p_num), p_c(p_num)
  p_n(p_num) = P_NAME
  p_c(p_num) = P_CHAR
ENDSCAN

USE
SELECT (point)

*------------------------------------------------------------------------

PUSH KEY CLEAR 
DO Prp_Nav_2
DO D_Win_N WITH 13, 49, "Заполните бланк"
*
*┌─────────────────────────────────────────────┐
*│             ┌──────────────────────────┐    │
*│    Площадка │ 123456789012345678901234 │    │
*│             └──────────────────────────┘    │
*│ ┌─ Тип депозита ──────────────────────────┐ │
*│ │    ( ) Б/Нал.  ( ) Нал.  ( ) Индекс.    │ │
*│ └─────────────────────────────────────────┘ │
*│ ┌─ Флажки ────────────────────────────────┐ │
*│ │ [ ] Примечание  [ ] Кредит  [ ] Задерж. │ │
*│ └─────────────────────────────────────────┘ │
*│       < OK Ctrl-W> < Отказаться Esc >       │
*└─────────────────────────────────────────────┘
DO Sun_Bord WITH 5, 3, 7, 45, " Тип депозита "
DO Sun_Bord WITH 8, 3,10, 45, " Флажки "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  6 SAY "Площадка"
@ 2, 15 GET pl PICTURE "@^ " FROM p_n SIZE 1, 28
@ 6,  8 GET dep_t PICTURE "@*RH Б/Нал.  ;Нал.  ;Индекс."
@ 9,  5 GET sw_note PICTURE "@*C Примечание"
@ 9, 21 GET sw_cr   PICTURE "@*C Кредит"
@ 9, 33 GET sw_hold PICTURE "@*C Задерж."
@ 11, 9 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE
pl = p_c(pl)

RELEASE WINDOW (win_name)
IF ex = 2 .OR. .NOT. sw_note .AND. .NOT. sw_cr .AND. .NOT. sw_hold
  POP KEY
  RETURN
ENDIF
*-----------------------------------------------------------------------

DO Wt_Mess WITH "Подбор данных..."
IF EMPTY(pl)
  USE ACCOUNT ORDER TAG CUS_NAME
ELSE
  USE ACCOUNT ORDER TAG PL_NAME
  SEEK pl
ENDIF

SELECT 0
DELETE FILE (tmpo_path+"CUS_W_N.DBF")
CREATE DBF (tmpo_path+"CUS_W_N")  ;
         ( NAME   C(FSIZE("CUS_NAME","ACCOUNT")),  ;
           DEP    C(FSIZE("ACC_SUM","ACCOUNT")+1), ;
           CREDIT C(FSIZE("ACC_SUM","ACCOUNT")+1), ;
           HOLD   C(FSIZE("ACC_SUM","ACCOUNT")+1), ;
           NOTE   C(FSIZE("ACC_NOTE","ACCOUNT"))  )

USE (tmpo_path+"CUS_W_N") EXCLUSIVE

PRIVATE w_cr, w_hold, w_note, w_dep, t_list, sw_tmp
DIMENSION t_list(3)
t_list( 1) = "Б/Нал."
t_list( 2) = "Нал."
t_list( 3) = "Индекс."
SELECT ACCOUNT
SCAN REST WHILE SUBSYS = pl
  sw_tmp = sw_note .AND. dep_t = 1 .AND. .NOT. EMPTY(ACC_NOTE)
  sw_tmp = sw_tmp .OR. sw_note .AND. dep_t = 2 .AND. .NOT. EMPTY(ACC_NOTE0)
  sw_tmp = sw_tmp .OR. sw_note .AND. dep_t = 3 .AND. .NOT. EMPTY(ACC_NOTED)
  sw_tmp = sw_tmp .OR. sw_cr .AND. dep_t = 1 .AND. CR_SUM # 0
  sw_tmp = sw_tmp .OR. sw_cr .AND. dep_t = 2 .AND. CR0_SUM # 0
  sw_tmp = sw_tmp .OR. sw_cr .AND. dep_t = 3 .AND. CRD_SUM # 0
  sw_tmp = sw_tmp .OR. sw_hold .AND. dep_t = 1 .AND. ACC_S_H # 0
  sw_tmp = sw_tmp .OR. sw_hold .AND. dep_t = 2 .AND. ACC_S0_H # 0
  sw_tmp = sw_tmp .OR. sw_hold .AND. dep_t = 3 .AND. ACC_SD_H # 0
  IF .NOT. sw_tmp
    LOOP
  ENDIF
  
  DO CASE
  CASE dep_t = 3
    IF CRD_SUM = 0
      w_cr = ""
    ELSE
      w_cr = ALLTRIM(STR(CRD_SUM, FSIZE("CREDIT","CUS_W_N"), 4))+"$"
      w_cr = PADL(w_cr, FSIZE("CREDIT","CUS_W_N"))
    ENDIF
    IF ACC_SD_H = 0
      w_hold = ""
    ELSE
      w_hold = ALLTRIM(STR(ACC_SD_H, FSIZE("HOLD","CUS_W_N"), 4))+"$"
      w_hold = PADL(w_hold, FSIZE("HOLD","CUS_W_N"))
    ENDIF
    IF ACC_SUMD = 0
      w_dep = ""
    ELSE
      w_dep = ALLTRIM(STR(ACC_SUMD, FSIZE("DEP","CUS_W_N"), 4))+"$"
      w_dep = PADL(w_dep, FSIZE("DEP","CUS_W_N"))
    ENDIF
    w_note = ACC_NOTED
  CASE dep_t = 1
    IF CR_SUM = 0
      w_cr = ""
    ELSE
      w_cr = ALLTRIM(STR(CR_SUM, FSIZE("CREDIT","CUS_W_N"),2))
      w_cr = PADL(w_cr, FSIZE("CREDIT","CUS_W_N"))
    ENDIF
    IF ACC_S_H = 0
      w_hold = ""
    ELSE
      w_hold = ALLTRIM(STR(ACC_S_H, FSIZE("HOLD","CUS_W_N"),2))
      w_hold = PADL(w_hold, FSIZE("HOLD","CUS_W_N"))
    ENDIF
    IF ACC_SUM = 0
      w_dep = ""
    ELSE
      w_dep = ALLTRIM(STR(ACC_SUM, FSIZE("DEP","CUS_W_N"),2))
      w_dep = PADL(w_dep, FSIZE("DEP","CUS_W_N"))
    ENDIF
    w_note = ACC_NOTE
  CASE dep_t = 2
    IF CR0_SUM = 0
      w_cr = ""
    ELSE
      w_cr = ALLTRIM(STR(CR0_SUM, FSIZE("CREDIT","CUS_W_N"),2))
      w_cr = PADL(w_cr, FSIZE("CREDIT","CUS_W_N"))
    ENDIF
    IF ACC_S0_H = 0
      w_hold = ""
    ELSE
      w_hold = ALLTRIM(STR(ACC_S0_H, FSIZE("HOLD","CUS_W_N"),2))
      w_hold = PADL(w_hold, FSIZE("HOLD","CUS_W_N"))
    ENDIF
    IF ACC_SUM0 = 0
      w_dep = ""
    ELSE
      w_dep = ALLTRIM(STR(ACC_SUM0, FSIZE("DEP","CUS_W_N"),2))
      w_dep = PADL(w_dep, FSIZE("DEP","CUS_W_N"))
    ENDIF
    w_note = ACC_NOTE0
  ENDCASE

  SELECT CUS_W_N
  APPEND BLANK
  REPLACE NAME   WITH ACCOUNT.CUS_NAME,  ;
          DEP    WITH w_dep,  ;
          CREDIT WITH w_cr,   ;
          HOLD   WITH w_hold, ;
          NOTE   WITH w_note
  SELECT ACCOUNT

ENDSCAN
DO Wt_Mess
POP KEY

SELECT CUS_W_N
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не удалось найти..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  DO View_A
ENDIF
CLOSE DATABASES
DELETE FILE (tmpo_path+"CUS_W_N.DBF")

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                 Просмотр полученного списка.                          ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE View_A

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
stat_type  = 3
what_do    = "list"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) Do Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DO Use_Dummy
SELECT CUS_W_N
GO TOP

*
*   Расчет размеров окна
*
PRIVATE ln, wd, tit
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = FSIZE("NAME", "CUS_W_N")+1+ ;
     FSIZE("DEP", "CUS_W_N")+6

tit = PADC("Клиент          ",FSIZE("NAME", "CUS_W_N"))+" "+  ;
      PADC(" Депозит",FSIZE("DEP", "CUS_W_N"))+" К З П"

DO D_Wins WITH ln, wd, t_list(dep_t), 1, 1
@ WROWS()-3, 2 SAY ;
 "────── Кредит ──── Задерж. ────── Примечание ─"
*┌────── Кредит ──── Задерж. ────── Примечание ──────────────────────┐
*│    9999999.99   9999999.99   ....:....!....:....!....:....!..     │
              
what_do = "list"
@ 1, 2 SAY tit

DO WHILE .T.

  DO CASE

  CASE what_do = "list"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:H="",     ;
                  DEP:H="",      ;
                  F001 = IIF(EMPTY(CREDIT), " ", "√"):H="", ;
                  F002 = IIF(EMPTY(HOLD), " ", "√"):H="", ;
                  F003 = IIF(EMPTY(NOTE), " ", "√"):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_I(win_name+"_M") ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    what_do = "exit"
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
*║                 Кредит, задержано, примечание.                        ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Show_I
PARAMETER win

ACTIVATE WINDOW (win) SAME

@ WROWS()-2, 6 SAY IIF(EMPTY(CREDIT),PADR("Нет",FSIZE("CREDIT")), CREDIT) COLOR SCHEME 1
@ WROWS()-2,19 SAY IIF(EMPTY(HOLD),PADR("Нет",FSIZE("HOLD")), HOLD) COLOR SCHEME 1
@ WROWS()-2,32 SAY IIF(EMPTY(NOTE),PADR("Нет",FSIZE("NOTE")), NOTE) COLOR SCHEME 1
ACTIVATE WINDOW (ALIAS())

RETURN .T.
