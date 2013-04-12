*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Conv_Dep     Разработчик Андрей Васин           23.09.98 12:27:58 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║              Конвертация (индексация) денег на депозите.               ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Conv_Dep
PARAMETERS c_code, mute

*
*  Переменные состояния для навигации
*
PRIVATE is_c, tmpNalog

tmpNalog = 0
is_c = .F.

PRIVATE max_ord
*max_ord = 0
*IF File_O(base_path+"MAX_ORD.MEM")
*  RESTORE FROM (base_path+"MAX_ORD.MEM") ADDITIVE
*ENDIF

PRIVATE what_do       && Имя режима.
PRIVATE dep_list, n_f
DIMENSION dep_list(1, 6)
*
* Подготовка списка фирм
SELECT 0
USE (base_path+"FIRMS") ORDER TAG NAME ALIAS F_9211 AGAIN
n_f = 0
SCAN
  n_f = n_f+1
  DIMENSION dep_list(n_f, 6)
  dep_list(n_f, 1) = FIRM_CODE
  dep_list(n_f, 2) = B_NAME
  dep_list(n_f, 3) = 0
  dep_list(n_f, 4) = 0
  dep_list(n_f, 5) = 0
  dep_list(n_f, 6) = 0
ENDSCAN
USE
IF n_f = 0
  PRIVATE mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не создали ни одного предприятия!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DO Use_Link
DO Use_Dummy
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME AGAIN ALIAS ACC_1215
what_do = "List"
IF TYPE("c_code") = "N"
  SET ORDER TO TAG CUS_CODE
  what_do = "Conv"
ENDIF

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка клиентов

    SELECT 0
    USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME AGAIN
    GO RECNO("ACC_1215")
    
    what_do = IIF(Custs(""), "Work", "Exit")
    SELECT ACC_1215
    IF BETWEEN(RECNO("ACCOUNT"), 1, RECCOUNT())
      GO RECNO("ACCOUNT")
    ENDIF
    SELECT ACCOUNT
    USE
    SELECT ACC_1215
    

  CASE what_do = "Work"    && Конвертация

    DO Prep_F
    DO Conv0
    SELECT ACC_1215
    what_do = "List"

  CASE what_do = "Conv"    && Конвертация

    SEEK c_code
    IF Prep_F()
      DO Conv0 WITH "Fill"
    ENDIF
    SELECT ACC_1215
    what_do = "Exit"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

USE

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_F       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Подготовка списка депозитов для клиента.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.02.1999 ──┘
PROCEDURE Prep_F

PRIVATE s_sav, i, usd_dep
s_sav = SELECT()

IF TYPE("c_code") = "N"
  is_c = BUHG_REQ.DEP_TYPE = 4
ELSE
  IF TYPE("c_code") = "C"
    is_c = "CHECK" $ c_code
  ENDIF
ENDIF

usd_dep = 0
SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS CR_9211 AGAIN
FOR i = 1 TO n_f
  IF SEEK(STR(ACC_1215.CUS_CODE)+dep_list(i, 1))
    dep_list(i, 3) = ACC_SUM
    dep_list(i, 4) = CR_SUM
    dep_list(i, 5) = ACC_SUMD
    dep_list(i, 6) = CRD_SUM
  ELSE
    dep_list(i, 3) = 0
    dep_list(i, 4) = 0
    dep_list(i, 5) = 0
    dep_list(i, 6) = 0
  ENDIF
  IF TYPE("c_code") = "N"
    IF BUHG_REQ.FIRM = dep_list(i,1)
      usd_dep = dep_list(i, 5)
    ENDIF
  ENDIF
ENDFOR
IF TYPE("c_code") = "N"
  PRIVATE   r_fr_d,  d_to_rn, d_to_rt, d_to_rk, d_to_r
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(BUHG_REQ.FIRM))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(BUHG_REQ.FIRM))
  r_fr_d  = BUHG_REQ.IND_INC    && Итого на рублевый депозит (руб)
  d_to_rn = BUHG_REQ.IND_INC*tmpNalog/100 && Плюс налог
  d_to_rt = r_fr_d+d_to_rn   && Итого снять с индексируемого депозита (руб)
  d_to_rk = BUHG_REQ.IND_INC/BUHG_REQ.IND_EXP  && Пересчитать по курсу...
  d_to_r  = d_to_rt/d_to_rk   && Снять с индексируемого депозита (USD)
  IF usd_dep < d_to_r .AND. .NOT. mute
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Денег на депозите не хватает!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ENDIF
ENDIF
USE
SELECT (s_sav)
RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Conv0        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Заполнение бланка для конвертации.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.02.1999 ──┘
PROCEDURE Conv0
PARAMETER mode
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

*┌───────────────────────────────────────────────────────────────────┐
*│ ┌─ Клиент ──────────────────────────────────────────────────────┐ │
*│ │    Имя ....:....!....:....!....:....!....:....!....:....!     │ │
*│ │         Примечание 1 ....:....!....:....!....:....!..         │ │
*│ │         Примечание 2 ....:....!....:....!....:....!..         │ │
*│ │                       Флажки 1 2 3 4 5                        │ │
*│ └───────────────────────────────────────────────────────────────┘ │
*│ ┌─ [ ] Предприятие ... ....:....!....:. ────────────────────────┐ │
*│ │       Депозит  999 999 999.99 руб    9 999 999.9999 USD       │ │
*│ └───────────────────────────────────────────────────────────────┘ │
*│ ┌─ Рубли в У Е ────────────────┐ ┌─ У Е в рубли ────────────────┐ │
*│ │ С депозита   999 999 999.99р │ │ С депозита   9 999 999.9999$ │ │
*│ │ Налог 999.9% 999 999 999.99р │ │ По курсу            999.999  │ │
*│ │ Итого        999 999 999.99р │ │ Итого        999 999 999.99р │ │
*│ │ По курсу            999.999  │ │ Налог 999.9% 999 999 999.99р │ │
*│ │ На депозит   9 999 999.9999$ │ │ На депозит   9 999 999.9999$ │ │
*│ └──────────────────────────────┘ └──────────────────────────────┘ │
*│                  ( ) Приходный ордер    ( ) Чек                   │
*│                 Примечание ....:....!....:....!..                 │
*│                 < OK Ctrl-W > < Отказаться Esc >                  │
*└───────────────────────────────────────────────────────────────────┘
PRIVATE ex, dep_sw, v_sav
v_sav = 0
dep_sw = .F.
PRIVATE p_dep       && Указатель на предприятие (массив dep_list)
PRIVATE dep_r       && Рублевый депозит (остаток)
PRIVATE dep_d       && Индексируемый депозит (остаток)
PRIVATE r_to_d      && Снять с рублевого депозита
PRIVATE r_to_dn     && Плюс налог
PRIVATE r_to_dt     && Итого перевести на индексируемый депозит (руб)
PRIVATE r_to_dk     && Пересчитать по курсу...
PRIVATE d_fr_r      && И перевести на индексируемый депозит (USD)

PRIVATE d_to_r      && Снять с индексируемого депозита (USD)
PRIVATE d_to_rk     && Пересчитать по курсу...
PRIVATE d_to_rt     && Итого снять с индексируемого депозита (руб)
PRIVATE d_to_rn     && Минус налог
PRIVATE r_fr_d      && Итого на рублевый депозит (руб)
PRIVATE nt          && Примечание
PRIVATE tmpCheck    && 
tmpCheck = IIF(is_c, 2, 1)
STORE 0 TO p_dep, dep_r, dep_d
STORE 0 TO r_to_d, r_to_dn, r_to_dt, d_fr_r
STORE 0 TO d_to_r, d_to_rt, d_to_rn, r_fr_d
r_to_dk = kurs
d_to_rk = kurs
*
*   Предприятие
*
p_dep = 1
FOR ex = 1 TO ALEN(dep_list,1)
  IF def_firm = dep_list(ex,1)
    p_dep = ex
    EXIT
  ENDIF
ENDFOR
IF TYPE("c_code") = "N"
  p_dep = 1
  FOR ex = 1 TO ALEN(dep_list,1)
    IF BUHG_REQ.FIRM = dep_list(ex,1)
      p_dep = ex
      EXIT
    ENDIF
  ENDFOR
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(dep_list(p_dep,1)))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(dep_list(p_dep,1)))
  r_fr_d  = BUHG_REQ.IND_INC    && Итого на рублевый депозит (руб)
  d_to_rn = BUHG_REQ.IND_INC*tmpNalog/100 && Плюс налог
  d_to_rt = r_fr_d+d_to_rn   && Итого снять с индексируемого депозита (руб)
  d_to_rk = BUHG_REQ.IND_INC/BUHG_REQ.IND_EXP  && Пересчитать по курсу...
  d_to_r  = d_to_rt/d_to_rk   && Снять с индексируемого депозита (USD)
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
IF .NOT. mute
  DO Prp_Nav_2
  DO D_Win_N WITH 22, 71, "Индексация, конвертация..."

  *
  *  Информация о клиенте
  *
  DO Sun_Bord WITH  2,  3,  7, 67, " Клиент "
  @  3,  8 SAY "Имя "+ACC_1215.CUS_NAME
  @  4, 13 SAY "Примечание 1 "+ACC_1215.ACC_NOTE
  @  5, 13 SAY "Примечание 2 "+ACC_1215.ACC_NOTED
  @  6, 27 SAY "Флажки"
  @ ROW(), COL()+1 SAY ACC_1215.MARK1 COLOR (SCHEME(1,1))
  @ ROW(), COL()+1 SAY ACC_1215.MARK2 COLOR (SCHEME(1,1))
  @ ROW(), COL()+1 SAY ACC_1215.MARK3 COLOR (SCHEME(1,1))
  @ ROW(), COL()+1 SAY ACC_1215.MARK4 COLOR (SCHEME(1,1))
  @ ROW(), COL()+1 SAY ACC_1215.MARK5 COLOR (SCHEME(1,1))
ENDIF
nt = LEFT(ACC_1215.CUS_NAME,22)
ex = 1
IF .NOT. mute
  DO Sun_Bord WITH  8,  3, 10, 67, "                                      "
  @ 8, 22 SAY dep_list(p_dep,1)+" "+dep_list(p_dep,2)
ENDIF
dep_r = dep_list(p_dep, 3)
dep_d = dep_list(p_dep, 5)
IF .NOT. mute
  @ 9, 11 SAY "Депозит                 руб                   USD"
  *
  *   Движение денег
  *
  DO Sun_Bord WITH 11,  3, 17, 34, " Рубли в У Е "
  @ 12, 32 SAY "р"
  @ 13, 32 SAY "р"
  @ 14, 32 SAY "р"
  @ 16, 32 SAY "$"
  DO Sun_Bord WITH 11, 36, 17, 67, " У Е в рубли "
  @ 12, 65 SAY "$"
  @ 14, 65 SAY "р"
  @ 15, 65 SAY "р"
  @ 16, 65 SAY "р"

  *------------------------------------------------------------------------
  *      Ввод полей бланка
  *
  @  8,  6 GET dep_sw PICTURE "@*C Предприятие" VALID Ch_Dep()
  @  9, 20 GET dep_r PICTUR "@Z 999 999 999.99" WHEN .F.
  @  9, 42 GET dep_d PICTUR "@Z 9 999 999.9999" WHEN .F.
  @ 12,  5 SAY "С депозита  " GET r_to_d PICTURE "@Z 999 999 999.99"  ;
                     WHEN Save_Val(r_to_d) VALID Clc_RD(1)
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(dep_list(p_dep,1)))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(dep_list(p_dep,1)))
  @ 13,  5 SAY "Налог "+STR(tmpNalog,5,1)+"%"    ;
           GET r_to_dn PICTURE "@Z 999 999 999.99" WHEN .F.
  @ 14,  5 SAY "Итого       " GET r_to_dt PICTURE "@Z 999 999 999.99" WHEN .F.
  @ 15,  5 SAY "По курсу           " GET r_to_dk PICTURE "@Z 999.999" ;
                     WHEN Save_Val(r_to_dk) VALID Clc_RD(2)
  @ 16,  5 SAY "На депозит  " GET d_fr_r PICTURE "@Z 9 999 999.9999" ;
                     WHEN Save_Val(d_fr_r) VALID Clc_RD(3)
  @ 12, 38 SAY "С депозита  " GET d_to_r PICTURE "@Z 9 999 999.9999" ;
                     WHEN Save_Val(d_to_r) VALID Clc_DR(1)
  @ 13, 38 SAY "По курсу           " GET d_to_rk PICTURE "@Z 999.999" ;
                     WHEN Save_Val(d_to_rk) VALID Clc_DR(2)
  @ 14, 38 SAY "Итого       " GET d_to_rt PICTURE "@Z 999 999 999.99" WHEN .F.
  @ 15, 38 SAY "Налог "+STR(tmpNalog,5,1)+"%"  ;
           GET d_to_rn PICTURE "@Z 999 999 999.99" WHEN .F.
  @ 16, 38 SAY "На депозит  " GET r_fr_d PICTURE "@Z 999 999 999.99" ;
                     WHEN Save_Val(r_fr_d) VALID Clc_DR(3)

  @ WROWS()-4, 20 GET tmpCheck PICTURE "@*RH Приходный ордер   ;Чек" WHEN .F.
  @ WROWS()-3, 19 SAY "Примечание" GET nt
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

  READ CYCLE
ENDIF
is_c = tmpCheck = 2
IF ex = 1
  *
  * Отрабатываем бланк
  IF r_to_d # 0 .OR. d_to_r # 0
    IF Prp_Doc(is_c)
      IF TYPE("c_code") = "N"
        IF BUHG_REQ.IND_INC # r_fr_d
          REPLACE BUHG_REQ.R_INC  WITH BUHG_REQ.R_INC+BUHG_REQ.IND_INC-r_fr_d, ;
                  BUHG_REQ.IS_INC WITH .T., ;
                  BUHG_REQ.IND_EXP WITH r_fr_d/d_to_rk,  BUHG_REQ.IND_INC WITH r_fr_d
          PRIVATE mss
          DIMENSION mss(3)
          mss(1) = ""
          mss(2) = CHR(0)+"ВНИМАНИЕ! Требование в рублевой части увеличилось!"
          mss(3) = ""
          DO Out_Mess WITH 7, "mss"
        ENDIF
        REPLACE BUHG_REQ.IS_CONV   WITH .F.,  ;
                BUHG_REQ.WHO_INP   WITH user, ;
                BUHG_REQ.WHERE_INP WITH arm,  ;
                BUHG_REQ.DATE_INP  WITH DATE()
        PRIVATE s_sav
        s_sav = SELECT()
        SELECT 0
        USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LLL_000 AGAIN
        IF SEEK(BUHG_REQ.LST_NUM)
          REPLACE KASSA WITH KASSA+r_fr_d
        ENDIF
        USE
        SELECT (s_sav)
      ENDIF
      DO P_Docs
    ENDIF
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Dep       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Смена предприятия.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.02.1999 ──┘
PROCEDURE Ch_Dep

IF .NOT. dep_sw
  RETURN .T.
ENDIF
dep_sw = .F.

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

*┌──────────────────────────────────────────────────────┐
*│   Предприятие           Депозит(руб)   Депозит(USD)  │
*│ ┌──────────────────────────────────────────────────┐ │
*│ │ААА NNNNNNNNNNNNNNNN 999 999 999.99 9 999 999.9999│ │
*│ └──────────────────────────────────────────────────┘ │
*│            < OK Ctrl-W > < Отказаться Esc >          │
*└──────────────────────────────────────────────────────┘

PRIVATE ex, w_point, w_arr, ln

FOR ex = 1 TO ALEN(dep_list,1)
  DIMENSION w_arr(ex)
  w_arr(ex) = dep_list(ex, 1)+" "+dep_list(ex, 2)+ ;
              TRANSFORM(dep_list(ex, 3), "999 999 999.99")+" "+ ;
              TRANSFORM(dep_list(ex, 5), "9 999 999.9999")
ENDFOR
ln = MIN(ALEN(dep_list,1), 10)
w_point = p_dep
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7+ln, 58, "Выбор предприятия"
@ 2, 5 SAY "Предприятие           Депозит(руб)   Депозит(USD)"
*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3, 3 GET w_point FROM w_arr FUNCTION "&" SIZE ln+2, 52
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

RELEASE WINDOW (win_name)
IF ex = 1
  *
  * Отрабатываем бланк
  p_dep = w_point
  dep_r = dep_list(p_dep, 3)
  dep_d = dep_list(p_dep, 5)
  STORE 0 TO r_to_d, r_to_dn, r_to_dt, d_fr_r
  STORE 0 TO d_to_r, d_to_rt, d_to_rn, r_fr_d
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(dep_list(p_dep,1)))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(dep_list(p_dep,1)))
  @ 13,  5 SAY "Налог "+STR(tmpNalog,5,1)
  @ 15, 38 SAY "Налог "+STR(tmpNalog,5,1)
ENDIF
*--------------------------------------------------------------------------
SHOW GETS
POP KEY
@ 8, 22 SAY dep_list(p_dep,1)+" "+dep_list(p_dep,2)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Save_Val     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                 Сохранение значения при входе в поле.                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.02.1999 ──┘
PROCEDURE Save_Val
PARAMETERS v
v_sav = v
RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Clc_RD       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│           Варианты выхода из полей конвертации РУБЛИ -> USD.           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.02.1999 ──┘
PROCEDURE Clc_RD
PARAMETERS mode
IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

DO CASE
CASE mode = 1      && Рублевая сумма, снимаемая с депозита
  IF v_sav = r_to_d
    RETURN .T.
  ENDIF
  IF r_to_d < 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Для обратной конвертации другая половина бланка!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(dep_list(p_dep,1)))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(dep_list(p_dep,1)))
  r_to_dn = ROUND(r_to_d*tmpNalog/100,2)
  r_to_dt = r_to_d+r_to_dn
  d_fr_r  = IIF(r_to_dk = 0, 0, ROUND(r_to_dt/r_to_dk,4))
CASE mode = 2      && Курс для пересчета рублей в доллары
  IF v_sav = r_to_dk
    RETURN .T.
  ENDIF
  IF r_to_dk <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Таких курсов долларов не бывает!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  d_fr_r  = IIF(r_to_dk = 0, 0, ROUND(r_to_dt/r_to_dk,4))
CASE mode = 3      && Долларовая сумма, поступающая на депозит
  IF v_sav = d_fr_r
    RETURN .T.
  ENDIF
  IF d_fr_r < 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Для обратной конвертации другая половина бланка!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  IF d_fr_r = 0
    r_to_dt = 0
    r_to_dn = 0
    r_to_d  = 0
  ELSE
    IF r_to_d = 0
      r_to_dt = ROUND(d_fr_r*r_to_dk,2)
*      tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                      0, GetNalog(dep_list(p_dep,1)))
      tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                     0, GetNalog(dep_list(p_dep,1)))
      r_to_dn = ROUND(tmpNalog*r_to_dt/(100+tmpNalog),2)
      r_to_d  = r_to_dt-r_to_dn
    ELSE
      r_to_dk = ROUND(r_to_d/d_fr_r, 3)
    ENDIF
  ENDIF
ENDCASE    
DO Clc_Dep
SHOW GETS

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Clc_dep      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Расчет остатков на депозитах.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.02.1999 ──┘
PROCEDURE Clc_dep

dep_r = dep_list(p_dep, 3) - r_to_d + r_fr_d
dep_d = dep_list(p_dep, 5) - d_to_r + d_fr_r

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Clc_DR       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│           Варианты выхода из полей конвертации USD -> РУБЛИ.           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.02.1999 ──┘
PROCEDURE Clc_DR
PARAMETERS mode

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

DO CASE
CASE mode = 1      && Долларовая сумма, снимаемая с депозита
  IF v_sav = d_to_r
    RETURN .T.
  ENDIF
  IF d_to_r < 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Для обратной конвертации другая половина бланка!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  d_to_rt = ROUND(d_to_r*d_to_rk,2)
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(dep_list(p_dep,1)))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(dep_list(p_dep,1)))
  d_to_rn = ROUND(d_to_rt*tmpNalog/(100+tmpNalog),2)
  r_fr_d  = d_to_rt-d_to_rn
CASE mode = 2      && Курс для долларов в рубли
  IF v_sav = d_to_rk
    RETURN .T.
  ENDIF
  IF d_to_rk <= 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Таких курсов долларов не бывает!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  d_to_rt = ROUND(d_to_r*d_to_rk,2)
*  tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                  0, GetNalog(dep_list(p_dep,1)))
  tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                 0, GetNalog(dep_list(p_dep,1)))
  d_to_rn = ROUND(d_to_rt*tmpNalog/(100+tmpNalog),2)
  r_fr_d  = d_to_rt-d_to_rn
CASE mode = 3      && Рублева сумма, поступающая на депозит
  IF v_sav = r_fr_d
    RETURN .T.
  ENDIF
  IF r_fr_d < 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Для обратной конвертации другая половина бланка!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  IF r_fr_d = 0
    d_to_rt = 0
    d_to_rn = 0
    d_to_r  = 0
  ELSE
*    tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                    0, GetNalog(dep_list(p_dep,1)))
    tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                   0, GetNalog(dep_list(p_dep,1)))
    IF d_to_r = 0
      d_to_rt = ROUND(r_fr_d*(100+tmpNalog)/100,2)
      d_to_rn = d_to_rt-r_fr_d
      d_to_r  = ROUND(d_to_rt/d_to_rk, 4)
    ELSE
      d_to_rt = ROUND(r_fr_d*(100+tmpNalog)/100,2)
      d_to_rn = d_to_rt-r_fr_d
      d_to_rk = ROUND(d_to_rt/d_to_r, 3)
    ENDIF
  ENDIF
ENDCASE    
DO Clc_Dep
SHOW GETS

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_Doc      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Формирование документов.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.02.1999 ──┘
PROCEDURE Prp_Doc
PARAMETER is_c

PRIVATE s_sav, doc_n, doc_d, w, doc1, doc2
PRIVATE rst, rst_0, rst_n, rst_d, ord_cnt, numbs, sums, k_w00, j00
PRIVATE is_kkm
is_kkm = .F.

IF is_c
  is_kkm = FirmKKM(dep_list(p_dep,1))
  IF EMPTY(is_kkm)
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"На данном предприятии нет кассового аппарата!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  is_kkm = VAL(ALLTRIM(is_kkm))
ENDIF

IF .NOT. LetMoney(dep_list(p_dep,1), "3")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Данному предприятию нельзя принимать доллары!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF is_c
  IF .NOT. LetMoney(dep_list(p_dep,1), "Ч")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Данному предприятию нельзя пробивать чеки!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ELSE
  IF .NOT. LetMoney(dep_list(p_dep,1), " ")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Данному предприятию нельзя работать с наличными деньгами!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ENDIF

s_sav = SELECT()
SELECT 0
DELETE FILE (tmpo_path+"D_LIST.DBF")
CREATE DBF (tmpo_path+"D_LIST") ;
  ( FIRM     C( 3), ;
    DOC_NUM  C(10), ;
    DOC_NUMV C(11), ;
    DOC_DATE D,     ;
    DOC_TYPE C( 1), ;
    DOC_NAME C(16)  )
SELECT 0
*
*  Документы, связанные с движением денег
*
PRIVATE chk_num, chk_date, chk_time, chk_err
chk_num = ""
chk_err = 0
USE (base_path+"INCMONEY") ALIAS MNY_9212 AGAIN
doc_d = DATE()
chk_date = DATE()
chk_time = SECONDS()

IF r_to_d # 0                              

  rst = SELECT()
  SELECT 0
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9B23 AGAIN
  SEEK dep_list(p_dep,1)
  max_ord = IIF(FSIZE("M_SUM")=0 .OR. is_c, 0, M_SUM)
  USE 
  SELECT (rst)
  
  rst   = r_to_d        
  rst_0 = ROUND(r_to_d/(100+nds_flt)*100,2)
  rst_n = r_to_dn
  rst_d = ROUND(r_to_d/kurs,4)
  k_w00 = rst/rst_d
  ord_cnt  = 0
  
  IF rst < max_ord .OR. max_ord = 0
    DIMENSION numbs(1), sums(1,4)
    ord_cnt  = 1
    numbs(1) = 0
    sums(1,1) = rst
    sums(1,2) = rst_0
    sums(1,3) = rst_n
    sums(1,4) = rst_d
  ELSE
    DO WHILE rst > 0
      IF rst < max_ord
        sum_w = rst
      ELSE
        sum_w = max_ord-ROUND(0.1*RAND()*max_ord, 0)
      ENDIF
      ord_cnt = ord_cnt+1
      rst = rst-sum_w
      DIMENSION numbs(ord_cnt), sums(ord_cnt,4)
      numbs(ord_cnt) = 0
      sums(ord_cnt,1)  = sum_w
      IF rst # 0
*        tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                        0, GetNalog(dep_list(p_dep,1)))
        tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                       0, GetNalog(dep_list(p_dep,1)))
        sums(ord_cnt,2)  = ROUND(sum_w/(100+nds_flt)*100, 2)
        rst_0 = rst_0-sums(ord_cnt,2)
        sums(ord_cnt,3)  = ROUND(sum_w*tmpNalog/100, 2)
        rst_n = rst_n-sums(ord_cnt,3)
        sums(ord_cnt,4)  = ROUND(sum_w/k_w00,4)
        rst_d = rst_d-sums(ord_cnt,4)
      ELSE
        sums(ord_cnt,2)  = rst_0
        sums(ord_cnt,3)  = rst_n
        sums(ord_cnt,4)  = rst_d
      ENDIF
    ENDDO
  ENDIF

  FOR j00 = 1 TO ord_cnt

    SELECT MNY_9212

    IF is_c              && Чек
      IF File_O(base_path+"TO_KKM.DBF") .AND. is_kkm # 0
        DO Do_Check WITH -sums(j00,2), -sums(j00,1)+sums(j00, 2), -sums(j00, 3), chk_num, chk_date, chk_time, chk_err
        IF .NOT. EMPTY(chk_err)
          DIMENSION mss(3)
          mss(1) = ""
          mss(2) = CHR(0)+"Ошибка "+ALLTRIM(STR(chk_err))+" при попытке пробить чек"
          mss(3) = ""
          DO Out_Mess WITH 7, "mss"
          SELECT MNY_9212
          USE
          SELECT D_LIST
          USE
          DELETE FILE (tmpo_path+"D_LIST.DBF")
          SELECT (s_sav)
          RETURN .F.
        ENDIF
      ENDIF
    ELSE
      chk_num = Doc_Num("EXP_ORDER", dep_list(p_dep,1), arm, doc_d)
      chk_num = sys_char+STR(-chk_num,9)
    ENDIF
    numbs(j00) = chk_num
    APPEND BLANK
*    tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                    0, GetNalog(dep_list(p_dep,1)))
    tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                   0, GetNalog(dep_list(p_dep,1)))
    REPLACE FIRM       WITH dep_list(p_dep,1),  ;
            DOC_TYPE   WITH IIF(is_c, "Ч", " "),;
            DOC_NUM    WITH chk_num,       ;
            DOC_DATE   WITH chk_date+j00-1,;
            DOC_TIME   WITH chk_time,      ;
            ENTER_DAT  WITH chk_date+j00-1,;
            CUS_CODE   WITH ACC_1215.CUS_CODE,  ;
            CUS_NAME   WITH ACC_1215.CUS_NAME,  ;
            MONEY_0    WITH sums(j00,2), ;
            NDS_       WITH nds_flt, ;
            MONEY_R    WITH -sums(j00,1), ;
            MONEY_D    WITH -sums(j00,4), ;
            NL_PRC     WITH tmpNalog,     ;
            N_OF_SALE  WITH -sums(j00,3), ;
            WHO        WITH user,    ;
            WHERE      WITH arm
    SELECT D_LIST
    APPEND BLANK
    REPLACE FIRM     WITH dep_list(p_dep,1), ;
            DOC_NUM  WITH chk_num , ;
            DOC_NUMV WITH chk_num,  ; 
            DOC_DATE WITH chk_date, ;
            DOC_TYPE WITH IIF(is_c, "Ч", " "), ;
            DOC_NAME WITH IIF(is_c, "Чек", "Расходный ордер")
  ENDFOR

  SELECT MNY_9212
  doc_n = Doc_Num("MONEY_$", dep_list(p_dep,1), arm, doc_d)
  APPEND BLANK
  REPLACE FIRM       WITH dep_list(p_dep,1),  ;
          DOC_TYPE   WITH "3",                ;
          DOC_NUM    WITH sys_char+STR(doc_n,9), ;
          DOC_DATE   WITH doc_d,   ;
          DOC_TIME   WITH SECONDS(), ;
          ENTER_DAT  WITH doc_d,   ;
          CUS_CODE   WITH ACC_1215.CUS_CODE,  ;
          CUS_NAME   WITH ACC_1215.CUS_NAME,  ;
          MONEY_0    WITH d_fr_r,   ;
          NDS_       WITH 0,        ;
          MONEY_R    WITH ROUND(d_fr_r*kurs,2),  ;
          MONEY_D    WITH d_fr_r,   ;
          NL_PRC     WITH 0,        ;
          N_OF_SALE  WITH 0,        ;
          WHO        WITH user,     ;
          WHERE      WITH arm
  SELECT D_LIST
  APPEND BLANK
  REPLACE FIRM     WITH dep_list(p_dep,1), ;
          DOC_NUM  WITH sys_char+STR(doc_n,9), ;
          DOC_NUMV WITH sys_char+ALLTRIM(STR(doc_n)), ;
          DOC_DATE WITH doc_d, ;
          DOC_TYPE WITH "3",   ;
          DOC_NAME WITH "Приход USD"
  SELECT MNY_9212
ENDIF

IF d_to_r # 0

  rst = SELECT()
  SELECT 0
  USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_9B23 AGAIN
  SEEK dep_list(p_dep,1)
  max_ord = IIF(FSIZE("M_SUM")=0 .OR. is_c, 0, M_SUM)
  USE 
  SELECT (rst)

  rst   = r_fr_d
  rst_0 = ROUND(r_fr_d/(100+nds_flt)*100,2)
  rst_n = d_to_rn
  rst_d = ROUND(r_fr_d/kurs,4)
  k_w00 = rst/rst_d
  ord_cnt  = 0
  IF rst < max_ord .OR. max_ord = 0
    DIMENSION numbs(1), sums(1,4)
    ord_cnt  = 1
    numbs(1) = 0
    sums(1,1) = rst
    sums(1,2) = rst_0
    sums(1,3) = rst_n
    sums(1,4) = rst_d
  ELSE
    DO WHILE rst > 0
      IF rst < max_ord
        sum_w = rst
      ELSE
        sum_w = max_ord-ROUND(0.1*RAND()*max_ord, 0)
      ENDIF
      ord_cnt = ord_cnt+1
      rst = rst-sum_w
      DIMENSION numbs(ord_cnt), sums(ord_cnt,4)
      numbs(ord_cnt) = 0
      sums(ord_cnt,1)  = sum_w
      IF rst # 0
*        tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                        0, GetNalog(dep_list(p_dep,1)))
        tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                       0, GetNalog(dep_list(p_dep,1)))
        sums(ord_cnt,2)  = ROUND(sum_w/(100+nds_flt)*100, 2)
        rst_0 = rst_0-sums(ord_cnt,2)
        sums(ord_cnt,3)  = ROUND(sum_w*tmpNalog/100, 2)
        rst_n = rst_n-sums(ord_cnt,3)
        sums(ord_cnt,4)  = ROUND(sum_w/k_w00,4)
        rst_d = rst_d-sums(ord_cnt,4)
      ELSE
        sums(ord_cnt,2)  = rst_0
        sums(ord_cnt,3)  = rst_n
        sums(ord_cnt,4)  = rst_d
      ENDIF
    ENDDO
  ENDIF

  FOR j00 = 1 TO ord_cnt

    SELECT MNY_9212

    IF is_c              && Чек
      IF File_O(base_path+"TO_KKM.DBF") .AND. is_kkm # 0
        DO Do_Check WITH sums(j00,2), sums(j00,1)-sums(j00,2), sums(j00,3), chk_num, chk_date, chk_time, chk_err
        IF .NOT. EMPTY(chk_err)
          DIMENSION mss(3)
          mss(1) = ""
          mss(2) = CHR(0)+"Ошибка "+ALLTRIM(STR(chk_err))+" при попытке пробить чек"
          mss(3) = ""
          DO Out_Mess WITH 7, "mss"
          SELECT MNY_9212
          USE
          SELECT D_LIST
          USE
          DELETE FILE (tmpo_path+"D_LIST.DBF")
          SELECT (s_sav)
          RETURN .F.
        ENDIF
      ENDIF
    ELSE
      chk_num = Doc_Num("INC_ORDER", dep_list(p_dep,1), arm, doc_d)
      chk_num = sys_char+STR(chk_num,9)
    ENDIF
    numbs(j00) = chk_num
*    tmpNalog = IIF(.NOT. is_c .AND. ACC_1215.MARK1 = " ", ;
*                    0, GetNalog(dep_list(p_dep,1)))
    tmpNalog = IIF(ACC_1215.MARK1 = " ", ;
                   0, GetNalog(dep_list(p_dep,1)))
    APPEND BLANK
    REPLACE FIRM       WITH dep_list(p_dep,1),  ;
            DOC_TYPE   WITH IIF(is_c, "Ч", " "),;
            DOC_NUM    WITH chk_num,     ;
            DOC_DATE   WITH chk_date+j00-1,  ;
            DOC_TIME   WITH chk_time,        ;
            ENTER_DAT  WITH chk_date+j00-1,  ;
            CUS_CODE   WITH ACC_1215.CUS_CODE,  ;
            CUS_NAME   WITH ACC_1215.CUS_NAME,  ;
            MONEY_0    WITH sums(j00,2), ;
            NDS_       WITH nds_flt, ;
            MONEY_R    WITH sums(j00,1), ;
            MONEY_D    WITH sums(j00,4), ;
            NL_PRC     WITH tmpNalog,    ;
            N_OF_SALE  WITH sums(j00,3), ;
            WHO        WITH user,    ;
            WHERE      WITH arm
    SELECT D_LIST
    APPEND BLANK
    REPLACE FIRM     WITH dep_list(p_dep,1), ;
            DOC_NUM  WITH chk_num, ;
            DOC_NUMV WITH chk_num, ;
            DOC_DATE WITH chk_date,;
            DOC_TYPE WITH IIF(is_c, "Ч", " "), ;
            DOC_NAME WITH IIF(is_c, "Чек", "Приходный ордер")
  ENDFOR

  SELECT MNY_9212
  doc_n = Doc_Num("MONEY_$", dep_list(p_dep,1), arm, doc_d)
  APPEND BLANK
  REPLACE FIRM       WITH dep_list(p_dep,1),  ;
          DOC_TYPE   WITH "3",                ;
          DOC_NUM    WITH sys_char+STR(doc_n,9), ;
          DOC_DATE   WITH doc_d,   ;
          DOC_TIME   WITH SECONDS(), ;
          ENTER_DAT  WITH doc_d,   ;
          CUS_CODE   WITH ACC_1215.CUS_CODE,  ;
          CUS_NAME   WITH ACC_1215.CUS_NAME,  ;
          MONEY_0    WITH -d_to_r,   ;
          NDS_       WITH 0,        ;
          MONEY_R    WITH -ROUND(d_to_r*kurs,2),  ;
          MONEY_D    WITH -d_to_r,   ;
          NL_PRC     WITH 0,        ;
          N_OF_SALE  WITH 0,        ;
          WHO        WITH user,     ;
          WHERE      WITH arm
  SELECT D_LIST
  APPEND BLANK
  REPLACE FIRM     WITH dep_list(p_dep,1), ;
          DOC_NUM  WITH sys_char+STR(doc_n,9), ;
          DOC_NUMV WITH sys_char+ALLTRIM(STR(doc_n)), ;
          DOC_DATE WITH doc_d, ;
          DOC_TYPE WITH "3",   ;
          DOC_NAME WITH "Расход USD"
  SELECT MNY_9212
ENDIF

*
*   Коррекция депозита
*
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE ALIAS DEP_9212 AGAIN
IF .NOT. SEEK(STR(ACC_1215.CUS_CODE)+dep_list(p_dep,1))
  APPEND BLANK
  REPLACE CUS_CODE WITH ACC_1215.CUS_CODE, ;
          FIRM     WITH dep_list(p_dep,1)
ENDIF
REPLACE ACC_SUM  WITH ACC_SUM+r_fr_d-r_to_d, ;
        ACC_SUMD WITH ACC_SUMD+d_fr_r-d_to_r
*
*  Налог
*
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS FR_9212 AGAIN
SEEK dep_list(p_dep,1)
REPLACE NALOG WITH NALOG+d_to_rn-r_to_dn

USE (base_path+"ARM_LST") ALIAS A1702 AGAIN
PRIVATE kass_usd, kass_r

GO arm_rec
kass_usd = KASSA_USD
kass_r = KASSA
*
*  Касса рабочего места
*
USE (base_path+"KASSA") ALIAS KSS_9212 AGAIN
IF d_to_rt-r_to_dt # 0
  doc1 = Doc_Num("KASSA", dep_list(p_dep,1), arm, doc_d)
  doc1 = sys_char+CHRTRAN(arm," ","0")+STR(doc1,6)
  APPEND BLANK
  REPLACE DOC_NUM    WITH doc1,      ;
          DOC_DATE   WITH doc_d,     ;
          DOC_TIME   WITH SECONDS(), ;
          VALUTA     WITH "Р",       ;
          MONEY_REST WITH 0, ;
          KASSA_REST WITH kass_r-(d_to_rt-r_to_dt), ;
          TOTAL      WITH d_to_rt-r_to_dt, ;
          WHO_REP    WITH user,      ;
          WHO_GET    WITH user,      ;
          WHERE      WITH arm
  SELECT D_LIST
  APPEND BLANK
  REPLACE DOC_NUM  WITH doc1, ;
          DOC_NUMV WITH LEFT(doc1,4)+"-"+ALLTRIM(RIGHT(doc1,5)), ;
          DOC_DATE WITH doc_d, ;
          DOC_TYPE WITH "!", ;
          DOC_NAME WITH "Сдача денег (руб)"

  SELECT KSS_9212
ENDIF

IF d_fr_r-d_to_r # 0
  doc2 = Doc_Num("KASSA", dep_list(p_dep,1), arm, doc_d)
  doc2 = sys_char+CHRTRAN(arm," ","0")+STR(doc2,6)
  APPEND BLANK
  REPLACE DOC_NUM    WITH doc2,      ;
          DOC_DATE   WITH doc_d,     ;
          DOC_TIME   WITH SECONDS(), ;
          VALUTA     WITH "$",       ;
          MONEY_REST WITH 0, ;
          KASSA_REST WITH kass_usd-(d_fr_r-d_to_r), ;
          TOTAL      WITH d_fr_r-d_to_r, ;
          WHO_REP    WITH user,      ;
          WHO_GET    WITH user,      ;
          WHERE      WITH arm
  SELECT D_LIST
  APPEND BLANK
  REPLACE DOC_NUM  WITH doc2, ;
          DOC_NUMV WITH LEFT(doc2,4)+"-"+ALLTRIM(RIGHT(doc2,5)), ;
          DOC_DATE WITH doc_d, ;
          DOC_TYPE WITH "!", ;
          DOC_NAME WITH "Сдача денег (USD)"

  SELECT KSS_9212
ENDIF
*
*   Основная касса
*
USE (base_path+"KASSA_M") ALIAS KSS_9212 AGAIN
IF d_to_rt-r_to_dt # 0
  doc_n = Doc_Num("KASSA_M", dep_list(p_dep,1), arm, doc_d)
  doc_n = sys_char+STR(doc_n,6)
  APPEND BLANK
  REPLACE DOC_NUM    WITH doc_n,  ;
          DOC_DATE   WITH doc_d,  ;
          DOC_TIME   WITH SECONDS(),  ;
          CODE       WITH 1,      ;
          REF_NUM    WITH doc1,   ;
          REF_DATE   WITH doc_d,  ;
          SUM_R      WITH d_to_rt-r_to_dt, ;
          NOTE       WITH nt,     ;
          WHO        WITH user,   ;
          WHERE      WITH arm
ENDIF

IF d_fr_r-d_to_r # 0
  doc_n = Doc_Num("KASSA_M", dep_list(p_dep,1), arm, doc_d)
  doc_n = sys_char+STR(doc_n,6)
  APPEND BLANK
  REPLACE DOC_NUM    WITH doc_n,  ;
          DOC_DATE   WITH doc_d,  ;
          DOC_TIME   WITH SECONDS(),  ;
          CODE       WITH 1,      ;
          REF_NUM    WITH doc2,   ;
          REF_DATE   WITH doc_d,  ;
          SUM_D      WITH d_fr_r-d_to_r,;
          NOTE       WITH nt,     ;
          WHO        WITH user,   ;
          WHERE      WITH arm
ENDIF
IF d_to_r # 0
  doc_n = Doc_Num("KASSA_M", dep_list(p_dep,1), arm, doc_d)
  doc_n = sys_char+STR(doc_n,6)
  APPEND BLANK
  REPLACE DOC_NUM    WITH doc_n,  ;
          DOC_DATE   WITH doc_d,  ;
          DOC_TIME   WITH SECONDS(),  ;
          CODE       WITH 2,      ;
          SUM_R      WITH -d_to_rt,;
          SUM_D      WITH d_to_r, ;
          KURS_FL    WITH d_to_rk,;
          NOTE       WITH nt,     ;
          WHO        WITH user,   ;
          WHERE      WITH arm
ENDIF

IF r_to_d # 0
  doc_n = Doc_Num("KASSA_M", dep_list(p_dep,1), arm, doc_d)
  doc_n = sys_char+STR(doc_n,6)
  APPEND BLANK
  REPLACE DOC_NUM    WITH doc_n,  ;
          DOC_DATE   WITH doc_d,  ;
          DOC_TIME   WITH SECONDS(),  ;
          CODE       WITH 2,      ;
          SUM_R      WITH r_to_dt,;
          SUM_D      WITH -d_fr_r,;
          KURS_FL    WITH d_to_rk,;
          NOTE       WITH nt,     ;
          WHO        WITH user,   ;
          WHERE      WITH arm
ENDIF
USE
SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура P_Docs       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Печать документов.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.02.1999 ──┘
PROCEDURE P_Docs

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
SELECT D_LIST

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Печать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
*┌───────────────────────────────┐
*│ФФФ NNNNNNNNNNN NNNNNNNNNNNNNNN│
*└───────────────────────────────┘
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 31

DO D_Wins WITH ln, wd, "Сформированы документы", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS FIRM:H="", DOC_NUMV:H="", DOC_NAME:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && Печать

    DO CASE
    CASE DOC_TYPE $ " 3Ч"
      DO P_Money WITH FIRM, DOC_TYPE, DOC_NUM, DOC_DATE
    CASE DOC_TYPE = "!"
      DO P_Rep WITH DOC_NUM, DOC_DATE
    ENDCASE
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
USE
SELECT (s_sav)

RETURN
