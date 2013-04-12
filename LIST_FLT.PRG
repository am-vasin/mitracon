*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла List_Flt     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Установка фильтра для списков.                     ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 10.05.2005 ══╝
PROCEDURE List_Flt
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

*┌───────────────────────────────────┐
*│ ┌─ Продажа ─────────────────────┐ │
*│ │ [ ] Продано   [ ] Не продано  │ │
*│ └───────────────────────────────┘ │
*│ ┌─ Счет ────────────────────────┐ │
*│ │ [ ] Выписан   [ ] Не выписан  │ │
*│ └───────────────────────────────┘ │
*│ ┌─ Оплата счета ────────────────┐ │
*│ │ [ ] Оплачен   [ ] Не оплачен  │ │
*│ └───────────────────────────────┘ │
*│ ┌─ Бронь ───────────────────────┐ │
*│ │ [ ] Бронь [ ] Резерв [ ] Ост. │ │
*│ └───────────────────────────────┘ │
*│  < OK Ctrl-W > < Отказаться Esc > │
*└───────────────────────────────────┘

PRIVATE ex
PRIVATE sl, n_sl, bl, n_bl, pay, n_pay, br, res, rst
STORE .T. TO sl, n_sl, bl, n_bl, pay, n_pay, br, res, rst
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 16, 39, "Фильтр"
DO Sun_Bord WITH   2,  3,  4, 35, " Продажа "
DO Sun_Bord WITH   5,  3,  7, 35, " Счет "
DO Sun_Bord WITH   8,  3, 10, 35, " Оплата счета "
DO Sun_Bord WITH  11,  3, 13, 35, " Бронь "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@   3,  5 GET sl    PICTURE "@*C Продано"
@   3, 19 GET n_sl  PICTURE "@*C Не продано"
@   6,  5 GET bl    PICTURE "@*C Выписан"
@   6, 19 GET n_bl  PICTURE "@*C Не выписан"
@   9,  5 GET pay   PICTURE "@*C Оплачен"
@   9, 19 GET n_pay PICTURE "@*C Не оплачен"
@  12,  5 GET br    PICTURE "@*C Бронь"
@  12, 15 GET res   PICTURE "@*C Резерв"
@  12, 26 GET rst   PICTURE "@*C Ост."

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  PRIVATE s_sav, cur_v
  *
  * Отрабатываем бланк
  s_sav = SELECT()
  SELECT 0
  USE (base_path+"LIST_DET") SHARED AGAIN ORDER TAG LIST ALIAS LLL_10505
  SELECT 0
  USE (base_path+"BILLS") SHARED AGAIN ORDER TAG DOC_NUM ALIAS BBB_10505
  SELECT 0
  USE (base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_ARM ALIAS TTT_10505
  SET RELATION TO BILL_FIRM+BILL_YEAR+BILL_NUM INTO BBB_10505
  SEEK arm
  SCAN REST WHILE LST_ARM = arm
    cur_v = ""
    DO CASE
    CASE .NOT. sl .AND. .NOT. EMPTY(SAL_NUM)
      cur_v = "*"
    CASE .NOT. n_sl .AND. EMPTY(SAL_NUM)
      cur_v = "*"
    CASE .NOT. bl .AND. .NOT. EMPTY(BILL_NUM)
      cur_v = "*"
    CASE .NOT. n_bl .AND. EMPTY(BILL_NUM)
      cur_v = "*"
    CASE .NOT. pay .AND. "Оплачено" $ BBB_10505.STATUS
      cur_v = "*"
    CASE .NOT. n_pay .AND. .NOT. "Оплачено" $ BBB_10505.STATUS
      cur_v = "*"
    CASE .NOT. br .OR. .NOT. res .OR. .NOT. rst .AND. EMPTY(SAL_NUM)
      IF Br_Exist()
        IF EMPTY(REQ_DATE)
          IF .NOT. m.res
            cur_v = "*"
          ENDIF
        ELSE
          IF .NOT. m.br
            cur_v = "*"
          ENDIF
        ENDIF
      ELSE
        IF .NOT. m.rst
          cur_v = "*"
        ENDIF
      ENDIF
    ENDCASE
    REPLACE FILTER WITH cur_v
  ENDSCAN
  USE IN BBB_10505
  USE IN TTT_10505
  USE IN LLL_10505
  SELECT (s_sav)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Br_Exist     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Наличие брони (резерва) в списке.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.01.2008 ──┘
PROCEDURE Br_Exist

PRIVATE l_n, ret_v
m.l_n = STR(TTT_10505.LST_NUM, 6)
m.ret_v = .F.

SELECT LLL_10505
SEEK m.l_n
SCAN REST WHILE LIST_NUM = m.l_n
  IF .NOT. EMPTY(QNT_REQ)
    m.ret_v = .T.
    EXIT
  ENDIF
ENDSCAN

SELECT TTT_10505

RETURN m.ret_v
