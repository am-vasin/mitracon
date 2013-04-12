*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Rev_Tit      Разработчик Андрей Васин           06.05.97 09:39:47 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║          Формирование заготовки приема от покупателя.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Rev_Tit
PRIVATE w_code, w_name
PRIVATE f_c000, f_n000, f_c_old
PRIVATE shab1, shab2, s_tag
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
f_c000 = def_firm
f_n000 = ""
f_c_old = def_firm
w_code = 0
w_name = SPACE(50)

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME

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
*│        Предприятие 999 9999999999999999 (F1)         │
*│ ┌─ [ ] Клиент ─────────────────────────────────────┐ │
*│ │....:....!....:....!....:....!....:....!....:....!│ │
*│ └──────────────────────────────────────────────────┘ │
*│           < OK Ctrl-W > < Отказаться Esc >           │
*└──────────────────────────────────────────────────────┘

PRIVATE ex, c_sw
ex = 1
c_sw = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 58, "Заполните бланк"
DO Sun_Bord WITH  3,  3,  5, 54, "            "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO Get_Firm WITH 2, 10, enab_firm
@ 3, 6 GET c_sw PICTURE "@*C Клиент" VALID C_Cust()
@ 4, 4 GET w_name WHEN .F.
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_B()
USE
SELECT INC_TIT

IF ex = 1
  *
  * Отрабатываем бланк
  IF RECCOUNT("TMP_INC") # 0
    SELECT TMP_INC
    ZAP
    SELECT INC_TIT
  ENDIF
  IF RECCOUNT("TMP_NEW") # 0
    SELECT TMP_NEW
    ZAP
    SELECT INC_TIT
  ENDIF

  SCATTER TO arr_w BLANK
  SET DELETED OFF
  LOCATE FOR DELETED()
  IF FOUND()
    RECALL
    GATHER FROM arr_w
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE FIRM      WITH f_c000,   ;
          SUP_CODE  WITH w_code,   ;
          SUP_NAME  WITH w_name,   ;
          FILE_TYPE WITH 1,        ;
          BUY       WITH 1,        ;
          IS_PRICE  WITH "Р"       ;
          LET_INC   WITH .T.       ;
          LOCKED    WITH "***"
  SELECT INC_LIST
  m.s_tag = TAG()
  SET ORDER TO DEL
  GO TOP
  IF DELETED()
    RECALL
    BLANK
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE LIST_NO WITH STR(RECNO("INC_TIT"),5)
  SET DELETED ON
  SET ORDER TO TAG (m.s_tag)
  SELECT 0
  USE (log_path+"DUE_LOG")
  COPY STRU TO (tmp_path+"DL"+ALLTRIM(STR(RECNO("INC_TIT"))))
  USE
  SELECT INC_TIT
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура C_Cust       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Выбор клиента.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10.02.1999 ──┘
PROCEDURE C_Cust

IF EMPTY(w_code)
  GO TOP
ELSE
  SET ORDER TO TAG CUS_CODE
  SEEK w_code
  SET ORDER TO TAG CUS_NAME
ENDIF

IF Custs("", .T., .F., .T.)
  w_code = CUS_CODE
  w_name = CUS_NAME
  SHOW GET w_name
ENDIF

c_sw = .F.
SHOW GET c_sw

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_B        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Выход из бланка.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10.02.1999 ──┘
PROCEDURE Tst_B

PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(f_c000)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали предприятие..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(w_code)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали клиента..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF
RETURN .T.
