*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Rd_Ord       Разработчик Андрей Васин           01.06.98 11:07:57 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Чтение заявки на реализацию.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Rd_Ord
PRIVATE s_sav, let_choose, n_list, c_list, n_list, n, mss, pth, point
PRIVATE f_c000, f_n000, f_c_old, q_w
PRIVATE shab1, shab2
shab1 = "abcdefghijklmnopqrstuvwxyzабвгдеёжзийклмнопрстуфхцчшщъыьэюя"
shab2 = "ABCDEFJHIJKLMNOPQRSTUVWXYZАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ"
f_c000 = def_firm
f_n000 = SPACE(16)
f_c_old = def_firm

s_sav = SELECT()
*
*  Формируем список филиалов.
*
SELECT 0
USE (base_path+"CUSTOMER") ALIAS CUS01698 AGAIN
n = 0
SCAN FOR RYN # " "
  n = n+1
  DIMENSION n_list(n), c_list(n)
  n_list(n) = B_NAME
  c_list(n) = CODE
ENDSCAN
USE

IF n = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не создали ни одного филиала!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (s_sav)
  RETURN
ENDIF

*
*  Ищем файлы с заявкой.
*
pth = ""
DO WHILE .T.
  pth = Int_Dir(pth, "Где ищем заявку?")
  IF EMPTY(pth)
    SELECT (s_sav)
    RETURN
  ENDIF
  IF .NOT. FIle_O(pth+"ORD_S.DBF")
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"  Там нет заявки! Продолжаем поиск? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 2
      SELECT (s_sav)
      RETURN
    ENDIF
    LOOP
  ENDIF
  EXIT
ENDDO

*
*    Обрабатываем заголовок файла, если от существует
*
IF .NOT. FIle_O(pth+"ORD_S_T.DBF")  && А вот и не существует!
  let_choose = .T.
  point = 1
ELSE
  let_choose = .F.
  USE (pth+"ORD_S_T")
  point = ASCAN(c_list, CUS_CODE)
  USE
  IF point = 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Эта заявка не от филиала!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    SELECT (s_sav)
    RETURN
  ENDIF
ENDIF
USE (pth+"ORD_S")
IF BOF() .OR. EOF()
  USE
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Эта заявка пуста!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT (s_sav)
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

*┌──────────────────────────────────────────┐
*│               ┌──────────────────┐       │
*│       Клиент  │ ....:....!....:. │       │
*│               └──────────────────┘       │
*│      Заявка N 99999 от ДД.ММ.ГГГГ        │
*│ ┌─ Позиции без кода ───────────────────┐ │
*│ │ ( ) Ищем по имени     ( ) Игнорируем │ │
*│ └──────────────────────────────────────┘ │
*│ ┌─ Нераспознанные позиции ─────────────┐ │
*│ │ ( ) Заносим в список  ( ) Игнорируем │ │
*│ └──────────────────────────────────────┘ │
*│ ┌─ Цены ───────────────────────────────┐ │
*│ │      ( ) В рублях    ( ) В USD       │ │
*│ └──────────────────────────────────────┘ │
*│   Предприятие 999 999999999999999 (F1)   │
*│     < OK Ctrl-W > < Отказаться Esc >     │
*└──────────────────────────────────────────┘

PRIVATE ex, seek_s, save_s, p_type
ex   = 1
seek_s = 1
save_s = 1
p_type = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 18, 46, "Заказ на поставку"
DO Sun_Bord WITH 6, 3, 8, 42, " Позиции без кода "
DO Sun_Bord WITH 9, 3, 11, 42, " Нераспознанные позиции "
DO Sun_Bord WITH 12, 3, 14, 42, " Цены "
@ 5, 2 SAY PADC("Заявка N "+ALLTRIM(ORD_NUM)+" от "+DTOC(ORD_DATE), WCOLS()-4)
@ 3, 9 SAY "Клиент"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2, 17 GET point PICTURE "@^" FROM n_list SIZE 1, 18 WHEN let_choose
@  7,  5 GET seek_s PICTURE "@*RH Ищем по имени    ;Игнорируем"
@ 10,  5 GET save_s PICTURE "@*RH Заносим в список ;Игнорируем"
@ 13, 10 GET p_type PICTURE "@*RH В рублях   ; В USD"
DO Get_Firm WITH 15, 5, enab_firm
@ 16,  7 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID T_Firm()

IF ex = 2
  POP KEY
  RELEASE WINDOW (win_name)
  USE
  SELECT (s_sav)
  RETURN
ENDIF
*
*  Проверяем заявку на повтор
*
SELECT 0
USE (base_path+"RYN_M_T") ORDER TAG CUS_CODE AGAIN
SEEK c_list(point)
SCAN REST WHILE CUS_CODE = c_list(point)
  IF ORD_NUM = ORD_S.ORD_NUM .AND. ORD_DATE = ORD_S.ORD_DATE
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Заявка с таким номером уже была! Продолжаем? [ Да ] [ Нет ]"
    mss(3) = ""
    IF Out_Mess(7, "mss") = 2
      USE
      SELECT ORD_S
      USE
      SELECT (s_sav)
      POP KEY
      RELEASE WINDOW (win_name)
      RETURN
    ENDIF
    EXIT
  ENDIF
ENDSCAN

*
*  Формируем список
*
USE (base_path+"LIST_DET") ALIAS LD03698 AGAIN
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS LT03698 AGAIN
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS SC03698 AGAIN
SET FILTER TO PRICE_R # 0 .AND. EMPTY(STOP_SALE)
SELECT 0
USE (base_path+"STOCK") ORDER TAG NAME_P ALIAS SN03698 AGAIN
SET FILTER TO PRICE_R # 0 .AND. EMPTY(STOP_SALE)

SELECT ORD_S
PRIVATE n_l, code_w, sum_w, p_rub, p_usd
n_l    = 0
code_w = 0
sum_w  = 0

SCAN
  code_w = 0
  IF .NOT. SEEK(CODE, "SC03698") .OR. CODE = 0
    IF seek_s = 1
      IF SEEK(NAME+PREFIX+PRODUCER, "SN03698")
        code_w = SN03698.CODE
        GO RECNO("SN03698") IN SC03698
      ENDIF
    ENDIF
  ELSE
    code_w = SC03698.CODE
  ENDIF
  IF code_w = 0 .AND. save_s = 2
    LOOP
  ENDIF
  IF n_l = 0
    n_l = Doc_Num("LISTS", def_firm, arm, DATE())
    SELECT LT03698
    APPEND BLANK
    REPLACE LST_ARM   WITH arm,    ;
            LST_NUM   WITH n_l,    ;
            CR_DAT    WITH DATE(), ;
            LST_NAME  WITH n_list(point), ;
            FIRM      WITH f_c000,        ;
            BANK      WITH def_bank,      ;
            REQ_DOC   WITH ORD_S.ORD_NUM, ;
            REQ_DATE  WITH ORD_S.ORD_DATE,;
            IS_USD    WITH p_type = 2,    ;
            CLI_CODE  WITH c_list(point), ;
            CLI_TYPE  WITH 4,             ;
            ORD_QUEUE WITH queuetype,     ;
            WHO_CR    WITH user,          ;
            WHERE_CR  WITH arm
  ENDIF
  SELECT LD03698
  p_rub = 0
  p_usd = 0
  IF LT03698.IS_USD
    p_usd = IIF(code_w # 0, SC03698.PRICE, 0)
  ELSE
    p_rub = IIF(code_w # 0, SC03698.PRICE_R, 0)
  ENDIF
  APPEND BLANK
  q_w = ORD_S.QNT
  IF code_w # 0
*    IF q_w % MAX(SC03698.QNT_ROUND,1) # 0 .AND. q_w > 0
      q_w = Round_P(SC03698.PREFIX+" "+SC03698.NAME+" "+SC03698.PRODUCER, q_w, SC03698.QNT_ROUND, SC03698.QNT_PACK, SC03698.PRICE)
*    ENDIF
  ENDIF
  REPLACE LIST_NUM  WITH STR(n_l, 6), ;
          CODE      WITH code_w,      ;
          PREFIX    WITH IIF(code_w # 0, SC03698.PREFIX, ORD_S.PREFIX), ;
          NAME      WITH IIF(code_w # 0, SC03698.NAME, ORD_S.NAME), ;
          PRO_CODE  WITH IIF(code_w # 0, SC03698.PRO_CODE, 0), ;
          PRODUCER  WITH IIF(code_w # 0, SC03698.PRODUCER, ORD_S.PRODUCER), ;
          ADDRESS   WITH IIF(code_w # 0, SC03698.ADDRESS, ""), ;
          QNT       WITH q_w, ;
          DEF_PRICE WITH IIF(code_w # 0, SC03698.PRICE_R, 0), ;
          USD_DEF   WITH IIF(code_w # 0, SC03698.PRICE, 0),   ;
          USD_PRICE WITH p_usd, ;
          SAL_PRICE WITH p_rub

  IF code_w # 0
    IF .NOT. EMPTY(SC03698.NOTE)
      DO View_Nt WITH "SC03698"
    ENDIF
  ENDIF

  IF LT03698.IS_USD
    sum_w = sum_w+QNT*USD_PRICE
  ELSE
    sum_w = sum_w+QNT*SAL_PRICE
  ENDIF
  SELECT ORD_S
ENDSCAN
USE
SELECT LD03698
USE
SELECT LT03698
REPLACE R_TOTAL WITH sum_w
USE
SELECT SC03698
USE
SELECT SN03698
USE
SELECT (s_sav)
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура T_Firm       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Проверка наличия предприятия.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04/13/1999 ──┘
PROCEDURE T_Firm

IF ex = 2
  RETURN .T.
ENDIF
IF EMPTY(f_c000)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали предприятие!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
