*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Fil_Pric     Разработчик Андрей Васин           18.06.98 10:20:08 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                        Установка цен для филиала.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Fil_Pric

PARAMETER f_code, p_type0, vlt0, mute
*
* f_code - Код филиала, если отсутствует - выбрать из списка.
* p_type0- Тип цены: 1 - не менять, 2 - оптовая, 3 - розничная
* vlt0   - Валюта: 1 - рубли, 2 - USD
* mute   - Подавить всякий вывод на экран
*
PRIVATE f_point, f_names, f_list, s_sav, n_fil, mss
PRIVATE tmpName1, tmpName2

s_sav = SELECT()

*
*      Формируем список филиалов.
*
SELECT 0
USE (base_path+"CUSTOMER") ALIAS CS18698 AGAIN
n_fil = 0
SCAN FOR RYN # " "
  n_fil = n_fil+1
  DIMENSION f_names(n_fil), f_list(n_fil)
  f_names(n_fil) = B_NAME
  f_list(n_fil)  = CODE
ENDSCAN
USE
IF n_fil = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одного филиала..."
  mss(3) = ""
  IF .NOT. mute
    DO Out_Mess WITH 7, "mss"
  ENDIF
  SELECT (s_sav)
  RETURN
ENDIF

*
*    Начальное значение для филиала
*
f_point = 1
IF TYPE("f_code") = "N"
  f_point = ASCAN(f_list,f_code)
  f_point = MAX(1, f_point)
ENDIF

*
*   Значения для формы
*
PRIVATE vlt, p_type, is_dis, dis_p, dis_prc, manual, ex, dis_1, dis_n, dis_q
vlt = 1
p_type = 2
IF TYPE("p_type0") = "N"
  p_type = p_type0
ENDIF

is_dis = .F.
dis_p = 1
manual = .F.
dis_prc = 0
ex = 1
*
*   Перечень списков-заготовок.
*
DIMENSION dis_1(1), dis_n(1)
dis_1(1) = 0
dis_n(1) = ""
dis_q = 0
IF File_O(base_path+"WK_TIT.DBF")
  USE (base_path+"WK_TIT") ALIAS WK18698 AGAIN
  SCAN
    dis_q = dis_q+1
    DIMENSION dis_1(dis_q), dis_n(dis_q)
    dis_1(dis_q) = RECNO()
    dis_n(dis_q) = L_NAME
  ENDSCAN
  USE
ENDIF
USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE ALIAS RT18698 AGAIN
IF SEEK(f_list(f_point))
  vlt = IIF(VALUTA="$", 2, 1)
ENDIF
IF TYPE("vlt0") = "N"
  vlt = vlt0
ENDIF
USE

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

*┌─────────────────────────────────────────────────┐
*│                   ┌────────────────┐            │
*│            Филиал │....:....!....:.│            │
*│                   └────────────────┘            │
*│ ┌─ Цена ──────────────────────────────────────┐ │
*│ │ ( ) Не менять   ( ) Оптовая   ( ) Розничная │ │
*│ └─────────────────────────────────────────────┘ │
*│ ┌─ Валюта ────────────────────────────────────┐ │
*│ │           ( ) Рубли   ( ) Доллары           │ │
*│ └─────────────────────────────────────────────┘ │
*│ ┌─ [ ] Скидка по списку ──────────────────────┐ │
*│ │ ┌─────────────────────────────────────────┐ │ │
*│ │ │....:....!....:....!....:....!....:....! │ │ │
*│ │ └─────────────────────────────────────────┘ │ │
*│ │            Процент скидки 999.99            │ │
*│ └─────────────────────────────────────────────┘ │
*│              [ ] Ручная коррекция               │
*│        < OK Ctrl-W > < Отказаться Esc >         │
*└─────────────────────────────────────────────────┘

IF .NOT. mute
  PUSH KEY CLEAR       && На всякий пожарный случай!
  DO Prp_Nav_2
  DO D_Win_N WITH 20, 53, "Установка цен для филиала"
  DO Sun_Bord WITH  5, 3,  7, 49, " Цена "
  DO Sun_Bord WITH  8, 3, 10, 49, " Валюта "
  DO Sun_Bord WITH 11, 3, 16, 49, "                      "
  @ 3, 14 SAY "Филиал"
*------------------------------------------------------------------------
  *      Ввод полей бланка
  *
  @  2, 21 GET f_point PICTURE "@^" FROM f_names SIZE 1, 18 WHEN TYPE("f_code") # "N"
  @  6,  5 GET p_type PICTURE "@*RH Не менять  ;Оптовая  ;Розничная"
  @  9, 15 GET vlt PICTURE "@*RH Рубли  ;Доллары"
  @ 11,  6 GET is_dis PICTURE "@*C Скидка по списку" WHEN dis_q # 0
  @ 12,  5 GET dis_p PICTURE "@^" FROM dis_n SIZE 1, 42 WHEN is_dis
  @ 15, 16 SAY "Процент скидки" GET dis_prc PICTURE "@Z 999.99" WHEN is_dis
  @ 17, 16 GET manual PICTURE "@*C Ручная коррекция"

  @ 18, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

  READ CYCLE
ENDIF
*--------------------------------------------------------------------------
IF ex = 1
  tmpName1 = SYS(3)
  PRIVATE p_d, p_r, tmp_p
  USE (base_path+"RYN_L") ALIAS RL18698 AGAIN ORDER TAG C_CODE
  SEEK STR(f_list(f_point),5)
  COPY REST WHILE CUS_CODE = f_list(f_point) TO (tmpo_path+tmpName1)
  IF p_type # 1
    IF .NOT. mute
      DO Wt_Mess WITH "Установка цен"
    ENDIF
    SELECT 0
    USE (base_path+"RYN_L_T") ALIAS RT18698 AGAIN ORDER TAG CUS_CODE
    IF SEEK(f_list(f_point))
      REPLACE DATE_P WITH DATE(), VALUTA WITH IIF(vlt = 1, "Р", "$")
    ENDIF
    USE (base_path+"STOCK") ALIAS ST18698 AGAIN ORDER TAG CODE
    SELECT RL18698
    SET RELATION TO CODE INTO ST18698
    SEEK STR(f_list(f_point),5)
    SCAN REST WHILE CUS_CODE = f_list(f_point)
      p_d = ST18698.PRICE
      p_r = IIF(own_val = "$", Clc_Rub(ST18698.PRICE), ST18698.PRICE_R)
      IF p_type = 3
        IF own_val = "$"
          p_d = RetPrice(ST18698.NAME, p_d, EMPTY(ST18698.STOP_PUR), IIF(EMPTY(ST18698.CLASS), ST18698.DIV_CODE, ST18698.CLASS))
          p_r = Clc_Rub(p_d)
        ELSE
          p_d = RetPrice(ST18698.NAME, ST18698.PRICE_R/kurs, EMPTY(ST18698.STOP_PUR), IIF(EMPTY(ST18698.CLASS), ST18698.DIV_CODE, ST18698.CLASS))
          p_r = Clc_Rub(p_d)
        ENDIF
      ENDIF
      REPLACE PRICE_R WITH p_r, PRICE_D WITH p_d
    ENDSCAN
    SELECT ST18698
    USE
    IF .NOT. mute
      DO Wt_Mess
    ENDIF
  ENDIF
  IF is_dis
    IF .NOT. mute
      DO Wt_Mess WITH "Установка скидки"
    ENDIF
    dis_prc = (100-dis_prc)/100
    SELECT 0
    USE (base_path+"WK_LIST") ALIAS WK18698 AGAIN ORDER TAG LIST_NO
    SEEK dis_1(dis_p)
    SCAN REST WHILE LIST_NO = dis_1(dis_p)
      IF SEEK(STR(f_list(f_point),5)+STR(CODE,7), "RL18698")
        REPLACE RL18698.PRICE_R WITH RL18698.PRICE_R*dis_prc, ;
                RL18698.PRICE_D WITH RL18698.PRICE_D*dis_prc
      ENDIF
    ENDSCAN
    USE
    IF .NOT. mute
      DO Wt_Mess
    ENDIF
  ENDIF
  IF manual
    DO Mnl_Prc WITH f_list(f_point)
  ENDIF
  SELECT 0
  USE (tmpo_path+tmpName1) ALIAS NEW_P
  tmp_p = "C:\"
  tmp_p = New_File(tmp_p, "DBF", "Позиции с новыми ценами")
  IF .NOT. EMPTY(tmp_p)
    DO New_P WITH tmp_p
  ENDIF
  SELECT RL18698
  USE
  SELECT NEW_P
  tmpName1 = DBF()
  USE
  DELETE FILE (tmpName1)
ENDIF

SELECT (s_sav)
IF .NOT. mute
  POP KEY
  RELEASE WINDOW (win_name)
ENDIF
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура New_P        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Файл с новыми ценами.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 19.02.2002 ──┘
PROCEDURE New_P
PARAMETERS f_name

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_2219
SELECT 0
IF vlt = 1
  CREATE DBF(f_name) ;
    ( CODE       N( 7), ;
      PREFIX     C(FSIZE("PREFIX", "STO_2219")),   ;
      NAME       C(FSIZE("NAME", "STO_2219")),     ;
      PRODUCER   C(FSIZE("PRODUCER", "STO_2219")), ;
      OLD_PRICE  N(FSIZE("PRICE_R", "NEW_P"), 2),  ;
      NEW_PRICE  N(FSIZE("PRICE_R", "NEW_P"), 2)   )
ELSE
  CREATE DBF(f_name) ;
    ( CODE       N( 7), ;
      PREFIX     C(FSIZE("PREFIX", "STO_2219")),   ;
      NAME       C(FSIZE("NAME", "STO_2219")),     ;
      PRODUCER   C(FSIZE("PRODUCER", "STO_2219")), ;
      OLD_PRICE  N(FSIZE("PRICE_D", "NEW_P"), 4),  ;
      NEW_PRICE  N(FSIZE("PRICE_D", "NEW_P"), 4)   )
ENDIF
USE (f_name) ALIAS TMP_001
SELECT RL18698
SET ORDER TO C_CODE
SELECT NEW_P

SCAN
  IF .NOT. SEEK(STR(f_list(f_point),5)+STR(CODE,7), "RL18698")
    LOOP
  ENDIF
  IF vlt = 1
    IF RL18698.PRICE_R = PRICE_R
      LOOP
    ENDIF
  ELSE
    IF RL18698.PRICE_D = PRICE_D
      LOOP
    ENDIF
  ENDIF
  = SEEK(CODE, "STO_2219")
  SELECT TMP_001
  APPEND BLANK
  REPLACE CODE       WITH STO_2219.CODE,     ;
          PREFIX     WITH STO_2219.PREFIX,   ;
          NAME       WITH STO_2219.NAME,     ;
          PRODUCER   WITH STO_2219.PRODUCER, ;
          OLD_PRICE  WITH IIF(vlt = 1, NEW_P.PRICE_R, NEW_P.PRICE_D), ;
          NEW_PRICE  WITH IIF(vlt = 1, RL18698.PRICE_R, RL18698.PRICE_D)
  SELECT NEW_P
ENDSCAN

SELECT TMP_001
USE
SELECT STO_2219
USE
RETURN
