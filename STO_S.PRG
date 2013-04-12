*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Sto_S        Разработчик Андрей Васин           25.05.98 15:52:42 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║            Передача номенклатуры в филиал (или еще куда...).           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Sto_S
PARAMETER f_code, _s_usd0, _s_rub0, _abs_d0, _abs_r0, _abs_t0, ;
          _s_qnt0, path0
   && Код филиала или 0, если без филиала, если 
   && параметр отсутствует, то спросить...
PRIVATE ask_f, is_f, code_wk, name_w, s_sav, __abs_r, __abs_d, __abs_t
__abs_r = .F.
IF TYPE("_abs_r0") = "N"
  __abs_r = _abs_r0 = 1
ENDIF

__abs_d = .F.
IF TYPE("_abs_d0") = "N"
  __abs_d = _abs_d0 = 1
ENDIF
__abs_t = 0
IF TYPE("_abs_r0") = "N"
  __abs_t = _abs_t0
ENDIF
IF TYPE("f_code") = "N"
  ask_f = .F.
  is_f  = f_code # 0
  code_wk = f_code
ELSE
  ask_f = .T.
  is_f  = .F.
  code_wk = 0
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
PRIVATE c_list, n_list, l_list
s_sav = SELECT()
SELECT 0
USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE ALIAS TIT25598
l_list = 0
name_w = "                "
SCAN
  l_list = l_list + 1
  DIMENSION c_list(l_list), n_list(l_list)
  c_list(l_list) = CUS_CODE
  n_list(l_list) = CUS_NAME
  IF code_wk = CUS_CODE
    name_w = CUS_NAME
  ENDIF
ENDSCAN
IF l_list = 0    && Нет списков? Так тому и быть!
  ask_f = .F.
  is_f  = .F.
  code_wk = 0
ENDIF
IF EMPTY(name_w)
  is_f = .F.
  code_wk = 0
ENDIF

*┌─────────────────────────────────────────────┐
*│ ┌─ Филиал ────────────────────────────────┐ │
*│ │       [ ] Есть   ....:....!....:.       │ │
*│ └─────────────────────────────────────────┘ │
*│ ┌─ Цены ──────────────────────────────────┐ │
*│ │     [ ] Долларовые     [ ] Рублевые     │ │
*│ └─────────────────────────────────────────┘ │
*│ ┌─ Цены на отсуств. позиции ──────────────┐ │
*│ │     [ ] Долларовые     [ ] Рублевые     │ │
*│ │       ( ) Оптовые  ( ) Розничные        │ │
*│ └─────────────────────────────────────────┘ │
*│ ┌─ Наличие ───────────────────────────────┐ │
*│ │ ( ) Количество  ( ) Пометки  ( ) Ничего │ │
*│ └─────────────────────────────────────────┘ │
*│ ┌─ Номенклатура ──────────────────────────┐ │
*│ │   [ ] Фильтр по разделам (  Снят    )   │ │
*│ └─────────────────────────────────────────┘ │
*│      < OK Ctrl-W > < Отказаться Esc >       │
*└─────────────────────────────────────────────┘

PRIVATE ex, __s_usd, __s_rub, __s_qnt, __s_div, ch_div, div_lst, div_cod
*
*  Разделы номенклатуры
*

__s_div = ""   && Все разделы
SELECT 0
USE (base_path+"DIVISION") ALIAS DIV28598 AGAIN
ex = 0
SCAN
  ex = ex+1
  DIMENSION div_lst(ex), div_cod(ex)
  div_lst(ex) = "√"+B_NAME
  div_cod(ex) = CODE
ENDSCAN
USE
__s_usd = .F.
IF TYPE("_s_usd0") = "N"
  __s_usd = _s_usd0 = 1
ENDIF
__s_rub = .F.
IF TYPE("_s_rub0") = "N"
  __s_rub = _s_rub0 = 1
ENDIF
__s_qnt = 2
IF TYPE("_s_qnt0") = "N"
  __s_qnt = _s_qnt0
ENDIF
ch_div = .F.
ex   = 1

*------------------------------------------------------------------------

IF TYPE("path0") # "C"
  PUSH KEY CLEAR       && На всякий пожарный случай!
  DO Prp_Nav_2
  DO D_Win_N WITH 20, 49, "Передача номенклатуры и наличия"
  DO Sun_Bord WITH  2, 3,  4, 45, " Филиал "
  DO Sun_Bord WITH  5, 3,  7, 45, " Цены "
  DO Sun_Bord WITH  8, 3, 11, 45, " Цены на отсуств. позиции "
  DO Sun_Bord WITH 12, 3, 14, 45, " Наличие "
  DO Sun_Bord WITH 15, 3, 17, 45, " Номенклатура "
  @ 16, 30 SAY "(  Снят    )"
  @ 3, 22 SAY name_w
  *------------------------------------------------------------------------
  *      Ввод полей бланка
  *
  @  3, 11 GET is_f PICTURE "@*C Есть" WHEN ask_f VALID Ch_Fil()
  @  6,  9 GET __s_usd PICTURE "@*C Долларовые"
  @  6, 28 GET __s_rub PICTURE "@*C Рублевые"
  @  9,  9 GET __abs_d PICTURE "@*C Долларовые"
  @  9, 28 GET __abs_r PICTURE "@*C Рублевые"
  @ 10, 11 GET __abs_t  PICTURE "@*RH Оптовые ;Розничные"
  @ 13,  5 GET __s_qnt PICTURE "@*RH Количество ;Пометки ;Ничего"
  @ 16,  7 GET ch_div  PICTURE "@*C Фильтр по разделам" VALID Ch_Div()
  @ 18,  8 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

  READ CYCLE
ENDIF

IF ex = 1
  DO Send
ENDIF
*--------------------------------------------------------------------------

IF TYPE("path0") # "C"
  POP KEY
  RELEASE WINDOW (win_name)
ENDIF
SELECT TIT25598
USE
SELECT (s_sav)

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Перечень разделов.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 26.05.98 13:56:22 ═╝
PROCEDURE Ch_Div
*
*  Переменные состояния для навигации
*
PRIVATE i
IF .NOT. ch_div
  RETURN .T.
ENDIF

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

PRIVATE point, sw, arr_w
DIMENSION arr_w(ALEN(div_lst))
FOR i = 1 TO ALEN(div_lst)
  arr_w(i) = div_lst(i)
ENDFOR
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH LEN(arr_w(1))+4, MIN(ALEN(arr_w)+3, 15), "Разделы"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
point = 1
DO WHILE .T.
  point = MAX(1, point)
  point = MIN(point, ALEN(arr_w))
  @ 1, 1 MENU arr_w, ALEN(arr_w), WROWS()-3
  READ MENU TO point
  IF READKEY() = 15
    IF LASTKEY() = 13
      point = 1
      EXIT
    ENDIF
    IF LEFT(arr_w(point),1) = " "
      arr_w(point) = "√"+SUBSTR(arr_w(point), 2)
    ELSE
      arr_w(point) = " "+SUBSTR(arr_w(point), 2)
    ENDIF
    point = point+1
    LOOP
  ENDIF
  IF READKEY() = 12
    point = 0
    EXIT
  ENDIF
ENDDO  

IF point = 1
  __s_div = "!"
  sw = .T.
  FOR i = 1 TO ALEN(arr_w)
    IF LEFT(arr_w(i),1) = " "
      sw = .F.
    ELSE
      __s_div = __s_div+ALLTRIM(STR(div_cod(i)))+"!"
    ENDIF
  ENDFOR
  IF sw
    __s_div = ""
  ENDIF
  FOR i = 1 TO ALEN(div_lst)
    div_lst(i) = arr_w(i)
  ENDFOR
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
ch_div = .F.
SHOW GET ch_div
@ 16, 30 SAY IIF(EMPTY(__s_div), "(  Снят    )", "(Установлен)")
RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Перечень филиалов.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 26.05.98 13:56:22 ═╝
PROCEDURE Ch_Fil
*
*  Переменные состояния для навигации
*
PRIVATE i
IF .NOT. is_f
  name_w = SPACE(LEN(n_list(1)))
  code_wk = 0
  @ 3, 22 SAY name_w
  RETURN .T.
ENDIF

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

PRIVATE point, sw
*------------------------------------------------------------------------

point = MAX(1, ASCAN(c_list,code_wk))

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH MIN(ALEN(n_list)+3, 15), LEN(n_list(1))+4, "Филиалы"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
point = 1
DO WHILE .T.
  point = MAX(1, point)
  point = MIN(point, ALEN(n_list))
  @ 1, 1 MENU n_list, ALEN(n_list), WROWS()-3
  READ MENU TO point
  IF READKEY() = 15
    name_w = n_list(point)
    code_wk = c_list(point)
    EXIT
  ENDIF
  IF READKEY() = 12
    is_f = .F.
    SHOW GET is_f
    EXIT
  ENDIF
ENDDO  

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
@ 3, 22 SAY name_w

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║            Непосредственно подготовка и пересылка наличия.             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 28.05.98 17:36:40 ═╝
PROCEDURE Send

PRIVATE cnt, pr_d, pr_r, pr_df, pr_rf, q_f, tmpw_path

IF TYPE("path0") = "C"
  tmpw_path = path0
ELSE
  tmpw_path = tmpo_path
ENDIF
USE (base_path+"RYN_L") ALIAS RL28598 AGAIN ORDER TAG C_CODE
SELECT 0
DELETE FILE (tmpw_path+"STO_S.DBF")
DELETE FILE (tmpw_path+"STO_S.FPT")
CREATE DBF (tmpw_path+"STO_S")  ;
  ( CODE      N( 7), ;
    PRO_CODE  N( 5), ;
    PRODUCER  C( 8), ;
    PREFIX    C( 4), ;
    NAME      C(35), ;
    NUM_PART  C(35), ;
    DIV_CODE  N( 3), ;
    CLASS     C(12), ;
    QNT       N( 6), ;
    QNT_STORE N( 6), ;
    PRICE_B   N(10,4),;
    PRICE_B_R N(10,2),;
    PRICE     N(10,4),;
    PRICE_R   N(10,2),;
    QNT_ROUND N( 6), ;
    STOP_PUR  C( 1), ;
    DATE_ON   D,     ;
    DATE_OFF  D,     ;
    NOTE      M      )
SELECT 0
USE (base_path+"STOCK") ALIAS ST28598 AGAIN
IF code_wk # 0
  SET RELATION TO STR(code_wk,5)+STR(CODE,7) INTO RL28598
ENDIF

cnt = 0
IF TYPE("path0") # "C"
  DO Wt_Mess WITH "Подготовка номенклатуры."
ENDIF
SCAN
  IF TYPE("path0") # "C"
    cnt = cnt+1
    @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)), WCOLS())
  ENDIF
  IF .NOT. EMPTY(__s_div)
    IF EMPTY(code_wk)
      IF .NOT. ("!"+ALLTRIM(STR(ST28598.DIV_CODE))+"!" $ __s_div)
        LOOP
      ENDIF
    ELSE
      IF .NOT. ("!"+ALLTRIM(STR(ST28598.DIV_CODE))+"!" $ __s_div) .AND. ;
         .NOT. FOUND("RL28598")
        LOOP
      ENDIF
    ENDIF
  ENDIF
  pr_d  = IIF(__s_usd, PRICE, 0)
  pr_r  = IIF(__s_rub, PRICE_R, 0)
  pr_df = 0
  pr_rf = 0
  q_f   = 0
  IF code_wk # 0
    IF FOUND("RL28598")
      pr_df = RL28598.PRICE_D
      pr_rf = RL28598.PRICE_R
      q_f   = RL28598.QNT
    ENDIF
  ENDIF
  IF .NOT. FOUND("RL28598")
    IF __abs_d .OR. __abs_r
      IF __abs_t = 1
        pr_df = ST28598.PRICE
      ELSE
        pr_df = RetPrice(ST28598.NAME, ST28598.PRICE, EMPTY(ST28598.STOP_PUR), IIF(EMPTY(ST28598.CLASS), ST28598.DIV_CODE, ST28598.CLASS))
      ENDIF
      IF __abs_r
        pr_rf = Clc_Rub(pr_df)
      ENDIF
      IF .NOT. __abs_d
        pr_df = 0
      ENDIF
    ENDIF
  ENDIF
  q = 0
  IF __s_qnt = 1
    q = QNT
  ELSE
    IF __s_qnt = 2
      q = IIF(QNT > 0, -99999, 0)
    ENDIF
  ENDIF
  SELECT STO_S
  APPEND BLANK
  REPLACE CODE      WITH ST28598.CODE,     ;
          PRO_CODE  WITH ST28598.PRO_CODE, ;
          PRODUCER  WITH ST28598.PRODUCER, ;
          PREFIX    WITH ST28598.PREFIX,   ;
          NAME      WITH ST28598.NAME,     ;
          NUM_PART  WITH ST28598.NUM_PART, ;
          CLASS     WITH ST28598.CLASS,    ;
          DIV_CODE  WITH ST28598.DIV_CODE, ;
          QNT       WITH q_f,              ;
          QNT_STORE WITH q,                ;
          PRICE_B   WITH pr_df,            ;
          PRICE_B_R WITH pr_rf,            ;
          PRICE     WITH pr_d,             ;
          PRICE_R   WITH pr_r,             ;
          QNT_ROUND WITH ST28598.QNT_ROUND,;
          STOP_PUR  WITH IIF(FSIZE("STOP_PUR","ST28598") = 0,STOP_PUR,ST28598.STOP_PUR),;
          DATE_ON   WITH ST28598.DATE_ON,  ;
          DATE_OFF  WITH ST28598.DATE_OFF, ;
          NOTE      WITH ST28598.NOTE
  SELECT ST28598
ENDSCAN
IF TYPE("path0") # "C"
  DO Wt_Mess
ENDIF
USE
SELECT STO_S
USE
SELECT RL28598
USE
PRIVATE pth, mss, i000, sz
sz = File_Size(tmpw_path+"STO_S.DBF")+  ;
     File_Size(tmpw_path+"DIVISION.DBF")+  ;
     File_Size(tmpw_path+"DEVELOP.DBF")
DO WHILE .T. .AND. TYPE("path0") # "C"
  pth = "\"
  pth = Int_Dir(pth, "Выберите директорию", sz)
  IF EMPTY(pth)
    EXIT
  ENDIF
  IF File_O(pth+"STO_S.DBF") .OR. ;
     File_O(pth+"STO_S_T.DBF") .OR. ;
     File_O(pth+"DIV_S.DBF") .OR. ;
     File_O(pth+"CLASSES.DBF") .OR. ;
     File_O(pth+"DEV_S.DBF")
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"А там уже есть файлы с номенклатурой для филиала! Что будем делать?"
    mss(3) = CHR(0)+"   [ Заменим файлы ] [ Сменим директорию ] [ Откажемся ]"
    mss(4) = ""
    i000 = Out_Mess(7, "mss")
    IF i000 = 3
      EXIT
    ENDIF
    IF i000 = 2
      LOOP
    ENDIF
  ENDIF
  DO Wt_Mess WITH "Запись..."
  DELETE FILE (pth+"STO_S.DBF")
  DELETE FILE (pth+"STO_S.FPT")
  COPY FILE (tmpw_path+"STO_S.DBF") TO (pth+"STO_S.DBF")
  COPY FILE (tmpw_path+"STO_S.FPT") TO (pth+"STO_S.FPT")
  USE (base_path+"DIVISION") ALIAS TMP29598
  DELETE FILE (pth+"DIV_S.DBF")
  COPY TO (pth+"DIV_S.DBF")
  USE (base_path+"DEVELOP") ALIAS TMP29598
  DELETE FILE (pth+"DEV_S.DBF")
  COPY TO (pth+"DEV_S.DBF")
  USE (base_path+"CLASSES") ALIAS TMP29598
  DELETE FILE (pth+"CLASSES.DBF")
  COPY TO (pth+"CLASSES.DBF")
  USE
  IF code_wk # 0
    SELECT TIT25598
    SEEK code_wk
    DELETE FILE (pth+"STO_S_T.DBF")
    COPY NEXT 1 TO (pth+"STO_S_T.DBF")
  ENDIF
  DO Wt_Mess
  EXIT
ENDDO
IF TYPE("path0") = "C"
  USE (base_path+"DIVISION") ALIAS TMP29598
  DELETE FILE (path0+"DIV_S.DBF")
  COPY TO (path0+"DIV_S.DBF")
  USE (base_path+"DEVELOP") ALIAS TMP29598
  DELETE FILE (path0+"DEV_S.DBF")
  COPY TO (path0+"DEV_S.DBF")
  USE (base_path+"CLASSES") ALIAS TMP29598
  DELETE FILE (path0+"CLASSES.DBF")
  COPY TO (path0+"CLASSES.DBF")
  USE
  IF code_wk # 0
    SELECT TIT25598
    SEEK code_wk
    DELETE FILE (path0+"STO_S_T.DBF")
    COPY NEXT 1 TO (path0+"STO_S_T.DBF")
  ENDIF
ENDIF

RETURN
