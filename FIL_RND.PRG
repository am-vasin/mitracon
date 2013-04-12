*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Fil_Rnd      Разработчик Андрей Васин           10.02.98 11:43:26 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                           Оборот по филиалу.                           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Fil_Rnd

USE (base_path+"CUSTOMER")
SET FILTER TO RYN # " "
DO Use_Dummy

DO WHILE .T.
  IF .NOT. Ch_Fil()
    EXIT
  ENDIF
  DO View_Rnd
ENDDO

CLOSE DATABASES

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                             Выбор филиала.                             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.02.98 11:46:57 ═╝
PROCEDURE Ch_Fil
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
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбор" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
SELECT CUSTOMER
PRIVATE ln, wd, ex
ln = 100   && Количество видимых строк BROWSE
wd = FSIZE("L_NAME")

ex = .F.

DO D_Wins WITH ln, wd, "Филиалы", 0, 0
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
    BROWSE FIELDS L_NAME,     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Просмотр списка

    ex = .T.
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN ex

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                           Просмотр оборота.                            ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.02.98 11:55:09 ═╝
PROCEDURE View_Rnd
PRIVATE sum_r, sum_d, mss, s_w_r, s_w_d, s_near, tot_d

IF USED("RYN_L_T")
  SELECT RYN_L_T
ELSE
  SELECT 0
  USE (base_path+"RYN_L_T") ORDER TAG CUS_CODE
ENDIF

IF .NOT. SEEK(CUSTOMER.CODE)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"За филиалом ничего не числится!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
s_near = SET("NEAR") = "OFF"
SET NEAR ON
SELECT 0
USE (base_path+"CURS_SET") ORDER TAG HRON DESCENDING ALIAS CURS99 AGAIN

IF USED("RYN_L")
  SELECT RYN_L
ELSE
  SELECT 0
  USE (base_path+"RYN_L") ORDER TAG CUS_CODE
ENDIF

DO Wt_Mess WITH "Расчет суммы"
SEEK CUSTOMER.CODE
IF RYN_L_T.VALUTA = "$"
  SUM REST WHILE CUS_CODE = CUSTOMER.CODE QNT*PRICE_D*kurs, QNT*PRICE_D TO sum_r, sum_d
ELSE
  SUM REST WHILE CUS_CODE = CUSTOMER.CODE QNT*PRICE_R, QNT*PRICE_R/kurs TO sum_r, sum_d
ENDIF
DO Wt_Mess

IF USED("FIL_RND")
  SELECT FIL_RND
  ZAP
ELSE
  SELECT 0
  CREATE DBF (tmpo_path+"FIL_RND") ;
     (  DOC_NUM   C(10), ;
        DOC_NUMV  C(11), ;
        DOC_DATE  D, ;
        DOC_SUM_D N(15,4), ;
        DOC_SUM_R N(15,2), ;
        REST_D    N(15,4), ;
        REST_R    N(15,2), ;
        PART_D    N(7,3),  ;
        PART_R    N(7,3)  )
  USE (tmpo_path+"FIL_RND") EXCLUSIVE
  INDEX ON DTOS(DOC_DATE) TAG HRON DESCENDING
ENDIF

IF USED("SALE_TIT")
  SELECT SALE_TIT
ELSE
  SELECT 0
  USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE
ENDIF

DO Wt_Mess WITH "Подготовка данных"
SEEK CUSTOMER.CODE
SCAN REST WHILE CUS_CODE = CUSTOMER.CODE
  = SEEK("CURS      "+DTOS(DOC_DATE), "CURS99")
  SELECT FIL_RND
  APPEND BLANK
  REPLACE DOC_NUM   WITH SALE_TIT.DOC_NUM, ;
          DOC_NUMV  WITH LEFT(SALE_TIT.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(SALE_TIT.DOC_NUM,5)), ;
          DOC_DATE  WITH SALE_TIT.DOC_DATE, ;
          DOC_SUM_R WITH SALE_TIT.TOTAL_R,  ;
          DOC_SUM_D WITH SALE_TIT.TOTAL_R/CURS99.CURS
  SELECT SALE_TIT
ENDSCAN

s_w_r = 0
s_w_d = 0
SELECT CURS99
USE
IF s_near
  SET NEAR OFF
ENDIF
SELECT FIL_RND
SCAN
  s_w_r = s_w_r+DOC_SUM_R
  s_w_d = s_w_d+DOC_SUM_D
  REPLACE REST_R WITH s_w_r, ;
          PART_R WITH s_w_r/sum_r, ;
          REST_D WITH s_w_d, ;
          PART_D WITH s_w_d/sum_d
ENDSCAN

GO TOP

DO Wt_Mess
IF RECCOUNT() = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одной продажи!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
PRIVATE is_usd
is_usd = .T.
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
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Ch_Vlt OF (menu_name) PROMPT "Уст. рубли" KEY Ctrl-A, "Enter"
ON SELECTION PAD Ch_Vlt OF (menu_name) DO Sw_Mode WITH "Ch_Vlt"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY")="ON", 62, 60)

DO D_Wins WITH ln, wd, ALLTRIM(CUSTOMER.L_NAME), 0, 1

@ 1, 2 SAY IIF(SET("CENTURY")="ON", "── Дата ───","─ Дата ──")+ ;
       "─ Документ ─Сумма по док-ту──────── Итого ──Коэфф."
@ WROWS()-2, 4 SAY "Сумма склада "+ALLTRIM(STR(sum_d,15,2))
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 0, WCOLS()-5 SAY IIF(is_usd, "У. Е.","Рубли") COLOR SCHEME 5
    @ WROWS()-2, 4 SAY "Сумма склада "+ALLTRIM(STR(IIF(is_usd, sum_d, sum_r),15,2))
    DO Prp_Nav_1

*
*    BROWSE - меню
*
    ON KEY LABEL Enter KEYBOARD CHR(23)
    IF is_usd
      BROWSE FIELDS DOC_DATE:H="", ;
                    DOC_NUMV:H="", ;
                    DOC_SUM_D:H="",  ;
                    REST_D:H="",     ;
                    PART_D:H="",     ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS DOC_DATE:H="", ;
                    DOC_NUMV:H="", ;
                    DOC_SUM_R:H="",  ;
                    REST_R:H="",     ;
                    PART_R:H="",     ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    what_do = IIF(READKEY() % 256 = 12, "Exit", "Ch_Vlt")

  CASE what_do = "Ch_Vlt"    && Смена валюты
    IF is_usd
      is_usd = .F.
      DEFINE PAD Ch_Vlt OF (menu_name) PROMPT "Уст. У. Е." KEY Ctrl-A, "Enter"
    ELSE
      is_usd = .T.
      DEFINE PAD Ch_Vlt OF (menu_name) PROMPT "Уст. рубли" KEY Ctrl-A, "Enter"
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN
