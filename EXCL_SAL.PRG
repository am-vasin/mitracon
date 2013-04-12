*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Excl_Sal     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Скидки, спеццены, возвраты.                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 27.01.2000 ══╝
PROCEDURE Excl_Sal
PRIVATE d0, d1, delta_r, delta_d

STORE DATE() TO d0, d1

delta_r = 0.01  && Допустимое (с точки зрения программы) 
delta_d = 0.01  && отклонение цены продажи от прайсовой цены 

IF .NOT. G_Period(d0, d1)
  RETURN
ENDIF

DO Wt_Mess WITH "Подготовка данных..."
DO Prp_Tmp
DO Wt_Mess
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"За указанный период нештатных продаж не было."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  DO View_
ENDIF

USE
DELETE FILE (tmpo_path+"EXCL_SAL.DBF")
DELETE FILE (tmpo_path+"EXCL_SAL.CDX")

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_Tmp      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка списка накладных.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.01.2000 ──┘
PROCEDURE Prp_Tmp

DELETE FILE (tmpo_path+"EXCL_SAL.DBF")
DELETE FILE (tmpo_path+"EXCL_SAL.CDX")
CREATE DBF (tmpo_path+"EXCL_SAL")  ;
  ( DOC_VIEW C(11),   ;
    DOC_DATE D,       ;
    DOC_NUM  C(10),   ;
    FIRM     C( 3),   ;
    SUM_D_L  N(12,4), ;
    SUM_R_L  N(12,2), ;
    SUM_D    N(12,4), ;
    SUM_R    N(12,2), ;
    SAL_CODE N( 3),   ;
    SAL_NAME C(30),   ;
    CUS_CODE N( 8),   ;
    CUS_NAME C(50),   ;
    NOTE     C(16),   ;
    IS_DIS   L,       ;
    IS_SPEC  L,       ;
    IS_REV   L        )
INDEX ON SAL_NAME TAG SAL_NAME UNIQUE
SET ORDER TO

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_0127 AGAIN

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS DET_0127 AGAIN

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS CUS_0127 AGAIN

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON ALIAS TIT_0127 AGAIN

PRIVATE is_near, sw_dis, sw_spec, sw_rev, s0d, s0r, sd, sr

is_near = SET("NEAR") = "OFF"
SET NEAR ON
SEEK DTOS(d0)
IF is_near
  SET NEAR OFF
ENDIF

SCAN REST WHILE DOC_DATE <= d1 FOR CUS_CODE >= 0
  STORE .F. TO sw_dis, sw_spec, sw_rev
  STORE 0 TO s0d, s0r, sd, sr
  SELECT DET_0127
  SEEK TIT_0127.FIRM+LEFT(DTOS(TIT_0127.DOC_DATE),4)+TIT_0127.DOC_NUM
  SCAN REST WHILE TIT_0127.FIRM = FIRM .AND. TIT_0127.DOC_DATE = DOC_DATE .AND. ;
                  TIT_0127.DOC_NUM = DOC_NUM
    IF QNT < 0
      sw_rev = .T.
    ENDIF
    IF ABS(PRICE_R_L - PRICE_R) > delta_r .OR. ABS(PRICE_D_L - PRICE_D) > delta_d
      IF PRICE_TYPE = "S"
        sw_spec = .T.
      ELSE
        sw_dis = .T.
      ENDIF
    ENDIF
    s0d = s0d + QNT*PRICE_D_L
    s0r = s0r + QNT*PRICE_R_L
    sd  = sd  + QNT*PRICE_D
    sr  = sr  + QNT*PRICE_R
  ENDSCAN
  IF sw_spec .OR. sw_dis .OR. sw_rev
    = SEEK(TIT_0127.CUS_CODE, "CUS_0127")
    = SEEK(TIT_0127.WHO, "P_0127")
    SELECT EXCL_SAL
    APPEND BLANK
    REPLACE DOC_VIEW WITH LEFT(TIT_0127.DOC_NUM,4)+"-"+ ;
                          ALLTRIM(SUBSTR(TIT_0127.DOC_NUM,5)), ;
            DOC_DATE WITH TIT_0127.DOC_DATE, ;
            DOC_NUM  WITH TIT_0127.DOC_NUM,  ;
            FIRM     WITH TIT_0127.FIRM,     ;
            SUM_D_L  WITH s0d, ;
            SUM_R_L  WITH s0r, ;
            SUM_D    WITH sd,  ;
            SUM_R    WITH sr,  ;
            SAL_CODE WITH TIT_0127.WHO,     ;
            SAL_NAME WITH ALLTRIM(P_0127.FAMILY)+" "+ ;
                          LEFT(P_0127.NAME,1)+" "+ ;
                          LEFT(P_0127.S_NAME,1),   ;
            CUS_CODE WITH TIT_0127.CUS_CODE,       ;
            CUS_NAME WITH IIF(FOUND("CUS_0127"), CUS_0127.CUS_NAME,  ;
                              TIT_0127.CUS_NAME),  ;
            NOTE     WITH TIT_0127.NOTE,   ;
            IS_DIS   WITH sw_dis,          ;
            IS_SPEC  WITH sw_spec,         ;
            IS_REV   WITH sw_rev
  ENDIF
  SELECT TIT_0127
ENDSCAN

USE

SELECT DET_0127
USE

SELECT CUS_0127
USE

SELECT P_0127
USE

SELECT EXCL_SAL

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Просмотр списка накладных.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.01.2000 ──┘
PROCEDURE View_

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
PRIVATE vlt_d, flt_t, flt_s
vlt_d = .T.
STORE 0 TO flt_t, flt_s
SELECT 0
DO Use_Dummy
SELECT EXCL_SAL
SET ORDER TO SAL_NAME
PRIVATE sal_list, sal_code, n
n = 1
DIMENSION sal_list(n), sal_code(n)
sal_list(n) = PADR("Все продавцы", 30)
sal_code(n) = 0
SCAN
  n = n+1
  DIMENSION sal_list(n), sal_code(n)
  sal_list(n) = SAL_NAME
  sal_code(n) = SAL_CODE
ENDSCAN
SET ORDER TO
GO TOP
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD View_N OF (menu_name) PROMPT "Просмотр" KEY Ctrl-A, "Enter"
ON SELECTION PAD View_N OF (menu_name) DO Sw_Mode WITH "View_N"

DEFINE PAD Print OF (menu_name) PROMPT "Печать" KEY Ctrl-A, "F1"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Filter OF (menu_name) PROMPT "Фильтр" KEY Ctrl-A, "F6"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Money OF (menu_name) PROMPT "Валюта" KEY Ctrl-A, "F2"
ON SELECTION PAD Money OF (menu_name) DO Sw_Mode WITH "Money"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
*┌──Дата─────Фрм──N док-та────Сумма прайс──Сумма прод──Спец─Скид─Возв┐
*│ДД.ММ.ГГГГ ФРМ A000-000000 999999999.99 999999999.99 Спец Скид Возв│
*└───────────────────────────────────────────────────────────────────┘
*┌───────────────────────────────────────────────────────────────────┐
*│   Покупатель ....:....!....:....!....:....!....:....!....:....!   │
*│  Продавец ....:....!....:....!....:....! Примеч. ....:....!....:  │
*└───────────────────────────────────────────────────────────────────┘
PRIVATE ln, wd
ln = MIN(RECCOUNT(),WROWS("")-11)   && Количество видимых строк BROWSE
wd = 67

DO D_Wins WITH ln, wd, "Список нештатных продаж за период с "+   ;
               DTOC(d0)+" по "+DTOC(d1), 1, 2
@ 2, 2 SAY "──Дата─────Фрм──N док-та────Сумма прайс──Сумма прод──Спец─Скид─Возв"
@ WROWS()-3,  5 SAY "Покупатель"
@ WROWS()-2,  4 SAY "Продавец"
@ WROWS()-2, 44 SAY "Примеч."
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 2 SAY "Валюта "+IIF(vlt_d, "У Е", "Руб")+ ;
       IIF(flt_t # 0 .OR. flt_s # 0, "    Фильтр установлен", SPACE(25))
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1    DO Sw_Mode WITH "Print"
    ON KEY LABEL F6    DO Sw_Mode WITH "Filter"
    ON KEY LABEL F2    DO Sw_Mode WITH "Money"

*
*    BROWSE - меню
*
    IF vlt_d
      BROWSE FIELDS DOC_DATE:H="",    ;
                    FIRM:3:H="",      ;
                    DOC_VIEW:11:H="", ;
                    SUM_D_L:H="":P="9999999.9999", ;
                    SUM_D:H="":P="9999999.9999",   ;
                    F001 = IIF(IS_SPEC, " √  ", "    "):4:H="", ;
                    F002 = IIF(IS_DIS, " √  ", "    "):4:H="",  ;
                    F003 = IIF(IS_REV, " √  ", "    "):4:H="",  ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Foot()    ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS DOC_DATE:H="",    ;
                    FIRM:3:H="",      ;
                    DOC_VIEW:11:H="", ;
                    SUM_R_L:H="":P="999999999.99", ;
                    SUM_R:H="":P="999999999.99",   ;
                    F001 = IIF(IS_SPEC, " √  ", "    "):4:H="", ;
                    F002 = IIF(IS_DIS, " √  ", "    "):4:H="",  ;
                    F003 = IIF(IS_REV, " √  ", "    "):4:H="",  ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Foot()    ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "View_N")
    ENDIF

  CASE what_do = "View_N"    && Просмотр списка

    DO Prep_Inv
    DO View_Inv
    what_do = "List"

  CASE what_do = "Print"    && Печать накладной

    DO Prn_Inv WITH FIRM, DOC_NUM, DOC_DATE
    what_do = "List"

  CASE what_do = "Filter"    && Фильтр

    DO Set_flt
    what_do = "List"

  CASE what_do = "Money"    && Валюта

    vlt_d = .NOT. vlt_d
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
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-3, 16 SAY CUS_NAME
@ WROWS()-2, 13 SAY SAL_NAME
@ WROWS()-2, 52 SAY NOTE

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_Flt      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Установка фильтра.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.02.2000 ──┘
PROCEDURE Set_Flt

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

*┌─────────────────────────────────────────────────────┐
*│               ┌──────────────────────────────┐      │
*│      Продавец │....:....!....:....!....:....!│      │
*│               └──────────────────────────────┘      │
*│ () Весь список  () Скидки  () Спеццены  () Возвраты │
*│                                                     │
*│           < OK Ctrl-W > < Отказаться Esc >          │
*└─────────────────────────────────────────────────────┘

PRIVATE ex, p1, p2
ex = 1
p1 = ASCAN(sal_code, flt_s)
p2 = flt_t + 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 57, "Фильтр"
@ 3, 8 SAY "Продавец"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 17 GET p1 PICTURE "@^" FROM sal_list SIZE 1, 30
@ 5,  3 GET p2 PICTURE "@*RH Весь список;Скидки;Спеццены;Возвраты"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  flt_t = p2 - 1
  flt_s = sal_code(p1)
  DO CASE
  CASE flt_t = 1
    ex = "IS_DIS"
  CASE flt_t = 2
    ex = "IS_SPEC"
  CASE flt_t = 3
    ex = "IS_REV"
  OTHERWISE
    ex = ""
  ENDCASE
  IF flt_s # 0
    ex = IIF(EMPTY(ex), "", ex +" .AND.")
    ex = ex + " SAL_CODE = "+ALLTRIM(STR(flt_s))
  ENDIF
  SET FILTER TO &ex
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Inv     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│         Подготовка файла для просмотра содержимого накладной.          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.02.2000 ──┘
PROCEDURE Prep_Inv

PRIVATE s_sav
s_sav = SELECT()
SELECT 0
DELETE FILE (tmpo_path+"TMP_INV.DBF")
DELETE FILE (tmpo_path+"TMP_INV.CDX")
CREATE DBF (tmpo_path+"TMP_INV")  ;
  ( CODE      N( 7), ;
    PREFIX    C( 4), ;
    NAME      C(35), ;
    PRODUCER  C( 8), ;
    QNT       N( 6), ;
    PRICE_D_L N(12,4), ;
    PRICE_R_L N(12,2),;
    PRICE_D   N(12,4), ;
    PRICE_R   N(12,2),;
    IS_DIS    L,     ;
    IS_SPEC   L  )
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
INDEX ON CODE TAG CODE

SELECT 0
USE (base_path+"STOCK") ALIAS ST_0202 ORDER TAG CODE AGAIN

SELECT 0
USE (base_path+"SALE") ALIAS SL_0202 ORDER TAG DOC_NUM AGAIN
SET RELATION TO CODE INTO ST_0202

SEEK EXCL_SAL.FIRM+LEFT(DTOS(EXCL_SAL.DOC_DATE),4)+EXCL_SAL.DOC_NUM
SCAN REST WHILE DOC_NUM = EXCL_SAL.DOC_NUM .AND. ;
                DOC_DATE = EXCL_SAL.DOC_DATE .AND. ;
                FIRM = EXCL_SAL.FIRM
  SELECT TMP_INV
  IF .NOT. SEEK(SL_0202.CODE)
    APPEND BLANK
    REPLACE CODE      WITH SL_0202.CODE,     ;
            PREFIX    WITH ST_0202.PREFIX,   ;
            NAME      WITH ST_0202.NAME,     ;
            PRODUCER  WITH ST_0202.PRODUCER, ;
            QNT       WITH SL_0202.QNT,      ;
            PRICE_D_L WITH SL_0202.PRICE_D_L,;
            PRICE_R_L WITH SL_0202.PRICE_R_L,;
            PRICE_D   WITH SL_0202.PRICE_D,  ;
            PRICE_R   WITH SL_0202.PRICE_R,  ;
            IS_DIS    WITH SL_0202.PRICE_TYPE = "D", ;
            IS_SPEC   WITH SL_0202.PRICE_TYPE = "S"
  ELSE
    REPLACE QNT WITH QNT+SL_0202.QNT
  ENDIF
  SELECT SL_0202
ENDSCAN

USE
SELECT ST_0202
USE

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_Inv     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Просмотр накладной.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.02.2000 ──┘
PROCEDURE View_Inv

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
SELECT TMP_INV
SET ORDER TO TAG NAME
GO TOP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Money OF (menu_name) PROMPT "Валюта" KEY Ctrl-A, "F2"
ON SELECTION PAD Money OF (menu_name) DO Sw_Mode WITH "Money"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
*┌───Наименование─────────────────────────Кол-во─────Ц.прод────Ц.прайс─Сп─Ск┐
*│XXXX XXXXXXXXXXXXXXXXXXXXXXXXX XXXXXXXX 999999 9999999999 9999999999 !  ! │
*└──────────────────────────────────────────────────────────────────────────┘
PRIVATE ln, wd, tit
tit = "Накладная N "+ALLTRIM(EXCL_SAL.DOC_VIEW)+" от "+DTOC(EXCL_SAL.DOC_DATE)
ln = MIN(WROWS("")-11, RECCOUNT())   && Количество видимых строк BROWSE

wd = 74

DO D_Wins WITH ln, wd, tit, 1, 3
@ 2, 2 SAY "───Наименование─────────────────────────Кол-во─────Ц.прод────Ц.прайс─Сп─Ск"
@ WROWS()-3, 3 SAY "Покупатель "+EXCL_SAL.CUS_NAME
@ WROWS()-2, 3 SAY "Продавец   "+EXCL_SAL.SAL_NAME
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 2 SAY "Валюта "+IIF(vlt_d, "У Е", "Руб")
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F2    DO Sw_Mode WITH "Money"

*
*    BROWSE - меню
*
    IF vlt_d
      BROWSE FIELDS PREFIX:H="",   ;
                    Brw_Name=Nm_Part(NAME):25:H="",     ;
                    PRODUCER:H="", ;
                    QNT:H="":P="999999", ;
                    PRICE_D:H="":P="99999.9999", ;
                    PRICE_D_L:H="":P="99999.9999", ;
                    MARK1 = IIF(IS_SPEC, "√ ", "  "):2:H="",  ;
                    MARK2 = IIF(IS_DIS, "√ ", "  "):2:H="",  ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Ft()    ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ELSE
      BROWSE FIELDS PREFIX:H="",   ;
                    Brw_Name=Nm_Part(NAME):25:H="",     ;
                    PRODUCER:H="", ;
                    QNT:H="":P="999999", ;
                    P_SL = ROUND(PRICE_R, 2):P="99999.99", ;
                    P_PR = ROUND(PRICE_R_L, 2):P="99999.99", ;
                    MARK1 = IIF(IS_SPEC, "√ ", "  "):2:H="",  ;
                    MARK2 = IIF(IS_DIS, "√ ", "  "):2:H="",  ;
             DUMMY.F:H="" FREEZE DUMMY.F ;
             WHEN Draw_Ft()    ;
             NOMODIFY   ;
             NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
             WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ENDIF
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "List")
    ENDIF

  CASE what_do = "Money"    && Просмотр списка

    vlt_d = .NOT. vlt_d
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
DELETE FILE (tmpo_path+"TMP_INV.DBF")
DELETE FILE (tmpo_path+"TMP_INV.CDX")

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Ft

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-4, 3 SAY PREFIX+" "+NAME+" "+PRODUCER

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
