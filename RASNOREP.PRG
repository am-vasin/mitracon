*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла RasnoRep     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                        Отчет по спецклиенту 10.                        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 26.10.1999 ══╝
PROCEDURE RasnoRep

PRIVATE d_0, d_1, spec_c
spec_c = -10 &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
d_1 = DATE()
d_0 = GOMONTH(d_1,-1)

IF .NOT. Get_Int()
  RETURN
ENDIF

DO Wt_Mess WITH "Подготовка данных..."
DO Prep_Tmp
DO Wt_Mess
SELECT TMP_CUS
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  DO View_C
ENDIF

SELECT TMP_CUS
USE
SELECT TMP_LIST
USE
DELETE FILE (tmpo_path+"TMP_CUS.DBF")
DELETE FILE (tmpo_path+"TMP_LIST.DBF")
DELETE FILE (tmpo_path+"TMP_LIST.CDX")
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Int      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Ввод интервала дат.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.10.1999 ──┘
PROCEDURE Get_Int
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
*    Разногласия с клиентами
*┌──────────────────────────────────────────┐
*│ ┌─ Интервал дат ───────────────────────┐ │
*│ │ С ДД.ММ.ГГГГ (F1) по ДД.ММ.ГГГГ (F1) │ │
*│ └──────────────────────────────────────┘ │
*│     < OK Ctrl-W > < Отказаться Esc >     │
*└──────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 46, "Разногласия с клиентами"
DO Sun_Bord WITH  2,  3,  4, 42, " Интервал дат "
@  3, 37 SAY "(F1)"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  5 SAY "С" GET d_0 PICTURE "@D" WHEN Set_F1(1) VALID Clr_F1()
@  3, 18 SAY "(F1) по" GET d_1 PICTURE "@D" WHEN Set_F1(2) VALID Clr_F1()
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_F1       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Установка обработчика F1.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.10.1999 ──┘
PROCEDURE Set_F1
PARAMETERS sw

IF sw = 1
  ON KEY LABEL F1 DO Set_C WITH 1
ELSE
  ON KEY LABEL F1 DO Set_C WITH 2
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_C        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Выбор даты по календарю.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.10.1999 ──┘
PROCEDURE Set_C
PARAMETERS sw

IF sw = 1
  d_0 = Get_Date(d_0)
  SHOW GET d_0
ELSE
  d_1 = Get_Date(d_1)
  SHOW GET d_1
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Clr_F1       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Очистка обработчика.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.10.1999 ──┘
PROCEDURE Clr_F1

ON KEY LABEL F1

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Tmp     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временных файлов.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.10.1999 ──┘
PROCEDURE Prep_Tmp

DELETE FILE (tmpo_path+"TMP_C.DBF")
DELETE FILE (tmpo_path+"TMP_L.DBF")

CREATE DBF (tmpo_path+"TMP_C.DBF") ;
  ( CUS_CODE  N( 8), ;
    CUS_NAME  C(50), ;
    SUM_D     N(15, 4))

SELECT 0
CREATE DBF (tmpo_path+"TMP_L.DBF") ;
  ( CUS_CODE  N( 8), ;
    CODE      N( 7), ;
    PREFIX    C( 4), ;
    NAME      C(35), ;
    PRODUCER  C( 8), ;
    QNT       N( 6), ;
    PRICE     N(10,4),;
    DOC_DATE  D,     ;
    DOC_FIRM  C( 3), ;
    DOC_NUM   C(10), ;
    REF_DATE  D,     ;
    REF_FIRM  C( 3), ;
    REF_NUM   C(10), ;
    WHO_SAL   C(28), ;
    WHO_CORR  C(28)  )

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE AGAIN ALIAS ST_9A26

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS WSAL_9A26

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS WCOR_9A26

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM AGAIN ALIAS REFT_9A26
SET RELATION TO WHO INTO WSAL_9A26

SELECT 0
USE (base_path+"SALE") ORDER TAG CODE AGAIN ALIAS REFD_9A26

SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM AGAIN ALIAS DOCD_9A26
SET RELATION TO CODE INTO ST_9A26

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE AGAIN ALIAS DOCT_9A26
SET RELATION TO WHO INTO WCOR_9A26

SEEK spec_c
SCAN REST WHILE CUS_CODE = spec_c FOR BETWEEN(DOC_DATE, d_0, d_1)
  IF .NOT. SEEK(LEFT(NEW_DOC,3)+LEFT(DTOS(REQ_DATE),4)+REQ_DOC, "REFT_9A26")
    LOOP
  ENDIF
  IF REFT_9A26.CUS_CODE = 0
    LOOP
  ENDIF
  SELECT REFD_9A26
  SET FILTER TO DOC_NUM  = REFT_9A26.DOC_NUM  .AND. ;
                DOC_DATE = REFT_9A26.DOC_DATE .AND. ;
                FIRM     = REFT_9A26.FIRM
  SELECT DOCD_9A26
  SEEK DOCT_9A26.FIRM+LEFT(DTOS(DOCT_9A26.DOC_DATE),4)+DOCT_9A26.DOC_NUM
  SCAN REST WHILE FIRM = DOCT_9A26.FIRM .AND. ;
                  DOC_NUM = DOCT_9A26.DOC_NUM .AND. ;
                  DOC_DATE = DOCT_9A26.DOC_DATE
    IF .NOT. SEEK(CODE, "REFD_9A26")
      LOOP
    ENDIF
    SELECT TMP_L
    APPEND BLANK
    REPLACE CUS_CODE  WITH REFT_9A26.CUS_CODE, ;
            CODE      WITH DOCD_9A26.CODE, ;
            PREFIX    WITH ST_9A26.PREFIX, ;
            NAME      WITH ST_9A26.NAME, ;
            PRODUCER  WITH ST_9A26.PRODUCER, ;
            QNT       WITH DOCD_9A26.QNT, ;
            PRICE     WITH REFD_9A26.PRICE_D, ;
            DOC_DATE  WITH DOCT_9A26.DOC_DATE, ;
            DOC_FIRM  WITH DOCT_9A26.FIRM, ;
            DOC_NUM   WITH DOCT_9A26.DOC_NUM, ;
            REF_DATE  WITH REFT_9A26.DOC_DATE, ;
            REF_FIRM  WITH REFT_9A26.FIRM, ;
            REF_NUM   WITH REFT_9A26.DOC_NUM, ;
            WHO_SAL   WITH ALLTRIM(WSAL_9A26.FAMILY)+" "+LEFT(WSAL_9A26.NAME,1)+" "+LEFT(WSAL_9A26.S_NAME,1), ;
            WHO_CORR  WITH ALLTRIM(WCOR_9A26.FAMILY)+" "+LEFT(WCOR_9A26.NAME,1)+" "+LEFT(WCOR_9A26.S_NAME,1)
      SELECT DOCD_9A26
    ENDSCAN
  SELECT DOCT_9A26
ENDSCAN

SELECT ST_9A26
USE

SELECT REFT_9A26
USE

SELECT REFD_9A26
USE

SELECT DOCD_9A26
USE

SELECT DOCT_9A26
USE

SELECT WSAL_9A26
USE

SELECT WCOR_9A26
USE

DELETE FILE (tmpo_path+"TMP_CUS.DBF")
DELETE FILE (tmpo_path+"TMP_LIST.DBF")
DELETE FILE (tmpo_path+"TMP_LIST.CDX")

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE AGAIN ALIAS CUS_9A26

SELECT TMP_L
INDEX ON STR(CUS_CODE)+NAME+PREFIX+PRODUCER TAG ORD
COPY TO (tmpo_path+"TMP_LIST.DBF")
PRIVATE c_code, s000
s000 = 0
c_code = 0

SCAN
  IF c_code # CUS_CODE
    IF c_code # 0
      = SEEK(c_code, "CUS_9A26")
      SELECT TMP_C
      APPEND BLANK
      REPLACE CUS_CODE WITH c_code, ;
              CUS_NAME WITH CUS_9A26.CUS_NAME, ;
              SUM_D    WITH s000
      SELECT TMP_L
    ENDIF
    c_code = CUS_CODE
    s000 = 0
  ENDIF
  s000 = s000+QNT*PRICE
ENDSCAN
USE
SELECT TMP_C
IF c_code # 0
  = SEEK(c_code, "CUS_9A26")
  APPEND BLANK
  REPLACE CUS_CODE WITH c_code, ;
          CUS_NAME WITH CUS_9A26.CUS_NAME, ;
          SUM_D    WITH s000
ENDIF
INDEX ON SYS(15, lwr, CUS_NAME) TAG ORD
COPY TO (tmpo_path+"TMP_CUS.DBF")
USE 
SELECT CUS_9A26
USE (tmpo_path+"TMP_LIST.DBF") EXCLUSIVE
INDEX ON CUS_CODE TAG CUS_CODE
SELECT 0
USE (tmpo_path+"TMP_CUS.DBF") EXCLUSIVE
DELETE FILE (tmpo_path+"TMP_C.DBF")
DELETE FILE (tmpo_path+"TMP_L.DBF")

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_C         Разработчик Андрей Васин                    │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Просмотр списка клиентов.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.10.1999 ──┘
PROCEDURE View_C
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

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT TMP_CUS
GO TOP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Detail OF (menu_name) PROMPT "Список позиций" KEY Ctrl-A, "Enter"
ON SELECTION PAD Detail OF (menu_name) DO Sw_Mode WITH "Detail"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 66

DO D_Wins WITH ln, wd, "Разногласия с "+DTOC(d_0)+" по "+DTOC(d_1), 0, 0
@ 1, 2 SAY "─Клиент─────────────────────────────────────────────────Сумма"
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
    BROWSE FIELDS CUS_NAME:H="", SUM_D:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F   ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Detail")
    ENDIF

  CASE what_do = "Detail"    && Просмотр списка

    DO View_D WITH CUS_CODE
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
*│   Процедура View_D       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Просмотр списка позиций.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.10.1999 ──┘
PROCEDURE View_D
PARAMETER c_c

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
SELECT TMP_LIST
GO TOP

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
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 71
*┌──Позиция───────────────────────────────Кол-во───Цена──────────Сумма───┐
*│.... ....:....!....:....!....: ....:... ....:. ....:... ....:....!....:│
*└────────────────────────────────────────────────────────....:....!....:┘
*┌────────────────────────────────────────────────────────────────────────┐
*│ Накл. прод. FFF/A000-999999 от ДД.ММ.ГГГГ ....:....!....:....!....:... │
*│ Накл. корр. FFF/A000-999999 от ДД.ММ.ГГГГ ....:....!....:....!....:... │
*└────────────────────────────────────────────────────────────────────────┘

DO D_Wins WITH ln, wd, TMP_CUS.CUS_NAME, 0, 2
@ 1, 2 SAY "──Позиция───────────────────────────────Кол-во───Цена──────────Сумма"
@ WROWS()-5, 58 SAY STR(TMP_CUS.SUM_D, 15, 4)
@ WROWS()-3,  3 SAY "Накл. прод.                 от"
@ WROWS()-2,  3 SAY "Накл. корр.                 от"
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
    BROWSE FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="",  ;
                  QNT:H="", ;
                  PRICE:H="":P="99999.9999", ;
                  SUM=QNT*PRICE:H="":P="99999999.9999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           KEY c_c  ;
           WHEN Draw_Foot()    ;
           NOAPPEND NOMODIFY NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && Печать док-та
    DO Print_Doc
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
@ WROWS()-3, 15 SAY REF_FIRM+"/"+LEFT(REF_NUM,4)+"-"+PADR(ALLTRIM(RIGHT(REF_NUM,6)),6)
@ WROWS()-3, 34 SAY DTOC(REF_DATE)
@ WROWS()-3, 45 SAY WHO_SAL
@ WROWS()-2, 15 SAY DOC_FIRM+"/"+LEFT(DOC_NUM,4)+"-"+PADR(ALLTRIM(RIGHT(DOC_NUM,6)),6)
@ WROWS()-2, 34 SAY DTOC(DOC_DATE)
@ WROWS()-2, 45 SAY WHO_CORR

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Print_Doc    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Бумажный документ.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.10.1999 ──┘
PROCEDURE Print_Doc

PRIVATE sav
sav = RECNO()
SEEK c_c
DELETE FILE (tmpo_path+"TMP_PRN.DBF")
COPY REST WHILE CUS_CODE = c_c TO (tmpo_path+"TMP_PRN.DBF")
GO sav
sav = SELECT()
SELECT 0
USE (tmpo_path+"TMP_PRN.DBF")

PRIVATE p_ln, fnt, n_cp, lft, n, i, j, l_num, d_ver
PRIVATE t_d, t_p, f_p, f_d, d_year, k00, sw, ffeed, p_drctry, d_date
PRIVATE sel_rem, f_name, sb, n_p_det, n_pg, f_pg, s_pg, str_w
PRIVATE nds, nn, c_w

PRIVATE sum_p

DIMENSION sb( 6,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Pg_Sum       }"
sb( 2,2) = ""

sb( 3,1) = "{Tot_Sum      }"
sb( 3,2) = STR(TMP_CUS.SUM_D, 15, 4)

sb( 4,1) = "{Customer     }"
sb( 4,2) = ALLTRIM(TMP_CUS.CUS_NAME)

sb( 5,1) = "{S_Date  }"
sb( 5,2) = DTOC(d_0)

sb( 6,1) = "{T_Date  }"
sb( 6,2) = DTOC(d_1)

SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "RASNO" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed =DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR

n = MEMLINES(DOC_H)
IF n > 0
  DIMENSION t_d(n)
  FOR i = 1 TO n
    t_d(i) = MLINE(DOC_H,i)
  ENDFOR
ELSE
  DIMENSION t_d(1)
  t_d(1) = ""
ENDIF

n = MEMLINES(PAGE_H)
IF n > 0
  DIMENSION t_p(n)
  FOR i = 1 TO n
    t_p(i) = MLINE(PAGE_H,i)
  ENDFOR
ELSE
  DIMENSION t_p(1)
  t_p(1) = ""
ENDIF

n = MEMLINES(PAGE_F)
IF n > 0
  DIMENSION f_p(n)
  FOR i = 1 TO n
    f_p(i) = MLINE(PAGE_F,i)
  ENDFOR
ELSE
  DIMENSION f_p(1)
  f_p(1) = ""
ENDIF

n = MEMLINES(DOC_F)
IF n > 0
  DIMENSION f_d(n)
  FOR i = 1 TO n
    f_d(i) = MLINE(DOC_F,i)
  ENDFOR
ELSE
  DIMENSION f_d(1)
  f_d(1) = ""
ENDIF
USE

SELECT TMP_PRN

DO Ini_Prn WITH "Расхождения ", p_ln, lft, n_cp, fnt, ffeed, p_drctry

GO TOP
nn = 0

n_p_det = p_ln-ALEN(t_p)-MAX(ALEN(f_p),ALEN(f_d))-1
IF n_p_det <= 0
  n_p_det = RECCOUNT()
  n_pg = 1
ELSE
  n_pg = MAX(CEILING(RECCOUNT()/n_p_det),1)
ENDIF

FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1
    EJECT
    FOR i = 1 TO ALEN(t_p)
      str_w = t_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(t_d)
      str_w = t_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      IF i = 1
        @ PROW(), PCOL() SAY str_w
      ELSE
        @ PROW()+1, 0 SAY str_w
      ENDIF
    ENDFOR
  ENDIF

  sum_p = 0
  IF RECCOUNT() # 0
    FOR i = 1 TO n_p_det
      nn = nn+1
      @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+" "+STR(QNT,6)+STR(PRICE,9,4)+STR(QNT*PRICE,16,4)
      sum_p = sum_p+QNT*PRICE
      SKIP
      IF EOF()
        EXIT
      ENDIF
    ENDFOR
  ENDIF
  sb( 2,2) = STR(sum_p,15,4)
  IF f_pg = n_pg
    FOR i = 1 TO ALEN(f_d)
      str_w = f_d(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ELSE
    FOR i = 1 TO ALEN(f_p)
      str_w = f_p(i)
      FOR j = 1 TO ALEN(sb,1)
        str_w = STRTRAN(str_w, sb(j,1), sb(j,2) )
      ENDFOR
      @ PROW()+1, 0 SAY str_w
    ENDFOR
  ENDIF
ENDFOR
USE

DO Term_Prn WITH "", tmpo_path+"TMP_PRN.DBF", c_c

DELETE FILE (tmpo_path+"TMP_PRN.DBF")
SELECT (sav)
RETURN
