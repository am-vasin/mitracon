*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Cus_Dem1     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                           Спрос по клиенту.                            ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 25.12.2000 ══╝
PROCEDURE Cus_Dem1
PARAMETERS c_code, c_name

PRIVATE s_sav, tmpFile, mss, s_date
s_date = {}
tmpFile = SYS(3)
CREATE DBF (tmpo_path+tmpFile) ;
  ( CODE     N( 7), ;
    DEM_DATE D,     ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    QNT      N( 6), ;
    QNT_STO  N( 6), ;
    PRICE    N(8,2),;
    WHO      C(28)  )
USE (tmpo_path+tmpFile) EXCLUSIVE ALIAS TMP_DEM

IF Prep_Dem()
  
  SELECT TMP_DEM
  IF RECCOUNT() = 0
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"За указанный период спроса не было!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
  ELSE
    DO View_Dem
  ENDIF
ENDIF

SELECT TMP_DEM
tmpFile = DBF()
USE
DELETE FILE (tmpFile)
DELETE FILE (LEFT(tmpFile, LEN(tmpFile)-3), "cdx")
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Dem     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Формирование файла спроса.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.12.2000 ──┘
PROCEDURE Prep_Dem

IF .NOT. S_Date()
  RETURN .F.
ENDIF

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE
SELECT 0
USE (base_path+"DEMAND") ORDER TAG CLI_CODE

SEEK c_code
SCAN REST WHILE CLI_CODE = c_code FOR DATE >= s_date
  IF .NOT. EMPTY(DEMAND.STO_CODE)
    = SEEK(DEMAND.STO_CODE, "STOCK")
    SELECT TMP_DEM
    APPEND BLANK
    REPLACE CODE     WITH DEMAND.STO_CODE, ;
            DEM_DATE WITH DEMAND.DATE,     ;
            PREFIX   WITH STOCK.PREFIX,    ;
            NAME     WITH STOCK.NAME,      ;
            PRODUCER WITH STOCK.PRODUCER,  ;
            QNT      WITH DEMAND.QTY,      ;
            PRICE    WITH STOCK.PRICE_R,   ;
            QNT_STO  WITH STOCK.QNT
  ELSE
    SELECT TMP_DEM
    APPEND BLANK
    REPLACE DEM_DATE WITH DEMAND.DATE,     ;
            PREFIX   WITH DEMAND.PREFIX,   ;
            NAME     WITH DEMAND.NAME,     ;
            PRODUCER WITH DEMAND.PRODUCER, ;
            QNT      WITH DEMAND.QTY
  ENDIF
  IF SEEK(DEMAND.WHO, "PERSONS")
    REPLACE WHO WITH ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)
  ENDIF
  SELECT DEMAND
ENDSCAN
USE
SELECT STOCK
USE
SELECT PERSONS
USE
SELECT TMP_DEM
INDEX ON NAME+PREFIX+PRODUCER TAG NAME
GO TOP

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла S_Date       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                               Ввод даты.                               ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 25.12.2000 ══╝
PROCEDURE S_Date
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

* Временной интервал
*┌──────────────────────────────────┐
*│  Начальная дата ДД.ММ.ГГГГ (F1)  │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 38, "Временной интервал"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

ON KEY LABEL F1 DO Date_Help
@ 2,  4 SAY "Начальная дата ДД.ММ.ГГГГ (F1)"
@ 2, 19 GET s_date PICTURE "@D"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE
ON KEY LABEL F1
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Date_Help    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Ввод даты через календарь.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.12.2000 ──┘
PROCEDURE Date_Help

s_date = Get_Date(s_date)
SHOW GET s_date

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_Dem     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Просмотр спроса.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.12.2000 ──┘
PROCEDURE View_Dem
PRIVATE f_value, mss
f_value = "Все позиции"

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
SELECT TMP_DEM

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Filter OF (menu_name) PROMPT "Фильтр"
ON PAD Filter OF (menu_name) ACTIVATE POPUP Filter

  DEFINE POPUP Filter MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Filter PROMPT "Все позиции" KEY Ctrl-A, ""
  ON SELECTION BAR 1 OF Filter DO Sw_Mode WITH "Filter_1"

  DEFINE BAR  2 OF Filter PROMPT "Только складские" KEY Ctrl-A, ""
  ON SELECTION BAR 2 OF Filter DO Sw_Mode WITH "Filter_2"

  DEFINE BAR  3 OF Filter PROMPT "Только имеющиеся в наличии" KEY Ctrl-A, ""
  ON SELECTION BAR 3 OF Filter DO Sw_Mode WITH "Filter_3"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Report OF (menu_name) PROMPT "Справка" KEY Ctrl-A, "Enter"
ON SELECTION PAD Report OF (menu_name) DO Sw_Mode WITH "Report"

DEFINE PAD List OF (menu_name) PROMPT "Список" KEY Ctrl-A, "F2"
ON SELECTION PAD List OF (menu_name) DO Sw_Mode WITH "Order"

DEFINE PAD Print OF (menu_name) PROMPT "Печать" KEY Ctrl-A, "F3"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Delete OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 75
*┌С────Дата─────────Наименование────────────────────────Спрос──Нал-е───Цена──┐
*│X DD.DD.DDDD PPPP NNNNNNNNNnNNNNNNNNNnNNNNN PPPPPPPP 999999 999999 99999.99│
*└───────────────────────────────────────────────────────────────────────────┘
*┌───────────────────────────────────────────────────────────────────────────┐
*│ Кто собрал ФФФФФФФФФфФФФФФФФФФфФФФФФФФФ И О                               │
*└───────────────────────────────────────────────────────────────────────────┘

DO D_Wins WITH ln, wd, ALLTRIM(c_name)+;
               ": спрос с "+DTOC(s_date), 1, 2
@ 2, 2 SAY ;
"С────Дата─────────Наименование────────────────────────Спрос──Нал-е───Цена"
@ WROWS()-2, 3 SAY "Кто собрал"  
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE WINDOW (win_name+"_M") SAME
    @ 1, 2 SAY "Фильтр: "+PADR(f_value, 30)
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"
    ON KEY LABEL F2    DO Sw_Mode WITH "Order"
    ON KEY LABEL F3    DO Sw_Mode WITH "Print"
    ON KEY LABEL Del   DO Sw_Mode WITH "Delete"
*
*    BROWSE - меню
*
    BROWSE FIELDS IS_STO = IIF(EMPTY(CODE), "X", " "):1:H="", ;
                  DEM_DATE:H="", ;
                  PREFIX:H="",   ;
                  Brw_Name=Nm_Part(NAME):25:H="",  ;
                  PRODUCER:H="", ;
                  QNT:P="@Z 999999":H="",     ;
                  QNT_STO:P="@Z 999999":H="", ;
                  PRICE:P="@Z 99999.99":H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Report")
    ENDIF

  CASE what_do = "Report"    && Справка

    IF .NOT. BOF() .AND. .NOT. EOF()
      IF CODE = 0
        DIMENSION mss(3)
        mss(1) = ""
        mss(2) = CHR(0)+"Позиция нескладская - движения нет!"
        mss(3) = ""
        DO Out_Mess WITH 7, "mss"
      ELSE
       DO Move_s WITH CODE
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Filter_1"  && Фильтр: Все позиции

    SET FILTER TO
    f_value = "Все позиции"

    what_do = "List"

  CASE what_do = "Filter_2"  && Фильтр: Только складские

    SET FILTER TO CODE # 0
    f_value = "Только складские"
    what_do = "List"

  CASE what_do = "Filter_3"  && Фильтр: Только имеющиеся в наличии

    SET FILTER TO CODE # 0 .AND. QNT_STO > 0
    f_value = "Только имеющиеся в наличии"
    what_do = "List"

  CASE what_do = "Seek"      && Поиск

    DO Seek_Pos WITH Get_Shab("", "Контекстный поиск")
    what_do = "List"

  CASE what_do = "Order"     && Список

    DO Cr_List
    what_do = "List"

  CASE what_do = "Print"     && Печать

    DO P_C_Dem
    what_do = "List"

  CASE what_do = "Delete"    && Удаление

    IF .NOT. EOF() .AND. .NOT. BOF()
      DELETE
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
@ WROWS()-3, 14 SAY WHO
@ WROWS()-2, 3 SAY PREFIX+" "+NAME+" "+PRODUCER

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_Pos     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Контекстный поиск позиции.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.12.2000 ──┘
PROCEDURE Seek_Pos
PARAMETERS patt

IF EMPTY(patt)
  RETURN
ENDIF

DO Prp_List WITH patt
DO Choose_P

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_List     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Отбор позиций по контексту.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.12.2000 ──┘
PROCEDURE Prp_List
PARAMETERS patt

PRIVATE s_sav, tmpName, r_sav

s_sav = SELECT()
SELECT 0
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ;
  ( LINK     N(10), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8)  )
USE (tmpo_path+tmpName) ALIAS FOUND

SELECT TMP_DEM
r_sav = RECNO()
SCAN
  IF patt $ PREFIX+NAME+PRODUCER
    SELECT FOUND
    APPEND BLANK
    REPLACE LINK     WITH RECNO("TMP_DEM"), ;
            PREFIX   WITH TMP_DEM.PREFIX,   ;
            NAME     WITH TMP_DEM.NAME,     ;
            PRODUCER WITH TMP_DEM.PRODUCER
    SELECT TMP_DEM
  ENDIF
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Choose_P     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│               Выбор позиции из отбранного по контексту.                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.12.2000 ──┘
PROCEDURE Choose_P

PRIVATE tmpName, mss

SELECT FOUND
tmpName = DBF()
*
*    А был ли мальчик?
IF RECCOUNT("FOUND") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE(tmpName)        && Приборка...
  SELECT TMP_DEM
  RETURN
ENDIF
GO TOP
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
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(WROWS("")-11, RECCOUNT())   && Количество видимых строк BROWSE
wd = 49
DO D_Wins WITH ln, wd, "Результат поиска", 0, 0
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
    BROWSE FIELDS PREFIX:H="",   ;
                  NAME:H="",     ;
                  PRODUCER:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"	&& Юзер выбрал...

    GO FOUND.LINK IN TMP_DEM
    EXIT

  OTHERWISE					&& Юзер отказался

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
DELETE FILE(tmpName)        && Приборка...
SELECT TMP_DEM

RETURN


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_P        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Параметры списка.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.12.2000 ──┘
PROCEDURE Get_P
PARAMETERS c_code, firm_c, excl_def, tmpUSD

PRIVATE f_point, s_sav, f_list, f_numb

f_point = 0
f_numb  = 0

*
*   Готовим список доступных предприятий и устанавливаем указатель
s_sav = SELECT()
SELECT 0
USE (base_path+"FIRMS") ORDER TAG B_NAME AGAIN ALIAS F_0C26
SCAN
  IF Leg_Firm(c_code, FIRM_CODE)
    f_numb = f_numb+1
    DIMENSION f_list(f_numb)
    f_list(f_numb) = FIRM_CODE+" "+B_NAME
    IF FIRM_CODE = firm_c			&& То самое предприятие!!!
      f_point = f_numb
    ENDIF
  ENDIF
ENDSCAN

USE
SELECT (s_sav)
IF f_numb = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Недоступно ни одно предприятие!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  firm_c = "   "
  RETURN
ENDIF

f_point = MAX(f_point,1)

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

*┌────────────────────────────────────┐
*│       [ ] Исключить дефицит        │
*│         [ ] Список в USD           │
*│             ┌────────────────────┐ │
*│ Предприятие │FFF NNNNNNNNNnNNNNNN│ │
*│             └────────────────────┘ │
*│  < OK Ctrl-W > < Отказаться Esc >  │
*└────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 40, "Создание списка"
@ 5, 3 SAY "Предприятие"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 4, 15 GET f_point PICTURE "@^" FROM f_list SIZE 1, 22 WHEN f_numb > 1
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
@ 2,  9 GET excl_def PICTURE "@*C Исключить дефицит"
@ 3, 11 GET tmpUSD PICTURE "@*C Список в USD"
READ CYCLE

IF ex # 1  && Юзер отказался...
  firm_c = "   "
ELSE
  firm_c = LEFT(f_list(f_point),3)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Cr_List      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Создание списка.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 26.12.2000 ──┘
PROCEDURE Cr_List

PRIVATE del_def, f_c, s_sav, tmpNum, sum_0
PRIVATE tmpName, newRec, tmpQnt, mss, r_sav, USD_list

USD_list = .T.
del_def = .T.
f_c     = def_firm
DO Get_P WITH c_code, f_c, del_def, USD_list
IF EMPTY(f_c)
  RETURN
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE AGAIN
SELECT 0
USE (base_path+"LIST_DET")
tmpName = SYS(3)
COPY STRUCTURE TO (tmpo_path+tmpName)
SELECT 0
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_LIST
INDEX ON CODE TAG CODE
INDEX ON NAME+PREFIX+PRODUCER TAG NAME ADDITIVE

SELECT TMP_DEM
r_sav = RECNO()
tmpNum = 0

SCAN FOR QNT > 0

  SELECT TMP_LIST 
  IF TMP_DEM.CODE = 0
    SET ORDER TO TAG NAME
    newRec = .NOT. SEEK(TMP_DEM.NAME+TMP_DEM.PREFIX+TMP_DEM.PRODUCER)
  ELSE
    = SEEK(TMP_DEM.CODE, "STOCK")
    SET ORDER TO TAG CODE
    newRec = .NOT. SEEK(TMP_DEM.CODE)
  ENDIF

  IF newRec
    APPEND BLANK
    REPLACE CODE      WITH TMP_DEM.CODE, ;
            PREFIX    WITH IIF(TMP_DEM.CODE=0, TMP_DEM.PREFIX, STOCK.PREFIX),     ;
            NAME      WITH IIF(TMP_DEM.CODE=0, TMP_DEM.NAME, STOCK.NAME),         ;
            PRODUCER  WITH IIF(TMP_DEM.CODE=0, TMP_DEM.PRODUCER, STOCK.PRODUCER), ;
            ADDRESS   WITH IIF(TMP_DEM.CODE=0, "", STOCK.ADDRESS),                ;
            USD_DEF   WITH IIF(TMP_DEM.CODE=0, 0, STOCK.PRICE),                   ;
            DEF_PRICE WITH IIF(TMP_DEM.CODE=0, 0, STOCK.PRICE_R),                 ;
            SAL_PRICE WITH IIF(TMP_DEM.CODE=0.OR.USD_list, 0, STOCK.PRICE_R),     ;
            USD_PRICE WITH IIF(TMP_DEM.CODE=0.OR..NOT.USD_list, 0, STOCK.PRICE)
  ENDIF
  REPLACE QNT WITH QNT+TMP_DEM.QNT
  SELECT TMP_DEM

ENDSCAN

SELECT TMP_LIST
SET RELATION TO CODE INTO STOCK

sum_0 = 0
SCAN
  tmpQnt = QNT
  IF del_def
    IF CODE = 0
      tmpQnt = 0
    ELSE
      tmpQnt = MIN(QNT, STOCK.QNT)
    ENDIF
  ENDIF

  IF CODE # 0
    tmpQnt = CEILING(tmpQnt/MAX(STOCK.QNT_PACK,1))*MAX(STOCK.QNT_PACK,1)
  ENDIF
  tmpQnt = MAX(tmpQnt, 0)
  IF tmpQnt <= 0
    LOOP
  ENDIF
  IF tmpNum = 0
    SELECT 0
    USE (base_path+"LIST_TIT")
    tmpNum = Doc_Num("LISTS", f_c, arm, DATE())

    APPEND BLANK
    REPLACE LST_ARM   WITH arm,     ;
            LST_NUM   WITH tmpNum,  ;
            CR_DAT    WITH DATE(),  ;
            IS_USD    WITH USD_list,;
            LST_NAME  WITH LEFT(c_name, FSIZE("LST_NAME")-4)+"Спрос", ;
            CLI_CODE  WITH c_code,  ;
            FIRM      WITH f_c,     ;
            ORD_QUEUE WITH queuetype, ;
            WHO_CR    WITH user,    ;
            WHERE_CR  WITH arm
  ENDIF
  SELECT LIST_DET
  APPEND BLANK
  REPLACE LIST_NUM  WITH STR(tmpNum,6),		;
          CODE      WITH TMP_LIST.CODE,		;
          PREFIX    WITH TMP_LIST.PREFIX,	;
          NAME      WITH TMP_LIST.NAME,		;
          PRODUCER  WITH TMP_LIST.PRODUCER,	;
          ADDRESS   WITH TMP_LIST.ADDRESS,	;
          QNT       WITH tmpQnt,			;
          USD_DEF   WITH TMP_LIST.USD_DEF,	;
          DEF_PRICE WITH TMP_LIST.DEF_PRICE,;
          USD_PRICE WITH TMP_LIST.USD_PRICE,;
          SAL_PRICE WITH TMP_LIST.SAL_PRICE
  IF USD_list
    sum_0 = sum_0+USD_PRICE*QNT
  ELSE
    sum_0 = sum_0+ROUND(SAL_PRICE*QNT,2)
  ENDIF
  SELECT TMP_LIST
ENDSCAN
tmpName = DBF()
USE
DELETE FILE (tmpName)
DELETE FILE (LEFT(tmpName, LEN(tmpName)-3)+"cdx")
IF USED("LIST_TIT")
  SELECT LIST_TIT
  REPLACE R_TOTAL WITH sum_0
  USE
ENDIF
SELECT LIST_DET
USE
SELECT STOCK
USE
SELECT TMP_DEM
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

SELECT (s_sav)
IF EMPTY(tmpNum)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего нет на складе, список не сформирован!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ELSE
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Сформирован список № "+ALLTRIM(STR(tmpNum))
  mss(3) = ""
  DO Out_Mess WITH 5, "mss"
ENDIF
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура P_C_Dem        Разработчик Андрей Васин                    │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Печать документа.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12/27/2000 ──┘
PROCEDURE P_C_Dem

PRIVATE p_ln, fnt, n_cp, lft, ffeed, sb, n, t_d, t_p, f_d, f_p, p_drctry
PRIVATE s_sav, r_sav

DIMENSION sb(4,2)
sb( 1,1) = "{Pg/Pgs}"
sb( 1,2) = ""

sb( 2,1) = "{Customer}"
sb( 2,2) = c_name

sb( 3,1) = "{Pg_Sum  }"
sb( 3,2) = ""

sb( 4,1) = "{Tot_Sum }"
sb( 4,2) = ""

n = ALEN(sb,1)
FOR i = 1 TO n
  sb(i,2) =PADR(sb(i,2), LEN(sb(i,1)))
ENDFOR
s_sav = SELECT()
SELECT 0
USE (base_path+"DOC_FORM")
LOCATE FOR "CUS_DEM" == ALLTRIM(UPPER(DOC_NAME))
p_ln = DOC_FORM.PAGE_LEN
fnt  = DOC_FORM.FONT+DOC_FORM.ORIENT
n_cp = DOC_FORM.N_COPIES
lft  = DOC_FORM.LEFT_FIELD
ffeed = DOC_FORM.F_FEED
p_drctry = DOC_FORM.P_DIR


n = MEMLINES(DOC_H)
DIMENSION t_d(n)
FOR i = 1 TO n
  t_d(i) = MLINE(DOC_H,i)
ENDFOR

n = MEMLINES(PAGE_H)
DIMENSION t_p(n)
FOR i = 1 TO n
  t_p(i) = MLINE(PAGE_H,i)
ENDFOR

n = MEMLINES(PAGE_F)
DIMENSION f_p(n)
FOR i = 1 TO n
  f_p(i) = MLINE(PAGE_F,i)
ENDFOR

n = MEMLINES(DOC_F)
DIMENSION f_d(n)
FOR i = 1 TO n
  f_d(i) = MLINE(DOC_F,i)
ENDFOR
USE

SELECT TMP_DEM
r_sav = RECNO()
DELETE FILE (tmpo_path+"P_C_DEM.DBF")
COPY TO (tmpo_path+"P_C_DEM.DBF")
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
SELECT 0
USE (tmpo_path+"P_C_DEM.DBF")
nn = RECCOUNT()
DO Ini_Prn WITH "Список спроса для клиента", p_ln, lft, n_cp, fnt, ffeed, p_drctry

PRIVATE n_pg, n_p_det, nn, sum_d, sum_p, f_pg, str_w
n_p_det  = p_ln-MAX(ALEN(t_d),ALEN(t_p))-MAX(ALEN(f_p),ALEN(f_d))
n_pg = 1

IF n_p_det <= 0  && Забили всю страницу под заголовки... ни каких страниц!
  n_p_det = nn
ELSE
  nn = MAX(nn-n_p_det, 0)
  n_pg = 1+CEILING(nn/n_p_det)
ENDIF

nn = 0

STORE 0 TO sum_d, sum_p
FOR f_pg = 1 TO n_pg
  sb( 1,2) = ALLTRIM(STR(f_pg))+"/"+ALLTRIM(STR(n_pg))
  sb( 1,2) = PADR(sb(1,2), LEN(sb(1,1)))
  IF f_pg # 1   && Заголовок НЕ первой страницы
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
  ELSE        && Заголовок первой страницы
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
  STORE 0 TO sum_p
*
*   Детальная часть страницы
* 
  FOR i = 1 TO n_p_det
    nn = nn+1
    @ PROW()+1,0 SAY PREFIX+" "+NAME+" "+PRODUCER+STR(QNT,7)+STR(QNT_STO,7)+  ;
                     STR(PRICE,9,2)+STR(QNT*PRICE,11,2)
    sum_p = sum_p+QNT*PRICE
    SKIP
    IF EOF()
      EXIT
    ENDIF
  ENDFOR
*
*    Окончание страницы
*
  sum_d = sum_d+sum_p

  sb( 3,2) = PADR(STR(sum_p,10,2), LEN(sb(3,1)))
  IF f_pg = n_pg
    sb(4,2) =PADR(STR(sum_d,10,2), LEN(sb(4,1)))
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
DO Term_Prn WITH "", tmpo_path+"P_C_DEM.DBF", c_code
DELETE FILE (tmpo_path+"P_C_DEM.DBF")

SELECT (s_sav)

RETURN
