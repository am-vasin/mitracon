*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Corr_Tch     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                    Коррекция номенклатуры клиента.                     ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 21.11.2000 ══╝
PROCEDURE Corr_Tch

PRIVATE tmpCusCode, tmpCusName

tmpCusCode = 0
tmpCusName = ""
DO Ch_Cus WITH tmpCusCode, tmpCusName
IF EMPTY(tmpCusCode)
  RETURN
ENDIF

DO Modi_Sto







*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Cus       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Выбор клиента.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.11.2000 ──┘
PROCEDURE Ch_Cus
PARAMETERS c_code, c_name

DO Prp_File

DO Choose

SELECT CUS_LIST
USE
DELETE FILE (tmpo_path+"CUS_LIST.DBF")
DELETE FILE (tmpo_path+"CUS_LIST.CDX")

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_File     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Подготовка файла.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.11.2000 ──┘
PROCEDURE Prp_File

DELETE FILE (tmpo_path+"CUS_LIST.DBF")
DELETE FILE (tmpo_path+"CUS_LIST.CDX")
CREATE DBF (tmpo_path+"CUS_LIST.DBF") ;
  ( CUS_CODE N( 8),  ;
    CUS_NAME C(50)   )
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE AGAIN
SELECT 0
USE (base_path+"TEACH") AGAIN
INDEX ON CUS_CODE TO (tmpo_path+"TEACH") UNIQUE FOR CUS_CODE # 0
SET RELATION TO CUS_CODE INTO ACCOUNT
SCAN
  SELECT CUS_LIST
  APPEND BLANK
  REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE, ;
          CUS_NAME WITH ACCOUNT.CUS_NAME
  SELECT TEACH
ENDSCAN
USE
DELETE FILE (tmpo_path+"TEACH.IDX")
SELECT ACCOUNT
USE
SELECT CUS_LIST
INDEX ON SYS(15, lwr, CUS_NAME) TAG CUS_NAME
GO TOP
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Choose       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Выбор клиента.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.11.2000 ──┘
PROCEDURE Choose

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
SELECT CUS_LIST

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Seek OF (menu_name) PROMPT "Контекстный поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 50

DO D_Wins WITH ln, wd, "Клиенты", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"

*
*    BROWSE - меню
*
    BROWSE FIELDS CUS_NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    c_code = CUS_LIST.CUS_CODE
    c_name = CUS_LIST.CUS_NAME
    EXIT

  CASE what_do = "Seek"      && Контекстный поиск

    DO Seek_Sh
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
*│   Процедура Seek_Sh      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Контекстный поиск.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.11.2000 ──┘
PROCEDURE Seek_Sh

PRIVATE shab, mss

shab = Get_Shab()
IF EMPTY(shab)
  RETURN
ENDIF

shab = SYS(15, lwr, ALLTRIM(shab))
DO Prep_Tmp

IF RECCOUNT("LINK_TMP") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT LINK_TMP
  USE
  DELETE FILE (tmpo_path+"LINK_TMP.DBF")
  SELECT CUS_LIST
ELSE
  DO Ch_Sh
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Shab     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Ввод шаблона.                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.11.2000 ──┘
PROCEDURE Get_Shab

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

*┌──────────────────────────────────────────────┐
*│ ┌─ Шаблон для поиска ──────────────────────┐ │
*│ │ ....:....!....:....!....:....!....:....! │ │
*│ └──────────────────────────────────────────┘ │
*│       < OK Ctrl-W > < Отказаться Esc >       │
*└──────────────────────────────────────────────┘

PRIVATE ex, tmpShab
tmpShab = SPACE(40)
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 50, "Контекстный поиск"
DO Sun_Bord WITH  2,  3,  4, 46, " Шаблон для поиска "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3, 5 GET tmpShab
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN IIF(ex = 1, ALLTRIM(tmpShab), "")

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Tmp     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временного файла.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.11.2000 ──┘
PROCEDURE Prep_Tmp

PRIVATE r_sav

SELECT 0
DELETE FILE (tmpo_path+"LINK_TMP.DBF")
CREATE DBF (tmpo_path+"LINK_TMP.DBF") ( LINK N(10) )
SELECT CUS_LIST
r_sav = RECNO()
SCAN
  IF shab $ SYS(15, lwr, CUS_NAME)
    SELECT LINK_TMP
    APPEND BLANK
    REPLACE LINK WITH RECNO("CUS_LIST")
  ENDIF
  SELECT CUS_LIST
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Sh        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Выбор из отобранного по контексту.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.11.2000 ──┘
PROCEDURE Ch_Sh

PRIVATE r_sav
SELECT CUS_LIST
r_sav = RECNO()
SET ORDER TO
SELECT LINK_TMP
SET RELATION TO LINK INTO CUS_LIST
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
ln = MIN(WROWS("")-11, RECCOUNT("LINK_TMP"))   && Количество видимых строк BROWSE
wd = 50

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
    BROWSE FIELDS CUS_LIST.CUS_NAME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    USE
    SELECT CUS_LIST
    SET ORDER TO CUS_NAME
    EXIT

  OTHERWISE

    USE
    SELECT CUS_LIST
    SET ORDER TO CUS_NAME
    IF BETWEEN(r_sav, 1, RECCOUNT())
      GO r_sav
    ENDIF
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
DELETE FILE (tmpo_path+"LINK_TMP.DBF")

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_Sto     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                    Коррекция номенклатуры клиента.                     │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.11.2000 ──┘
PROCEDURE Modi_Sto

DELETE FILE (tmpo_path+"FOR_DEL.DBF")
DELETE FILE (tmpo_path+"FOR_DEL.CDX")

CREATE DBF (tmpo_path+"FOR_DEL.DBF") ;
  ( LINK N(10) )
INDEX ON LINK TAG LINK

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE

SELECT 0
USE (base_path+"TEACH") ORDER TAG NAME
SET RELATION TO CODE INTO STOCK
SET RELATION TO WHO INTO PERSONS ADDITIVE
SET RELATION TO RECNO() INTO FOR_DEL ADDITIVE

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

DEFINE PAD Del OF (menu_name) PROMPT "Удалить/Восст." KEY Ctrl-A, "Tab"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Seek OF (menu_name) PROMPT "Конт. поиск" KEY Ctrl-A, "F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD Save OF (menu_name) PROMPT "Сохранить" KEY Ctrl-A, "Enter"
ON SELECTION PAD Save OF (menu_name) DO Sw_Mode WITH "Save"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
PRIVATE mss
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 63

DO D_Wins WITH ln, wd, ALLTRIM(tmpCusName), 0, 2
*┌──────Наименование клиента───────────Наименование──────────────┐
*│! XXXX xxxxXxxxxXxxxxXxxxxXxxxxX XXXX xxxxXxxxxXxxxxXxxxxXxxxxX│
*└───────────────────────────────────────────────────────────────┘
*┌──────────────Фирма──────────────────────────Фирма─────────────┐
*│             xxxxXxxx                       xxxxXxxx           │
*│ Позиция занесена                                              │
*└───────────────────────────────────────────────────────────────┘
@ 1, 2 SAY "──────Наименование клиента───────────Наименование"
@ WROWS()-4, 2 SAY "──────────────Фирма──────────────────────────Фирма"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Del"

*
*    BROWSE - меню
*
    BROWSE KEY STR(tmpCusCode)         ;
           FIELDS ToDel = IIF(FOUND("FOR_DEL"), "√", " "):1:H="",  ;
                  PREFIX:H="",         ;
                  Brw_Name=Nm_Part(NAME):25:H="",           ;
                  STOCK.PREFIX:H="",   ;
                  Brw_Name1=Nm_Part(STOCK.NAME):25:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Save")
    ENDIF

  CASE what_do = "Save"    && Просмотр списка

    EXIT

  CASE what_do = "Seek"    && Поиск

    DO S_Sh
    what_do = "List"

  CASE what_do = "Del"     && Удалить/восстановить

    IF FOUND("FOR_DEL")
      SELECT FOR_DEL
      REPLACE LINK WITH 0
      SELECT TEACH
    ELSE
      SELECT FOR_DEL
      GO TOP
      IF LINK # 0 .OR. RECCOUNT() = 0
        APPEND BLANK
      ENDIF
      REPLACE LINK WITH RECNO("TEACH")
      SELECT TEACH
    ENDIF

    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO
SET RELATION TO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
SELECT PERSONS
USE
SELECT STOCK
USE
SELECT FOR_DEL
IF what_do = "Save"
  SCAN FOR LINK # 0
    SELECT TEACH
    GO FOR_DEL.LINK
    REPLACE CUS_CODE WITH 0
    SELECT FOR_DEL
  ENDSCAN
ENDIF
USE
DELETE FILE (tmpo_path+"FOR_DEL.DBF")
DELETE FILE (tmpo_path+"FOR_DEL.CDX")
SELECT TEACH
USE

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
@ WROWS()-3, 15 SAY PRODUCER
@ WROWS()-3, 46 SAY STOCK.PRODUCER
@ WROWS()-2,  3 SAY "Позиция занесена "+DTOC(WHEN)+" "+ ;
   PADR(ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1), 30)
ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура S_Sh         Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Контекстный поиск.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.11.2000 ──┘
PROCEDURE S_Sh

PRIVATE shab, mss

shab = Get_Shab()
IF EMPTY(shab)
  RETURN
ENDIF

DO Pr_Tmp

IF RECCOUNT("LINK_TMP") = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SELECT LINK_TMP
  USE
  DELETE FILE (tmpo_path+"LINK_TMP.DBF")
  SELECT TEACH
ELSE
  DO C_Sh
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Pr_Tmp       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временного файла.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.11.2000 ──┘
PROCEDURE Pr_Tmp

PRIVATE r_sav

SELECT 0
DELETE FILE (tmpo_path+"LINK_TMP.DBF")
CREATE DBF (tmpo_path+"LINK_TMP.DBF") ( LINK N(10) )
SELECT TEACH
r_sav = RECNO()
SEEK STR(tmpCusCode)
SCAN REST WHILE CUS_CODE = tmpCusCode
  IF shab $ PREFIX+NAME+PRODUCER
    SELECT LINK_TMP
    APPEND BLANK
    REPLACE LINK WITH RECNO("TEACH")
  ENDIF
  SELECT TEACH
ENDSCAN

IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура C_Sh         Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Выбор из отобранного по контексту.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.11.2000 ──┘
PROCEDURE C_Sh

PRIVATE r_sav
SELECT TEACH
r_sav = RECNO()
SET ORDER TO
SELECT LINK_TMP
SET RELATION TO LINK INTO TEACH
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
ln = MIN(WROWS("")-11, RECCOUNT("LINK_TMP"))   && Количество видимых строк BROWSE
wd = 39

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
    BROWSE FIELDS TEACH.PREFIX:H="",   ;
                  Brw_Name=Nm_Part(TEACH.NAME):25:H="",     ;
                  TEACH.PRODUCER:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    USE
    SELECT TEACH
    SET ORDER TO TAG NAME
    EXIT

  OTHERWISE

    USE
    SELECT TEACH
    SET ORDER TO NAME
    IF BETWEEN(r_sav, 1, RECCOUNT())
      GO r_sav
    ENDIF
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
DELETE FILE (tmpo_path+"LINK_TMP.DBF")

RETURN
