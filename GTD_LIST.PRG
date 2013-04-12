*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла GTD_List     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                         Выбор из архива ГТД.                           ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 09/04/2002 ══╝
PROCEDURE GTD_List
PARAMETER s_Value
PRIVATE r_value

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
r_value = ""
IF TYPE("s_value") = "C"
  IF .NOT. EMPTY(s_value)
    r_value = s_value
  ENDIF
ENDIF


s_sav = SELECT()

SELECT 0
DO Use_Dummy

SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_NUM ALIAS DET_0303 AGAIN

SELECT 0
USE (path_comm+"GTD_C") ORDER TAG GTD_NUM ALIAS CLS_0303 AGAIN

SELECT 0
USE (path_comm+"GTD_TIT") ALIAS TIT_0302 AGAIN ORDER TAG GTD_NUM
SET RELATION TO GTD_NUM INTO DET_0303
SET RELATION TO GTD_NUM INTO CLS_0303 ADDITIVE
IF .NOT. EMPTY(r_value)
  IF .NOT. SEEK(r_value)
    SET ORDER TO TAG HRON
    GO TOP
    r_value = ""
    GO TOP
  ENDIF
ELSE
  SET ORDER TO TAG HRON
  GO TOP
ENDIF
SET ORDER TO TAG HRON
IF EMPTY(r_value)
  r_value = SPACE(FSIZE("GTD_NUM"))
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбор" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Modify OF (menu_name) PROMPT "Содержимое"
ON PAD Modify OF (menu_name) ACTIVATE POPUP Modify

  DEFINE POPUP Modify MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Modify PROMPT "Позиции ГТД" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 1 OF Modify DO Sw_Mode WITH "Modify"

  DEFINE BAR  2 OF Modify PROMPT "Корреция классов" KEY Ctrl-A, "Shift-Tab"
  ON SELECTION BAR 2 OF Modify DO Sw_Mode WITH "Classes"

DEFINE PAD Print OF (menu_name) PROMPT "Печать" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "Поиск ГТД" KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "Seek"

  DEFINE BAR  2 OF Seek PROMPT "Повтор поиска" KEY Ctrl-A, "Ctrl-Enter"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "Again"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 43

*┌── Дата ──── Номер ─────────────────────П─К┐
*│ДД.ММ.ГГГГ NNNNNNNNNNNNNNNNNNNNNNNNNN X X X│
*└───────────────────────────────────────────┘

DO D_Wins WITH ln, wd, "Перечень ГТД", 0, 0
@ 1, 2 SAY "── Дата ──── Номер ─────────────────────П─К"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter   KEYBOARD CHR(23)
    ON KEY LABEL Tab     DO Sw_Mode WITH "Modify"
    ON KEY LABEL BackTab DO Sw_Mode WITH "Classes"
    ON KEY LABEL Ctrl+P  DO Sw_Mode WITH "Print"
    ON KEY LABEL F7      DO Sw_Mode WITH "Seek"
    ON KEY LABEL Ctrl+F7 DO Sw_Mode WITH "Again"

*
*    BROWSE - меню
*
    BROWSE FIELDS GTD_DATE:H="", GTD_NUM:H="", GTD_TYPE:H="", ;
           M001 = IIF(FOUND("DET_0303"), "√", " "):1:H= "", ;
           M002 = IIF(FOUND("CLS_0303"), "√", " "):1:H= "", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор ГТД

    IF .NOT. BOF() .AND. .NOT. EOF()
      r_value = GTD_NUM
    ENDIF
    EXIT

  CASE what_do = "Modify"    && Просмотр содержимого ГТД

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO View_GTD
    ENDIF
    what_do = "List"

  CASE what_do = "Classes"    && Просмотр содержимого ГТД

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO View_Cls
    ENDIF
    what_do = "List"

  CASE what_do = "Print"    && Печать

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_GTD WITH GTD_NUM
    ENDIF
    what_do = "List"

  CASE what_do = "Seek"     && Поиск

    DO SeekGTD
    what_do = "List"

  CASE what_do = "Again"    && Повтор

    DO SeekGTD WITH .T.
    what_do = "List"

  CASE what_do = "Ins"       && Добавление заголовка ГТД

    DO Add_GTD
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
SELECT DET_0303
USE
SELECT CLS_0303
USE
SELECT (s_sav)

RETURN r_value

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Add_GTD      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Добавление заголовка ГТД.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/02/2000 ──┘
PROCEDURE Add_GTD

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

*┌───────────────────────────────────────┐
*│  Номер ГТД ....:....!....:....!....:. │
*│                                       │
*│  Дата ГТД  ДД.ММ.ГГГГ (F1)            │
*│                                       │
*│  [ ] Флажок                           │
*│                                       │
*│   < OK Ctrl-W > < Отказаться Esc >    │
*└───────────────────────────────────────┘

PRIVATE ex, tmpNum, tmpDate, tmpFlg
tmpNum  = SPACE(26)
tmpDate = DATE()
tmpFlg  = .F.
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 34, "Добавление ГТД"

@ 2, 4 SAY "Номер ГТД"
@ 4, 4 SAY "Дата ГТД             (F1)"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
ON KEY LABEL F1 DO Set_Dt
@ 2, 14 GET tmpNum
@ 4, 14 GET tmpDate
@ 6,  4 GET tmpFlg PICTURE "@*C Флажок"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_GTD()
ON KEY LABEL F1

IF ex = 1
  *
  * Отрабатываем бланк
  APPEND BLANK
  REPLACE GTD_NUM  WITH tmpNum,  ;
          GTD_DATE WITH tmpDate, ;
          GTD_TYPE WITH IIF(tmpFlg, "√", " ")
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_Dt       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Установка даты по календарю.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.03.2000 ──┘
PROCEDURE Set_Dt

tmpDate = Get_Date(tmpDate)
SHOW GET tmpDate

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_GTD      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Проверка корректности ГТД.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/02/2000 ──┘
PROCEDURE Tst_GTD

PRIVATE mss, r_sav, o_sav, sw

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(tmpNum)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Номер ГТД должен быть! Ну хоть какой-нибудь..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 1
ENDIF

r_sav = RECNO()
o_sav = TAG()

SET ORDER TO GTD_NUM
sw =  SEEK(tmpNum)
SET ORDER TO (o_sav)
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF
IF sw
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ГТД с таким номером уже существует!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 1
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_GTD     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Просмотр содержимого ГТД.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/03/2000 ──┘
PROCEDURE View_GTD

DO Prep_Tmp
DO Modi_Tmp
SELECT GTD_TMP
USE
DELETE FILE (tmpo_path+"GTD_TMP.DBF")
DELETE FILE (tmpo_path+"GTD_TMP.CDX")

SELECT TIT_0302

RETURN


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Tmp     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временного файла.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/03/2000 ──┘
PROCEDURE Prep_Tmp

DELETE FILE (tmpo_path+"GTD_TMP.DBF")
DELETE FILE (tmpo_path+"GTD_TMP.CDX")

SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_NUM ALIAS GTD_0303 AGAIN

COPY STRUCTURE FIELDS NAME, COUNTRY, GTD_LINE, QNT TO (tmpo_path+"GTD_TMP")
CREATE DBF (tmpo_path+"GTD_TMP") ;
  ( MARK C(1), NAME C(29), COUNTRY C(16), GTD_LINE C( 4), QNT N( 8) )

USE (tmpo_path+"GTD_TMP") EXCLUSIVE

SELECT GTD_0303
SEEK TIT_0302.GTD_NUM
SCAN REST WHILE GTD_NUM = TIT_0302.GTD_NUM
  SELECT GTD_TMP
  APPEND BLANK
  REPLACE NAME     WITH GTD_0303.NAME,    ;
          COUNTRY  WITH GTD_0303.COUNTRY, ;
          GTD_LINE WITH GTD_0303.GTD_LINE,;
          QNT      WITH GTD_0303.QNT
  SELECT GTD_0303
ENDSCAN

USE
SELECT GTD_TMP
INDEX ON NAME TAG NAME

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_Tmp     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Коррекция заготовки.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.03.2000 ──┘
PROCEDURE Modi_Tmp


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
SELECT GTD_TMP

*
*   Определяем асинхронное меню
*
DEFINE WINDOW (win_name+"_H") FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
ACTIVATE WINDOW (win_name+"_H") NOSHOW
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
*┌──Наименование──────────────────Страна──────────N стр──Кол-во─┐
*│M ....:....!....:....!....:.... ....:....!....:. .... ....:...│
*└──────────────────────────────────────────────────────────────┘
*┌──────────────────────────────────────────────────────────────┐
*│        Наименование ....:....!....:....!....:....            │
*│                                                              │
*│ [ ] Страна ....:....!....:.  Строка N 9999  Кол-во 99999999  │
*│                                                              │
*│              < OK Ctrl-W > < Отказаться Esc >                │
*└──────────────────────────────────────────────────────────────┘
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 62

DO D_Wins WITH ln, wd, "ГТД N "+ALLTRIM(TIT_0302.GTD_NUM)+" от "+DTOC(TIT_0302.GTD_DATE), 0, 5
@ 1, 2 SAY "──Наименование──────────────────Страна──────────N стр──Кол-во"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1

    BROWSE FIELDS MARK:H="",  ;
                  NAME:H="",  ;
                  COUNTRY:H="", ;
                  GTD_LINE:H="",;
                  QNT:H="":P="@Z 99999999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

*
*    BROWSE - меню
*
    ON KEY
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_H")

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
@ WROWS()-6,  2 CLEAR TO WROWS()-2, WCOLS()-3
@ WROWS()-6, 10 SAY "Наименование " + NAME
@ WROWS()-4,  7 SAY "Страна " + COUNTRY
@ WROWS()-4, 32 SAY "Строка N " + GTD_LINE
@ WROWS()-4, 47 SAY "Кол-во " + STR(QNT,8)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура SeekGTD      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                         Контекстный поиск ГТД                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.03.2002 ──┘
PROCEDURE SeekGTD
PARAMETERS repeat

PRIVATE tmpShab, o_sav, r_sav

SELECT TIT_0302
r_sav = RECNO()

IF .NOT. repeat
  tmpShab = Get_Shab()
  IF EMPTY(tmpShab)
    RETURN
  ENDIF
  tmpShab = UPPER(tmpShab)
  SELECT 0
  DELETE FILE (tmpo_path+"SeekGTD.DBF")
  CREATE DBF (tmpo_path+"SeekGTD.DBF") ( LINK N(10) )
  SELECT TIT_0302
  SCAN FOR tmpShab $ UPPER(GTD_NUM)
    SELECT SeekGTD
    APPEND BLANK
    REPLACE LINK WITH RECNO("TIT_0302")
    SELECT TIT_0302
  ENDSCAN
  SELECT SeekGTD
  USE
  SELECT TIT_0302
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF

IF .NOT. File_O(tmpo_path+"SeekGTD.DBF")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
o_sav = ORDER()
SET ORDER TO
SELECT 0
USE (tmpo_path+"SeekGTD")
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Ничего не найдено..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT TIT_0302
  SET ORDER TO TAG (o_sav)
  RETURN  
ENDIF

SET RELATION TO LINK INTO TIT_0302

tmpShab = Ch_Link()

USE
SELECT TIT_0302
SET ORDER TO TAG (o_sav)

IF BETWEEN(tmpShab, 1, RECCOUNT())
  GO tmpShab
ELSE
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
ENDIF

RETURN
*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Link      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Выбор из отобранного                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.03.2002 ──┘
PROCEDURE Ch_Link

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

PRIVATE tmpRet
tmpRet = 0

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
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 39

*┌── Дата ──── Номер ────────────────────┐
*│ДД.ММ.ГГГГ NNNNNNNNNNNNNNNNNNNNNNNNNN X│
*└───────────────────────────────────────┘

DO D_Wins WITH ln, wd, "Найдено ГТД...", 0, 0
@ 1, 2 SAY "── Дата ──── Номер "
what_do = "List"
GO TOP

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS TIT_0302.GTD_DATE:H="", TIT_0302.GTD_NUM:H="", TIT_0302.GTD_TYPE:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    tmpRet = LINK
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

RETURN tmpRet

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_Cls     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                 Просмотр содержимого ГТД в части классов.              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/03/2000 ──┘
PROCEDURE View_Cls

DO Prep_Cls
DO Corr_Cls
SELECT GTD_TMP
USE
DELETE FILE (tmpo_path+"GTD_TMP.DBF")
DELETE FILE (tmpo_path+"GTD_TMP.CDX")

SELECT TIT_0302

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prep_Cls     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                  Подготовка временного файла классов.                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/03/2000 ──┘
PROCEDURE Prep_Cls

DELETE FILE (tmpo_path+"GTD_TMP.DBF")
DELETE FILE (tmpo_path+"GTD_TMP.CDX")

SELECT 0
USE (path_comm+"GTD_C") ORDER TAG GTD_NUM ALIAS GTD_0303 AGAIN

COPY STRUCTURE FIELDS NAME, COUNTRY, GTD_LINE, QNT TO (tmpo_path+"GTD_TMP")
CREATE DBF (tmpo_path+"GTD_TMP") ;
  ( MARK C(1), CLASS C(12), COUNTRY C(16), GTD_LINE C( 4), QNT N( 8) )

USE (tmpo_path+"GTD_TMP") EXCLUSIVE

SELECT GTD_0303
SEEK TIT_0302.GTD_NUM
SCAN REST WHILE GTD_NUM = TIT_0302.GTD_NUM
  SELECT GTD_TMP
  APPEND BLANK
  REPLACE CLASS    WITH GTD_0303.CLASS,   ;
          COUNTRY  WITH GTD_0303.COUNTRY, ;
          GTD_LINE WITH GTD_0303.GTD_LINE,;
          QNT      WITH GTD_0303.QNT
  SELECT GTD_0303
ENDSCAN

USE
SELECT GTD_TMP
INDEX ON CLASS TAG NAME
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Corr_Cls     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│              Коррекция заготовки в части классов.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.03.2000 ──┘
PROCEDURE Corr_Cls


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
USE (base_path+"CLASSES") SHARED AGAIN ALIAS CCC_0303 ORDER TAG CODE
DO Use_Dummy
SELECT GTD_TMP
SET RELATION TO CLASS INTO CCC_0303

*
*   Определяем асинхронное меню
*
DEFINE WINDOW (win_name+"_H") FROM 0, 0 TO 0, WCOLS("") NONE COLOR SCHEME 13
ACTIVATE WINDOW (win_name+"_H") NOSHOW
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
*┌──Наименование──────────────────Страна──────────N стр──Кол-во─┐
*│M ....:....!....:....!....:.... ....:....!....:. .... ....:...│
*└──────────────────────────────────────────────────────────────┘
*┌──────────────────────────────────────────────────────────────┐
*│ ....:....!....:....!....:....!....:....!....:....!....:....! │
*│                                                              │
*│ [ ] Страна ....:....!....:.  Строка N 9999  Кол-во 99999999  │
*│                                                              │
*│              < OK Ctrl-W > < Отказаться Esc >                │
*└──────────────────────────────────────────────────────────────┘
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 62

DO D_Wins WITH ln, wd, "ГТД N "+ALLTRIM(TIT_0302.GTD_NUM)+" от "+DTOC(TIT_0302.GTD_DATE), 0, 5
@ 1, 2 SAY "──Класс─────────────────────────Страна──────────N стр──Кол-во"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1

    BROWSE FIELDS MARK:H="",  ;
                  CCC_0303.NAME:29:H="",  ;
                  COUNTRY:H="", ;
                  GTD_LINE:H="",;
                  QNT:H="":P="@Z 99999999", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Dr_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

*
*    BROWSE - меню
*
    ON KEY
    EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE WINDOW (win_name+"_H")

RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
SELECT CCC_0303
USE
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Dr_Foot      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Dr_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-6,  2 CLEAR TO WROWS()-2, WCOLS()-3
@ WROWS()-6,  3 SAY LEFT(CCC_0303.NAME, 60)
@ WROWS()-4,  7 SAY "Страна " + COUNTRY
@ WROWS()-4, 32 SAY "Строка N " + GTD_LINE
@ WROWS()-4, 47 SAY "Кол-во " + STR(QNT,8)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
