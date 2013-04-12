*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла GTD          Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                         Коррекция архива ГТД.                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 03/02/2000 ══╝
PROCEDURE GTD

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

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_001 AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_002 AGAIN

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P_003 AGAIN

SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_NUM ALIAS DET_0303 AGAIN

SELECT 0
USE (path_comm+"GTD_C") ORDER TAG GTD_NUM ALIAS CLS_0303 AGAIN

SELECT 0
USE (path_comm+"GTD_TIT") ALIAS TIT_0302 AGAIN ORDER TAG HRON
SET RELATION TO GTD_NUM  INTO DET_0303
SET RELATION TO GTD_NUM  INTO CLS_0303 ADDITIVE
SET RELATION TO WHO_CR   INTO P_001 ADDITIVE
SET RELATION TO WHO_CORR INTO P_002 ADDITIVE
SET RELATION TO WHO_OFF  INTO P_003 ADDITIVE

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "Коррекция"
ON PAD Modify OF (menu_name) ACTIVATE POPUP Modify

  DEFINE POPUP Modify MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Modify PROMPT "Добавление ГТД" KEY Ctrl-A, "Ins"
  ON SELECTION BAR 1 OF Modify DO Sw_Mode WITH "Ins"

  DEFINE BAR  2 OF Modify PROMPT "Заголовок" KEY Ctrl-A, "Tab"
  ON SELECTION BAR 2 OF Modify DO Sw_Mode WITH "Title"

  DEFINE BAR  3 OF Modify PROMPT "Корреция позиций" KEY Ctrl-A, "Enter"
  ON SELECTION BAR 3 OF Modify DO Sw_Mode WITH "Modify"

  DEFINE BAR  4 OF Modify PROMPT "Корреция классов" KEY Ctrl-A, "Ctrl-Enter"
  ON SELECTION BAR 4 OF Modify DO Sw_Mode WITH "Classes"

  DEFINE BAR  5 OF Modify PROMPT "Удалить/Восст." KEY Ctrl-A, "Del"
  ON SELECTION BAR 5 OF Modify DO Sw_Mode WITH "Del"

DEFINE PAD Print OF (menu_name) PROMPT "Печать" KEY Ctrl-A, "F1"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Report OF (menu_name) PROMPT "Справка" KEY Ctrl-A, "F2"
ON SELECTION PAD Report OF (menu_name) DO Sw_Mode WITH "Report"

DEFINE PAD Seek OF (menu_name) PROMPT "Поиск"
ON PAD Seek OF (menu_name) ACTIVATE POPUP Seek

  DEFINE POPUP Seek MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Seek PROMPT "Поиск ГТД" KEY Ctrl-A, "F7"
  ON SELECTION BAR 1 OF Seek DO Sw_Mode WITH "Seek"

  DEFINE BAR  2 OF Seek PROMPT "Повтор поиска" KEY Ctrl-A, "Ctrl-F7"
  ON SELECTION BAR 2 OF Seek DO Sw_Mode WITH "Again"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-8   && Количество видимых строк BROWSE
wd = 43

*┌── Дата ──── Номер ─────────────────────П─К┐
*│ДД.ММ.ГГГГ NNNNNNNNNNNNNNNNNNNNNNNNNN X X X│
*└───────────────────────────────────────────┘
*┌───────────────────────────────────────────┐
*│ Созд. ДД.ММ.ГГГГ ФФФФФФФФФФФФФФФФФФФФФФФФ │
*│ Корр. ДД.ММ.ГГГГ ФФФФФФФФФФФФФФФФФФФФФФФФ │
*│ Удал. ДД.ММ.ГГГГ ФФФФФФФФФФФФФФФФФФФФФФФФ │
*└───────────────────────────────────────────┘

DO D_Wins WITH ln, wd, "Перечень ГТД", 0, 3
@ 1, 2 SAY "── Дата ──── Номер ─────────────────────П─К"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl-Enter DO Sw_Mode WITH "Classes"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Ins"
    ON KEY LABEL Tab   DO Sw_Mode WITH "Title"
    ON KEY LABEL Del   DO Sw_Mode WITH "Del"
    ON KEY LABEL F1    DO Sw_Mode WITH "Print"
    ON KEY LABEL F2    DO Sw_Mode WITH "Report"
    ON KEY LABEL F7    DO Sw_Mode WITH "Seek"
    ON KEY LABEL Ctrl+F7 DO Sw_Mode WITH "Again"

*
*    BROWSE - меню
*
    BROWSE FIELDS GTD_DATE:H="", GTD_NUM:H="", GTD_TYPE:H="", ;
           M001 = IIF(FOUND("DET_0303"), "√", " "):1:H= "", ;
           M002 = IIF(FOUND("CLS_0303"), "√", " "):1:H= "", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Foot() ;
           NOMODIFY    ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE what_do = "Modify"    && Коррекция содержимого ГТД

    IF .NOT. BOF() .AND. .NOT. EOF()
      IF Not_Del(TIT_0302.DATE_OFF)
        DO Modi_GTD
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Del"      && Удаление ГТД

    IF .NOT. BOF() .AND. .NOT. EOF()
      IF EMPTY(TIT_0302.DATE_OFF)
        REPLACE DATE_OFF WITH DATE(), WHO_OFF WITH user
      ELSE
        REPLACE DATE_OFF WITH {}, WHO_OFF WITH user
      ENDIF
      SELECT DET_0303
      SEEK TIT_0302.GTD_NUM
      SCAN REST WHILE TIT_0302.GTD_NUM = GTD_NUM
        REPLACE OFF WITH .NOT. OFF
      ENDSCAN
      SELECT TIT_0302
    ENDIF
    what_do = "List"

  CASE what_do = "Title"    && Коррекция заголовка ГТД


    IF .NOT. BOF() .AND. .NOT. EOF()
      IF Not_Del(TIT_0302.DATE_OFF)
        DO Add_GTD WITH .T.
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Classes"    && Коррекция содержимого ГТД

    IF .NOT. BOF() .AND. .NOT. EOF()
      IF Not_Del(TIT_0302.DATE_OFF)
        DO Modi_Cls
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Print"    && Печать

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_GTD WITH GTD_NUM
    ENDIF
    what_do = "List"

  CASE what_do = "Report"   && Справка

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO V_GTD WITH GTD_NUM
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
SELECT P_001
USE
SELECT P_002
USE
SELECT P_003
USE
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Add_GTD      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Добавление заголовка ГТД.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/02/2000 ──┘
PROCEDURE Add_GTD
PARAMETER old

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

PRIVATE ex, tmpNum, tmpDate, tmpFlg, old_num, old_date
IF old
  old_num = TIT_0302.GTD_NUM
  old_date = TIT_0302.GTD_NUM
  tmpNum  = TIT_0302.GTD_NUM
  tmpDate = TIT_0302.GTD_DATE
  tmpFlg  = .NOT. EMPTY(TIT_0302.GTD_TYPE)
ELSE
  old_num = SPACE(26)
  old_date = {}
  tmpNum  = SPACE(26)
  tmpDate = DATE()
  tmpFlg  = .F.
ENDIF
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 43, "Добавление ГТД"

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
  IF old
    REPLACE WHO_CORR WITH user, DATE_CORR WITH DATE()
    IF tmpNum # old_num
      SELECT DET_0303
      DO WHILE SEEK(old_num)
        REPLACE GTD_NUM WITH tmpNum
      ENDDO
      SELECT CLS_0303
      DO WHILE SEEK(old_num)
        REPLACE GTD_NUM WITH tmpNum
      ENDDO
    ENDIF
    IF tmpDate # old_date
      SELECT DET_0303
      SEEK(tmpNum)
      SCAN REST WHILE GTD_NUM = tmpNum
        REPLACE GTD_DATE WITH tmpDate
      ENDSCAN
      SELECT CLS_0303
      SEEK(tmpNum)
      SCAN REST WHILE GTD_NUM = tmpNum
        REPLACE GTD_DATE WITH tmpDate
      ENDSCAN
    ENDIF
    SELECT TIT_0302
  ELSE
    APPEND BLANK
    REPLACE WHO_CR WITH user, DATE_CR WITH DATE()
  ENDIF
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
IF old_num # tmpNum
  sw =  SEEK(tmpNum)
ELSE
  sw = .F.
ENDIF
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
*│   Процедура Modi_GTD     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Коррекция содержимого ГТД.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/03/2000 ──┘
PROCEDURE Modi_GTD

DO Prep_Tmp
DO Modi_Tmp
DO Save_GTD
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

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Modi_Pos WITH .T.

DEFINE PAD Modify OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Modi_Pos WITH .F.

DEFINE PAD Del OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Del_Pos

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
    ON KEY LABEL Enter DO Modi_Pos WITH .F.
    ON KEY LABEL Ins   DO Modi_Pos WITH .T.
    ON KEY LABEL Del   DO Del_Pos

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
*│   Процедура Modi_Pos     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                  Коррекция/добавление позиции в ГТД.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/06/2000 ──┘
PROCEDURE Modi_Pos
PARAMETERS new

IF .NOT. new .AND. (BOF() .OR. EOF())
  RETURN
ENDIF

PUSH KEY CLEAR       && На всякий пожарный случай!

SHOW WINDOW (win_name+"_H")

DEFINE WINDOW Modi_Pos FROM WLROW(win_name+"_M")+WROWS(win_name+"_M")-6,  ;
                            WLCOL(win_name+"_M")+2 TO ;
                            WLROW(win_name+"_M")+WROWS(win_name+"_M")-2,  ;
                            WLCOL(win_name+"_M")+WCOLS(win_name+"_M")-3 NONE COLOR SCHEME 13

PRIVATE ex, tmpName, swCountry, tmpCountry, tmpLine, tmpQnt
ex = 1
tmpName    = IIF(new, SPACE(FSIZE("NAME")), NAME)
swCountry  = .F.
tmpCountry = IIF(new, SPACE(FSIZE("COUNTRY")), COUNTRY)
tmpLine    = IIF(new, SPACE(FSIZE("GTD_LINE")), GTD_LINE)

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO WHILE .T.
  REPLACE MARK WITH ""
  SHOW WINDOW (ALIAS()) SAME REFRESH
  ACTIVATE WINDOW Modi_Pos
  tmpName = IIF(new, SPACE(FSIZE("NAME")), NAME)
  tmpQnt  = IIF(new, 0, QNT)
  @ 0, 10 SAY "Наименование" GET tmpName
  @ 2,  3 GET swCountry PICTURE "@*C Страна" VALID Get_C()
  @ 2, 14 GET tmpCountry WHEN .F.
  @ 2, 32 SAY "Строка N " GET tmpLine
  @ 2, 47 SAY "Кол-во" GET tmpQnt PICTURE "@Z 99999999"
  @ WROWS()-1, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

  READ CYCLE VALID Empty_P()

  REPLACE MARK WITH " "
  SHOW WINDOW (ALIAS()) SAME REFRESH
  IF ex = 1
    *
    * Отрабатываем бланк
    IF new
      APPEND BLANK
    ENDIF
    REPLACE NAME     WITH tmpName,    ;
            COUNTRY  WITH tmpCountry, ;
            GTD_LINE WITH tmpLine,    ;
            QNT      WITH tmpQnt
    IF .NOT. new
      EXIT
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDDO
*--------------------------------------------------------------------------
RELEASE WINDOW Modi_Pos

POP KEY
HIDE WINDOW (win_name+"_H")
SHOW MENU (menu_name)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_C        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Выбор страны.                              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.03.2000 ──┘
PROCEDURE Get_C

IF swCountry
  swCountry = .F.
  tmpCountry = Country(tmpCountry)
ENDIF

SHOW GET tmpCountry
SHOW GET swCountry

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Empty_P      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Выход из бланка.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.03.2000 ──┘
PROCEDURE Empty_P

PRIVATE mss, r_sav

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(tmpName)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Наименование позиции не может быть пустым!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

r_sav = RECNO()
IF .NOT. new
  SET FILTER TO RECNO() # r_sav
ENDIF
IF SEEK(tmpName)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Такая позиция уже есть!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  SET FILTER TO
  IF BETWEEN(r_sav, 1, RECCOUNT())
    GO r_sav
  ENDIF
  RETURN .F.
ENDIF
SET FILTER TO
IF BETWEEN(r_sav, 1, RECCOUNT())
  GO r_sav
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Del_Pos      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Удаление позиции.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.03.2000 ──┘
PROCEDURE Del_Pos

IF .NOT. BOF() .AND. .NOT. EOF()
  DELETE
  SKIP
  IF EOF()
    GO BOTTOM
  ENDIF
  SHOW WINDOW (ALIAS()) REFRESH
ENDIF

RETURN


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Save_GTD     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Сохранение содержимого ГТД.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.03.2000 ──┘
PROCEDURE Save_GTD

PRIVATE mss, d, k

DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"Сохранить изменения? [ Да ] [ Нет ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 2
  RETURN
ENDIF

d = SET("DELETED") = "ON"
SET DELETED OFF
SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_NUM ALIAS GTD_0303 AGAIN
DO WHILE SEEK(TIT_0302.GTD_NUM)
  REPLACE GTD_NUM WITH ""
  DELETE
ENDDO
SELECT TIT_0302
REPLACE WHO_CORR WITH user, DATE_CORR WITH DATE()
SELECT GTD_TMP
k = SPACE(FSIZE("GTD_NUM", "TIT_0302"))
SCAN FOR .NOT. DELETED()
  SELECT GTD_0303
  IF SEEK(k)
    RECALL
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE GTD_NUM  WITH TIT_0302.GTD_NUM, ;
          GTD_DATE WITH TIT_0302.GTD_DATE,;
          COUNTRY  WITH GTD_TMP.COUNTRY,  ;
          GTD_LINE WITH GTD_TMP.GTD_LINE, ;
          NAME     WITH GTD_TMP.NAME,     ;
          QNT      WITH GTD_TMP.QNT
  SELECT GTD_TMP
ENDSCAN

SELECT GTD_0303
USE

SELECT GTD_TMP

IF d
  SET DELETED ON
ENDIF

RETURN

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
*│   Процедура Modi_Cls     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Коррекция содержимого ГТД в части классов.              │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/03/2000 ──┘
PROCEDURE Modi_Cls

DO Prep_Cls
DO Corr_Cls
DO Save_Cls
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

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Modi_C WITH .T.

DEFINE PAD Modify OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Modi_C WITH .F.

DEFINE PAD Del OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Del_Pos

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
    ON KEY LABEL Enter DO Modi_C WITH .F.
    ON KEY LABEL Ins   DO Modi_C WITH .T.
    ON KEY LABEL Del   DO Del_Pos

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


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_C     Разработчик Андрей Васин                        │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                  Коррекция/добавление класса в ГТД.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03/06/2000 ──┘
PROCEDURE Modi_C
PARAMETERS new

IF .NOT. new .AND. (BOF() .OR. EOF())
  RETURN
ENDIF

PUSH KEY CLEAR       && На всякий пожарный случай!

SHOW WINDOW (win_name+"_H")

DEFINE WINDOW Modi_Pos FROM WLROW(win_name+"_M")+WROWS(win_name+"_M")-6,  ;
                            WLCOL(win_name+"_M")+2 TO ;
                            WLROW(win_name+"_M")+WROWS(win_name+"_M")-2,  ;
                            WLCOL(win_name+"_M")+WCOLS(win_name+"_M")-3 NONE COLOR SCHEME 13

*┌──────────────────────────────────────────────────────────────┐
*│ [ ] ....:....!....:....!....:....!....:....!....:....!....:  │
*│                                                              │
*│ [ ] Страна ....:....!....:.  Строка N 9999  Кол-во 99999999  │
*│                                                              │
*│              < OK Ctrl-W > < Отказаться Esc >                │
*└──────────────────────────────────────────────────────────────┘
PRIVATE ex, tmpName, swCountry, tmpCountry, tmpLine, tmpQnt, tmpClass, swName
ex = 1
tmpClass   = IIF(new, SPACE(55), LEFT(CCC_0303.NAME,55))
swName     = .F.
tmpName    = IIF(new, SPACE(12), CLASS)
swCountry  = .F.
tmpCountry = IIF(new, SPACE(FSIZE("COUNTRY")), COUNTRY)
tmpLine    = IIF(new, SPACE(FSIZE("GTD_LINE")), GTD_LINE)

*------------------------------------------------------------------------
*      Ввод полей бланка
*
DO WHILE .T.
  REPLACE MARK WITH ""
  SHOW WINDOW (ALIAS()) SAME REFRESH
  ACTIVATE WINDOW Modi_Pos
  tmpClass= IIF(new, SPACE(55), LEFT(CCC_0303.NAME,55))
  tmpName  = IIF(new, SPACE(12), CLASS)
  tmpQnt  = IIF(new, 0, QNT)
  
  @ 0,  3 GET swName PICTURE "@*C" VALID Get_Cl()
  @ 0,  7 GET tmpClass WHEN .F.
  @ 2,  3 GET swCountry PICTURE "@*C Страна" VALID Get_C()
  @ 2, 14 GET tmpCountry WHEN .F.
  @ 2, 32 SAY "Строка N " GET tmpLine
  @ 2, 47 SAY "Кол-во" GET tmpQnt PICTURE "@Z 99999999"
  @ WROWS()-1, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

  READ CYCLE VALID Empty_P()

  REPLACE MARK WITH " "
  SHOW WINDOW (ALIAS()) SAME REFRESH
  IF ex = 1
    *
    * Отрабатываем бланк
    IF new
      APPEND BLANK
    ENDIF
    REPLACE CLASS    WITH tmpName,    ;
            COUNTRY  WITH tmpCountry, ;
            GTD_LINE WITH tmpLine,    ;
            QNT      WITH tmpQnt
    GO RECNO()
    SHOW WINDOW (ALIAS()) SAME REFRESH
    IF .NOT. new
      EXIT
    ENDIF
  ELSE
    EXIT
  ENDIF
ENDDO
*--------------------------------------------------------------------------
RELEASE WINDOW Modi_Pos

POP KEY
HIDE WINDOW (win_name+"_H")
SHOW MENU (menu_name)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Cl       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Выбор класса.                                 │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.03.2000 ──┘
PROCEDURE Get_Cl

IF swName
  swName = .F.
  DO Ch_Class WITH tmpName, tmpClass
  tmpClass = LEFT(tmpClass, 55)
ENDIF

SHOW GET tmpClass
SHOW GET swName

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Save_Cls     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Сохранение классов ГТД.                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.03.2000 ──┘
PROCEDURE Save_Cls

PRIVATE mss, d, k

DIMENSION mss(3)
mss(1) = ""
mss(2) = CHR(0)+"Сохранить изменения? [ Да ] [ Нет ]"
mss(3) = ""
IF Out_Mess(5, "mss") = 2
  RETURN
ENDIF

d = SET("DELETED") = "ON"
SET DELETED OFF
SELECT 0
USE (path_comm+"GTD_C") ORDER TAG GTD_NUM ALIAS GTD_0303 AGAIN
DO WHILE SEEK(TIT_0302.GTD_NUM)
  REPLACE GTD_NUM WITH ""
  DELETE
ENDDO
SELECT TIT_0302
REPLACE WHO_CORR WITH user, DATE_CORR WITH DATE()
SELECT GTD_TMP
k = SPACE(FSIZE("GTD_NUM", "TIT_0302"))
SCAN FOR .NOT. DELETED()
  SELECT GTD_0303
  IF SEEK(k)
    RECALL
  ELSE
    APPEND BLANK
  ENDIF
  REPLACE GTD_NUM  WITH TIT_0302.GTD_NUM, ;
          GTD_DATE WITH TIT_0302.GTD_DATE,;
          COUNTRY  WITH GTD_TMP.COUNTRY,  ;
          GTD_LINE WITH GTD_TMP.GTD_LINE, ;
          CLASS    WITH GTD_TMP.CLASS,    ;
          QNT      WITH GTD_TMP.QNT
  SELECT GTD_TMP
ENDSCAN

SELECT GTD_0303
USE

SELECT GTD_TMP

IF d
  SET DELETED ON
ENDIF

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Not_Del      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Удалена ли ГТД?                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.08.2002 ──┘
PROCEDURE Not_Del

PARAMETER d_off

IF .NOT. EMPTY(d_off)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"ГТД удалена. Коррекция недопустима!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Foot         Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Подвал для списка ГТД.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.08.2002 ──┘
PROCEDURE Foot

*┌───────────────────────────────────────────┐
*│ Созд. ДД.ММ.ГГГГ ФФФФФФФФФФФФФФФФФФФФФФФФ │
*│ Корр. ДД.ММ.ГГГГ ФФФФФФФФФФФФФФФФФФФФФФФФ │
*│ Удал. ДД.ММ.ГГГГ ФФФФФФФФФФФФФФФФФФФФФФФФ │
*└───────────────────────────────────────────┘

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-4,  2 SAY "Созд. "+DTOC(DATE_CR)+" "+P_001.FAMILY
@ WROWS()-3,  2 SAY "Корр. "+DTOC(DATE_CORR)+" "+P_002.FAMILY
@ WROWS()-2,  2 SAY "Удал. "+DTOC(DATE_OFF)+" "+P_003.FAMILY

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
