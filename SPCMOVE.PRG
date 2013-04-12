*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла SpcMove      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                   Движение по спецфилиалу за период.                   ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 03.01.2002 ══╝
PROCEDURE SpcMove
PARAMETERS prmCode

PRIVATE spcCode, letSpc, spcName, d1, d2
PRIVATE sum01, sum02, sum00
STORE 0 TO sum01, sum02, sum00
IF TYPE("prmCode") = "N"
  spcCode = prmCode
ELSE
  spcCode = 0
ENDIF
d1 = DATE()-6
d2 = DATE()

USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE SHARED AGAIN ALIAS SPC_LIST
spcName = SPACE(50)

letSpc = .T.
IF .NOT. EMPTY(spcCode)
  IF SEEK(spcCode)
    spcName = SPC_LIST.CUS_NAME
    letSpc = .F.
  ELSE
    spcCode = 0
  ENDIF
ENDIF

IF .NOT. FilBlank()
  USE
  RETURN
ENDIF
SELECT SPC_LIST
USE

DO Wt_Mess WITH "Подготовка данных"
DO PrepTmp
DO Wt_Mess

DO View_Res

USE
DELETE FILE (tmpo_path+"SPEC_TMP.DBF")
DELETE FILE (tmpo_path+"SPEC_TMP.CDX")
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура FilBlank     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Заполняем бланк                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.01.2002 ──┘
PROCEDURE FilBlank
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

*┌──────────────────────────────────────────────────────┐
*│ ┌─ [ ] Спецфилиал ─────────────────────────────────┐ │
*│ │....:....!....:....!....:....!....:....!....:....!│ │
*│ └──────────────────────────────────────────────────┘ │
*│  Начальная дата ДД.ММ.ГГГГ  Конечная дата ДД.ММ.ГГГГ │
*│               < OK Ctrl-W > < Отказаться Esc >       │
*└──────────────────────────────────────────────────────┘

PRIVATE ex, tmpSw
tmpSw = .F.
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 58, "Движение по спецфилиалу за период"
DO Sun_Bord WITH  2,  3,  4, 54, "                "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2,  6 GET tmpSw PICTURE "@*C Спецфилиал" WHEN letSpc VALID Get_Spec()
@ 3,  4 GET spcName WHEN .F.
@ 5,  3 SAY "Начальная дата" GET d1
@ 5, 30 SAY "Конечная дата" GET d2
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID TstBlank()

IF ex = 1
  *
  * Отрабатываем бланк
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Scpec    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Выбор спецфилиала                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.01.2002 ──┘
PROCEDURE Get_Spec

IF .NOT. tmpSw
  RETURN .T.
ENDIF

tmpSw = .F.

DO Spec_Fil WITH spcCode, spcName

SHOW GET spcName
SHOW GET tmpSw

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура TstBlank     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Проверка корректности бланка                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.01.2002 ──┘
PROCEDURE TstBlank

PRIVATE mss

IF ex # 1
  RETURN .T.
ENDIF

IF EMPTY(spcCode)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали спецфилиал"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура PrepTmp      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Подготовка временного файла                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.01.2002 ──┘
PROCEDURE PrepTmp

SELECT 0
DELETE FILE (tmpo_path+"SPEC_TMP.DBF")
DELETE FILE (tmpo_path+"SPEC_TMP.CDX")
CREATE DBF (tmpo_path+"SPEC_TMP") ;
  ( CODE     N( 7), ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    DOC_DATE D,     ;
    FIRM     C( 3), ;
    DOC_NUMV C(15), ;
    DOC_NUM  C(10), ;
    DOC_TYPE C( 6), ;
    QNT      N( 6), ;
    QNT_SPEC N( 6), ;
    PRICE_R  N(10,2),;
    PRICE_D  N(10,4),;
    WHO      C(28)  )

SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (base_path+"SALE") SHARED AGAIN ORDER TAG DOC_NUM ALIAS DETAIL
SET RELATION TO CODE INTO STOCK
SELECT 0
USE (base_path+"SALE_TIT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS TITLE
SET RELATION TO WHO INTO PERSONS
SEEK spcCode
SCAN REST WHILE CUS_CODE = spcCode FOR BETWEEN(DOC_DATE, d1, d2)
  SELECT DETAIL
  SEEK TITLE.FIRM+LEFT(DTOS(TITLE.DOC_DATE),4)+TITLE.DOC_NUM
  SCAN REST WHILE FIRM = TITLE.FIRM .AND. DOC_DATE = TITLE.DOC_DATE .AND. DOC_NUM = TITLE.DOC_NUM FOR QNT # 0
    SELECT SPEC_TMP
    APPEND BLANK
    REPLACE CODE     WITH DETAIL.CODE,    ;
            PREFIX   WITH STOCK.PREFIX,   ;
            NAME     WITH STOCK.NAME,     ;
            PRODUCER WITH STOCK.PRODUCER, ;
            DOC_DATE WITH TITLE.DOC_DATE, ;
            FIRM     WITH TITLE.FIRM,     ;
            DOC_NUMV WITH LEFT(TITLE.DOC_NUM,4)+"-"+ALLTRIM(SUBSTR(TITLE.DOC_NUM,5))+"/"+TITLE.FIRM, ;
            DOC_NUM  WITH TITLE.DOC_NUM,  ;
            DOC_TYPE WITH "Накл.",        ;
            QNT      WITH DETAIL.QNT,     ;
            PRICE_R  WITH STOCK.PRICE_R,  ;
            PRICE_D  WITH STOCK.PRICE,    ;
            WHO      WITH ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)
            sum01 = sum01 + IIF(QNT>0,PRICE_D*QNT,0)
            sum02 = sum02 + IIF(QNT<0,PRICE_D*QNT,0)
    SELECT DETAIL
  ENDSCAN
  SELECT TITLE
ENDSCAN

SELECT DETAIL
USE (base_path+"ACT_S") SHARED AGAIN ORDER TAG DOC_NUM ALIAS DETAIL
SET RELATION TO CODE INTO STOCK
SELECT TITLE
USE (base_path+"ACT_S_T") SHARED AGAIN ORDER TAG SUP_CODE ALIAS TITLE
SET RELATION TO WHO_ACT INTO PERSONS
SEEK spcCode
SCAN REST WHILE SUP_CODE = spcCode FOR BETWEEN(DOC_DATE, d1, d2)
  SELECT DETAIL
  SEEK TITLE.FIRM+LEFT(DTOS(TITLE.DOC_DATE),4)+TITLE.DOC_NUM
  SCAN REST WHILE FIRM = TITLE.FIRM .AND. DOC_DATE = TITLE.DOC_DATE .AND. DOC_NUM = TITLE.DOC_NUM FOR QNT_INC # 0
    SELECT SPEC_TMP
    APPEND BLANK
    REPLACE CODE     WITH DETAIL.CODE,    ;
            PREFIX   WITH STOCK.PREFIX,   ;
            NAME     WITH STOCK.NAME,     ;
            PRODUCER WITH STOCK.PRODUCER, ;
            DOC_DATE WITH TITLE.DOC_DATE, ;
            FIRM     WITH TITLE.FIRM,     ;
            DOC_NUMV WITH ALLTRIM(TITLE.DOC_NUM)+"/"+TITLE.FIRM, ;
            DOC_NUM  WITH TITLE.DOC_NUM,  ;
            DOC_TYPE WITH "Пр.акт.",      ;
            QNT      WITH -DETAIL.QNT_INC,;
            PRICE_R  WITH STOCK.PRICE_R,  ;
            PRICE_D  WITH STOCK.PRICE,    ;
            WHO      WITH ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+" "+LEFT(PERSONS.S_NAME,1)
            sum01 = sum01 + IIF(QNT>0,PRICE_D*QNT,0)
            sum02 = sum02 + IIF(QNT<0,PRICE_D*QNT,0)
    SELECT DETAIL
  ENDSCAN
  SELECT TITLE
ENDSCAN

sum00 = sum01 + sum02

SELECT 0
USE (base_path+"SPEC_SUM") SHARED AGAIN ORDER TAG POS_CUS

SELECT SPEC_TMP
SET RELATION TO STR(CODE,7)+STR(spcCode,8) INTO SPEC_SUM
REPLACE ALL QNT_SPEC WITH SPEC_SUM.QNT

SELECT SPEC_SUM
USE
SELECT DETAIL
USE
SELECT TITLE
USE
SELECT STOCK
USE
SELECT PERSONS
USE
SELECT SPEC_TMP
INDEX ON NAME+PREFIX+PRODUCER+DTOS(DOC_DATE) TAG NAME
GO TOP

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_Res     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Посмотр результата.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.01.2002 ──┘
PROCEDURE View_Res


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
SELECT SPEC_TMP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Info OF (menu_name) PROMPT "Информация"
ON PAD Info OF (menu_name) ACTIVATE POPUP Info

  DEFINE POPUP Info MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Info PROMPT "Движение" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Info DO Sw_Mode WITH "Move"

DEFINE PAD Print OF (menu_name) PROMPT "Записать" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*

*┌─────Позиция───────────────────────────────Дата────Кол-во──────Цена──Наличие┐
*│PPPP nnnnnnnnnNnnnnnnnnnNnnnnn ppppPppp ДД.ММ.ГГГГ 999999 9999999.99  999999│
*└────────────────────────────────────────────────────────────────────────────┘
*┌────────────────────────────────────────────────────────────────────┐
*│           Док-т  № 999999999999999 от ДД.ММ.ГГГГ                   │
*│         Автор                                                      │
*└────────────────────────────────────────────────────────────────────┘

PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 75

DO D_Wins WITH ln, wd, ALLTRIM(spcName), 1, 2
@ 1, 3 SAY "Период с "+DTOC(d1)+" по "+DTOC(d2)
@ 2, 2 SAY "─────Позиция───────────────────────────────Дата────Кол-во──────Цена─Наличие"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL F1 DO Sw_Mode WITH "Move"
    ON KEY LABEL Ctrl+P DO Sw_Mode WITH "Print"

*
*    BROWSE - меню
*
    BROWSE FIELDS PREFIX:H="",   ;
                  Brw_Name=Nm_Part(NAME):25:H="", ;
                  PRODUCER:H="", ;
                  DOC_DATE:H="", ;
                  QNT:H="",      ;
                  PRICE_D:H="",  ;
                  QNT_SPEC:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Exit")
    ENDIF

  CASE what_do = "Move"    && Движение

    DO Move_S WITH CODE
    what_do = "List"

  CASE what_do = "Print"   && Печать

    DO Spec_File
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
*┌──────────────────────────────────────────────────────────────────┐
*│         Док-т  № 999999999999999 от ДД.ММ.ГГГГ                   │
*│         Автор                                                    │
*│Приход 99999999.9999   Расход 99999999.9999   Итого 99999999.9999 │
*└──────────────────────────────────────────────────────────────────┘

@ WROWS()-3,  2 SAY DOC_TYPE+" № "+DOC_NUMV+" от "+DTOC(DOC_DATE)
@ WROWS()-3, 42 SAY "Автор "+WHO
@ WROWS()-2,  2 SAY "Приход "+STR(sum01,12,4)
@ WROWS()-2, 25 SAY "Расход "+STR(sum02,12,4)
@ WROWS()-2, 48 SAY "Итого "+STR(sum00,12,4)

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Файл в DBF-формате.                            ║
*║                                                                        ║
*╚═══════════════════════════════════════════════════════════ 09.01.2002 ═╝
PROCEDURE Spec_file

PRIVATE new_f
new_f = New_File("","DBF", "Куда пишем документ?", 0)

IF .NOT. EMPTY(new_f)
  DO Wt_Mess WITH "Запись..."
  new_f = ALLTRIM(new_f)
  DO Wt_Mess
  SELECT SPEC_TMP
  COPY TO (new_f)
  
ENDIF
RETURN