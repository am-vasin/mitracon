*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла BoxesTmp     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                     Список коробок для накладной.                      ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 12.09.2000 ══╝
PROCEDURE BoxesTmp
PARAMETERS start_no
PRIVATE s_sav

s_sav = SELECT()

IF .NOT. USED("BOXNO")   && Временного файла еще нет: создать!
  SELECT 0
  USE (path_as+"MATERS")
  SELECT 0
  USE (path_as+"BOXNO")
  SELECT 0
  DELETE FILE (tmpo_path+"BOXESTMP.DBF")
  CREATE DBF (tmpo_path+"BOXESTMP.DBF") (REF_NO N(10))
  SET RELATION TO REF_NO INTO BOXNO
ELSE
  SELECT BOXESTMP
ENDIF
IF RECCOUNT() = 0
  DO Modi_Box WITH .T.
ENDIF
IF RECCOUNT() = 0
  SELECT (s_sav)
  RETURN
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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
SELECT 0
DO Use_Dummy
SELECT BOXESTMP
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Modify OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Tab"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Create OF (menu_name) PROMPT "Создать" KEY Ctrl-A, "Ins"
ON SELECTION PAD Create OF (menu_name) DO Sw_Mode WITH "Create"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-18   && Количество видимых строк BROWSE
wd = 25

DO D_Wins WITH ln, wd, "Список коробок", 0, 0
@ 1, 2 SAY "Коробка N────Маркировка"
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab DO Sw_Mode WITH "Modify"
    ON KEY LABEL Ins DO Sw_Mode WITH "Create"

*
*    BROWSE - меню
*
    BROWSE FIELDS ;
           BOXID   = ALLTRIM(BOXNO.OFFICE)+ALLTRIM(STR(BOXNO.BOXNO)):12, ;
           BOXMARK = IIF(EMPTY(BOXNO.MASK),  ;
                     ALLTRIM(BOXNO.OFFICE)+ALLTRIM(STR(BOXNO.BOXNO)), ;
                     BOXNO.MASK):12,  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")

    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Choose", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Просмотр списка

    EXIT

  CASE what_do = "Modify"    && Коррекция коробки

    DO Modi_Box
    what_do = "List"

  CASE what_do = "Create"    && Создание коробки

    DO Modi_Box WITH .T.
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
*   Нет! В данном случае не закрываем!
*
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Modi_Box     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Коррекция/создание коробки.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 12.09.2000 ──┘
PROCEDURE Modi_Box
PARAMETERS new

PRIVATE new_n
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
* Коробка OOOOOOOO9999
*┌──────────────────────────────────┐
*│                                  │
*│ Маркировка 9999999999  Вес 999.9 │
*│                                  │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex, tmpWeight, tmpMask
ex = 1
IF new
  tmpWeight = 0
  tmpMask   = "          "
ELSE
  tmpWeight = BOXNO.WEIGHT
  tmpMask   = BOXNO.MASK
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 38, IIF(new, "Новая коробка", "Коробка "+ALLTRIM(BOXNO.OFFICE)+ALLTRIM(STR(BOXNO.BOXNO)))

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  3 SAY "Маркировка" GET tmpMask
@  3, 26 SAY "Вес" GET tmpWeight PICTUR "999.9"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  IF new
    PRIVATE tmpCus, tmpSup, tmpCusO, tmpSupO
    IF "ACT_S_T.DBF" $ UPPER(DBF("SOURCE_T"))   && Приемный акт
      tmpCus = SOURCE_T.SUP_CODE
      tmpSup = sup_own
    ELSE
      tmpCus = SOURCE_T.CUS_CODE
      tmpSup = SOURCE_T.SUP_CODE
    ENDIF
    SELECT 0
    STORE "" TO tmpCusO, tmpSupO
    USE (path_as+"OFFICE") ORDER TAG CODE
    IF SEEK(tmpCus)
      tmpCusO = NAME
    ENDIF
    IF SEEK(tmpSup)
      tmpSupO = NAME
    ENDIF
    USE
    SELECT MATERS
    new_n = BOX_MT
    REPLACE BOX_MT WITH BOX_MT+1
    FLUSH
    SELECT BOXNO
    APPEND BLANK
    REPLACE BOXNO     WITH new_n,         ;
            OFFICE    WITH MATERS.OFFICE, ;
            BOX_YEAR  WITH YEAR(DATE()),  ;
            DATE      WITH DATE(),        ;
            DATE_MOVE WITH DATE(),        ;
            MOVEST    WITH "0",           ;
            NAKL_TYPE WITH "I",           ;
            NAKL_NUM  WITH IIF(TYPE("SOURCE_T.DOC_NUM") = "N", STR(SOURCE_T.DOC_NUM, 10), SOURCE_T.DOC_NUM),  ;
            NAKL_DATE WITH SOURCE_T.DOC_DATE, ;
            SUP_CODE  WITH tmpSup,        ;
            CUS_CODE  WITH tmpCus,        ;
            SUP_OFFIC WITH tmpSupO,       ;
            CUS_OFFIC WITH tmpCusO,       ;
            TECHNID   WITH SOURCE_T.TECHNID,;
            TECHNTP   WITH SOURCE_T.TECHNTP
    new_n = RECNO()
    SELECT BOXESTMP
    APPEND BLANK
    REPLACE REF_NO WITH new_n
  ENDIF
  SELECT BOXNO
  REPLACE WEIGHT WITH tmpWeight, MASK WITH tmpMask
  SELECT BOXESTMP
ENDIF
SELECT BOXESTMP
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
