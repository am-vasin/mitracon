*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла OptKoeff     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║            Установка коэффициентов для расчета оптовых цен             ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 08.05.2006 ══╝
PROCEDURE OptKoeff

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
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = "kff"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
m.s_sav = SELECT()
PRIVATE tmpName, maxBound
m.maxBound = 9999999999.9999
m.tmpName = SYS(3)

SELECT 0
USE (m.base_path+"OPTKOEFF") SHARED AGAIN ALIAS KFF_6508 ORDER TAG PRC_BOUND
COPY TO (m.tmpo_path+m.tmpName) WITH CDX
USE (m.tmpo_path+m.tmpName) SHARED AGAIN ALIAS KFF_6508 ORDER TAG PRC_BOUND

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Modify OF (menu_name) PROMPT "Корректировать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Modify OF (menu_name) DO Sw_Mode WITH "Modify"

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = 24

DO D_Wins WITH m.ln, m.wd, "Коэфф. для расч. опт. цен", 0, 0
@ 1, 2 SAY ;
"───Цена до ($)───Коэфф."
*9999999999.9999 999.9999
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Del"

*
*    BROWSE - меню
*
    BROWSE FIELDS PPP=PRC_B(PRC_BOUND):15:H="", ;
                  PRC_K:H="":P="999.9999" ;
                  FREEZE PRC_K ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Modify")
    ENDIF

  CASE m.what_do = "Modify"    && Коррекция

    DO ModiLine
    m.what_do = "List"

  CASE m.what_do = "Add"       && Добавление

    DO ModiLine WITH .T.
    m.what_do = "List"

  CASE m.what_do = "Del"       && Удаление

    IF PRC_BOUND = m.maxBound
      DIMENSION mss[3]
      mss[1] = ""
      mss[2] = CHR(0)+"Эту строку нельзя удалять"
      mss[3] = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      DELETE
    ENDIF
    m.what_do = "List"

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
PRIVATE tmpArr
SELECT 0
SET DELETED OFF
USE (m.base_path+"OPTKOEFF") SHARED AGAIN ALIAS RS_6508
SELECT KFF_6508
SCAN FOR .NOT. DELETED()
  SCATTER TO tmpArr
  SELECT RS_6508
  IF EOF()
    APPEND BLANK
  ELSE
    RECALL
  ENDIF
  GATHER FROM tmpArr
  SKIP
  SELECT KFF_6508
ENDSCAN
USE
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
SELECT RS_6508
IF .NOT. EOF()
  DELETE REST
ENDIF
SET DELETED ON
USE
SELECT (m.s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prc_B        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Цена до...                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 08.05.2006 ──┘
PROCEDURE Prc_B
PARAMETERS prmPrc 


IF m.prmPrc = m.maxBound
  RETURN "               "
ENDIF

RETURN TRANSFORM(m.prmPrc, "9999999999.9999")

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура ModiLine     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Коррекция/добавление строки                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 08.05.2006 ──┘
PROCEDURE ModiLine
PARAMETER addMode

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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌─────────────────────────────────────┐
*│                                     │
*│   Граница цен ($) 9999999999.9999   │
*│                                     │
*│        Коэффициент 999.9999         │
*│                                     │
*│  < OK Ctrl-W > < Отказаться Esc >   │
*└─────────────────────────────────────┘

PRIVATE ex, tmpB, tmpK, letMax
m.ex = 1
IF m.addMode
  STORE 0 TO m.tmpB, m.tmpK
ELSE
  m.tmpB = PRC_BOUND
  m.tmpK = PRC_K
ENDIF
m.letMax = PRC_BOUND=m.maxBound
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 41, "Коэффициент для ценовой группы"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  5 SAY "Граница цен ($) "
IF m.tmpB < m.maxBound
  @  3, COL() GET m.tmpB PICTURE "@Z 9999999999.9999"
ENDIF
@  5, 10 SAY "Коэффициент" GET m.tmpK PICTURE "@Z 999.9999"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID TstBound()

IF m.ex = 1
  *
  * Отрабатываем бланк
  IF m.addMode
    APPEND BLANK
  ENDIF
  REPLACE PRC_BOUND WITH m.tmpB, PRC_K WITH m.tmpK
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура TstBound     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Проверка корректности бланка                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 08.05.2006 ──┘
PROCEDURE TstBound

PRIVATE mss

IF m.ex # 1
  RETURN .T.
ENDIF

IF m.tmpB <= 0 .OR. m.tmpB > 9999999999 .AND. .NOT. m.letMax
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Ценовая граница должна быть больше нуля и меньше 9999999999"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF m.tmpK < 0
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Коэффициент должен быть больше или равен нулю."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
