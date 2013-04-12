*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла PosFound     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Выбор позиции из найденных                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 27.10.2005 ══╝
PROCEDURE PosFound


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
PRIVATE ch_mode		  && .F. - выбираем из удаленных

ch_mode = .T.
*
*   Заполняем значениями переменные состояния...
*
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
PRIVATE tmpRet
m.tmpRet = 0
m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.path_comm+"SUBJECT") SHARED AGAIN ALIAS SUB_5A27 ORDER TAG STO_CODE
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ALIAS STO_5A27 ORDER TAG CODE
SELECT TMP_5A27
SET RELATION TO CODE INTO SUB_5A27, CODE INTO STO_5A27

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
m.ln = MIN(WROWS("")-11, RECCOUNT())   && Количество видимых строк BROWSE
m.wd = FSIZE("PREFIX")+FSIZE("NAME")+FSIZE("PRODUCER")+28

DO D_Wins WITH m.ln, m.wd, "Результат поиска", 0, 6
@ 1, 2 SAY ;
"─────Наименование──────────────────────────────────Кол-во───Корпус"
*pppp nnnnnnnnnNnnnnnnnnNnnnnnnnnnNnnnnn ppppPppp 99999999 ....:....!....:.
*"─────Наименование──────────────────────────────────Кол-во───Удалена"
*pppp nnnnnnnnnNnnnnnnnnNnnnnnnnnnNnnnnn ppppPppp 99999999 ДД.ММ.ГГГГ
SCROLL WROWS()-8, 0, WROWS()-7, WCOLS()-1, -1
@ WROWS()-7, 3 SAY "Функциональное назначение"
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS PREFIX:H="", NAME:H="", PRODUCER:H="",  ;
                  STO_5A27.QNT:H="":P="99999999",         ;
                  SUB_5A27.CORPUS:H="":16,				  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    IF TYPE("m.for_del") = "L"
      ch_mode = .NOT. m.for_del
    ENDIF
    IF ch_mode
      IF .NOT. EMPTY(STO_5A27.DATE_OFF)
        PRIVATE mss
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"Позиция удалена..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        LOOP
      ENDIF
    ELSE
      IF EMPTY(STO_5A27.DATE_OFF)
        PRIVATE mss
        DIMENSION mss[3]
        mss[1] = ""
        mss[2] = CHR(0)+"Позиция не удалена..."
        mss[3] = ""
        DO Out_Mess WITH 7, "mss"
        what_do = "List"
        LOOP
      ENDIF
    ENDIF
    m.tmpRet = LINK
    EXIT

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

USE IN SUB_5A27
USE IN STO_5A27
SELECT (m.s_sav)

RETURN m.tmpRet

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot
PRIVATE tmpDummy, lin, msDel

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
@ WROWS()-8,  2 SAY "Цена" GET STO_5A27.PRICE_R
@ WROWS()-8, 25 SAY "Нал. у поставщика" GET STO_5A27.QNT_STORE

IF FOUND("SUB_5A27")
  @ WROWS()-6, 2 EDIT SUB_5A27.FUNCTION SIZE 5, WCOLS()-4
ELSE
  m.tmpDummy = ""
  @ WROWS()-6, 2 EDIT m.tmpDummy SIZE 5, WCOLS()-4
ENDIF
CLEAR GETS
IF .NOT. EMPTY(DATE_OFF)
  m.msDel = "Удалена "+DTOC(DATE_OFF)
  m.lin   = FLOOR((WCOLS()-LEN(m.msDel))/2)
  @ WROWS()-1, m.lin SAY m.msDel COLOR SCHEME 7
ELSE
   @ WROWS()-1, 3 SAY REPLICATE("─", WCOLS()-6) COLOR (SCHEME(14, 2))
ENDIF

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
