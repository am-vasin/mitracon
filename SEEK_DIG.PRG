*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Seek_Dig     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                  Поиск по цифровой части наименования                  ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 14.04.2008 ══╝
PROCEDURE Seek_Dig
PARAMETERS digKey

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
PRIVATE tmpKey, i, c, retValue

m.retValue = 0
m.tmpKey = ""
FOR m.i = 1 TO LEN(m.digKey)
  c = SUBSTR(m.digKey, m.i, 1)
  IF m.c $ "0123456789"
    m.tmpKey = m.tmpKey+m.c
  ENDIF
ENDFOR

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
m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (path_comm+"SUBJECT") SHARED AGAIN ORDER TAG STO_CODE ALIAS DG_SUB
SELECT 0
USE (base_path+"CLASSES") SHARED AGAIN ORDER TAG CODE ALIAS DG_CLS
SELECT 0
USE (base_path+"STOCK") SHARED AGAIN ORDER TAG NUM_PART ALIAS DG_STO
SET RELATION TO CLASS INTO DG_CLS, CODE INTO DG_SUB
*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Filter OF (menu_name) PROMPT "Фильтр" KEY Ctrl-A, "Tab"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = 72	&&IIF(SET("CENTURY") = "ON", !!!!!!!!!,  !!!!!!!!!!)

DO D_Wins WITH m.ln, m.wd, "Поиск по цифровой части", 0, 1

@ 1, 2 SAY ;
"──Наименование────────────────────────Фирма──Кол-во────Цена───────Пост."
*....:....!....:....!....:....!....: AAAAAAAA 999999 999999.99999 999999

@ WROWS()-3, 3 SAY "Корпус"

m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Filter"

*
*    BROWSE - меню
*
    BROWSE KEY m.tmpKey			;
    FIELDS NAME:35:H="",		;
           PRODUCER:8:H="",		;
           QNT:H="":P="999999",	;
           PRICE_R:H="":P="9999999.99999",	;
           QNT_STORE:H="":P="999999",	;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
*

    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    m.retValue = RECNO()
    EXIT
*    m.what_do = "List"

  CASE m.what_do = "Filter"    && Фильтр

    IF EMPTY(SET("FILTER"))
      SET FILTER TO QNT # 0 .OR. QNT_STORE # 0
    ELSE
      SET FILTER TO
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
USE IN DG_CLS
USE IN DG_STO
USE IN DG_SUB
SELECT (m.s_sav)

RETURN m.retValue

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
@ WROWS()-2, 3 SAY DG_SUB.CORPUS

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
