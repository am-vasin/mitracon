*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла V_B_Prc      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Закупочные цены по партиям.                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 13.10.2006 ══╝
PROCEDURE V_B_Prc
PARAMETERS prmCode	&& Код позиции


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
m.win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS ACC_61013
SELECT 0
USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS STO_61013
SEEK m.prmCode
SELECT 0
USE (m.base_path+"ACT_S_T") SHARED AGAIN ORDER TAG DOC_NUM ALIAS TIT_61013
SET RELATION TO SUP_CODE INTO ACC_61013
SELECT 0
USE (m.base_path+"ACT_S") SHARED AGAIN ORDER TAG SALE ALIAS DET_61013
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE), 4)+DOC_NUM INTO TIT_61013

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Filter OF (menu_name) PROMPT "Все партии" KEY Ctrl-A, "Tab"
ON SELECTION PAD Filter OF (menu_name) DO Sw_Mode WITH "Filter"

DEFINE PAD Price OF (menu_name) PROMPT "Цены"
ON PAD Price OF (menu_name) ACTIVATE POPUP Price

  DEFINE POPUP Price MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Price PROMPT "√ В валюте документа" KEY Ctrl-A, "F1"
  ON SELECTION BAR 1 OF Price DO Sw_Mode WITH "Price1"

  DEFINE BAR  2 OF Price PROMPT "  В рублях" KEY Ctrl-A, "F2"
  ON SELECTION BAR 2 OF Price DO Sw_Mode WITH "Price2"

  DEFINE BAR  3 OF Price PROMPT "  В У Е" KEY Ctrl-A, "F3"
  ON SELECTION BAR 3 OF Price DO Sw_Mode WITH "Price3"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-20   && Количество видимых строк BROWSE
m.wd = 62

*  Закупочная цена для позиции
* Позиция PPPP nnnnnnnnnNnnnnnnnnnNnnnnnnnnnN ppppPppp
* Фрм───№ док-та─────Дата─────Приход──Остаток────────Цена──────Р
* FRM NNNNNNNNNN 99.99.9999 99999999 99999999 9999999999.9999V 
DO D_Wins WITH m.ln, m.wd, "Закупочные цены по партиям", 1, 1
@ 1, 2 SAY "Позиция "+IIF(EMPTY(STO_61013.PREFIX), "", ALLTRIM(STO_61013.PREFIX)+" ")+ ;
                     ALLTRIM(STO_61013.NAME)+ ;
                     IIF(EMPTY(STO_61013.PRODUCER), "", +" /"+ALLTRIM(STO_61013.PRODUCER)+"/")
@ 2, 2 SAY "Фрм───№ док-та─────Дата─────Приход──Остаток────────Цена──────Р"
m.what_do = "List"

PRIVATE tmpKey, prcType
m.tmpKey = STR(STO_61013.CODE, 7)
m.prcType = 1

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Tab DO Sw_Mode WITH "Filter"
    ON KEY LABEL F1  DO Sw_Mode WITH "Price1"
    ON KEY LABEL F2  DO Sw_Mode WITH "Price2"
    ON KEY LABEL F3  DO Sw_Mode WITH "Price3"

*
*    BROWSE - меню
*
    BROWSE REST KEY m.tmpKey ;
           FIELDS FIRM:H="", ;
                  DOC_NUM:H= "",  ;
                  DOC_DATE:H= "", ;
                  QNT_INC:P="@Z 99999999":H= "",  ;
                  QNT_REST:P="@Z 99999999":H= "", ;
                  PRC_FIELD = ShowPrc():16:H= "", ;
                  IS_REAL   = IIF(TIT_61013.FLG_BUY = "R", "√", " "):1:H="", ;
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

    m.what_do = "List"

  CASE m.what_do = "Filter"    && Фильтр

    IF TYPE("m.tmpKey") = "C"
      DEFINE PAD Filter OF (menu_name) PROMPT "Непустые партии" KEY Ctrl-A, "Tab" BEFORE PRice
      m.tmpKey = STO_61013.CODE
      SET ORDER TO TAG CODE
    ELSE
      DEFINE PAD Filter OF (menu_name) PROMPT "Все партии" KEY Ctrl-A, "Tab" BEFORE PRice
      m.tmpKey = STR(STO_61013.CODE, 7)
      SET ORDER TO TAG SALE
    ENDIF  
    m.what_do = "List"

  CASE m.what_do = "Price1"    && Цены в валюте документа

    m.prcType = 1
    DEFINE BAR  1 OF Price PROMPT "√ В валюте документа" KEY Ctrl-A, "F1"
    DEFINE BAR  2 OF Price PROMPT "  В рублях" KEY Ctrl-A, "F2"
    DEFINE BAR  3 OF Price PROMPT "  В У Е" KEY Ctrl-A, "F3"
    m.what_do = "List"

  CASE m.what_do = "Price2"    && Цены в рублях

    m.prcType = 2
    DEFINE BAR  1 OF Price PROMPT "  В валюте документа" KEY Ctrl-A, "F1"
    DEFINE BAR  2 OF Price PROMPT "√ В рублях" KEY Ctrl-A, "F2"
    DEFINE BAR  3 OF Price PROMPT "  В У Е" KEY Ctrl-A, "F3"
    m.what_do = "List"

  CASE m.what_do = "Price3"    && Цены в USD

    m.prcType = 3
    DEFINE BAR  1 OF Price PROMPT "  В валюте документа" KEY Ctrl-A, "F1"
    DEFINE BAR  2 OF Price PROMPT "  В рублях" KEY Ctrl-A, "F2"
    DEFINE BAR  3 OF Price PROMPT "√ В У Е" KEY Ctrl-A, "F3"
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
USE IN ACC_61013
USE IN STO_61013
USE IN TIT_61013
USE IN DET_61013

SELECT (m.s_sav)

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
@ WROWS()-2, 3 SAY ACC_61013.CUS_NAME

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура ShowPrc      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Заполняем поле цены                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 13.10.2006 ──┘
PROCEDURE ShowPrc

PRIVATE retVal

m.retVal = SPACE(16)
DO CASE
CASE m.prcType = 1		&& В валюте документа
  DO CASE
  CASE DET_61013.PRICE_R = 0 .AND. DET_61013.PRICE_D = 0 .AND. TIT_61013.FLG_BUY = "R"
    m.retVal = STR(STO_61013.PRICE_B, 15, 4)+"$"
  CASE TIT_61013.VALUTA = "Р"
    m.retVal = STR(DET_61013.PRICE_R, 15, 4)+"р"
  CASE TIT_61013.VALUTA = "$"
    m.retVal = STR(DET_61013.PRICE_D, 15, 4)+"$"
  ENDCASE
CASE m.prcType = 2		&& В рублях
  DO CASE
  CASE DET_61013.PRICE_R = 0 .AND. DET_61013.PRICE_D = 0 .AND. TIT_61013.FLG_BUY = "R"
    m.retVal = STR(STO_61013.PRICE_B*m.kurs, 15, 4)+"р"
  CASE TIT_61013.VALUTA = "Р"
    m.retVal = STR(DET_61013.PRICE_R, 15, 4)+"р"
  CASE TIT_61013.VALUTA = "$"
    m.retVal = STR(DET_61013.PRICE_D*m.kurs, 15, 4)+"р"
  ENDCASE
CASE m.prcType = 3		&& В USD
  DO CASE
  CASE DET_61013.PRICE_R = 0 .AND. DET_61013.PRICE_D = 0 .AND. TIT_61013.FLG_BUY = "R"
    m.retVal = STR(STO_61013.PRICE_B, 15, 4)+"$"
  CASE TIT_61013.VALUTA = "Р"
    m.retVal = STR(DET_61013.PRICE_R/m.kurs, 15, 4)+"$"
  CASE TIT_61013.VALUTA = "$"
    m.retVal = STR(DET_61013.PRICE_D, 15, 4)+"$"
  ENDCASE
ENDCASE

RETURN m.retVal
