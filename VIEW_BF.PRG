*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла View_BF      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Счета-фактуры от платежей                        ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 10.01.2009 ══╝
PROCEDURE View_BF
PARAMETERS prmCus 

PRIVATE tmpKey
m.tmpKey = IIF(EMPTY(m.prmCus), "", STR(m.prmCus))
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
USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS PB_ACC
SELECT 0
USE (m.base_path+"INCMONEY") SHARED AGAIN ORDER TAG DOC_NUM ALIAS PB_MON
SELECT 0
USE (m.base_path+"SALE_TIT") SHARED AGAIN ORDER TAG DOC_NUM ALIAS PB_SAL
SELECT 0
USE (m.base_path+"INCMONEY") SHARED AGAIN ORDER TAG DOC_NUM ALIAS PB_SRC

SELECT 0
IF EMPTY(m.tmpKey)
  USE (m.base_path+"PAY_BILL") SHARED AGAIN ORDER TAG HRON DESCENDING
ELSE
  USE (m.base_path+"PAY_BILL") SHARED AGAIN ORDER TAG CUS_CODE DESCENDING
ENDIF
SET RELATION TO	CUS_CODE INTO PB_ACC, ;
				FIRM+"1"+PAY_YEAR+PAY_DOC INTO PB_SRC, ;
				FIRM+"1"+REV_YEAR+REV_DOC INTO PB_MON, ;
				FIRM+REV_YEAR+REV_DOC     INTO PB_SAL

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Печать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = 69
DO D_Wins WITH m.ln, m.wd, "Счета фактуры по платежам", 0, 2
@ 1, 2 SAY "Фрм──Док-т №───────────Дата──────────Сумма─────Исх. платеж────Дата─"
* ФФФ 12345678901/999 ДД.ММ.ГГГГ 999999999999.99 12345678901 ДД.ММ.ГГГГ
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
    BROWSE KEY m.tmpKey REST ;
           FIELDS FIRM:H="",  ;
                  D1=Show_Doc(DOC_NUM, SUB_DOC):15:H="", ;
                  DOC_DATE:H="", ;
                  BILL_SUM:H="":P="999999999999.99", ;
                  D2=LEFT(PB_SRC.DOC_NUM,1)+"-"+ALLTRIM(SUBSTR(PB_SRC.DOC_NUM,2)):11:H="", ;
                  PB_SRC.DOC_DATE:H="",	;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE m.what_do = "Print"    && Печать

    WAIT WINDOW "Печать"
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
@ WROWS()-3, 3 SAY "Клиент"
@ WROWS()-3, COL()+1 SAY PB_ACC.CUS_NAME COLOR SCHEME 1

DO CASE
CASE REV_TYPE = " "
  @ WROWS()-2, 2 SAY SPACE(WCOLS()-4)
CASE REV_TYPE = "S"
  @ WROWS()-2, 2 SAY " Продажа № "+LEFT(PB_SAL.NEW_DOC, 1)+"-"+ALLTRIM(SUBSTR(PB_SAL.NEW_DOC, 2))+ ;
                     " от "+DTOC(PB_SAL.DOC_DATE)+" сумма "+ALLTRIM(TRANSFORM(PB_SAL.TOTAL_R, "999 999 999.99"))
CASE REV_TYPE = "M"
  @ WROWS()-2, 2 SAY " Платеж № "+LEFT(PB_MON.DOC_NUM, 1)+"-"+ALLTRIM(SUBSTR(PB_MON.DOC_NUM, 2))+ ;
                     " от "+DTOC(PB_MON.DOC_DATE)+" сумма "+ALLTRIM(TRANSFORM(PB_MON.MONEY_R, "999 999 999.99"))
ENDCASE

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Show_Doc     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Номер документа для печати                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 11.01.2009 ──┘
PROCEDURE Show_Doc
PARAMETERS tmpDoc, tmpSub

PRIVATE res
m.res = LEFT(m.tmpDoc,2)+"-"+ALLTRIM(SUBSTR(m.tmpDoc,3))
IF .NOT. EMPTY(m.tmpSub)
  m.res = m.res+"/"+ALLTRIM(m.tmpSub)
ENDIF

RETURN m.res
