*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Lett_Ans      Разработчик Андрей Васин                     ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                           Создание сообщения                           ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 21.10.2005 ══╝
PROCEDURE Lett_Ans
PARAMETER for_who
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

*------------------------------------------------------------------------
*    Открываем и создаем требуемые таблицы
*
PRIVATE tmpTit, tmpText, tmpDate, tmpH, tmpM, s_sav, tmp_addr
m.s_sav = SELECT()
SELECT 0
USE base_path+"PERSONS" SHARED AGAIN ALIAS PRS_5A28 ORDER TAG CODE
SEEK m.for_who
m.tmp_addr = ALLTRIM(FAMILY)+" "+LEFT(NAME, 1)+" "+LEFT(S_NAME, 1)
m.tmp_addr = PADR(m.tmp_addr, 50)
USE base_path+"LETT_OUT" SHARED AGAIN ALIAS LOUT_5A28
SELECT 0
m.tmpTit  = SPACE(32)
m.tmpText = ""
m.tmpDate = DATE()
m.tmpH    = LEFT(TIME(), 2)
m.tmpM    = SUBSTR(TIME(), 4, 2)

*┌───────────────────────────────────────────────────────────────┐
*│   Кому ....:....!....:....!....:....!....:....!....:....!     │
*│             Тема ....:....!....:....!....:....!..             │
*│               Прочитать ДД.ММ.ГГГГ F1 99ч 99м                 │
*│┌──Текст сообщения────────────────────────────────────────────┐│
*││                                                             ││
*││                                                             ││
*││                                                             ││
*││                                                             ││
*││                                                             ││
*││                                                             ││
*││                                                             ││
*│└─────────────────────────────────────────────────────────────┘│
*│               < OK Ctrl-W > < Отказаться Esc >                │
*└───────────────────────────────────────────────────────────────┘

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 16, 67, "Создание сообщения"
DO Sun_Bord WITH  5,  2, 13, 64, " Текст сообщения "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2,  5 SAY "Кому"
@  2, 10 SAY m.tmp_addr COLOR SCHEME 1
@  3, 15 SAY "Тема" GET m.tmpTit
@  4, 17 SAY "Прочитать" GET tmpDate PICTURE "@D"
@  4, COL()+1 SAY "(F1)" GET tmpH PICTURE "99"
@  4, COL() SAY "ч"
@  4, COL()+1 GET tmpM PICTURE "99"
@  4, COL() SAY "м"
@  6,  3 EDIT m.tmpText SIZE 7, 61
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

ON KEY LABEL F1 DO Ch_Date
READ CYCLE VALID Tst_Mess()
ON KEY LABEL F1

IF m.ex = 1
  *
  * Отрабатываем бланк
  DO Add_Lett
ENDIF

USE IN LOUT_5A28
SELECT (m.s_sav)

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN


*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Mess     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                Проверка корректности заполнения бланка                 │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.10.2005 ──┘
PROCEDURE Tst_Mess

PRIVATE mss

IF m.ex = 2
  RETURN .T.
ENDIF

IF EMPTY(m.tmpTit)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Тему сообщения следует указать..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

m.tmpH = CHRTRAN(PADL(ALLTRIM(m.tmpH), 2), " ", "0")
m.tmpM = CHRTRAN(PADL(ALLTRIM(m.tmpM), 2), " ", "0")
IF .NOT. BETWEEN(m.tmpH, "00", "23") .OR. ;
   .NOT. BETWEEN(m.tmpM, "00", "59")
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Неверно указано время доставки сообщения..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Add_Lett     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Добавление сообщения                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.10.2005 ──┘
PROCEDURE Add_Lett

PRIVATE s_sav, tmp_ID, tmpTime

m.s_sav = SELECT()

SELECT 0
USE base_path+"LETT_OUT" SHARED AGAIN ALIAS OUT_5A28A
m.tmp_ID = DOC_NUM("LETT_ID", "", m.arm, DATE())
m.tmp_ID = SUBSTR(DTOS(DATE()), 3, 2)+STR(m.tmp_ID, 10)
APPEND BLANK
REPLACE MESS_ID		WITH m.tmp_ID, ;
        TITLE		WITH m.tmpTit, ;
        MESS_TEXT	WITH m.tmpText,;
        CR_DATE		WITH DATE(),   ;
        CR_TIME		WITH TIME(),   ;
        WHO			WITH m.user
USE base_path+"LETT_IN" SHARED AGAIN ALIAS IN_5A28A ORDER TAG MESS_ID
m.tmpTime = m.tmpH+":"+m.tmpM+":00"
SELECT IN_5A28A
GO TOP
IF .NOT. EMPTY(MESS_ID)
  APPEND BLANK
ENDIF
REPLACE MESS_ID		WITH m.tmp_ID, ;
        ADDRESS_T	WITH " ",      ;
        ADDRESS		WITH STR(m.for_who, 5), ;
        SHOW_DATE	WITH m.tmpDate,;
        SHOW_TIME	WITH m.tmpTime

USE IN IN_5A28A

SELECT (m.s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Date      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Вызов календаря                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.10.2005 ──┘
PROCEDURE Ch_Date

m.tmpDate = Get_Date(m.tmpDate)

SHOW GET m.tmpDate

RETURN
