*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Lett_Cr      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                           Создание сообщения                           ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 21.10.2005 ══╝
PROCEDURE Lett_Cr
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
PRIVATE tmpPers, tmpArm, tmpType, tmpTit, tmpText, tmpDate, tmpH, tmpM, s_sav
m.s_sav = SELECT()
SELECT 0
USE base_path+"LETT_OUT" SHARED AGAIN ALIAS LOUT_5A21
SELECT 0
m.tmpPers = SYS(3)
m.tmpArm = SYS(3)
DO WHILE m.tmpPers = m.tmpArm
  m.tmpArm = SYS(3)
ENDDO
CREATE DBF (m.tmpo_path+m.tmpPers) ;
	(	CODE	N( 5), ;
		MARK	C( 1), ;
		NAME	C(30)  )
INDEX ON SYS(15, m.lwr, NAME) TAG NAME FOR .NOT. EMPTY(MARK)
INDEX ON CODE TAG CODE
USE (m.tmpo_path+m.tmpPers) EXCLUSIVE ALIAS TMP_PERS ORDER TAG CODE

SELECT 0
CREATE DBF (m.tmpo_path+m.tmpArm) ;
	(	CODE	C( 3), ;
		MARK	C( 1), ;
		NAME	C(30)  )
INDEX ON SYS(15, m.lwr, NAME) TAG NAME FOR .NOT. EMPTY(MARK)
INDEX ON CODE TAG CODE
USE (m.tmpo_path+m.tmpArm) EXCLUSIVE ALIAS TMP_ARM ORDER TAG CODE
m.tmpType = 1
m.tmpTit  = SPACE(32)
m.tmpText = ""
m.tmpDate = DATE()
m.tmpH    = LEFT(TIME(), 2)
m.tmpM    = SUBSTR(TIME(), 4, 2)

*┌───────────────────────────────────────────────────────────────┐
*│             Тема ....:....!....:....!....:....!..             │
*│ ( ) Кому ....:....!....:....!....:....!....:....!....:....!  │
*│ ( ) Куда ....:....!....:....!....:....!....:....!....:....!  │
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

PRIVATE ex, tmpRec
m.ex = 1
m.tmpRec = 0
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 17, 67, "Создание сообщения"
DO Sun_Bord WITH  6,  2, 14, 64, " Текст сообщения "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@  2, 15 SAY "Тема" GET m.tmpTit
@  3,  3 GET m.tmpType PICTURE "@*RV Кому;Куда" VALID Get_Addr()
@  5, 17 SAY "Прочитать" GET tmpDate PICTURE "@D"
@  5, COL()+1 SAY "(F1)" GET tmpH PICTURE "99"
@  5, COL() SAY "ч"
@  5, COL()+1 GET tmpM PICTURE "99"
@  5, COL() SAY "м"
@  7,  3 EDIT m.tmpText SIZE 7, 61
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

ON KEY LABEL F1 DO Ch_Date
READ CYCLE VALID Tst_Mess()
ON KEY LABEL F1

IF m.ex = 1
  *
  * Отрабатываем бланк
  DO Add_Lett
ENDIF

USE IN LOUT_5A21
USE IN TMP_ARM
USE IN TMP_PERS
DELETE FILE (tmpo_path+m.tmpPers+".dbf")
DELETE FILE (tmpo_path+m.tmpPers+".cdx")
DELETE FILE (tmpo_path+m.tmpArm+".dbf")
DELETE FILE (tmpo_path+m.tmpArm+".cdx")
SELECT (m.s_sav)

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.tmpRec

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tmp_Addr     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Кому или куда...                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 21.10.2005 ──┘
PROCEDURE Get_Addr
PRIVATE tmpLin

m.tmpLin = SPACE(52)
IF m.tmpType = 1
  @ 4, 12 SAY m.tmpLin
  SELECT TMP_PERS
  SET ORDER TO TAG CODE
  DO Lst_Pers WITH "TMP_PERS"
  SET ORDER TO TAG NAME
  GO TOP
  IF .NOT. EOF() .AND. .NOT. BOF()
    m.tmpLin = PADR(NAME, 50)
    SKIP
    m.tmpLin = m.tmpLin+IIF(.NOT. EOF(), " ", "  ")
  ENDIF
  @ 3, 12 SAY m.tmpLin COLOR SCHEME 1
ELSE
  @ 3, 12 SAY m.tmpLin
  SELECT TMP_ARM
  SET ORDER TO TAG CODE
  DO Lst_Arm WITH "TMP_ARM"
  SET ORDER TO TAG NAME
  GO TOP
  IF .NOT. EOF() .AND. .NOT. BOF()
    m.tmpLin = PADR(NAME, 50)
    SKIP
    m.tmpLin = m.tmpLin+IIF(.NOT. EOF(), " ", "  ")
  ENDIF
  @ 4, 12 SAY m.tmpLin COLOR SCHEME 1
ENDIF  

RETURN .T.

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

IF m.tmpType = 1
  SELECT TMP_PERS
  SET ORDER TO TAG NAME
  GO TOP
  IF EOF() .OR. BOF()
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"Вы забыли указать адресата..."
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
ELSE
  SELECT TMP_ARM
  SET ORDER TO TAG NAME
  GO TOP
  IF EOF() .OR. BOF()
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"Вы забыли указать адресата..."
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
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
USE base_path+"LETT_OUT" SHARED AGAIN ALIAS OUT_5A21A
m.tmp_ID = DOC_NUM("LETT_ID", "", m.arm, DATE())
m.tmp_ID = SUBSTR(DTOS(DATE()), 3, 2)+STR(m.tmp_ID, 10)
APPEND BLANK
REPLACE MESS_ID		WITH m.tmp_ID, ;
        TITLE		WITH m.tmpTit, ;
        MESS_TEXT	WITH m.tmpText,;
        CR_DATE		WITH DATE(),   ;
        CR_TIME		WITH TIME(),   ;
        WHO			WITH m.user
m.tmpRec = RECNO()
USE base_path+"LETT_IN" SHARED AGAIN ALIAS IN_5A21A ORDER TAG MESS_ID
m.tmpTime = m.tmpH+":"+m.tmpM+":00"
IF m.tmpType = 1
  SELECT TMP_PERS
  SET ORDER TO TAG NAME
  SCAN
    SELECT IN_5A21A
    GO TOP
    IF .NOT. EMPTY(MESS_ID)
      APPEND BLANK
    ENDIF
    REPLACE MESS_ID		WITH m.tmp_ID, ;
            ADDRESS_T	WITH " ",      ;
            ADDRESS		WITH STR(TMP_PERS.CODE, 5), ;
            SHOW_DATE	WITH m.tmpDate,;
            SHOW_TIME	WITH m.tmpTime
    SELECT TMP_PERS
  ENDSCAN
ELSE
  SELECT TMP_ARM
  SET ORDER TO TAG NAME
  SCAN
    SELECT IN_5A21A
    GO TOP
    IF .NOT. EMPTY(MESS_ID)
      APPEND BLANK
    ENDIF
    REPLACE MESS_ID		WITH m.tmp_ID, ;
            ADDRESS_T	WITH "*",      ;
            ADDRESS		WITH TMP_ARM.CODE, ;
            SHOW_DATE	WITH m.tmpDate,;
            SHOW_TIME	WITH m.tmpTime
    SELECT TMP_ARM
  ENDSCAN
ENDIF

USE IN IN_5A21A

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
