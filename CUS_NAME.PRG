*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Cus_Name     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                          Длинное имя клиента.                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 02.02.2007 ══╝
PROCEDURE Cus_Name
PARAMETERS prmCode

PRIVATE s_sav, tmpName, tmpShow, sName

m.s_sav = SELECT()
SELECT 0
USE m.base_path+"ACCOUNT" SHARED AGAIN ORDER TAG CUS_CODE ALIAS ACC_70202
SEEK m.prmCode
m.sName = ACC_70202.CUS_NAME
SELECT 0
USE m.base_path+"CUS_NAME" SHARED AGAIN ORDER TAG CUS_CODE ALIAS LONG_70202 IN 0

IF SEEK(m.prmCode)
  m.tmpName = ALLTRIM(CUS_NAME)
ELSE
  m.tmpName = ALLTRIM(ACC_70202.CUS_NAME)
ENDIF
m.tmpShow = m.tmpName

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

*┌─────────────────────────────────────────────────────────────────┐
*│    Клиент ....:....!....:....!....:....!....:....!....:....!    │
*│┌─ Полное имя клиента ──────────────────────────────────────────┐│
*││....:....!....:....!....:....!....:....!....:....!....:....!...││
*││                                                               ││
*││....:....!....:....!....:....!....:....!....:....!....:....!...││
*││....:....!....:....!....:....!....:....!....:....!....:....!...││
*││....:....!....:....!....:....!....:....!....:....!....:....!...││
*││....:....!....:....!....:....!....:....!....:....!....:....!...││
*│└───────────────────────────────────────────────────────────────┘│
*│                < OK Ctrl-W > < Отказаться Esc >                 │
*└─────────────────────────────────────────────────────────────────┘

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 13, 69, "Полное имя клиента"
DO Sun_Bord WITH  3,  2, 10, 66, " Полное имя клиента "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2,  6 SAY "Клиент" GET m.sName WHEN .F.
@  4,  3 EDIT m.tmpName SIZE 1, 63, FSIZE("CUS_NAME") VALID S_Name() COLOR (SCHEME(1,1))
@  6,  3 EDIT m.tmpShow SIZE 4, 63 COLOR (SCHEME(1,1)) WHEN .F.

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_Name()

IF m.ex = 1
  *
  * Отрабатываем бланк
  SELECT LONG_70202
  DO CASE
  CASE EMPTY(m.tmpName)
    IF FOUND("LONG_70202")
      REPLACE CUS_NAME WITH "", CUS_CODE WITH 0
    ENDIF 
  CASE SYS(15, m.lwr, ALLTRIM(m.tmpName)) == SYS(15, m.lwr, ALLTRIM(ACC_70202.CUS_NAME))
    IF FOUND("LONG_70202")
      REPLACE CUS_NAME WITH "", CUS_CODE WITH 0
    ENDIF 
  OTHERWISE
    IF .NOT. FOUND("LONG_70202")
      GO TOP
      IF .NOT. EMPTY(CUS_CODE) .OR. BOF()
        APPEND BLANK
      ENDIF
      REPLACE CUS_CODE WITH ACC_70202.CUS_CODE
    ENDIF
    REPLACE CUS_NAME WITH ALLTRIM(m.tmpName)
  ENDCASE  
ENDIF
*--------------------------------------------------------------------------
POP KEY
RELEASE WINDOW (win_name)
USE IN ACC_70202
USE IN LONG_70202
SELECT (m.s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура S_Name       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Дубль полного имени                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.02.2007 ──┘
PROCEDURE S_Name

m.tmpShow = m.tmpName
SHOW GET m.tmpShow

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Name     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Проверка корректности имени                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 02.02.2007 ──┘
PROCEDURE Tst_Name

IF m.ex # 1
  RETURN .T.
ENDIF

IF EMPTY(m.tmpName)
  RETURN .T.
ENDIF

IF SYS(15, m.lwr, ALLTRIM(m.tmpName)) == SYS(15, m.lwr, ALLTRIM(ACC_70202.CUS_NAME))
  RETURN .T.
ENDIF

IF LEN(ALLTRIM(m.tmpName)) <= FSIZE("CUS_NAME", "ACC_70202")
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Имя не столь длинное что бы использовать длинное наименование"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
