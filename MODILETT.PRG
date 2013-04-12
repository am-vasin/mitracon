*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла ModiLett     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Коррекция заготовки письма                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 10.04.2006 ══╝
PROCEDURE ModiLett
PARAMETERS prmID

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

PRIVATE s_sav, tmpSub, tmpText, blnk, cnt, tmpID, ln, retRec
m.retRec = 0
IF EMPTY(m.prmID)
  m.tmpID = 0
ELSE
  m.tmpID = m.prmID
ENDIF
m.s_sav = SELECT()
SELECT 0
USE (base_path+"MAILTXT") SHARED AGAIN ALIAS ML1_6410 ORDER TAG LETTID
SCATTER TO blnk BLANK
m.tmpText = ""
IF EMPTY(tmpID)
  m.tmpSub = SPACE(FSIZE("SUBJECT"))
ELSE
  SEEK STR(m.tmpID)
  m.tmpSub = SUBJECT
  SCAN REST WHILE LETTID = m.tmpID
    m.tmpText = m.tmpText+TEXT
  ENDSCAN
  m.tmpText = ALLTRIM(m.tmpText)
ENDIF

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌───────────────────────────────────────────────────────────────────┐
*│ Тема ....:....!....:....!....:....!....:....!....:....!....:....! │
*│ ┌─ Текст письма ────────────────────────────────────────────────┐ │
*│ │                                                               │ │
*│ │                                                               │ │
*│ │                                                               │ │
*│ │                                                               │ │
*│ │                                                               │ │
*│ │                                                               │ │
*│ │                                                               │ │
*│ │                                                               │ │
*│ │                                                               │ │
*│ │                                                               │ │
*│ │                                                               │ │
*│ │                                                               │ │
*│ │                                                               │ │
*│ └───────────────────────────────────────────────────────────────┘ │
*│                 < OK Ctrl-W > < Отказаться Esc >                  │
*└───────────────────────────────────────────────────────────────────┘

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 20, 71, "Заготовка письма"
DO Sun_Bord WITH  3,  3, 17, 67, " Текст письма "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 3 SAY "Тема" GET m.tmpSub
@ 4, 4 EDIT m.tmpText SIZE 13, 63 COLOR (SCHEME(1,1))

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID TstBlank()

IF m.ex = 1
  *
  * Отрабатываем бланк
  IF EMPTY(m.tmpID)
    GO BOTT
    m.tmpID = LETTID+1
  ELSE
    DO WHILE SEEK(STR(m.tmpID))
      GATHER FROM blnk
    ENDDO
  ENDIF
  m.cnt = 0
  m.tmpText = ALLTRIM(m.tmpText)
  m.ln = FSIZE("TEXT")
  DO WHILE .NOT. EMPTY(m.tmpText)
    m.cnt = m.cnt+1
    IF m.cnt = 0
      m.retRec = RECNO()
    ENDIF
    GO TOP
    IF .NOT. EMPTY(LETTID) .OR. BOF() .OR. EOF()
      APPEND BLANK
    ENDIF
    REPLACE WHO		WITH m.user,   ;
            LETTID	WITH m.tmpID,  ;
            SUBJECT	WITH m.tmpSub, ;
            N_LINE	WITH m.cnt,    ;
            TEXT	WITH LEFT(m.tmpText, m.ln)
    IF LEN(m.tmpText) < m.ln
      m.tmpText = ""
    ELSE
      m.tmpText = SUBSTR(m.tmpText, m.ln+1)
    ENDIF
  ENDDO
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

USE
SELECT (m.s_sav)

RETURN m.retRec

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура TstBlank     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Проверка корректности бланка.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10.04.2006 ──┘
PROCEDURE TstBlank

PRIVATE mss

IF m.ex = 2
  RETURN .T.
ENDIF

IF EMPTY(m.tmpSub)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Тема не может быть пустой!"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(m.tmpText)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Содержимое письма не может быть пустым!"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
