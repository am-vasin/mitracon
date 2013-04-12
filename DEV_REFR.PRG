*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Dev_Refr     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                         Сверка списка брендов                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 09.12.2005 ══╝
PROCEDURE Dev_Refr
PARAMETERS prmCus, prmFile

*
* prmCus	- код поставщика,
* prmFile	- название файла поставщика.
*

* Структура файла поставщика
*	CODE - код бренда (необязательно)
*   NAME - название бренда (или B_NAME)
*

PRIVATE s_sav, keyCus, tmpCode, tmpName

m.keyCus = STR(prmCus)
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"TEACH_U") SHARED AGAIN ORDER TAG CODE ALIAS TC_5C09
SELECT 0
USE (m.prmFile) SHARED AGAIN ALIAS SRC_5C09
SCAN
  DO CASE
  CASE TYPE("CODE") = "C"
    m.tmpCode = CODE
  CASE TYPE("CODE") = "N"
    m.tmpCode = STR(CODE, 15)
  CASE TYPE("NAME") = "C"
    m.tmpCode = LEFT(NAME, 8)
  OTHERWISE
    m.tmpCode = LEFT(B_NAME, 8)
  ENDCASE

  IF TYPE("NAME") = "C"
    m.tmpName = LEFT(NAME, 8)
  ELSE
    m.tmpName = LEFT(B_NAME, 8)
  ENDIF
    
  IF SEEK("D"+m.keyCus+m.tmpCode, "TC_5C09")
    LOOP
  ENDIF
  IF .NOT. Connect(m.tmpCode, m.tmpName)
    USE
    USE IN TC_5C09
    SELECT (m.s_sav)
    RETURN .F.
  ENDIF
ENDSCAN

USE
USE IN TC_5C09
SELECT (m.s_sav)
RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Connect      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Привязка класса                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 09.12.2005 ──┘
PROCEDURE Connect
PARAMETERS pCode, pName

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

*┌─────────────────────────────────────────────────────┐
*│                                                     │
*│ Бренд поставщика ....:... привязать к ....:... (F1) │
*│                                                     │
*│           < OK Ctrl-W > < Отказаться Esc >          │
*└─────────────────────────────────────────────────────┘

PRIVATE ex, tmpCd, tmpNm, tmpRet, s_sav
m.ex = 1
m.tmpSw  = .F.
m.tmpRet = .F.
m.tmpCd = 0
m.tmpNm  = SPACE(8)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 57, "Укажите соответствие"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  3 SAY "Бренд поставщика" GET m.pName WHEN .F.
@ 3, 29 SAY "привязать к" GET m.tmpNm VALID T_Dev()
@ 3, 50 SAY "(F1)"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

ON KEY LABEL F1 DO Tmp_Ch
READ CYCLE
ON KEY LABEL F1

IF m.ex = 1
  *
  * Отрабатываем бланк
  IF .NOT. EMPTY(m.tmpCd)
    m.s_sav = SELECT()
    SELECT TC_5C09
    APPEND BLANK
    REPLACE TEACH_TYPE	WITH "D",      ;
            CUS_CODE	WITH m.prmCus, ;
            CODE_CUS	WITH m.pCode,  ;
            NAME_CUS	WITH m.pName,  ;
            CODE_OWN	WITH STR(m.tmpCd,12)
    SELECT (m.s_sav)
    m.tmpRet = .T.
  ENDIF
  
ENDIF
*--------------------------------------------------------------------------
	
POP KEY
RELEASE WINDOW (win_name)
RETURN m.tmpRet

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура T_Dev        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Проверка бренда                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 09.12.2005 ──┘
PROCEDURE T_Dev

PRIVATE s_sav

IF EMPTY(m.tmpNm)
  m.tmpCd = 0
  RETURN .T.
ENDIF

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"DEVELOP") SHARED AGAIN ORDER TAG B_NAME ALIAS BRN_5C09
IF SEEK(ALLTRIM(m.tmpNm))
  m.tmpNm = LEFT(B_NAME, 8)
  m.tmpCd = CODE
ELSE
  m.tmpNm = SPACE(8)
  m.tmpCd = 0
ENDIF
SHOW GET m.tmpNm

USE
SELECT (m.s_sav)

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tmp_Ch        Разработчик Андрей Васин                     │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Выбор бренда                               │
*│                                                                        │
*└────────────────────────────────────────────────────────── 09.12.2005 ──┘
PROCEDURE Tmp_Ch

ON KEY LABEL F1

DO Ch_Dev WITH m.tmpCd, m.tmpNm
SHOW GET m.tmpNm

ON KEY LABEL F1 DO Tmp_Ch

RETURN
