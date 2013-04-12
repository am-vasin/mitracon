*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Corpus       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Коррекция/создание корпуса                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 17.12.2005 ══╝
PROCEDURE Corpus
PARAMETERS tmpCode		&& Код корпуса, если пусто - создание

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

*┌──────────────────────────────────────────┐
*│                                          │
*│ Название ....:....!....:.   Вес 9999.999 │
*│                                          │
*│     < OK Ctrl-W > < Отказаться Esc >     │
*└──────────────────────────────────────────┘

PRIVATE ex, s_sav, tmpN, tmpW, tmpC, newName
m.s_sav = SELECT()
SELECT 0
USE (m.path_comm+"CORPUS") SHARED AGAIN ALIAS CRP_5B17 ORDER TAG CODE
IF .NOT. EMPTY(m.tmpCode)
  SEEK m.tmpCode
  m.tmpN = NAME
  m.tmpW = WEIGHT
ELSE
  m.tmpN = SPACE(16)
  m.tmpW = 0
ENDIF
m.newName = ""
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 46, "Корпус "+IIF(EMPTY(m.tmpCode), "(создание)", "(коррекция)")

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@  3,  3 SAY "Название" GET m.tmpN
@  3, 31 SAY "Вес" GET m.tmpW PICTURE "9999.999"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID TstBlank()

IF m.ex = 1
  *
  * Отрабатываем бланк
  IF EMPTY(m.tmpCode)
    GO BOTTOM
    m.tmpC = MAX(CODE+1, 9000000)
    APPEND BLANK
    REPLACE CODE WITH m.tmpC
    IF TYPE("m.new_rec") = "N"
      m.new_rec = RECNO()
    ENDIF
  ELSE
    IF m.tmpN # NAME
      m.newName = m.tmpN
    ENDIF
  ENDIF
  REPLACE NAME   WITH m.tmpN, ;
          WEIGHT WITH m.tmpW
  IF .NOT. EMPTY(m.newName)
    USE (m.path_comm+"SUBJECT") SHARED AGAIN ALIAS SBJ_5B17
    DO Wt_Mess WITH "Обновляем корпус в номенклатуре"
    SCAN FOR COR_CODE = m.tmpCode
      REPLACE CORPUS WITH m.newName
    ENDSCAN
    DO Wt_Mess WITH "Обновляем корпус в номенклатуре"
  ENDIF
ENDIF
*--------------------------------------------------------------------------

USE
SELECT (m.s_sav)
POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура TstBlank     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Проверка корректности бланка                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 17.12.2005 ──┘
PROCEDURE TstBlank

IF m.ex # 1
  RETURN .T.
ENDIF

PRIVATE mss
IF EMPTY(m.tmpN)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Название не может быть пустым..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
