*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Prod1        Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Коррекция/добавление бренда                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 24.12.2005 ══╝
PROCEDURE Prod1
PARAMETERS prmCode	&& код, если пусто, то добавление

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

*┌──────────────────────────────────────────────────────────────┐
*│                    Наименование ....:...                     │
*│┌─ Полное наименование ──────────────────────────────────────┐│
*││....:....!....:....!....:....!....:....!....:....!....:....!││
*│└────────────────────────────────────────────────────────────┘│
*│   Папка с изобр. ....:...   Срок поставки ....:....!....:.   │
*│              < OK Ctrl-W > < Отказаться Esc >                │
*└──────────────────────────────────────────────────────────────┘

PRIVATE ex, s_sav
PRIVATE tmpCode, tmpBName, tmpLName, tmpFolder, tmpPeriod, savName
m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"DEVELOP") SHARED AGAIN ALIAS BRND_5C24 ORDER TAG CODE
m.tmpCode = IIF(EMPTY(m.prmCode), 0, m.prmCode)

IF m.tmpCode = 0
  m.tmpBName  = SPACE(8)
  m.tmpLName  = SPACE(FSIZE("L_NAME"))
  m.tmpFolder = SPACE(FSIZE("FOLDER"))
  m.tmpPeriod = SPACE(FSIZE("PERIOD"))
ELSE
  SEEK m.tmpCode
  m.tmpBName  = LEFT(B_NAME, 8)
  m.tmpLName  = L_NAME
  m.tmpFolder = FOLDER
  m.tmpPeriod = PERIOD
ENDIF
m.savName = m.tmpBName
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 66, "Фирма производитель "+IIF(EMPTY(m.prmCode), "(добавление)", "(коррекция)")
DO Sun_Bord WITH  3,  2,  5, 63, " Полное наименование "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2, 22 SAY "Наименование" GET m.tmpBName
@  4,  3 GET m.tmpLName
@  6,  5 SAY "Папка с изобр." GET m.tmpFolder
@  6, 31 SAY "Срок поставки" GET m.tmpPeriod
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID TstBrend()

IF m.ex = 1
  *
  * Отрабатываем бланк
  IF m.tmpBName # m.savName .AND. m.tmpCode # 0
    SELECT 0
    USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG PRO_CODE ALIAS ST_B_5C24
    SEEK STR(m.tmpCode, 5)
    SCAN REST WHILE PRO_CODE = m.tmpCode
      REPLACE PRODUCER WITH m.tmpBName
    ENDSCAN
    USE
    SELECT BRND_5C24
  ENDIF
  IF m.tmpCode = 0
    SET ORDER TO TAG CODE
    GO BOTT
    m.tmpCode = MAX(CODE+1, 90000)
    APPEND BLANK
    REPLACE CODE    WITH m.tmpCode, ;
            DATE_ON WITH DATE(),    ;
            WHO_ADD WITH m.user
    IF TYPE("m.new_rec") = "N"
      m.new_rec = RECNO()
    ENDIF
  ENDIF
  REPLACE B_NAME WITH m.tmpBName, ;
          L_NAME WITH m.tmpLName, ;
          FOLDER WITH m.tmpFolder,;
          PERIOD WITH m.tmpPeriod
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
USE
SELECT (m.s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура TstBrend     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Проверка корректности бренда                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 24.12.2005 ──┘
PROCEDURE TstBrend

PRIVATE mss, s_sav

IF m.ex # 1
  RETURN .T.
ENDIF

IF EMPTY(m.tmpBName)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Название не может быть пустым..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF EMPTY(m.tmpLName)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Полное название не может быть пустым..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"DEVELOP") SHARED AGAIN ORDER TAG B_NAME ALIAS TMP_B_5C24
IF .NOT. EMPTY(m.tmpCode)
  SET FILTER TO CODE # m.tmpCode
ENDIF
IF SEEK(m.tmpBName)
  USE
  SELECT (m.s_sav)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Фирма с таким названием уже существует..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

USE
SELECT (m.s_sav)
RETURN .T.
