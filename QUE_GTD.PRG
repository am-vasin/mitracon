*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Que_Gtd      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║             Растановка ГТД на основании внешнего запроса.              ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 06.04.2000 ══╝
PROCEDURE Que_Gtd

PRIVATE ext_name, sw_emp, sw_GTD, s_sav

STORE .F. TO sw_emp, sw_GTD
ext_name = ""
s_sav = SELECT()
SELECT 0

IF .NOT. Fill_Bl()		&& Источник и состав обрабатываемых позиций
  SELECT (s_sav)
  RETURN
ENDIF

IF .NOT. Prp_Link()		&& Подготовка временного файла для обработки
  SELECT (s_sav)
  RETURN
ENDIF

DO Gtd_Link WITH .T.

DO Set_Gtd_0

SELECT GTD_LINK
USE
DELETE FILE (tmpo_path+"GTD_LINK.DBF")
DELETE FILE (tmpo_path+"GTD_LINK.CDX")

SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Fill_Bl      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Заполнение бланка.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.04.2000 ──┘
PROCEDURE Fill_Bl

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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌────────────────────────────────────────────────────────┐
*│ ┌─ [ ] Файл с кодами позиций ────────────────────────┐ │
*│ │ ....:....!....:....!....:....!....:....!....:....! │ │
*│ └────────────────────────────────────────────────────┘ │
*│ ┌─ Исключить позиции ────────────────────────────────┐ │
*│ │  [ ] С ГТД во всех партиях  [ ] С пустым наличием  │ │
*│ └────────────────────────────────────────────────────┘ │
*│            < OK Ctrl-W > < Отказаться Esc >            │
*└────────────────────────────────────────────────────────┘

PRIVATE ex, swTmp
ex = 1
swTmp = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 60, "Заполните бланк"
DO Sun_Bord WITH  2,  3,  4, 56, " [ ] Файл с кодами позиций "
DO Sun_Bord WITH  5,  3,  7, 56, " Исключить позиции "
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2,  6 GET swTmp PICTURE "@*C Файл с кодами позиций" VALID G_File()
@ 3,  5 EDIT ext_name SIZE 1, 50 WHEN .F.
@ 6,  6 GET sw_GTD PICTURE "@*C С ГТД во всех партиях"
@ 6, 33 GET sw_emp PICTURE "@*C С пустым наличием"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Tst_File()

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_File       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Выбор файла с подборкой.                        │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.04.2000 ──┘
PROCEDURE G_File

IF swTmp
  ext_name = Int_File(ext_name, "dbf", "Файл с кодами позиций")
  SHOW GET ext_name
  swTmp = .F.
  SHOW GET swTmp
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_File     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Проверка корректности имени файла.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 06.04.2000 ──┘
PROCEDURE Tst_File

IF ex = 2
  RETURN .T.
ENDIF

IF .NOT. File_O(ext_name)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы ошиблись с указанием файла..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Prp_Link     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│      Подготовка временного файла позиций для обработки обработки.      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.04.2000 ──┘
PROCEDURE Prp_Link

PRIVATE f_code, swTmp
f_code = .T.

USE (ext_name) ALIAS SOURCE AGAIN	&& Источник данных

*
*  Определяем имя поля с кодом позиции
IF TYPE("SOURCE.STO_CODE") = "N"
  f_code = "STO_CODE"
ENDIF
IF TYPE("f_code") # "C"
  IF TYPE("SOURCE.CODE") = "N"
    f_code = "CODE"
  ENDIF
ENDIF
IF TYPE("f_code") # "C"
  f_code = Get_Field()
ENDIF
IF TYPE("f_code") # "C"
  USE
  RETURN .F.
ENDIF
f_code = ALLTRIM(f_code)
DELETE FILE (tmpo_path+"GTD_LINK.DBF")
DELETE FILE (tmpo_path+"GTD_LINK.CDX")
SELECT 0
CREATE DBF (tmpo_path+"GTD_LINK") ;
  ( LINK     N(10), ;
    CODE     N( 7), ;
    MARK     L,     ;
    PREFIX   C( 4), ;
    NAME     C(35), ;
    PRODUCER C( 8), ;
    GTD_NAME C(29), ;
    GTD_NUM  C(26), ;
    GTD_LINE C( 4), ;
    COUNTRY  C(16), ;
    B_GTD    L  )
SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE DESCENDING ALIAS P_0407 AGAIN
*  DESCENDING - для ускорения перебора - пустые ГТД скорее всего в последних партиях
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S_0407 AGAIN
SELECT SOURCE
SET RELATION TO &f_code INTO S_0407
IF sw_GTD
  SET RELATION TO &f_code INTO P_0407 ADDITIVE
ENDIF sw_GTD
SCAN FOR FOUND("S_0407")
  swTmp = .T.
  IF sw_emp .AND. S_0407.QNT+S_0407.QNT_SAVE+S_0407.QNT_REZ+S_0407.QNT_REAL = 0
    LOOP		&& Пропускаем позиции с пустым наличием
  ENDIF
  IF sw_GTD		&& Пропускаем позиции с заполненым ГТД
    SELECT P_0407
    swTmp = .F.
    SCAN REST WHILE CODE = S_0407.CODE
      IF EMPTY(GTD_NUM)
        swTmp = .T.
        EXIT
      ENDIF
    ENDSCAN
  ENDIF
  IF swTmp
    SELECT GTD_LINK
    APPEND BLANK
    REPLACE LINK     WITH S_0407.CODE,   ;
            CODE     WITH S_0407.CODE,   ;
            MARK     WITH .T.,           ;
            PREFIX   WITH S_0407.PREFIX, ;
            NAME     WITH S_0407.NAME,   ;
            PRODUCER WITH S_0407.PRODUCER
  ENDIF
  SELECT SOURCE
ENDSCAN
USE
SELECT S_0407
USE
SELECT P_0407
USE
SELECT GTD_LINK
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не осталось ни одной позиции..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"GTD_LINK.DBF")
  DELETE FILE (tmpo_path+"GTD_LINK.CDX")
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_Field    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                 Интерактивный выбор поля кода позиции.                 │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.04.2000 ──┘
PROCEDURE Get_Field

PRIVATE f_list, f_count, i, tmpVal

f_count = 0
i = 0
DO WHILE .T.
  i = i+1
  tmpVal = FIELD(i)
  IF EMPTY(tmpVal)
    EXIT
  ENDIF
  IF TYPE("SOURCE."+tmpVal) = "N"
    f_count = f_count+1
    DIMENSION f_list(f_count)
    f_list(f_count) = PADR(tmpVal, 40)
  ENDIF
ENDDO

IF f_count = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Указанный Вами файл не содержит ни одного числового поля!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*
*Укажите имя поля с кодом позиции
*┌─────────────────────────────────────────────┐
*│ ┌─────────────────────────────────────────┐ │
*│ │ ....:....!....:....!....:....!....:....!│ │
*│ └─────────────────────────────────────────┘ │
*│       < OK Ctrl-W > < Отказаться Esc >      │
*└─────────────────────────────────────────────┘

PRIVATE ex, ln
ex = 1
ln = MIN(WROWS("")-10, f_count+6)
tmpVal = f_list(1)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH ln, 49, "Укажите имя поля с кодом позиции"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 3 GET tmpVal FROM f_list PICTURE "@&T" SIZE ln-4, 43
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex # 1
  mtpVal = .F.
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN tmpVal

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_Gtd_0    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                      Собственно расстановка ГТД.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.04.2000 ──┘
PROCEDURE Set_Gtd_0

PRIVATE empSw, gtdSw
empSw = .T.
gtdSw = .F.

IF .NOT. F_Blank()
  RETURN
ENDIF

SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE ALIAS P_0407 AGAIN
SELECT GTD_LINK
SCAN FOR MARK
  IF SEEK(LINK, "P_0407")
    SELECT P_0407
    SCAN REST WHILE CODE = GTD_LINK.LINK
      IF .NOT. empSw .AND. QNT_REST = 0
        LOOP
      ENDIF
      IF .NOT. gtdSw .AND. .NOT. EMPTY(GTD_NUM)
        LOOP
      ENDIF
      REPLACE GTD_NUM  WITH GTD_LINK.GTD_NUM, ;
              GTD_LINE WITH GTD_LINK.GTD_LINE,;
              COUNTRY  WITH GTD_LINK.COUNTRY, ;
              B_GTD    WITH GTD_LINK.B_GTD
    ENDSCAN
    SELECT GTD_LINK
  ENDIF
ENDSCAN
SELECT P_0407
USE
SELECT GTD_LINK
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура F_Blank      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Заполнение бланка.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.04.2000 ──┘
PROCEDURE F_Blank
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
stat_type  = 1
what_do    = ""
menu_name  = ""
last_mouse = 0
win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*
* 
*┌───────────────────────────────────────────────┐
*│                                               │
*│ [ ] С пустым наличием  [ ] С заполненным ГТД  │
*│                                               │
*│        < OK Ctrl-W > < Отказаться Esc >       │
*└───────────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 51, "Установить ГТД для партий"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  3 GET empSw PICTURE "@*C С пустым наличием"
@ 3, 26 GET gtdSw PICTURE "@*C С заполненным ГТД"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1
