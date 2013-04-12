*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Que_Gtd1      Разработчик Андрей Васин                     ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║             Растановка ГТД на основании внешнего запроса.              ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 06.04.2000 ══╝
PROCEDURE Que_Gtd1

PRIVATE ext_name, sw_emp, sw_GTD, s_sav, pointGTD, tmpLast

STORE .F. TO sw_emp, sw_GTD
ext_name = ""
pointGTD = ""
tmpLast = .F.
s_sav = SELECT()
SELECT 0
USE (path_comm+"GTD") AGAIN ALIAS GTD_0525

IF .NOT. Fill_Bl()		&& Источник и состав обрабатываемых позиций
  SELECT GTD_0525
  USE
  SELECT (s_sav)
  RETURN
ENDIF

DO Set_GTD_0               && Собственно расстановка ГТД

SELECT GTD_0525
USE
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
*│┌─ [ ] Выбор позиции ГТД ──────────────────────────────┐│
*││      Наименование ....:....!....:....!....:....      ││
*││Номер ГТД ....:....!....:....! Страна ....:....!....:.││
*│└──────────────────────────────────────────────────────┘│
*│ ┌─ Исключить партии ─────────────────────────────────┐ │
*│ │          [ ] С ГТД   [ ] С пустым наличием         │ │
*│ └────────────────────────────────────────────────────┘ │
*│     [ ] Всегда устанавливать для последней партии      │
*│            < OK Ctrl-W > < Отказаться Esc >            │
*└────────────────────────────────────────────────────────┘
PRIVATE ex, swTmp, swPos, tmpPos, tmpGTD, tmpCountry
ex = 1
swPos = .F.
tmpPos = SPACE(29)
tmpGTD = SPACE(20)
tmpCountry = SPACE(16)
swTmp = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 15, 60, "Заполните бланк"
DO Sun_Bord WITH  2,  3,  4, 56, " [ ] Файл с кодами позиций "
DO Sun_Bord WITH  5,  2,  8, 57, " [ ] Выбор позиции ГТД "
DO Sun_Bord WITH  9,  3, 11, 56, " Исключить партии "
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2,  6 GET swTmp PICTURE "@*C Файл с кодами позиций" VALID G_File()
@  3,  5 EDIT ext_name SIZE 1, 50 WHEN .F.
@  5,  5 GET swPos PICTURE "@*C С Выбор позиции ГТД" VALID SwPos()
@  6,  9 SAY "Наименование" GET tmpPos WHEN .F.
@  7,  3 SAY "Номер ГТД" GET tmpGTD WHEN .F.
@  7, 34 SAY "Страна" GET tmpCountry WHEN .F.
@ 10, 14 GET sw_GTD PICTURE "@*C С ГТД"
@ 10, 26 GET sw_emp PICTURE "@*C С пустым наличием"
@ 12,  7 GET tmpLast PICTURE "@*C Всегда устанавливать для последней партии"
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

IF EMPTY(tmpGTD)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы так и не указали номер ГТД..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
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
*│   Процедура SwPos        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Выбор позиции ГТД.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.05.2000 ──┘
PROCEDURE SwPos

IF .NOT. swPos
  RETURN
ENDIF

swPos = .F.
SHOW GET swPos

pointGTD = GTD_Sto1(pointGTD)

IF TYPE("pointGTD") = "N"
  IF BETWEEN(pointGtd, 1, RECCOUNT("GTD_0525"))
    GO pointGTD IN GTD_0525
    tmpPos = GTD_0525.NAME
    tmpGTD = GTD_0525.GTD_NUM
    tmpCountry = GTD_0525.COUNTRY
    SHOW GET tmpPos
    SHOW GET tmpGTD
    SHOW GET tmpCountry
  ENDIF
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Set_GTD_0    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Расстановка ГТД.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 25.05.2000 ──┘
PROCEDURE Set_GTD_0

PRIVATE s_sav, f_code, tmp_code
f_code = .T.

SELECT 0
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
SELECT 0
USE (base_path+"ACT_S") ORDER TAG CODE AGAIN ALIAS ACT_0525

PRIVATE last_p, wk_date
SELECT SOURCE
DO Wt_Mess WITH "Расстановка ГТД..."
SCAN
  tmp_code = "SOURCE."+f_code
  tmp_code = &tmp_code
  last_p = 0
  wk_date = {}
  SELECT ACT_0525
  SEEK tmp_code
  SCAN REST WHILE CODE = tmp_code
    IF wk_date <= DOC_DATE
      wk_date = DOC_DATE
      last_p = RECNO()
    ENDIF
    IF .NOT. EMPTY(GTD_NUM) .AND. sw_GTD
      LOOP
    ENDIF
    IF QNT_REST = 0 .AND. sw_emp
      LOOP
    ENDIF
    REPLACE GTD_NUM  WITH GTD_0525.GTD_NUM, ;
            COUNTRY  WITH GTD_0525.COUNTRY, ;
            GTD_LINE WITH GTD_0525.GTD_LINE
  ENDSCAN
  IF last_p # 0 .AND. tmpLast
    GO last_p
    REPLACE GTD_NUM  WITH GTD_0525.GTD_NUM, ;
            COUNTRY  WITH GTD_0525.COUNTRY, ;
            GTD_LINE WITH GTD_0525.GTD_LINE
  ENDIF
  SELECT SOURCE
ENDSCAN
DO Wt_Mess
USE
SELECT ACT_0525
USE
RETURN
