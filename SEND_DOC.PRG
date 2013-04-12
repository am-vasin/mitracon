*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Send_Doc     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                Передача документов в филиал для сверки.                ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 03.06.2000 ══╝
PROCEDURE Send_Doc
PARAMETERS fil_code, prmDate, prmPath

PRIVATE tmpDate, tmpPath, s_sav

s_sav = SELECT()
SELECT 0
IF TYPE("prmDate") = "C" .AND. TYPE("prmPatm") = "C"
  tmpDate = prmDate
  tmpPath = prmPath
ELSE
  tmpDate = DATE()-30
  tmpPath = SPACE(60)
  IF .NOT. Get_P()
    SELECT (s_sav)
    RETURN
  ENDIF
ENDIF

tmpPath = ALLTRIM(tmpPath)
CREATE DBF (tmpPath+"s_sale_t") ;
  (   DOC_NUM  C(10),  ;
      DOC_DATE D,      ;
      DOC_FIRM C( 3),  ;
      SUP_CODE N( 8),  ;
      CUS_CODE N( 8),  ;
      REF_NUM  C(10),  ;
      REF_DATE D       )
SELECT 0
CREATE DBF (tmpPath+"s_sale") ;
  (   DOC_NUM  C(10),  ;
      DOC_DATE D,      ;
      DOC_FIRM C( 3),  ;
      CODE     N( 7),  ;
      QNT      N( 8),  ;
      PRICE_R  N(15,5),;
      PRICE_D  N(13,4)  )
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS S_0603 AGAIN
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG CUS_CODE ALIAS ST_0603 AGAIN
SEEK fil_code
SCAN REST WHILE fil_code = CUS_CODE ;
          FOR EMPTY(LIST_NUM) .AND. DOC_DATE >= tmpDate
  SELECT S_SALE_T
  APPEND BLANK
  REPLACE DOC_NUM  WITH ST_0603.DOC_NUM,  ;
          DOC_DATE WITH ST_0603.DOC_DATE, ;
          DOC_FIRM WITH ST_0603.FIRM,     ;
          SUP_CODE WITH sup_own,          ;
          CUS_CODE WITH fil_code,         ;
          REF_NUM  WITH ST_0603.REQ_DOC,  ;
          REF_DATE WITH ST_0603.REQ_DATE
  SELECT S_0603
  SEEK ST_0603.FIRM+LEFT(DTOS(ST_0603.DOC_DATE), 4)+ST_0603.DOC_NUM
  SCAN REST WHILE ST_0603.FIRM = FIRM .AND. ;
                  ST_0603.DOC_DATE = DOC_DATE .AND.  ;
                  ST_0603.DOC_NUM = DOC_NUM
    SELECT S_SALE
    APPEND BLANK
    REPLACE DOC_NUM  WITH ST_0603.DOC_NUM,  ;
            DOC_DATE WITH ST_0603.DOC_DATE, ;
            DOC_FIRM WITH ST_0603.FIRM,     ;
            CODE     WITH S_0603.CODE,      ;
            QNT      WITH S_0603.QNT,       ;
            PRICE_R  WITH S_0603.PRICE_R,   ;
            PRICE_D  WITH S_0603.PRICE_D
    SELECT S_0603
  ENDSCAN
  SELECT ST_0603
ENDSCAN

SELECT S_SALE_T
CREATE DBF (tmpPath+"s_ryn_t") ;
  (   DOC_NUM  C(10),  ;
      DOC_DATE D,      ;
      DOC_FIRM C( 3),  ;
      SUP_CODE N( 8),  ;
      CUS_CODE N( 8),  ;
      REF_NUM  C(10),  ;
      REF_DATE D       )
SELECT S_SALE
CREATE DBF (tmpPath+"s_ryn") ;
  (   DOC_NUM  C(10),  ;
      DOC_DATE D,      ;
      DOC_FIRM C( 3),  ;
      CODE     N( 7),  ;
      QNT      N( 8),  ;
      PRICE_R  N(15,5),;
      PRICE_D  N(13,4)  )
SELECT S_0603
USE (base_path+"RYN_M") ORDER TAG DOC_NUM ALIAS S_0603 AGAIN
SELECT ST_0603
USE (base_path+"RYN_M_T") ORDER TAG CUS_CODE ALIAS ST_0603 AGAIN
SEEK fil_code
SCAN REST WHILE fil_code = CUS_CODE
  SELECT S_RYN_T
  APPEND BLANK
  REPLACE DOC_NUM  WITH STR(ST_0603.DOC_NUM,10), ;
          DOC_DATE WITH ST_0603.DOC_DATE,        ;
          SUP_CODE WITH sup_own,                 ;
          CUS_CODE WITH fil_code
  SELECT S_0603
  SEEK LEFT(DTOS(ST_0603.DOC_DATE), 4)+STR(ST_0603.DOC_NUM,5)
  SCAN REST WHILE ST_0603.DOC_DATE = DOC_DATE .AND.  ;
                  ST_0603.DOC_NUM = DOC_NUM
    SELECT S_RYN
    APPEND BLANK
    REPLACE DOC_NUM  WITH STR(ST_0603.DOC_NUM,10),  ;
            DOC_DATE WITH ST_0603.DOC_DATE, ;
            CODE     WITH S_0603.CODE,      ;
            QNT      WITH S_0603.QNT
    SELECT S_0603
  ENDSCAN
  SELECT ST_0603
ENDSCAN
USE
SELECT S_0603
USE
SELECT S_RYN
USE
SELECT S_RYN_T
USE
SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Get_P        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Интерактивный ввод данных.                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.06.2000 ──┘
PROCEDURE Get_P
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

*┌────────────────────────────────────────────────────────────────┐
*│ ┌─ [ ] Куда поместить результат ─────────────────────────────┐ │
*│ │....:....!....:....!....:....!....:....!....:....!....:....!│ │
*│ └────────────────────────────────────────────────────────────┘ │
*│                Передать документы с ДД.ММ.ГГГГ                 │
*│                < OK Ctrl-W > < Отказаться Esc >                │
*└────────────────────────────────────────────────────────────────┘

PRIVATE ex, sw
ex = 1
sw = .F.
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 66, "Передача документов в филиал"
DO Sun_Bord WITH  2,  2,  4, 63, " [ ] Куда поместить результат "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2,  5 GET sw PICTURE "@*C Куда поместить результат" VALID G_Path()
@ 3,  3 GET tmpPath WHEN .F.
@ 5, 18 SAY "Передать документы с" GET tmpDate
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID TstBlank()

IF ex = 1
  *
  * Отрабатываем бланк
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Path       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Выбор директории                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.06.2000 ──┘
PROCEDURE G_Path

PRIVATE tmp, mss, p

IF sw = .F.
  RETURN.T.
ENDIF

tmp = tmpPath
DO WHILE .T.
  tmp = Int_Dir(tmp, "Выберите директорию")
  IF EMPTY(tmp)
    EXIT
  ENDIF
  IF File_O(tmp+"s_sale.dbf")   .OR. ;
     File_O(tmp+"s_sale_t.dbf") .OR. ;
     File_O(tmp+"s_ryn.dbf")    .OR. ;
     File_O(tmp+"s_ryn_t.dbf")
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Там уже есть файлы с документами!"
    mss(3) = CHR(0)+"   [ Удалить ] [ Сменить директорию ] [ Отказаться ]"
    mss(4) = ""
    p = Out_Mess(7, "mss")
    IF p = 3
      EXIT
    ENDIF
    IF p = 1
      DELETE FILE (tmp+"s_sale.dbf")
      DELETE FILE (tmp+"s_sale_t.dbf")
      DELETE FILE (tmp+"s_ryn.dbf")
      DELETE FILE (tmp+"s_ryn_t.dbf")
    ENDIF
    IF p = 2
      LOOP
    ENDIF
  ENDIF
  tmpPath = PADR(tmp, LEN(tmpPath))
  EXIT
ENDDO
sw = .F.
SHOW GET sw
SHOW GET tmpPath

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура TstBlank     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Выход из бланка.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 03.06.2000 ──┘
PROCEDURE TstBlank
PRIVATE mss

IF ex = 2
  RETURN .T.
ENDIF

IF EMPTY(tmpPath)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали директорию!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
