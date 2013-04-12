*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Act_GTD      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                Установка ГТД для партий приемного акта.                ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 27.02.2000 ══╝
PROCEDURE Act_GTD
PARAMETERS pFirm, pDoc, pDate

PRIVATE s_sav, doc_d, tmpVal
s_sav = SELECT()
SELECT 0
USE (base_path+"ACT_S") ORDER TAG DOC_NUM ALIAS INC_0227 AGAIN

doc_d = STR(YEAR(DATE()),4)
DO CASE
CASE TYPE("pDate") = "C"
  doc_d = pDate
CASE TYPE("pDate") = "D"
  doc_d = STR(YEAR(pDate),4)
CASE TYPE("pDate") = "N"
  doc_d = STR(pDate,4)
ENDCASE

IF .NOT. SEEK(pFirm+doc_d+pDoc)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Для указанного Вами документа не найдено ни одной партии!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

PRIVATE swGTD, swQnt, swRus, swManual

swGTD = .F.
swRus = .F.
swQnt = .T.
swManual = .T.

IF .NOT. GetMode()
  USE
  SELECT (s_sav)
  RETURN
ENDIF
SELECT 0
USE (base_path+"STOCK") ORDER TAG CODE ALIAS STO_0227 AGAIN
SELECT 0

DELETE FILE (tmpo_path+"GTD_LINK.CDX")
DELETE FILE (tmpo_path+"GTD_LINK.DBF")

CREATE DBF (tmpo_path+"GTD_LINK") ;
  ( LINK     N(10), ;
    CODE     N( 7), ;
    MARK     L,     ;
    PREFIX   C( 4), ;
    NAME     C(78), ;
    PRODUCER C( 8), ;
    GTD_NAME C(29), ;
    GTD_NUM  C(26), ;
    GTD_LINE C( 4), ;
    COUNTRY  C(16), ;
    COUNTRYC C( 3), ;
    B_GTD    L  )

SELECT INC_0227
SET RELATION TO CODE INTO STO_0227
DO Wt_Mess WITH "Подготовка списка партий"
SCAN REST WHILE FIRM = pFirm .AND. DOC_NUM = pDoc .AND. STR(YEAR(DOC_DATE),4) = doc_d
  IF .NOT. swRus .AND. STO_0227.STOP_PUR = "S"
    LOOP
  ENDIF
  IF .NOT. swGTD .AND. .NOT. EMPTY(GTD_NUM)
    LOOP
  ENDIF
  IF .NOT. swQnt .AND. EMPTY(QNT_REST)
    LOOP
  ENDIF
  SELECT GTD_LINK
  APPEND BLANK
  REPLACE LINK     WITH RECNO("INC_0227"), ;
          CODE     WITH STO_0227.CODE,     ;
          MARK     WITH .T.,               ;
          PREFIX   WITH STO_0227.PREFIX,   ;
          NAME     WITH STO_0227.NAME,     ;
          PRODUCER WITH STO_0227.PRODUCER
  SELECT INC_0227
ENDSCAN
DO Wt_Mess

SELECT STO_0227
USE
SELECT GTD_LINK
IF RECCOUNT() = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Не найдено ни одной подходящей партии!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (tmpo_path+"GTD_LINK.CDX")
  DELETE FILE (tmpo_path+"GTD_LINK.DBF")
  SELECT INC_0227
  USE
  SELECT (s_sav)
  RETURN
ENDIF

tmpVal = GTD_Link(swManual)
SELECT GTD_LINK
IF .NOT. swManual
  IF tmpVal = 0
    PRIVATE mss
    DIMENSION mss(3)
    mss(1) = ""
    mss(2) = CHR(0)+"Не удалось подобрать ни одной ГТД!"
    mss(3) = ""
    DO Out_Mess WITH 7, "mss"
    USE
    DELETE FILE (tmpo_path+"GTD_LINK.CDX")
    DELETE FILE (tmpo_path+"GTD_LINK.DBF")
    SELECT INC_0227
    USE
    SELECT (s_sav)
    RETURN
  ELSE
    PRIVATE mss
    DIMENSION mss(4)
    mss(1) = ""
    mss(2) = CHR(0)+"Удалось подобрать ГТД для "+ALLTRIM(STR(tmpVal))+" партий из "+ALLTRIM(STR(RECCOUNT()))+"."
    mss(3) = CHR(0)+"  Проставить найденные ГТД? [ Да ] [ Нет ]"
    mss(4) = ""
    IF Out_Mess(5, "mss") = 2
      USE
      DELETE FILE (tmpo_path+"GTD_LINK.CDX")
      DELETE FILE (tmpo_path+"GTD_LINK.DBF")
      SELECT INC_0227
      USE
      SELECT (s_sav)
      RETURN
    ENDIF
  ENDIF
ELSE
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Приступаем к установке ГТД? [ Да ] [ Нет ]"
  mss(3) = ""
  IF Out_Mess(5, "mss") = 2
    USE
    DELETE FILE (tmpo_path+"GTD_LINK.CDX")
    DELETE FILE (tmpo_path+"GTD_LINK.DBF")
    SELECT INC_0227
    USE
    SELECT (s_sav)
    RETURN
  ENDIF
ENDIF

DO Wt_Mess WITH "Установка ГТД"
SELECT INC_0227
SET ORDER TO
SELECT GTD_LINK
SCAN
  IF .NOT. EMPTY(GTD_NUM)
    GO GTD_LINK.LINK IN INC_0227
    REPLACE INC_0227.GTD_NUM  WITH GTD_LINK.GTD_NUM, ;
            INC_0227.COUNTRY  WITH GTD_LINK.COUNTRY, ;
            INC_0227.COUNTRYC WITH GTD_LINK.COUNTRYC,;
            INC_0227.GTD_LINE WITH GTD_LINK.GTD_LINE,;
            INC_0227.B_GTD    WITH GTD_LINK.B_GTD
  ENDIF
ENDSCAN
DO Wt_Mess

USE
DELETE FILE (tmpo_path+"GTD_LINK.CDX")
DELETE FILE (tmpo_path+"GTD_LINK.DBF")
SELECT INC_0227
USE
SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура GetMode      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Установка режима.                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.02.2000 ──┘
PROCEDURE GetMode

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

*┌──────────────────────────────────┐
*│ ┌─ В т. ч. для партий ─────────┐ │
*│ │ [ ] с пустым остатком        │ │
*│ │ [ ] с непустым ГТД           │ │
*│ │ [ ] отеч. позиций            │ │
*│ └──────────────────────────────┘ │
*│       [ ] Ручной контроль        │
*│ < OK Ctrl-W > < Отказаться Esc > │
*└──────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 38, "Установка ГТД"
DO Sun_Bord WITH  2,  3,  6, 34, " В т. ч. для партий "
*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 3, 5 GET swQnt PICTURE "@*C с пустым остатком"
@ 4, 5 GET swGTD PICTURE "@*C с непустым ГТД"
@ 5, 5 GET swRus PICTURE "@*C отеч. позиций"
@ 7, 9 GET swManual PICTURE "@*C Ручной контроль"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1
