*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Sto_Add      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Дополнительная информация о позиции.                   ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 04.04.2004 ══╝
PROCEDURE Sto_Add
PARAMETER c_code, stop_corr

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

PRIVATE ex, tmpFunc, tmpSegment, tmpCorpC, tmpCorpN, segList, nList, i, swCorp
PRIVATE tmpPdf, tmpImage, tmpName
PRIVATE sel_sav
swCorp = .F.
sel_sav = SELECT()
SELECT 0
USE (base_path+"STOCK") ALIAS ST_4404 AGAIN ORDER TAG CODE
SEEK c_code
SELECT 0
USE (path_comm+"SUBJECT") ALIAS SUB_4404 AGAIN ORDER TAG SEGMENT
nList = 0
SCAN
  nList = nList+1
  DIMENSION segList(nList)
  segList(nList) = SEGMENT
ENDSCAN
IF nList = 0
  DIMENSION segList(1)
  segList(1) = SPACE(FSIZE("SEGMENT"))
  nList = 1
ENDIF
SET ORDER TO TAG STO_CODE
IF .NOT. SEEK(c_code)
  APPEND BLANK
  REPLACE STO_CODE WITH ST_4404.CODE,   ;
          PREFIX   WITH ST_4404.PREFIX, ;
          NAME     WITH ST_4404.NAME,   ;
          PRODUCER WITH ST_4404.PRODUCER
ENDIF
m.tmpFunc    = FUNCTION
m.tmpSegment = SEGMENT
m.tmpCorpC   = COR_CODE
m.tmpCorpN   = CORPUS
m.tmpPdf     = DATASHEET
m.tmpImage   = IMAGE
FOR i = 1 TO nList
  IF SYS(15, m.lwr, m.tmpSegment) = SYS(15, m.lwr, segList(i))
    m.tmpSegment = i
    EXIT
  ENDIF
ENDFOR
IF TYPE("tmpSegment") = "C"
  m.tmpSegment = 1
ENDIF

                     &&   Объявляем и заполняем поля бланка
m.ex   = 1             &&

*
*┌──────────────────────────────────────────────────────────────────┐
*│Позиция ▒▒▒▒ ▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓▒▒▒▒█▒▒▒▒▓ ▒▒▒▒▓▒▒▒         │
*│                                                ┌────────┐        │
*│        [X] Корпус ....:....!....:.     Сегмент │....:...│        │
*│                                                └────────┘        │
*│┌── Функциональное назначение ───────────────────────────────────┐│
*││....:....!....:....!....:....!....:....!....:....!....:....!....││
*││....:....!....:....!....:....!....:....!....:....!....:....!....││
*││....:....!....:....!....:....!....:....!....:....!....:....!....││
*││....:....!....:....!....:....!....:....!....:....!....:....!....││
*││....:....!....:....!....:....!....:....!....:....!....:....!....││
*│└────────────────────────────────────────────────────────────────┘│
*│      ┌─ PDF файл ─────────────────────┐ ┌─Файл с изобр.──┐       │
*│      │....:....!....:....!....:....!..│ │....:....!....:.│       │
*│      └────────────────────────────────┘ └────────────────┘       │
*│                 < OK Ctrl-W >< Отказаться Esc >                  │
*└──────────────────────────────────────────────────────────────────┘
*│                             < OK >                               │

*------------------------------------------------------------------------

PUSH MENU _MSYSMENU
SET SYSMENU TO

SET SYSMENU AUTOMATIC

DEFINE PAD _Edit OF _MSYSMENU PROMPT "Радактировать" COLOR SCHEME 4
ON PAD _Edit OF _MSYSMENU ACTIVATE POPUP _Edit
DEFINE POPUP _Edit MARGIN RELATIVE SHADOW COLOR SCHEME 4
DEFINE BAR _MED_UNDO  OF _Edit PROMPT "Отмена"     KEY Ctrl+Z, "Ctrl-Z"
DEFINE BAR _MED_CUT   OF _Edit PROMPT "Вырезать"   KEY Ctrl+X, "Ctrl-X"
DEFINE BAR _MED_COPY  OF _Edit PROMPT "Копировать" KEY Ctrl+C, "Ctrl-C"
DEFINE BAR _MED_PASTE OF _Edit PROMPT "Втавить"    KEY Ctrl+V, "Ctrl-V"

PUSH KEY CLEAR       && На всякий пожарный случай!

DO Prp_Nav_2
DO D_Win_N WITH 23, 70, "Дополнительная информация по позиции"
DO Sun_Bord WITH  6,  2, 17, 67, " Функциональное назначение "
DO Sun_Bord WITH 18,  8, 20, 41, " PDF файл "
DO Sun_Bord WITH 18, 43, 20, 60, "Файл с изобр."

*      Ввод полей бланка
*
m.tmpName = ALLTRIM(ST_4404.PREFIX)+ALLTRIM(ST_4404.NAME)+" "+ALLTRIM(ST_4404.PRODUCER)
m.tmpName = PADR(m.tmpName, WCOLS()-12)
@  2, 2 SAY "Позиция"
@  2, COL()+1 GET m.tmpName
*@  2, COL()+1 GET ST_4404.PREFIX
*@  2, COL()+1 GET ST_4404.NAME
*@  2, COL()+1 GET ST_4404.PRODUCER
CLEAR GETS
@  4, 10 GET swCorp PICTURE "@*C Корпус" VALID G_Corp()
@  4, 21 GET tmpCorpN WHEN .F.
@  4, 42 SAY "Сегмент"
@  3, 50 GET tmpSegment FROM segList PICTURE "@^" SIZE 1, 10
IF stop_corr
  CLEAR GETS
  @  7,  3 EDIT m.tmpFunc SIZE 10, 63 NOMODIFY
  @ 19,  9 SAY  m.tmpPdf COLOR SCHEME 1
  @ 19, 44 SAY  m.tmpImage COLOR SCHEME 1
  @ WROWS()-2, 31 GET ex PICTURE "@*HT \! OK "
ELSE
  @  7,  3 EDIT tmpFunc SIZE 10, 63
  @ 19,  9 GET  m.tmpPdf
  @ 19, 44 GET  m.tmpImage
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
ENDIF
READ CYCLE

IF ex = 1
  REPLACE COR_CODE  WITH tmpCorpC, ;
          CORPUS    WITH tmpCorpN, ;
          FUNCTION  WITH tmpFunc,  ;
          DATASHEET WITH tmpPdf,   ;
          IMAGE     WITH tmpImage, ;
          SEGMENT   WITH segList(tmpSegment)
ENDIF

*--------------------------------------------------------------------------

SET SYSMENU OFF
POP MENU _MSYSMENU

POP KEY
RELEASE WINDOW (win_name)

USE IN ST_4404
USE IN SUB_4404
SELECT (sel_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Corp       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Выбор корпуса.                                  │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04.04.2004 ──┘
PROCEDURE G_Corp

DO Get_Corp WITH tmpCorpC, tmpCorpN
swCorp = .F.
SHOW GET tmpCorpN
SHOW GET swCorp

RETURN .T.
