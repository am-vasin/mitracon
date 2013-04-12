*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Ld_Note      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Примечание к позиции заказа                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 21.04.2006 ══╝
PROCEDURE Ld_Note
PARAMETERS prmList, prmCode, prmROnly
*
* prmList  - номер списка (C),
* prmCode  - код позиции (или NAME+PREFIX+PRODUCER для нескладских позиций),
* prmROnly - только чтение.
*

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
PRIVATE s_sav, tmpNote, tmpName, tmpPref, tmpProd
m.s_sav = SELECT()
SELECT 0
IF TYPE("m.prmCode") = "N"
  USE (m.base_path+"LIST_DET") SHARED AGAIN ALIAS LD_6421 ORDER TAG L_CODE
  IF .NOT. SEEK(m.prmList+STR(m.prmCode, 7))
    USE
    SELECT (m.s_sav)
    RETURN
  ENDIF
  m.tmpNote = NOTE

  USE (m.base_path+"STOCK") SHARED AGAIN ALIAS ST_6421 ORDER TAG CODE IN 0
  = SEEK(m.prmCode, "ST_6421")
  m.tmpName = ST_6421.NAME
  m.tmpPref = ST_6421.PREFIX
  m.tmpProd = ST_6421.PRODUCER
  USE IN ST_6421
ELSE
  USE (m.base_path+"LIST_DET") SHARED AGAIN ALIAS LD_6421 ORDER TAG L_NAME
  IF .NOT. SEEK(m.prmList+m.prmCode)
    USE
    SELECT (m.s_sav)
    RETURN
  ENDIF
  m.tmpNote = NOTE

  m.tmpName = NAME
  m.tmpPref = PREFIX
  m.tmpProd = PRODUCER
ENDIF

*┌───────────────────────────────────────────────────┐
*│ PPPP ....:....!....:....!....:....!....: PPPPPPPP │
*│┌─ Примечание ────────────────────────────────────┐│
*││                                                 ││
*││                                                 ││
*││                                                 ││
*││                                                 ││
*││                                                 ││
*││                                                 ││
*││                                                 ││
*││                                                 ││
*││                                                 ││
*│└─────────────────────────────────────────────────┘│
*│          < OK Ctrl-W > < Отказаться Esc >         │
*└───────────────────────────────────────────────────┘

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 16, 55, "Примечание к позиции заказа"
DO Sun_Bord WITH  3,  2, 13, 52, " Примечание "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
*@ 2, 4 SAY "Позиция"
@ 2, 3 SAY m.tmpPref COLOR SCHEME 1
@ 2, COL()+1 SAY m.tmpName COLOR SCHEME 1
@ 2, COL()+1 SAY m.tmpProd COLOR SCHEME 1
IF m.prmROnly
  @ WROWS()-2, FLOOR(WCOLS()/2-3) GET m.ex PICTURE "@*HT \! OK "
  @ 4, 3 EDIT m.tmpNote SIZE 9, 48 COLOR (SCHEME(1,1)) NOMODIFY
ELSE
  @ 4, 3 EDIT m.tmpNote SIZE 9, 48 COLOR (SCHEME(1,1))
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
ENDIF

READ CYCLE

IF m.ex = 1 .AND. .NOT. m.prmROnly
  *
  * Отрабатываем бланк
  IF .NOT. NOTE == m.tmpNote
    REPLACE NOTE WITH m.tmpNote
  ENDIF
ENDIF
*--------------------------------------------------------------------------
USE
SELECT (m.s_sav)
POP KEY
RELEASE WINDOW (win_name)

RETURN
