*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Tst1_INN     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                    Проверка на наличие дублей ИНН.                     ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 14.05.2006 ══╝
PROCEDURE Tst1_INN
PARAMETERS prmCus

PRIVATE tmpRet, s_sav, tmpName, tmpCount, tmpCName, tmpINN

m.tmpRet = .T.
m.tmpCount = 0

IF EMPTY(m.prmCus)
  RETURN m.tmpRet
ENDIF

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS ACC_6514
SEEK m.prmCus
m.tmpCName = ALLTRIM(CUS_NAME)
SELECT 0
USE (m.base_path+"CUS_BIL") SHARED AGAIN ORDER TAG CUS_CODE ALIAS BIL_6514
SEEK m.prmCus
IF EMPTY(INN)
  USE IN ACC_6514
  USE IN BIL_6514
  SELECT (m.s_sav)
  RETURN m.tmpRet
ENDIF
SELECT 0
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	CUS_NAME	C(50) )
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_LIST
SELECT BIL_6514
m.tmpINN = INN
SET ORDER TO TAG INN
SEEK m.tmpINN
SCAN REST WHILE INN = m.tmpINN FOR CUS_CODE # m.prmCus
  = SEEK(CUS_CODE, "ACC_6514")
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE CUS_NAME WITH ACC_6514.CUS_NAME
  SELECT BIL_6514
ENDSCAN

USE IN ACC_6514
USE IN BIL_6514
IF RECCOUNT("TMP_LIST") = 0
  USE IN TMP_LIST
  DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
  SELECT (m.s_sav)
  RETURN m.tmpRet
ENDIF

m.tmpRet = View_Dbl()

USE IN TMP_LIST
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
SELECT (m.s_sav)

RETURN m.tmpRet

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_Dbl     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                 Показываем дубли и спрашиваем как быть                 │
*│                                                                        │
*└────────────────────────────────────────────────────────── 14.05.2006 ──┘
PROCEDURE View_Dbl
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

*┌─────────────────────────────────────────────────┐
*│ ┌─ Заголовок рамки ───────────────────────────┐ │
*│ │                                             │ │
*│ └─────────────────────────────────────────────┘ │
*│                                                 │
*│        < OK Ctrl-W > < Отказаться Esc >         │
*└─────────────────────────────────────────────────┘

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH MIN(RECCOUNT("TMP_LIST")+6, 20), 68, "Проверка ИНН"
SELECT TMP_LIST
GO TOP

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2,  3 SAY "Для клиента "+ALLTRIM(m.tmpCName)
@  3,  3 SAY "существуют клиенты с таким же ИНН:"
SCAN NEXT 14
  @ ROW()+1, 3 SAY CUS_NAME
ENDSCAN
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*IF m.ex = 1
  *
  * Отрабатываем бланк
  
*ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.ex = 1
