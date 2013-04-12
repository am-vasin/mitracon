*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла SeekList     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                         Поиск списка по номеру                         ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 27.02.2006 ══╝
PROCEDURE SeekList

PRIVATE tmpNum, mss, s_sav, r_val

m.tmpNum = Get_List()
IF EMPTY(m.tmpNum)
  RETURN 0
ENDIF

m.s_sav = SELECT()
SELECT 0
USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS LT_6227
IF .NOT. SEEK(m.tmpNum)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Список не найден..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (m.s_sav)
  RETURN 0
ENDIF

IF LT_6227.LST_ARM # m.arm
  DO L_Info
  USE
  SELECT (m.s_sav)
  RETURN 0
ENDIF

m.r_val = RECNO("LT_6227")
USE
SELECT (m.s_sav)

RETURN m.r_val

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура L_Info       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Сообщение о списке                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.02.2006 ──┘
PROCEDURE L_Info
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
PRIVATE p, a

USE (m.base_path+"ARM_LST") SHARED AGAIN ORDER TAG ARM_C ALIAS A_6227 IN 0
=SEEK(LT_6227.LST_ARM, "A_6227")
m.a = PADR(A_6227.ARM_N, 50)
USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS A_6227 IN A_6227
=SEEK(LT_6227.WHO_CR, "A_6227")
m.p = ALLTRIM(A_6227.FAMILY)+" "+LEFT(A_6227.NAME, 1)+" "+LEFT(A_6227.S_NAME, 1)
m.p = PADR(m.p, 50)
USE IN A_6227

*┌───────────────────────────────────────────────────────────────────┐
*│ Рабочее место ....:....!....:....!....:....!....:....!....:....!  │
*│                                                                   │
*│ Менеджер      ....:....!....:....!....:....!....:....!....:....!  │
*│                          < OK >                                   │
*└───────────────────────────────────────────────────────────────────┘

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 71, "Список № "+ALLTRIM(STR(LT_6227.LST_NUM))
*DO Sun_Bord WITH  2,  2,  4, 49, " Заголовок рамки "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 3 SAY "Рабочее место" GET m.a
@ 4, 3 SAY "Менеджер     " GET m.p
CLEAR GETS

@ WROWS()-2, FLOOR(WCOLS()/2-3) GET m.ex PICTURE "@*HT \! OK  "

READ CYCLE

IF m.ex = 1
  *
  * Отрабатываем бланк
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
