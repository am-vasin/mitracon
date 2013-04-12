*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Time_Inc     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                             Срок поставки                              ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 24.12.2005 ══╝
PROCEDURE Time_Inc
PARAMETERS s_code

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
PRIVATE s_sav, tmpC

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

*┌─────────────────────────────────────────────┐
*│ ┌─ Наименование позиции ──────────────────┐ │
*│ │.... ....:....!....:....!....: ....:....!│ │
*│ └─────────────────────────────────────────┘ │
*│        Срок поставки ....:....!....:.       │
*│               < Выход Esc >                 │
*└─────────────────────────────────────────────┘

PRIVATE ex, tmpCode, tmpTime, tmpName
m.ex = 1

m.s_sav = SELECT()

USE (m.base_path+"STOCK") SHARED AGAIN ORDER TAG CODE ALIAS ST_5C24 IN 0
= SEEK(m.s_code, "ST_5C24")
m.tmpCode = ST_5C24.PRO_CODE
m.tmpName = ALLTRIM(ST_5C24.PREFIX)
m.tmpName = m.tmpName+IIF(EMPTY(m.tmpName), ""," ")+ALLTRIM(ST_5C24.NAME)

SELECT 0
DO Use_DMS WITH "DMS_5C24"
SET ORDER TO TAG CODE
m.tmpC = ALLTRIM(STR(m.s_code))
m.tmpC = PADR(m.tmpC, FSIZE("CODE", "DMS_5C24"))
= SEEK(m.tmpC, "DMS_5C24")
m.tmpTime = DMS_5C24.SUP_DATE

USE
SELECT (m.s_sav)

USE (m.base_path+"DEVELOP") SHARED AGAIN ORDER TAG CODE ALIAS PRO_5C24 IN ST_5C24
= SEEK(m.tmpCode, "PRO_5C24")
IF .NOT. EMPTY(PRO_5C24.B_NAME)
  m.tmpName = m.tmpName+" /"+ALLTRIM(PRO_5C24.B_NAME)+"/"
ENDIF
IF EMPTY(m.tmpTime)
  m.tmpTime = PRO_5C24.PERIOD
ENDIF
USE IN PRO_5C24
IF EMPTY(m.tmpTime)
  PRIVATE mss
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Для позиции срок поставки не указан."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 49, "Срок поставки для позиции"
DO Sun_Bord WITH  2,  3,  4, 45, " Наименование позиции "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  3,  4 SAY m.tmpName
@  5, 10 SAY "Срок поставки "+m.tmpTime

@ WROWS()-2, FLOOR(WCOLS()/2-6) GET m.ex PICTURE "@*HT \? Выход Esc "

READ CYCLE

*IF m.ex = 1
  *
  * Отрабатываем бланк
  
*ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
