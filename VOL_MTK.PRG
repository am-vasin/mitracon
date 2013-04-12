*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Vol_Mtk      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Продажи через МиТраКон в оптовых ценах                 ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 01/10/2002 ══╝
PROCEDURE Vol_Mtk

PRIVATE d0, d1, d_type, s0

d_type = SET("DATE")
SET DATE GERMAN
d0 = DTOC(DATE())
d0 = "01"+SUBSTR(d0,3)
d0 = CTOD(d0)
d1 = d0-1
d0 = GOMONTH(d0,-1)
SET DATE (d_type)
IF .NOT. G_Period(d0, d1)
  RETURN
ENDIF

USE (base_path+"STOCK") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (base_path+"SALE") SHARED AGAIN ORDER TAG DOC_NUM
SET RELATION TO CODE INTO STOCK
SELECT 0
USE (base_path+"SALE_TIT") SHARED AGAIN ORDER TAG HRON
SET FILTER TO LEFT(DOC_NUM,1) = "B"

DO Wt_Mess WITH "Подготовка данных"
s0 = 0
d_type = SET("NEAR") = "OFF"
SET NEAR ON
SEEK DTOS(d0)
SCAN REST WHILE DOC_DATE <= d1
  SELECT SALE
  SEEK SALE_TIT.FIRM+LEFT(DTOS(SALE_TIT.DOC_DATE),4)+SALE_TIT.DOC_NUM
  SCAN REST WHILE SALE_TIT.FIRM=FIRM .AND. SALE_TIT.DOC_DATE=DOC_DATE .AND. SALE_TIT.DOC_NUM=DOC_NUM
    s0 = s0+QNT*STOCK.PRICE
  ENDSCAN
  SELECT SALE_TIT
ENDSCAN
USE
SELECT SALE
USE
SELECT STOCK
USE

DO Wt_Mess
DO View_Res

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_Res     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                               Результат                                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10.01.2002 ──┘
PROCEDURE View_Res
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

*┌────────────────┐
*│ 9999999999.999 │
*│     < OK >     │
*└────────────────┘

PRIVATE ex, c
ex = 1
c = ALLTRIM(TRANSFORM(s0, "99 999 999.999"))
c = PADC(c, 14)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 5, 20, "Итого..."

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 3 SAY c
@ WROWS()-2, 7 GET ex PICTURE "@*HT \! OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
