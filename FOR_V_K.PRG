*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла For_V_K      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                          Отчет для Валентины.                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 04.06.1999 ══╝
PROCEDURE For_V_K
PRIVATE dt_0, dt_1, mn, ye, dt_w

mn = MONTH(DATE())
ye = YEAR(DATE())

dt_w = "01."+STR(mn,2)+"."+STR(ye,4)
dt_1 = CTOD(dt_w)-1
mn = mn-1
IF mn < 1
  mn = 12
  ye = ye-1
ENDIF
dt_w = "01."+STR(mn,2)+"."+STR(ye,4)
dt_0 = CTOD(dt_w)
dt_0 = DATE()
dt_1 = DATE()

IF .NOT. G_Period(dt_0, dt_1)
  RETURN
ENDIF

USE (base_path+"SALE") ORDER TAG DOC_NUM
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG HRON
SET RELATION TO FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM INTO SALE
SET SKIP TO SALE
PRIVATE sd_000, sr_000, q_000, dt_w

IF .NOT. SEEK(DTOS(dt_0))
  q_000 = RECNO(0)
  IF BETWEEN(q_000,1,RECCOUNT())
    GO q_000
  ENDIF
ENDIF
STORE 0 TO sd_000, sr_000, q_000

dt_w = DOC_DATE
DO Wt_Mess WITH " Подсчет... "
SCAN REST WHILE DOC_DATE <= dt_1
  IF dt_w # DOC_DATE
    dt_w = DOC_DATE
    @ WROWS()-1, 0 SAY PADC(DTOC(dt_w), WCOLS())
  ENDIF
  sd_000 = sd_000+SALE.QNT*SALE.PRICE_D
  sr_000 = sr_000+SALE.QNT*SALE.PRICE_R
  q_000  = q_000 +SALE.QNT
ENDSCAN

DO Wt_Mess

CLOSE DATABASES

DO V_Res

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура V_Res        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Просмотр результата.                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 04.06.1999 ──┘
PROCEDURE V_Res

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

*┌────────────────────────────────┐
*│   Количество  999 999          │
*│   Сумма (Руб) 999 999 999.99   │
*│   Сумма (USD) 999 999 999.99   │
*│                                │
*│            < OK >              │
*└────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 36, "Результат"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 5 SAY "Количество  "+TRANSFORM(q_000, "999 999")
@ 3, 5 SAY "Сумма (Руб) "+TRANSFORM(sr_000, "999 999 999.99")
@ 4, 5 SAY "Сумма (USD) "+TRANSFORM(sd_000, "999 999 999.99")
@ WROWS()-2, 14 GET ex PICTURE "@*HT \! OK "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
