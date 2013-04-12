*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Set_Nsp      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║         Уснановка сумма нал. денег для накладной (расчет НСП).         ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 10.04.2000 ══╝
PROCEDURE Set_Nsp
PARAMETERS pFirm, pNum, pDate

PRIVATE d_year, s_sav

IF TYPE("pDate") = "D"
  d_year = LEFT(DTOS(pDate),4)
ENDIF
IF TYPE("pDate") = "N"
  d_year = STR(pDate,4)
ENDIF
IF TYPE("inv_d") = "C"
  d_year = pDate
ENDIF

s_sav = SELECT()

SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM ALIAS S_0410
IF .NOT. SEEK(pFirm+d_year+pNum)
  USE
  SELECT (s_sav)
  RETURN
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

*┌──────────────────────────────────────────────────────┐
*│ ┌─ Клиент ─────────────────────────────────────────┐ │
*│ │....:....!....:....!....:....!....:....!....:....!│ │
*│ └──────────────────────────────────────────────────┘ │
*│ Сумма по накладной 999999999.99   Процент НСП 999.99 │
*│                                                      │
*│ Наличная часть 999999999.99   Сумма НСП 999999999.99 │
*│                                                      │
*│           < OK Ctrl-W > < Отказаться Esc >           │
*└──────────────────────────────────────────────────────┘

PRIVATE ex, n_part, s_nsp
t_sum  = TOTAL_R
p_nsp  = NSP_
n_part = KASSA
s_nsp  = ROUND(n_part*NSP_/100, 2)
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 11, 58, "Накладная N "+LEFT(DOC_NUM,4)+"-"+  ;
                        ALLTRIM(SUBSTR(DOC_NUM, 5))+"/"+FIRM
DO Sun_Bord WITH  2,  3,  4, 54, " Клиент "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  4 GET CUS_NAME WHEN .F.
@ 5,  3 SAY "Сумма по накладной" GET TOTAL_R WHEN .F. PICTURE "999999999.99"
@ 5, 37 SAY "Процент НСП" GET NSP_ WHEN .F. PICTURE "999.99"
@ 7,  3 SAY "Наличная часть" GET n_part VALID T_Val() PICTURE "999999999.99"
@ 7, 33 SAY "Сумма НСП" GET s_nsp WHEN .F. PICTURE "999999999.99"

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  REPLACE KASSA WITH n_part
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
USE
SELECT (s_sav)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура T_Val        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                     Проверка введенного значения.                      │
*│                                                                        │
*└────────────────────────────────────────────────────────── 10.04.2000 ──┘
PROCEDURE T_Val

PRIVATE mss

IF ex = 2 .OR. READKEY() % 256 = 12
  RETURN .T.
ENDIF

IF ABS(n_part) > ABS(TOTAL_R)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Наличная часть не может превышать общей суммы!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

IF SIGN(n_part) # SIGN(TOTAL_0) .AND. n_part # 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Наличная часть должна иметь тот же знак, что и общая сумма!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

s_nsp  = ROUND(n_part*NSP_/100, 2)
SHOW GET s_nsp

RETURN .T.
