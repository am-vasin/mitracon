*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                 Установка фильтра для счетов-фактур.                   ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 03.04.97 17:12:25 ═╝
PROCEDURE Bill_flt

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
PRIVATE ex, p01, p02, p03, p04, p05, p06, l1, c1, ln, wk_arr
PRIVATE p07, p08

DIMENSION wk_arr(p_numb)
FOR ln = 1 TO p_numb
  wk_arr(ln) = CHR(ASC("A")+ln-1) $ p_line
ENDFOR
ln = MAX(15, 11+p_numb)

p01 = is_hr
p02 = dt1
p03 = dt2
p04 = f_c000
p05 = g_shab
p06 = c_shab
p07 = nds_0
p08 = nds_1
ex  = 1

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH ln, 59, "Установите фильтр"

*
*┌───────────────────────────────────────────────────────┐
*│ ┌─ Интервал дат ────────────────────────────────────┐ │
*│ │   [ ] Установить    С ДД.ММ.ГГ    по ДД.ММ.ГГ     │ │
*│ └───────────────────────────────────────────────────┘ │
*│         Предприятие 999 9999999999999999 (F1)         │
*│┌─ Товар ──────────────┐┌─ Площадки ──────────────────┐│
*││ ....:....!....:....! ││ [ ] ....:....!....:....!....││
*│└──────────────────────┘│ [ ]                         ││
*│┌─ Клиент ─────────────┐│ [ ]                         ││
*││ ....:....!....:....! ││                             ││
*│└──────────────────────┘└─────────────────────────────┘│
*│                 НДС с 999.9 по 999.9                  │
*│         < OK Ctrl-W > < Отказаться Esc >              │
*└───────────────────────────────────────────────────────┘

DO Sun_Bord WITH  2,  3, 4, 55, " Интервал дат "
DO Sun_Bord WITH  6,  2,  8, 25, " Клиент "
DO Sun_Bord WITH  9,  2, 11, 25, " Товар "
DO Sun_Bord WITH  6, 26, WROWS()-4, 56, " Площадки "

@  3,  7 GET p01 PICTURE "@*C Установить" VALID P01_V()
@  3, 25 SAY "С" GET p02 PICTURE "@D" WHEN p01
@  3, 39 SAY "по" GET p03 PICTURE "@D" WHEN p01
DO Get_Firm WITH 5, 11, enab_firm
@  7,  4 GET p06
@ 10,  4 GET p05
DO G_Place WITH 7, 28, wk_arr
@ WROWS()-3, 19 SAY "НДС с" GET p07 PICTURE "999.9"
@ WROWS()-3, 31 SAY "по" GET p08 PICTURE "999.9"
@ WROWS()-2, 11 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

PRIVATE s, s0
POP KEY
RELEASE WINDOW (win_name)

IF READKEY() % 256 = 12 .OR. ex = 2
  f_c000 = p04
  RETURN .F.
ENDIF

is_hr  = p01
dt1    = p02
dt2    = p03
nds_0  = p07
nds_1  = p08
g_shab = SYS(15, lwr, p05)
c_shab = SYS(15, lwr, p06)
p_line = ""
FOR ln = 1 TO p_numb
  IF wk_arr(ln)
    p_line = p_line+CHR(ASC("A")+ln-1)
  ENDIF
ENDFOR
DO Prp_Tmp

RETURN .T.

FUNCTION P01_V     &&  p01 VALID
#REGION 1
IF .NOT. p01
  p02 = {}
  p03 = DATE()
  SHOW GET p01
  SHOW GET p02
ENDIF
RETURN .T.
