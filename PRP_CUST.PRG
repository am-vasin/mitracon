*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл prp_cust     Разработчик Андрей Васин           14.04.97 16:43:53 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║               Подготовка бланков клиентов для площадок.                ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Prp_Cust

PRIVATE mss, pl_list, pl_chr, pl_num, cnt
IF (sys_char # "A" .AND. sale_mode # "Mitin")
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Создавать бланки можно только на основной площадке!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

pl_num = 0
pl_chr = ""

USE (base_path+"PLACES")
SCAN
  IF P_CHAR = "A"
    LOOP
  ENDIF
  pl_num = pl_num+1
  DIMENSION pl_list(pl_num)
  pl_list(pl_num) = P_NAME
  pl_chr = pl_chr+P_CHAR
ENDSCAN
USE (base_path+"ACCOUNT") ORDER TAG EMP_NAME

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

PRIVATE ex, point, n, c, i, n_flt
ex   = 1
point = 1
n = 0
*------------------------------------------------------------------------
*
*┌────────────────────────────────────────────────┐
*│          Площадки                              │
*│ ┌─────────────────────────┐ ┌ Кол-во бланков ┐ │
*│ │ ....:....!....:....!....│ │ Создано 99999  │ │
*│ │ ....:....!....:....!....│ │ Добавить 9999  │ │
*│ │ ....:....!....:....!....│ └────────────────┘ │
*│ │ ....:....!....:....!....│                    │
*│ └─────────────────────────┘                    │
*│                                                │
*│        < OK Ctrl-W > < Отказаться Esc >        │
*│                                                │
*└────────────────────────────────────────────────┘

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH MAX(9+MIN(pl_num,4),11), 52, "Бланки клиентов"
@ 2, 12 SAY "Площадки"
DO Sun_Bord WITH 3, 31, 6, 48
@ 3, 32 SAY " Кол-во бланков "
@ 4, 34 SAY "Создано"
= Cnt_Emp()
@ 5, 34 SAY "Добавить"
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  3 GET point PICTURE "@&" FROM pl_list SIZE pl_num+2, 27 VALID Cnt_Emp()
@ 5, 43 GET n PICTURE "@Z 999"
@ WROWS()-3, 10 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

cnt = 0
IF ex = 1 .AND. n > 0
  SELECT ACCOUNT
  c = SUBSTR(pl_chr, point,1)
  DO Wt_Mess WITH "Создаем..."
  FOR i = 1 TO n
    n_flt = Doc_Num("CUS_CODE", def_firm, arm, DATE())
    APPEND BLANK
    REPLACE SUBSYS WITH c, CUS_CODE WITH n_flt
    cnt = cnt+1
    @ WROWS()-1, 0 SAY PADC(ALLTRIM(STR(cnt)), WCOLS())
  ENDFOR
  DO Wt_Mess
ENDIF
USE
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                     Подсчет бланков для площадки.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 15.04.97 10:52:28 ═╝
PROCEDURE Cnt_Emp

PRIVATE i, c

i = 0
c = SUBSTR(pl_chr, point,1)
IF SEEK(c)
  COUNT REST WHILE c = SUBSYS TO i
ENDIF

@ 4, 41 SAY STR(i,5)

RETURN .T.
