*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                  Программа определения скидки.                        ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Dis_Inv
PARAMETERS f_code, inv_n, inv_d
*
*  f_code - код предприятия;
*  inv_n  - номер накладной ( внутренний формат );
*  inv_d  - либо дата накладной либо год в символьном или числовом формате;
*
PRIVATE s_sav, d_year, mss

IF TYPE("inv_d") = "D"
  d_year = LEFT(DTOS(inv_d),4)
ENDIF
IF TYPE("inv_d") = "N"
  d_year = STR(inv_d,4)
ENDIF
IF TYPE("inv_d") = "C"
  d_year = inv_d
ENDIF

s_sav = SELECT()
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM AGAIN ALIAS SL_9927
IF .NOT. SEEK(f_code+d_year+inv_n)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Накладная не найдена или пуста."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
PRIVATE s0, s_sp, s_dis
s0    = 0
s_sp  = 0
s_dis = 0
SCAN REST WHILE f_code = FIRM .AND. d_year = LEFT(DTOS(DOC_DATE),4) .AND. inv_n = DOC_NUM
  IF PRICE_TYPE = "S"
    s_sp = s_sp + QNT*ROUND(PRICE_R_L-PRICE_R,2)
  ENDIF
  IF PRICE_TYPE = "D"
    s_dis = s_dis + QNT*ROUND(PRICE_R_L-PRICE_R,2)
  ENDIF
  s0 = s0+QNT*ROUND(PRICE_R_L,2)
ENDSCAN

DO View_Dis

USE
SELECT (s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура View_Dis     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                        Показываем результат...                         │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.09.1999 ──┘
PROCEDURE View_Dis
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

*┌────────────────────────────────────────────────────┐
*│ Сумма по прайсовым ценам 99 999 999 999.99         │
*│ Скидка                   99 999 999 999.99 999.99% │
*│ Спеццены                 99 999 999 999.99 999.99% │
*│                       < OK >                       │
*└────────────────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 56, "Накладная N "+LEFT(inv_n,4)+"-"+ALLTRIM(SUBSTR(inv_n,5))+ ;
                   "/"+f_code
*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2, 3 SAY "Сумма по прайсовым ценам "+TRANSFORM(s0, "99 999 999 999.99")
@ 3, 3 SAY "Скидка                   "+TRANSFORM(s_dis, "99 999 999 999.99")+ ;
               TRANSFORM(ROUND(100*s_dis/s0,2), " 999.99")+"%"
@ 4, 3 SAY "Спеццены                 "+TRANSFORM(s_sp, "99 999 999 999.99")+ ;
               TRANSFORM(ROUND(100*s_sp/s0,2), " 999.99")+"%"
@ WROWS()-2, 25 GET ex PICTURE "@*HT \ OK "

READ CYCLE

IF ex = 1
  *
  * Отрабатываем бланк
  
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
