*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║          Программа определения скидки для списка.                     ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE Dis_List
PARAMETERS l_num
*
*  l_num - номер списка.
*
PRIVATE s_sav, is_d

s_sav = SELECT()
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM AGAIN ALIAS TMP_9927
IF .NOT. SEEK(l_num)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет такого списка!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
is_d = IS_USD
USE (base_path+"LIST_DET") ORDER TAG LIST AGAIN ALIAS TMP_9927
PRIVATE s0, s_sp, s_dis
s0    = 0
s_sp  = 0
s_dis = 0
SEEK STR(l_num,6)
SCAN REST WHILE STR(l_num,6) = LIST_NUM
  IF is_d
    IF EMPTY(IS_SPEC)
      s_dis = s_dis + QNT*ROUND(USD_DEF-USD_PRICE,4)
    ELSE
      s_sp = s_sp + QNT*ROUND(USD_DEF-USD_PRICE,4)
    ENDIF
    s0 = s0+QNT*ROUND(USD_DEF,4)
  ELSE
    IF EMPTY(IS_SPEC)
      s_dis = s_dis + QNT*ROUND(DEF_PRICE-SAL_PRICE,2)
    ELSE
      s_sp = s_sp + QNT*ROUND(DEF_PRICE-SAL_PRICE,2)
    ENDIF
    s0 = s0+QNT*ROUND(DEF_PRICE,2)
  ENDIF
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
DO D_Win_N WITH 7, 56, "Список N "+ALLTRIM(STR(l_num))+ ;
                   IIF(is_d, " (У Е)"," (руб)")
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
