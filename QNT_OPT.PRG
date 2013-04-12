*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Qnt_Opt     Разработчик Галина Дмитриенко                  ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                  Установка мелкооптовых количеств.                     ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 16.10.2001 ══╝
PROCEDURE Qnt_Opt

PRIVATE s_sav

s_sav = SELECT()

IF sale_mode # "Mitracon"
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
PRIVATE stor, temp, qnt_sal, sum_sal, ex, tmp, tmp_r
ex = 1
tmp = 0
stor = PADR(Get_Sys("QNT_OPT"), 40)
temp = SUBSTR(stor,1,10)
qnt_sal = IIF(EMPTY(temp), "0", temp)
qnt_sal = VAL(qnt_sal)

temp = SUBSTR(stor,12,10)
sum_sal = IIF(EMPTY(temp), "0", temp)
sum_sal = VAL(sum_sal)

temp = SUBSTR(stor,23,10)
tmp_r = IIF(EMPTY(temp), "0", temp)
tmp_r = VAL(tmp_r)

*┌───────────────────────────────────┐
*│ Минимальная сумма продажи 99.999  │
*│                                   │
*│ Минимальное количество    999999  │
*│                                   │
*│ Кратность                 999999  │
*│                                   │
*│ < OK Ctrl-W > < Отказаться Esc >  │
*└───────────────────────────────────┘

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 11, 39, "Продажи мелким оптом"

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
@ 3, 3 SAY "Минимальная сумма продажи " GET sum_sal PICTURE "99.99"
@ 5, 3 SAY "Минимальное количество    " GET qnt_sal PICTURE "99999"
@ 7, 3 SAY "Кратность                 " GET tmp_r PICTURE "99999"

READ CYCLE
POP KEY
RELEASE WINDOW (win_name)

IF ex = 1

  DO Put_Sys WITH "QNT_OPT", STR(qnt_sal)+" "+STR(sum_sal)+" "+STR(tmp_r)
  
  USE(base_path+"STOCK") ALIAS STO_1A16 AGAIN ORDER TAG NAME_P
  SCAN
    WAIT WINDOW PREFIX+" "+NAME+" "+PRODUCER NOWAIT
    IF QNT_ROUND > 1
      REPLACE QNT_OPT WITH QNT_ROUND
    ELSE
      IF PRICE # 0
        IF sum_sal/PRICE <= qnt_sal
          REPLACE QNT_OPT WITH qnt_sal
        ELSE
          IF PRICE >= sum_sal
            tmp = qnt_sal
          ELSE
            tmp = ROUND(sum_sal/PRICE,0)
            IF tmp <= qnt_sal
              tmp = qnt_sal
            ELSE
              DO WHILE MOD(tmp,tmp_r) # 0
                tmp = tmp - 1
              ENDDO
            ENDIF
          ENDIF
          REPLACE QNT_OPT WITH tmp
        ENDIF
      ENDIF
    ENDIF
  ENDSCAN
  USE
ENDIF

SELECT(s_sav)
RETURN