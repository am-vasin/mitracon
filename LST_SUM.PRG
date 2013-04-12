*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Lst_Sum      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                            Сумма по списку.                            ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 06.03.1999 ══╝
PROCEDURE Lst_Sum
PARAMETERS l_num

PRIVATE s_sav, s000, is_rub, mss, l_chr

s_sav = SELECT()
SELECT 0
USE (base_path+"LIST_TIT") ORDER TAG LST_NUM ALIAS L_9306 AGAIN
IF .NOT. SEEK(l_num)
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет списка с номером "+ALLTRIM(STR(l_num))+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF
is_rub = .NOT. IS_USD

USE (base_path+"LIST_DET") ORDER TAG LIST ALIAS L_9306 AGAIN
s000 = 0
l_chr = STR(l_num,6)
SEEK l_chr
SCAN REST WHILE l_chr = LIST_NUM
  IF is_rub
    s000 = s000+round(SAL_PRICE*QNT,2)
  ELSE
    s000 = s000+USD_PRICE*QNT
  ENDIF
ENDSCAN
USE
SELECT (s_sav)

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
*Сумма по списку N 999999
*┌─────────────────────────┐
*│   99999999999.999 руб   │
*│                         │
*│  Нажмите что-нибудь...  │
*└─────────────────────────┘

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

PUSH KEY CLEAR       && На всякий пожарный случай!
DO D_Win_N WITH 6, 29, "Сумма по списку N "+ALLTRIM(l_chr)

*------------------------------------------------------------------------
*      Ввод полей бланка
*
IF is_rub
  @ 2, 2 SAY PADC(ALLTRIM(TRANSFORM(s000, "999999999999.99")+" руб"), WCOLS()-4)
ELSE
  @ 2, 2 SAY PADC(ALLTRIM(TRANSFORM(s000, "9999999999.9999")+" USD"), WCOLS()-4)
ENDIF
@ 4, 4 SAY "Нажмите что-нибудь..."
= INKEY(0, "MH")
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
