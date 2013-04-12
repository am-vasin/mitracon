*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Crs_Inv      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                           Курс по накладной.                           ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 18.12.1998 ══╝
PROCEDURE Crs_Inv
PARAMETERS d_firm, d_num, d_date, m_name

PRIVATE s_sav, c1, c2

s_sav = SELECT()
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM ALIAS TMP_8C18 AGAIN
IF .NOT. SEEK(d_firm+LEFT(DTOS(d_date),4)+d_num)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет такой накладной!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

c1 = 0
c2 = 0
SCAN REST WHILE DOC_NUM = d_num .AND. DOC_DATE = d_date .AND. FIRM = d_firm
  c1 = c1+QNT*PRICE_D
  c2 = c2+QNT*PRICE_R
ENDSCAN
IF c1 # 0
  c1 = ABS(ROUND(c2/c1,3))
ENDIF
USE

c2 = Get_Curs("CURS", "", d_date)

SELECT (s_sav)

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
* Накладная N A000-000000 от 99.99.9999
*┌──────────────────────────────────────┐
*│                                      │
*│      Курс по накладной  999.999      │
*│                                      │
*│      Курс на 99.99.9999 999.999      │
*│                                      │
*│        Нажмите что-нибудь...         │
*└──────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 42, "Накладная N "+LEFT(d_num,4)+"-"+ALLTRIM(RIGHT(d_num,6))+" от "+DTOC(d_date)

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  8 SAY "Курс по накладной  "+TRANSFORM(c1, "999.999")
@ 5,  8 SAY "Курс на "+DTOC(d_date)
@ 5, 27 SAY TRANSFORM(c2, "999.999")
@ WROWS()-2, 11 SAY "Нажмите что-нибудь..."
= INKEY(0,"MH")
IF TYPE("m_name") = "C"
  IF .NOT. EMPTY(m_name)
    SHOW MENU (m_name)
  ENDIF
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN
