*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║               Выбор платежного документа для удаления.                ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE D_Money

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

*   Открываем файлы БД
PRIVATE ex, tmpDoc, tmpFirm, tmpType, tmpCode, tmpYear, sel_sav
PRIVATE tmpName, tmpReason, nFirms, t_list, tmp1Doc, t_name
tmpYear = YEAR(DATE())
tmpReason = SPACE(48)
DIMENSION t_list(7), t_name(7)
t_list(1) = " "
t_name(1) = "Приходный ордер "

t_list(2) = " "
t_name(2) = "Расходный ордер "

t_list(3) = "1"
t_name(3) = "Б/Н платеж "

t_list(4) = "3"
t_name(4) = "Индексируемый пл."

t_list(5) = "Ч"
t_name(5) = "Чек      "

t_list(6) = "Б"
t_name(6) = "Сбербанк "

t_list(7) = "4"
t_name(7) = "Индкс. Б/Н "

sel_sav = SELECT()

SELECT 0
USE (base_path+"FIRMS") ORDER TAG B_NAME ALIAS D_MONEY AGAIN
nFirms = 0
tmpFirm = 1
tmpType = 1
tmpDoc  = 0
SCAN
  nFirms = nFirms+1
  DIMENSION tmpCode(nFirms), tmpName(nFirms)
  tmpCode(nFirms) = FIRM_CODE
  tmpName(nFirms) = FIRM_CODE+" "+B_NAME
ENDSCAN
USE
SELECT (sel_sav)
IF nFirms = 0
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одного предприятия"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

*    Удаление платежного документа
*┌─────────────────────────────────────────────────────────────┐
*│ Год 9999  Номер документа (только цифровая часть) 999999999 │
*│       ┌───────────────────┐             ┌─────────────────┐ │
*│ Фирма │                   │ Тип платежа │                 │ │
*│       └───────────────────┘             └─────────────────┘ │
*│ Примечание ....:....!....:....!....:....!....:....!....:... │
*│              < OK Ctrl-W > < Отказаться Esc >               │
*└─────────────────────────────────────────────────────────────┘

ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 9, 65, "Удаление платежного документа"
*------------------------------------------------------------------------
*      Ввод полей бланка
*

@  2,  3 SAY "Год" GET tmpYear PICTURE "9999"
@  2, 13 SAY "Номер документа (только цифровая часть)" GET tmpDoc PICTURE "@Z 999999999"
@  4,  3 SAY "Фирма"
@  4, 31 SAY "Тип платежа"
@  3,  9 GET tmpFirm  PICTURE "@^" FROM tmpName SIZE 1, 19
@  3, 43 GET tmpType  PICTURE "@^" FROM t_name  SIZE 1, 17
@  6,  3 SAY "Примечание" GET tmpReason
@  7, 14 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  DO S_Money
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN

*╔═══════════════════════════════════════════════════════════════════════╗
*║                                                                       ║
*║                         Просмотр документа.                           ║
*║                                                                       ║
*╚═══════════════════════════════════════════════════════════════════════╝
PROCEDURE S_Money

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

*   Открываем файлы БД
PRIVATE ex, sel_sav, mss

sel_sav = SELECT()

SELECT 0
USE (base_path+"INCMONEY") ORDER TAG DOC_NUM ALIAS M_MONEY AGAIN
IF tmpType = 2
  tmp1Doc = STR(-tmpDoc, 9)
ELSE
  tmp1Doc = STR(tmpDoc, 9)
ENDIF
tmp1Doc = sys_char+tmp1Doc
IF .NOT. SEEK(tmpCode(tmpFirm)+t_list(tmpType)+STR(tmpYear,4)+tmp1Doc)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Документ не найден"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (sel_sav)
  RETURN
ENDIF

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS C_MONEY AGAIN
SEEK M_MONEY.CUS_CODE

SELECT 0
USE (base_path+"FIRMS") ORDER TAG FIRM_CODE ALIAS F_MONEY AGAIN
SEEK M_MONEY.FIRM


*    Удаление платежного документа
*┌───────────────────────────────────────────────────────────┐
*│ Документ № A999999999 от 99.99.9999                       │
*│                                                           │
*│ Тип документа:                                            │
*│                                                           │
*│ Клиент ....:....!....:....!....:....!....:....!....:....! │
*│                                                           │
*│ Сумма 999 999 999.99р    999 999 999.99$                  │
*│                                                           │
*│           < Удалить Ctrl-W > < Отказаться Esc >           │
*└───────────────────────────────────────────────────────────┘

ex   = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 12, 63, "Удаление платежного документа"
*------------------------------------------------------------------------
*      Ввод полей бланка
*

@  2,  3 SAY "Документ № "+LEFT(M_MONEY.DOC_NUM, 1)+ALLTRIM(SUBSTR(M_MONEY.DOC_NUM, 2))
@  2, 25 SAY "от "+DTOC(M_MONEY.DOC_DATE)
@  4,  3 SAY "Тип документа: "+t_name(tmpType)
@  6,  3 SAY "Клиент "+ALLTRIM(C_MONEY.CUS_NAME)
@  8,  3 SAY "Сумма "+TRANSFORM(M_MONEY.MONEY_R, "999 999 999.99")+"р    "+ ;
             TRANSFORM(M_MONEY.MONEY_D, "999 999 999.99")+"$"
@ 10, 13 GET ex PICTURE "@*HT \! Удалить Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 1
  DO Del_Mon WITH tmpCode(tmpFirm), t_list(tmpType), tmp1Doc, tmpYear, tmpReason
ENDIF
*--------------------------------------------------------------------------

USE IN C_MONEY
USE IN F_MONEY
USE IN M_MONEY
SELECT (sel_sav)
POP KEY
RELEASE WINDOW (win_name)

RETURN
