*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Del_Inv      Разработчик Андрей Васин           28.09.98 16:03:50 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                  Удаление накладной отпуска В ФИЛИАЛ.                  ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Del_Inv

PRIVATE n_doc, d_doc, i, n_doc_w, f_code
SET DATE GERMAN
n_doc = SPACE(11)
d_doc = {}
f_code = "   "

DO Fill_Form
IF EMPTY(n_doc)
  RETURN
ENDIF

USE (base_path+"STOCK") ORDER TAG CODE
SELECT 0
USE (base_path+"ACT_S") ORDER TAG PARTY
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
SELECT 0
USE (base_path+"CUS_REST") ORDER TAG CUS_CODE
SELECT 0
USE (base_path+"RYN_L") ORDER TAG C_CODE
SELECT 0
USE (base_path+"SALE") ORDER TAG DOC_NUM
SELECT 0
USE (base_path+"SALE_TIT") ORDER TAG DOC_NUM
n_doc = ALLTRIM(n_doc)
i = AT("-", n_doc)
n_doc_w = f_code+LEFT(DTOS(d_doc),4)+LEFT(n_doc, i-1)+PADL(SUBSTR(n_doc, i+1), 6)
IF .NOT. SEEK(n_doc_w)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет накладной с номером "+n_doc+" от "+DTOC(d_doc)+"!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  CLOSE DATABASES
  RETURN
ENDIF

= SEEK(CUS_CODE,"ACCOUNT")
= SEEK(STR(CUS_CODE)+FIRM,"CUS_REST")
IF .NOT. Show_N()
  CLOSE DATABASES
  RETURN
ENDIF

SELECT CUS_REST
REPLACE ACC_SUM WITH ACCOUNT.ACC_SUM+SALE_TIT.TOTAL_R

SELECT SALE
SEEK n_doc_w
SCAN REST WHILE FIRM+LEFT(DTOS(DOC_DATE),4)+DOC_NUM = n_doc_w

  *  Возвращаем на склад
  = SEEK(CODE,"STOCK")
  = SEEK(PARTY, "ACT_S")
  REPLACE STOCK.QNT_REAL WITH STOCK.QNT_REAL+QNT, ;
          STOCK.QNT_REP  WITH STOCK.QNT_REP+QNT_REP
  REPLACE ACT_S.QNT_REST WITH ACT_S.QNT_REST+QNT, ;
          ACT_S.REST_REP WITH ACT_S.REST_REP+QNT_REP
  * Возвращаем на реализацию
  SELECT RYN_L
  IF .NOT. SEEK(STR(SALE_TIT.CUS_CODE,5)+STR(SALE.CODE,7))
    APPEND BLANK
    REPLACE CUS_CODE WITH SALE_TIT.CUS_CODE, ;
            CODE     WITH SALE.CODE
  ENDIF
  REPLACE QNT WITH SALE.QNT+QNT
  SELECT SALE
  DELETE

ENDSCAN

SELECT SALE_TIT
DELETE
CLOSE DATABASES

RETURN


*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                          Указываем накладную.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 28.09.98 16:27:21 ═╝
PROCEDURE Fill_Form

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

*┌───────────────────────────────────────┐
*│ Накладная N 99999999999 от ДД.ММ.ГГГГ │
*│                                       │
*│   < OK Ctrl-W > < Отказаться Esc >    │
*└───────────────────────────────────────┘

ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 6, 43, "Андрюха, заполни!"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2,  3 SAY "Накладная N" GET n_doc
@ 2, 27 SAY "от" GET d_doc PICTURE "@D"
@ 4,  5 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF ex = 2
  n_doc = SPACE(11)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                        Подтверждаем возврат...                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 28.09.98 16:33:22 ═╝
PROCEDURE Show_N

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

*┌───────────────────────────────────────────────────────────────┐
*│ Накладная N 99999999999 от ДД.ММ.ГГГГ на сумму 999 999 999.99 │
*│    ┌─ Клиент ─────────────────────────────────────────────┐   │
*│    │  ....:....!....:....!....:....!....:....!....:....!  │   │
*│    └──────────────────────────────────────────────────────┘   │
*│               < OK Ctrl-W > < Отказаться Esc >                │
*└───────────────────────────────────────────────────────────────┘

PRIVATE ex, nm_w     &&
                     &&   Объявляем и заполняем поля бланка
ex   = 1             &&
nm_w = CUS_NAME      &&
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 67, "Андрюха, подтверди..."
DO Sun_Bord WITH 3, 6, 5, 61, " Клиент "
@ 2, 3 SAY "Накладная N "+PADR(n_doc,11)+" от "+DTOC(d_doc)+ ;
   " на сумму "+TRANSFORM(SALE_TIT.TOTAL_R,"999 999 999.99")
@ 4, 9 SAY ACCOUNT.CUS_NAME
*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 6, 17 GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN ex = 1
