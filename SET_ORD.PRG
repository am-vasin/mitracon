*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Set_Ord      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║               Добавление номера приходного ордера в чек.               ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 16.12.2004 ══╝
PROCEDURE Set_Ord
PARAMETERS pFirm, pType, pDoc, pDate

*
* pFirm - фирма;
* pType - тип док-та;
* pDoc  - номер документа;
* pDate - дата документа.
*
PRIVATE tmpYear, s_sav, tmpDoc, tmpCus

IF m.pType # "Ч"
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Документ не является чеком!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN
ENDIF

m.tmpYear = Doc_Year(m.pDate)
USE (m.base_path+"INCMONEY") SHARED AGAIN ORDER TAG DOC_NUM ALIAS MON_04C16 IN 0
IF .NOT. SEEK(m.pFirm+"Ч"+m.tmpYear+m.pDoc, "MON_04C16")
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Документ не найден!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE IN MON_04C16
  RETURN
ENDIF

USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS CUS_04C16 IN 0
= SEEK(MON_04C16.CUS_CODE, "CUS_04C16")
m.tmpCus = CUS_04C16.CUS_NAME
USE IN CUS_04C16
IF .NOT. Is_Ur(MON_04C16.CUS_CODE)
  PRIVATE mss
  DIMENSION mss(4)
  mss(1) = ""
  mss(2) = CHR(0)+"Клиент "+ALLTRIM(m.tmpCus)
  mss(3) = CHR(0)+"  не юридическое лицо. Формируем приходный ордер? [ Да ] [ Нет ]"
  mss(4) = ""
  IF Out_Mess(7, "mss") = 2
    USE IN MON_04C16
    RETURN
  ENDIF
ENDIF
IF LEFT(MON_04C16.ENTER_DOC, 1) = CHR(0)
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Для этого чека приходный ордер уже сформирован!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE IN MON_04C16
  RETURN
ENDIF

IF Conf()
  m.s_sav = SELECT()
  SELECT MON_04C16
  IF MON_04C16.MONEY_R > 0
    m.tmpDoc = Doc_Num("INC_ORDER ", pFirm, m.arm, pDate)
  ELSE
    m.tmpDoc = Doc_Num("EXP_ORDER ", pFirm, m.arm, pDate)
  ENDIF
  REPLACE ENTER_DOC WITH CHR(0)+sys_char+STR(m.tmpDoc, 9)
  SELECT (m.s_sav)
ENDIF

USE IN MON_04C16

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Conf         Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Подтверждение для создания док-та.                   │
*│                                                                        │
*└────────────────────────────────────────────────────────── 16.12.2004 ──┘
PROCEDURE Conf
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

*┌─────────────────────────────────────────────────────┐
*│      Чек на сумму 999 999 999.99 от ДД.ММ.ГГГГ      │
*│┌─ Клиент ──────────────────────────────────────────┐│
*││ ....:....!....:....!....:....!....:....!....:....!││
*│└───────────────────────────────────────────────────┘│
*│          < OK Ctrl-W > < Отказаться Esc >           │
*└─────────────────────────────────────────────────────┘

PRIVATE ex
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 8, 57, "Подтвердите создание документа"
DO Sun_Bord WITH  3,  2,  5, 54, " Клиент "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 2, 8 SAY "Чек на сумму "+TRANSFORM(MON_04C16.MONEY_R, "999 999 999.99")+  ;
           " от "+DTOC(MON_04C16.DOC_DATE)
@ 4, 4 SAY m.tmpCus
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)

RETURN ex = 1
