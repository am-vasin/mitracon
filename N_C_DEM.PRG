*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла N_C_Dem      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                           Спрос по клиенту.                            ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 27.12.2000 ══╝
PROCEDURE N_C_Dem
PARAMETER super
PRIVATE pl, tmpCusCode, tmpCusName

tmpCusCode = 0
tmpCusName = SPACE(50)
DO Use_Dummy
DO Use_Link
SELECT 0
IF super
  USE (base_path+"ACCOUNT") ORDER TAG CUS_NAME
ELSE
  USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE
ENDIF
IF super
  pl = Choose_P()
ELSE
  pl = sys_char
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

*┌─────────────────────────────────────────────────────────┐
*│ ┌─ [ ] Клиент ────────────────────────────────────────┐ │
*│ │ ....:....!....:....!....:....!....:....!....:....!  │ │
*│ └─────────────────────────────────────────────────────┘ │
*│           < Справка > < Спрос > < Выход Esc >           │
*└─────────────────────────────────────────────────────────┘

PRIVATE sw_cus, ex
sw_cus = .F.
ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 61, "Спрос по клиенту"
DO Sun_Bord WITH  2,  3,  4, 57, "            "

*------------------------------------------------------------------------
*      Ввод полей бланка
*

@ 2,  6 GET sw_cus PICTURE "@*C Клиент" VALID Ch_Cus()
@ 3,  5 GET tmpCusName WHEN .F.
@ 5, 13 GET ex PICTURE "@*HT \ Справка ;\ Спрос ;\? Выход Esc "

READ CYCLE VALID To_Work()

*--------------------------------------------------------------------------
SELECT ACCOUNT
USE
POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Cus       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Выбор клиента.                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.12.2000 ──┘
PROCEDURE Ch_Cus
PRIVATE tmpC, tmpN

IF sw_cus = .F.
  RETURN .T.
ENDIF
sw_cus = .F.

SHOW GET sw_cus

IF super
  IF Custs("", .F., .F., .F., pl)
    tmpCusName = ACCOUNT.CUS_NAME
    SHOW GET tmpCusName
    tmpCusCode = ACCOUNT.CUS_CODE
  ENDIF
ELSE
  tmpC = tmpCusCode
  tmpN = tmpCusName
  DO Own_Cus WITH tmpC, tmpN
  IF tmpC # tmpCusCode
    tmpCusCode = tmpC
    tmpCusName = tmpN
    = SEEK(tmpC, "ACCOUNT")
    SHOW GET tmpCusName
  ENDIF
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура To_Work      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Обработка запроса.                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 27.12.2000 ──┘
PROCEDURE To_Work
PRIVATE mss

IF ex = 3
  RETURN .T.
ENDIF

IF tmpCusCode = 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не указали клиента"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  RETURN 1
ENDIF

IF ex = 1
  DO Acc_Hist WITH tmpCusCode
ELSE
  DO Cus_Dem1 WITH tmpCusCode, tmpCusName
ENDIF
ACTIVATE SCREEN
@ 0, 0 SAY SPACE(WCOLS()) COLOR (SCHEME(1,2))

RETURN .F.
