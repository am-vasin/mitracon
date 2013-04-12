*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Imp_List     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                Импорт списка из чужой рабочей области.                 ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 07.09.2006 ══╝
PROCEDURE Imp_List
*
*  Переменные состояния для навигации
*

*
*    Проверяем, а доступно ли юзеру хоть что-нибудь?
USE (m.base_path+"LET_LIST") SHARED AGAIN ORDER TAG USER_CODE ALIAS L_L_6915 IN 0
IF .NOT. SEEK(STR(m.user), "L_L_6915")
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Вам не доступны чужие списки"
  mss[3] = ""
  USE IN L_L_6915
  DO Out_Mess WITH 7, "mss"
  RETURN 0
ENDIF
USE IN L_L_6915

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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌─────────────────────────────────────────────────┐
*│  ( ) По номеру списка 999999                    │
*│  ( ) Выбрать на рабочем месте ....:....!....:.  │
*│                                                 │
*│        < OK Ctrl-W > < Отказаться Esc >         │
*└─────────────────────────────────────────────────┘

PRIVATE ex, tmpSw, tmpList, tmpArmC, tmpArmN, resList
m.resList = 0
m.ex = 1
m.tmpSw   = 1
m.tmpList = "      "
m.tmpArmC = "   "
m.tmpArmN = SPACE(16)
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 53, "Импорт списка с чужого рабочего места"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  2,  4 GET m.tmpSw PICTURE "@*RV По номеру списка;Выбрать на рабочем месте" ;
                     VALID G_Arm()
@  2, 25 GET m.tmpList PICTURE "999999" WHEN m.tmpSw = 1
@  3, 33 GET m.tmpArmN WHEN .F.

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID Ex_Blank()

*IF m.ex = 1
  *
  * Отрабатываем бланк
  
*ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.resList

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура G_Arm        Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Выбор рабочего места                          │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.09.2006 ──┘
PROCEDURE G_Arm

PRIVATE tmpC, tmpN

IF m.tmpSw = 1
  m.tmpArmC = "   "
  m.tmpArmN = SPACE(16)
ELSE
  m.tmpC = m.tmpArmC
  m.tmpN = m.tmpArmN
  m.tmpC = EnabList(.F., m.tmpN)
  IF .NOT. EMPTY(m.tmpC)
    m.tmpArmC = m.tmpC
    m.tmpArmN = m.tmpN
  ENDIF
  IF EMPTY(m.tmpArmC)
    m.tmpSw = 1
    m.tmpArmC = "   "
    m.tmpArmN = SPACE(16)
  ELSE
    m.tmpList = "      "
  ENDIF
ENDIF

SHOW GET m.tmpSw
SHOW GET m.tmpArmC
SHOW GET m.tmpArmN
SHOW GET m.tmpList

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ex_Blank     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                            Выход из бланка                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.09.2006 ──┘
PROCEDURE Ex_Blank
PRIVATE mss

IF m.ex = 2
  RETURN .T.
ENDIF

IF m.tmpSw = 1
  IF EMPTY(m.tmpList)
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"Вы не указали номер списка!"
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    RETURN .F.
  ENDIF
  USE (m.base_path+"LIST_TIT") SHARED AGAIN ALIAS TMP_TIT_ ORDER TAG LST_NUM IN 0
  IF .NOT. SEEK(VAL(ALLTRIM(m.tmpList)), "TMP_TIT_")
    DIMENSION mss[3]
    mss[1] = ""
    mss[2] = CHR(0)+"Указанного Вами списка не существует!"
    mss[3] = ""
    DO Out_Mess WITH 7, "mss"
    USE IN TMP_TIT_
    RETURN .F.
  ENDIF
  USE IN TMP_TIT_
  m.resList = VAL(ALLTRIM(m.tmpList))
ELSE
  m.resList = Ch_List(m.tmpArmC)
  IF EMPTY(m.resList)
    m.resList = 0
    RETURN .F.
  ENDIF
  m.resList = VAL(ALLTRIM(m.resList))
ENDIF
IF .NOT. List_Info(m.resList)
  m.resList = 0
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура List_Info    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Информация о списке                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 07.09.2006 ──┘
PROCEDURE List_Info
PARAMETERS n_list
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
m.stat_type  = 1
m.what_do    = ""
m.menu_name  = ""
m.last_mouse = 0
m.win_name   = PROGRAM()

*------------------------------------------------------------------------
*    Содержательная часть программы:
*

*┌────────────────────────────────────────────────────┐
*│ Список создал МММММММММММММ 99.99.9999             │
*│ Рабоее место АААААААААА                            │
*│┌─ Клиент ─────────────────────────────────────────┐│
*││....:....!....:....!....:....!....:....!....:....!││
*│└──────────────────────────────────────────────────┘│
*│       Сумма по списку 99999999.9999                │
*│        < OK Ctrl-W > < Отказаться Esc >            │
*└────────────────────────────────────────────────────┘

PRIVATE ex, tmpStr, is_enab

m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 10, 56, "Информация о списке № "+ALLTRIM(STR(m.n_list))
DO Sun_Bord WITH  4,  2,  6, 53, " Клиент "

*------------------------------------------------------------------------
*      Ввод полей бланка
*
USE (m.base_path+"LIST_TIT") SHARED AGAIN ORDER TAG LST_NUM ALIAS TMP0_6907 IN 0
= SEEK(m.n_list, "TMP0_6907")
m.tmpStr = "Сумма по списку "+ALLTRIM(STR(TMP0_6907.R_TOTAL, 15, 4))+IIF(TMP0_6907.IS_USD, "$", "Р")
@  7, 2 SAY PADC(m.tmpStr, WCOLS()-4)
m.is_enab = TMP0_6907.LST_ARM
USE (m.base_path+"LET_LIST") SHARED AGAIN ORDER TAG USER_CODE ALIAS TMP9_6907 IN 0
m.is_enab = SEEK(STR(m.user)+is_enab, "TMP9_6907")
USE IN TMP9_6907

USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS TMP1_6907 IN 0
= SEEK(TMP0_6907.WHO_CR, "TMP1_6907")
m.tmpStr = "Список создал "+ALLTRIM(TMP1_6907.FAMILY)+" "+LEFT(TMP1_6907.NAME, 1)+" "+LEFT(TMP1_6907.S_NAME, 1)+"  "+;
           DTOC(TMP0_6907.CR_DAT)
@  2, 2 SAY PADC(m.tmpStr, WCOLS()-4)

USE (m.base_path+"ARM_LST") SHARED AGAIN ORDER TAG ARM_C ALIAS TMP1_6907 IN TMP1_6907
= SEEK(TMP0_6907.LST_ARM, "TMP1_6907")
m.tmpStr = "Рабочее место "+ALLTRIM(TMP1_6907.ARM_N)
@  3, 2 SAY PADC(m.tmpStr, WCOLS()-4)

USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS TMP1_6907 IN TMP1_6907
= SEEK(TMP0_6907.CLI_CODE, "TMP1_6907")
@  5, 3 SAY TMP1_6907.CUS_NAME
USE IN TMP1_6907
USE IN TMP0_6907

IF m.is_enab
  @ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "
  READ CYCLE
ELSE
  m.tmpStr = "Этот список Вам недоступен. Намите что-нибудь..."
  @ WROWS()-2, 2 SAY PADC(m.tmpStr, WCOLS()-4)
  m.ex = 2
  SET CURSOR OFF
  = INKEY(0)
  SET CURSOR ON
ENDIF

*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN m.ex = 1
