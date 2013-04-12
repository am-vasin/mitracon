*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Cus_Bank     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                         Список банков клиента                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 22.10.2006 ══╝
PROCEDURE Cus_Bank
PARAMETERS prmCode, prmFlags, prmStart
*
*  prmCode  - код клиента
*  prmFlags - флажки (строка - пусто/непусто): 1-выбор, 2- коррекция
*  prmStart - начальное значение банка
*

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
PRIVATE s_sav         && Номер рабочей области для сохранения и восстановления!

*
*   Заполняем значениями переменные состояния...
*
m.stat_type  = 3
m.menu_name  = "C_BNK"
m.last_mouse = 0
m.win_name   = "C_BNK"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
PRIVATE tmpName, tmpFlags, retVal
m.retVal = -1
m.s_sav = SELECT()
m.tmpFlags = IIF(TYPE("m.prmFlags")="C", PADR(m.prmFlags, 2), "  ")

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"ACCOUNT") SHARED AGAIN ORDER TAG CUS_CODE ALIAS ACC_61022
SEEK m.prmCode
SELECT 0
USE (m.base_path+"CUS_BIL") SHARED AGAIN ORDER TAG CUS_CODE ALIAS BIL_61022
SELECT 0
USE (m.base_path+"CUS_BANK") SHARED AGAIN ORDER TAG BANK_CODE ALIAS BANK_61022
m.tmpName = SYS(3)
SEEK STR(m.prmCode)
COPY REST WHILE CUS_CODE=m.prmCode TO (m.tmpo_path+m.tmpName) WITH CDX FOR .NOT. EMPTY(BANK_CODE)
SELECT 0
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_61022 ORDER TAG BANK_CODE
IF SEEK(m.prmCode, "BIL_61022")
  APPEND BLANK
  REPLACE	CUS_CODE	WITH BIL_61022.CUS_CODE,	;
  			BANK		WITH BIL_61022.BANK,		;
  			BIK			WITH BIL_61022.BIK,			;
  			CITY		WITH BIL_61022.CITY,		;
  			CORR_BILL	WITH BIL_61022.CORR_BILL,	;
  			ACC_BILL	WITH BIL_61022.ACC_BILL
ENDIF
IF TYPE("m.prmStart") = "N"
  LOCATE FOR BANK_CODE = m.prmStart
  IF .NOT. FOUND()
    GO TOP
  ENDIF
ELSE
  GO TOP
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF .NOT. EMPTY(SUBSTR(m.tmpFlags, 1, 1))
  DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"
ENDIF

IF .NOT. EMPTY(SUBSTR(m.tmpFlags, 2, 1))
  DEFINE PAD Modi OF (menu_name) PROMPT "Коррекция" KEY Ctrl-A, "Ctrl-Enter"
  ON SELECTION PAD Modi OF (menu_name) DO Sw_Mode WITH "Modi"
  DEFINE PAD Add OF (menu_name) PROMPT "Добавление" KEY Ctrl-A, "Ins"
  ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"
ENDIF

IF .NOT. EMPTY(SUBSTR(m.tmpFlags, 1, 1))
  DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ELSE
  DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ENDIF
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd

* Клиент ....:....!....:....!....:....!....:....!....:....!
*┌Название─────────────────────────────────Город───────────┐
*│....:....!....:....!....:....!....:....! ....:....!....:.│
*                 БИК ....:....!....:....!
*    К/С ....:....!....:....!   Р/С ....:....!....:....!

m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = FSIZE("BANK")+FSIZE("CITY")+1

DO D_Wins WITH m.ln, m.wd, "Список банков клиента", 1, 2
@ 1, 2 SAY "Клиент"
@ 1, 9 SAY ACC_61022.CUS_NAME COLOR SCHEME 1
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    IF .NOT. EMPTY(SUBSTR(m.tmpFlags, 1, 1))
      ON KEY LABEL Enter KEYBOARD CHR(23)
    ENDIF
    IF .NOT. EMPTY(SUBSTR(m.tmpFlags, 2, 1))
      ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "Modi"
      ON KEY LABEL Ins        DO Sw_Mode WITH "Add"
    ENDIF

*
*    BROWSE - меню
*
    BROWSE FIELDS BANK:H="",  ;
                  CITY:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Выбор

    IF EMPTY(SUBSTR(m.tmpFlags, 1, 1)) .OR. BOF() .OR. EOF()
      m.what_do = "List"
    ELSE
      m.retVal = BANK_CODE
      EXIT
    ENDIF

  CASE m.what_do = "Modi"    && Коррекция записи
    IF .NOT. BOF() .AND. .NOT. EOF()
      DO ModiBank WITH BANK_CODE
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Add"    && Добавление записи

    DO ModiBank
    m.what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
USE IN ACC_61022
USE IN BIL_61022
USE IN BANK_61022
USE IN TMP_61022
DELETE FILE (m.tmpo_path+m.tmpName+".dbf")
DELETE FILE (m.tmpo_path+m.tmpName+".cdx")
SELECT (m.s_sav)

RETURN m.retVal

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Draw_Foot    Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                   Рисуем дополнительную информацию.                    │
*│                                                                        │
*└────────────────────────────────────────────────────────── 20.10.1998 ──┘
PROCEDURE Draw_Foot

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
*                 БИК ....:....!....:....!
*    К/С ....:....!....:....!   Р/С ....:....!....:....!

@ WROWS()-3, 18 SAY "БИК "+BIK
@ WROWS()-2,  5 SAY "К/С "+CORR_BILL
@ WROWS()-2, 32 SAY "Р/С "+ACC_BILL

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура ModiBank     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Коррекция/добавление банка                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.10.2006 ──┘
PROCEDURE ModiBank
PARAMETERS prmCode

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

*┌───────────────────────────────────────────────────────────┐
*│ Клиент ....:....!....:....!....:....!....:....!....:....! │
*│┌Банк─────────────────────────────────────Город───────────┐│
*││....:....!....:....!....:....!....:....! ....:....!....:.││
*│└─────────────────────────────────────────────────────────┘│
*│                 БИК ....:....!....:....!                  │
*│                                                           │
*│    К/С ....:....!....:....!   Р/С ....:....!....:....!    │
*│                                                           │
*│             < OK Ctrl-W > < Отказаться Esc >              │
*└───────────────────────────────────────────────────────────┘

PRIVATE ex, new
m.ex = 1
m.new = TYPE("m.prmCode") # "N"
PRIVATE s_sav, tmpBank, tmpCity, tmpBIK, tmpCS, tmpRS

*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 12, 63, "Банк клиента"
DO Sun_Bord WITH  3,  2,  5, 60, ""
@ 2, 3 SAY "Клиент "
@ 2, COL() SAY ACC_61022.CUS_NAME COLOR SCHEME 1
@ 3,  3 SAY "Банк"
@ 3, 44 SAY "Город"

*------------------------------------------------------------------------
*    Начальное заполнение полей бланка
*
IF m.new
  m.tmpBank = SPACE(FSIZE("BANK", "BANK_61022"))
  m.tmpCity = SPACE(FSIZE("CITY", "BANK_61022"))
  m.tmpBIK  = SPACE(FSIZE("BIK",  "BANK_61022"))
  m.tmpCS   = SPACE(FSIZE("CORR_BILL", "BANK_61022"))
  m.tmpRS   = SPACE(FSIZE("ACC_BILL", "BANK_61022"))
ELSE
  m.tmpBank = TMP_61022.BANK
  m.tmpCity = TMP_61022.CITY
  m.tmpBIK  = TMP_61022.BIK
  m.tmpCS   = TMP_61022.CORR_BILL
  m.tmpRS   = TMP_61022.ACC_BILL
ENDIF

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@  4,  3 GET m.tmpBank
@  4, 44 GET m.tmpCity
@  6, 19 SAY "БИК" GET m.tmpBIK
@  8,  6 SAY "К/С" GET m.tmpCS
@  8, 33 SAY "Р/С" GET m.tmpRS

@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE VALID TstBank()

IF m.ex = 1
  PRIVATE tmpCode
  *
  * Отрабатываем бланк
  IF m.new
    SELECT TMP_61022
    GO BOTTOM
    IF BOF() .OR. EOF()
      m.tmpCode = 0
    ELSE
      m.tmpCode = BANK_CODE+1
    ENDIF
    APPEND BLANK
    REPLACE	CUS_CODE	WITH ACC_61022.CUS_CODE, ;
    		BANK_CODE	WITH m.tmpCode
  ENDIF
  REPLACE	BANK		WITH m.tmpBank,	;
  			BIK			WITH m.tmpBIK,	;
  			CITY		WITH m.tmpCity,	;
  			CORR_BILL	WITH m.tmpCS,	;
  			ACC_BILL	WITH m.tmpRS
  *
  *  Сразу же отрабатываем изменения в базе данных
  *
  IF BANK_CODE = 0
    SELECT BIL_61022
    IF .NOT. SEEK(TMP_61022.CUS_CODE)
      APPEND BLANK
      REPLACE	CUS_CODE	WITH ACC_61022.CUS_CODE,	;
      			CUS_NAME	WITH ACC_61022.CUS_NAME,	;
      			DATE_CORR	WITH DATE(),				;
      			WHO_CORR	WITH m.user,				;
      			WHERE_CORR	WITH m.arm
    ENDIF
    IF	BANK # m.tmpBank	.OR.	;
  		BIK # m.tmpBIK		.OR.	;
  		CITY # m.tmpCity	.OR.	;
  		CORR_BILL # m.tmpCS	.OR.	;
  		ACC_BILL # m.tmpRS
  	  REPLACE	BANK		WITH m.tmpBank,	;
  				BIK			WITH m.tmpBIK,	;
  				CITY		WITH m.tmpCity,	;
  				CORR_BILL	WITH m.tmpCS,	;
  				ACC_BILL	WITH m.tmpRS,	;
      			DATE_CORR	WITH DATE(),	;
      			WHO_CORR	WITH m.user,	;
      			WHERE_CORR	WITH m.arm
    ENDIF
  ELSE
    SELECT BANK_61022
    IF .NOT. SEEK(STR(TMP_61022.CUS_CODE)+STR(TMP_61022.BANK_CODE))
      APPEND BLANK
      REPLACE	CUS_CODE	WITH TMP_61022.CUS_CODE, ;
      			BANK_CODE	WITH TMP_61022.BANK_CODE
    ENDIF
    IF	BANK # m.tmpBank	.OR.	;
  		BIK  # m.tmpBIK		.OR.	;
  		CITY # m.tmpCity	.OR.	;
  		CORR_BILL # m.tmpCS	.OR.	;
  		ACC_BILL # m.tmpRS
  	  REPLACE	BANK		WITH m.tmpBank,	;
  				BIK			WITH m.tmpBIK,	;
  				CITY		WITH m.tmpCity,	;
  				CORR_BILL	WITH m.tmpCS,	;
  				ACC_BILL	WITH m.tmpRS
    ENDIF
  ENDIF
  SELECT TMP_61022
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура TstBank      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Выход и бланка                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 22.10.2006 ──┘
PROCEDURE TstBank

PRIVATE mss

IF m.ex # 1
  RETURN .T.
ENDIF

IF EMPTY(m.tmpBank)
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Название банка не может быть пучтым..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.
