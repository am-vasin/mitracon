*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Add_Sto      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                  Поиск в дополнительной номенклатуре                   ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 18.03.2008 ══╝
PROCEDURE Add_Sto
PARAMETER tmpList, rc_no

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
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (m.base_path+"LIST_DET") SHARED AGAIN ORDER TAG L_NAME ALIAS ADD_STO_L
SELECT 0
USE (m.base_path+"ADD_CLS") SHARED AGAIN ORDER TAG CODE
SELECT 0
USE (m.base_path+"ADD_STO") SHARED AGAIN ORDER TAG NAME
SET RELATION TO SOURCE+CLASS INTO ADD_CLS
IF .NOT. EMPTY(m.rc_no)
  GO m.rc_no
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF .NOT. EMPTY(m.tmpList)
  DEFINE PAD Choose OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"
ENDIF

DEFINE PAD Seek OF (menu_name) PROMPT "Конт.поиск F7"
ON SELECTION PAD Seek OF (menu_name) DO Sw_Mode WITH "Seek"

DEFINE PAD ASeek OF (menu_name) PROMPT "Поиск по арт. Ctrl-F7"
ON SELECTION PAD ASeek OF (menu_name) DO Sw_Mode WITH "ASeek"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


DIMENSION interr1(1), interr2(1)
interr1(1) = -6
interr2(1) = "{F7}"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, fnc
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = 75

DO D_Wins WITH m.ln, m.wd, "Дополнительная номенклатура", 1, 6
ACTIVATE WINDOW (win_name+"_M")
@ 1, 3 SAY "Поиск"
@ 1, 9 SAY PADR("Tab",25) COLOR (SCHEME(14,2))

m.fnk = ""
@ WROWS()-6, 2 EDIT m.fnk SIZE 5, 75 NOMODIFY
@ 2,  5 SAY "Наименование"
@ 2, 52 SAY "Корпус"
@ 2, 69 SAY "Сегмент"

@ WROWS()-8, 2 SAY "Функц. назначение"
SCROLL WROWS()-8, 1, WROWS()-7, WCOLS()-2, -1
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab DO Seek_P
    ON KEY LABEL F7  DO Sw_Mode WITH "Seek"
    ON KEY LABEL Ctrl+F7  DO Sw_Mode WITH "ASeek"

*
*    BROWSE - меню
*
    BROWSE FIELDS PREFIX:H="",		;
                  NAME:H="",		;
                  PRODUCER:H="",	;
                  CORPUS:H="",		;
                  SEGMENT:H="",		;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    IF .NOT. EMPTY(m.tmpList)
      IF Tst_Dubl()
        DO Add_Sto1
      ENDIF
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Seek"      && Поиск

    CLEAR GETS
    DO Seek_C
    m.what_do = "List"

  CASE m.what_do = "ASeek"      && Поиск по артикулу

    CLEAR GETS
    DO Seek_A
    m.what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

CLEAR READ
RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED
*
*   Аккуратненько (аккуратненько!) закрываем DBF-файлы.
*
USE IN ADD_STO
USE IN ADD_STO_L
USE IN ADD_CLS

SELECT (m.s_sav)

RETURN

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
CLEAR GETS
m.fnk = FUNCTION
@ WROWS()-8, 2 SAY "Класс" GET ADD_CLS.NAME
@ WROWS()-6, 2 EDIT m.fnk SIZE 5, 75 NOMODIFY
@ WROWS()-1, WCOLS()-23 SAY "Артикул "
@ WROWS()-1, COL() SAY Articul(CODE) COLOR (SCHEME(1, 1))
CLEAR GETS

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                      Поиск по начальным символам.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.06.97 10:43:18 ═╝
PROCEDURE Seek_P
PRIVATE prf, s, ln, rc_sav, i

ACTIVATE WINDOW (win_name+"_M") SAME

prf = ""
ln = 35
DO WHILE .T.
  ACTIVATE WINDOW (win_name+"_M") SAME
  @ 1, 9 SAY REPLICATE("▒",35) COLOR SCHEME 1
  @ 1, 9 SAY prf COLOR SCHEME 1
  s = INKEY(0)
  *
  *   Исследуем на предмет наличия обработчика
  *
  i = ASCAN(interr1, s)
  IF i # 0
    KEYBOARD interr2(i)
    EXIT
  ENDIF
  IF s < 0       && Функциональная клавиша
    IF s = -9
      KEYBOARD "{F10}"
      EXIT
    ENDIF
    LOOP
  ENDIF
  IF BETWEEN(s,  32,126) .OR.   ;
     BETWEEN(s, 128,175) .OR.   ;
     BETWEEN(s, 224,241)     && Символ
    IF LEN(prf) >= 35        && Все! Больше некуда!
      LOOP
    ENDIF
    rc_sav = RECNO()
    IF SEEK(UPPER(prf+CHR(s)))
      prf = prf+CHR(s)
      SHOW WINDOW (ALIAS()) REFRESH
    ELSE
      ?? CHR(7)
      GO rc_sav
    ENDIF
  ENDIF
  IF s = 127       && Back Space
    IF LEN(prf) > 0
      prf = LEFT(prf, LEN(prf)-1)
      = SEEK(UPPER(prf))
      SHOW WINDOW (ALIAS()) REFRESH
    ENDIF
  ENDIF
  IF s = 13        && Enter
    EXIT
  ENDIF
  IF s = 24        && Down Arrow
    KEYBOARD "{DnArrow}"
    EXIT
  ENDIF
  IF s = 5         && Up Arrow
    KEYBOARD "{UpArrow}"
    EXIT
  ENDIF
  IF s = 3         && Page Down
    KEYBOARD "{PgDn}"
    EXIT
  ENDIF
  IF s = 18        && Page Up
    KEYBOARD "{PgUp}"
    EXIT
  ENDIF
  IF s = 27        && Escape
    KEYBOARD "{Esc}"
    EXIT
  ENDIF
ENDDO

@ 1, 9 SAY PADR("Tab",35) COLOR (SCHEME(14,2))
  
ACTIVATE WINDOW (ALIAS()) SAME

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_C       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Контекстный поиск                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.03.2008 ──┘
PROCEDURE Seek_C

PRIVATE tmpShab, tmpFile, s_sav, r_sav

m.tmpSahb = Get_Shab()
IF EMPTY(m.tmpSahb)
  RETURN 
ENDIF

m.s_sav = SELECT()
SELECT ADD_STO
m.r_sav = RECNO()
m.tmpFile = SYS(3)
SELECT 0
CREATE DBF (m.tmpo_path+m.tmpFile) ;
	(	LINK		N(10),	;
		PREFIX		C(FSIZE("PREFIX", "ADD_STO")),	;
		NAME		C(FSIZE("NAME", "ADD_STO")),	;
		PRODUCER	C(FSIZE("PRODUCER", "ADD_STO"))	)
USE (m.tmpo_path+m.tmpFile) EXCLUSIVE ALIAS ADD_SEEK
m.tmpSahb = UPPER(m.tmpSahb)
SELECT ADD_STO
DO Wt_Mess WITH "Поиск..."
SCAN
  IF m.tmpSahb $ UPPER(PREFIX+NAME+PRODUCER)
    SELECT ADD_SEEK
    APPEND BLANK
    REPLACE LINK		WITH RECNO("ADD_STO"),	;
            PREFIX		WITH ADD_STO.PREFIX,	;
            NAME		WITH ADD_STO.NAME,		;
            PRODUCER	WITH ADD_STO.PRODUCER
    SELECT ADD_STO
  ENDIF
ENDSCAN
DO Wt_Mess
IF BETWEEN(m.r_sav, 1, RECCOUNT())
  GO m.r_sav
ENDIF

SELECT ADD_SEEK
m.tmpFile = DBF()
USE

DO Ch_Found WITH m.tmpFile

SELECT (m.s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Ch_Found     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                          Выбор из найденного                           │
*│                                                                        │
*└────────────────────────────────────────────────────────── 18.03.2008 ──┘
PROCEDURE Ch_Found
PARAMETERS tmpName

PRIVATE mss

SELECT 0
USE (m.tmpName) EXCLUSIVE ALIAS ADD_FOUND
IF RECCOUNT() = 0
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Ничего не найдено..."
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE
  DELETE FILE (m.tmpName)
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
m.stat_type  = 3
m.menu_name  = PROGRAM()
m.last_mouse = 0
m.win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DO Use_Dummy
SELECT ADD_FOUND

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = 49

DO D_Wins WITH m.ln, m.wd, "Результат поиска", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS PREFIX:H="",	;
                  NAME:H="",	;
                  PRODUCER:H="",;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    GO ADD_FOUND.LINK IN ADD_STO
    EXIT

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
USE
DELETE FILE (m.tmpName)

SELECT (m.s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tst_Dubl     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                А может такая позиция в заказе уже есть?                │
*│                                                                        │
*└────────────────────────────────────────────────────────── 23.03.2008 ──┘
PROCEDURE Tst_Dubl

PRIVATE tmpName, mss

m.tmpName = PADR(ALLTRIM(ADD_STO.NAME),     FSIZE("NAME",   "ADD_STO_L"))+ ;
            PADL(ALLTRIM(ADD_STO.PREFIX),   FSIZE("PREFIX", "ADD_STO_L"))+ ;
            PADR(ALLTRIM(ADD_STO.PRODUCER), FSIZE("PRODUCER",   "ADD_STO_L"))
IF SEEK(m.tmpList+m.tmpName, "ADD_STO_L")
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Эта позиция уже есть в заказе"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  RETURN .F.
ENDIF

RETURN .T.

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Add_Sto1     Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                       Добавление позиции в заказ                       │
*│                                                                        │
*└────────────────────────────────────────────────────────── 23.03.2008 ──┘
PROCEDURE Add_Sto1
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
PRIVATE s_sav

PRIVATE tPrf, tNam, tProd, tQnt

m.tPrf  = PADL(ALLTRIM(ADD_STO.PREFIX),   FSIZE("PREFIX", "ADD_STO_L"))
m.tNam  = PADR(ALLTRIM(ADD_STO.NAME),     FSIZE("NAME",   "ADD_STO_L"))
m.tProd = PADR(ALLTRIM(ADD_STO.PRODUCER), FSIZE("PRODUCER",   "ADD_STO_L"))
m.tQnt  = 0

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

*┌─────────────────────────────────────────────────────────────┐
*│ ┌─ Позиция ───────────────────────────────────────┐┌Кол-во┐ │
*│ │.... ....:....!....:....!....:....!....: ....:...││123456│ │
*│ └─────────────────────────────────────────────────┘└──────┘ │
*│               < OK Ctrl-W > < Отказаться Esc >              │
*└─────────────────────────────────────────────────────────────┘

PRIVATE ex
m.ex = 1
*------------------------------------------------------------------------

PUSH KEY CLEAR       && На всякий пожарный случай!
DO Prp_Nav_2
DO D_Win_N WITH 7, 65, "Добавление позиции в заказ"
DO Sun_Bord WITH  2,  3,  4, 53, " Позиция "
DO Sun_Bord WITH  2,  54,  4, 61, ""
@ 2, 55 SAY "Кол-во"

*------------------------------------------------------------------------
*      Ввод полей бланка
*
@ 3,  4 SAY m.tPrf COLOR SCHEME 1
@ 3,  9 SAY m.tNam COLOR SCHEME 1
@ 3, 45 SAY m.tProd COLOR SCHEME 1
@ 3, 55 GET m.tQnt PICTURE "@Z 999999"
@ WROWS()-2, FLOOR(WCOLS()/2-16) GET m.ex PICTURE "@*HT \! OK Ctrl-W ;\? Отказаться Esc "

READ CYCLE

IF m.ex = 1 .AND. m.tQnt > 0
  *
  * Отрабатываем бланк
  m.s_sav = SELECT()
  SELECT ADD_STO_L
  APPEND BLANK
  REPLACE LIST_NUM	WITH m.tmpList,	;
          PREFIX	WITH m.tPrf, 	;
          NAME		WITH m.tNam, 	;
          PRODUCER	WITH m.tProd, 	;
          QNT		WITH m.tQnt
  
  SELECT (m.s_sav)
ENDIF
*--------------------------------------------------------------------------

POP KEY
RELEASE WINDOW (win_name)
RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Seek_A       Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                           Поиск по артикулу                            │
*│                                                                        │
*└────────────────────────────────────────────────────────── 24.03.2008 ──┘
PROCEDURE Seek_A

PRIVATE tmpShab, seekSh, i, c

m.tmpSahb = Get_ShA()

m.seekSh = ""
IF LEN(m.tmpSahb) > 0
  FOR m.i = 1 TO LEN(m.tmpSahb)
    m.c = SUBSTR(m.tmpSahb, m.i, 1)
    IF m.c $ "0123456789"
      m.seekSh = m.seekSh+m.c
    ENDIF
  ENDFOR
ENDIF

m.seekSh = VAL(m.seekSh)

IF EMPTY(m.seekSh)
  RETURN 
ENDIF

USE (m.base_path+"ADD_STO") SHARED AGAIN ALIAS SEEK_TMP ORDER TAG CODE IN 0
IF SEEK(STR(m.seekSh), "SEEK_TMP")
  GO RECNO("SEEK_TMP") IN ADD_STO
ELSE
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Нет такого артикула!"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

USE IN SEEK_TMP

RETURN
