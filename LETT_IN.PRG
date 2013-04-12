*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Lett_In      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Просмотр входящих сообщений                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 28.10.2005 ══╝
PROCEDURE Lett_In
PARAMETERS prmUser, prmArm

PRIVATE tmpUser, tmpArm, tmpBlank
m.tmpUser = IIF(.NOT. EMPTY(m.prmUser), m.prmUser, m.user)
m.tmpArm  = IIF(.NOT. EMPTY(m.prmArm ), m.prmArm , m.arm )

PRIVATE tmpKey, tmpSw, whoName, whereName
m.tmpSw = .T.


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
USE (base_path+"PERSONS") SHARED AGAIN ORDER TAG CODE ALIAS TMP_PRS
SEEK m.tmpUser
m.whoName = ALLTRIM(FAMILY)+" "+LEFT(NAME, 1)+" "+LEFT(S_NAME, 1)
m.whoName = PADR(m.whoName, 40)

SELECT 0
USE (base_path+"ARM_LST") SHARED AGAIN ORDER TAG ARM_C ALIAS TMP_ARM
SEEK m.tmpArm
m.whereName = "АРМ "+ALLTRIM(ARM_N)
m.whereName = PADR(m.whereName, 40)

USE (base_path+"LETT_OUT") SHARED AGAIN ORDER TAG MESS_ID ALIAS TMP_OUT
SET RELATION TO WHO INTO TMP_PRS

SELECT 0
USE (base_path+"LETT_IN") SHARED AGAIN ORDER TAG ADDRESS DESCENDING ALIAS TMP_IN
SET RELATION TO MESS_ID INTO TMP_OUT

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Read OF (menu_name) PROMPT "Прочитать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Read OF (menu_name) DO Sw_Mode WITH "Read"

DEFINE PAD Address OF (menu_name) PROMPT "Получатель" KEY Ctrl-A, "Tab"
ON SELECTION PAD Address OF (menu_name) DO Sw_Mode WITH "Address"

DEFINE PAD Answer OF (menu_name) PROMPT "Ответить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Answer OF (menu_name) DO Sw_Mode WITH "Answer"

DEFINE PAD Delete OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = 52

DO D_Wins WITH m.ln, m.wd, "Входящие сообщения", 1, 1
m.what_do = "List"

@ 1, 2 SAY "Получатель"
@ 2, 2 SAY ;
 "Тема─────────────────────────────Отправлено"
*X....:....!....:....!....:....!..XДД.ММ.ГГГГXЧЧ:ММ:ССX
@ WROWS()-3, 2 SAY "Отправитель"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 2
    SHOW MENU (m.menu_name)
    ACTIVATE WINDOW (win_name+"_M") SAME
    IF m.tmpSw
      @ 1, 13 SAY m.whoName COLOR SCHEME 1
      m.tmpKey = "  "+STR(m.tmpUser, 5)
    ELSE
      @ 1, 13 SAY m.whereName COLOR SCHEME 1
      m.tmpKey = " *"+PADR(m.tmpArm, 5)
    ENDIF
    
    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab   DO Sw_Mode WITH "Address"
    ON KEY LABEL Ins   DO Sw_Mode WITH "Answer"
    ON KEY LABEL Del   DO Sw_Mode WITH "Delete"

*
*    BROWSE - меню
*
    BROWSE KEY m.tmpKey REST    ;
           FIELDS TMP_OUT.TITLE:H="",    ;
                  TMP_OUT.CR_DATE:H="",  ;
                  TMP_OUT.CR_TIME:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Read")
    ENDIF

  CASE m.what_do = "Read"    && Просмотр сообщения

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO LettView WITH MESS_ID
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Answer"  && Ответить

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Lett_Ans WITH TMP_OUT.WHO
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Delete"  && Удалить

    IF .NOT. BOF() .AND. .NOT. EOF()
      SCATTER TO tmpBlank BLANK
      GATHER FROM tmpBlank
    ENDIF
    m.what_do = "List"

  CASE m.what_do = "Address" && Получатель

    m.tmpSw = .NOT. m.tmpSw
    GO TOP
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
USE IN TMP_IN
USE IN TMP_OUT
USE IN TMP_PRS
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
PRIVATE tmpV

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*
m.tmpV = ALLTRIM(TMP_PRS.FAMILY)+" "+LEFT(TMP_PRS.NAME, 1)+" "+LEFT(TMP_PRS.S_NAME, 1)
m.tmpV = PADR(m.tmpV, WCOLS()-6)
@ 	WROWS()-2, 3 SAY m.tmpV

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.


