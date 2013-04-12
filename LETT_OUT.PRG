*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Lett_Out     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                          Исходящие сообщения.                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 19.10.2005 ══╝
PROCEDURE Lett_Out
PARAMETERS prmWho

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
PRIVATE tmpWho, tmpRc

IF TYPE("m.prmWho") = "N"
  m.tmpWho = m.prmWho
ELSE
  m.tmpWho = m.user
ENDIF

m.s_sav = SELECT()

SELECT 0
DO Use_Dummy

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE AGAIN ALIAS PR_5A19

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C AGAIN ALIAS ARM_5A19

SELECT 0
USE (base_path+"LETT_IN") ORDER TAG MESS_ID AGAIN ALIAS IN_5A19

SELECT 0
USE (base_path+"LETT_OUT") ORDER TAG WHO DESCENDING AGAIN ALIAS OUT_5A19
SET RELATION TO MESS_ID INTO IN_5A19

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Text OF (menu_name) PROMPT "Текст сообщения" KEY Ctrl-A, "Enter"
ON SELECTION PAD Text OF (menu_name) DO Sw_Mode WITH "Text"

DEFINE PAD For_Who OF (menu_name) PROMPT "Адресаты" KEY Ctrl-A, "Tab"
ON SELECTION PAD For_Who OF (menu_name) DO Sw_Mode WITH "For_Who"

DEFINE PAD Create OF (menu_name) PROMPT "Создать" KEY Ctrl-A, "Ins"
ON SELECTION PAD Create OF (menu_name) DO Sw_Mode WITH "Create"

*DEFINE PAD Delete OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
*ON SELECTION PAD Delete OF (menu_name) DO Sw_Mode WITH "Delete"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd, d_name, ln_1
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.ln_1 = FSIZE("TITLE")
m.wd = m.ln_1+20

DO D_Wins WITH m.ln, m.wd, "Исходящие сообщения", 0, 1
@ 1, 2 SAY "Тема"
@ 1, WCOLS()-21 SAY "Дата"
@ WROWS()-3, WCOLS()-21 SAY "Дата"
@ 1, WCOLS()-10 SAY "Время"
@ WROWS()-3, WCOLS()-10 SAY "Время"

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    ACTIVATE SCREEN
    @ 0, 0 SAY SPACE(WCOLS()) COLOR SCHEME 2
    SHOW MENU (m.menu_name)
    ACTIVATE WINDOW (win_name+"_M") SAME
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Tab DO Sw_Mode WITH "For_Who"
    ON KEY LABEL Ins DO Sw_Mode WITH "Create"
*    ON KEY LABEL Del DO Sw_Mode WITH "Delete"

*
*    BROWSE - меню
*
    BROWSE KEY STR(m.tmpWho) REST ;
           FIELDS TITLE:H="",   ;
                  CR_DATE:H="", ;
                  CR_TIME:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Draw_Foot()    ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Text")
    ENDIF

  CASE what_do = "Text"			&& Просмотр сообщения

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO LettView WITH MESS_ID
    ENDIF
    what_do = "List"

  CASE what_do = "For_Who"		&& Адресаты

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO Lett_Who WITH MESS_ID
    ENDIF
    what_do = "List"

  CASE what_do = "Delete"		&& Удалить

    IF .NOT. BOF() .AND. .NOT. EOF()
      wait wind what_do
    ENDIF
    what_do = "List"

  CASE what_do = "Create"		&& Создать

    m.tmpRc = Lett_Cr()
    IF BETWEEN(m.tmpRc, 1, RECCOUNT())
      GO m.tmpRc
    ENDIF
    what_do = "List"

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
USE IN PR_5A19
USE IN ARM_5A19
USE IN IN_5A19
USE IN OUT_5A19

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
PRIVATE tmpAddr

ACTIVATE WINDOW (win_name+"_M") SAME

*
*   Вот здесь, рисуем...
*

IF EMPTY(IN_5A19.ADDRESS_T)
  @ WROWS()-3, 3 SAY "Кому"
  = SEEK(VAL(IN_5A19.ADDRESS), "PR_5A19")
  m.tmpAddr = ALLTRIM(PR_5A19.FAMILY)+" "+LEFT(PR_5A19.NAME, 1)+" "+LEFT(PR_5A19.S_NAME, 1)
ELSE
  @ WROWS()-3, 3 SAY "Куда"
  = SEEK(LEFT(IN_5A19.ADDRESS, FSIZE("ARM_C", "ARM_5A19")), "ARM_5A19")
  m.tmpAddr = ARM_5A19.ARM_N
ENDIF
m.tmpAddr = PADR(m.tmpAddr, ln_1)
@ WROWS()-2, 2 SAY m.tmpAddr
@ WROWS()-2, COL()+1 SAY IN_5A19.SHOW_DATE
@ WROWS()-2, COL()+1 SAY IN_5A19.SHOW_TIME

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
