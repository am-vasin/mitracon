*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Pers_Acc     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Специальные санкции для пользователей.                 ║
*║                            Список санкций.                             ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 17.10.2006 ══╝
PROCEDURE Pers_Acc

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
DO Tmp_Acc

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Список пользователей" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"


DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = FSIZE("ACC_NAME")

DO D_Wins WITH m.ln, m.wd, "Специальные санкции для пользователей", 0, 0
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
    BROWSE FIELDS ACC_NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    DO Acc_Pers WITH RECNO("TMP_ACC"), ALLTRIM(TMP_ACC.ACC_NAME)
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
PRIVATE tmpName
m.tmpName = DBF()
USE
DELETE FILE (m.tmpName)
SELECT (m.s_sav)

RETURN

*┌────────────────────────────────────────────────────────────────────────┐
*│   Процедура Tmp_Acc      Разработчик Андрей Васин                      │
*├────────────────────────────────────────────────────────────────────────┤
*│                                                                        │
*│                             Список санкций                             │
*│                                                                        │
*└────────────────────────────────────────────────────────── 17.10.2006 ──┘
PROCEDURE Tmp_Acc

PRIVATE tmpName

m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(	ACC_NAME	C(50)	)
USE (m.tmpo_path+m.tmpName) EXCLUSIVE ALIAS TMP_ACC
APPEND BLANK
REPLACE ACC_NAME WITH "Доступ к закупочным ценам при продаже"
GO TOP

RETURN

*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Acc_Pers     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Специальные санкции для пользователей.                 ║
*║                         Список пользователей.                          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 17.10.2006 ══╝
PROCEDURE Acc_Pers
PARAMETERS tmpCode, tmpName

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
USE (m.base_path+"ACC_PERS") SHARED AGAIN ORDER TAG PERS_CODE ALIAS ACC_61017
SELECT 0
USE (m.base_path+"USERS") SHARED AGAIN ORDER TAG USER_CODE ALIAS USER_61017
SELECT 0
USE (m.base_path+"PERSONS") SHARED AGAIN ORDER TAG NAME ALIAS PERS_61017
SET RELATION TO CODE INTO ACC_61017, CODE INTO USER_61017
SET FILTER TO FOUND("USER_61017")

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Доступ" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = FSIZE("FAMILY", "PERS_61017")+ ;
       FSIZE("NAME", "PERS_61017")+   ;
       FSIZE("S_NAME", "PERS_61017")+9

DO D_Wins WITH m.ln, m.wd, m.tmpName, 0, 0
@ 1, 2 SAY ;
       PADR("Фамилия", FSIZE("FAMILY", "PERS_61017")+1, "─")+ ;
       PADR("Имя", FSIZE("NAME", "PERS_61017")+1, "─")+   ;
       PADR("Отчество", FSIZE("S_NAME", "PERS_61017")+1, "─")+"Доступ"

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
    BROWSE FIELDS PERS_61017.FAMILY:H="",  ;
                  PERS_61017.NAME:H="",    ;
                  PERS_61017.S_NAME:H="",  ;
                  TMP_ACC=SUBSTR(ACC_61017.ACCESS, m.tmpCode, 1):6, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    SELECT ACC_61017
    IF .NOT. FOUND("ACC_61017")
      APPEND BLANK
      REPLACE PERS_CODE WITH PERS_61017.CODE
    ENDIF
    IF EMPTY(SUBSTR(ACCESS, m.tmpCode, 1))
      REPLACE ACCESS WITH STUFF(ACCESS, m.tmpCode, 1, "√")
    ELSE
      REPLACE ACCESS WITH STUFF(ACCESS, m.tmpCode, 1, " ")
    ENDIF
    SELECT PERS_61017
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
USE IN ACC_61017
USE IN USER_61017
USE IN PERS_61017
SELECT (m.s_sav)

RETURN
