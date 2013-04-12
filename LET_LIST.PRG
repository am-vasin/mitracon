*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Let_List     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║         Коррекция списка допущенных к заказам рабочего места.          ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 15.09.2006 ══╝
PROCEDURE Let_List
PARAMETERS prmArm	&& код рабочего места, если пусто, то текущее.

PRIVATE tmpArm
m.tmpArm = IIF(EMPTY(m.prmArm), m.arm, m.prmArm)

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
USE (m.base_path+"ARM_LST") SHARED AGAIN ALIAS ARM_6914 ORDER TAG ARM_C
SEEK m.tmpArm
SELECT 0
USE (m.base_path+"LET_LIST") SHARED AGAIN ALIAS LL_6914 ORDER TAG USER_CODE
SELECT 0
USE (m.base_path+"PERSONS") SHARED AGAIN ALIAS PR_6914 ORDER TAG NAME
SET RELATION TO STR(CODE)+m.tmpArm INTO LL_6914


*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Разрешить/запретить" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd
m.ln = WROWS("")-5   && Количество видимых строк BROWSE
m.wd = 46

DO D_Wins WITH m.ln, m.wd, "Список допущенных к заказам рабочего места", 1, 0
m.what_do = "List"
@ 1, 2 SAY "Рабочее место "+ARM_6914.ARM_N
@ 2, 2 SAY ;
"─Сотрудник───────────────────────────────Дост."
*....:....!....:....!....:....!....:....! 

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS ;
    	   tmpFIO = ALLTRIM(PR_6914.FAMILY)+" "+LEFT(PR_6914.NAME, 1)+" "+LEFT(PR_6914.S_NAME, 1):40:H="", ;
    	   tmpMrk = IIF(FOUND("LL_6914"), "  √  ", "     "):5:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    IF .NOT. EOF() .AND. .NOT. BOF()
      SELECT LL_6914
      IF FOUND("LL_6914")
        REPLACE USER_CODE WITH 0, ARM_CODE WITH ""
      ELSE
        GO TOP
        IF .NOT. EMPTY(USER_CODE) .OR. EOF()
          APPEND BLANK
        ENDIF
        REPLACE USER_CODE WITH PR_6914.CODE, ARM_CODE WITH ARM_6914.ARM_C
      ENDIF
      SELECT PR_6914
    ENDIF
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
USE IN LL_6914
USE IN PR_6914
USE IN ARM_6914
SELECT (m.s_sav)

RETURN
