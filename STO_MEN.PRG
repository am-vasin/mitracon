*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Sto_Men      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                       Список сотрудников склада.                       ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 30.12.1999 ══╝
PROCEDURE Sto_Men

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
stat_type  = 3
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS M_9C30 AGAIN
SELECT 0
USE (base_path+"STO_MEN") ORDER TAG ALL_NAMES ALIAS S_M_9C30 AGAIN
SET RELATION TO CODE INTO M_9C30
REPLACE ALL NAME   WITH M_9C30.NAME,   ;
            S_NAME WITH M_9C30.S_NAME, ;
            FAMILY WITH M_9C30.FAMILY
SELECT M_9C30
USE
SELECT S_M_9C30

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Work OF (menu_name) PROMPT "На работе" KEY Ctrl-A, "Enter"
ON SELECTION PAD Work OF (menu_name) DO Sw_Mode WITH "Work"

DEFINE PAD Add OF (menu_name) PROMPT "Добавить" KEY Ctrl-A, "Ins"
ON SELECTION PAD Add OF (menu_name) DO Sw_Mode WITH "Add"

DEFINE PAD Del OF (menu_name) PROMPT "Удалить" KEY Ctrl-A, "Del"
ON SELECTION PAD Del OF (menu_name) DO Sw_Mode WITH "Del"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
*┌────────────────────────────────────────────────────────────────┐
*│....:....!....:....!....:....!....:....!....:....!....:....!.. .│
*└────────────────────────────────────────────────────────────────┘
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 64

DO D_Wins WITH ln, wd, "Сотрудники склада", 0, 0
what_do = "List"

PRIVATE tmpF, tmpN, tmpS, tmpC, www

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ins   DO Sw_Mode WITH "Add"
    ON KEY LABEL Del   DO Sw_Mode WITH "Del"

*
*    BROWSE - меню
*
    BROWSE FIELDS S000 = FAMILY-(" "+NAME)-(" "+S_NAME):H="",  ;
                  S001 = IIF(WORK, "√", " "):H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Work")
    ENDIF

  CASE what_do = "Work"    && На работе

    REPLACE WORK WITH .NOT. WORK
    what_do = "List"

  CASE what_do = "Del"     && Удалить

    REPLACE CODE WITH 0, WORK WITH .F.
    what_do = "List"

  CASE what_do = "Add"     && Добавить

    STORE "" TO tmpF, tmpN, tmpS, www
    tmpC = 0
    DO Ch_Pers WITH tmpC, www, tmpF, tmpN, tmpS
    IF tmpC # 0
      www = RECNO()
      LOCATE FOR tmpC = CODE
      IF FOUND()
        what_do = "List"
        LOOP
      ENDIF
      SET ORDER TO
      LOCATE FOR CODE = 0
      IF .NOT. FOUND()
        APPEND BLANK
      ENDIF
      REPLACE CODE   WITH tmpC, ;
              FAMILY WITH tmpF, ;
              NAME   WITH tmpN, ;
              S_NAME WITH tmpS, ;
              WORK   WITH .F.
      SET ORDER TO ALL_NAMES
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
SELECT S_M_9C30
USE
SELECT (s_sav)

RETURN
