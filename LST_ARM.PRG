*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Lst_Arm      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Безальтернативный список рабочих мест                  ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 21.10.2005 ══╝
PROCEDURE Lst_Arm
PARAMETERS tmpAlias


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
USE base_path+"ARM_LST" SHARED AGAIN ORDER TAG NAME ALIAS ARM_5A21
SET RELATION TO ARM_C INTO (m.tmpAlias)

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать/Отказаться" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD All OF (menu_name) PROMPT "Выбрать все" KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION PAD All OF (menu_name) DO Sw_Mode WITH "All"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd, rc_sav
m.ln = WROWS("")-11   && Количество видимых строк BROWSE
m.wd = FSIZE("ARM_N")+2

DO D_Wins WITH m.ln, m.wd, "Список рабочих мест", 0, 0
m.what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    m.statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+Enter DO Sw_Mode WITH "All"

*
*    BROWSE - меню
*
    BROWSE FIELDS &tmpAlias..MARK:H="", ;
                  ARM_N:H="",           ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

   SELECT (m.tmpAlias)
   IF .NOT. SEEK(ARM_5A21.ARM_C)
     APPEND BLANK
     REPLACE CODE WITH ARM_5A21.ARM_C
     IF FSIZE("NAME") # 0
       REPLACE NAME WITH ALLTRIM(ARM_5A21.ARM_N)
     ENDIF
   ENDIF
   REPLACE MARK WITH IIF(EMPTY(MARK), "√", " ")
   SELECT ARM_5A21
   what_do = "List"

  CASE what_do = "All"    && Отметить все

    m.rc_sav = RECNO()
    SCAN
      SELECT (m.tmpAlias)
      IF .NOT. SEEK(ARM_5A21.ARM_C)
        APPEND BLANK
        REPLACE CODE WITH ARM_5A21.ARM_C
        IF FSIZE("NAME") # 0
          REPLACE NAME WITH ARM_5A21.ARM_N
        ENDIF
      ENDIF
      REPLACE MARK WITH "√"
      SELECT ARM_5A21
    ENDSCAN
    IF BETWEEN(m.rc_sav, 1, RECCOUNT())
      GO m.rc_sav
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
USE IN ARM_5A21
SELECT (m.s_sav)

RETURN

