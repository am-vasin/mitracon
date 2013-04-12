*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Ch_O_T       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                           Выбор типа заказа                            ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 16.11.2007 ══╝
PROCEDURE Ch_O_T
PARAMETERS srcType

PRIVATE tmpType, s_sav, tmpName, rc

m.tmpType = PADL(ALLTRIM(m.srcType), 10)
m.s_sav = SELECT()
SELECT 0
USE m.base_path+"ORD_TYPE" SHARED AGAIN ALIAS TMP_OT
m.tmpName = SYS(3)
CREATE DBF (m.tmpo_path+m.tmpName) ;
	(CODE C(10), NAME C(FSIZE("ORD_NAME", "TMP_OT")))
USE (m.tmpo_path+m.tmpName) ALIAS TMP_LIST
m.rc = 1
APPEND BLANK
REPLACE NAME WITH "Автоматический выбор типа заказа"
APPEND BLANK
REPLACE CODE WITH "         0", ;
        NAME WITH "Тип заказа по умолчанию"
IF m.tmpType = CODE
  m.rc = RECNO()
ENDIF
SELECT TMP_OT
SCAN
  SELECT TMP_LIST
  APPEND BLANK
  REPLACE CODE WITH STR(TMP_OT.CODE), ;
          NAME WITH TMP_OT.ORD_NAME
  IF m.tmpType = CODE
    m.rc = RECNO()
  ENDIF
  SELECT TMP_OT
ENDSCAN

USE

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
SELECT 0
DO Use_Dummy
SELECT 0
SELECT TMP_LIST
GO m.rc

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
m.wd = FSIZE("NAME")

DO D_Wins WITH m.ln, m.wd, "Укажите тип заказа", 0, 0
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
    BROWSE FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    m.tmpType = CODE
    EXIT
*    m.what_do = "List"

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
m.tmpName = DBF()
USE
DELETE FILE (m.tmpName)
SELECT (m.s_sav)

RETURN m.tmpType
