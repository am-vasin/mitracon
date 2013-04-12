*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла EnabList     Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║   Список рабочих мест, доступных пользователю для импорта/экспорта.    ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 15.09.2006 ══╝
PROCEDURE EnabList
PARAMETERS prmUser, prmArmN

PRIVATE tmpUser, mss

m.tmpUser = IIF(EMPTY(m.prmUser), m.user, m.prmUser)
USE (m.base_path+"LET_LIST") SHARED AGAIN ALIAS LL_6915 ORDER TAG USER_CODE IN 0
IF .NOT. SEEK(STR(m.tmpUser), "LL_6915")
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Вам не доступны списки ни одного рабочего места"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN LL_6915
  RETURN "   "
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
USE (m.base_path+"ARM_LST") SHARED AGAIN ALIAS ARM_6915 ORDER TAG NAME
SET RELATION TO STR(m.tmpUser)+ARM_C INTO LL_6915
SET FILTER TO FOUND("LL_6915")
GO TOP
IF BOF() .OR. EOF()
  DIMENSION mss[3]
  mss[1] = ""
  mss[2] = CHR(0)+"Вам не доступны списки ни одного рабочего места"
  mss[3] = ""
  DO Out_Mess WITH 7, "mss"
  USE IN LL_6915
  USE IN ARM_6915
  SELECT (m.s_sav)
  RETURN "   "
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Ctrl-Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"


*
*   Расчет размеров окна
*
PRIVATE ln, wd, retV
m.retV = "   "
m.ln = WROWS("")-7   && Количество видимых строк BROWSE
m.wd = 16

DO D_Wins WITH m.ln, m.wd, "Дост. рабочие места", 0, 0
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
    BROWSE FIELDS ARM_N:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF m.what_do = "List"
      m.what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE m.what_do = "Choose"    && Просмотр списка

    m.retV = ARM_C
    IF TYPE("m.prmArmN") = "C"
      m.prmArmN = ARM_N
    ENDIF
    EXIT
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
USE IN LL_6915
USE IN ARM_6915
SELECT (m.s_sav)

RETURN m.retV
