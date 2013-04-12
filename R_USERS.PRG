*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла R_Users      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                Список пользователей с заданным доступом                ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 20.02.2002 ══╝
PROCEDURE R_Users
PARAMETERS r_code

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
PRIVATE tmpName, tmpRight

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

s_sav = SELECT()

SELECT 0
DO Use_Dummy
SELECT 0
USE (base_path+"S_RIGHTS") SHARED AGAIN ALIAS TMP_20220 ORDER TAG RIGHT
SEEK r_code
tmpRight = ALLTRIM(NAME)
USE (base_path+"PERSONS") SHARED AGAIN ALIAS PS_20220 ORDER TAG CODE
SELECT 0
USE (base_path+"USERS") SHARED AGAIN ALIAS USER_20220
SET RELATION TO USER_CODE INTO PS_20220

SELECT 0
tmpName = SYS(3)
CREATE DBF (tmpo_path+tmpName) ;
  ( CODE   N( 5), ;
    FAMILY C(FSIZE("FAMILY", "PS_20220")), ;
    NAME   C(FSIZE("NAME", "PS_20220")),   ;
    S_NAME C(FSIZE("S_NAME", "PS_20220"))  )
USE (tmpo_path+tmpName) EXCLUSIVE ALIAS TMP_20220

SELECT USER_20220
SCAN FOR S_RIGHTS = r_code
  SELECT TMP_20220
  APPEND BLANK
  REPLACE CODE   WITH PS_20220.CODE,  ;
          FAMILY WITH PS_20220.FAMILY,;
          NAME   WITH PS_20220.NAME,  ;
          S_NAME WITH PS_20220.S_NAME
  SELECT USER_20220
ENDSCAN
USE
SELECT PS_20220
USE
SELECT TMP_20220
INDEX ON SYS(15, lwr, FAMILY+NAME+S_NAME) TAG NAME
GO TOP

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = FSIZE("FAMILY")+FSIZE("NAME")+FSIZE("S_NAME")+2

DO D_Wins WITH ln, wd, tmpRight, 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
*    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS FAMILY:H="",  ;
                  NAME:H="",    ;
                  S_NAME:H="",  ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Просмотр списка

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
tmpRight = DBF()
USE
DELETE FILE (tmpRight)
DELETE FILE (LEFT(tmpRight, LEN(tmpRight)-3)+"cdx")
SELECT (s_sav)

RETURN
