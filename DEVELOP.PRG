*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Develop      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║            Программа выхода из поля фирмы-производителя ЭК.            ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 06.06.2000 ══╝
PROCEDURE Develop
PARAMETER dev_code

IF TYPE("ex") = "N"
  IF ex = 2
    RETURN .T.
  ENDIF
ENDIF

IF READKEY() % 256 = 12
  RETURN .T.
ENDIF

PRIVATE varName, varValue, s_sav, r

varName = VARREAD()
varValue = &varName

s_sav = SELECT()
SELECT 0
USE (base_path+"DEVELOP") ORDER TAG B_NAME AGAIN ALIAS DEV_0606
IF SEEK(varValue)
  IF TYPE("dev_code") = "N"
    dev_code = CODE
  ENDIF
  USE
  SELECT (s_sav)
  RETURN .T.
ENDIF

r = RECNO(0)
IF BETWEEN(r, 1, RECCOUNT())
  GO r
ELSE
  GO BOTTOM
ENDIF

WAIT WINDOW "Нет такой фирмы, попробуйте поискать в списке!" NOWAIT

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

SELECT 0
DO Use_Dummy
SELECT DEV_0606

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
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = 8

DO D_Wins WITH ln, wd, "Произв. ЭК", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS F000=LEFT(B_NAME,8):8:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    &varName = LEFT(B_NAME,8)
    SHOW GET &varName
    IF TYPE("dev_code") = "N"
      dev_code = CODE
    ENDIF
    r = .T.
    EXIT

  OTHERWISE

    r = .F.
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
SELECT (s_sav)

RETURN r
