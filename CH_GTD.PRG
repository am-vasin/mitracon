*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Ch_GTD       Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                               Выбор ГТД.                               ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 14.02.2000 ══╝
PROCEDURE Ch_GTD
PARAMETERS StartVal

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
PRIVATE retVal
retVal = startVal

SELECT 0
DO Use_Dummy
SELECT 0
USE (path_comm+"GTD") ORDER TAG GTD_NUM ALIAS GTD_0214 AGAIN
IF EOF() .OR. BOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Вы не заготовили ни одной ГТД!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  stat_type = SPACE(FSIZE("GTD_NUM"))+CHR(13)+SPACE(FSIZE("COUNTRY"))+CHR(13)+SPACE(FSIZE("GTD_LINE"))
  USE
  SELECT (s_sav)
  RETURN stat_type
ENDIF

IF .NOT. SEEK(startVal)
  GO TOP
ENDIF
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
wd = FSIZE("GTD_NUM")+FSIZE("COUNTRY")+1

DO D_Wins WITH ln, wd, "Выберите ГТД", 0, 0
@ 1, 2 SAY PADR("Номер ГТД", FSIZE("GTD_NUM")+1, "─")+  ;
           PADR("Страна", FSIZE("COUNTRY"), "─")
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
    BROWSE FIELDS GTD_NUM:H="", COUNTRY:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F        ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор

    retVal = GTD_NUM+CHR(13)+COUNTRY+CHR(13)+GTD_LINE+CHR(13)+COUNTRYC
    EXIT

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
USE
SELECT (s_sav)

RETURN retVal
