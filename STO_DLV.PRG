*╔════════════════════════════════════════════════════════════════════════╗
*║   Имя файла Sto_Dlv      Разработчик Андрей Васин                      ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                Выбор позиции из номенклатуры доставки.                 ║
*║                                                                        ║
*╚══════════════════════════════════════════════════════════ 29.03.2000 ══╝
PROCEDURE Sto_Dlv
PARAMETERS pCode, pName


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
USE (base_path+"STOCK") ORDER TAG CODE ALIAS S0_0329 AGAIN

SELECT 0
USE (base_path+"DLV_STO") ORDER TAG NAME ALIAS S1_0329 AGAIN
SET RELATION TO CODE INTO S0_0329

REPLACE ALL NAME WITH ALLTRIM(S0_0329.PREFIX)+S0_0329.NAME

SET RELATION TO
SELECT S0_0329
USE

SELECT S1_0329
IF EMPTY(pCode)
  GO TOP
ELSE
  SET ORDER TO TAG CODE
  IF .NOT. SEEK(pCode)
    SET ORDER TO TAG NAME
    GO TOP
  ELSE
    SET ORDER TO TAG NAME
  ENDIF
ENDIF
IF BOF() .OR. EOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Номенклатура доставки пуста..."
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT (s_sav)
  RETURN
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

IF TYPE("pCode") = "N"
  DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
  ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

  DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
  ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

ELSE

  DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
  ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"
ENDIF

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS("")-11   && Количество видимых строк BROWSE
wd = FSIZE("NAME")

DO D_Wins WITH ln, wd, "Номенклатура доставки", 0, 0
what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    IF TYPE("pCode") = "N"
      ON KEY LABEL Enter KEYBOARD CHR(23)
    ENDIF

*
*    BROWSE - меню
*
    BROWSE FIELDS NAME:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOMODIFY   ;
           NOAPPEND NODELETE NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      IF TYPE("pCode") = "N"
        what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
      ELSE
        what_do = "Exit"
      ENDIF
    ENDIF

  CASE what_do = "Choose"    && Выбор

    pCode = CODE
    IF TYPE("pName") = "C"
      pName = NAME
    ENDIF
    EXIT
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
USE
SELECT (s_sav)

RETURN
