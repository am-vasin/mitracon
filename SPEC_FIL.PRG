*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Spec_Fil     Разработчик                                          ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                            Список спецфилиалов.                        ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Spec_Fil
PARAMETERS c_code, c_name

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
stat_type  = 3
what_do    = "List"
menu_name  = PROGRAM()
last_mouse = 0
win_name   = PROGRAM()

PRIVATE s_sav

s_sav = SELECT()
*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DO Use_Dummy
SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE ALIAS A_1411 AGAIN
SET FILTER TO EMPTY(DATE_OFF)

IF CUS_CODE >= 0
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одного спецклиента!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
ENDIF

IF .NOT. SEEK(c_code)
  GO TOP
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбор" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Отказаться" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = 15   && Количество видимых строк BROWSE
wd = 24

DO D_Wins WITH ln, wd, "Спец.клиенты" , 0, 0

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter      KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE KEY -9999999, -1 ;
           FIELDS F000 = LEFT(CUS_NAME,24):24:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Формирование списка и утверждение
    c_code = CUS_CODE
   IF TYPE("c_name") = "C"
     c_name = CUS_NAME
   ENDIF

   EXIT

  OTHERWISE

    EXIT

  ENDCASE

ENDDO
USE

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
RELEASE MENU (menu_name) EXTENDED

SELECT(s_sav)

RETURN
