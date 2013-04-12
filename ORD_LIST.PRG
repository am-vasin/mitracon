*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Ord_List     Разработчик Андрей Васин           02.02.98 09:34:56 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║            Печать и запись в файл заявок на поставку товара.           ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Ord_List
PARAMETER manCode
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

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DO Use_Dummy
SELECT 0
USE (base_path+"CUSTOMER") ORDER TAG CODE
SELECT 0
USE (base_path+"ORD_TIT")
SET RELATION TO SUP_CODE INTO CUSTOMER
IF TYPE("m.manCode") ="N"
  SET FILTER TO WHO_ORD = m.manCode
ENDIF
GO BOTT
IF EOF() .OR. BOF()
  PRIVATE mss
  DIMENSION mss(3)
  mss(1) = ""
  mss(2) = CHR(0)+"Нет ни одной заявки!"
  mss(3) = ""
  DO Out_Mess WITH 7, "mss"
  USE
  SELECT CUSTOMER
  USE
  SELECT DUMMY
  USE
  RETURN
ENDIF

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Send OF (menu_name) PROMPT "Передача" KEY Ctrl-A, "Enter"
ON SELECTION PAD Send OF (menu_name) DO Sw_Mode WITH "Send"

DEFINE PAD URM OF (menu_name) PROMPT "УРМ" KEY Ctrl-A, "Home"
ON SELECTION PAD URM OF (menu_name) DO Sw_Mode WITH "URM"

DEFINE PAD Print OF (menu_name) PROMPT "Печать" KEY Ctrl-A, "Ctrl-P"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD View OF (menu_name) PROMPT "Просмотр" KEY Ctrl-A, "Tab"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = IIF(SET("DATE") = "ON", 75, 73)

DO D_Wins WITH ln, wd, "Заявки", 0, 0

IF SET("DATE") = "ON"
  @ 1, 2 SAY "Фрм─Номер док.─────Дата─────Поставщик────────Примечание"
ELSE
  @ 1, 2 SAY "Фрм─Номер док.───Дата────Поставщик────────Примечание"
ENDIF

what_do = "List"

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Ctrl+P DO Sw_Mode WITH "Print"
    ON KEY LABEL Tab    DO Sw_Mode WITH "View"
    ON KEY LABEL Home   DO Sw_Mode WITH "URM"

*
*    BROWSE - меню
*
    BROWSE FIELDS FIRM:H="", ORD_NUM:H="", ORD_DATE:H="",     ;
                  CUSTOMER.B_NAME:H="", NOTE:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Send")
    ENDIF

  CASE what_do = "Send"    && Файл для передачи

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO S_Ord WITH FIRM, ORD_NUM, ORD_DATE, ""
    ENDIF
    what_do = "List"

  CASE what_do = "Print"   && Печать

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO P_Ord WITH FIRM, ORD_NUM, ORD_DATE
    ENDIF
    what_do = "List"

  CASE what_do = "View"    && Просмотр

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO V_Order WITH FIRM, ORD_DATE, ORD_NUM
    ENDIF
    what_do = "List"

  CASE what_do = "URM"    && УРМ

    IF .NOT. BOF() .AND. .NOT. EOF()
      DO To_URM_O WITH FIRM, ORD_DATE, ORD_NUM
    ENDIF
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
RELEASE MENU (menu_name) EXTENDED

RETURN
