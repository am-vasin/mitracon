*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Ryn_Sto      Разработчик Андрей Васин           04.02.98 13:55:11 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                      Ведомость наличия в филиале.                      ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Ryn_Sto

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
DELETE FILE (tmpo_path+"RYN_STO.DBF")
DELETE FILE (tmpo_path+"RYN_STO.CDX")

CREATE DBF (tmpo_path+"RYN_STO.DBF") ;
  ( CUS_CODE N( 8),  ;
    CUS_NAME C(50),  ;
    VALUTA   C( 5)   )
INDEX ON SYS(15, lwr, CUS_NAME) TAG NAME

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"RYN_L_T")
SET RELATION TO CUS_CODE INTO ACCOUNT
SCAN
  SELECT RYN_STO
  APPEND BLANK
  REPLACE CUS_CODE WITH ACCOUNT.CUS_CODE, ;
          CUS_NAME WITH ACCOUNT.CUS_NAME, ;
          VALUTA   WITH IIF(RYN_L_T.VALUTA = "$", "USD", "Рубли")
  SELECT RYN_L_T
ENDSCAN

USE
SELECT ACCOUNT
USE
DO Use_Dummy

*
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Print OF (menu_name) PROMPT "Печать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Print OF (menu_name) DO Sw_Mode WITH "Print"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

SELECT RYN_STO
GO TOP
*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = 56

DO D_Wins WITH ln, wd, "Филиалы", 0, 0
what_do = "List"
@ 1, 2 SAY "── Название ────────────────────────────────────── Цена "
DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)

*
*    BROWSE - меню
*
    BROWSE FIELDS CUS_NAME:H="", VALUTA:H="",     ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Print")
    ENDIF

  CASE what_do = "Print"    && Печать ведомости наличия

    DO P_STO_F WITH CUS_CODE
    what_do = "List"

  OTHERWISE

    EXIT

  ENDCASE

ENDDO

RELEASE WINDOW (win_name+"_I")
RELEASE WINDOW (win_name+"_E")
RELEASE WINDOW (win_name+"_M")
CLOSE DATABASES
DELETE FILE (tmpo_path+"RYN_STO.DBF")
DELETE FILE (tmpo_path+"RYN_STO.CDX")
RELEASE MENU (menu_name) EXTENDED

RETURN
