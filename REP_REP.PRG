*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Rep_Rep      Разработчик Андрей Васин           10.10.97 15:49:10 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                             Повтор отчета.                             ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Rep_Rep
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
*   Определяем асинхронное меню
*
DEFINE MENU (menu_name) IN SCREEN COLOR SCHEME 4

DEFINE PAD Choose OF (menu_name) PROMPT "Выбрать" KEY Ctrl-A, "Enter"
ON SELECTION PAD Choose OF (menu_name) DO Sw_Mode WITH "Choose"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
DO Use_Dummy

SELECT 0
USE (base_path+"ACCOUNT") ORDER TAG CUS_CODE

SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE

SELECT 0
USE (base_path+"ARM_LST") ORDER TAG ARM_C

SELECT 0
USE (base_path+"REP_TIT")
SET RELATION TO SUP_CODE INTO ACCOUNT
GO BOTTOM

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = MIN(100, RECCOUNT())   && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY") = "ON", 57, 55)

DO D_Wins WITH ln, wd, "Отчеты перед поставщиками", 0, 1
@ 1, 2 SAY "Номер─"+IIF(SET("CENTURY") = "ON", "───Дата────", "──Дата───")+ ;
           "Поставщик"
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
    BROWSE FIELDS N_REP:H="":5,     ;
                  D_REP:H="",       ;
                  ACCOUNT.CUS_NAME:H="":50, ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_F()  ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "Choose")
    ENDIF

  CASE what_do = "Choose"    && Выбор отчета

    DO Out_R_S WITH REP_TIT.N_REP, REP_TIT.D_REP
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

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                        Печать доп. информфции.                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 10.10.97 16:10:13 ═╝
PROCEDURE Show_F
PRIVATE nm1, nm2

ACTIVATE WINDOW (win_name+"_M") SAME

IF .NOT. SEEK(REP_TIT.WHO, "PERSONS")
  nm1 = SPACE(FSIZE("FAMILY", "PERSONS")+6)
ELSE
  nm1 = ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+". "+LEFT(PERSONS.S_NAME,1)+"."
  nm1 = PADR(nm1, FSIZE("FAMILY", "PERSONS")+6)
ENDIF

IF .NOT. SEEK(REP_TIT.WHERE, "ARM_LST")
  nm2 = SPACE(FSIZE("ARM_N", "ARM_LST")+6)
ELSE
  nm2 = ARM_LST.ARM_N
ENDIF

@ WROWS()-2, 2 SAY "Кто "
@ ROW(), COL() SAY nm1 COLOR SCHEME 1
@ ROW(), COL() SAY " Где "
@ ROW(), COL() SAY nm2 COLOR SCHEME 1

ACTIVATE WINDOW (ALIAS())

RETURN .T.
