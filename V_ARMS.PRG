*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл V_Arms       Разработчик Андрей Васин           21.01.98 11:15:58 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                        Состояния рабочих мест.                         ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE V_Arms

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

DEFINE PAD Switch OF (menu_name) PROMPT "Только активные" KEY Ctrl-A, "F6"
ON SELECTION PAD Switch OF (menu_name) DO Sw_Mode WITH "Switch"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*
DO Use_Dummy
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE
SELECT 0
USE (base_path+"PERSONS") ORDER TAG CODE ALIAS P0001 AGAIN
SELECT 0
USE (base_path+"ARM_INFO") ORDER TAG ARM_C
SET RELATION TO WHO INTO P0001
SELECT 0
USE (base_path+"ARM_LST")
SET FILTER TO ARM_S = sys_char
SET RELATION TO US_CODE INTO PERSONS
SET RELATION TO ARM_C INTO ARM_INFO ADDITIVE
*
*   Расчет размеров окна
*
PRIVATE ln, wd, dt_l, fn_l, sw
ln = WROWS("")-14   && Количество видимых строк BROWSE
wd = FSIZE("ARM_N")+FSIZE("SYS_NAM")+IIF(SET("CENTURY") = "ON", 21, 19)
dt_l = IIF(SET("CENTURY") = "ON", 10, 8)
sw = .T.

DO D_Wins WITH ln, wd, "Состояние рабочих мест", 0, 5
@ 1, 2 SAY   PADR("Рабочее место", FSIZE("ARM_N"), "─")
@ 1, COL()+1 SAY PADR("Система", FSIZE("SYS_NAM"), "─")
@ 1, COL()+1 SAY PADC("Дата", dt_l, "─")
@ 1, COL()+1 SAY PADC("Время", 8, "─")

fn_l = FSIZE("ARM_N")+FSIZE("SYS_NAM")+1
@ WROWS()-7, 2 SAY PADR("Функция", fn_l, "─")
@ WROWS()-7, COL()+1 SAY PADC("Дата", dt_l, "─")
@ WROWS()-7, COL()+1 SAY PADC("Время", 8, "─")
what_do = "List"

*!....:....!....:....!....:....!....:....!....:....
*!Расп. ....:....!....:....!  Тел. ....:....!....:.
*!        Сетевое имя ....:....!....:....!
*!   Пользователь ....:....!....:....!....:....!

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL F6 DO Sw_Mode WITH "Switch"

*
*    BROWSE - меню
*
    BROWSE FIELDS ARM_N:H="", SYS_NAM:H="",     ;
                  F000 = IIF(EMPTY(SYS_DAT), SPACE(dt_l), DTOC(SYS_DAT)):H="", ;
                  SYS_TIM:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Show_F() ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "exit", "edit")
    ENDIF

  CASE what_do = "Switch"    && Смена фильтра

    DO Sw_List
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
*║                          Информационное окно.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.01.98 14:52:35 ═╝
PROCEDURE Show_F
PRIVATE nm

ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-6, 2 CLEAR TO WROWS()-5, WCOLS()-3

IF .NOT. EMPTY(SYS_NAM)
  @ WROWS()-6, 2 SAY LEFT(FUN_NAM,fn_l)+" "+DTOC(FUN_DAT)+" "+FUN_TIM
ENDIF

IF .NOT. EMPTY(US_DAT)
  IF EMPTY(US_CODE)
    nm = "САМ работает!"
  ELSE
    IF .NOT. FOUND("PERSONS")
      nm = "Кто-то неизвестный!"
    ELSE
      nm = ALLTRIM(PERSONS.FAMILY)+" "+LEFT(PERSONS.NAME,1)+"."+LEFT(PERSONS.S_NAME,1)+"."
    ENDIF
  ENDIF
  @ WROWS()-5, 2 SAY PADR(nm,fn_l)+" "+DTOC(US_DAT)+" "+US_TIM
ENDIF

*!Расп. ....:....!....:....!  Тел. ....:....!....:.
*!        Сетевое имя ....:....!....:....!
*!   Пользователь ....:....!....:....!....:....!
@ WROWS()-4, 2 CLEAR TO WROWS()-2, WCOLS()-3

IF FOUND("ARM_INFO")
  @ WROWS()-4,  2 SAY "Где "+ARM_INFO.LOCATE+"  Тел. "+ARM_INFO.PHONE
  @ WROWS()-3, 10 SAY "Сетевое имя "+ARM_INFO.NET_NAME
  nm = ALLTRIM(P0001.FAMILY)+" "+LEFT(P0001.NAME,1)+" "+LEFT(P0001.S_NAME,1)
  @ WROWS()-2, 5 SAY "Пользователь "+PADR(nm, 30)
ENDIF

ACTIVATE WINDOW (ALIAS())

RETURN .T.

*╔════════════════════════════════════════════════════════════════════════╗
*║                                                                        ║
*║                         Переключение фильтра.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 21.01.98 15:05:45 ═╝
PROCEDURE Sw_List

sw = .NOT. sw
IF sw
  DEFINE PAD Switch OF (menu_name) PROMPT "Только активные" KEY Ctrl-A, "F6"
  SET FILTER TO ARM_S = sys_char
ELSE
  DEFINE PAD Switch OF (menu_name) PROMPT "Все" KEY Ctrl-A, "F6"
  SET FILTER TO ARM_S = sys_char .AND. .NOT. EMPTY(SYS_NAM)
ENDIF

RETURN


