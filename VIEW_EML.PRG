*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл View_Fax     Разработчик Андрей Васин           30.09.98 16:00:53 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                           Просмотр эл. почты.                          ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE View_Eml

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

DEFINE PAD View OF (menu_name) PROMPT "Просмотр" KEY Ctrl-A, "Enter"
ON SELECTION PAD View OF (menu_name) DO Sw_Mode WITH "View"

DEFINE PAD Delete OF (menu_name) PROMPT "Удаление"
ON PAD Delete OF (menu_name) ACTIVATE POPUP Delete

  DEFINE POPUP Delete MARGIN RELATIVE SHADOW COLOR SCHEME 4

  DEFINE BAR  1 OF Delete PROMPT "Текущей строки" KEY Ctrl-A, "Del"
  ON SELECTION BAR 1 OF Delete DO Sw_Mode WITH "Del_1"

  DEFINE BAR  2 OF Delete PROMPT "Всех отправленных" KEY Ctrl-A, "F8"
  ON SELECTION BAR 2 OF Delete DO Sw_Mode WITH "Del_All"

DEFINE PAD Exit OF (menu_name) PROMPT "Выход" KEY Ctrl-A, "Esc"
ON SELECTION PAD Exit OF (menu_name) DO Sw_Mode WITH "Exit"

*
*   Открываем файлы Б.Д., устанавливаем связи и т. д.
*

DO Use_Dummy
SELECT 0
USE (send_log+"SENDMESS")
SET FILTER TO WHERE = arm .AND. MAIL # " "
GO TOP

*
*   Расчет размеров окна
*
PRIVATE ln, wd
ln = WROWS()-12    && Количество видимых строк BROWSE
wd = IIF(SET("CENTURY") = "ON", 55, 51)

DO D_Wins WITH ln, wd, "Отправка эл. почты", 0, 2
@ WROWS()-3, 3 SAY "Тема"
@ WROWS()-2, 3 SAY "Адрес"
IF SET("CENTURY") = "ON"
  @ 1, 2 SAY ;
  "Номер───Дата───────Корреспондент─────────────Отправлен"
ELSE
  @ 1, 2 SAY ;
  "Номер───Дата─────Корреспондент─────────────Отправл."
ENDIF
what_do = "List"
GO BOTT

DO WHILE .T.

  DO CASE

  CASE what_do = "List"    && Просмотр списка

    statys_type = 3
    DO Prp_Nav_1
    ON KEY LABEL Enter KEYBOARD CHR(23)
    ON KEY LABEL Del   DO Sw_Mode WITH "Del_1"
    ON KEY LABEL F8    DO Sw_Mode WITH "Del_All"

*
*    BROWSE - меню
*
    BROWSE FIELDS CODE:H="",    ;
                  DATE_ON:H="", ;
                  TO:H="",      ;
                  SENDMAIL:H="", ;
           DUMMY.F:H="" FREEZE DUMMY.F ;
           WHEN Footer()  ;
           NOAPPEND NODELETE NOMODIFY NOLGRID NOMENU NOCLEAR  ;
           WINDOW (win_name+"_I") IN WINDOW (win_name+"_E")
    ON KEY
    IF what_do = "List"
      what_do = IIF(READKEY() % 256 = 12, "Exit", "View")
    ENDIF

  CASE what_do = "View"    && Просмотр документа

    DELETE FILE (tmpo_path+"VIEW_FAX.TXT")
    IF .NOT. File_O(ALLTRIM(NAME))
      PRIVATE mss
      DIMENSION mss(3)
      mss(1) = ""
      mss(2) = CHR(0)+"Само послание по каким-то причинам утрачено..."
      mss(3) = ""
      DO Out_Mess WITH 7, "mss"
    ELSE
      IF ".DBF" $ NAME
        DO View_DBF WITH ALLTRIM(NAME), ALLTRIM(SUBJECT)
      ELSE
        DO View_Txt WITH ALLTRIM(NAME), ALLTRIM(SUBJECT), 66, 0, "1"
      ENDIF
    ENDIF
    what_do = "List"

  CASE what_do = "Del_1"   && Удаление строки

    IF .NOT. BOF() .AND. .NOT. EOF()
      DELETE FILE (ALLTRIM(NAME))
      DELETE
    ENDIF
    what_do = "List"

  CASE what_do = "Del_All"    && Удаление отправленных факсов

    SCAN FOR .NOT. EMPTY(SENDFAX)
      DELETE
      DELETE FILE (ALLTRIM(NAME))
      DELETE
    ENDSCAN
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
*║                                Footer!                                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════ 30.09.98 16:45:32 ═╝
PROCEDURE Footer

ACTIVATE WINDOW (win_name+"_M") SAME

@ WROWS()-3, 8 SAY LEFT(SUBJECT,WCOLS()-10)
@ WROWS()-2, 9 SAY MAILADDR

ACTIVATE WINDOW (ALIAS()) SAME

RETURN .T.
